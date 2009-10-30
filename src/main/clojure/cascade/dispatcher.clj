; Copyright 2009 Howard M. Lewis Ship
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
; implied. See the License for the specific language governing permissions
; and limitations under the License.

(ns 
  #^{:doc "Request dispatch to views and actions"}
  cascade.dispatcher
  (:import (javax.servlet ServletResponse))
  (:use 
    cascade
    (cascade asset config dom logging path-map pipeline fail func-utils)
    (cascade.internal utils)))

(defn prepare-dom-for-render
  "Post-processes the DOM after the view has constructed it, but before it has been rendered out
   as the response. This is a chance to make modifications to the DOM to support global concerns.
   This function is often decorated with filters to add new functionality. It is passed the
   env and a seq of DOM nodes and should return the same DOM nodes, or a modified set of DOM nodes."
   [env dom-nodes]
   ; Return unchanged
   dom-nodes)
   
(decorate prepare-dom-for-render 
  (fn [delegate env dom-nodes]
    (delegate env
      (extend-dom dom-nodes [:html :head] :top 
        (template :meta { :name "generator" :content "Cascade http://github.com/hlship/cascade" })))))   

(defn add-script-links-for-imported-javascript-libraries
  [env dom-nodes]
  (let [aggregation (-> env :cascade :resource-aggregation)
        libraries (@aggregation :libraries)]
    (extend-dom dom-nodes [:html :head] :top
      (template-for [asset-map libraries
                     :let [path (to-asset-path env asset-map)]]
        ; The linebreak is to keep some braindead browsers from getting confused.
        :script { :type "text/javascript" :src path } [ linebreak ]))))

(defn add-script-block-for-initialization
  [env dom-nodes]
  (let [aggregation (-> env :cascade :resource-aggregation)
        immediate (@aggregation :immediate)
        onready (@aggregation :onready)]
    (extend-dom dom-nodes [:html :body] :bottom
      (template
        ; this could be done more efficiently
        :script {:type "text/javascript"} [
          (for [line immediate]
            [line linebreak])
          (when-not (empty? onready)
            (import-jquery env)
            (interpose "\n"
              (concat [ "jQuery(document).ready(function() {" ]
                onready
                [ "});"])))            
        ]))))
        
(defn add-links-for-stylesheets
  [env dom-nodes]
    (let [aggregation (-> env :cascade :resource-aggregation)
          stylesheets (@aggregation :stylesheets)]
      (extend-dom dom-nodes [:html :head] :top
        (template-for [asset-map stylesheets
                       :let [path (to-asset-path env asset-map)]]
          :link { :rel :stylesheet :type "text/css" :href path })))) 
                
(decorate prepare-dom-for-render
  (fn [delegate env dom-nodes]
    (delegate env (add-script-links-for-imported-javascript-libraries env dom-nodes))))
    
; This decoration comes first in execution order, to ensure that the jQuery library (if added)
; gets reflected in the rendered DOM.

(decorate prepare-dom-for-render
  (fn [delegate env dom-nodes]
    (delegate env (add-script-block-for-initialization env dom-nodes))))   
    
(decorate prepare-dom-for-render
  (fn [delegate env dom-nodes]
    (delegate env (add-links-for-stylesheets env dom-nodes))))      

(defn render-view-as-xml
  "Renders the provided view function as an XML stream. Returns true."
  [env view-fn]
  (debug "Rendering view function %s" (qualified-function-name view-fn))
  (let [#^ServletResponse response (-> env :servlet-api :response)
        dom (view-fn env)
        prepared (prepare-dom-for-render env dom)]
    (debug "Streaming XML response")         
    (with-open [writer (.getWriter response)]
      (render-xml prepared writer)))
  true)

(defn render-view
  "Renders a view; in the future this will use meta-data to determine the correct way to do this, for 
   the moment, this simply sets up for resource aggegation and invokes render-view-as-xml."
  [env view-fn]
  (let [aggregation (atom { :libraries [] :immediate [] :onready [] :stylesheets [] })
        new-env (assoc-in env [:cascade :resource-aggregation] aggregation)]
    ; TODO: Eventually we may have a render as HTML pipeline based on view function meta-data.
    (render-view-as-xml new-env view-fn)))

(defn handle-view-request
  "A pipeline that calls render-view."  
  [env view-fn]
  (render-view env view-fn))

(defn verify-is-view-fn
  "A filter that verifies tha the provided function is in fact a view function (by checking
   its meta-data)."
  [delegate env view-fn]
  (if (and view-fn (= (^view-fn :cascade-type) :view))
    (delegate env view-fn)
    false))
       
(decorate handle-view-request verify-is-view-fn)    

(defn default-handle-action
  "Default dispatcher to an action function, responsible for passing the env to the
   function, and for interpreting the return value."
  [env action-fn]
  (debug "Invoking action function %s" (qualified-function-name action-fn))
  (let [result (action-fn env)]
    (cond
      ;;  TODO: This should be an multimethod for extensibility
      (true? result) true
      (function? result) (render-view env result)
      :otherwise (fail "Unexpected response value %s from %s." (ppstring result) (qualified-function-name action-fn)))))
  
(defn handle-action-request
  [env action-fn]
  (default-handle-action env action-fn))

(defn verify-is-action-fn
  [delegate env action-fn]
  (if (and action-fn (= (^action-fn :cascade-type) :action))
    (delegate env action-fn)
    false))    

(decorate handle-action-request verify-is-action-fn)     

(defn dispatch-named-function-to-pipeline
  "Expects the namespace name and the function name to be the 2nd and 3rd terms of the split path, uses this
   to identify a function, which is passed (with the env) to the request-handler function."
  [env request-handler]
  (let [split-path (-> env :cascade :split-path)
        [_ ns-name fn-name] split-path
        fn-namespace (and ns-name (find-ns (symbol ns-name)))
        named-function (and fn-namespace fn-name (ns-resolve fn-namespace (symbol fn-name)))
        new-env (assoc-in env [:cascade :extra-path] (drop 3 split-path))]
    (request-handler new-env named-function)))

(defn named-view-dispatcher 
  "Mapped to /view, this attempts to identify a namespace and a view function
  which is then invoked to render a DOM which is then streamed to the client."
  [env]
  (dispatch-named-function-to-pipeline env handle-view-request))
  
(defn named-action-dispatcher
  "Mapped to /action, attempts to identify a namespace and an action function,
  which is then invoked. The action may render a response directly (and return true),
  or it may return a rendering hint. Rendering hints are view functions (to render that view)
  or other values as yet unspecified."
  [env]
  (dispatch-named-function-to-pipeline env handle-action-request))

(defn invoke-mapped-function
  "Called from path-dispatcher to try one of a series of functions that may match the request path."
  [env request-path [path function]]
  (let [function-meta ^function
        type (function-meta :cascade-type)
        handler-fn (read-config [:type-to-handler type])
        new-env (assoc-in env [:cascade :extra-path] (drop (count path) request-path))]
    (fail-if (nil? handler-fn) "Function %s defines a :cascade-type of %s which is not supported."
      (qualified-function-name function) type)    
    (handler-fn new-env function)))
        
(defn path-dispatcher
  "Dispatches to a matching view or action function by looking for a match against the :mapped-functions configuration."
  [env]
  (let [split-path (-> env :cascade :split-path)
        matches (find-matching-functions :mapped-functions split-path)]
        ;; Invoke each matching function until one returns true
      (first (filter true? (map #(invoke-mapped-function env split-path %) matches)))))
        
(add-function-to-config :dispatchers "view" #'named-view-dispatcher)
(add-function-to-config :dispatchers "action" #'named-action-dispatcher)
(add-function-to-config :dispatchers "" #'path-dispatcher)

(assoc-in-config [:type-to-handler :view] #'render-view)    
(assoc-in-config [:type-to-handler :action] #'default-handle-action)    

