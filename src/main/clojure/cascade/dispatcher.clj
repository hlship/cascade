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
    (cascade asset config dom logging path-map pipeline fail func-utils utils)
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
        :script { :type "text/javascript" :src path }))))

(defn inject-script-block
  [env dom-nodes immediate onready]
  (extend-dom dom-nodes [:html :body] :bottom
    (template
      ; this could be done more efficiently
      :script {:type "text/javascript"} [
        (for [line immediate]
          [line linebreak])
        (when-not (empty? onready)
          (import-jquery env)
          (interpose "\n"
            (concat [ "jQuery(function() {" ]
              onready
              [ "});"])))            
      ])))

(defn add-script-block-for-initialization
  [env dom-nodes]
  (let [aggregation (-> env :cascade :resource-aggregation)
        immediate (@aggregation :immediate)
        onready (@aggregation :onready)]
    (if (and (empty? immediate) (empty? onready))
      dom-nodes
      (inject-script-block env dom-nodes immediate onready))))
            
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

(defn do-render
  [env view-fn markup-type content-type markup-renderer-fn]
  (debug "Rendering view function %s" (qualified-function-name view-fn))
  (let [#^ServletResponse response (-> env :servlet-api :response)
        ; Invoke the function, getting back a lazy seq of rendered nodes
        dom (view-fn env)
        prepared (prepare-dom-for-render env dom)]
    (debug "Streaming %s response" markup-type)
    (.setContentType response content-type)          
    (with-open [writer (.getWriter response)]      
      (markup-renderer-fn prepared writer)))
  true)
  
(defn render-view-as-xml
  "Renders the provided view function as an XML stream. Returns true."
  [env view-fn]
  (do-render env view-fn "XML" "text/html" render-xml))
  
(defn render-view-as-html
  "Renders the provided view function as an HTML markup stream. Returns true."
  [env view-fn]
  (do-render env view-fn "HTML" "text/html" render-html))

(assoc-in-config [:view-renderer :html] #'render-view-as-html)
(assoc-in-config [:view-renderer :xml] #'render-view-as-xml)

(defn render-view
  "Renders a views a view. The function's :view-renderer meta key is used to select
  the style of rendering. This should be either :html or :xml and defaults to :html."
  [env view-fn]
  (let [render-type (or ((meta view-fn) :view-renderer) :html)
        view-renderer (read-config [:view-renderer render-type])
        new-env (assoc-in env [:cascade :resource-aggregation] (atom {}))]
    (view-renderer new-env view-fn)))

(defn handle-view-request
  "A pipeline that calls render-view."  
  [env view-fn]
  (render-view env view-fn))

(defn verify-is-view-fn
  "A filter that verifies that the provided function is in fact a view function (by checking
   its meta-data)."
  [delegate env view-fn]
  (if (and view-fn (= ((meta view-fn) :cascade-type) :view))
    (delegate env view-fn)
    false))
       
(decorate handle-view-request verify-is-view-fn)    

(defn default-handle-action
  "Default dispatcher to an action function, responsible for passing the env to the
   function, and for interpreting the return value. Actions may return a boolean: true
   means that the action already provided a response, nil or false indicates that the servlet
   container should provide a default response. Actions may also return a function, which
   must be a view function, which will be rendered. Other return values are an error."
  [env action-fn]
  (debug "Invoking action function %s" (qualified-function-name action-fn))
  (let [result (action-fn env)]
    (cond      
      (boolean? result) result
      (nil? result) false
      (function? result) (render-view env result)
      :otherwise (fail "Unexpected response value %s from %s." (ppstring result) (qualified-function-name action-fn)))))
  
(defn handle-action-request
  [env action-fn]
  (default-handle-action env action-fn))

(defn verify-is-action-fn
  [delegate env action-fn]
  (if (and action-fn (= ((meta action-fn) :cascade-type) :action))
    (delegate env action-fn)
    false))    

(decorate handle-action-request verify-is-action-fn)     

(defn invoke-mapped-function
  "Called from path-dispatcher to try one of a series of functions that may match the request path."
  [env request-path [path function]]
  (let [function-meta ^function
        type (function-meta :cascade-type)
        handler-fn (find-config [:type-to-handler type])
        new-env (assoc-in env [:cascade :extra-path] (drop (count path) request-path))]
    (fail-if (nil? handler-fn) "Function %s defines a :cascade-type of %s which is not supported."
      (qualified-function-name function) type)    
    (handler-fn new-env function)))
        
(defn mapped-function-dispatcher
  "Dispatches to a matching view or action function by looking for a match against the :mapped-functions configuration.
  The extra level of mapping allows for the [:cascade :extra-path] env key to be set before invoking
  the view or action function."
  [env]
  (let [split-path (-> env :cascade :split-path)
        matches (find-matching-functions :mapped-functions split-path)]
        ;; Invoke each matching function until one returns true
      (first (filter true? (map #(invoke-mapped-function env split-path %) matches)))))
        
(add-function-to-config :dispatchers "" #'mapped-function-dispatcher)

(assoc-in-config [:type-to-handler :view] #'render-view)    
(assoc-in-config [:type-to-handler :action] #'default-handle-action)    

