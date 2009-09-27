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
  (:use (cascade config dom logging path-map pipeline fail func-utils)
        (cascade.internal utils)))

(create-pipeline :render-view-as-xml
  (fn [env view-fn]
    (debug "Rendering view function %s as XML" (qualified-function-name view-fn))
    (let [#^ServletResponse response (-> env :servlet-api :response)
          dom (view-fn env)]
      (with-open [writer (.getWriter response)]
        (render-xml dom writer)))
    true))

(create-pipeline :render-view
  (fn [env view-fn]
    ;; TODO: Eventually we may have a render as HTML pipeline based on view function meta-data.
    (call-pipeline :render-view-as-xml env view-fn)))

(create-pipeline :view
  (fn [env view-fn]
    (call-pipeline :render-view env view-fn)))

(deffilter :is-view-fn
  [delegate env view-fn]
  (if (and view-fn (= (^view-fn :cascade-type) :view))
    (delegate env view-fn)
    false))

(create-pipeline :default-handle-action
  (fn [env action-fn]
    (debug "Invoking action function %s" (qualified-function-name action-fn))
    (let [result (action-fn env)]
      (cond
        ;;  TODO: This should be an multimethod for extensibility
        (true? result) true
        (function? result) (call-pipeline :view result)
        :otherwise (fail "Unexpected response value %s from %s." (ppstring result) (qualified-function-name action-fn))))))

(create-pipeline :action
  (fn [env action-fn]
    (call-pipeline :default-handle-action env action-fn)))

(deffilter :is-action-fn
  [delegate env action-fn]
  (if (and action-fn (= (^action-fn :cascade-type) :action))
    (delegate env action-fn)
    false))    

(defn dispatch-named-function-to-pipeline
  [env pipeline]
  (let [split-path (-> env :cascade :split-path)
        [_ ns-name fn-name] split-path
        fn-namespace (and ns-name (find-ns (symbol ns-name)))
        named-function (and fn-namespace fn-name (ns-resolve fn-namespace (symbol fn-name)))
        new-env (assoc-in env [:cascade :extra-path] (drop 3 split-path))]
    (call-pipeline pipeline new-env named-function)))

(defn named-view-dispatcher 
  "Mapped to /view, this attempts to identify a namespace and a view function
  which is then invoked to render a DOM which is then streamed to the client."
  [env]
  (dispatch-named-function-to-pipeline env :view))
  
(defn named-action-dispatcher
  "Mapped to /action, attempts to identify a namespace and an action function,
  which is then invoked. The action may render a response directly (and return true),
  or it may return a rendering hint. Rendering hints are view functions (to render that view)
  or other values as yet unspecified."
  [env]
  (dispatch-named-function-to-pipeline env :action))

(defn invoke-mapped-function
  "Called from path-dispatcher to try one of a series of functions that may match the request path."
  [env request-path [path function]]
  (let [function-meta ^function
        type (function-meta :cascade-type)
        pipeline (read-config :type-to-pipeline type)
        new-env (assoc-in env [:cascade :extra-path] (drop (count path) request-path))]
    (fail-if (nil? pipeline) "Function %s defines a :cascade-type of %s which is not supported."
      (qualified-function-name function) type)    
    (call-pipeline pipeline new-env function)))
        
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

(assoc-in-config [:type-to-pipeline :view] :render-view)    
(assoc-in-config [:type-to-pipeline :action] :default-handle-action)    

(assoc-in-config [:filters :view] :is-view-fn)
(assoc-in-config [:filters :action] :is-action-fn)
