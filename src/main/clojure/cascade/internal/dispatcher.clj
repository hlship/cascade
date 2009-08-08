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

; Contains the basic dispatchers

(ns cascade.internal.dispatcher
  (:import (javax.servlet ServletResponse))
  (:use cascade
        (cascade config dom logging)
        (cascade.internal utils)))

(def #^{:doc "Renders a view function as an XML stream."} 
  xml-render-pipeline
  (create-pipeline :render-view-xml
    (fn [env view-fn]
      (debug "Rendering view function %s as XML" (qualified-function-name view-fn))
      (let [#^ServletResponse response (-> env :servlet-api :response)
            dom (view-fn env)]
        (with-open [writer (.getWriter response)]
          (render-xml dom writer)))
      true)))

(def #^{:doc "Invokes an action function."}
  action-request-pipeline
  (create-pipeline :action-pipeline
    (fn [env action-fn]
      (debug "Invoking action function %s" (qualified-function-name action-fn))
      (let [result (action-fn env)]
        (cond
          (true? result) true
          (nil? result) (fail "Action function %s returned nil." (qualified-function-name action-fn))
          ; TODO: Should fail if function returned but it can't render. Or should a function returned from
          ;  an action always be considered a rendering function?
          (function? result) (xml-render-pipeline result)
          :otherwise (fail "Unexpected response value %s from %s." (ppstring result) (qualified-function-name action-fn)))))))

(deffilter :is-view-fn
  [delegate env view-fn]
  (if (and view-fn (= (^view-fn :cascade-type) :view))
    (delegate env view-fn)
    false))

; Add :is-view-fn to the :render-view-xml pipeline.
(assoc-in-config [:pipelines :render-view-xml] [:is-view-fn])
    
(deffilter :is-action-fn
  [delegate env action-fn]
  (if (and action-fn (= (^action-fn :cascade-type) :action))
    (delegate env action-fn)
    false))    
    
(assoc-in-config [:pipelines :action-pipeline] [:is-action-fn])
    
(defn extract-fn-from-path
  "Examines the path in the environment to extract a namespace and function name which is resolved
  to a function (or nil if the value are not present as the 2nd and 3rd terms in the split path,
  or if the names can not be resolved)."
  [env]
  (let [split-path (-> env :cascade :split-path)
        [_ ns-name fn-name] split-path
        ns (and ns-name (find-ns (symbol ns-name)))]
     (and ns fn-name (ns-resolve ns (symbol fn-name)))))

(defn view-dispatcher 
  "Mapped to /view, this attempts to identify a namespace and a view function
  which is then invoked to render a DOM which is then streamed to the client."
  [env]
  ; TODO: different pipelines for XML vs. HTML, and a meta pipeline that
  ; chooses them.      
  (xml-render-pipeline env (extract-fn-from-path env)))

(assoc-in-config [:dispatchers "/view/"] view-dispatcher)  


(defn action-dispatcher
  "Mapped to /action, attempts to identify a namespace and an action function,
  which is then invoked. The action may render a response directly (and return true),
  or it may return a rendering hint. Rendering hints are view functions (to render that view)
  or other values as yet unspecified."
  [env]
  (action-request-pipeline (extract-fn-from-path env)))

(assoc-in-config [:dispatchers "/action/"] action-dispatcher)
