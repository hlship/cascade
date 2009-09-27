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

(ns #^{:doc "Exception reporting"}
  cascade.exception
  (:import (java.lang Throwable StackTraceElement))
  (:use cascade (cascade logging pipeline)))
  
;; Identifies the properties of Throwable that are excluded from each exception-map's set of properties  
(def throwable-properties (keys (bean (Throwable.))))  
  
(defn transform-stack-frame
  [#^StackTraceElement element]
  ;; TODO: some regexp matching to identify Clojure stack frames and transform them into a better format.
  ;; Also, mark a lot of the clojure's API classes as "uninteresting".
  { :method-name (.toString element) })  
  
(defn transform-stack-trace
  "Transforms a primitive array of StackTraceElements into individual maps; each with two keys:
  :frame is a string representing the method name and location,
  :class is a string (may be nil) used when rendering the frame (values to be defined)."
  [elements]
  ;; Currently very simple, but will expand in the future, when we convert Java stack frames
  ;; for Clojure code into clojure names, and use the CSS class to hide uninteresting
  ;; stack frames, and highlight application stack frames.
  (map transform-stack-frame (seq elements)))  
  
(defn expand-exception-stack
    "Expands a simple exception into a seq of exception maps, representing the stack of exceptions (the first or outer
    exceptions wrap the later, inner, deeper exceptions). Each map has keys :class-name, :message, :stack-trace and :properties.
    :class is the name of the exception class, :message is the message associated with the exception,
    :stack-trace is a via transform-stack-trace (and will only be present in the last, or deepest, exception map),
    and :properties is a map of additional JavaBean properties of the map that should be presented to the user. The values
    of the :properties map are Java objects, not necesarilly strings, and will need further transformation to be
    presented."
    [#^Throwable thrown-exception]
    (loop [#^Throwable current thrown-exception
           stack []]
      (let [bean-properties (bean current)
            next-exception (.getCause current)
            is-deepest (nil? next-exception)
            exception-map { :class-name (.. current getClass getName)
                            :message (.getMessage current)
                            :properties (apply dissoc bean-properties throwable-properties) }]
        (if is-deepest
          (conj stack (assoc exception-map :stack-trace (transform-stack-trace (.getStackTrace current))))
          (recur next-exception (conj stack exception-map))))))
          
(def exception-banner "An unexpected exception has occured.")
    
(defn render-exception-map
  "Renders an individual exception map. "
  [{:keys [class-name message properties stack-trace]}]
  (template 
    :span { :class "ccd-exception-class-name" } [ class-name ]
    (if-not (nil? message)
      (template :span { :class "ccd-exception-message" } [ message ]))
    (when-not (and (nil? properties) (nil? stack-trace))
      (template 
        :dl [
          (template-for [k (sort (keys properties))]
            :dt [ (name k)]
            ;; TODO: Expand here to do more than just .toString!
            :dd [ (.toString (get properties k)) ])
          (when-not (nil? stack-trace)
            (template
              :dt [ "Stack Trace" ]
              :ul { :class "ccd-stack-trace" } [
                (template-for [frame stack-trace]
                  :li [ (frame :method-name) ])
              ]))
        ]))))

(defview exception-report
  "The default exception report view. The top-most thrown exception is expected in the [:cascade :exception] key of the environment.
  Formats a detailed HTML report of the exception and the overall environment."
  ;; TODO: link in a CSS file
  [env]
  :html [
    :head [ :title exception-banner ]
    :body [
      :h1 [ exception-banner ]
      :ul [
        (template-for [m (expand-exception-stack (-> env :cascade :exception))]
          :li [ (render-exception-map m) ])
      ]
      ;; TODO request details, session details? etc.
    ]
  ])
  
(create-pipeline :request-exception
  (fn [env exception]
    (debug "Request exception: %s" exception)
    (call-pipeline :render-view (assoc-in env [:cascade :exception] exception) #'exception-report)))
            