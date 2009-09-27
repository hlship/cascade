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

(ns #^{:doc "Define filters and chains for extending Cascade"}
  cascade.pipeline
  (:use
    (cascade config fail logging)
    (cascade.internal utils)))
  
(defn create-pipeline
  "A pipeline is like a chain, but the individual functions (called filters) that
  are each passed an extra parameter (the first
  parameter) which is the next function in the pipeline. The final function in the pipeline is passed
  a bridge to an ordinary function, called the terminator. In this way, each function can control
  parameters, return values and exception behavior for functions further down the pipeline (in AOP terms, \"around advice\"). Returns a function
  with the same arity as the terminator. Nil return values from filter functions, or the terminator, have
  no special meaning."
  [selector terminator]
  (assoc-in-config [:pipelines selector]
    (fn [& params]
      (loop [bridge terminator
             queue (reverse (expand-function-list :filters selector))]
         (if (empty? queue)
           ; So, the outer function simply passes the params to the outermost bridge,
           ; whose arity should match the arity of the terminator. 
           ; The bridge may in fact be the terminator.
           (apply bridge params)

           ; Build a new bridge that invokes the current function passing the
           ; current bridge to it as the first parameter.
         
           (recur (fn [& args]
             (apply (first queue) bridge args)) (rest queue)))))))

(defn call-pipeline
  "Calls into a pipeline, identified by its selector (a keyword). The pipeline is expected to be found
  inside the :pipelines configuration map."
  [selector & args]
  (debug "Calling pipeline %s" selector)
  (apply (read-config :pipelines selector) args))  
  
(defmacro defchain
  "Defines a function as part of a chain. Chain functions take a single parameter. The name parameter is a keyword
  added to the :chains configuration map (this is to encourage chains to be composable by keyword)."
  [name fn-params & forms]
  (fail-unless (keyword? name) "A chain is identified by a keyword, not a symbol.")
  (fail-unless (vector? fn-params) "A chain function must define parameters like any other function.")
  `(assoc-in-config [:chains ~name] (fn ~fn-params ~@forms)))
  
(defmacro deffilter
  "Defines a filter function that can be used to assemble a pipeline. The name of the filter is a keyword (not
  a symbol). A filter function receives a delegate before its other parameters, it should invoke the delegate
  function, passing it appropriate parameters."
  [name fn-params & forms]
  (fail-unless (keyword? name) "A filter is identified by a keyword, not a symbol.")
  (fail-unless (vector? fn-params) "A filter function must define parameters like any other function.")
  (fail-unless (>= (count fn-params) 1) "A filter function must define at least one parameter (to recieve the delegate).")
  ; TODO: Is ":pipelines" the right name?  Should it be :filters or :pipeline-filters?  Oh, well.
  `(assoc-in-config [:filters ~name] (fn ~fn-params ~@forms)))

