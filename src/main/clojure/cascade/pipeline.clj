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

(ns #^{:doc "Macros used to define filters and chains for extending Cascade."}
  cascade.pipeline
  (:use
    (cascade config)
    (cascade.internal utils)))
  
;; TODO: get away from defining chains in terms of a keyword, use a symbol  
  
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

