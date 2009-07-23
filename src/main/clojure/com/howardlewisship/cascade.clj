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

(ns com.howardlewisship.cascade
  (:use
    (com.howardlewisship.cascade config)
    (com.howardlewisship.cascade.internal utils viewbuilder)))
  
(defmacro inline
  "Defines a block of template that renders inline."
  [& template]
  (parse-embedded-template template))
    
(defmacro defview
  "Defines a Cascade view function, which uses an embedded template."
  [fn-name fn-params & template]
  ; TODO: support for a doc string; I guess you can do it
  (fail-unless (vector? fn-params) "Must provide parameters (as with any function).")
  (fail-unless (not (empty? fn-params)) "At least one parameter (for the environment) is required when defining a view function.")
  `(defn ~fn-name {:cascade-type :view} ~fn-params (inline ~@template)))

(defmacro block
  "Defines a block of template that renders with with an environment controlled by its container. The result is a function
that takes a single parameter (the env map)."
  [fn-params & template]
  (fail-unless (and (vector? fn-params) (= 1 (count fn-params))) "Blocks require that exactly one parameter be defined.")
    `(fn ~fn-params (inline ~@template)))

(def linebreak
  (text-node "\r"))
  
(defmacro defchain
  "Defines a function as part of a chain. Chain functions take a single parameter. The name parameter is a keyword
  added to the :chains configuration map (this is to encourage chains to be composable by keyword)."
  [name fn-params & forms]
  (fail-unless (keyword? name) "A chain is identified by a keyword, not a symbol.")
  (fail-unless (and (vector? fn-params) (= 1 (count fn-params))) "Chain definitions require exactly one parameter be defined.")
  `(assoc-in-config [:chains ~name] (fn ~fn-params ~@forms)))

