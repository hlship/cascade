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
  (:use (com.howardlewisship.cascade.internal utils viewbuilder)))
  
(defmacro inline
  "Defines a block of template that renders inline."
  [& template]
  (parse-embedded-template template))
    
(defmacro defview
  "Defines a Cascade view function, which uses an embedded template."
  [fn-name fn-params & template]
  ; TODO: support for a doc string
  (fail-unless (vector? fn-params) "Must provide parameters (as with any function).")
  (fail-unless (not (empty? fn-params)) "At least one parameter (for the environment) is required when defining a view function.")
  ; TODO: add meta data
  `(defn ~fn-name ~fn-params (inline ~@template)))

(defmacro inline
  "Defines a block of template that renders inline."
  [& template]
  (parse-embedded-template template))
  
(defmacro block
  "Defines a block of template that renders with with an environment controlled by its container. The result is a function
that takes a single parameter (the env map)."
  [fn-params & template]
  (fail-unless (and (vector? fn-params) (= 1 (count fn-params))) "Blocks require that exactly one parameter be defined.")
    `(fn ~fn-params (inline ~@template)))

(def linebreak
  (text-node "\r"))
