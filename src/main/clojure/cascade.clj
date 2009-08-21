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

(ns cascade
  (:use
    (cascade path-map)
    (cascade.internal utils viewbuilder parse-functions)))
  
(defmacro inline
  "Defines a block of template that renders inline."
  [& template]
  (parse-embedded-template template))
    
(defmacro defview
  "Defines a Cascade view function, which uses an embedded template. A view function may have a doc string and meta data
  preceding the parameters vector. The function's forms are an implicit inline block."
  [& forms]
  (let [[fn-name fn-params template] (parse-function-def forms)
        full-meta (merge ^fn-name {:cascade-type :view})]
  `(add-mapped-function (defn ~fn-name ~full-meta ~fn-params (inline ~@template)))))

(defmacro block
  "Defines a block of template that renders with an environment controlled by its container. The result is a function
that takes a single parameter (the env map)."
  [fn-params & template]
  (fail-unless (and (vector? fn-params) (= 1 (count fn-params))) "Blocks require that exactly one parameter be defined.")
    `(fn ~fn-params (inline ~@template)))

(def #^{:doc "A DOM text node for a line break."}
  linebreak
  (text-node "\r"))
