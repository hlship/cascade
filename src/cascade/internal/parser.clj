; Copyright 2009, 2011 Howard M. Lewis Ship
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
; implied. See the License for the specific language governing permissions
; and limitations under the License.

(ns
  cascade.internal.parser
  "Parser utilities for converting Clojure forms into higher-level structures."
  (:use
    cascade.fail))

(defn maybe-consume
  "Evaluates a predicate against the first form. Returns a vector of two values. When the predicate matches, the return value is the first form and the rest of the forms.
when the predicate does not match, the return value is nil and the forms."
  [predfn forms]
  (let [first-form (first forms)]
    (if (predfn first-form)
      [first-form (rest forms)]
      [nil forms])))

(defn must-consume
  "Like maybe-consume, but the first form must pass the predicate function, or a failure occurs."
  [predfn forms description]
  (let [first-form (first forms)]
    (if (predfn first-form)
      [first-form (rest forms)]
      (fail "Unexpected input form when expecting %s: %s" description (str first-form)))))

(defn parse-function-def
  "Parses a flexible set of forms consisting of an optional documention string, an optional meta-data map, a
  required vector of parameters, and a series of additional forms. Returns a vector of the name (with additional meta
  data from the documentation string provided meta-data map), the parameters vector, and a seq of the additional forms."
  [fn-def-forms]
  (let [[fn-name remaining] (must-consume symbol? fn-def-forms "symbol to define function name")
        [doc-string remaining] (maybe-consume string? remaining)
        [fn-meta-data remaining] (maybe-consume map? remaining)
        [parameters fn-forms] (must-consume vector? remaining "vector of function parameters")]
    (let [doc-meta (and doc-string {:doc doc-string})
          full-meta (merge (meta fn-name) fn-meta-data doc-meta)
          symbol-with-meta (with-meta fn-name full-meta)]
      [symbol-with-meta parameters fn-forms])))
