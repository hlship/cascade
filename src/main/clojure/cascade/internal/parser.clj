; Copyright 2009 Howard M. Lewis Ship
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
  #^{:doc "Parser monad used to parse Clojure forms into higher-level structures."}
  cascade.internal.parser
  (:use clojure.contrib.monads
        (cascade fail)
        (cascade.internal utils)))

(def parser-m (state-t maybe-m))

(defn any-form
  "Fundamental parser action; returns [first, rest] if forms is not empty, nil otherse."
  [forms]
  (if (empty? forms)
    nil
    ; "Consume" the form
    (list (first forms) (rest forms))))

(with-monad parser-m

  (defn optional 
    [parser]
    (m-plus parser (m-result nil)))

  (declare one-or-more)

  (defn none-or-more [parser]
    (optional (one-or-more parser)))

  (defn one-or-more [parser]
    (domonad [a parser
              as (none-or-more parser)]
      (cons a as)))

  (def match-first m-plus)

  (defn form-test
     "Parser factory using a predicate. When a form matches the predicate, it becomes the new result."
     [pred]
     (domonad
       [form any-form :when (pred form)]
       ; And return the matched form
       form))

  (def match-keyword
    (form-test keyword?))  

  (def match-string
    (form-test string?))    

  (def match-map
    (form-test map?))

  (def match-vector
    (form-test vector?))

  (def match-list
    (form-test list?))

  (def match-symbol
    (form-test symbol?))

  (def match-form
    (match-first match-list match-symbol))

)  ;  with-monad parser-m

(defn run-parse
  "Executes a parse given a parser function and a set of forms to parse. The construct-name is used for reporting errors:
  either a nil monadic result, or an incomplete parse (that leaves some forms unparsed)."
  [parser forms construct-name]
  (let [monadic-result (parser forms)]
    
    (when (nil? monadic-result)
      (fail "Parse of %s completed with no result." construct-name))
      
    (let [[result remaining-forms] monadic-result]
      
      (when-not (empty? remaining-forms)
        (fail "Incomplete parse of %s, %s forms remain, starting with %s."
          (count remaining-forms)
          (first remaining-forms)))

      result)))