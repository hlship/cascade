; Copyright 2009, 2010, 2011 Howard M. Lewis Ship
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
  cascade.internal.viewbuilder
  "Form parser for the template DSL"
  (:use 
    [clojure [pprint :only (pprint)]]
    [clojure.contrib monads]
    [cascade dom fail]
    [cascade.internal utils parser]))  

(defn convert-render-result
  [any]
  "Checks the result of invoking a rendering function (or evaluating a symbol), to ensure that only
   acceptible values are returned."
  (cond
    ; A map is assumed to be a DOM node
    (dom-node? any) any
    (string? any) (text-node any)
    (number? any) (raw-node (str any))
    true (throw (RuntimeException.
      (format "A rendering function returned %s. Rendering functions should return nil, a string, a seq of DOM nodes, or a single DOM node."
    (pr-str any))))))

(defn combine
  "Given the results of rendering (where each step provides a render result), combine the results
into a single sequence of DOM nodes. Each of the render results should be a DOM node,
or a collection of DOM nodes (or a nested collection of DOM nodes, etc.). Strings are also allowed,
which are converted into :text DOM nodes."
  [& render-results]
  (loop [output (transient [])
         queue render-results]
    (let [current (first queue)
          remainder (next queue)]
      (cond
        (nil? current) (if (empty? remainder) (persistent! output) (recur output remainder))
        (sequential? current) (recur output (concat current remainder))
        :otherwise (recur (conj! output (convert-render-result current)) remainder)))))

(with-monad parser-m
  (declare parse-embedded-template)

  (def parse-text
    (domonad [text match-string]
      ; The encode-string occurs just once, at macro expansion time
      `(raw-node ~(encode-string text))))

  (def parse-name
    ; An attribute or element name is either a keyword or a form that yields a keyword.
    (match-first match-keyword))

  (def parse-body
    (domonad [body match-vector]
      (parse-embedded-template body)))

  (def parse-element
    (domonad [name parse-name
              attributes (optional match-map)
              body (optional parse-body)]
       `(element-node ~name ~attributes ~body)))

  ; Accept a single form that will act as a renderer, returning a render
  ; result, which will be combined with other render results via the
  ; parse-forms parser. This form may be a symbol or a list (a function call).
  (def parse-form
       (domonad [form match-form]
         form))

  (def parse-single-form
    (match-first parse-text parse-element parse-form))

  (def parse-forms
    (domonad [forms (none-or-more parse-single-form)]
      ; Evaluation order means that the DOM tree is effectively constructed bottom-to-top
      ; (because combine is not lazy). 
      ; This may be be relevent in terms of ordering of CSS stylesheets & JavaScript libraries,
      ; since that information is "collected on the side" (in an atom) and therefore not
      ; purely functional. Perhaps there's a monadic approach that will allow the construction
      ; of the DOM tree and the collection of CSS/JS data to occur in a stricly functional way?
      `(combine ~@forms)))
) ; with-monad parser-m

(defn parse-embedded-template
  "Used as part of (defview) or (template) to convert the a form, the embedded template, into
a new list of forms that constructs the structure layed out by the template."
  [forms]
  (run-parse parse-forms forms "embedded template forms"))