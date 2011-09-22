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
  (:require
    [clojure.string :as str])
  (:use
    [clojure [pprint :only (pprint)]]
    [clojure.algo monads]
    [cascade dom fail]
    [cascade.internal parser]))

(defn convert-render-result
  [any]
  "Checks the result of invoking a rendering function (or evaluating a symbol), to ensure that only
   acceptible values are returned."
  (cond
    (dom-node? any) any
    (string? any) (text-node any)
    (number? any) (raw-node (str any))
    true (throw (RuntimeException.
    (format "A rendering function returned %s. Rendering functions should return nil, a string, a number, a DOM node or a seq of such values."
      (pr-str any))))))

(defn explode-element-name
  "Explodes an element name keyword into a seq of three-element vectors. Each vector
  consists of the portion of the name prior to the match, the match character, and
  the match term. :div.alpha#beta would split to [[\"div\" \".\" \"alpha\"] [\"div.alpha\" \"#\" \"beta\"]]."
  [element-name]
  (let [name-str (name element-name)]
    ; match sequences of word characters prefixed with '.' or '#' within the overall name
    (loop [matcher (re-matcher #"([.#])(\w+)" name-str)
           result []]
      (if (.find matcher)
        (recur matcher (conj result [(.substring name-str 0 (.start matcher))
                                     (.group matcher 1)
                                     (.group matcher 2)]))
        result))))

(defn extract-attributes
  [exploded match key attributes]
  (let [matches (map #(nth % 2) (filter #(= match (nth % 1)) exploded))
        c (count matches)]
    (cond
      (= c 0) attributes
      (= c 1) (assoc attributes key (keyword (first matches)))
      :else (assoc attributes key (str/join " " matches)))))

(defn factor-element-name
  "Factors a keyword representing an element name into a simple element name keyword and a
  map of up to two attributes: :id and :class. The complex element name includes the implicit id and class(es)
  using CSS-inspired naming; :p.important.top-level#status would factor to
  [:p { :id :status :class \"important top-level\" } ]. The value for :class will be
  a keyword if there is only a single value. The attributes map may be nil if the
  element-name is simple."
  [element-name]
  (let [exploded (explode-element-name element-name)]
    (if (empty? exploded)
      [element-name nil]
      (let [simple-element-name (keyword (get-in exploded [0 0]))
            extract (partial extract-attributes exploded)
            attributes (->> {} (extract "." :class) (extract "#" :id))]
        [simple-element-name attributes]))))

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

  (def parse-entity
    (domonad [entity-name match-keyword :when (.startsWith (name entity-name) "&")]
      `(raw-node ~(str (name entity-name) ";"))))

  (def parse-element
    (domonad [name parse-name
              attributes (optional match-map)
              body (optional parse-body)]
      (let [[factored-element-name implicit-attributes] (factor-element-name name)
            assembled-attributes (merge implicit-attributes attributes)]
        `(element-node ~factored-element-name ~assembled-attributes ~body))))

  ; Accept a single form that will act as a renderer, returning a render
  ; result, which will be combined with other render results via the
  ; parse-forms parser. This form may be a symbol or a list (a function call).
  (def parse-form
    (domonad [form match-form]
      form))

  (def parse-single-form
    (match-first parse-text parse-entity parse-element parse-form))

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