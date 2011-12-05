;;; Copyright 2009, 2010, 2011 Howard M. Lewis Ship
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");;;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;;; implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(ns
    cascade.internal.viewbuilder
  "Form parser for the markup DSL"
  (:require
   [clojure.string :as str])
  (:use
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
   :else (fail
          "A rendering function returned %s. Rendering functions should return nil, a string, a number, a DOM node or a seq of such values."
          (pr-str any))))

(defn explode-element-name
     "Explodes an element name string into a seq of three-element vectors. Each vector
consists of the portion of the name prior to the match, the match character, and
the match term. \"div.alpha#beta\" would split to [[\"div\" \".\" \"alpha\"] [\"div.alpha\" \"#\" \"beta\"]]."
     [^String element-name]
                                        ; match sequences of word characters prefixed with '.' or '#' within the overall name
     (loop [matcher (re-matcher #"([.#])([\w-]+)" element-name)
            result []]
       (if (.find matcher)
         (recur matcher (conj result [(.substring element-name 0 (.start matcher))
                                      (.group matcher 1)
                                      (.group matcher 2)]))
         result)))

(defn extract-attributes
  [exploded match key attributes]
  (let [matches (map #(nth % 2) (filter #(= match (nth % 1)) exploded))
        c (count matches)]
    (cond
     (= c 0) attributes
     (= c 1) (assoc attributes key (keyword (first matches)))
     :else (assoc attributes key (str/join " " matches)))))

(defn factor-element-name
  "Factors a string representing an element name into a simple element name keyword and a
map of up to two attributes: :id and :class. The complex element name includes the implicit id and class(es)
using CSS-inspired naming; \"p.important.top-level#status\" would factor to
[:p { :id :status :class \"important top-level\" } ]. The value for :class or :id will be
a keyword if there is only a single value, or joined as a string if there are multiple values.
The attributes map may be nil if the element-name is simple."
  [element-name]
  (let [exploded (explode-element-name element-name)]
    (if (empty? exploded)
      [(keyword element-name) nil]
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

(defn split-string
  [^String string split]
  (seq (.split string split)))

(defn- combine-body [forms]
  (if (empty? forms)
    nil
    `(combine ~@forms)))


(defn do-text
  [text output-forms]
  ; The encode-string occurs just once, at macro expansion time
  (conj output-forms `(raw-node ~(encode-string text))))


(defn is-entity? [form]
  (and (keyword? form) (.startsWith (name form) "&")))

(defn do-entity [keyword output-forms]
  (let [entity-name (str (name keyword) ";")]
    (conj output-forms `(raw-node ~entity-name))))

(declare parse-forms)

(defn do-element
  "Processes a vector form, which represents an element (a leading keyword), an optional map of attributes, then
nested template elements to form the body of the element. The element keyword may be structured, implying implicit structure
and attributes. Recursively invokes (parse-forms) to construct the body."
  [form output-forms]
  (let [element-selector (first form)
        element-body (rest form)
        [attributes element-body] (maybe-consume map? element-body)
        expanded-body (parse-forms [] element-body)]
    (loop [element-names (reverse (split-string (name element-selector) ">"))
           active-attributes attributes
           active-body  (combine-body expanded-body)
           wrap-body false]
      (if (nil? element-names)
        ; Add the fully expanded form to the output forms and done.
        (conj output-forms active-body)
        (let [element-name (first element-names)
              [factored-element-name implicit-attributes] (factor-element-name (name element-name))
              assembled-attributes (merge implicit-attributes active-attributes)
              new-body (if wrap-body `[~active-body] active-body)]
          (recur
           (next element-names)
           nil
           `(element-node ~factored-element-name ~assembled-attributes ~new-body) true))))))

(defn form?
  "True if the form is a list (e.g., a function call) or a symbol."
  [form]
  (or (list? form) (symbol? form)))

(defn do-form
  "Passes a form (a list or a symbol) through unchanged."
  [form output-forms]
  (conj output-forms form))

(defn parse-forms  [output-forms input-forms]
  (let [first-form (first input-forms)
        remaining-forms (next input-forms)]
    (cond
     (empty? input-forms) output-forms
     (string? first-form) (recur (do-text first-form output-forms) remaining-forms)
     (is-entity? first-form) (recur (do-entity first-form output-forms) remaining-forms)
     (vector? first-form) (recur (do-element first-form output-forms) remaining-forms)
     (form? first-form) (recur (do-form first-form output-forms) remaining-forms)
     :else (fail "Unexpected input while parsing template forms: %s" (str first-form)))))

(defn parse-markup
  "Used as part of (defview) or (template) to convert the forms, the embedded template, into
a new list of forms that constructs the structure layed out by the template."
  [forms]
  (parse-forms [] forms))