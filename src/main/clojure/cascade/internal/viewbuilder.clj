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

(ns #^{:doc "Form parser for the template DSL"}
  cascade.internal.viewbuilder
  (:use (clojure.contrib monads [pprint :only (pprint)])
        (cascade dom fail view-cache)
        (cascade.internal utils parser)))

(def uidgen (atom 0))

(defn mark-as-invariant
  "Marks a collection object as invariant (setting an invariance level of 1)."
  [obj]
  (vary-meta obj assoc :invariant-level 1))

; TODO: Turn this into a multimethod to make it more extensible.

(defn convert-render-result
  [any]
  "Checks the result of invoking a rending function or closure, to ensure that only
   acceptible values are returned."
  (cond
    ; A map is assumed to be a DOM node
    (map? any) (if (:cascade-dom-node ^any) any (fail "Not a DOM node: %s" (ppstring any)))
    (string? any) (text-node any)
    (number? any) (text-node (str any))
    true (throw (RuntimeException.
      (format "A rendering function returned %s. Rendering functions should return nil, a string, a seq of DOM nodes, or a single DOM node."
    (pr-str any))))))

(defn combine
  "Given the results of rendering (where each step provides a render result), combine the results
into a single sequence of DOM nodes. Each of the render results should be a DOM node,
or a collection of DOM nodes (or a nested collection of DOM nodes, etc.). Strings are also allowed,
which are converted into :text DOM nodes."
  [& render-results]
  (loop [output []
         queue render-results]
    (let [current (first queue)
          remainder (next queue)]
      (cond
        (nil? current) (if (empty? remainder) output (recur output remainder))
        (sequential? current) (recur output (concat current remainder))
        :otherwise (recur (conj output (convert-render-result current)) remainder)))))

(defn cache-invariant-form
  "Returns the form as is, unless it has meta key :invariant-level, in which case the form is wrapped using
   just-in-time."
  [form]
  (if-not (nil? (:invariant-level (meta form)))
    (let [key (swap! uidgen inc)]
      `(read-view-cache ~key  ~form))
    form))                      

(defn invoke-combine
  "Called to combine some forms (Clojure code forms generated from template forms) together into a call to combine.
  At a transition point (where some, but not all, of the forms yield invariant results), then the
  invariant forms will be wrapped with cache-invariant-form."
  [forms]
  (let [form-metas (map meta forms)
        invariant-levels (map :invariant-level form-metas)
        all-invariant? (not-any? nil? invariant-levels)
        max-invariant (and all-invariant? (apply max invariant-levels))
        wrapped-forms (map cache-invariant-form forms)]
    (if all-invariant?
      (vary-meta `(combine ~@forms) assoc :invariant-level (inc max-invariant))
      `(combine ~@wrapped-forms))))
    
(with-monad parser-m
  (declare parse-embedded-template)

  (def parse-text
    (domonad [text match-string]
      (mark-as-invariant `(text-node ~text))))

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
  ; parse-forms parser.
  (def parse-form
       (domonad [form match-form]
         form))

  (def parse-single-form
    (match-first parse-text parse-element parse-form))

  (def parse-forms
    (domonad [forms (none-or-more parse-single-form)]
      (invoke-combine forms)))
) ; with-monad parser-m

(defn parse-embedded-template
  "Used as part of the defview or template macros to convert the forms, the embedded template, into
a new list of forms that constructs the structure layed out by the template."
  [forms]
  (run-parse parse-forms forms "embedded template forms"))