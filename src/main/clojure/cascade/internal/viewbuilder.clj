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
        (cascade dom)
        (cascade.internal utils parser)))

(defn map-to-attribute-nodes
  "Converts a map that defines the attributes for an element into a seq of
element DOM nodes."
  ; TODO: Would it be advantageous to do this conversion at macro evaluation time,
  ; rather than runtime?
  [attribute-map]
  (if attribute-map
    (for [[k v] attribute-map]
      (struct-map dom-node :type :attribute :name k :value v))))  
  
(defn element-node
  [name attributes content]
  (struct-map dom-node :type :element 
                       :name name
                       :attributes (map-to-attribute-nodes attributes)                        
                       :content content))  

; TODO: Turn this into a multimethod to make it more extensible.
(defn convert-render-result
  [any]
  "Checks the result of invoking a rending function or closure, to ensure that only
   acceptible values are returned."
  (cond
    ; A map is assumed to be a DOM node
    (map? any) any
    
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

(with-monad parser-m

  (declare parse-embedded-template)
  
  (def parse-text
    (domonad [text match-string]
      `(text-node ~text)))

  (def parse-name
    ; An attribute or element name is either a keyword or a form that yields a keyword.
    ; TODO: support for qualified names ([:prefix :name]).
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
      `(combine ~@forms))) 
  
) ; with-monad parser-m     

(defn parse-embedded-template
  "Used as part of (defview) or (deffragment) to convert the a form, the embedded template, into
a new list of forms that constructs the structure layed out by the template."
  [forms]
  (run-parse parse-forms forms "embedded template forms"))