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

(ns com.howardlewisship.cascade.internal.viewbuilder
  (:use (clojure.contrib monads [pprint :only (pprint)])
        (com.howardlewisship.cascade dom)
        (com.howardlewisship.cascade.internal utils)))
(defn text-node
  [text]
  (struct-map dom-node :type :text :value text))

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

(defn convert-render-result
  [any]
  "Checks the result of invoking a rending function or closure, to ensure that only
   acceptible values are returned."
  (cond
    ; A map is assumed to be a DOM node
    (map? any) any
    
    (string? any) (struct-map dom-node :type :text :value any)
    
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

(def parser-m (state-t maybe-m))

(defn any-form
  "Fundamental parser action; returns [first, rest] if forms is not empty, nil otherse."
  [forms]
  (if (empty? forms)
    nil
    ; "Consume" the form
    (list (first forms) (rest forms))))
    
(with-monad parser-m

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
    
  (def match-form
    (form-test list?))
           
  (defn optional 
    [parser]
    (m-plus parser (m-result nil)))
    
  (declare one-or-more parse-embedded-template)
    
  (defn none-or-more [parser]
    (optional (one-or-more parser)))
    
  (defn one-or-more [parser]
    (domonad [a parser
              as (none-or-more parser)]
      (cons a as)))
    
  (def match-first m-plus)
                    
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
  ; result. Combine that result so that it can be combined with the other
  ; elements in the body with the form.
  (def parse-form
       (domonad [form match-form]
         `(combine ~form)))
      
  (def parse-single-form
    (match-first
      ; TODO: Parse a form
      parse-text parse-element parse-form))
              
  (def parse-forms
    (domonad [forms (none-or-more parse-single-form)]
      (if forms `(combine ~@forms)))) 
  
) ; with-monad parser-m     


(defn parse-embedded-template
  "Used as part of (defview) or (deffragment) to convert the a form, the embedded template, into
a new list of forms that constructs the structure layed out by the template."
  [forms]
  (let [monadic-result (parse-forms forms)]

      (when (nil? monadic-result)
        (pprint forms)
        (fail "Embedded template form parse completed with no result."))
        
    (let [[master-form remaining-forms] monadic-result]
      (when-not (empty? remaining-forms)
        (fail (format "Not all embedded template forms were parsed, %s remain, starting with %s."
          (count remaining-forms)
          (first remaining-forms))))
      master-form)))       