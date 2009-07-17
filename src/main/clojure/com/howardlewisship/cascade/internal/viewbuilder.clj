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

(defn to-dom-node-seq
  [any]
  "Converts the result of a render function to a seq as needed."
  (cond
    (nil? any) nil

    ; TODO: But what if they want to return an vector of strings? Perhaps we should map
    ; any through to-dom-node-seq.
    (sequential? any) any

    ; A map is assumed to be a DOM node, wrap it in a vector
    (map? any) [any]
    
    (string? any) [(struct-map dom-node :type :text :value any)]
    
    true (throw (RuntimeException. 
      (format "A rendering function returned %s. Rendering functions should return nil, a string, a seq of DOM nodes, or a single DOM node."
    (pr-str any))))))
     
(defn combine
  "Given the results of rendering (where each step provides a render result), combine the results
into a single sequence of DOM nodes."
  [& render-results]
  ; TODO: a bit of busy work here, to wrap nodes into a vector just so we can combine them into a overall list.
  ; perhaps we should all to-dom-node-seq to leave maps bare and we write a reduce here to combine it all together, rather
  ; than use apply concat.
  (remove nil? (apply concat (map to-dom-node-seq render-results))))     

; Parse a seq of forms into a sequence of ??

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
  
  ; Accept a single form that will act as a rendering, returning a render
  ; result.
  (def parse-form
       (domonad [form match-form]
         form))     
      
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