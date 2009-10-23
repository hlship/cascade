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

(ns 
  #^{:doc "DOM node structure, rendering and manipulation"}
  cascade.dom
  (:import
  	(clojure.lang Keyword)
  	(java.io Writer))
  (:require 
  	(clojure [zip :as z])
  	(clojure.contrib [str-utils2 :as s2]))
  (:use 
  	(cascade fail utils logging)
 		cascade.internal.utils))

; TODO: Replace deep tail recursion with some kind of queue or visitor pattern.
; Need to be able to control close tags for empty elements (XML vs. HTML style)
; Need to be able to control quote character (single vs. double quote)
; Need to filter entities in :text nodes (can that be done earlier?)
; Who should URL encode attributes?

; TODO: For testing purposes (i.e., to get consistent results across JVMs, etc.)
; it may be necessary to sort attributes when rendering (but I'd rather not bother
; for production).

(defstruct dom-node
  :type ; :element, :attribute, :text, :raw
  ; TODO: :ns-prefix
  :name ; element tag (for :element)
  :attributes ; seq of attribute/value pairs (for :element)
  :value ; attribute value or literal text (text must be encoded), nested nodes for :element
)
  
(declare render-xml debug-dom)

(def char-to-entity-map 
	(merge 
		; Start with whitespace plus all ASCII printable
		(reduce #(assoc %1 (char %2) (char %2)) {}
			(concat [9 10 13] (range 32 126)))
		
		; Then merge in the characters that must be XML entities	
		{
			\< "&lt;"
			\> "&gt;"
			\& "&amp;"
		}))

(defn char-to-entity
	"Converts a single character to an entity, returning the entity or the original character."	
	[ch]
	(or
		(char-to-entity-map ch)
		(format "&#x%x;" (int ch))))
  
(defn encode-string
  "Encodes a string, replacing all non-ASCII characters with XML escape codes. In addition,
  unsafe characters are replaced with HTML entities."
 	[s]
 	; Could rewrite to more efficiently determine if out == s
 	(let [out (s2/map-str #(char-to-entity %) s)]
 		(if (= s out) s out)))
    
(defn text-node
	"Creates a text node from the string. The string is encoded when the node is constructed."
  [text]
  (struct-map dom-node :type :text :value (encode-string text)))
      
(defmulti to-attr-string
  "Converts an attribute value to a string. It is not necessary to apply quotes (those come at a later stage)."
  class)
  
(defmethod to-attr-string String
  [str-value]
  (encode-string str-value))
  
(defmethod to-attr-string Number
  [#^Number numeric-value]
  (.toString numeric-value))     

(defmethod to-attr-string Keyword
  [kw]
  (encode-string (name kw))) 
  
(defn- write
	"Write a number of strings to the writer."
  [#^Writer out & strings]
  (doseq [#^String s strings]
    (.write out s)))

(defmulti render-node-xml
  (fn [node & rest] 
    (node :type)))

(defmethod render-node-xml :text
  [node out]
  (write out (node :value)))

(defmethod render-node-xml :comment
  [comment-node out]
  (write out "<!--" (comment-node :value) "-->"))

(defmethod render-node-xml :element
  [element-node out]
  (let [element-name (name (element-node :name))
        content (element-node :value)]

    (write out "<" element-name)

    ; Write out normal attributes

    (doseq [[attr-name attr-value] (element-node :attributes)]
        ; TODO: Escape embedded quotes in the value.
        (if-not (nil? attr-value)
        	(write out " " (name attr-name) "=\"" (to-attr-string attr-value) "\"")))

    ; TODO: ugly gotchas about rendering HTML: can't always close an empty tag (i.e., <script>), etc.

    (if (empty? content)
      (write out "/>")
      (do
        (write out ">")

        (render-xml content out)

        (write out "</" element-name ">")))))

(defn render-xml
  "Renders a seq of DOM nodes representing a complete document (generally the list will include just
  a single root element node, but text and comments and the like may come into play as well)."
  [dom-nodes out]
  ; TODO: Render out the <?xml version="1.0"?> P.I.?
  (doseq [node dom-nodes]
    (render-node-xml node out)))

(defmulti render-node-debug
  (fn [node & rest] 
    (node :type)))
  	
(defmethod render-node-debug :text
	[node]
	(node :value))
	
(defmethod render-node-debug :comment
	[node]
	[:comment (node :value)])
	
(defmethod render-node-debug :element
	[node]
	(let [content (node :value)]
		[(node :name) (node :attributes) (if content (debug-dom content))]))		  	
  
(defn debug-dom
	"Renders DOM nodes to a format similar to the template DSL. Returns a seq."
	[dom-nodes]
	(for [node dom-nodes]
		(render-node-debug node)))
  
(defn element?
  "Returns true if the dom node is type :element."
  [node]
  (= :element (node :type)))    
    
(defn dom-zipper
	[root-node]
	(fail-unless (element? root-node) "Root node for dom-zipper must be an element node.")
	(z/zipper 
		element? ; can have children 
		:value ; children are (already) a seq in the :value key
		(fn [node children] (assoc node :value children)) 
		root-node))    
    
(defn navigate-dom-path
	"Starting from the root location (from dom-zipper) search for the first element child
	 that matches the first element in the path. The search continues looking for a child that
	 matches each successive keyword in the path. Returns the zipper loc for the final match or
	 nil if the path could not be resolved."
	[root-loc path]
	(fail-if (empty? path) "Must supply at least one keyword as the path.")
	(loop	[loc root-loc
				 target-element (first path)
				 remaining-path (rest path)]
		(lcond			
			(nil? loc) nil
			:let [n (z/node loc)]
			(and (element? n) (= (n :name) target-element))
				(if (empty? remaining-path)
					loc
					(recur (z/down loc) (first remaining-path) (rest remaining-path)))
			true (recur (z/right loc) target-element remaining-path))))

(defn update-dom
	"Updates the DOM using a DOM zipper, a position (:before, :after, :top, :bottom)
	and a seq of new nodes. Returns a DOM zipper loc from which the entire DOM tree can
	be retrieved."
	[loc position new-nodes]
	(cond
		(= position :before) (reduce z/insert-left loc new-nodes)
		(= position :top) (reduce z/insert-child loc (reverse new-nodes))
		(= position :after) (reduce z/insert-right loc (reverse new-nodes))
		(= position :bottom) (reduce z/append-child loc new-nodes)
		true (fail "Unexpected position: %s" position)))

(defn extend-root-element
	"Extends the DOM from the root DOM element, returning a new DOM.
	The path is a seq of keywords, used to walk down to a specifc element in
	the DOM. The new nodes (often via the template macro) will be inserted
	according to position. :before, :after, :top (left-most, first children)
	:bottom (right-most, last children). Returns the modified root DOM node, or 
	(if the path was unable to locate a specific element), the original root DOM node."
	[dom-node path position new-nodes]
	(lcond
		:let [root-loc (dom-zipper dom-node)
				  loc (navigate-dom-path root-loc path)]
		(nil? loc) dom-node
		true (z/root (update-dom loc position new-nodes))))
		
(defn extend-dom 
	"Extends a DOM (a set of root DOM nodes), adding new nodes at a specific position. Uses the path 
	 (a seq of keywords, representing elements) to locate an element within the DOM
	 then adds the new nodes at the position (position can be :top :bottom :before :after).
	 Assumes that the first element node in dom-nodes
	 is the root of the element tree (other nodes are possibly text or comments). 
	 Does nothing if the targetted node can't be found. Returns a new
	 sequence of dom nodes."
	[dom-nodes path position new-nodes]
	(loop [result []
		     queue dom-nodes]
     (lcond
     		; Never found an element node? Return the original nodes unchanged.
     		(empty? queue) dom-nodes      
     		
     		:let [node (first queue)]
     		
     		(= (node :type) :element) (concat result [(extend-root-element node path position new-nodes)] (rest queue))
     		
     		true (recur (conj result node) (rest queue))))) 
