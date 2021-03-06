;;; Copyright 2009, 2010, 2011 Howard M. Lewis Ship
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
;;; implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(ns
    cascade.dom
  "DOM node structure, rendering and manipulation"
  (:import
   (clojure.lang Keyword Sequential))
  (:require
   (clojure [zip :as z] [string :as str]))
  (:use
   (cascade fail utils)))

;;; TODO: For testing purposes (i.e., to get consistent results across JVMs, etc.)
;;; it may be necessary to sort attributes when rendering (but I'd rather not bother
;;; for production).  

(def char-to-entity-map
  (merge
                                        ; Start with whitespace plus all ASCII printable
   (reduce #(assoc %1 (char %2) (char %2)) {}
           (concat [9 10 13 32 33 \' \"] (range 35 38) (range 40 126)))
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
                                        ; Perhaps a regexp to see if encoding is necessary?
  (let [out (apply str (map #(char-to-entity %) (seq s)))]
    (if (= s out) s out)))


(defprotocol ToAttributeValueString
  "Converts an attribute value to a string. It is not necessary to apply quotes (those come at a later stage)."
                                        ; TODO control over quotes inside the value string; I think
  (to-attribute-value-string [value]
    "Converts the value to a string that can be safely streamed as an attribute value."))

(extend-protocol ToAttributeValueString
  String
  (to-attribute-value-string [string] (encode-string string))

  Number
  (to-attribute-value-string [num] (.toString num))

  Keyword
  (to-attribute-value-string [kw] (encode-string (name kw)))

  Sequential
  (to-attribute-value-string [coll]
    (str/join " " (map to-attribute-value-string coll))))

(defprotocol DOMSerializing
  "Defines how a tree of DOM Nodes can be recursively serialized as seq of streamable strings."
  (serialize
    [node strategy]
    "Converts an individual DOM node to a (lazy) seq of strings, safe to write into a response. The strategy encapsulates the differences between HTML and XML
    in terms of attribute quoting and handling of empty elements."))

(defn serialize-nodes
  [nodes strategy]
  (str/join (mapcat (fn [node] (serialize node strategy)) nodes)))

(defn create-serialize-attribute-pair
  "Returns a function that a key/value attribute pair into a seq of strings for rendering the attribute value. The key is expected a string or keyword. The value is a string,
  number, or keyword. If the value is nil, the returned function returns nil."
  [attr-quote]
  (fn [[attr-name attr-value]]
    (if (nil? attr-value)
      nil
      [" " (name attr-name) "=" attr-quote (to-attribute-value-string attr-value) attr-quote])))

(defn serialize-attribute-pairs
  [attributes strategy]
  (mapcat (:serialize-attribute-pair strategy) attributes))

(defn serialize-content-and-close [element-name content strategy]
  (if (empty? content)
    ((:write-empty-element-close strategy) element-name)
    (cons ">" (concat (serialize-nodes content strategy) ["</" element-name ">"]))))

(defrecord Element [name attributes content]

  DOMSerializing
  (serialize [node strategy]
                                        ; The field 'name' shadows clojure.core/name, so we have to be explicit
    (let [element-name (clojure.core/name name)
          preamble ["<" element-name]
          attribute-pairs (serialize-attribute-pairs attributes strategy)
          postamble (serialize-content-and-close element-name content strategy)]
      (concat preamble attribute-pairs postamble))))

;;; Static is used for statically defined portions of the DOM tree, such as literal strings and
                          ;;; comments.
(defrecord Static [string]

  DOMSerializing
  (serialize [node strategy] string))

(defrecord Comment [text]

  DOMSerializing
                                        ; TODO: seems a bit wasteful to create a new vector each time a Comment is asked to serialize itself.
  (serialize [node strategy]
    ["<!--" text "-->"]))

;;; Most outside code will use these standard constructor functions, rather than using the records' constructors.

(defn element-node
  [name attributes content]
  (->Element name attributes content))

(defn raw-node
  "Wraps a string as a Static DOM node, but does not do any encoding of the value."
  [s]
  (->Static s))

(defn text-node
  "Creates a Static DOM node from the string. The string is encoded when the node is constructed."
  [text]
  (raw-node (encode-string text)))

(defn comment-node
  "Creates a Comment DOM node from the string, providing the comment prefix and suffix. "
  [comment]
  (->Comment comment))

(def xml-strategy {
                   :serialize-attribute-pair (create-serialize-attribute-pair "\"")
                   :write-empty-element-close (constantly ["/>"])
                   })

(defn serialize-xml
  "Serializes a seq of DOM nodes representing a complete document into a lazy seq of strings. Generally the dom-nodes seq will include just
a single root element node, but text and comments and the like may come into play as well."
  [dom-nodes]
  (serialize dom-nodes xml-strategy))

(def html-must-close-elements #{"script"})

(defn html-empty-element-writer
  [element-name]
  (if (contains? html-must-close-elements element-name)
    ["></" element-name ">"]
    [">"]))

(def html-strategy {
                    :serialize-attribute-pair (create-serialize-attribute-pair "\"")
                    :write-empty-element-close html-empty-element-writer
                    })

(defn serialize-html
  "Serializes a seq of DOM nodes representing a complete document (as with serialize-xml) but with HTML
output semantics: attributes are still quoted, but tags may not be balanced (the end tag
may be ommitted if the element has no content). Result is a seq of strings that can be concatinated to provide the
full response."
  [nodes]
  (serialize-nodes nodes html-strategy))

(defn element?
  "Is the node a DOM Element?"
  [node]
  (instance? Element node))

(defn dom-node?
  "Is the object some kind of DOM node?"
  [node]
  (or (element? node)
      (instance? Static node)
      (instance? Comment node)))

(defn dom-zipper
  [root-node]
  (fail-unless (element? root-node) "Root node for dom-zipper must be an element node.")
  (z/zipper
   element? ; can have children
   :content ; children are (already) a seq in the :content key
   (fn [node children] (assoc node :content children))
   root-node))

(defn navigate-dom-path
  "Starting from the root location (from dom-zipper) search for the first element child
that matches the first element in the path. The search continues looking for a child that
matches each successive keyword in the path. Returns the zipper loc for the final match or
nil if the path could not be resolved."
  [root-loc path]
  (fail-if (empty? path) "Must supply at least one keyword as the path.")
  (loop [loc root-loc
         target-element (first path)
         remaining-path (rest path)]
    (lcond
     (nil? loc) nil
     :let [n (z/node loc)]
                                        ; if matching element?
     (and (element? n) (= (:name n) target-element))
                                        ; then either we've found it, or we need to go into the element and continue searching down.
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
   :else (fail "Unexpected position: %s" position)))

(defn extend-root-element
  "Extends the DOM from the root DOM element, returning a new DOM.
The path is a seq of keywords, used to walk down to a specifc element in
the DOM. The new nodes (often via the template macro) will be inserted
according to position. :before, :after, :top (left-most, first children)
:bottom (right-most, last children). Returns the modified root DOM node, or
(if the path was unable to locate a specific element), nil."
  [dom-node path position new-nodes]
  (let [root-loc (dom-zipper dom-node)
        loc (navigate-dom-path root-loc path)]
    (and loc (z/root (update-dom loc position new-nodes)))))

(defn apply-extend-dom-rules
  "Invoked from extend-dom, once the root element is known. Returns the modified node, if any rule
is viable, or the root-node, if no rule is viable.
root-node
  Root element DOM node.
rules
  seq of rules definining where the nodes are to be inserted.
new-nodes
  New DOM nodes to insert."
  [root-node rules new-nodes]
  (or
   (->>
    (map (fn [[path position]]
           (extend-root-element root-node path position new-nodes)) rules)
    (remove nil?)
    first)
   root-node))

(defn extend-dom
  "Extends a DOM (a set of root DOM nodes), adding new nodes at a specific position. Uses the path
(a seq of keywords, representing elements) to locate an element within the DOM
then adds the new nodes at the position (position can be :top :bottom :before :after).
Assumes that the first element node in dom-nodes
is the root of the element tree (other nodes are possibly text or comments).
Does nothing if the targetted node can't be found. Returns a new
sequence of dom nodes.
dom-nodes
A seq of nodes representing the document. The first element node is considered the root.
rules
A seq of rules identifying where to place the new nodes. Each rule is a pair of values.
The first value is a seq of element types (as keywords) to navigate down. The second
value is a position (:before, :after:, :top, or :bottom). The first matching rule
is used.
new-nodes
The new nodes to insert in a position within the document."
  [dom-nodes rules new-nodes]
  (loop [result []
         queue dom-nodes]
    (lcond
      ; Never found an element node? Return the original nodes unchanged.
      (empty? queue) dom-nodes
      :let [node (first queue)]
      (element? node) (concat result [(apply-extend-dom-rules node rules new-nodes)] (rest queue))
      :else (recur (conj result node) (rest queue)))))
         
        
    
