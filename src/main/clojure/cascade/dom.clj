; Copyright 2009, 2010, 2011 Howard M. Lewis Ship
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
  cascade.dom
  "DOM node structure, rendering and manipulation"
  (:import
    (clojure.lang Keyword)
    (java.io Writer))
  (:require
    (clojure [zip :as z]))
  (:use
    (cascade fail utils)
    cascade.internal.utils))

; TODO: Replace deep tail recursion with some kind of queue or visitor pattern, perhaps lazy function to walk tree
; Need to be able to control close tags for empty elements (XML vs. HTML style)
; Need to be able to control quote character (single vs. double quote), for responses encoded into JSON
; Need to filter entities in :text nodes (can that be done earlier?)
; Who should URL encode attributes?

; TODO: For testing purposes (i.e., to get consistent results across JVMs, etc.)
; it may be necessary to sort attributes when rendering (but I'd rather not bother
; for production).  

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
  (let [out (apply str (map #(char-to-entity %) (seq s)))]
    (if (= s out) s out)))


(defmulti to-attr-string
  "Converts an attribute value to a string. It is not necessary to apply quotes (those come at a later stage)."
  class)

(defmethod to-attr-string String
  [str-value]
  (encode-string str-value))

(defmethod to-attr-string Number
  [^Number numeric-value]
  (.toString numeric-value))

(defmethod to-attr-string Keyword
  [kw]
  (encode-string (name kw)))

(defn- write
  "Write a number of strings to the writer."
  [^Writer out & strings]
  (doseq [^String s strings]
    (.write out s)))

(defprotocol NodeStreaming
  "Defines how a tree of DOM Nodes can be recursively streamed out as characters."
  (stream
    [node strategy ^Writer out]
    "Streams the node as characters to the writer. The strategy encapsulates the differences between HTML and XML
    in terms of attribute quoting and handling of empty elements."))

; This could possibly be replaced by extending NodeStreaming to the seq?

(defn stream-nodes
  [dom-nodes strategy ^Writer out]
  (doseq [node dom-nodes]
    (stream node strategy out)))

(defrecord Element [name attributes content]

  NodeStreaming
  (stream [node strategy out]
    ; I'm hoping the need to qualify clojure.core/name is a bug in the 1.2 defrecord code that can be
    ; removed when we switch up to 1.3.
    (let [element-name (clojure.core/name name)
          attr-quote (strategy :attribute-quote)]
      (write out "<" element-name)
      ; Write out normal attributes
      (doseq [[attr-name attr-value] attributes]
        (if-not (nil? attr-value)
          (write out
            " " (clojure.core/name attr-name) "=" attr-quote (to-attr-string attr-value) attr-quote)))

      (if (empty? content)
        ((strategy :write-empty-element-close) element-name out)
        (do
          (write out ">")
          (stream-nodes content strategy out)
          (write out "</" element-name ">"))))))

; TODO: possibly don't need Text, instead extend NodeStreaming to String

(defrecord Text [text]

  NodeStreaming
  (stream [node strategy out]
    (write out text)))

(defrecord Comment [text]

  NodeStreaming
  (stream [node strategy out]
    (write out "<!--" text "-->")))

; Most outside code will use these standard constructor functions, rather than using the records' constructors.

(defn element-node
  [name attributes content]
  (Element. name attributes content))

(defn raw-node
  "Wraps a string as a text DOM node, but does not do any filtering of the value."
  [s]
  (Text. s))

(defn text-node
  "Creates a text node from the string. The string is encoded when the node is constructed."
  [text]
  (raw-node (encode-string text)))


(def xml-strategy {
  :attribute-quote "\""
  :write-empty-element-close (fn [element-name out] (write out "/>"))
  })

(defn stream-xml
  "Streams a seq of DOM nodes representing a complete document to a writer. Ggenerally the seq will include just
a single root element node, but text and comments and the like may come into play as well."
  [dom-nodes out]
  (write out "<?xml version=\"1.0\"?>\n")
  (stream-nodes dom-nodes xml-strategy out))

(def html-must-close-elements #{"script"})

(defn html-empty-element-renderer
  [element-name out]
  (if (contains? html-must-close-elements element-name)
    (write out "></" element-name))
  (write out ">"))

(def html-strategy {
  :attribute-quote "\""
  :write-empty-element-close #'html-empty-element-renderer
  })

(defn stream-html
  "Streams a seq of DOM nodes representing a complete document (as with stream-xml) but with HTML
output semantics: attributes are still quoted, but tags may not be balanced (the end tag
may be ommitted if the element has no content)."
  [dom-nodes out]
  (stream-nodes dom-nodes html-strategy out))

(defn element?
  "Is the node a DOM Element?"
  [node]
  (instance? Element node))

(defn dom-node?
  "Is the object some kind of DOM node?"
  [node]
  (or (element? node)
    (instance? Text node)
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
      (and (element? n) (= (:name n) target-element))
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
(if the path was unable to locate a specific element), the original root DOM node."
  [dom-node path position new-nodes]
  (lcond
    :let [root-loc (dom-zipper dom-node)
          loc (navigate-dom-path root-loc path)]
    (nil? loc) dom-node
    :else (z/root (update-dom loc position new-nodes))))

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
      (element? node) (concat result [(extend-root-element node path position new-nodes)] (rest queue))
      :else (recur (conj result node) (rest queue)))))
         
        
    
