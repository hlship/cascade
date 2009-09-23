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

(ns cascade.dom
  (:import (java.io Writer))
  (:use cascade.internal.utils))

; TODO: Split out rendering (that may be internal).

; TODO: Replace deep tail recursion with some kind of queue or visitor pattern.
; Need to be able to control close tags for empty elements (XML vs. HTML style)
; Need to be able to control quote character (single vs. double quote)
; Need to filter entities in :text nodes (can that be done earlier?)
; Who should URL encode attributes?

; TODO: For testing purposes (i.e., to get consistent results across JVMs, etc.)
; it may be necessary to sort attributes when rendering (but I'd rather not bother
; for production).

(defstruct dom-node
  :type ; :element, :attribute, :text
  ; TODO: :ns-prefix
  :name ; element tag or attribute name (as a keyword)
  :value ; attribute value or literal text
  :attributes ; attribute dom-nodes within element dom-nodes
  :content) ; dom-nodes for any children
  
(declare render-xml)
  
(defn- write
  [#^Writer out & strings]
  (doseq [#^String s strings]
    (.write out s)))

(defmulti render-node-xml
  (fn [node & rest] 
    (node :type)))

(defmethod render-node-xml :text
  [text-node out]
  ; TODO: entity filtering, also a :raw-text (for pre-filtered?)
  (write out (text-node :value)))

(defmethod render-node-xml :comment
  [comment-node out]
  (write out "<!--" (comment-node :value) "-->"))

(defmethod render-node-xml :element
  [element-node out]
  (let [element-name (name (element-node :name))
        content (element-node :content)]

    (write out "<" element-name)

    ; Write out normal attributes

    (doseq [{attr-name :name attr-value :value} (element-node :attributes)]
        ; TODO: URL escaping here, or elsewhere?
        (write out " " (name attr-name) "=\"" attr-value "\""))

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
