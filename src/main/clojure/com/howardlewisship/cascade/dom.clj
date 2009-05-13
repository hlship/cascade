(ns com.howardlewisship.cascade.dom
    (:import (java.io Writer)))



; Somewhat similar to the DOM parsed by clojure.xml/parse, but
; it includes extra support for namespaces.

(defstruct dom-node
  :type ; :element, :attribute, :text
  :ns-uri ; namespace of element or attribute (often empty string)
  :name ; element tag or attribute name (as a keyword)
  :value ; attribute value or literal text
  :attributes ; attribute dom-nodes within element dom-nodes
  :content) ; dom-nodes for any children

(defn- write
  [#^Writer out & strings]
  (doseq [#^String s strings]
         (.write out s)))

(defmulti render-node-xml :type)

(defmethod render-node-xml :text
  [text-node out]
  ; TODO: entity filtering
  (write out (text-node :value)))

(declare render-xml)

(defmethod render-node-xml :element
  [element-node out]
  (let [element-name (name (element-node :name))
        content (element-node :content)]
       ; TODO: namespace support
       (write out "<" element-name)

       (doseq [{attr-name :name attr-value :value} (element-node :attributes)]
              ; TODO: URL escaping
              (write out " " (name attr-name) "=\"" attr-value "\""))

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
  (doseq [node dom-nodes]
         (render-node-xml node out)))
