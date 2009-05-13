(ns com.howardlewisship.cascade.dom
    (:import (java.io Writer)))

; TODO: Replace deep tail recursion with some kind of queue or visitor pattern.
; Need to be able to control close tags for empty elements (XML vs. HTML style)
; Need to be able to control quote character (single vs. double quote)
; Need to filter entities in :text nodes (can that be done earlier?)
; Who should URL encode attributes?

; TODO: For testing purposes (i.e., to get consistent results across JVMs, etc.)
; it may be necessary to sort attributes when rendering (but I'd rather not bother
; for production).

; Somewhat similar to the DOM parsed by clojure.xml/parse, but
; it includes extra support for namespaces.

(defstruct dom-node
  :type ; :element, :attribute, :text
  :ns-uri ; namespace of element or attribute (often empty string)
  :ns-uri-to-prefix ; map from ns-uri to desired prefix (for elements)
  :name ; element tag or attribute name (as a keyword)
  :value ; attribute value or literal text
  :attributes ; attribute dom-nodes within element dom-nodes
  :content) ; dom-nodes for any children

(defn- write
  [#^Writer out & strings]
  (doseq [#^String s strings]
         (.write out s)))

(defn- blank?
  [string]
  (or (nil? string) (= 0 (.. string trim length))))

(defn- qualify-name
  "Qualifies a name based on a namespace URI and a mapping from URI to prefix"
  [name ns-uri ns-uri-to-prefix]
  (if (blank? ns-uri)
      name
      (let [prefix (get ns-uri-to-prefix ns-uri)]
           (if (blank? prefix)
               name
               (str prefix ":" name)))))

(defmulti render-node-xml (fn [node & rest] (node :type)))

(defmethod render-node-xml :text
  [text-node ns-uri-to-prefix out]
  ; TODO: entity filtering
  (write out (text-node :value)))

(declare render-xml-with-ns)

(defmethod render-node-xml :element
  [element-node ns-uri-to-prefix out]
  (let [node-ns-uri-to-prefix (element-node :ns-uri-to-prefix)
        element-ns-uri-to-prefix (merge ns-uri-to-prefix node-ns-uri-to-prefix)
        element-name (name (element-node :name))
        element-qname (qualify-name element-name (element-node :ns-uri) element-ns-uri-to-prefix)
        content (element-node :content)]

       ; TODO: When Ajax is supported, we may need to create artificial namespace URI mappings
       ; to account for

       (write out "<" element-qname)

       ; Write out normal attributes

       (doseq [{attr-name :name attr-value :value ns-uri :ns-uri} (element-node :attributes)]
              (let [attr-qname (qualify-name (name attr-name) ns-uri element-ns-uri-to-prefix)]
                   ; TODO: URL escaping
                   (write out " " attr-qname "=\"" attr-value "\"")))

       ; Write out namespace mappings for this element
       ; TODO: handle default namespace (prefix is "").

       (doseq [ns-uri (keys node-ns-uri-to-prefix)]
              (let [prefix (get node-ns-uri-to-prefix ns-uri)]
                   (write out " xmlns")
                   (when-not (blank? prefix)
                             (write out ":" prefix))
                   (write out "=\"" ns-uri "\"")))

       ; TODO: ugly gotchas about rendering HTML: can't always close an empty tag (i.e., <script>), etc.

       (if (empty? content)
           (write out "/>")
           (do
             (write out ">")

             ; Render children recursively; they inherit the namespace mappings from this element
             ; and its containers.

             (render-xml-with-ns content element-ns-uri-to-prefix out)

             (write out "</" element-qname ">")))))

(defn- render-xml-with-ns
  [dom-nodes ns-uri-to-prefix out]
  (doseq [node dom-nodes]
         (render-node-xml node ns-uri-to-prefix out)))

(defn render-xml
  "Renders a seq of DOM nodes representing a complete document (generally the list will include just
  a single root element node, but text and comments and the like may come into play as well)."
  [dom-nodes out]
  ; TODO: Render out the <?xml version="1.0"?> P.I.?
  (render-xml-with-ns dom-nodes {} out))
