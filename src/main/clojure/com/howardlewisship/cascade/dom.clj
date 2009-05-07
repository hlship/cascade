(ns com.howardlewisship.cascade.dom)

; Somewhat similar to the DOM parsed by clojure.xml/parse, but
; it includes extra support for namespaces.

(defstruct dom-node
  :type
  :ns-uri
  :ns-prefix
  :tag
  :name
  :text)



