(ns com.howardlewisship.wafer.parser
    (:use clojure.xml)
    (:import (clojure.lang ISeq IPersistentMap)))



; To be renamed and shifted elsewhere.

(defn sniff
  "Sniffs the DOM object to identify how it should be converted to a function."
  [o]
  (cond (map? o) :element
        (vector? o) :children
        (seq? o) :children
        (string? o) :string
        true (throw (Exception. (format "Can't sniff %s" o)))))

(defmulti parsed-dom-to-function sniff)

; A string is a kind of pass-thru. The function ignores the parameters and returns the fixed string value.
(defmethod parsed-dom-to-function :string [s]
  (fn string [node] s))

(defmethod parsed-dom-to-function :element [e]
  (let [tag (e :tag)
        attrs (or (e :attrs) nil)
        children (if (e :content) (parsed-dom-to-function (e :content)))]
       (fn element [node]
           {:tag tag
            :attrs attrs
            :content (if children (children node))})))

; A sequence is the content children of an element. The children are recursively converted into
; functions, and the collection of DOM nodes is replaced with a collection of view fragment functions.
(defmethod parsed-dom-to-function :children [seq]
  (let [parsed (map parsed-dom-to-function seq)]
       (fn children [node]
           (map #(% node) parsed))))

(defn parse-view-fragment-template
  "Parses a template file from a source (as per clojure.xml/parse), returning a template fragment function."
  [source]
  (parsed-dom-to-function (clojure.xml/parse source)))



(defmulti dom-to-string sniff)

(defmethod dom-to-string :string [s] [s])
(defmethod dom-to-string :children [c] (apply str (map dom-to-string c)))
(defmethod dom-to-string :element [e]
  (let [tag-name (name (e :tag))
        attrs (apply str (map (fn [[key value]] (format " %s='%s'" (name key) value)) (e :attrs)))
        children (if (e :content) (dom-to-string (e :content)) "")]
       (apply str "<" tag-name attrs ">" children "</" tag-name ">")))

