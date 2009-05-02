; Uses a SAX parser to tokenize an XML document into
; a stream of token structures.

(ns com.howardlewisship.wafer.internal.xmltokenizer
    (:import (org.xml.sax ContentHandler Attributes Locator)
             (org.xml.sax.helpers DefaultHandler)
             (clojure.lang Keyword)
             (javax.xml.parsers SAXParserFactory)))


(defstruct xml-token
  :type :location ; used for all
  :ns-uri ; used for :start-element, :begin-ns-prefix and :attribute
  :ns-prefix ; used for :begin-ns-prefix and :end-ns-prefix
  :tag ; used for :start-element
  :name ; used for :attribute
  :value ; used for :text and :comment
  )

(defstruct location :resource :line)

; tag names, namespace prefixes and attribute names are converted
; from strings into keywords

; For performance, we may provide accessor functions for the xml-token struct

(def tokens {
  :start-element "start of element"
  :end-element "end of element"
  :attribute "attribute"
  :begin-ns-prefix "begin namespace prefix"
  :end-ns-prefix "end namespace prefix"
  :text "literal text"
  :comment "comment"
  })

; Until there's a native (monadic?) XML parser, we use the SAX parser. Its model
; doesn't fit well into our world, so we have to have some thread-local mutable state.

(def *tokens)
(def *buffer)
(def *resource)
(def *text-location)
(def *locator)

(defn- to-keyword
  "Converts a string into a keyword."
  [string]
  (. Keyword (intern (symbol string))))

(defn- add-token
  "Adds a token to the *tokens var."
  [token]
  (set! *tokens (conj *tokens token)))

(defn- current-location
  "Current location."
  []
  ; TODO: this is invoked far more often than the line number changes,
  ; so do some caching.
  (struct location *resource (.getLineNumber *locator)))

(defn- add-attribute-tokens
  "Adds a token for each attribute."
  [#^Attributes attrs]
  (doseq [x (range (. attrs getLength))]
         (let [uri (. attrs (getURI x))
               name (. attrs (getLocalName x))
               value (. attrs (getValue x))
               token (struct-map xml-token
                                 :type :attribute
                                 :location (current-location)
                                 :ns-uri uri
                                 :name (to-keyword name)
                                 :value value)]
              (add-token token))))


(defn- flush-text
  "Adds a text token if there's any text."
  []
  (when (pos? (.length *buffer))
        (add-token (struct-map xml-token :type :text :value (.toString *buffer) :location *text-location))
        (set! *text-location nil)
        (.setLength *buffer 0)))

(defn- add-text
  "Adds text to the buffer and manages the *text-location var."
  [ch start length]
  (.append *buffer ch start length)
  (when (nil? *text-location)
        (set! *text-location (current-location))))

(def sax-handler
  (proxy [DefaultHandler] []
         (startElement [uri local-name q-name #^Attributes attrs]
                       (flush-text)
                       (add-token (struct-map xml-token
                                              :type :start-element
                                              :location (current-location)
                                              :ns-uri uri
                                              :tag (to-keyword local-name)))
                       (add-attribute-tokens attrs))

         (endElement [uri local-name q-name]
                     (flush-text)
                     (add-token (struct-map xml-token :type :end-element :location (current-location))))

         (ignorableWhitespace [ch start length]
                              (add-text ch start length))

         (characters [ch start length]
                     (add-text ch start length))

         (setDocumentLocator [#^Locator locator]
                             (set! *locator locator))))

(defn tokenize-xml
  "Parses an XML file using a standard source (file, path, URL, etc.) into a collection
  of xml-token struct-maps."
  [src]
  (binding [*tokens []
            *buffer (StringBuilder.)
            *resource src
            *locator nil
            *text-location nil]
           (let [factory (SAXParserFactory/newInstance)]
                (.setNamespaceAware factory true)
                (.. factory newSAXParser (parse src sax-handler))
                *tokens)))