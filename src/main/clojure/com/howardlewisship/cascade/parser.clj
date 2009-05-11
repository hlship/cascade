(ns com.howardlewisship.cascade.parser
    (:use clojure.contrib.monads
          com.howardlewisship.cascade.dom
          com.howardlewisship.cascade.internal.xmltokenizer))

(def cascade-uri "cascade")

; We parse streams of xml-tokens (from the xmltokenizer) into rendering functions.
; a rendering function takes a map (its environment) and returns a list of DOM nodes that can be rendered, or
; nil. At render time the env will have keys for many values, and special keys:
; :type :element
; :token - the token of the referencing fragment
; :body - text and element nodes
; :parameters - map of parameters in the referencing fragment
; :attributes - map of attributes in the referencing fragment

(defstruct element-node :type :token :body :attributes)
(defstruct text-node :type :token)


; the parsing functions

(defn- fail
  [#^String msg]
  (throw (RuntimeException. msg)))

; Let's parse the XML stream to an intermdiate DOM-like structure.
; Thus our monadic values will be functions that take a state
; and return a DOM node.  The state will always be the remaining vector
; of tokens.

; This is supposed to be the same as (state-t maybe-m) and we'll try that later.
; I'm calling the monadic values "actions", as that seems to fit ... they perform
; a delta on the current state and return the new result.

; TODO: change back to (def parser-m (state-t maybe-m)

(defmonad parser-m
          [m-result (fn [x]
                        (fn [tokens]
                            (list x tokens)))

           m-bind (fn [parser action]
                      (fn [tokens]
                          (let [result (parser tokens)]
                               (when-not (nil? result)
                                         ((action (first result)) (second result))))))

           m-zero (fn [tokens]
                      nil)

           m-plus (fn [& parsers]
                      (fn [tokens]
                          (first
                            (drop-while nil?
                                        (map #(% tokens) parsers)))))])

; And some actions and parser generators

(defn any-token
  "Fundamental parser action: returns [first, rest] if tokens is not empty, nil otherwise."
  [tokens]
  (if (empty? tokens)
      nil
      ; This is what actually "consumes" the tokens seq
      (list (first tokens) (rest tokens))))




; Utilities that will likely move elsewhere

(defn add-to-key-list
  "Updates the map adding the value to the list stored in the indicated key."
  [map key value]
  (update-in map [key] #(conj (or % []) value)))

(with-monad
  parser-m

  (defn token-test
    "Parser factory using a predicate. When a token matches the predicate, it becomes
  the new result."
    [pred]
    (domonad
      [t any-token :when (pred t)]
      ; return the matched token
      t))

  (defn match-type
    "Parser factory that matches a particular token type (making the matched
token the result), or returns nil."
    [type]
    (token-test #(= (% :type) type)))

  (defn optional [parser]
    (m-plus parser (m-result nil)))

  (declare element one-or-more)

  (defn none-or-more [parser]
    (optional (one-or-more parser)))

  (defn one-or-more [parser]
    (domonad [a parser
              as (none-or-more parser)]
             (cons a as)))

  (defn attribute
    "Parser generator that matches attribute tokens, adding them to
    :attributes key of the element-node, and returning the new
    element-node."
    [element-node]
    (domonad [attribute-token (match-type :attribute)]
             (add-to-key-list element-node :attributes attribute-token)))

  (defn text
    "Parser generator that matches text tokens, adding them to the :body
key of the element-node, and returning the new element-node."
    [element-node]
    (domonad [text-token (match-type :text)]
             (add-to-key-list element-node :body text-token)))

  (def match-first m-plus)

  (defn body-token
    [element-node]
    (match-first
      (attribute element-node)
      (text element-node)
      (element element-node)))

  (defn process-body
    [element-node]
    (domonad [modified-element (optional (body-token element-node))
      ; final-result (process-body modified-element)
              ]
             (or modified-element element-node)))

  (defn element
    "Parses an element token into an element-node, and then adds the fully constructed element-node to the
:body of the containing element node."
    [container-element-node]
    (domonad [token (match-type :start-element)
              element-node (m-result (struct element-node :element token))
              assembled-node (process-body element-node)
              _ (match-type :end-element)]
             (add-to-key-list container-element-node :body assembled-node)))


  (def template
    ; Creates a psuedo-element (an empty map) and parses the root element into it, then extracts the root element.
    ; Will eventually have to expand to support text and comments, etc., around the root element."
    (domonad [root (element {})]
             (first (root :body))))

  ) ; with-monad parser-m

(defn parse-template
  [src]
  (let [tokens (tokenize-xml src)
        result (template tokens)]

       (if (nil? result)
           (fail "Parse completed with no result."))

       (let [[root-element remaining-tokens] result]
            (when-not (empty? remaining-tokens)
                      (fail (format "Not all XML tokens were parsed, %s remain, starting with %s." (count result) (first result))))
            root-element)))
