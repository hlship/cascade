(ns com.howardlewisship.cascade.internal.parser
    (:use clojure.contrib.monads
          [clojure.contrib.pprint :only (pprint)]
          com.howardlewisship.cascade.internal.xmltokenizer))

; We parse streams of xml-tokens (from the xmltokenizer) into rendering functions.
; a rendering function takes a map (its environment) and returns a list of DOM nodes that can be rendered, or
; nil. At render time the env will have keys for many values, and special keys:
; :type :element
; :token - the token of the referencing fragment
; :body - text and element nodes
; :parameters - map of parameters in the referencing fragment
; :attributes - map of attributes in the referencing fragment
; :ns-uri-to-prefix - map from namespace URI to namespace prefix

(defstruct element-node :type :token :body :attributes :ns-uri-to-prefix)
(defstruct text-node :type :token)

(defn- build-uri-to-prefix
  "Converts a list of :begin-ns-prefix tokens into a map from URI to prefix."
  [tokens]
  (reduce (fn [map token] (assoc map (token :ns-uri) (token :ns-prefix))) {} tokens))

(defn- fail
  [#^String msg]
  (throw (RuntimeException. msg)))

; Let's parse the XML stream to an intermdiate DOM-like structure.
; Thus our monadic values will be functions that take a state
; and return a DOM node.  The state will always be the remaining vector
; of tokens.

(def parser-m (state-t maybe-m))

; And some actions and parser generators

(defn any-token
  "Fundamental parser action: returns [first, rest] if tokens is not empty, nil otherwise."
  [tokens]
  (if (empty? tokens)
      nil
      ; This is what actually "consumes" the tokens seq
      (list (first tokens) (rest tokens))))

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

  (declare parse-element one-or-more)

  (defn none-or-more [parser]
    (optional (one-or-more parser)))

  (defn one-or-more [parser]
    (domonad [a parser
              as (none-or-more parser)]
             (cons a as)))


  (def parse-text
    (domonad [text-token (match-type :text)]
             (struct text-node :text text-token)))

  (def match-first m-plus)

  ; This needs to be a parser generator, not a parser, to
  ; work around a chicken-and-the-egg issue: parse-body and parse-element
  ; are mutually dependent and need full definitions of each; making this a function
  ; defers the need for parse-element to be constructed.
  (defn parse-body
    []
    (match-first parse-text parse-element))

  (def parse-element
    (domonad [ns-begin-tokens (none-or-more (match-type :begin-ns-prefix))
              token (match-type :start-element)
      ; attributes immediately follow the start-element token
              attribute-tokens (none-or-more (match-type :attribute))
      ; after which, there may be the tokens for the body
      ; (including text and recursive elements)
              body-elements (none-or-more (parse-body))
      ; and matched by an end element token
              _ (match-type :end-element)
      ; Trust that the XML tokenizer balances each :begin-ns-prefix with an :end-ns-prefix
              _ (none-or-more (match-type :end-ns-prefix))
              ]
             ; Package everything together
             (struct element-node :element token body-elements attribute-tokens
                     (build-uri-to-prefix ns-begin-tokens))))


  (def parse-template-root
    ; For the moment, parsing a template is exactly the same as parsing the root element.
    ; Later we'll support doctypes, text and comments before and after the root element.
    parse-element)

  ) ; with-monad parser-m

(defn parse-template
  [src]
  (let [tokens (tokenize-xml src)
        result (parse-template-root tokens)]

       (when (nil? result)
             (pprint tokens)
             (fail "Parse completed with no result."))

       (let [[root-element remaining-tokens] result]
            (when-not (empty? remaining-tokens)
                      (fail (format "Not all XML tokens were parsed, %s remain, starting with %s." (count result) (first result))))
            root-element)))