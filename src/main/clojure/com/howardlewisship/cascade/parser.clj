(ns com.howardlewisship.cascade.parser
    (:use clojure.contrib.monads
          com.howardlewisship.cascade.dom
          com.howardlewisship.cascade.internal.xmltokenizer))

(def cascade-uri "cascade")

; We parse streams of xml-tokens (from the xmltokenizer) into rendering functions.
; a rendering function takes a map (its environment) and returns a list of DOM nodes that can be rendered, or
; nil. At render time the env will have keys for many values, and special keys:
; :token - the token of the referencing fragment
; :body - a rendering function that renders contents of the fragment
; :parameters - map of parameters in the referencing fragment
; :attributes - map of attributes in the referencing fragment

(defstruct element-node :token :body :attributes)

; the parsing functions

(defn- fail
  [token msg]
  (throw (RuntimeException. (format "Failure for token %s: %s" token msg))))

; Let's parse the XML stream to an intermdiate DOM-like structure.
; Thus our monadic values will be functions that take a state
; and return a DOM node.  The state will always be the remaining vector
; of tokens.

; This is supposed to be the same as (state-t maybe-m) and we'll try that later.
; I'm calling the monadic values "actions", as that seems to fit ... they perform
; a delta on the current state and return the new result.

(defmonad parser-m
          [m-result (fn [x]
                        (fn [tokens]
                            (list x tokens)))

           m-bind (fn [parser action]
                      (fn [tokens]
                          (let [result (parser tokens)]
                               (when-not (nil? result)
                                         ((action (first result)) (second result))))))

           m-zero (fn [strn]
                      nil)

           m-plus (fn [& parsers]
                      (fn [tokens]
                          (first
                            (drop-while nil?
                                        (map #(% tokens) parsers)))))])


(defn any-token
  "Returns [first, rest] if tokens is not empty, nil otherwise."
  [tokens]
  (if (empty? tokens)
      nil
      ; This is what actually "consumes" the tokens seq
      (list (first tokens) (rest tokens))))

(defn token-test
  "Parser factory using a predicate."
  [pred]
  (domonad parser-m
           [t any-token :when (pred t)]
           ; return the matched token
           t))

(defn match-type
  "Parser that matches a particular token type or returns nil."
  [type]
  (token-test #(= (% :type) type)))

(defn add-to-key-list
  "Updates the map adding the value to the list stored in the indicated key."
  [map key value]
  (update-in map [key] #(conj (or % []) value)))

(defn add-to-key-list-action
  [element key value]
  (fn [state]
      (list (add-to-key-list element key value) state)))

(defn set-result
  "Creates an action that forces the result to some value."
  [v]
  (fn action [state]
      [v state]))


(def element
  (domonad parser-m
           [token (match-type :start-element)
            tree-node (set-result {:tag (token :tag)})
             ; todo ... handle the body, including element attributes
            _ (match-type :end-element)]
           tree-node))

(defn parse-template
  [src]
  ; Assumes the first token is the outer element, need
  ; to tweak this.
  (let [tokens (tokenize-xml src)

    ; I know this isn't right, but why is it null?

        result (element tokens)]

       (if (nil? result)
           (fail "Stream did not parse."))

       (let [[tree remaining-tokens] result]
            (when-not (empty? remaining-tokens)
                      (fail "Not all tokens were parsed."))
            tree)))

; (let [tokens (tokenize-xml "/Users/Howard/work/clojure/cascade/src/test/resources/com/howardlewisship/cascade/internal/root-only.xml")

(time
  (let [tree (parse-template "src/test/resources/com/howardlewisship/cascade/internal/root-only.xml")]
       (pr tree)
       (println)))
