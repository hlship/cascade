(set! *warn-on-reflection* true)

(ns com.howardlewisship.cascade.test-xml-tokenizer
    (:use clojure.contrib.test-is com.howardlewisship.cascade.internal.xmltokenizer))

(def #^{:private true} base "src/test/resources/com/howardlewisship/cascade/internal/")

(defn expect
  [f tokens expected]
  (is (= (map f tokens) expected)))

(defn by-type
  "Matches tokens by type"
  [type tokens]
  (filter #(= (% :type) type) tokens))

(deftest parse-without-namespace
         (let [tokens (tokenize-xml (str base "simple.xml"))]

              ; Check that the right series of tokens was parsed

              (expect :type tokens [:start-element :attribute :text :start-element :end-element :text :end-element])

              ; Check the line number tracking.
              ; TODO: these line numbers aren't what I'd expect; the text tokens are ending up with the line number
              ; at the *end* (not *start*) of the text. Have to see how Tapestry handles that.

              (expect #(-> % :location :line) tokens [2 2 4 4 4 6 6])

              (expect :tag (by-type :start-element tokens) [:root :flintstone])

              (let [attr (tokens 1)]
                   (is (= (attr :name) :id))
                   (is (= (attr :value) "fred")))

              (is (= (.trim #^String ((tokens 2) :value)) "Before"))))


(deftest parse-with-namespaces
         (let [tokens (tokenize-xml (str base "ns.xml"))]

              (expect :type tokens [:begin-ns-prefix :start-element :attribute :attribute :text :start-element
                                    :attribute :end-element :begin-ns-prefix :text :start-element :end-element
                                    :end-ns-prefix :text :end-element :end-ns-prefix])

              (expect :ns-uri (by-type :begin-ns-prefix tokens) ["clojure" "zeta/1.0"])

              (expect :ns-prefix (by-type :begin-ns-prefix tokens) [:c :z])

              (expect :ns-prefix (by-type :end-ns-prefix tokens) [:z :c])

              (expect :ns-uri (by-type :start-element tokens) ["clojure" "" "zeta/1.0"])

              (expect :ns-uri (by-type :attribute tokens) ["" "clojure" ""])

              ))

