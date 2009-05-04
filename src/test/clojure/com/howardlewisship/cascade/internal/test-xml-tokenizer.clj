(ns com.howardlewisship.cascade.internal.test-xml-tokenizer
    (:use clojure.contrib.test-is com.howardlewisship.cascade.internal.xmltokenizer))

(set! *warn-on-reflection* true)

(def base "src/test/resources/com/howardlewisship/cascade/internal/")

(defn expect
  [f tokens expected]
  (is (= (map f tokens) expected)))

(deftest parse-without-namespace
         (let [tokens (tokenize-xml (str base "simple.xml"))]

              ; Check that the right series of tokens was parsed

              (expect :type tokens [:start-element :attribute :text :start-element :end-element :text :end-element])

              ; Check the line number tracking.
              ; TODO: these line numbers aren't what I'd expect; the text tokens are ending up with the line number
              ; at the *end* (not *start*) of the text. Have to see how Tapestry handles that.

              (expect #(-> % :location :line) tokens [2 2 4 4 4 6 6])

              (expect :tag (filter #(= (% :type) :start-element) tokens) [:root :flintstone])

              (let [attr (tokens 1)]
                   (is (= (attr :name) :id))
                   (is (= (attr :value) "fred")))

              (is (= (.trim #^String ((tokens 2) :value)) "Before"))

              ))

(time (run-tests))

