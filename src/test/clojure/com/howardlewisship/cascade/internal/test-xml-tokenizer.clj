(ns com.howardlewisship.cascade.internal.test-xml-tokenizer
    (:use clojure.contrib.test-is com.howardlewisship.cascade.internal.xmltokenizer))

(set! *warn-on-reflection* true)

(def base "src/test/resources/com/howardlewisship/cascade/internal/")

(deftest parse-without-namespace
         (let [tokens (tokenize-xml (str base "simple.xml"))]
              (is (= (map :type tokens) [:start-element :attribute :text :start-element :end-element :text :end-element]))))

(run-tests)

