(ns com.howardlewisship.wafer.internal.test-xml-tokenizer
    (:use clojure.contrib.test-is com.howardlewisship.wafer.internal.xmltokenizer))

(set! *warn-on-reflection* true)

(def base "src/test/resources/com/howardlewisship/wafer/internal/")

(deftest parse-without-namespace
         (let [tokens (tokenize-xml (str base "simple.xml"))]
              ; (dorun (map println tokens))
              (is (= (map :type tokens) [:start-element :attribute :text :start-element :end-element :text :end-element]))))

(run-tests)