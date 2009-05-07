(ns com.howardlewisship.cascade.test-parser
    (:use clojure.contrib.test-is
          com.howardlewisship.cascade.parser))

(def #^{:private true} base "src/test/resources/com/howardlewisship/cascade/internal/")

(deftest test-add-to-key-list
         (let [initial {}
               first-add (add-to-key-list initial :foo 1)
               second-add (add-to-key-list first-add :foo 2)]
              (is (= first-add {:foo [1]}))
              (is (= second-add {:foo [1 2]}))))

(deftest trival-document
         (let [tree (parse-template (str base "root-only.xml"))]
              ))

(run-tests)