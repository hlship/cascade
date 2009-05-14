; Script that loads all tests for execution.

(use 'clojure.contrib.test-is)

(set! *warn-on-reflection* true)

(time (let [spaces ['com.howardlewisship.cascade.test-xml-tokenizer
                    'com.howardlewisship.cascade.test-parser
                    'com.howardlewisship.cascade.test-views
                    'com.howardlewisship.cascade.test-utils]]
           (apply use spaces)
           (apply run-tests (map find-ns spaces))))
