; Script that loads all tests for execution.

(use 'clojure.contrib.test-is)

(time (let [spaces ['com.howardlewisship.cascade.test-xml-tokenizer
                    'com.howardlewisship.cascade.test-parser
                    'com.howardlewisship.cascade.test-views]]
           (apply use spaces)
           (apply run-tests (map find-ns spaces))))
