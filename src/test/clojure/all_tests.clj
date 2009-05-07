; Script that loads all tests for execution.

(use 'clojure.contrib.test-is
     'com.howardlewisship.cascade.internal.test-xml-tokenizer
     'com.howardlewisship.cascade.test-parser)

(time (run-all-tests))
