; Script that loads all tests for execution.

(use 'clojure.contrib.test-is)
(require 'com.howardlewisship.cascade.internal.test-xml-tokenizer)

(time (run-all-tests))
