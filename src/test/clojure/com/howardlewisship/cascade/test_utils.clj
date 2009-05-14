(ns com.howardlewisship.cascade.test-utils
  (:use
    clojure.contrib.test-is
    clojure.contrib.pprint
    clojure.contrib.duck-streams
    com.howardlewisship.cascade.internal.utils))


(deftest classpath-resource-does-not-exist
  (= (find-classpath-resource "does/not/exist.txt") nil))

(deftest classpath-resource-exists
  (let [path "com/howardlewisship/cascade/internal/resource.txt"]
    (= (slurp* (find-classpath-resource path))
      (slurp (str "src/test/resources/" path)))))

(deftest namespace-relative
  (= (slurp* (find-classpath-resource 'namespace-relative "ns.txt"))
    (slurp "src/test/resources/com/howardlewisship/cascade/test_utils/ns.txt")))