(ns com.howardlewisship.cascade.test-views
    (:use clojure.contrib.test-is
          clojure.contrib.pprint
          com.howardlewisship.cascade.dom
          com.howardlewisship.cascade.view-manager))

(def #^{:private true} base "src/test/resources/")


(defn- execute-view-test
  [name]
  (let [input-path (str base name ".xml")
        expected-path (str base name "-expected.xml")
        view (parse-and-create-view input-path)
        dom (view {})
        output (with-out-str (render-xml dom *out*))
        expected (slurp expected-path)]
       (is (= output expected))))

(deftest simple-view (execute-view-test "simple-view"))

(deftest basic-namespaces (execute-view-test "basic-namespaces"))
