(ns com.howardlewisship.cascade.test-views
  (:use
   clojure.contrib.test-is
   clojure.contrib.pprint
   clojure.contrib.duck-streams
   app1.views
   app1.fragments
   com.howardlewisship.cascade.internal.utils
   com.howardlewisship.cascade.config
   com.howardlewisship.cascade.dom
   com.howardlewisship.cascade.view-manager))

(def #^{:private true} base "src/test/resources/")

(defn- render
  [dom]
  (with-out-str (render-xml dom *out*)))

(defn- execute-view-test
  [name]
  (let [input-path (str base name ".xml")
        expected-path (str base name "-expected.xml")
        view (parse-and-create-view input-path)
        dom (view {})
        output (render dom)
        expected (slurp expected-path)]
    (is (= output expected))))

(deftest simple-view (execute-view-test "simple-view"))

(deftest basic-namespaces (execute-view-test "basic-namespaces"))

(add-to-config :view-namespaces 'app1.views)
(add-to-config :fragment-namespaces 'app1.fragments)

(defn test-view
  [view-name expected-output-file]
  (let [view (get-view view-name)
        dom (view {})
        output (render dom)
        expected (slurp* (find-classpath-resource expected-output-file))]
    (is (= output expected))))


(deftest simple-view-and-fragment
  (test-view "simple" "simple-view-and-fragment-expected.txt"))

(deftest comments
  (test-view "commentsview" "comments-expected.txt"))
