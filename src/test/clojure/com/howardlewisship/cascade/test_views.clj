; Copyright 2009 Howard M. Lewis Ship
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
; implied. See the License for the specific language governing permissions
; and limitations under the License.

(ns com.howardlewisship.cascade.test-views
  (:use
   (clojure.contrib test-is pprint duck-streams)
   (app1 views fragments)
   (com.howardlewisship.cascade config dom view-manager)
   com.howardlewisship.cascade.internal.utils))

(def #^{:private true} base "src/test/resources/")

(defn render
  [dom]
  (with-out-str (render-xml dom *out*)))

(defn minimize-ws [string]
	(.replaceAll string "\\s+" " ")) 
	

(defn execute-view-test
  [name]
  (let [input-path (str base name ".xml")
        expected-path (str base name "-expected.xml")
        view (parse-and-create-view (namespace 'render) input-path)
        dom (view {})
        output (render dom)
        expected (slurp expected-path)]
    (is (= output expected))))

(deftest simple-view (execute-view-test "simple-view"))

(deftest basic-namespaces (execute-view-test "basic-namespaces"))

(add-to-config :view-namespaces 'app1.views)
(add-to-config :fragment-namespaces 'app1.fragments)

(defn test-view
  ([view-name expected-output-file]
   (test-view view-name expected-output-file {}))
  ([view-name expected-output-file env]
   (let [view (get-view view-name)
         dom (view env)
         output (minimize-ws (render dom))
         expected (minimize-ws (slurp* (find-classpath-resource expected-output-file)))]
     (is (= output expected)))))


(deftest simple-view-and-fragment
  (test-view "simple" "simple-view-and-fragment-expected.txt"))

(deftest comments
  (test-view "commentsview" "comments-expected.txt"))

(deftest pass-parameter-into-fragment
  (test-view "use-echo" "use-echo-expected.txt"))

(deftest compute-params
  (test-view "calculate-value-passed-as-param" "calculate-value-passed-as-param-expected.txt" {:count 99}))

(deftest access-private-fns
  (test-view "access-private-fns" "access-private-fns-expected.txt" {:user-input "clojure"}))

(deftest expansions
  (test-view "expansions" "expansions-expected.txt"
    {:time "A Fine Day to learn Cascade"
     :message-class "dk-blue"
     :message "No, this is not Tapestry 6."}))
     
(deftest simple-loop
	(test-view "simple-loop" "simple-loop-expected.txt"))
	     
(deftest nested-loops
	(test-view "nested-loops" "nested-loops-expected.txt"))
	
(deftest render-body-fragment
	(test-view "body-renderer" "body-renderer-expected.txt"))
	     