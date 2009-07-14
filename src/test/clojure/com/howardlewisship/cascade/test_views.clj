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
  (:import (java.util Date))
  (:use
   (clojure.contrib test-is pprint duck-streams)
   (app1 views fragments)
   (com.howardlewisship.cascade config dom view-manager (test-utils :only (unreachable)))
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
  
(deftest expansions-in-attributes-passed-to-fragments
  (test-view "attribute-expansions" "attribute-expansions-expected.xml" {:link-class "nav" :click "return false;"}))

; TODO: Make this locale and time zone insensitive
(deftest call-fragment-fn
  (test-view "call-fragment-fn" "call-fragment-fn-expected.xml" {:current-time (Date. 109 7 13, 9 27 32) }))
  
(deftest fragment-not-found
	(try
		(get-fragment 'not-here-not-there)
		(unreachable)
	(catch RuntimeException ex
		(is (= (.getMessage ex) "Could not locate fragment function not-here-not-there or template 'not-here-not-there.cml' in namespaces app1.fragments, com.howardlewisship.cascade.corelib.fragments.")))))
		
(deftest view-not-found
	(try
		(get-view 'not-here-not-there)
		(unreachable)
	(catch RuntimeException ex
		(is (= (.getMessage ex) "Could not locate view function not-here-not-there or template 'not-here-not-there.cml' in namespace app1.views.")))))
		
		  