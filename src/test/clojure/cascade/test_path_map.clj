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

(ns cascade.test-path-map
  (:use 
    (cascade config path-map urls pipeline)
    (cascade.internal utils)
    (clojure [test :only [deftest is are]])))
    
(defn fn-ab [env])
(defn fn-c [env])
(defn fn-any [env])

(defn find-fns
  [path]
  (for [[path function] (find-matching-functions :mapped-functions (split-path path))] function))
  
(deftest test-find-mappings
  (binding [configuration (atom { :mapped-functions { ["a" "b"] fn-ab
                                                      ["c"] fn-c
                                                      [] fn-any }})]
    (are [path expected]
      (= (find-fns path) expected)
      "a/b/c" [fn-ab fn-any]
      "a/b" [fn-ab fn-any]
      "c" [fn-c fn-any]
      "d" [fn-any]
      "" [fn-any])))
      
(defn valid-view-fn {:cascade-type :view} [])      
(defn valid-action-fn {:cascade-type :action} [])      
(defn pathed-action-fn {:cascade-type :action :path "do/something"} [])      
(defn pathed-view-fn {:cascade-type :view :path "show/something"} [])
(defn unknown-type-fn {:cascade-type :willow}[])

(deftest test-path-for-function
  (are [f path]
    (= (path-for-function f) path)
    valid-view-fn "view/cascade.test-path-map/valid-view-fn"
    valid-action-fn "action/cascade.test-path-map/valid-action-fn"
    pathed-action-fn "do/something"
    pathed-view-fn "show/something"))
    
(deftest not-a-valid-function-for-path-for-function
  (is (thrown-with-msg? RuntimeException 
      #"^Function cascade\.internal\.utils/function\? is neither a view function nor an action function\.$"
    (path-for-function function?))))
    
(deftest unknown-cascade-type-for-path-for-function
  (is (thrown? RuntimeException (path-for-function unknown-type-fn))))
  