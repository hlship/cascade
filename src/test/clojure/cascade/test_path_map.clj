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

; Contains the basic dispatchers

(ns cascade.test-path-map
  (:use 
    (cascade config path-map)
    (cascade.internal utils)
    (clojure.contrib [test-is :only [deftest are]])))
    
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
    (are (= (find-fns _1) _2)
      "a/b/c" [fn-ab fn-any]
      "a/b" [fn-ab fn-any]
      "c" [fn-c fn-any]
      "d" [fn-any]
      "" [fn-any])))