; Copyright 2009, 2011 Howard M. Lewis Ship
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

(ns cascade.test-parse-functions
  (:use
    (clojure test)
    (cascade.internal parse-functions)))

(deftest test-simple-fn
  (let [[n p forms] (parse-function-def '(my-fn [a b] c d))]
    (are [value expected]
      (= value expected)
      n 'my-fn
      p '[a b]
      forms '(c d))))

(deftest meta-data-for-doc-string-available
  (let [[n p forms] (parse-function-def '(my-fn "Test" [a b] c))]
    (are [value expected]
      (= value expected)
      n 'my-fn
      p '[a b]
      forms '(c))
    (is (= ((meta n) :doc) "Test"))))

(deftest meta-data-applied-to-symbol
  (let [[n p forms] (parse-function-def '(a-fn "Next" {:key :value} [x y z] :gnip :gnop))]
    (are [value expected]
      (= value expected)
      n 'a-fn
      p '[x y z]
      forms '(:gnip :gnop))
    (are [k v]
      (= ((meta n) k) v)
      :doc "Next"
      :key :value)))

(deftest binding-vector
  (let [[n p forms] (parse-function-def '(a-fn "Next" [a b] :gnip :gnop))]
    (are [value expected]
      (= value expected)
      n 'a-fn
      p '[a b]
      forms '(:gnip :gnop))))