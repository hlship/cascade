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

(ns com.howardlewisship.cascade.test-utils
  (:use
    app1.fragments
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

; (pprint (macroexpand-1 '(using-namespace 'app1.fragments (fn [string] (double-talk string)))))

(deftest define-function-using-namespace
  (let [f (eval-in-namespace 'app1.fragments (fn [string] (double-talk string)))]
    (is (= (f "cascade") "cascade cascade"))))

