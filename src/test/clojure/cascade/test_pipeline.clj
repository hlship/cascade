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

(ns cascade.test-pipeline
  (:use
    (clojure (test :only [is are deftest]) )
    (cascade config pipeline)))

(defn my-to-upper
  [^String s]
  (.toUpperCase s))

(defn my-in-filter
  [d ^String s]
  (d (format "%s: (length %d)" s (.length s))))

(decorate my-to-upper my-in-filter)

(deftest test-decorate-with-in-filter
  (is (= (my-to-upper "fred") "FRED: (LENGTH 4)")))

(defn my-to-lower
  [^String s]
  (.toLowerCase s))

(deftest test-decorate-with-out-filter
  (is (= (my-to-lower "Fred") "fred"))
  (decorate my-to-lower (fn [delegate s] (format "%s: (length %d)" (delegate s) (.length s))) )
  (is (= (my-to-lower "Barney") "barney: (length 6)")))