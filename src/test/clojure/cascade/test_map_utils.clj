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

(ns cascade.test-map-utils
  (:use
    (cascade map-utils)
    (clojure (test :only [is are deftest]))))
    
    
(deftest test-list-to-map
  (is (= (list-to-map-of-seqs [:foo :bar :baz] [[:foo :f1 :f2 :f3] [:bar :b1 :b2] [:baz]])
        {:foo [:f1 :f2 :f3] :bar [:b1 :b2] :baz []})))

(deftest unexpected-key-in-list-to-map
  (is
    (thrown-with-msg? RuntimeException #"Key :baz is not allowed; keys must be one of \[:foo :bar\]\."
      (list-to-map-of-seqs [:foo :bar] [[:baz]]))))