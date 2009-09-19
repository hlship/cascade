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

(ns cascade.test-urls
  (:use
    (clojure (test :only [is are deftest]))
    cascade.urls))
    
(deftest convert-to-url-string
  (are [v s] (= (to-url-string v) s)
    "fred" "fred"
    23  "23"
    -42.7 "-42.7"
    :any-keyword "any-keyword"
    'any-symbol "any-symbol"))
    
(deftest test-split-path
  (is (vector? (split-path "foo/bar")))
  (are [input split] 
       (= (split-path input) split)
       "foo/bar" ["foo" "bar"]
       "/foo/bar/" ["foo" "bar"]
       "//foo//bar//baz" ["foo" "bar" "baz"]
       "" []
       "foo" ["foo"]))  
  
(deftest test-construct-absolute-path
  (are [context-path path extra-path parameters expected]
      (= (construct-absolute-path context-path (link-map-from-path path extra-path parameters)) expected)
      "" "foo" ["bar"] nil "/foo/bar"
      "/ctx" "account/list" [12 34] { :format :brief 'per-path 23 } "/ctx/account/list/12/34?format=brief&per-path=23"))  
      
(deftest test-parse-url-positional
  (let [env { :cascade { :extra-path ["123" "beta" "456"]}}]
    (is (= (class (parse-url env [p0 :int] p0)) Integer) ":int parses to Integer")
    (is (= (parse-url env [p0 :int p1 :str p2 :int] [p0 p1 p2]) [123 "beta" 456])))
    )          