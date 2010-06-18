; Copyright 2009, 2010 Howard M. Lewis Ship
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

(ns cascade.test-viewbuilder
  (:use
    [clojure test]
    cascade.dom
    cascade.internal.viewbuilder))

(deftest test-convert-render-result
  (let [dom-node (element-node :foo nil nil)]
    (are [input output] (= (convert-render-result input) output)
      dom-node dom-node
      "any string" (text-node "any string")
      123.4 (text-node "123.4"))))
      
(deftest combine-a-non-dom-node-is-failure
  (is
    (thrown? RuntimeException ; Should check the message, but RE hell
      (combine {:not-an :element-node}))))