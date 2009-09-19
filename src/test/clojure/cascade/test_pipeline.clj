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

(deftest test-pipeline
  (let [to-upper-filter (fn [delegate s] (delegate (.toUpperCase s)))
        doubler-filter (fn [delegate s] (format "before=%s after=%s" s (delegate s)))]
    (binding [configuration (atom {:filters {:upper to-upper-filter 
                                      :doubler doubler-filter 
                                      :default [:doubler :upper]}})]
      (create-pipeline :default identity)
      (is (= (call-pipeline :default "portland") "before=portland after=PORTLAND")))))
