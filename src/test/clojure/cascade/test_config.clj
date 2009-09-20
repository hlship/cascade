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

(ns cascade.test-config
  (:use
    (clojure test)
    cascade.config))
    
(deftest test-assoc-in-config-single
  (assoc-in-config :single :barney)
  (is (= (@configuration :single) :barney))
  (assoc-in-config :single :wilma)
  (is (= (@configuration :single) :wilma)))
  
(deftest test-assoc-in-config
  (assoc-in-config :nested { :fred :flinstone })
  (assoc-in-config [:nested :fred] :murray)
  (assoc-in-config [:nested :barney] :rubble)
  (is (= (@configuration :nested) { :fred :murray :barney :rubble})))    
  
(deftest test-add-to-config
  (add-to-config :list :fred)
  (is (= (@configuration :list) [:fred]))
  (add-to-config :list :barney)
  (is (= (@configuration :list) [:barney :fred])))
  
(deftest missing-key-on-read
  (is (thrown-with-msg? RuntimeException #"Configuration key: \(:missing-key\) was nil\." (read-config :missing-key))))
    
(deftest test-find-config
  (assoc-in-config :mykey :myvalue)
  (is (nil? (find-config :missing-key)))
  (is (= (find-config :mykey) :myvalue)))    