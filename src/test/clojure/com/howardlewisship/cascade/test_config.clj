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

(ns com.howardlewisship.cascade.test-config
  (:use
    (clojure.contrib test-is)
    com.howardlewisship.cascade.config))
    
(deftest alter-config-single
  (alter-config :single :barney)
  (is (= (@configuration :single) :barney))
  (alter-config :single :wilma)
  (is (= (@configuration :single) :wilma)))
  
(deftest alter-config-nested
  (alter-config :nested { :fred :flinstone })
  (alter-config [:nested :fred] :murray)
  (alter-config [:nested :barney] :rubble)
  (is (= (@configuration :nested) { :fred :murray :barney :rubble})))    
  
(deftest test-add-to-config
  (add-to-config :list :fred)
  (is (= (@configuration :list) [:fred]))
  (add-to-config :list :barney)
  (is (= (@configuration :list) [:barney :fred])))
  
(deftest assoc-in-config-test
  (assoc-in-config [::outer :inner] :value)
  (is (= (@configuration ::outer) { :inner :value})))
  
    