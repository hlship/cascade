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

(ns com.howardlewisship.cascade.config)

(def configuration {})

(defn update-config
  "Updates the configuration by applying a function to a current (nested) value; key is either a single key,
  or a vector of keys, leading to the node to update."
  [key update-fn]
  (let [key-path (if (vector? key) key [key])]
    (alter-var-root (var configuration) (fn [current] (update-in current key-path update-fn)))))

(defn alter-config
  "Updates a configuration key to a new value. key is either a single key,
  or a vector of keys, leading to the node to update."
  [key value]
  (update-config key (fn [_] value)))
  
(defn add-to-config
  "Adds a value to a configuration list stored in the configuration var. The new value goes first in the identified configuration list.
  key is either a single key,
  or a vector of keys, leading to the node to update."
  [key value]
  (update-config key #(cons value %1)))



