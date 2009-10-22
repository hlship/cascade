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

(ns 
  #^{:doc "Global configuration"}
  cascade.config
  (:use cascade.fail))

(defn get-boolean-system-property
	[property-name default-value]
	(let [value (System/getProperty property-name)]
		(if value
			(Boolean/valueOf value)
			default-value)))

(def configuration (atom {
  :jquery-path "cascade/jquery-1.3.2.js"
  :production-mode (get-boolean-system-property "cascade.production-mode" true)
  }))

(defn- to-key-path
  [key]
  (if (vector? key) key [key]))

(defn update-config
  "Updates the configuration by applying a function to a current (nested) value; key is either a single key,
  or a vector of keys, leading to the node to update."
  [key update-fn]
  (swap! configuration (fn [current] (update-in current (to-key-path key) update-fn))))
  
(defn add-to-config
  "Adds a value to a configuration list stored in the configuration var. The new value goes first in the identified configuration list.
  key is either a single key,
  or a vector of keys, leading to the node to update."
  [key value]
  (update-config key #(cons value %1)))

(defn assoc-in-config
  "Associates a value into a map inside a configuration. The key is either a single key or a vector of keys."
  [key value]
  (swap! configuration (fn [current] (assoc-in current (to-key-path key) value))))
  
(defn find-config
  "Obtains a value inside the configuration from a nested set of keys. key may be a single key or a seq of keys. May return nil."
  [key]
  (get-in @configuration (to-key-path key)))
  
(defn read-config
  "Obtains a value inside the configuration from a key or seq of keys. Throws RuntimeException if the value could not be found."
  [key]
  (let [result (find-config key)]
  	(if (nil? result)
	    (fail "Configuration key %s was nil." key))
	  result))

