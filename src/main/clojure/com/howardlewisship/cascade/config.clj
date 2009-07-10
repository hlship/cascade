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

(ns com.howardlewisship.cascade.config
	(:require (com.howardlewisship.cascade.corelib fragments)))

(def configuration {
  :view-namespaces []
  :fragment-namespaces ['com.howardlewisship.cascade.corelib.fragments]
  :action-namespaces []
  })

(defn add-to-config
  "Adds a value to a configuration list stored in the configuration var. The new value goes first in the identified configuration list."
  [key value]
  (alter-var-root (var configuration) (fn [current] (update-in current [key] #(cons value %1)))))
   



