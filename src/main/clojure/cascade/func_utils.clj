; Copyright 2009 Howard M. Lewis Ship
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
; implied. See the License for the specific language governing permissions
; and limitations under the License.

(ns cascade.func-utils)

(defn qualified-function-name-from-meta
  [fn-meta]
  (str (ns-name (fn-meta :ns)) "/"  (fn-meta :name)))

(defn qualified-function-name
   "Accesses the meta-data for a function to extract its name and namespace, concatinated and
   returned as a string."
  [f]
  (qualified-function-name-from-meta ^f))
