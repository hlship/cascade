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

(ns #^{:doc "Utilities for building and manipulating maps"}
  cascade.map-utils
  (:use
    (cascade fail)))

(defn list-to-map-of-seqs
  "Converts a seq into a map based on a set of predetermined keys. Each form in forms is a seq; the
  first value in the seq is the key, which must match a value from keys. The remainder is the
  value for that key. This is patterned on the (ns) macro's format."
  [keys forms]
  (let [keys-set (set keys)
        extracted-keys (map first forms)
        extracted-values (map rest forms)]
    (doseq [key extracted-keys]
      (fail-unless (contains? keys-set key) (format "Key %s is not allowed; keys must be one of %s." key keys)))
    (zipmap extracted-keys extracted-values)))