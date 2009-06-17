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

(ns app1.fragments
  (:use com.howardlewisship.cascade.dom))

(defn echo
  [env params]
  ; TODO: Allow a fragment function to return a single dom-node, not an array of dom-nodes
  [(struct-map dom-node :type :text
                        :value (str (params :value)))])