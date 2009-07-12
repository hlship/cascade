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

; Utility functions needed by views and fragments.

(ns com.howardlewisship.cascade
  (:require (com.howardlewisship.cascade.corelib.fragments))
  (:gen-class))

(defn render-body
  "Utility that renders the body of a fragment (passed as the :render-body env key). Takes the current environment and 
  returns a seq of DOM nodes."
  [env]
  ((env :render-body) env))
  

