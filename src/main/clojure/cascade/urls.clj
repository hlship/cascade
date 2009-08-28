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

(ns #^{:doc "Functions used when encoding data into portions of a URL."} 
  cascade.urls
  (:import (clojure.lang Keyword Symbol)))
  
(defmulti to-url-string
  "Used to encode a value for inclusion as a query parameter value, or as extra path data.
  Dispatches on the class of the single parameter."
  class)
  
;; TODO: Encode quotes and such inside the string.  
(defmethod to-url-string String [s] s)
(defmethod to-url-string Number [#^Number n] (.toString n))

;;  Assumes that keyword and symbols names are already URL safe
(defmethod to-url-string Keyword [kw] (name kw))
(defmethod to-url-string Symbol [sym] (name sym))
