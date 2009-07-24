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

(ns #^{:doc "Clojure wrappers around Simple Logging Facade for Java (SLF4J)"} 
  com.howardlewisship.cascade.logging
  (:import (org.slf4j LoggerFactory Logger)))
  
(defn #^Logger get-logger [#^String name]
  (LoggerFactory/getLogger name))  

; My Macro-Fu is inadequate to avoid cut-and-paste here. Also the Logger interface makes it hard
; (there's no log-at-level method, so you have to invoke specific methods) and the . special form
; doesn't support dynamically calling a method, etc., etc.

(defmacro debug
  [format & args]
  (let [logger-name (name (ns-name *ns*))]
    `(let [logger# (get-logger ~logger-name)]
      (and (.isDebugEnabled logger#)
        (.debug logger# (format ~format ~@args))))))
        
(defmacro info
  [format & args]
  (let [logger-name (name (ns-name *ns*))]
    `(let [logger# (get-logger ~logger-name)]
      (and (.isInfoEnabled logger#)
        (.info logger# (format ~format ~@args))))))        
        
