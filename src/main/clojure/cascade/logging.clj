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

(ns #^{:doc "Wrappers around Simple Logging Facade for Java (SLF4J)"}
  cascade.logging
  (:import (org.slf4j LoggerFactory Logger)))

(defn #^Logger get-logger [#^String name]
  (LoggerFactory/getLogger name))

(defmacro log*
  [check-fn log-fn fmt & args]
  (let [logger-name (name (ns-name *ns*))]
    `(let [logger# (get-logger ~logger-name)]
      (and
        (. logger# ~check-fn)
        (. logger# ~log-fn (format ~fmt ~@args))))))

(defmacro debug
  [fmt & args]
  `(log* isDebugEnabled debug ~fmt ~@args))

(defmacro info
  [fmt & args]
  `(log* isInfoEnabled info ~fmt ~@args))

(defmacro error
  [fmt & args]
  `(log* isErrorEnabled error ~fmt ~@args))