; Copyright 2009, 2010 Howard M. Lewis Ship
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
(ns
  ^{:doc "Assert-like failures"}
  cascade.fail)

(defn fail
  "A quick way to throw a RuntimeException."
  [^String fmt & args]
  (let [^String msg (apply format fmt args)]
    (throw (RuntimeException. msg))))

(defmacro fail-unless
  "Throws a runtime exception if the condition is false. The message is generated from the format
  and additional arguments and is only evaluated if a failure occurs."
  [condition fmt & args ]
  `(if-not ~condition (fail ~fmt ~@args)))

(defmacro fail-if
  "Throws a runtime exception if the condition is true.  The message is generated from the format
  and additional arguments and is only evaluated if a failure occurs."
  [condition fmt & args]
  `(if ~condition (fail ~fmt ~@args)))