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

(ns #^{:doc "Define filters and chains for extending Cascade"}
  cascade.pipeline
  (:use
    (cascade config fail logging)
    (cascade.internal utils)))

(defmacro decorate
  "Decorates a function with a filter. The filter will be passed the decorated function
   as its first argument (called the 'delegate'), and all remaining arguments after that. The filter may
   decide what parameters to pass to the delegate (or to not invoke the delegate at all), and
   may also apply other concerns, such as exception handling, or conversion of return values.  A function
   may be decorated multiple times, with later filters wrapping around the earlier ones."
  [decorated-fn filter-fn]
  `(let [var#  #'~decorated-fn
         delegate# (deref var#)
          bridge# (fn [& args#] (apply ~filter-fn delegate# args#))]
    (alter-var-root var# (constantly bridge#))))