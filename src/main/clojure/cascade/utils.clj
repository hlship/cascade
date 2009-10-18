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

(ns cascade.utils
	(:use
		(clojure.contrib pprint macro-utils)))

(defmacro lcond
	"A reimplementation of Clojure's cond, with the addition of a special :let
	keyword that injects an implicit let into the logic."
	[& clauses]
	(when clauses
		(if (= 1 (count clauses))
			(throw (IllegalArgumentException. "lcond requires an even number of forms")))
			(let
				[tst (first clauses)
				 expr (second clauses)
				 rst (next (next clauses))]
				 (if (= tst :let)
				 	`(let [~@expr] (lcond ~@rst))
				 	`(if ~tst ~expr (lcond ~@rst))))))

(defmacro debug-form
	"Expands all macros in a form, recursively, and prints the result. Returns nil."
	[form]
	`(pprint (mexpand-all '~form))) 
