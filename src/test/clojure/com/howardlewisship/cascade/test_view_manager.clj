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

(ns com.howardlewisship.cascade.test-view-manager
	(:use 
	  (com.howardlewisship.cascade view-manager)
	  (clojure.contrib test-is)))
	  
(deftest empty-namespace-list
	(is (= (namespaces-list []) "namespaces (none)")))  
	
(deftest single-namespace-list
	(is (= (namespaces-list ['app1.fragments]) "namespace app1.fragments")))
		
(deftest non-empty-namespace-list
	(is (= (namespaces-list ['app1.fragments 'core.fragments]) "namespaces app1.fragments, core.fragments")))

			