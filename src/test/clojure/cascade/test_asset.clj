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

(ns cascade.test-asset
	(:use
		(cascade asset)
		(clojure test)))
		
(deftest blacklist
	(is (is-allowed-path "foo/bar.baz"))
	
	(are [path]
		(is 
			(thrown-with-msg? RuntimeException #".*on the blacklist.*"
			(fail-if-blacklisted path)))
		"any-package/any-class.class"
		"any-package/any-source.clj"))	

(deftest missing-classpath-asset
	(is
		(thrown-with-msg? RuntimeException #".*'missing\.file' not found.*"
			(get-classpath-asset "missing.file"))))
			
(deftest get-classpath-asset-enforces-blacklist
	(is
		(thrown? RuntimeException
			(get-classpath-asset "java/lang/Object.class"))))
