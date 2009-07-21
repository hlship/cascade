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

; A filter that directs traffic to Clojure (to determine if processing is necessary).

(ns com.howardlewisship.cascade.filter
  (:use (com.howardlewisship.cascade config))
  (:import (javax.servlet Filter FilterChain ServletRequest ServletResponse)
           (javax.servlet.http HttpServletRequest HttpServletResponse))
  (:gen-class
   :implements [javax.servlet.Filter]))

(declare pass-to-dispatchers)

(defn -init 
	[this filter-config] 
	nil)

(defn -destroy 
  [this]
	nil)

(defn -doFilter 
	[this #^ServletRequest request #^ServletResponse response #^FilterChain chain]
  (if (not (pass-to-dispatchers request response))
  	(.doFilter chain request response)))
  	
; Ah ... and back into the world of Clojure
  	
(defn match-and-extract-dispatcher
   [path]
   (fn [[path-prefix dispatcher-fn]] (if (.startsWith path path-prefix) dispatcher-fn)))   	
  	
(defn pass-to-dispatchers
  "Invoked from the filter to process an incoming request." 
	[#^HttpServletRequest request #^HttpServletResponse response]
	(println request)
	(let [path (str (.getServletPath request) (.getPathInfo request))
	      env { :servlet-api { :request request :response response :path path } }
	      dispatchers (remove nil? (map (match-and-extract-dispatcher path) (configuration :dispatchers)))]
	  ; working hard, despite laziness, to avoid evaluating a dispatcher-fn after the first one to return true
	  (loop [queue dispatchers]
	    (cond
	      (empty? queue) nil
	      ((first queue) env) true
	      :otherwise (recur (rest queue))))))
	      