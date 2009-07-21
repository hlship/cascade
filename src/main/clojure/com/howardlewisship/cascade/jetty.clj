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

; Used to start an embedded Jetty server.

(ns com.howardlewisship.cascade.jetty
  (:import (org.eclipse.jetty.server Server)
           (org.eclipse.jetty.servlet ServletContextHandler)))

(defn run-jetty
	"Starts an instance of the Jetty Server running. webapp defines the folder containing
	ordinary static resources. The default for the port parameter is 8080. Returns the new Jetty Server instance."
  ([webapp] (run-jetty webapp 8080))
  ([#^String webapp port] 
  
  	(let [server (Server. port)
  		    context (doto (ServletContextHandler. server "/" false false)
  		              (.setResourceBase webapp)
  		              (.setClassLoader (.. (Thread/currentThread) getContextClassLoader))
  		              (.addFilter com.howardlewisship.cascade.filter "/*" 0))] 
  		(doto server (.setHandler context) .start .join))))
  