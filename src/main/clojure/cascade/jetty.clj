; Copyright 2009, 2010 Howard M. Lewis Ship
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

(ns
  ^{:doc "Run Jetty server embedded"}
  cascade.jetty
  (:use
    [clojure.contrib str-utils])
  (:require [cascade.filter])
  (:import
    [cascade.filter CascadeFilter]
    [org.eclipse.jetty.server Server]
    [org.eclipse.jetty.servlet ServletContextHandler FilterMapping FilterHolder DefaultServlet]))

(defn start-jetty-server
  "Like run-jetty, but does not join the started server; this is used as part of
   integration testing a Cascade application. Returns the started Jetty Server instance."
  [^String webapp port & namespaces]
  (let [server (Server. port)
        context (ServletContextHandler. server "/" true false)
        namespace-list (str-join "," (map name namespaces))
        holder (FilterHolder. (CascadeFilter. (atom [])))]
    (.setInitParameter holder "cascade.namespaces" namespace-list)
    (doto context
      (.setResourceBase webapp)
      #_ (.setClassLoader (.. (Thread/currentThread) getContextClassLoader))
      ; Add a servlet to vend out ordinary static files. Also, filters don't work without a servlet to
      ; filter around!
      (.addServlet DefaultServlet "/*")
      (.addFilter holder "/*" FilterMapping/DEFAULT))
    (doto server 
      (.setHandler context) 
      .start)))  

(defn run-jetty
  "Starts an instance of the Jetty Server running.
  webapp defines the folder containing ordinary static resources (the docroot).
  Returns the new Jetty Server instance. The instance is started with session support, but
  without security support. No web.xml is necessary, a
  cascade.filter will automatically be installed. Any provided namespace-symbols will be
  loaded at filter startup (this is to specify namespaces containing views and actions)."
  [^String webapp port & namespaces]
  (let [server (apply start-jetty-server webapp port namespaces)]
    (.join server)
    server)) 