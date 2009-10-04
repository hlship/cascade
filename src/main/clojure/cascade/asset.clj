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

(ns 
  #^{:doc "Asset management"}
  cascade.asset
  (:import
  	(javax.servlet ServletContext) 
  	(javax.servlet.http HttpServletRequest HttpServletResponse))
  (:require 
  	(clojure.contrib (str-utils2 :as s2)))
  (:use 
    (clojure.contrib duck-streams)
  	(cascade config fail path-map logging)))
  

(assoc-in-config :application-version
  (Long/toHexString (System/nanoTime)))
  
;; Assets represents files stored either under the web context or stored within the classpath.
;; Identifying an asset will eventually incorporate a locale-specific search. Ultimately,
;; assets will be (optionally) GZip compressed, for clients which support GZip compression.
;; For JavaScript assets, there will eventually be a aggregation option.
;; Once an asset is located, it is represented as a map, with keys:
;;   :type (:context or :classpath)
;;   :path string -- path relative to the "root" (the web context, or the root of the classpath), no leading slash

;; TODO: Extra keys for date time modified, file size, mime type?

;; TODO: lots of caching

;; TODO: Whitelist of regexps to allow access to classpath assets. Maybe
;; a blacklist as well.

(def #^ClassLoader context-class-loader (.getContextClassLoader (Thread/currentThread)))

(defn get-classpath-asset
  "Locates an asset on the classpath. Throws RuntimeException if the asset does not exist.
  Returns an asset map. The path is relative to the classpath root and should not
  start with a slash."
  [path]
  (let [asset-url (.getResource context-class-loader path)]
    (fail-if (nil? asset-url) "Asset '%s' not found on the classpath." asset-url)
    ;; TODO: Implement whitelist, verify that path is valid on the whitelist.
    { :type :classpath
      :path path
      ::URL asset-url }))
    
(defmulti to-asset-path
  "Converts an asset map into a URL path that can be used by the client to obtain the contents of the asset."
  #(:type %2))
  
(defmethod to-asset-path :classpath
  [env asset-map]
  (let [#^HttpServletRequest request (-> env :servlet-api :request)
        ;; TODO: Servlet 2.5 defines a ServletContext.getContextPath()
        context-path (.getContextPath request)]
        (str context-path "/asset/classpath/" (read-config :application-version) "/" (asset-map :path))))      
  
  
(defn get-mime-type 
  "Determine the MIME type of the path."  
  [env path]
  (let [#^ServletContext context (-> env :servlet-api :context)]
    (or
      (.getMimeType context path)
      ;; TODO: internal configuration lookup
      "application/octet-stream")))  

(defn classpath-asset-dispatcher
  [env]
  (let [#^HttpServletResponse response (-> env :servlet-api :response)
        [_ _ application-version & asset-path-terms] (-> env :cascade :split-path)
        asset-path (s2/join "/" asset-path-terms)
    		_ (fail-if (nil? application-version) "Invalid classpath asset URL.")
    		_ (debug "Application version: %s, expect %s" application-version (read-config :application-version))
    		_ (fail-unless (= application-version (read-config :application-version)) "Incorrect application version.")    		
    		; TODO Check whitelist/blacklist
    		; TODO Gzip, etc.
    		asset-url (.getResource context-class-loader asset-path)
    		_ (fail-if (nil? asset-url) "Could not locate classpath asset %s." asset-path)
    	  mime-type (get-mime-type env asset-path)
        output-stream (.getOutputStream (doto response (.setContentType mime-type)))]
        (with-open [input-stream (.openStream asset-url)]
          (copy input-stream output-stream)
          (.flush output-stream)))
          ; If we got this far, we copied the contents (or failed, throwing an exception)
          true)
            
(add-function-to-config :dispatchers "asset/classpath" #'classpath-asset-dispatcher)
