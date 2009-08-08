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

; A filter that directs traffic to into Cascade.

(ns cascade.filter
  (:use (cascade config logging)
        (cascade.internal utils))
  (:import (javax.servlet Filter FilterChain FilterConfig ServletContext ServletRequest ServletResponse)
  (javax.servlet.http HttpServletRequest HttpServletResponse))
  (:gen-class
    :state context
    :init psuedo-constructor
    :implements [javax.servlet.Filter]))

(defn get-path
  "Extracts the complete path (including extra path info) from the request."
  [#^HttpServletRequest request]
  (let [servlet-path (.getServletPath request)
        path-info (.getPathInfo request)]
    (cond
      (nil? path-info) servlet-path
      ; This is for you Websphere 6.1, which returns the empty string for the servlet path
      ; and puts the true path into path-info
      (zero? (.length path-info)) servlet-path
    :otherwise (str servlet-path path-info))))

(defn static-file?
  "Checks to see if the request is for a static file, which is passed through to the servlet container."
  [#^ServletContext context path]
  (cond
    ; TODO: This was critical for Tapestry, but may not be needed for Cascade since it will not
    ; look like a component event request.
    (= path "/favicon.ico") true  ;  whether it exists or not!

    ; If we can get a URL to the resource inside the context then it's a real static file
    ; to be ignored.
    (.getResource context path) true
    :otherwise false))

(defn match-and-extract-dispatcher
  "Passed a path-prefix/dispatcher-fn pair, returns the dispatcher-fn if the request path
  matches the path-prefix, or returns nil otherwise."
  ;;  TODO change the path prefix to a seq of terms (i.e.[["view"]])
  [#^String path] 
  (fn [[#^String path-prefix dispatcher-fn]]
    (if (.startsWith path path-prefix) dispatcher-fn)))

(defn pass-to-dispatchers
  "Invoked from the filter to process an incoming request. Returns true if the request was processed and a response sent,
  false otherwise (i.e., forward to the servlet container for normal processing)." 
  [#^HttpServletRequest request #^HttpServletResponse response context path]
  (let [env { :servlet-api { :request request
                             :response response 
                             :context context }
              :cascade { :path path
                         :split-path (split-path path) } }
        dispatchers (remove nil? (map (match-and-extract-dispatcher path) (@configuration :dispatchers)))]
    (apply-until-non-nil dispatchers [env])))
    
;; And now the ugly side, interfacing directly with Java and creating a Java class ("filter")
;; Just couldn't bring myself to call this namespace "CascadeFilter".

(defn -psuedo-constructor
  "Initializes the 'class' of filter by providing a new atom to hold the context.
  Named this way due to the fact that Filter defines method init()."
  []
  ; No super-class constructor, state is an atom
  [[] (atom nil)])

(defn -init 
  "Filter lifecycle method used to print startup messages and capture the ServletContext."
  [this #^FilterConfig filter-config]
  
  (reset! (.context this) (.getServletContext filter-config))

  (info "Cascade Filter Startup\nConfiguration: %s" (ppstring @configuration))

  ; TODO: require a set of clojure namespaces defined in web.xml, if available 

  nil)

(defn -destroy 
  [this]
  nil)

(defn -doFilter 
  [this #^ServletRequest request #^ServletResponse response #^FilterChain chain]
  (debug "Filtering %s" request)
  (let [path (get-path request)
        context @(.context this)]
  (when (or 
        (static-file? context path) 
        (not (pass-to-dispatchers request response context path)))
    ; Let the servlet container process this normally.
    (debug "Not handled; forwarding to next in chain")
    (.doFilter chain request response))))



