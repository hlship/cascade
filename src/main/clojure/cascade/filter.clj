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
  #^{:doc "Servlet API Filter that directs requests into Cascade"}
  cascade.filter
  (:require
    (cascade exception))
  (:use
    (clojure stacktrace)
    (clojure.contrib str-utils)
    (cascade config change-tracker dispatcher logging path-map pipeline urls exception)
    (cascade.internal utils))
  (:import
    (javax.servlet Filter FilterChain FilterConfig ServletContext ServletRequest ServletResponse)
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
    ; Incoming requests for the context end up with a path of "/", which WILL be considered
    ; a static file. TODO: need to extend this for subdirs?
    (= path "/") false
    ; Always treat this as a static file even if it doesn't exist; otherwise is routes
    ; requests to the function mapped to "/"!
    (= path "/favicon.ico") true
    ; If we can get a non-nil URL to the resource inside the context then it's a real static file
    ; to be ignored.
    (.getResource context path) true
    :otherwise false))

(defn handle-request-exception
  "Prints the exception (including stack trace) to the console then renders the exception-report view."
  [env exception]
  (debug "Request exception: %s" exception)
  (let [#^Throwable root (root-cause exception)]
    (.printStackTrace root))
  (try
    (render-view (assoc-in env [:cascade :exception] exception) #'exception-report)
    (catch Exception e
      (error "Unable to render exception report view: %s" e)
      (.printStackTrace e))))    

(defn pass-to-dispatchers
  "Invoked from the filter to process an incoming request. Returns true if the request was processed and a response sent,
  false otherwise (i.e., forward to the servlet container for normal processing)."
  [#^HttpServletRequest request #^HttpServletResponse response context path]
  (let [split (split-path path)
        env { :servlet-api { :request request
                             :response response
                             :context context }
              :cascade { :path path
                         :split-path split } }]
    (try
      (let [matches (find-matching-functions :dispatchers split)
            dispatchers (for [[path function] matches] function)]
        (apply-until-non-nil dispatchers [env]))
      (catch Throwable t (handle-request-exception env t)))))

;; And now the ugly side, interfacing directly with Java and creating a Java class ("filter")
;; Just couldn't bring myself to call this namespace "CascadeFilter".

(defn -psuedo-constructor
  "Initializes the 'class' of filter by providing a new atom to hold the context.
  Named this way due to the fact that Filter defines method init()."
  []
  ; No super-class constructor, state is an atom whichs stores the servlet context
  [[] (atom nil)])

(defn -init
  "Filter lifecycle method used to print startup messages and capture the ServletContext."
  [this #^FilterConfig filter-config]
  (let [context (.getServletContext filter-config)]
    (assoc-in-config :servlet-context context)
    (reset! (.context this) context))
  (info "Cascade startup")
  (let [namespace-list (.getInitParameter filter-config "cascade.namespaces")
        namespace-names (and namespace-list (re-split #"," namespace-list))]
    (doseq [#^String ns-name namespace-names]
      (info "Loading namespace: %s" ns-name)
      (require (symbol (.trim ns-name)))))
  (start-scan-thread)
  (info "Configuration:\n%s" (ppstring @configuration))
  nil)

(defn -destroy
  [this]
  (stop-scan-thread)
  (debug "Cascade shutdown")
  nil)

(defn -doFilter
  [this #^ServletRequest request #^ServletResponse response #^FilterChain chain]
  (let [path (get-path request)
        qs (.getQueryString request)
        debug-path (if (nil? qs) path (str path "?" qs))
        context @(.context this)
        start-time (System/currentTimeMillis)]
    (debug "Filtering %s" debug-path)
    (if (or
            (static-file? context path)
            (not (pass-to-dispatchers request response context path)))
      ; Let the servlet container process this normally.
      (do
        (debug "Not handled; forwarding to next in chain")
        (.doFilter chain request response))
      (debug "Request procesing time: %,d ms" (- (System/currentTimeMillis) start-time)))))