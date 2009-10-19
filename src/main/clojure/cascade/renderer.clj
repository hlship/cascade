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

(ns #^{:doc "Render objects as markup"}
  cascade.renderer
  (:import
    (javax.servlet ServletContext)
  	(javax.servlet.http HttpServletRequest HttpSession)
  	(java.util Enumeration))
  (:use
  	cascade))

(defn enumeration-to-seq
  [#^Enumeration enumeration]
  (loop [result []]
  	(if (.hasMoreElements enumeration)
  		(recur (conj result (.nextElement enumeration)))
  		result))) 
  

(defmulti render
	"Renders an arbitrary object (identified by its type) as markup. This is often needed
	 by the exception report page."
	 class)
	 
(defmethod render Object
	[obj]
	(str obj))	 
	 
(defn render-values
  [values]
  (if (= 1 (count values))
  	(first values)
  	(template 
  		:ul [
  			(template-for [value values]
  				:li [ value ])
  		])))
  		 
(defn data-section
	[title]
	(template :div {:class :c-env-data-section } [ title ]))
	  		 
(defn definition-list
	[& terms]
	(template :dl [
		(template-for [[term value] (partition 2 terms)]
			:dt [ term] :dd [ value ])
		]))
			  		 
(defn render-parameters	 
	[#^HttpServletRequest request]
  (let [parameter-names (sort (seq (.. request getParameterMap keySet)))]
	  (when-not (empty? parameter-names)
	  	(template
	  		(data-section "Query Parameters")
	  		:dl [
					(template-for [name parameter-names] 
						:dt [ name ]
						:dd [ (render-values (seq (.getParameterValues request name))) ])
				]))))
		
(defn render-headers
 	[#^HttpServletRequest request]
 	(let [header-names (sort (enumeration-to-seq (.getHeaderNames request)))]
 		(template
 			(data-section "Headers")
 			:dl [
 			  (template-for [name header-names]
 			  	:dt [ name ]
 			  	:dl [ (render-values (enumeration-to-seq (.getHeaders request name))) ])
 			])))

(defn render-attributes
	[keys-enumeration access-fn]
	(let [sorted-names (sort (enumeration-to-seq keys-enumeration))]
	  (when-not (empty? sorted-names)
			(template
				(data-section "Attributes")
				:dl [
					(template-for [name sorted-names]
						:dt [ name ]
						:dd [ (render (access-fn name)) ])
				]))))		
										
(defmethod render HttpServletRequest
  [#^HttpServletRequest request]
  (let [context-path (.getContextPath request)]
  	(template  
	  	(definition-list
	  		  	
	  		"Context Path" (if-not (= context-path "")
	  	    	context-path
	  	    	(template :em [ "none (deployed as root)" ]))
	  	
	  		"Request URI" (.getRequestURI request)
	  		
	  		"Locale" (str (.getLocale request))
	  		
	  		"Secure" (str (.. request isSecure))
	  		
	  		"Server Name" (.getServerName request))
		  	    	
			(render-parameters request)
	  	(render-headers request)
	  	(render-attributes (.getAttributeNames request) #(.getAttribute request %)))))	
						 
(defmethod render ServletContext
	[#^ServletContext context]
	(template
		(definition-list
			"Version"  (str (.getMajorVersion context) "." (.getMinorVersion context))
			
			"Server Info" (.getServerInfo context)
			
			"Context Name" (.getServletContextName context))

	(render-attributes (.getAttributeNames context) #(.getAttribute context %))))
		 
(defn
	to-date
	[long-date]
	(format "%tc" long-date))
		 
(defmethod render HttpSession
	[#^HttpSession session]
	(template
		(definition-list
			"Id" (.getId session)
			"New" (str (.isNew session))
			"Creation Time" (to-date (.getCreationTime session))
			"Last Accessed" (to-date (.getLastAccessedTime session)))

    (render-attributes (.getAttributeNames session) #(.getAttribute session %))))
		