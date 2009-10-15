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
  #^{:doc "Track file changes with notifications"}
  cascade.change-tracker
  (:use
  	(cascade logging))
  (:import
  	(java.net URL URISyntaxException)
  	(java.io File)))
  
(def #^{:doc "Tracked values are nested maps (:file is the underlying file, :last-modified is a long, :watcher is a function of no arguments)." }
  resource-tracker (agent []))  

(def
	thread-info (atom {}))

(defn splitp
	"Splits a sequence into two sequences based on a predicate function. Returns the two sequences, first all matches, then all non-matches
	(as a two element vector). Not lazy."
	[predicate coll]
	(loop [matches []
				 non-matches []
				 queue coll]
		(if (empty? queue)
			[matches non-matches]
			(let [head (first queue)
					  tail (rest queue)
					  is-match (predicate head)]
				(if is-match
					(recur (conj matches head) non-matches tail)
					(recur matches (conj non-matches head) tail))))))
								 

(defn #^File to-file
	"Converts a URL to a file."
	[#^URL resource]
	(try
		(File. (.toURI resource))
		(catch URISyntaxException t (File. (.getPath resource)))))
		
(defn track-resource
	"Tracks a change to a resource, represented by a URL. Only resources with the \"file:\" protocol will be tracked, others will be ignored.
	 The date time modified for the resource is tracked. When it changes, the resource is un-tracked and a notification is sent to the watcher
	 (a function of no parameters)."
	[#^URL resource watcher]
	(if (= (.getProtocol resource) "file")
		(let [file (to-file resource)
				  last-modified (.lastModified file)]
			(debug "Tracking %s (last-modified %tc)" file last-modified)
		  (send resource-tracker conj { :file file :last-modified last-modified :watcher watcher }))))

(defn is-changed?
  "Determines if a tracked file's lastModified has changed."
	[{:keys [#^File file last-modified]}]
	(not= last-modified (.lastModified file)))	
				  
(defn scan-for-changed-resources*
  "Identifies any changed resources, notifying the corresponding watcher. Returns the provided resources with any changed resources
   removed."
	[resources]
	(let [[changed unchanged] (splitp is-changed? resources)]
		(doseq [{:keys [file watcher]} changed]
			(debug "Changed: %s" file)
			(try
				(watcher)
				(catch Throwable t
					(error "Watcher for %s threw %s." file t))))			
		; Return the unchanged resources	
		unchanged))
					  
(defn scan-for-changed-resources
	"Sends a function to the resource tracker to identify any files that have changed and notify their watcher."
	[]
	(send resource-tracker scan-for-changed-resources*))
						  
(defn scan-loop
	[]
	(while (not (@thread-info :shutdown))
		(scan-for-changed-resources)
		(try
			(Thread/sleep 1000)
			(catch InterruptedException e))))
									  
(defn start-scan-thread
	"Starts a thread that scans for changed resources."
	[]
	(let [thread (Thread. scan-loop "change-tracker scanner daemon")]
		(doto thread
			(.setContextClassLoader (.getContextClassLoader (Thread/currentThread)))
			(.setDaemon true))
			
		(swap! thread-info assoc :thread thread)
		
		(.start thread)))
		
(defn stop-scan-thread
	[]
	(swap! thread-info assoc :shutdown true)
	
	(.interrupt (@thread-info :thread))
	
	; Release the (soon to stop) thread to the GC.
	(swap! thread-info dissoc :thread))
	