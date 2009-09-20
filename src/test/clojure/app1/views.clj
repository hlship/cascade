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

(ns app1.views
  (:import (java.util Date))
  (:use cascade
        (cascade logging)
        (cascade.internal utils)))
  
(defview itworks
  {:path "working"}
  [env]
  :html [
    :head [ :title [ "It Works!"]]
    :body [
      :h1 [ "It Works!"]
      
      :p  [ 
        "Current date and time: " 
        :span { :id "time"} [ (.toString (Date.)) ]
      ]
      
      :p [
        "Let's count:"
        :ul [
          (for [x (range 10 0 -1)]
            (inline :li [ (str x) " ..." ] )
          )
        ]
      ]
      
      :hr
      
      (render-link env itworks ["extra"] { :class "nav" } "refresh")
    ]
  ])  

(declare show-counter)

(defaction increment-count
  {:path "count/increment"}
  [env] 
  [count :int]
  (send-redirect env (link env show-counter [(inc count)])))

(defn page-template
  [env title body-block]
  (inline
    :html [
      :head [ :title title ]
      :body [
        :h1 [ title ]
        (body-block env)
      ]
    ]))
          
    
(defview show-counter
  {:path "count/current"}
  [env]
  [count :int]
  (page-template env "Current Count"
    (block [env]
      :p [
        "The current count is: "
        :strong {:id "current"} [ count ]
        "."
      ]
      :p [
        "Click "
        (render-link env increment-count [ count ] "here to increment")
        "."
      ])))
