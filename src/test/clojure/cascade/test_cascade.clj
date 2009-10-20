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

(ns cascade.test-cascade
  (:import
    (javax.servlet.http HttpServletRequest HttpServletResponse)
    (java.io PrintWriter CharArrayWriter))
  (:use 
    cascade
    (cascade dom mock)
    (cascade.internal utils)
    (clojure (test :only [is are deftest]))
    (clojure.contrib duck-streams pprint)))

(defn render
  [dom]
  (let [writer (CharArrayWriter.)]
    (render-xml dom writer)
    (.toString writer)))
  
(defn minimize-ws [string]
  (.replaceAll string "\\s+" " "))

(defn render-test
  [view-fn name env]
  (let [input-path (str "expected/" name ".xml")
        expected (slurp* (find-classpath-resource input-path))
        trimmed-expected (minimize-ws expected)
        dom (view-fn env)
        ; _ (pprint dom)
        rendered (render dom)
        trimmed-render (minimize-ws rendered)]
      (is (= trimmed-render trimmed-expected))))


(defview #^{:custom :bit-of-meta-data} simple-view
  [env] 
  :p [ (env :message) ])

(deftest simple-defview
  (render-test simple-view "simple-defview" {:message "Embedded Template"}))

(deftest meta-data
  (let [md ^#'simple-view]
    (is (= (md :name) 'simple-view) "standard meta-data")
    (is (= (md :custom) :bit-of-meta-data) "added meta-data")
    (is (= (md :cascade-type) :view) "cascade-added meta-data")))
  
(defview attributes-view
  [env]
  :p {:id "outer"} [
    :em {:id (env :inner)} [
      (env :message)
     ]
     linebreak
     :hr
     (env :copyright)
  ])  
  
(deftest attribute-rendering
  (render-test  attributes-view "attribute-rendering" {:message "Nested Text" 
    :copyright "(c) 2009 HLS"
    :inner "frotz"}))
  
(defview special-attribute-values-view
   [env]
   :p {:class :foo :height 0 :skipped nil } [ "some text" ])
   
(deftest special-attribute-values
  (render-test special-attribute-values-view "special-attribute-values" nil))  
  
  
(defn fetch-accounts
  [env]
  [{:name "Dewey" :id 595}
   {:name "Cheatum" :id 1234}
   {:name "Howe" :id 4328}])
   
(defview list-accounts
  [env]
  :html [
    :head [ :title [ "List Accounts" ] ]
    linebreak
    :body [
      :h1 [ "List of Accounts" ]
      :ul [
        (for [acct (fetch-accounts env)]
          (template :li [ (acct :name) ] linebreak))
      ]
    ]
  ])

(deftest inline-macro
    (render-test list-accounts "inline-macro" nil))
    
    
; Ugh: all the good names are taken by clojure.core!

(defn looper
  "Loop fragment function. Iterates over its source and updates the env with the value key before
  rendering its body (a block)."
  [env source value-key body]
  (for [value source]
       (body (assoc env value-key value))))
       
(defview list-accounts-with-loop
  {:path "accounts/list" }
  [env]
  :html [
    :head [ :title [ "List Accounts" ] ]
    linebreak
    :body [
      :h1 [ "List of Accounts using (block)" ]
      :ul [
        ; We'll have to see to what degree block is actually useful;
        ; certainly the smple (for) version is easier. I think, ultimately,
        ; block will be more about layout components than typical
        ; dynamic rendering.
        (looper env (fetch-accounts env) :acct (block [env]
          :li [ (-> env :acct :name) ] linebreak))
      ]
    ]
  ])            
       
(deftest block-macro
  (render-test list-accounts-with-loop "block-macro" nil))
  
(defn symbol-view
  [env]
  (let [copyright (template  
    linebreak :hr :p [ 
      (raw "&copy; 2009 ") 
      :a {:href "mailto:hlship@gmail.com"} [ "Howard M. Lewis Ship" ]
    ] linebreak)]
    (template
      :html [ 
        :head [ :title [ "Symbol Demo" ]]
        :body [
          copyright
          :h1 [ "Symbol Demo" ]
          copyright
        ]
      ]
    )))

(deftest use-of-symbol
  (render-test symbol-view "use-of-symbol" nil))

(defview template-for-view
  [env]
  :ul [
    (template-for [x [1 2 3]] :li [ x ])
  ])

(deftest test-template-for
  (render-test template-for-view "template-for" nil))

(defview fn-with-meta "doc meta" {:other-meta :data} [env] :p)

(deftest meta-data-for-function
  (are [k v]
    (= (^#'fn-with-meta k) v)
    :doc "doc meta"
    :other-meta :data
    :cascade-type :view))
  
(deftest test-link-map
  (are [f extra-path query-params expected-path]
    (let [link (link-map-from-function f extra-path query-params)]
      (is (= (link :path) expected-path))
      (is (= (link :parameters) query-params)))
      
      list-accounts-with-loop nil nil "accounts/list"
      
      list-accounts-with-loop [5050 true] nil "accounts/list/5050/true"
      
      simple-view nil nil "view/cascade.test-cascade/simple-view"
      
      simple-view nil {:foo :bar} "view/cascade.test-cascade/simple-view"))  
  
(deftest test-link
  (with-mocks [request HttpServletRequest
               response HttpServletResponse]
    (:train 
      (expect .getContextPath request "/ctx")
      (expect .encodeURL response "/ctx/accounts/list" "*encoded*"))
    (:test
      (is (= (link {:servlet-api {:request request :response response}} list-accounts-with-loop)
             "*encoded*")))))  