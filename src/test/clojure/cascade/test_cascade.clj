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

(ns cascade.test-cascade
  (:import
    [java.io PrintWriter CharArrayWriter])
  (:use
    cascade
    [cascade dom]
    [cascade.internal utils]
    [clojure [test :only (is are deftest)] pprint]
    [clojure.contrib [duck-streams :only [slurp*]]]))

(defn render [dom]
  (let [writer (CharArrayWriter.)]
    (render-html dom writer)
    (.toString writer)))

(defn minimize-ws [string]
  (.replaceAll string "\\s+" " "))

(defn find-classpath-resource [path]
  (.. Thread currentThread getContextClassLoader (getResourceAsStream path)))

(defn render-test
  [view-fn name & rest]
  (let [input-path (str "expected/" name ".txt")
        expected (slurp* (find-classpath-resource input-path))
        trimmed-expected (minimize-ws expected)
        dom (apply view-fn rest)
        ; _ (pprint dom)
        rendered (render dom)
        trimmed-render (minimize-ws rendered)]
    (is (= trimmed-render trimmed-expected))))

(defview ^{:custom :bit-of-meta-data} simple-view
  [env]
  :p [(env :message)])

(deftest simple-defview
  (render-test simple-view "simple-defview" {:message "Embedded Template"}))

(deftest meta-data
  (let [md (meta #'simple-view)]
    (is (= (md :name) 'simple-view) "standard meta-data")
    (is (= (md :custom) :bit-of-meta-data) "added meta-data")))

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
  (render-test attributes-view "attribute-rendering" {:message "Nested Text"
                                                      :copyright "(c) 2009 HLS"
                                                      :inner "frotz"}))

(defview special-attribute-values-view []
  :p {:class :foo :height 0 :skipped nil} ["some text"])

(deftest special-attribute-values
  (render-test special-attribute-values-view "special-attribute-values"))

(defn fetch-accounts []
  [{:name "Dewey" :id 595}
   {:name "Cheatum" :id 1234}
   {:name "Howe" :id 4328}])

(defview list-accounts
  []
  :html [
  :head [:title ["List Accounts"]]
  linebreak
  :body [
    :h1 ["List of Accounts"]
    :ul [
      (for [acct (fetch-accounts)]
        (template :li [(acct :name)] linebreak))
      ]
    ]
  ])

(deftest inline-macro
  (render-test list-accounts "inline-macro"))

(defn looper
  "Loop fragment function. Iterates over its source and updates the env with the value key before
  rendering its body (a block)."
  [env source value-key body]
  (for [value source]
    (body (assoc env value-key value))))

(defview list-accounts-with-loop [env]
  :html [
  :head [:title ["List Accounts"]]
  linebreak
  :body [
    :h1 ["List of Accounts using (block)"]
    :ul [
      ; We'll have to see to what degree block is actually useful;
      ; certainly the smple (for) version is easier. I think, ultimately,
      ; block will be more about layout components than typical
      ; dynamic rendering.
      (looper env (fetch-accounts) :acct (block [env]
        :li [(-> env :acct :name)] linebreak))
      ]
    ]
  ])

(deftest block-macro
  (render-test list-accounts-with-loop "block-macro" {}))

(defn symbol-view []
  (let [copyright (template
    linebreak :hr :p [
      (raw "&copy; 2009 ")
      :a {:href "mailto:hlship@gmail.com"} ["Howard M. Lewis Ship"]
      ] linebreak)]
    (template
      :html [
      :head [:title ["Symbol Demo"]]
      :body [
        copyright
        :h1 ["Symbol Demo"]
        copyright
        ]
      ])))

(deftest use-of-symbol
  (render-test symbol-view "use-of-symbol"))

(defview template-for-view []
  :ul [(template-for [x [1 2 3]] :li [x])])

(deftest test-template-for
  (render-test template-for-view "template-for"))

