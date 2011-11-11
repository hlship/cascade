; This is a temporary file that allows the use of "lein ring server"
; to test the framework with a sample app.

(ns main
  (:use compojure.core cascade cascade.asset cascade.import ring.adapter.jetty)
  (:require
    :reload
    [cascade.request :as cr]
    [ring.util.response :as response]
    [compojure.route :as route]
    [compojure.handler :as handler]))

(set! *warn-on-reflection* true)

(defragment layout [title body]
  (import-stylesheet classpath-asset "cascade/bootstrap.css")
  (import-stylesheet file-asset "app.css")
  :html [
  :head>title [title]
  :body>div.container [
    :h1 [title]
    body
    :hr
    :&copy " 2011 Howard M. Lewis Ship"
    ]])

(defview hello-world [req]
  (javascript-invoke ["cascade/bootstrap-twipsy" "cascade/bootstrap-popover"] "#force-failure" "popover")
  (layout "Cascade Hello World"
    (markup
      :div.alert-message.success>p [
      "This page rendered at "
      :strong [(str (java.util.Date.))]
      "."
      ]
      :div.well [
      :a.btn.primary.large {:href "/hello"} ["Refresh"] " "
      :a.btn {:href "/cascade/grid"} ["Grid Demo"] " "
      :a.btn.danger#force-failure {:href "/hello/fail"
                                   :title "Caution!"
                                   :data-content "We force a divide by zero error to see Cascade's exception report."} ["Force Failure"]
      ])))

(defn grid-url
  [width height]
  (format "/cascade/grid/%d/%d" width height))

(defragment add-button [
  width height]
  :a.btn {:href (grid-url width height)} ["+"])

(defview grid [width height]
  (layout "Grid Demo"
    (markup
      :div.well>a.btn.primary.large {:href "/hello"} ["Main"]
      :h2 [width "x" height " Grid"]
      :table.bordered-table.zebra-striped [
      :thead>tr [
        :th
        (markup-for [column (range 1 (inc width))]
          :th ["Column " column])
        :th [(add-button (inc width) height)]
        ]
      :tbody [
        (markup-for [row (range 1 (inc height))]
          :tr [
          :td ["# " row]
          (markup-for [column (range 1 (inc width))]
            :td>a {:href (grid-url column row)} ["Cell " column "x" row])
          :td [:&nbsp]
          ])
        ]
      :tfoot>tr [
        :td [(add-button width (inc height))]
        (markup-for [column (range (inc width))] :td [:&nbsp])
        ]
      ])))

(defn parse-int [string]
  (Integer/parseInt string))

(defroutes html-routes
  (GET "/hello" [] hello-world)
  (GET "/cascade/grid" [] (grid 3 5))
  (GET "/cascade/grid/:width/:height" [width height] (grid (parse-int width) (parse-int height)))
  ; /hello/fail provokes an exception:
  (GET "/hello/fail" [] (try (/ 0 0) (catch Exception e (throw (RuntimeException. "Failure dividing by zero." e))))))

(defroutes master-routes
  (cr/initialize "1.0"
    :public-folder "webapp"
    :html-routes html-routes)
  (ANY "/" [] (response/redirect "/hello"))
  (route/not-found "Cascade Demo: No such resource"))

(def app
  (handler/site master-routes))
