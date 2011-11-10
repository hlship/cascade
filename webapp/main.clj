(ns main
  (:use compojure.core cascade cascade.asset cascade.import ring.adapter.jetty)
  (:require
    [cascade.request :as cr]
    [compojure.route :as route]
    [compojure.handler :as handler]))

(set! *warn-on-reflection* true)

(defragment layout [title body]
  (import-stylesheet classpath-asset "cascade/bootstrap.css")
  :html [
  :head>title [title]
  :body>div.container [
    :h1 [title]
    body
    :hr
    :&copy " 2011 Howard M. Lewis Ship"
    ]])

(defview hello-world [req]
  (layout "Cascade Hello World"
    (markup
      :div.alert-message.success>p [
      "This page rendered at "
      :strong [(str (java.util.Date.))]
      "."
      ]
      :div.well [
      :a.btn.primary {:href "/hello"} ["Refresh"]
      :a.btn {:href "/hello/fail"} ["Force Failure"]
      ])))

(defroutes html-routes
  (GET "/hello" [] hello-world)
  ; /hello/fail provokes an exception:
  (GET "/hello/fail" [] (try (/ 0 0) (catch Exception e (throw (RuntimeException. "Failure dividing by zero." e))))))

(defroutes master-routes
  (cr/initialize "1.0"
    :public-folder "webapp"
    :html-routes html-routes)
  (route/not-found "Cascade Demo: No such resource"))

(def app
  (handler/site master-routes))

(run-jetty app {:port 8080})