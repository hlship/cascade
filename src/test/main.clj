(ns main
  (:use compojure.core cascade cascade.asset cascade.import ring.adapter.jetty)
  (:require
    [cascade.request :as cr]
    [compojure.route :as route]
    [compojure.handler :as handler]))

(set! *warn-on-reflection* true)

(defn layout [req title body]
  (import-stylesheet req (file-asset "css/bootstrap.css"))
  (import-stylesheet req (classpath-asset "cascade/cascade.css"))
  (template
    :html [
    :head [:title [title]]
    :body [
      :div.container [
        :h1 [title]
        body
        :hr
        :&copy " 2011 Howard M. Lewis Ship"
        ]]]))

(defview hello-world [req]
  (layout req "Cascade Hello World"
    (template
      :div.alert-message.success [
      :p ["This page rendered at "
          :strong [(str (java.util.Date.))]
          "."
          ]
      ]
      :p [
      :a.btn.primary.large {:href "/hello"} ["Refresh"]
      ])))

(defroutes html-routes
  (GET "/hello" [] hello-world))

(defroutes master-routes
  ; Temporary: eventually we'll pass a couple of routes
  ;; into cr/initialize
  (cr/initialize "1.0"
    :public-folder "src/test/webapp"
    :html-routes html-routes)
  (route/not-found "Cascade Demo: No such resource"))

(def app
  (handler/site master-routes))

(run-jetty app {:port 8080})