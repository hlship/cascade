(ns main
  (:use compojure.core cascade cascade.asset ring.adapter.jetty)
  (:require
    [cascade.request :as cr]
    [compojure.route :as route]
    [compojure.handler :as handler]))

(set! *warn-on-reflection* true)

(defview hello-world [request]
  :html
  [:head [:title ["Cascade Hello World"]
          (stylesheet (file-asset "css/bootstrap.css"))
          (stylesheet (classpath-asset "cascade/cascade.css"))]
   :body [
    :h1 ["Hello World"]
    :p [
      "The page rendered at "
      :em [(str (java.util.Date.))]
      "."
      ]
    :p [
      :a {:href "/hello"} ["click to refresh"]
      ]
    ]])

(defroutes main-routes
  ; Temporary: eventually we'll pass a couple of routes
  ;; into cr/initialize
  (GET "/hello" [] (cr/wrap-html hello-world))
  (cr/initialize "1.0" :public-folder "src/test/webapp")
  (route/not-found "Cascade Demo: No such resource"))

(def app
  (handler/site main-routes))

(run-jetty app {:port 8080})