(ns main
  (:use compojure.core cascade ring.adapter.jetty)
  (:require [compojure.route :as route]
    [compojure.handler :as handler]))

(defview hello-world [request]
  :html [:head [:title ["Cascade Hello World"]]
         :body [
    :h1 ["Hello World"]
    :p [
      "The page rendered at "
      :em [(str (java.util.Date.))]
      "."
      ]
    :p [
      :a {:href "/"} ["click to refresh"]
      ]
    ]])

(defroutes main-routes
  (GET "/" [] (wrap-html hello-world))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (handler/site main-routes))

(run-jetty app {:port 8080})