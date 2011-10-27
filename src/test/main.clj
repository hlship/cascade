(ns main
  (:use compojure.core cascade ring.adapter.jetty)
  (:require
    [compojure.route :as route]
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
  (GET "/hello" [] (wrap-html hello-world))
  (initialize-assets "1.0")
  (route/not-found "Cascade Demo: No such resource"))

(def app
  (handler/site main-routes))

(run-jetty app {:port 8080})