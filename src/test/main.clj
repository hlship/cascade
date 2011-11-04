(ns main
  (:use compojure.core cascade ring.adapter.jetty)
  (:require
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
  (GET "/hello" [] (wrap-html hello-world))
  ; Executes with src/test as the current directory
  (initialize-assets "1.0" :public-folder "webapp")
  (route/not-found "Cascade Demo: No such resource"))

(def app
  (handler/site main-routes))

(run-jetty app {:port 8080})