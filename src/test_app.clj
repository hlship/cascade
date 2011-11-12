; This is a temporary file that allows the use of "lein ring server"
; to test the framework with a sample app.

(ns test-app
  (:use compojure.core cascade cascade.asset cascade.import
    [hiccup core page-helpers])
  (:require
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

(defn alert-message [level body]
  (javascript-invoke ["cascade/bootstrap-alerts"] ".alert-message" "alert")
  (markup
    :div {:class [:alert-message level] :data-alert :alert} [
    :a.close {:href "#"} ["x"]
    :p [body]
    ])
  )

(defview hello-world [req]
  (javascript-invoke ["cascade/bootstrap-twipsy" "cascade/bootstrap-popover"] "#force-failure" "popover")
  (layout "Cascade Hello World"
    (markup
      (alert-message :warning
        (markup
          :p [
          "This page rendered at "
          :strong [(str (java.util.Date.))]
          "."
          ]))
      :div.well [
      :a.btn.primary.large {:href "/hello"} ["Refresh"] " "
      :a.btn {:href "/cascade/grid"} ["Cascade Grid Demo"] " "
      :a.btn {:href "/hiccup/grid"} ["Hiccup Grid Demo"] " "
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

(defn hiccup-layout [title & body]
  ; (import-stylesheet classpath-asset "cascade/bootstrap.css")
  ; (import-stylesheet file-asset "app.css")

  [:html
   [:head
    (include-css "/assets/1.0/classpath/cascade/bootstrap.css")
    [:title title]]
   [:body
    [:div.container
     [:h1 title]
     body
     [:hr]
     "&copy; 2011 Howard M. Lewis Ship"
     ]
    ]])

(defn hgrid-url
  [width height]
  (format "/hiccup/grid/%d/%d" width height))

(defn hadd-button [
  width height]
  [:a.btn {:href (hgrid-url width height)} "+"])


(defhtml hiccup-grid [width height]
  (html
    (hiccup-layout "Hiccup Grid Demo"
      [:div.well [:a.btn.primary.large {:href "/hello"} "Main"]]
      [:h2 width "x" height " Grid"]
      [:table.bordered-table.zebra-striped
       [:thead
        [:tr
         [:th
          (for [column (range 1 (inc width))]
            [:th "Column " column])
          [:th (hadd-button (inc width) height)]
          ]
         ]
        ]
       [:tbody
        (for [row (range 1 (inc height))]
          [:tr
           [:td "# " row]
           (for [column (range 1 (inc width))]
             [:td
              [:a {:href (hgrid-url column row)} "Cell " column "x" row]])
           [:td "&nbsp;"]
           ])
        ]
       [:tfoot
        [:tr
         [:td (hadd-button width (inc height))]
         (for [column (range (inc width))]
           [:td "&nbsp;"])
         ]]
       ])))

;      :div.well>a.btn.primary.large {:href "/hello"} ["Main"]
;      :h2 [width "x" height " Grid"]
;      :table.bordered-table.zebra-striped [
;      :thead>tr [
;        :th
;        (markup-for [column (range 1 (inc width))]
;          :th ["Column " column])
;        :th [(add-button (inc width) height)]
;        ]
;      :tbody [
;        (markup-for [row (range 1 (inc height))]
;          :tr [
;          :td ["# " row]
;          (markup-for [column (range 1 (inc width))]
;            :td>a {:href (grid-url column row)} ["Cell " column "x" row])
;          :td [:&nbsp]
;          ])
;        ]
;      :tfoot>tr [
;        :td [(add-button width (inc height))]
;        (markup-for [column (range (inc width))] :td [:&nbsp])
;        ]
;      ])))

(defn parse-int [string]
  (Integer/parseInt string))

(defroutes html-routes
  (GET "/hello" [] hello-world)
  (GET "/cascade/grid" [] (grid 3 5))
  (GET "/cascade/grid/:width/:height" [width height] (grid (parse-int width) (parse-int height)))
  ; /hello/fail provokes an exception:
  (GET "/hello/fail" [] (try (/ 0 0) (catch Exception e (throw (RuntimeException. "Failure dividing by zero." e))))))

(defroutes master-routes
  (GET "/hiccup/grid" [] (hiccup-grid 3 5))
  (GET "/hiccup/grid/:width/:height" [width height] (hiccup-grid (parse-int width) (parse-int height)))
  (cr/initialize "1.0"
    :public-folder "webapp"
    :html-routes html-routes)
  (ANY "/" [] (response/redirect "/hello"))
  (route/not-found "Cascade Demo: No such resource"))

(def app
  (handler/site master-routes))
