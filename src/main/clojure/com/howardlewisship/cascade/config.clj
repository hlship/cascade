(ns com.howardlewisship.cascade.config)

(def configuration {
  :view-namespaces []
  :fragment-namespaces []
  :action-namespaces []
  })

(defn add-to-config
  "Adds a value to a configuration list stored in the configuration var."
  [key value]
  (alter-var-root (var configuration) (fn [current] (update-in current [key] conj value))))



