(ns com.howardlewisship.cascade.internal.utils
  (:use
   clojure.contrib.str-utils))

(declare find-namespace-resource)

(defn find-classpath-resource
  "Finds a resource on the classpath (as a URL) or returns nil if not found. Optionally takes
  a symbol and evaluates the path relative to the symbol's namespace."
  ([path]
   (.. (Thread/currentThread) getContextClassLoader (getResource path)))
  ([symbol path]
   (let [ns (:ns (meta (resolve symbol)))]
     (find-namespace-resource ns path))))

(defn find-namespace-resource
  "Given a namespace (or a symbol identifying a namespace),
  locates a resource relative to the namespace, or nil if not found."
  [namespace path]
  (let [ns-str (name (ns-name namespace))
        ns-path (.. ns-str (replace \. \/) (replace \- \_))]
    (find-classpath-resource (str ns-path "/" path))))


(defn to-str-list
  [coll]
  "Creates a comma-seperated list from the collection, or returns \"(none\")
  if the collection is null or empty."
  (if (empty? coll)
    "(none)"
    (str-join ", " coll)))

(defn first-non-nil
  "Returns the first non-nil value from the collection."
  [coll]
  (first (remove nil? coll)))