(use 'clojure.contrib.monads)

(defn combine-all [a b]
  (domonad sequence-m
           [left a
            right b]
           [left right]))

