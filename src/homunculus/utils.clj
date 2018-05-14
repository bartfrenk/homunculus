(ns homunculus.utils
  (:require [kixi.stats.distribution :as d]))


(defn choice
  "Returns a categorical distribution on `options` in which every option is
  equally likely."
  [options]
  (let [n (count options)]
    (d/categorical options (repeat n (/ n)))))


(defn select-max-keys
  "Returns the set of keys that map to the maximal value."
  [[act score] & more]
  (loop [max score
         acc [act]
         remain more]
    (if (seq remain)
      (let [[[next-act next-score] & next-remain] remain]
        (cond
          (< max next-score)
          (recur next-score #{next-act} next-remain)

          (= max next-score)
          (recur max (conj acc next-act) next-remain)

          :else (recur max acc next-remain)))

      acc)))
