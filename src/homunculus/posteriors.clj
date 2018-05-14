(ns homunculus.posteriors
  (:require [kixi.stats.distribution :as d]))

(defprotocol Posterior
  (update-posterior [this data]
    "Returns the new posterior conditioned on the data."))


(defrecord BetaConjugate [beta-dist]

  d/ISampleable
  (sample-1 [this rng] (d/sample-1 beta-dist rng))
  (sample-n [this n rng] (d/sample-n beta-dist n rng))

  Posterior
  (update-posterior [this data]
    (let [alpha (.-alpha beta-dist)
          beta (.-beta beta-dist)]
      (->BetaConjugate (case data
                         0 (d/beta {:alpha alpha :beta (inc beta)})
                         1 (d/beta {:alpha (inc alpha) :beta beta}))))))


(defn beta
  ([] (beta {}))
  ([params]
   (->BetaConjugate (d/beta params))))
