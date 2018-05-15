(ns homunculus.distribution
  (:require [kixi.stats.distribution :as d]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :as m]
            [clojure.test.check.random :refer [split]]))


(m/set-current-implementation :vectorz)


(defn- sample-normal
  ([n rng] (sample-normal n rng []))
  ([n rng acc]
   (if (pos? n)
     (let [[r1 r2] (split rng)]
       (recur (dec n) r2 (conj acc (d/rand-normal r1))))
     acc)))


(defn- sample-multivariate-normal
  [mu sigma rng]
  (let [n (count mu)
        e (m/scale (m/identity-matrix n) 1e-8)
        L (:L (mp/cholesky (m/add (m/matrix sigma) e)
                           {:results [:L]}))
        u (m/matrix (sample-normal n rng))
        samples (m/add (m/matrix mu)
                       (m/mmul L u))]
    (m/to-nested-vectors samples)))


(deftype ^:nodoc MultivariateNormal [mu sigma]
  d/ISampleable
  (sample-1 [this rng]
    (sample-multivariate-normal mu sigma rng))
  (sample-n [this n rng]
    (d/default-sample-n this n rng))
  #?@(:clj (clojure.lang.ISeq
              (seq [this] (d/sampleable->seq this)))
      :cljs (ISeqable
              (-seq [this] (d/sampleable->seq this)))))


(defn multivariate-normal
  "Returns a multivariate normal distribution.
  Params: {vector of real numbers :mu, covariance matrix :sigma}."
  [{:keys [mu sigma]}]
  (->MultivariateNormal mu sigma))
