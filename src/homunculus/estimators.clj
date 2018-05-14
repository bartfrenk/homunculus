(ns homunculus.estimators)

(defprotocol SequentialEstimator
  (update-estimate [this x] "Updates the estimator with a single observation")
  (get-estimate [this] "Return the current estimate"))


(defrecord ^:no-doc SampleMean [mean count]

  SequentialEstimator
  (update-estimate [this x]
    (let [next-count (inc count)
          next-mean (+ mean (/ (- x mean) next-count))]
      (->SampleMean next-mean next-count)))

  (get-estimate [this] mean))


(defn sample-mean
  "Create a fresh sequential estimator for the sample mean."
  []
  (->SampleMean 0 0))


(defrecord ^:no-doc SampleVariance [count mean ss]

  SequentialEstimator

  (update-estimate [this x]
    (let [diff (- x mean)
          next-count (inc count)
          next-mean (+ mean (/ (- x mean) next-count))
          next-ss (+ ss (* diff (- x next-mean)))]
      (->SampleVariance next-count next-mean next-ss)))

  (get-estimate [this]
    (if (> count 1) (/ ss (dec count)) 0.0)))


(defn new-sample-variance
  "Creates a fresh sequential estimator for the sample variance. Uses Wellford's
  method."
  []
  (->SampleVariance 0 0 0))


(defrecord ^:no-doc CombinedEstimator [components]

  SequentialEstimator
  (update-estimate [this x]
    (->CombinedEstimator (map #(update-estimate % x) components)))

  (get-estimate [this]
    (map get-estimate components)))

(defn combine
  "Combine multiple sequential estimators into a single one."
  [& estimators]
  (->CombinedEstimator estimators))


(defn ->transducer
  "Create a transducer from a sequential estimator. The value produced is the
  estimate of the estimator updated with the input."
  [estimator]
  (fn [xf]
    (let [state (volatile! estimator)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [next-estimator (update-estimate @state input)]
           (vreset! state next-estimator)
           (xf result (get-estimate next-estimator))))))))
