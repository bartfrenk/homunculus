(ns homunculus.bandits.scratch
  (:require [incanter.core :as i]
            [homunculus.bandits.bernoulli :as b]
            [homunculus.bandits.simulation :as s]))



(def ba (b/bernoulli-agent [:a :b :c]))

(def be (b/bernoulli-env {:a 0.02 :b 0.03 :c 0.05}))

(s/generate-reward be :a)

(time (-> (s/average-per-period-regret ba be {:horizon 100 :samples 500})
          (s/regret-plot)
          (i/view)))

(time  (->> (s/simulate ba be :no-context)
            (s/per-period-regret be)
            (take 1000)
            (doall)))

