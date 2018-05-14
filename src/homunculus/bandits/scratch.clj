(ns homunculus.bandits.scratch
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [homunculus.bandits.core :refer [mix select-action]]
            [homunculus.bandits.bernoulli :as b]
            [homunculus.bandits.simulation :as s]))


(defn compare-strategies-bernoulli
  [env {:keys [horizon samples] :as params}]
  (let [actions (-> env :probabilities keys)
        regret-greedy (-> (b/bernoulli-agent actions :greedy)
                          (s/average-per-period-regret env params))
        regret-rpm (-> (b/bernoulli-agent actions :rpm)
                       (s/average-per-period-regret env params))
        regret-random (-> (b/bernoulli-agent actions :random)
                          (s/average-per-period-regret env params))
        mixed (mix [(b/bernoulli-agent actions :greedy)
                    (b/bernoulli-agent actions :random)]
                   [0.9 0.1])

        regret-mixed (s/average-per-period-regret mixed env params)
        xs (range horizon)]
    (-> (c/xy-plot xs regret-greedy
                   :x-label "Step"
                   :y-label "Regret"
                   :title (format "Average per-period regret (%d samples)"
                                  samples)
                   :legend true
                   :series-label "Greedy")
        (c/add-lines xs regret-rpm :series-label "RPM")
        (c/add-lines xs regret-random :series-label "Random")
        (c/add-lines xs regret-mixed :series-label "Epsilon-Greedy")
        (i/view))))

#_(time (let [env (b/bernoulli-env {:a 0.2
                                    :b 0.15
                                    :c 0.1})]
          (compare-strategies-bernoulli env {:horizon 2000 :samples 1000})))
