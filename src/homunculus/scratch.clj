(ns homunculus.scratch
  (:require [kixi.stats.distribution :as d]))

(defprotocol BanditAgent
  (update-agent [this ctx act reward])
  (select-action [this ctx]))

(defrecord BernoulliBanditTS [posteriors])

;; Add spec for actions
(defn new-bernoulli-bandit-ts [actions]
  (BernoulliBanditTS. (zipmap actions (repeat {:alpha 0 :beta 0}))))


(defprotocol Strategy
  (process-outcome [this ctx act outcome])
  (select-action [this ctx]))

(defn update-beta-posterior
  [reward prior]
  (if (= reward 1)
    (update prior :alpha inc)
    (update prior :beta inc)))

(defrecord ThompsonSamplingBernoulli [actions posteriors]
    Strategy
    (process-reward [this ctx act outcome]
      (swap! posteriors #(update % act (partial update-beta-posterior outcome))))
    (select-action [this ctx]
      (apply max-key (fn [act] (d/draw (d/beta (act @posteriors)))) actions)))

(defrecord RandomStrategy [actions]
  Strategy
  (process-reward [this ctx act outcome])
  (select-action [this ctx]
    (rand-nth actions)))

(defn new-thompson-sampling-bernoulli
  [actions]
  (map->ThompsonSamplingBernoulli
    {:posteriors (atom (zipmap actions (repeat {:alpha 1.0 :beta 1.0})))
     :actions actions}))

(defn new-random-strategy
  [actions]
  (map->RandomStrategy {:actions actions}))

(zipmap [1 2 3] (repeat 4))

(d/draw (d/beta {}))


(def x (new-bernoulli-bandit [:A :B]))

(def y (new-random-strategy [:A :B :C]))

(process-reward y {} :A 1)
(select-action y {})

(process-reward x {} :A 1)
(select-action x {})
