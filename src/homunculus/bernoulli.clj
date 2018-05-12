(ns homunculus.bernoulli
  (:require
   [kixi.stats.distribution :as d]
   [homunculus.core :refer [BanditAgent update-belief select-action]]
   [homunculus.simulation :refer [Environment
                                  generate-context
                                  generate-reward
                                  max-expected-reward
                                  expected-reward
                                  per-period-regret
                                  simulate] :as s]
   [incanter.charts :as c]
   [incanter.core :as i]))


(defn choice
  "Returns a categorical distribution on `options` in which every option is
  equally likely."
  [options]
  (let [n (count options)]
    (d/categorical options (repeat n (/ n)))))

(defn select-best-actions
  "Returns the set of actions with maximal score."
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

(defn update-beta
  [beta-params reward]
  (case reward
    0 (update beta-params :beta inc)
    1 (update beta-params :alpha inc)
    beta-params))

(defrecord BernoulliAgentRPM [posterior]
  BanditAgent

  (select-action [this] (select-action this nil))

  (select-action [this _]
    (->> posterior
         (map (fn [[act beta-params]] [act (d/draw (d/beta beta-params))]))
         (apply select-best-actions)
         (choice)
         (d/draw)))

  (update-belief [this act reward]
    (update-belief this act nil reward))

  (update-belief [this _ctx act reward]
    (->BernoulliAgentRPM (update posterior act #(update-beta % reward)))))

(defn new-bernoulli-agent-rpm
  [actions]
  (->BernoulliAgentRPM (zipmap actions (repeat {:alpha 1.0 :beta 1.0}))))

(def bandit-agent (new-bernoulli-agent-rpm [:a :b :c]))

(defrecord BernoulliEnv [expected-rewards max-exp-rew]
  Environment
  (generate-context [this])

  (generate-reward [this act] (generate-reward this nil act))
  (generate-reward [this _ctx act]
    (-> expected-rewards
        act
        (d/bernoulli)
        (d/draw)
        (#(if % 1 0))))

  (max-expected-reward [this] (max-expected-reward this nil))
  (max-expected-reward [this _ctx] max-exp-rew)

  (expected-reward [this act] (expected-rewards this nil act))
  (expected-reward [this ctx act] (expected-rewards act)))


(defn new-bernoulli-env
  [expected-rewards]
  (->BernoulliEnv expected-rewards
                  (apply max (vals expected-rewards))))

(select-action (update-belief bandit-agent :b 1))

(def ba (new-bernoulli-agent-rpm [:a :b :c]))

(def be (new-bernoulli-env {:a 0.02 :b 0.03 :c 0.05}))

(generate-reward be :a)




(defn regret-plot
  [ys & args]
  (let [xs (range (count ys))]
    (c/xy-plot xs ys
               :x-label "Iterations"
               :y-label "Per-period regret")))


(time (-> (s/average-per-period-regret ba be {:horizon 1000 :samples 100})
          (regret-plot)
          (i/view)))

(time  (->> (simulate ba be :no-context)
            (per-period-regret be)
            (take 1000)
            (doall)))

