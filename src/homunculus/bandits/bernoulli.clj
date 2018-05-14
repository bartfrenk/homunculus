(ns homunculus.bandits.bernoulli
  (:require [kixi.stats.distribution :as d]
            [homunculus.utils :refer [select-max-keys choice]]
            [homunculus.posteriors :as p]
            [homunculus.estimators :as e]
            [homunculus.bandits.simulation :refer [Environment] :as s]
            [homunculus.bandits.core :refer [BanditAgent]]))


(defrecord ^:nodoc BernoulliAgentRPM [posteriors]
  BanditAgent

  (select-action* [this _ctx]
    (->> posteriors
         (map (fn [[act beta]] [act (d/draw beta)]))
         (apply select-max-keys)
         (choice)
         (d/draw)))

  (update-belief* [this _ctx act reward]
    (->BernoulliAgentRPM (update posteriors act #(p/update-posterior % reward)))))


(defrecord ^:nodoc BernoulliAgentGreedy [estimates]
  BanditAgent

  (select-action* [this _ctx]
    (->> estimates
         (map (fn [[act seq-mean]] [act (e/get-estimate seq-mean)]))
         (apply select-max-keys)
         (choice)
         (d/draw)))

  (update-belief* [this _ctx act reward]
    (->BernoulliAgentGreedy (update estimates act #(e/update-estimate % reward)))))


(defrecord ^:nodoc BernoulliAgentRandom [actions]
  BanditAgent

  (select-action* [this ctx]
    (d/draw (choice actions)))

  (update-belief* [this _ctx _act _reward]
    this))

(defn bernoulli-agent
  "Creates a new agent for the multi-armed bandit problem with actions
  `actions`. The optional parameter `type` indicates the strategy of the
  agent. Possible strategies are:

  - :rpm (random probability matching, Thompson sampling)
  - :greedy
  - :random
  "
  ([actions] (bernoulli-agent actions :rpm))
  ([actions type]
   (case type
     :rpm (->BernoulliAgentRPM (zipmap actions (repeat (p/beta))))
     :greedy (->BernoulliAgentGreedy (zipmap actions (repeat (e/sample-mean))))
     :random (->BernoulliAgentRandom actions)
     (throw (ex-info (str "No such agent") {:type type})))))


(defrecord BernoulliEnv [probabilities max-probability]
  Environment
  (generate-context [this] nil)

  (generate-reward* [this _ctx act]
    (-> probabilities
        act
        (d/bernoulli)
        (d/draw)
        (#(if % 1 0))))

  (max-expected-reward* [this _ctx] max-probability)

  (expected-reward* [this ctx act] (probabilities act)))


(defn bernoulli-env
  "Returns a context-less static Bernoulli bandit environment, with reward
  probabilities distributed according to `probabilities` map. The keys of this
  map are the allowed actions, while the values are the success probabilities."
  [probabilities]
  (->BernoulliEnv probabilities (apply max (vals probabilities))))


