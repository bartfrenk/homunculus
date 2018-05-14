(ns homunculus.bandits.core
  (:require [kixi.stats.distribution :as d]))

(defprotocol BanditAgent
  (update-belief* [this ctx act reward] )
  (select-action* [this ctx]))

(defn update-belief
  ([this act reward] (update-belief* this nil act reward))
  ([this ctx act reward] (update-belief* this ctx act reward)))

(defn select-action
  ([this] (select-action* this nil))
  ([this ctx] (select-action* this ctx)))


(defrecord MixedAgent [agents ps]
  BanditAgent

  (update-belief* [this ctx act reward]
    (->MixedAgent (map #(update-belief* % ctx act reward) agents) ps))

  (select-action* [this ctx]
    (let [selected (d/draw (d/categorical agents ps))]
      (select-action* selected ctx))))


(defn mix
  "Returns an agent whose strategy is a mixed strategy of the specified list of
  agents `agents`. The probability of selecting the i-th agent is given by the
  i-th element of the probability vector `ps`."
  [agents ps]
  (->MixedAgent agents ps))






