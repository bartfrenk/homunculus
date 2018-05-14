(ns homunculus.bandits.core
  (:gen-class))

(defprotocol BanditAgent
  (update-belief* [this ctx act reward] )
  (select-action* [this ctx]))

(defn update-belief
  ([this act reward] (update-belief* this nil act reward))
  ([this ctx act reward] (update-belief* this ctx act reward)))

(defn select-action
  ([this] (select-action this))
  ([this ctx] (select-action this ctx)))
