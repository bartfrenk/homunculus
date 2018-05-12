(ns homunculus.core
  (:gen-class))

(defprotocol BanditAgent
  (update-belief [this act reward] [this ctx act reward] )
  (select-action [this] [this ctx]))

(defprotocol SequentialEstimator
  (put-observation [this obs])
  (get-estimate [this]))
