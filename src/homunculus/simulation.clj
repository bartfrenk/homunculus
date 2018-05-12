(ns homunculus.simulation
  (:require [homunculus.core :refer [select-action update-belief]]))

(defprotocol Environment
  (generate-context [this])
  (generate-reward [this act] [this ctx act])
  (expected-reward [this act] [this ctx act])
  (max-expected-reward [this] [this ctx]))



(defn simulate
  [agent env & flags]
  (let [flag-set (set flags)
        ctx (generate-context env)
        act (select-action agent ctx)
        rew (generate-reward env ctx act)]
    (lazy-seq
      (cons (cond-> {:action act :reward rew}
              (not (flag-set :no-context)) (assoc :context ctx)
              (flag-set :agent) (assoc :agent agent))
            (apply simulate (update-belief agent ctx act rew) env flags)))))


(defn regret
  [env trace-item]
  (let [ctx (:context trace-item)
        max (max-expected-reward env ctx)
        p (expected-reward env ctx (:action trace-item))]
    (- max p)))

(defn per-period-regret
  [env trace]
  (map (partial regret env) trace))

(defn prepeatedly
  [n f]
  (apply pcalls (repeat n f)))

(defn average-per-period-regret
  ([agent env] (average-per-period-regret agent env {}))
  ([agent env {:keys [horizon samples] :or {horizon 10 samples 10}}]
   (apply map (comp #(/ % samples) +)
          (prepeatedly samples #(->> (simulate agent env)
                                     (per-period-regret env)
                                     (take horizon)
                                     (doall))))))
