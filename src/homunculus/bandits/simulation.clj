(ns homunculus.bandits.simulation
  (:require [incanter.charts :as c]
            [homunculus.bandits.core :refer [select-action update-belief*]]))

(defprotocol ^:no-doc Environment
  (generate-context [this])
  (generate-reward* [this ctx act])
  (expected-reward* [this ctx act])
  (max-expected-reward* [this] [this ctx]))

(defn generate-reward
  ([this act] (generate-reward* this nil act))
  ([this ctx act] (generate-reward* this ctx act)))

(defn expected-reward
  ([this act] (expected-reward* this nil act))
  ([this ctx act] (expected-reward* this ctx act)))

(defn max-expected-reward
  ([this] (max-expected-reward* this nil))
  ([this ctx] (max-expected-reward* this ctx)))

(defn simulate
  "Runs a single simulation of the agent `agent` in the environment
  `environment`. The return value of this function the trace of the simulation
  as a lazy list. Each item in the trace is a map which contains information on
  a single step of the simulation. By default, the keys in the map
  are `:action`, `:reward` and `:context`."
  [agent env & flags]
  (let [flag-set (set flags)
        ctx (generate-context env)
        act (select-action agent ctx)
        rew (generate-reward env ctx act)]
    (lazy-seq
      (cons (cond-> {:action act :reward rew}
              (not (flag-set :no-context)) (assoc :context ctx)
              (flag-set :agent) (assoc :agent agent))
            (apply simulate (update-belief* agent ctx act rew) env flags)))))


(defn regret
  "The regret computed on a single trace item in the simulation. It is defined as
  the difference between the maximal reward and the expected reward for the action
  in the trace item."
  [env trace-item]
  (let [ctx (:context trace-item)
        max (max-expected-reward env ctx)
        p (expected-reward env ctx (:action trace-item))]
    (- max p)))


(defn prepeatedly
  "Evaluates the nullary function `f` `n` times, in parallel."
  [n f]
  (apply pcalls (repeat n f)))

(defn average-per-period-regret
  "Returns a lazy list of the average per-period regret of a `samples` number of
  simulations. The horizon over which a single simulation runs is specified by
  the `horizon` parameter. Both parameters are supplied in an optional options
  map.
  "
  ([agent env] (average-per-period-regret agent env {}))
  ([agent env {:keys [horizon samples] :or {horizon 10 samples 10}}]
   (apply map (comp #(/ % samples) +)
          (prepeatedly samples #(->> (simulate agent env)
                                     (map (partial regret env))
                                     (take horizon)
                                     (doall))))))


(defn regret-plot
  [ys & args]
  (let [xs (range (count ys))]
    (c/xy-plot xs ys :x-label "Iterations" :y-label "Per-period regret")))
