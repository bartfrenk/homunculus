(ns homunculus.estimators-test
  (:require [homunculus.estimators :refer [->transducer] :as sut]
            [clojure.test :refer [deftest is testing]]))


(deftest sample-mean-test
  (let [estimator (sut/sample-mean)]
    (testing "correct result with a short list of observations"
      (is (= [1.0 1.5 2.0] (into [] (->transducer estimator) [1.0 2.0 3.0]))))))


(deftest sample-variance-test
  (let [estimator (sut/new-sample-variance)]
    (testing "correct result with a short list of observations"
      (is (= (into [] (->transducer estimator) [1.0 2.0 3.0]) [0.0 0.5 1.0])))))
