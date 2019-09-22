(ns gol.core-test
  (:require [clojure.test :refer :all]
            [gol.core :refer :all]))

(def test-board
  [[{:alive true} {:alive false} {:alive false}]
   [{:alive true} {:alive false} {:alive false}]
   [{:alive false} {:alive false} {:alive true}]])

(deftest transform-board-test
  (is (= (transform-board test-board)
         [[{:alive false} {:alive false} {:alive false}]
          [{:alive false} {:alive true} {:alive false}]
          [{:alive false} {:alive false} {:alive false}]])))
