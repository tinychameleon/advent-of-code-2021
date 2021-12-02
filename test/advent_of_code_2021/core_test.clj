(ns advent-of-code-2021.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2021.core :refer :all]))

(deftest imports-work-fine
  (testing "importing the core module"
    (is (= 1 1))))
