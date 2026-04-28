(ns erp12.cbgp-lite.genome-test
  (:require [clojure.test :refer [deftest is]]
            [erp12.cbgp-lite.genome :as g]
            [erp12.cbgp-lite.program.types :as t]))

(deftest plushy->push-test
  (is (= [(g/->Lit 0 t/INT)
          (assoc (g/->Let)
                 :push [(g/->Lit 1 t/INT)])
          (g/->Lit 2 t/INT)
          (assoc (g/->Abs [t/INT] t/INT)
                 :push [])]
         (g/plushy->push (list (g/->Lit 0 t/INT)
                               (g/->Let)
                               (g/->Lit 1 t/INT)
                               :close
                               (g/->Lit 2 t/INT)
                               (g/->Abs [t/INT] t/INT))))))
