(ns com.owoga.frp.infrastructure-test
  (:require [com.owoga.frp.infrastructure :as frp]
            [clojure.test :refer [deftest is testing]]))

(deftest test-insert!
  (testing "insert!"
    (let [Offer (frp/->BaseRelVar 'Offer nil (atom #{}))]
      (frp/insert! Offer {:address "123 Fake St." :price 1.5e5})
      (is (= @Offer #{{:address "123 Fake St." :price 1.5e5}})))))

(deftest test-project
  (testing "projection"
    (let [Offer (frp/->BaseRelVar 'Offer nil (atom #{}))
          OfferPrices (frp/project Offer [:price])]
      (frp/load! Offer #{{:address "123 Fake St." :price 2e5}})
      (is (= @OfferPrices #{{:price 2e5}})))))

