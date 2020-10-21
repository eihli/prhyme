(ns com.owoga.frp.infrastructure-test
  (:require [com.owoga.frp.infrastructure :as frp]
            [clojure.test :refer [deftest is testing]]))

(deftest test-project
  (testing "projection"
    (let [Offer (frp/->BaseRelVar 'Offer (atom #{}) '())
          OfferPrices (frp/project Offer [:price])]
      (frp/load! Offer #{{:address "123 Fake St." :price 2e5}})
      (is (= @OfferPrices #{{:price 2e5}})))))

(deftest test-insert!
  (testing "insert!"
    (let [Offer (frp/->BaseRelVar 'Offer (atom #{}) '())]
      (frp/insert! Offer {:address "123 Fake St." :price 1.5e5})
      (is (= @Offer #{{:address "123 Fake St." :price 1.5e5}})))))

(deftest test-defrelvar
  (testing "failed constraint raises"
    (let [Offer (frp/defrelvar Offer (fn [offers] (map #(> (:price %) 0) offers)))]
      (is (thrown-with-msg?
           Exception
           #"Constraint Exception"
           (frp/insert! Offer {:price -1})))))
  (testing "passed constraint doesn't raise"
    (let [Offer (frp/defrelvar Offer (fn [offers] (map #(> (:price %) 0) offers)))]
      (frp/insert! Offer {:price 20})
      (is (= @Offer #{{:price 20}})))))

(deftest test-extend
  (testing "extend-"
    (let [Offer (frp/->BaseRelVar 'Offer (atom #{}) '())]
      (frp/load! Offer #{{:price 1e6}})
      (frp/extend- Offer [:price-band (fn [e] (if (> (:price e) 1e6) :high :low))])
      (is (= :low (-> @Offer first :price-band))))))
