(ns example.real-estate
  (:require [com.owoga.frp.infrastructure :as frp]))

(frp/defrelvar Offer
  #(string? (:address %))
  #(number? (:offer-price %))
  #(inst? (:offer-date %))
  #(string? (:bidder-name %))
  #(string? (:bidder-address %)))

(frp/defrelvar Property
  #(string? (:address %))
  #(number? (:price %))
  #(string? (:photo %))
  #(string? (:agent-name %))
  #(inst? (:date-registered %)))
