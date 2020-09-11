(ns example.real-estate)

(defrelvar Offer
  :address string?
  :offer-price number?
  :offer-date inst?
  :bidder-name string?
  :bidder-address string?)

(defrelvar Property
  :address string?
  :price number?
  :photo string?
  :agent-name string?
  :date-registered inst?)
