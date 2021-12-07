(ns awbc.view.events
  (:require
    [awbc.rules :as rules]
    [re-frame.core :as rf]))


(rf/reg-event-db
  ::init!
  (fn [_ _]
    {:game {:tiles (rules/sample-tiles-two 10 10)}}))


(rf/reg-event-db
  ::set-hovered
  (fn [db [_ [x y]]]
    (assoc-in db [:game :hovered-coord] [x y])))
