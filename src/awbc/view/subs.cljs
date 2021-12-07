(ns awbc.view.subs
  (:require
    [re-frame.core :as rf]))


(rf/reg-sub ::width
            (fn [{:keys [game] :as db} _]
              (->> game :tiles keys (mapv first) (apply max))))


(rf/reg-sub ::height
            (fn [{:keys [game] :as db} _]
              (->> game :tiles keys (mapv second) (apply max))))

(rf/reg-sub ::tiles
            (fn [{:keys [game] :as db} _]
              (->> game :tiles)))

(rf/reg-sub ::y-sorted-tiles
            (fn [{:keys [game] :as db} _]
              (->> game
                   :tiles
                   (sort-by first)
                   (mapv second))))


(rf/reg-sub ::hovered-tile
            (fn [{:keys [game]}]
              (-> (:tiles game)
                  (get (:hovered-coord game)))))
