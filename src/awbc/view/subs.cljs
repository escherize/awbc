(ns awbc.view.subs
  (:require
   [re-frame.core :as rf]
   [awbc.movement :as movement]))


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

(rf/reg-sub ::game-mode (fn [{:keys [game]}] (:mode game)))

(rf/reg-sub ::game (fn [{:keys [game]}] game))

(rf/reg-sub ::game-mode-info (fn [{:keys [game]}] (:mode-info game)))

(defn cycle-nth [n coll]
  (nth coll (mod n (count coll))))

(rf/reg-sub ::current-player-turn
            (fn [{:keys [game]}]
              (cycle-nth (:turn-number game) (:players game))))

(rf/reg-sub
 ::can-move-to?
 (fn [db [_ [x y]]]
   (contains?
    (movement/movement-coords (:game db) (-> db :game :mode-info :from-coord))
    [x y])))
