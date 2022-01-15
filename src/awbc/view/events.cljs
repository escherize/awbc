(ns awbc.view.events
  (:require
    [awbc.rules :as rules]
    [re-frame.core :as rf]))

(defn p
  ([x] (js/console.log (pr-str x)) x)
  ([tag x] (js/console.log tag ": " (pr-str x)) x))

(rf/reg-event-db
  ::init!
  (fn [_ _]
    {:game {:tiles (rules/sample-tiles-two 10 10)
            :mode :unselected
            :players [{:team :red :funds 0}
                      {:team :blue :funds 0}]}}))

(rf/reg-event-db
  ::set-hovered
  (fn [db [_ [x y]]]
    (assoc-in db [:game :hovered-coord] [x y])))

(rf/reg-event-db
 ::set-game-mode
 (fn [db [_ mode mode-info]]
   (-> db
       (assoc-in [:game :mode] mode)
       (assoc-in [:game :mode-info] mode-info))))

(defn dissoc-in [m key-path]
  (update-in m (drop-last key-path) dissoc (last key-path)))

(rf/reg-event-db
 ::move-unit
 (fn [db [_ to-coord]]
   (let [game (:game db)
         tiles (:tiles game)
         from-coord (-> db :game :mode-info :from-coord)
         unit (get-in tiles [from-coord :unit])
         new-tiles (-> tiles
                       (dissoc-in [from-coord :unit])
                       (assoc-in [to-coord :unit] unit))]
     (-> db
         (assoc-in [:game :tiles] new-tiles)
         (assoc-in [:game :mode] :unit-moved)
         (assoc-in [:game :mode-info :to-coord] to-coord)))))

(rf/reg-event-db
 ::unmove-unit
 (fn [db [_ ]]
   (let [game (:game db)
         tiles (:tiles game)
         to-coord (-> db :game :mode-info :to-coord)
         from-coord (-> db :game :mode-info :from-coord)
         unit (get-in tiles [to-coord :unit])
         new-tiles (-> tiles
                       (dissoc-in [to-coord :unit])
                       (assoc-in [from-coord :unit] unit))]
     (-> db
         (assoc-in [:game :tiles] new-tiles)
         (assoc-in [:game :mode] :unselected)))))

(rf/reg-event-db
 ::wait-unit
 (fn [db [_ at-coord]]
   (p "at-coord in wait unit:" at-coord)
   (-> db
       (assoc-in [:game :tiles at-coord :unit :waited?] true)
       (assoc-in [:game :mode] :unselected))))

(rf/reg-event-db
 ::unselect-unit
 (fn [db _]
   (let [{:keys [to-coord from-coord]} (-> db :game :mode-info)
         tiles (-> db :game :tiles)
         unit (get-in tiles [to-coord :unit])
         new-tiles (-> tiles
                       (dissoc-in [to-coord :unit])
                       (assoc-in [from-coord :unit] unit))]
     (-> db
         (assoc-in [:game :tiles] new-tiles)
         (assoc-in [:game :mode] :unselected)))))
