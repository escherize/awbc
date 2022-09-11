(ns awbc.view.events
  (:require
    [awbc.rules :as rules]
    [re-frame.core :as rf]
    [awbc.movement :as movement]
    [clojure.data :as data]
    [clojure.pprint :as pprint]
    [lambdaisland.deep-diff2 :as ddiff]))


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

(defn can-move-distance? [board {:keys [unit] :as _cell} hp]
  (let [{:keys [move-type move]} unit
        mapped-costs (mapv #(movement/->cost
                              move-type
                              (:terrain (get (:tiles board) %)))
                           (drop 1 hp))
        move-cost (reduce + mapped-costs)]
    (<= move-cost move)))

;; todo fix me
(rf/reg-event-db
  ::add-path
  (fn [db [_ [x y]]]
    (let [game (:game db)
          moving-from-coord (:moving-from-coord db)
          hp (get-in db [:game :hovered-path])
          from-coord (or (first hp) [x y])
          from-unit (get-in db [:game :tiles from-coord])
          travelable-tiles (set (keys (:movement-coords db)))]
      (if (contains? travelable-tiles [x y])
        (cond
          (contains? (set (drop-last hp)) [x y])
          (do
            (js/console.log "step backwards")
            (update-in db [:game :hovered-path]
                       (fn [path]
                         (vec (conj
                                (vec (take-while #(not= % [x y]) path)) [x y])))))

          (not (movement/continuous-path? hp))
          (do (js/console.log "non continuous path")
            (let [new-movement-coords (movement/movement-coords game
                                                                (or moving-from-coord
                                                                    from-coord))]
              (assoc-in db [:game :hovered-path]
                        (:path (get new-movement-coords [x y])))))

          (can-move-distance? game from-unit (conj hp [x y]))
          (do (js/console.log "can move dist")
            (->
                db
                (update-in [:game :hovered-path]
                           #(-> % ((fnil conj []) [x y]) distinct vec))
                (assoc :moving-from-coord from-coord)))

          :else
          (do
            (js/console.log "else")
            (let [new-movement-coords (movement/movement-coords game
                                                                (or moving-from-coord
                                                                    from-coord))]
              (assoc-in db [:game :hovered-path]
                        (:path (get new-movement-coords [x y]))))))

        ;; moved outside of travel tiles
        db))))

(rf/reg-event-db
 ::set-game-mode
 (fn [db [_ mode mode-info]]
   (cond-> db
     true
     (assoc-in [:game :mode] mode)

     true
     (assoc-in [:game :mode-info] mode-info)

     (= mode :unselected)
     (assoc :hovered-coord []))))

(rf/reg-event-db
 ::set-movement-coords
 (fn [{:keys [game] :as db} [_ [x y]]]
   (assoc db :movement-coords (movement/movement-coords game [x y]))))

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
       (assoc-in [:game :mode] :unselected)
       (assoc-in [:game :hovered-path] []))))

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
