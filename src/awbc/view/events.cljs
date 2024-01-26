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
  (fn [db [_ [to-x to-y]]]
    (let [game (:game db)
          moving-from-coord (:moving-from-coord db)
          hpath (get-in db [:game :hovered-path])
          starting-coord (or (first hpath) [to-x to-y])
          from-unit (get-in db [:game :tiles starting-coord])
          travelable-tiles (set (keys (:movement-coords db)))]
      (if (contains? travelable-tiles [to-x to-y])
        (cond
          (contains? (set (drop-last hpath)) [to-x to-y])
          (do
            (js/console.log "step back")
            (update-in db [:game :hovered-path]
                       (fn [path]
                         (vec (conj
                                (vec (take-while #(not= % [to-x to-y]) path)) [to-x to-y])))))

          (not (movement/continuous-path? hpath))
          (do (js/console.log "non-continuous path")
            (let [new-movement-coords (movement/movement-coords game
                                                                (or moving-from-coord
                                                                    starting-coord))]
              (assoc-in db [:game :hovered-path]
                        (:path (get new-movement-coords [to-x to-y])))))

          (can-move-distance? game from-unit (conj hpath [to-x to-y]))
          (do (js/console.log "adding tile to path")
            (->
                db
                (update-in [:game :hovered-path]
                           #(-> % ((fnil conj []) [to-x to-y]) distinct vec))
                (assoc :moving-from-coord starting-coord)))

          :else
          (do
            (js/console.log "else")
            (let [new-movement-coords (movement/movement-coords game
                                                                (or moving-from-coord
                                                                    starting-coord))]
              (assoc-in db [:game :hovered-path]
                        (:path (get new-movement-coords [to-x to-y]))))))

        ;; moved outside of travel tiles
        db))))

(rf/reg-event-db
  ::reset-path
  (fn [db [_ coord]]
    (assoc-in db [:game :hovered-path] [coord])))

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
         to-unit (get-in tiles [to-coord :unit])
         new-tiles (-> tiles
                       (dissoc-in [from-coord :unit])
                       (assoc-in [to-coord :unit] unit))]
     (if to-unit
       db
       (-> db
           (assoc-in [:game :tiles] new-tiles)
           (assoc-in [:game :mode] :unit-moved)
           (assoc-in [:game :mode-info :to-coord] to-coord))))))

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
  ::select-target
  (fn [db [_ at-coord]]
    (p "at-coord in select-target:" at-coord)
    (let [game (:game db)
          tiles (:tiles game)
          to-coord (-> db :game :mode-info :to-coord)
          unit (get-in tiles [to-coord :unit])]
      (if (:indirect unit)
        (do (p "TODO indirects not supported") db)
        (let [o (-> db
                    (assoc-in [:game :mode] :select-target)
                    (assoc-in [:game :mode-info] (rules/can-attack tiles at-coord)))]
          (prn o)
          o)))))

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
