(ns awbc.view.events
  (:require
    [awbc.rules :as rules]
    [re-frame.core :as rf]
    [awbc.movement :as movement]
    [clojure.data :as data]
    [clojure.pprint :as pprint]
    [lambdaisland.deep-diff2 :as ddiff]))

(def dbg
  "
  Output includes:
  1. the event vector
  2. a `clojure.data/diff` of db, before vs after, which shows
     the changes caused by the event handler.  You will absolutely have
     to understand https://clojuredocs.org/clojure.data/diff to
     understand the output.

  You'd typically include this interceptor after (to the right of) any
  path interceptor.

  Warning:  calling clojure.data/diff on large, complex data structures
  can be slow. So, you won't want this interceptor present in production
  code. So condition it out like this :

    (re-frame.core/reg-event-db
       :evt-id
       [(when ^boolean goog.DEBUG re-frame.core/debug)]  ;; <-- conditional
       (fn [db v]
         ...))

  To make this code fragment work, you'll also have to set goog.DEBUG to
  false in your production builds - look in `project.clj` of /examples/todomvc.
  "
  (rf/->interceptor
    :id     :debug
    :before (fn debug-before
              [context]
              (js/console.log "Handling re-frame event:" (pr-str (rf/get-coeffect context :event)))
              context)
    :after  (fn debug-after
              [context]
              (let [event   (rf/get-coeffect context :event)
                    orig-db (rf/get-coeffect context :db)
                    new-db  (rf/get-effect   context :db ::not-found)]
                (if (= new-db ::not-found)
                  (js/console.log "No :db changes caused by:" (pr-str event))
                  (let [[only-before only-after] (data/diff orig-db new-db)
                        db-changed?    (or (some? only-before) (some? only-after))]
                    (if db-changed?
                      (do (js/console.group "db clojure.data/diff for:" (pr-str event))
                          (js/console.log (pr-str (movement/diffmap only-before only-after)))
                          (js/console.groupEnd))
                      (js/console.log "No :db changes caused by:" (pr-str event)))))
                context))))


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
  [dbg]
  (fn [db [_ [x y]]]
    (let [game (:game db)
          hp (get-in db [:game :hovered-path])
          from-unit (get-in db [:game :tiles (or (first hp) [x y])])
          movement-coords (:movement-coords db)  ;; TODO #1 use movement-coords
          travelable-tiles (set (keys movement-coords))]
      (if (contains? travelable-tiles [x y])
        (if (can-move-distance? game from-unit (conj hp [x y]))
          (update-in db [:game :hovered-path]
                     #(-> % ((fnil conj []) [x y])
                          distinct
                          vec))
          (assoc db [:game :hovered-path]
                 (get [x y] movement-coords)))

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
