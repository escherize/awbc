(ns awbc.movement
  (:require
   [malli.core :as m]
   [awbc.shortest-path :as shortest-path]
   [lambdaisland.deep-diff2 :as ddiff]
   [clojure.walk :as walk])
  #?(:clj
     (:import
      [lambdaisland.deep_diff2.diff_impl Deletion Mismatch Insertion])))

(defn a-diff? [x]
  (#{"lambdaisland.deep-diff2.diff-impl/Deletion"
     "lambdaisland.deep-diff2.diff-impl/Mismatch"
     "lambdaisland.deep-diff2.diff-impl/Insertion"} (pr-str (type x))))

(defn ->paths
  ([x] (->paths x [] false))
  ([x p value]
   (into []
         (cond
           (a-diff? x) (if value {(conj p x) value} {p x})
           (map? x) (sort-by (comp pr-str first)
                             (concat (mapcat #(->paths (val %) (conj p (key %)) false) x)
                                     (mapcat #(->paths (key %) p (val %)) x)))
           :else nil))))

(defn build-diffmap [path->mismatch]
  (reduce #(assoc-in %1 (first %2) (second %2)) {} path->mismatch))

(defn diffmap [a b]
  (build-diffmap (->paths (ddiff/diff a b))))

(def coord
  [:tuple {:title "coord"} :int :int])

(def _ nil)

(def costs
  [[:road         [0 1 1 1 1 1 1 1 1 _ _]]
   [:bridge       [0 1 1 1 1 1 1 1 1 _ _]]
   [:plain        [1 1 1 2 1 1 _ _ _ _ _]]
   [:mtn          [4 2 1 _ _ _ _ _ 1 _ _]]
   [:shoal        [0 1 1 1 1 1 _ 1 _ _ _]]
   [:water        [0 _ _ _ _ _ _ _ 1 1 1]]
   [:reef         [1 _ _ _ _ _ _ _ 1 2 2]]
   [:dock         [3 1 1 1 1 1 1 1 _ _ _]]
   [:hq           [4 1 1 1 1 1 _ _ _ _ _]]
   [:city         [3 1 1 1 1 1 _ _ _ _ _]]
   [:factory      [3 1 1 1 1 1 _ _ _ _ _]]
   [:airport      [3 1 1 1 1 1 _ _ _ _ _]]
   [:river        [0 2 1 _ _ _ _ _ 1 _ _]]
   [:sky          [0 _ _ _ _ _ _ _ 1 _ _]]
   [:forest       [2 1 1 3 2 1 _ _ _ _ _]]
   [:missile-silo [3 1 1 1 1 1 _ _ _ _ _]]])

(def move-type #{:infantry :mech :tires :tread :tire-a :tire-b :tank :air :ships :trans})

(def cost-table
  "Shaped like {:road {:infantry 1 ...}}"
  (let [->row (fn [[terrain [d i m t tr ta tb tank air ship trans]]]
                {terrain (zipmap
                          [:stars  :infantry :mech :tires :tread :tire-a :tire-b :tank :air :ships :trans]
                          [ d       i         m     t      tr     ta      tb      tank  air  ship   trans])})]
    (apply merge (mapv ->row costs))))

(defn ->cost [move-type terrain]
  (get-in cost-table [terrain move-type]))

;; TODO: fix me
(defn neighbors [[x y]]
  ;; n s e w
  #{[x (dec y)]
    [(inc x) y]
    [x (inc y)]
    [(dec x) y]})

(defn can-move-to?
  "If a unit can move to a certain tile, returns remaining-movement, an int or returns false"
  [{:keys [move-type] :as unit}
   remaining-move
   {:keys [terrain] :as to-tile}]
  (let [cost (->cost move-type terrain)]
    (cond
      ;; TODO fix for transports
      (:unit to-tile)
      false

      (nil? cost)
      false

      :else
      (let [remaining (- remaining-move cost)]
        (if (or (zero? remaining) (pos? remaining))
          remaining
          false)))))

(comment

  (can-move-to?
   {:indirect? false,
    :move-type :mech,
    :move 3,
    :can-load-units #{},
    :type :infantry,
    :waited? false,
    :team :red,
    :hp 10,
    :base-vision 2}
   2
   {:terrain :mtn})

  )

(defn in-bounds? [max-x max-y [x y]]
  (and (<= 0 x max-x) (<= 0 y max-y)))

(defn max-coords [tiles]
  (let [xs (map first (keys tiles))
        ys (map second (keys tiles))]
    [(apply max xs) (apply max ys)]))

(defn ->graph [from tiles]
  (let [move-type (get-in tiles [from :unit :move-type])
        [max-x max-y] (max-coords tiles)]
    (into {}
          (for [coord (keys tiles)
                :let [neighbors (filter #(in-bounds? max-x max-y %) (neighbors coord))]]
            [coord (zipmap neighbors
                           (map (fn [n-coord]
                                  (let [to-terrain (get-in tiles [n-coord :terrain])
                                        move-cost (->cost move-type to-terrain)]
                                    move-cost))
                                neighbors))]))))


(defn shortest-path
  ([from-coord tiles]
   (let [raw-results (->> tiles
                          (->graph from-coord)
                          (shortest-path/dijkstra* from-coord))]
     (into {}
           (for [[from [cost path]]
                 (assoc raw-results from-coord [0 [from-coord]])]
             [from {:cost cost :path (vec path)}]))))
  ([from-coord to-coord tiles]
   (let [graph (->graph from-coord tiles)
         raw-results (shortest-path/dijkstra* from-coord to-coord graph)]
     (when raw-results
       {:cost (first raw-results)
        :path (vec (second raw-results))}))))

(defn movement-coords
  "Calculates all walkable paths from mover-coord.
   Returns a map of coord -> {:cost cost :path path} like:
  {[1 0] {:cost 2, :path [[0 0] [1 0]]}}"
  [{:keys [tiles] :as game} mover-coord]
  (let [tile (get tiles mover-coord)]
    (if-let [unit (:unit tile)]
      (let [coord->cost+path (shortest-path mover-coord tiles)]
        (into {}
              (filter
               (fn [[coord {:keys [cost _path]}]]
                 (let [dist-ok? (<= cost (-> unit :move))
                       unit-at-to (get-in tiles [coord :unit :team])]
                   (cond
                     (= coord mover-coord)
                     dist-ok?

                     (= nil unit-at-to)
                     dist-ok?

                     (= (:team unit) unit-at-to)
                     dist-ok?

                     ;; cant move through other team
                     (not= (:team unit) unit-at-to)
                     false)))
               coord->cost+path)))
      (do
        #?(:cljs (js/console.log (pr-str {:mover-coord mover-coord
                                          :coord-tile (find tiles mover-coord)})))
        (throw (ex-info (str "There is no unit at mover-coord: " (pr-str mover-coord))
                        {:mover-coord mover-coord
                         :coord-tile (find tiles mover-coord)}))))))

(defn continuous-path? [path]
  (let [steps (mapv (fn [[[x1 y1] [x2 y2]]] [(- x1 x2) (- y1 y2)]) (partition 2 1 path))]
    (every? #{[-1 0] [1 0] [0 1] [0 -1]} steps)))

(comment

  (def tiles-with-red-inf-center
    {[0 0]
     {:terrain :plain, :x 0, :y 0,
      :unit {:indirect? false, :move-type :infantry, :move 3, :can-load-units #{},
             :type :infantry, :waited? false, :team :red, :hp 10, :base-vision 2}},
     [0 1] {:terrain :plain, :x 0, :y 1
            :unit {:indirect? false, :move-type :infantry, :move 3, :can-load-units #{},
                   :type :infantry, :waited? false, :team :red, :hp 10, :base-vision 2}},
     [0 2] {:terrain :plain, :x 0, :y 2},
     [0 3] {:terrain :plain, :x 0, :y 3},
     [0 4] {:terrain :plain, :x 0, :y 4},
     [0 5] {:terrain :plain, :x 0, :y 5},
     [1 0] {:terrain :mtn, :x 1, :y 0},
     [1 1] {:terrain :mtn, :x 1, :y 1},
     [1 2] {:terrain :plain, :x 1, :y 2},
     [1 3] {:terrain :plain, :x 1, :y 3},
     [1 4] {:terrain :plain, :x 1, :y 4},
     [1 5] {:terrain :plain, :x 1, :y 5},
     [2 0] {:terrain :mtn, :x 2, :y 0},
     [2 1] {:terrain :plain, :x 2, :y 1},
     [2 2] {:terrain :plain, :x 2, :y 2},
     [2 3] {:terrain :plain, :x 2, :y 3},
     [2 4] {:terrain :plain, :x 2, :y 4},
     [2 5] {:terrain :plain, :x 2, :y 5},
     [3 0] {:terrain :plain, :x 3, :y 0},
     [3 1] {:terrain :plain, :x 3, :y 1},
     [3 2] {:terrain :plain, :x 3, :y 2},
     [3 3] {:terrain :plain, :x 0, :y 0},
     [3 4] {:terrain :plain, :x 3, :y 4},
     [3 5] {:terrain :plain, :x 3, :y 5},
     [4 0] {:terrain :plain, :x 4, :y 0},
     [4 1] {:terrain :plain, :x 4, :y 1},
     [4 2] {:terrain :plain, :x 4, :y 2},
     [4 3] {:terrain :plain, :x 4, :y 3},
     [4 4] {:terrain :plain, :x 4, :y 4},
     [4 5] {:terrain :plain, :x 4, :y 5},
     [5 0] {:terrain :plain, :x 5, :y 0},
     [5 1] {:terrain :plain, :x 5, :y 1},
     [5 2] {:terrain :plain, :x 5, :y 2},
     [5 3] {:terrain :plain, :x 5, :y 3},
     [5 4] {:terrain :plain, :x 5, :y 4},
     [5 5] {:terrain :plain, :x 5, :y 5}})

  (movement-coords
    {:tiles tiles-with-red-inf-center}
   [0 0])
;; => {[0 0] {:cost 0, :path [[0 0]]}, [1 0] {:cost 2, :path [[0 0] [1 0]]}, [1 1] {:cost 3, :path [[0 0] [0 1] [1 1]]}, [0 3] {:cost 3, :path [[0 0] [0 1] [0 2] [0 3]]}, [0 2] {:cost 2, :path [[0 0] [0 1] [0 2]]}, [1 2] {:cost 3, :path [[0 0] [0 1] [0 2] [1 2]]}, [0 1] {:cost 1, :path [[0 0] [0 1]]}}

  )
