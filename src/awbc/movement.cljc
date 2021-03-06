(ns awbc.movement
  (:require
    [malli.core :as m]))


;; (def Coord (m/tuple int? int?))
;; (m/=>movment-squares [:cat [:game :coord] :=> [:cat :coord]])

(def cost-table
  (let [->row (fn [terrain d i m t tr ta tb tank air ship trans]
                {terrain (zipmap
                          [:stars  :infantry :mech :tires :tread :tire-a :tire-b :tank :air :ships :trans]
                          [d       i         m t tr ta tb tank air ship trans])})
        _ nil]
    (merge
     (->row :road    0 1 1 1 1 1 1 1 1 _ _)
     (->row :bridge  0 1 1 1 1 1 1 1 1 _ _)
     (->row :plain   1 1 1 2 1 1 _ _ _ _ _)
     (->row :mtn     4 2 1 _ _ _ _ _ 1 _ _)
     (->row :shoal   0 1 1 1 1 1 _ 1 _ _ _)
     (->row :water   0 _ _ _ _ _ _ _ 1 1 1)
     (->row :reef    1 _ _ _ _ _ _ _ 1 2 2)
     (->row :dock    3 1 1 1 1 1 1 1 _ _ _)
     (->row :hq      4 1 1 1 1 1 _ _ _ _ _)
     (->row :city    3 1 1 1 1 1 _ _ _ _ _)
     (->row :factory 3 1 1 1 1 1 _ _ _ _ _)
     (->row :airport 3 1 1 1 1 1 _ _ _ _ _)
     ;; todo?
     (->row :river   0 2 1 _ _ _ _ _ 1 _ _)
     (->row :sky     0 _ _ _ _ _ _ _ 1 _ _)
     (->row :wood    2 1 1 3 2 1 _ _ _ _ _)
     (->row :missile-silo
            3 1 1 1 1 1 _ _ _ _ _))))

(defn ->cost [move-type terrain]
  (get-in cost-table [terrain move-type]))

2 1 2
1 0 1
2 1 2

;; TODO: fix me
(defn neighbors [[x y]]
  ;; n s e w
  #{[x (dec y)]
    [(inc x) y]
    [x (inc y)]
    [(dec x) y]})

(defn can-move-to?
  "If a unit can move to a certain tile, returns remaining-movement, an int or returns false"
  [{:keys [move-type] :as unit} remaining-move {:keys [terrain] :as to-tile}]
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

(defn movement-coords
  [{:keys [tiles] :as game} mover-coord]
  (if-let [unit (get tiles mover-coord)]
    ;; TODO fix this
    (let [coord->cost {mover-coord 0}]

      )
    (throw (ex-info (str "There is no mover at mover-coord: " mover-coord)
                    {:mover-coord mover-coord
                     :coord-tile (find tiles mover-coord)}))))


(comment

  (def board-with-red-inf-center
    {[4 3] {:terrain :plain, :x 4, :y 3}
     [2 2] {:terrain :plain, :x 2, :y 2}
     [0 0] {:terrain :plain, :x 0, :y 0}
     [1 0] {:terrain :plain, :x 1, :y 0}
     [2 3] {:terrain :plain, :x 2, :y 3}
     [2 5] {:terrain :plain, :x 2, :y 5}
     [3 3]
     {:terrain :plain,
      :x 3,
      :y 3,
      :unit
      {:indirect? false,
       :move-type :infantry,
       :waited? false,
       :move 3,
       :can-load-units #{},
       :type :infantry,
       :team :red,
       :hp 10,
       :base-vision 2}},
     [5 4] {:terrain :plain, :x 5, :y 4},
     [1 1] {:terrain :plain, :x 1, :y 1},
     [0 5] {:terrain :plain, :x 0, :y 5},
     [3 4] {:terrain :plain, :x 3, :y 4},
     [4 2] {:terrain :plain, :x 4, :y 2},
     [3 0] {:terrain :plain, :x 3, :y 0},
     [5 3] {:terrain :plain, :x 5, :y 3},
     [4 1] {:terrain :plain, :x 4, :y 1},
     [5 2] {:terrain :plain, :x 5, :y 2},
     [1 4] {:terrain :plain, :x 1, :y 4},
     [1 3] {:terrain :plain, :x 1, :y 3},
     [1 5] {:terrain :plain, :x 1, :y 5},
     [0 3] {:terrain :plain, :x 0, :y 3},
     [5 1] {:terrain :plain, :x 5, :y 1},
     [5 5] {:terrain :plain, :x 5, :y 5},
     [2 4] {:terrain :plain, :x 2, :y 4},
     [4 5] {:terrain :plain, :x 4, :y 5},
     [0 2] {:terrain :plain, :x 0, :y 2},
     [2 0] {:terrain :plain, :x 2, :y 0},
     [0 4] {:terrain :plain, :x 0, :y 4},
     [3 1] {:terrain :plain, :x 3, :y 1},
     [2 1] {:terrain :plain, :x 2, :y 1},
     [4 4] {:terrain :plain, :x 4, :y 4},
     [5 0] {:terrain :plain, :x 5, :y 0},
     [1 2] {:terrain :plain, :x 1, :y 2},
     [3 5] {:terrain :plain, :x 3, :y 5},
     [3 2] {:terrain :plain, :x 3, :y 2},
     [0 1] {:terrain :plain, :x 0, :y 1},
     [4 0] {:terrain :plain, :x 4, :y 0}})

  (movement-squares
   {:tiles board-with-red-inf-center}
   [3 3])

  )
