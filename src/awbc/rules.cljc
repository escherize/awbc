(ns awbc.rules
  (:require
   [awbc.damage :as damage]
   [awbc.movement :as movement]
   [malli.core :as m]
   #?(:clj [malli.dev :as md])
   [malli.generator :as mg]))

(defn ->enum
  [type]
  (into [:enum] type))

(def UnitType
  [:infantry :mech :recon :apc :artillery :tank :md-tank :rocket
   :neo-tank :missle :anti-air :b-copter :t-copter :bomber :fighter])

(def MoveType
  [:infantry :mech :wheels :treads :air :sea])

(def Team [:red :blue :green :yellow :black])

(def Unit
  [:map
   [:type (->enum UnitType)]
   [:team (->enum Team)]
   [:hp nat-int?]
   [:ammo {:optional true} nat-int?]
   [:base-vision nat-int?]
   [:move-type (->enum MoveType)]
   [:move nat-int?]
   [:indirect? boolean?]
   [:waited? boolean?]
   [:shot-distance {:optional true}
    [:map [:min int?] [:max int?]]]
   [:loadable-unit-types [:set (->enum UnitType)]]
   [:load-slots {:optional true} int?]
   [:loaded-units {:optional true} [:vector Unit]]])

(def Terrain
  [:road :bridge :plain :mtn :shoal :water :reef
   :dock :hq :city :factory :airport])

(def Tile
  [:map
   [:terrain (->enum Terrain)]
   [:team {:optional true} (->enum Team)]
   [:x nat-int?]
   [:y nat-int?]
   [:unit {:optional true} Unit]])

(def Tiles [:map-of [:tuple int? int?] Tile])

(def Player
  [:map
   [:funds nat-int?]
   [:power-meter nat-int?]
   [:max-power-meter nat-int?]
   [:team (->enum Team)]])

(def Game
  [:map
   [:players [:vector {:min 1} Player]]
   [:board Tiles]])

(defn create-unit
  [team unit-type]
  (let [unit (-> (case unit-type
                   :infantry {:type :infantry
                              :hp 10
                              ;; on mtn its higher
                              :base-vision 2
                              :move-type :infantry
                              :move 3
                              :indirect? false
                              :waited? false
                              :loadable-unit-types #{}})
                 (assoc :team team))]
    unit))

(defn create-tile
  ([terrain x y] (create-tile terrain x y {}))
  ([terrain x y {:keys [team] :as opts}]
   (cond-> {:terrain terrain :x x :y y}
     team (assoc :team team))))

(defn create-empty-tiles
  [w h & [type]]
  (->> (for [x (range w) y (range h)]
         [[x y] (create-tile (or type :plain) x y)])
       (into {})))

(defn sample-tiles
  []
  (-> (create-empty-tiles 3 3)
      (update [0 0] merge {:terrain :hq :team "red"})
      (update [2 2] merge {:terrain :hq :team "blue"})
      (assoc-in [[1 1] :terrain] :mtn)
      (assoc-in [[2 1] :unit] (create-unit :red :infantry))
      (assoc-in [[2 2] :unit] (create-unit :blue :infantry))))

(defn adjacent-cells [[from-x from-y]]
  (for [[dx dy] [[0 1] [1 0] [-1 0] [0 -1]]]
    [(+ dx from-x) (+ dy from-y)]))

(defn can-attack [tiles from-coord]
  (let [{:keys [indirect? team] :as unit} (get tiles from-coord)]
    (if indirect?
      (do (println "TODO indirects") [])
      (let [adj-units (keep #(get tiles %) (adjacent-cells from-coord))]
        (keep
          #(when-let [nme-team (:team (:unit %))]
             (and (contains? (set Team) nme-team)
                  (not= team nme-team))
             [(:x %) (:y %)])
          adj-units)))))

(comment
  (-> (create-empty-tiles 2 2)
      (update [0 0] merge {:terrain :hq :team :red})
      (update [1 1] merge {:terrain :hq :team :blue})
      (assoc-in [[0 1] :unit] (create-unit :red :infantry))
      (assoc-in [[1 1] :unit] (create-unit :blue :infantry))
      (can-attack [1 1]))
  )

(defn sample-tiles-two
  [w h]
  (-> (create-empty-tiles w h)

      (update [0 0] merge {:terrain :hq :team "red"})
      (update [1 1] merge {:terrain :factory :team "red"})

      (update [0 (dec h)] merge {:terrain :hq :team "yellow"})
      (update [1 (- h 2)] merge {:terrain :factory :team "yellow"})

      (update [(dec w) 0] merge {:terrain :hq :team "green"})
      (update [(- w 2) 1] merge {:terrain :factory :team "green"})

      (assoc-in [[3 3] :terrain] :forest)
      (assoc-in [[3 4] :terrain] :forest)
      (assoc-in [[4 4] :terrain] :forest)

      (update [(dec w) (dec h)] merge {:terrain :hq :team "blue"})
      (update [(- w 2) (- h 2)] merge {:terrain :factory :team "blue"})
      (assoc-in [[(- w 3) (- h 3)] :unit] (create-unit :blue :infantry))
      (assoc-in [[(- w 4) (- w 4)] :terrain] :mtn)

      (assoc-in [[2 2] :terrain] :mtn)
      (assoc-in [[2 3] :terrain] :mtn)
      (assoc-in [[3 2] :terrain] :mtn)
      (assoc-in [[5 4] :terrain] :mtn)
      (assoc-in [[5 5] :terrain] :mtn)
      (assoc-in [[6 4] :terrain] :mtn)
      (assoc-in [[6 3] :terrain] :mtn)
      (assoc-in [[7 3] :terrain] :mtn)
      (assoc-in [[2 1] :unit] (create-unit :red :infantry))
      (assoc-in [[3 2] :unit] (create-unit :red :infantry))
      (assoc-in [[3 3] :unit] (create-unit :red :infantry))
      (assoc-in [[5 3] :unit] (create-unit :red :infantry))
      (assoc-in [[6 6] :unit] (create-unit :red :infantry))
      ;; (assoc-in [[2 1] :team] :red)

      #_(assoc-in [[2 2] :team] :blue)))

(defn print-tiles
  [tiles]
  (vec (vals (sort-by first tiles))))

(defn new-game [?tiles]
  {:game {:turn-number 0
          :mode :unselected
          :mode-info {}
          :players []}
   :tiles (or ?tiles (create-empty-tiles 6 6))})

(defn current-player [game]
  )

#_(defn move-unit [{:keys [tiles] :as game} from-coord to-coord]
  (let [unit (get-in tiles [from-coord :unit])]
    (-> game
        (update :tiles dissoc-in [from-coord :unit])
        (assoc-in [:tiles to-coord :unit] unit)
        (assoc :mode :unit-moved))))

(comment

  (print-tiles (sample-tiles))

  (damage/compute
   {:terrain :hq, :unit {:type :tank, :hp 10 :ammo 1} :team :blue}
   {:terrain :plain, :unit {:type :anti-air, :hp 10} :team :red})

  (create-unit
    :red
    :infantry)

  )

(comment

  (def t

    {[0 0]
     {:terrain :hq,
      :x 0,
      :y 0,
      :team "red",
      :unit
      {:indirect? false,
       :move-type :infantry,
       :move 3,
       :type :infantry,
       :waited? false,
       :team :red,
       :hp 10,
       :loadable-unit-types #{},
       :base-vision 2}},
     [0 1] {:terrain :plain, :x 0, :y 1},
     [0 2] {:terrain :plain, :x 0, :y 2},
     [1 0] {:terrain :plain, :x 1, :y 0},
     [1 1] {:terrain :mtn, :x 1, :y 1},
     [1 2] {:terrain :plain, :x 1, :y 2},
     [2 0] {:terrain :plain, :x 2, :y 0},
     [2 1] {:terrain :plain, :x 2, :y 1},
     [2 2]
     {:terrain :hq,
      :x 2,
      :y 2,
      :team "blue",
      :unit
      {:indirect? false,
       :move-type :infantry,
       :move 3,
       :type :infantry,
       :waited? false,
       :team :blue,
       :hp 10,
       :loadable-unit-types #{},
       :base-vision 2}}})

  (movement/shortest-path [0 0] t)
  (movement/shortest-path [0 0] [2 1] t)

  )


;; ;;;; non-sense below

;; (defn uuid4 [] (java.util.UUID/randomUUID))

;; (defn uuid->byte-array [u]
;;   (let [^long lo (.getLeastSignificantBits u)  ; least significant bits
;;         ^long hi (.getMostSignificantBits u)]  ; most significant bits (left-most bits)
;;     (-> (java.nio.ByteBuffer/allocate 16)      ; http://docs.oracle.com/javase/6/docs/api/java/util/UUID.html
;;         (.putLong hi)
;;         (.putLong lo)
;;         (.array))))

;; (def alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

;; (defn ->base-58 [uuid]
;;   (loop [in (uuid->byte-array uuid)
;;          zero-counter 0
;;          encoding-flag 0
;;          b58-bytes 0
;;          b58-encoding []
;;          carry 0]


;;     ))

;; (type #uuid "341fefd6-6135-4170-8551-e11e670a5167")

;; (defn <-base-58 [in]


;;   )

;; (defn uuid->58 [u]
;;   (let [bytes (uuid->byte-array u)]
;;     (loop [b bytes
;;            offset 0]

;;       )))


;; "The quick brown fox jumps over the lazy dog."

;; (uuid-as-byte-array)
