(ns awbc.rules
  (:require
    [awbc.damage :as damage]
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
  [:infantry :mech :wheels :treads
   :air :sea
   ;; TODO and more
   ])

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
   [:can-load-units [:set (->enum UnitType)]]
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
   ;; TODO:
   ;;
   ;; [:power-meter nat-int?]
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
                              :can-load-units #{}})
                 (assoc :team team))]
    unit))

(defn create-tile
  ([terrain x y] (create-tile terrain x y {}))
  ([terrain x y {:keys [team] :as opts}]
   (cond-> {:terrain terrain :x x :y y}
     team (assoc :team team))))

(defn create-tiles
  [w h]
  (->> (for [x (range w) y (range h)]
         [[x y] (create-tile :plain x y)])
       (into {})))

(defn sample-tiles
  []
  (-> (create-tiles 3 3)

      (update [0 0] merge {:terrain :hq :team "red"})
      (update [2 2] merge {:terrain :hq :team "blue"})

      (assoc-in [[1 1] :terrain] :mtn)

      (assoc-in [[2 1] :unit] (create-unit :red :infantry))

      (assoc-in [[2 2] :unit] (create-unit :blue :infantry))))

(defn sample-tiles-two
  [w h]
  (-> (create-tiles w h)

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
      (assoc-in [[2 1] :unit] (create-unit :red :infantry))
      (assoc-in [[3 2] :unit] (create-unit :red :infantry))
      (assoc-in [[3 3] :unit] (create-unit :red :infantry))
      (assoc-in [[5 3] :unit] (create-unit :red :infantry))
      ;; (assoc-in [[2 1] :team] :red)

      #_(assoc-in [[2 2] :team] :blue)))

(defn print-tiles
  [tiles]
  (vec (vals (sort-by first tiles))))

(defn new-game []
  {:game {:turn-number 0
          :mode :unselected
          :mode-info {}
          :players []}
   :tiles (create-tiles 6 6)})

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

  (-> (create-tiles 6 6)
      (assoc-in [[3 3] :unit] (create-unit :red :infantry)))
  )
