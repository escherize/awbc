(ns awbc.damage
  (:require
    [awbc.terrain :as terrain]))

(defn ^:private format-row
  [args]
  (let [->p+s (fn [[p s]] {:primary p :secondary s})
        ->p (fn [p] {:primary p})
        [i mp ms r tp ts mtp mts ntp nts a rok aa mis bcp bcs f b cp cs sub bs] args]
    (->> (mapv vector
               [:infantry :mech :recon :tank
                :medium-tank :neotank :artillery
                :rockets :anti-air :missiles
                :b-copter :fighter :bomber
                :cruiser :submarine :battleship]
               [(->p i) (->p+s [mp ms]) (->p r) (->p+s [tp ts])
                (->p+s [mtp mts]) (->p+s [ntp nts]) (->p a)
                (->p rok) (->p aa) (->p mis)
                (->p+s [bcp bcs]) (->p f) (->p b)
                (->p+s [cp cs]) (->p sub) (->p bs)])
         (into {}))))

(defn- map-vals [m f]
  (zipmap (keys m) (map f (vals m))))

(def ^:private damage-table
  ;;;;;;;;;;;;;;;;;;;;;;;;;; i   mp  ms  r    tp  ts  mtp mts ntp nts a  rok  aa  mis bcp bcs f   b   cp  cs  sub bs
  (->> {:infantry    [55  nil 65  70   35  75  50  105 50  125 90  95  105 nil 45  75  nil 110 nil nil nil 95]
        :mech        [45  nil 55  65   30  70  45  95  45  115 85  90  105 nil 50  75  nil 110 nil nil nil 90]
        :recon       [12  85  18  35   85  40  105 45  125 65  80  90  60  nil 55  30  nil 105 nil nil nil 90]
        :tank        [5   55  6   6    55  6   85  8   105 10  70  85  25  nil 55  6   nil 105 nil nil nil 85]
        :medium-tank [1   15  1   1    15  1   55  1   75  1   45  55  10  nil 25  1   nil 95  nil nil nil 55]
        :neotank     [1   15  1   1    15  1   45  1   55  1   40  50  5   nil 20  1   nil 90  nil nil nil 50]
        :apc         [14  75  20  45   75  45  105 45  125 65  70  80  50  nil 60  20  nil 105 nil nil nil 80]
        :artillery   [15  70  32  45   70  45  105 45  115 65  75  80  50  nil 65  25  nil 105 nil nil nil 80]
        :rockets     [25  85  35  55   85  55  105 55  125 75  80  85  45  nil 65  35  nil 105 nil nil nil 85]
        :anti-air    [5   65  6   4    65  5   105 7   115 17  75  85  45  nil 25  6   nil 95  nil nil nil 85]
        :missiles    [25  85  35  28   85  30  105 35  125 55  80  90  55  nil 65  35  nil 105 nil nil nil 90]
        :b-copter    [7   nil 9   10   nil 10  nil 12  nil 22  nil nil 120 120 nil 65  100 nil nil 115 nil nil]
        :t-copter    [30  nil 35  35   nil 40  nil 45  nil 55  nil nil 120 120 nil 95  100 nil nil 115 nil nil]
        :fighter     [nil nil nil nil  nil nil nil nil nil nil nil nil 65  100 nil nil 55  nil nil 55  nil nil]
        :bomber      [nil nil nil nil  nil nil nil nil nil nil nil nil 75  100 nil nil 100 nil nil 65  nil nil]
        :lander      [nil nil nil nil  10  nil 35  nil 40  nil 55  60  nil nil 25  nil nil 95  nil nil 95  95]
        :cruiser     [nil nil nil nil  5   nil 45  nil 50  nil 65  85  nil nil 55  nil nil 85  nil nil 25  95]
        :submarine   [nil nil nil nil  1   nil 10  nil 15  nil 60  85  nil nil 25  nil nil 95  90  nil 55  95]
        :battleship  [nil nil nil nil  1   nil 10  nil 10  nil 40  55  nil nil 25  nil nil 75  nil nil 55  50]
        :pipe-seam   [1   15  1   1    15  1   55  1   75  1   45  55  10  nil 25  1   nil 95  nil nil nil 55]}
       (into (sorted-map))
       (#(map-vals % format-row))))

(defn base-damage [from to]
  (get-in damage-table [to from]))

(defn can-attack?
  [from-unit to-unit]
  (first (let [base (base-damage from-unit to-unit)]
           (if (map? base) (vals base) base))))

(defn compute
  [from-tile to-tile]
  ;; TODO: https://warswiki.org/wiki/Damage
  (let [attacker (-> from-tile :unit)
        defender (-> to-tile :unit)
        attacker-type (:type attacker)
        defender-type (:type defender)
        ;; b = Base damage (in damage chart)
        bb (base-damage attacker-type defender-type)
        ;; TODO: handle the ammo thing
        b (cond
            (number? bb) ; no ammo required
            bb
            (and (map? bb) (zero? (:ammo attacker)))
            (:secondary bb)

            :else
            (:primary bb))
        ;; TODO s = Strength modifier of CO (day-to-day boosts, i.e. 120% mechs for Sami => 1.2)
        s 1
        ;; TODO d = Defense modifier of CO (again, day-to-day, i.e. 90% artillery for Max => 0.9)
        d 1
        ;; a = HP of attacker
        a (:hp attacker)
        ;; r = Defense rating (number of stars)
        r (terrain/->stars (:terrain to-tile))
        ;; t = Total damage. This is the first part of the equation, (b*s/d)*(a*.1).
        t (* b (/ s d) a 0.1)
        ;; h = HP lost by defender
        defender-hp (-> defender :hp)]
    ;; f= [t]-r[(t*.1)-(t*.01*h)] (%)
    (when (and t r defender-hp)
      (-> (- t (* r (- (* t 0.1) (* t defender-hp 0.01)))) (/ 10.0)))))

(defmulti mover "returns possible coordinates for a unit that is movable."
  (fn [unit map] (:move-type unit)))

(defmethod mover :default [unit map]
  (throw (ex-info "Something broke in mover, invalid unit?" {:unit unit :map map})))
