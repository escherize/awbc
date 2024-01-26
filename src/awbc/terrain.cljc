(ns awbc.terrain)

(defn ->stars
  [t]
  ;; TODO check these:
  (case t
    (:road :shoal :bridge :water) 0
    (:plain :reef) 1
    :woods 2
    (:city :dock :factory :airport) 3
    (:hq :mtn) 4))
