(ns awbc.view.main
  (:require
    [awbc.rules]
    [awbc.view.events :as e]
    [awbc.view.subs :as s]
    [re-frame.core :as rf]
    [reagent.core :as reagent :refer [atom]]
    [reagent.dom :as rdom]
    [cljs.pprint :as pp]))


(defn p
  [x]
  (js/console.log (pr-str x)) x)


(defn ->svg-image
  [{:keys [x y terrain team] :as tile}]
  (let [tiles @(rf/subscribe [::s/tiles])
        shadow? (#{:mtn :hq :factory :forest} (:terrain (get tiles [(dec x) y])))]
    (if (#{:mtn :plain :hq :factory :forest} terrain)
      [:svg
       [:svg ; image
        (when (#{:hq :factory} terrain)
          (->svg-image (assoc tile :terrain :plain)))
        [:image
         {:href (case terrain
                  :mtn "assets/mtn.svg"
                  :forest (str "assets/forest" (when shadow? "_shadow")  ".svg")
                  :plain (str "assets/plain" (when shadow? "_shadow")  ".svg")
                  (:hq :factory) (str "assets/" team (name terrain) ".svg"))
          :x (* 64 x)
          :y (* 64 y)
          :width 64
          :height 128}]]]
      (do (js/console.log (str "No ->svg-image for tile:  " (pr-str tile)))
          (throw (js/Error (str "No ->svg-image for tile:  " (pr-str tile))))))))


(defn ->svg-unit
  [{:keys [x y terrain team unit] :as tile}]
  #_{:indirect? false,
     :move-type :infantry,
     :has-moved? false,
     :move 3,
     :can-load-units #{},
     :type :infantry,
     :team :red,
     :hp 10,
     :base-vision 2}
  (let [{:keys [indirect? move-type has-moved? move can-load type team hp base-vision]} unit]
    (when unit
      [:svg
       [:image
        {:href (str "assets/" (name team) (name type) ".svg")
         :x (dec (* 64 x))
         :y (dec (* 64 (inc y)))
         :width 64
         :height 64}]])))


(defn ->svg-interaction
  [{:keys [x y terrain team] :as tile}]
  (let [hovered? (atom false)]
    (fn []
      [:svg
       #_(when @hovered?
         [:rect
          {:fill "none"
           :stroke-width 2
           :stroke "rgba(135, 206, 235,0.8)"
           :x (dec (* 64 x))
           :y (dec (* 64 (inc y)))
           :width 66
           :height 66}])
       [:rect ; hover cell
        {:fill "rgba(135, 206, 235, 0.0)"
         :on-mouse-enter (fn [_]
                           (reset! hovered? true)
                           (rf/dispatch [::e/set-hovered [x y]])
                           #_(js/console.log "mouse entered: " x y))
         :on-mouse-leave (fn [_]
                           (reset! hovered? false)
                           #_(js/console.log "mouse left: " x y))
         :on-mouse-down (fn [_] #_(js/console.log "click down: " x y))
         :on-mouse-up (fn [_] #_(js/console.log "click up: " x y))
         :x (* 64 x)
         :y (* 64 (inc y))
         :width 64
         :height 64}]])))

(defn ->svg-cursor []
  (let [{:keys [x y]} @(rf/subscribe [::s/hovered-tile])]
    [:image
     {:href (str "assets/cursor.svg")
      :x (-  (* 64 x) 32)
      :y (- (* 64 (inc y)) 32)
      :width 128
      :height 128}]))

(defn svg-view
  []
  [:div
   (let [width @(rf/subscribe [::s/width])
         height @(rf/subscribe [::s/height])
         ;; (->> @game :tiles (sort-by first) (mapv second))
         tiles @(rf/subscribe [::s/y-sorted-tiles])]
     (when (and width height)
       (into
         [:svg {:width (* (inc width) 64) :height (* (+ 2 height) 64)}]
         (concat
          (map ->svg-image tiles)
          (map ->svg-unit tiles)
          [(->svg-cursor)]
          (map (fn [t] [->svg-interaction t]) tiles)))))])


(defn game-view
  []
  [:div {:style {:margin-top 20}}
   [:h3 {:style {:width "300px" :margin :auto}}
    "Advance Wars"]
   [:div {:style {:margin "20px"}} [svg-view]]
   [:pre (with-out-str (pp/pprint @(rf/subscribe [::s/hovered-tile])))]])


(defn main!
  []
  (js/document.getElementById "app")
  (rf/dispatch [::e/init!])
  (rdom/render
    [game-view]
    (js/document.getElementById "app")))


(defn reload!
  []
  (rdom/render
    [game-view]
    (js/document.getElementById "app")))


(comment


  )
