(ns awbc.view.main
  (:require
    [awbc.rules]
    [awbc.view.events :as e]
    [awbc.view.subs :as s]
    [re-frame.core :as rf]
    [reagent.core :as reagent :refer [atom]]
    [reagent.dom :as rdom]
    [cljs.pprint :as pp]
    [awbc.movement :as movement]))

(defn log [x] (js/console.log x))

(defn p
  ([x] (js/console.log (pr-str x)) x)
  ([tag x] (js/console.log tag ": " (pr-str x)) x))

(defn pp-str [x]
  (with-out-str (pp/pprint x)))

(def sz 64)

(defn ->svg-image
  [{:keys [x y terrain team] :as tile}]
  (let [tiles @(rf/subscribe [::s/tiles])
        shadow? (#{:mtn :hq :factory :forest} (:terrain (get tiles [(dec x) y])))
        tall? (#{:mtn} (:terrain (get tiles [x (dec y)])))]
    (if (#{:mtn :plain :hq :factory :forest} terrain)
      [:svg
       [:svg ; image
        (when (#{:hq :factory} terrain)
          (->svg-image (assoc tile :terrain :plain)))
        [:image
         {:href (case terrain
                    :mtn (str "assets/mtn" (when tall? "_tall") ".svg")
                    :forest (str "assets/forest" (when shadow? "_shadow")  ".svg")
                    :plain (str "assets/plain" (when shadow? "_shadow")  ".svg")
                    (:hq :factory) (str "assets/" team (name terrain) ".svg"))
          :x (* sz x)
          :y (* sz y)
          :width sz
          :height (* sz 2)}]]]
      (let [msg (str "No ->svg-image for tile:  " (pr-str tile))]
        (js/console.log msg)
        (throw (js/Error msg))))))

(defn ->svg-unit
  [{:keys [x y terrain team unit] :as tile}]
  (let [{:keys [indirect? move-type waited? move can-load type team hp base-vision]} unit]
    ;; TODO: sprites for waited?
    ;; (p "svg-tile:" tile)
    ;; (p "svg-unit:" unit)
    (when unit
      [:svg
       [:image
        (merge
         (when waited? {:style {:filter "grayscale(0.75)"}})
         {:href (str "assets/" (name team) (name type) ".svg")
          :x (dec (* sz x))
          :y (dec (* sz (inc y)))
          :width sz
          :height sz})]])))

;; game modes:
(def game-modes
  #{:unselected :unit-selected :unit-moved
    :factory-selected :aiming :menu?})

(defn ->mouse-over-events [current-player game-mode
                           {:keys [x y terrain team unit] :as tile}]
  (pp-str ["->mouse-over-events" game-mode])
  (case game-mode
    :unselected {:on-click (fn [_]
                             #_(log "clicked")
                             #_(p tile)
                             (cond
                               (and unit
                                    (not (:waited? unit))
                                    ;; "red" team's turn
                                    (= (:team unit) (:team current-player)))
                               (do
                                 (rf/dispatch [::e/set-game-mode :unit-selected {:from-coord [x y]}])
                                 (rf/dispatch [::e/set-movement-coords [x y]])
                                 (rf/dispatch [::e/add-path [x y]]))))}
    :unit-selected (let [can-move-here? @(rf/subscribe [::s/can-move-to? [x y]])]
                     {:fill
                      (str "rgba(135, 206, 235, " (if can-move-here? "0.5" "0.0") ")")
                      ;; TODO
                      :on-mouse-enter (fn [_]
                                        (rf/dispatch [::e/set-hovered [x y]])
                                        (rf/dispatch [::e/add-path [x y]]))
                      :on-click (fn [_]
                                  ;; (log "clicked")
                                  ;; (p tile)
                                  ;; (p "can-move-here?" can-move-here?)
                                  (if can-move-here?
                                    (rf/dispatch [::e/move-unit [x y]])
                                    (rf/dispatch [::e/set-game-mode :unselected])))})
    :unit-moved {:on-click (fn [_]
                             (rf/dispatch [::e/unmove-unit [x y]]))}))

(defn ->svg-interaction
  [current-player game-mode game-mode-info {:keys [x y terrain team] :as tile}]
  [:svg
   #_(when @hovered?
       [:rect
        {:fill "none"
         :stroke-width 2
         :stroke "rgba(135, 206, 235,0.8)"
         :x (dec (* sz x))
         :y (dec (* sz (inc y)))
         :width 66
         :height 66}])
   [:rect ; hover cell
    (merge {:fill "rgba(135, 206, 235, 0.0)"
            :on-mouse-enter (fn [_]
                              (rf/dispatch [::e/set-hovered [x y]]))
            ;;:on-mouse-leave (fn [_])
            :x (* sz x)
            :y (* sz (inc y))
            :width sz
            :height sz}
           (->mouse-over-events current-player game-mode tile))]])

(defn ->svg-cursor []
  (let [{:keys [x y]} @(rf/subscribe [::s/hovered-tile])]
    [:image
     {:href (str "assets/cursor.svg")
      :x (-  (* sz x) (/ sz 2))
      :y (- (* sz (inc y)) (/ sz 2))
      :width (* 2 sz)
      :height (* 2 sz)}]))

(defn menu [on? [x y] {:keys [options]}]
  (let [top-left-x (+ (/ sz 4) (* sz (inc x)))
        top-left-y (+ (/ sz 4) (* sz (inc y)))]
    (when on?
      [:svg
       [:rect
        {:fill "rgba(200,200,200,0.9)"
         :stroke-width 2
         :stroke "rgba(135, 206, 235, 0.8)"
         :x top-left-x
         :y top-left-y
         :width (+ 4 (* sz 1.5))
         :height (+ 4 (* sz 1))}]
       [:text {:x (+ 10 top-left-x)
               :y (+ 25 top-left-y)
               :on-click (fn [] (rf/dispatch [::e/wait-unit [x y]]))
               ;; :on-mouse-enter (fn [] (p "hover"))
               ;; :on-mouse-leave (fn [] (p "hover end"))
               :fill "black"} "Wait"]
       #_[:text {:x (+ 10 top-left-x)
                 :y (+ 50 top-left-y)
                 :fill "black"} "Fire"]])))

;; :start :horiz :southwest :downend
;; [0 0]  [0 1]  [1 1]      [2 1]

(defn dir [[from-x from-y] [to-x to-y]]
  (get {[0 1] :north [0 -1] :south [-1 0] :east [1 0] :west}
       [(- from-x to-x) (- from-y to-y)]))

(defn ->svg-hover-path []
  (let [hovered-path @(rf/subscribe [::s/hover-path])]
    ;; (js/console.log (ppp hovered-path))
    ;; (js/console.log (ppp (partition 3 1 hovered-path)))
    (when (> (count hovered-path) 1)
      [:svg
       ;; start:
       (let [start-dir (dir (first hovered-path) (second hovered-path))
             [x y] (first hovered-path)]
         [:image
          {:href (case start-dir
                   :north "assets/path_start_up.svg"
                   :south "assets/path_start_down.svg"
                   :east "assets/path_start_right.svg"
                   :west "assets/path_start_left.svg")
           :x (dec (* sz x))
           :y (dec (* sz y))
           :width sz
           :height (* 2 sz)}])

       ;; middle:
       (into [:svg]
             (for [[from here to] (partition 3 1 hovered-path)]
               (let [[x y] here
                     from-dir (dir here from)
                     to-dir (dir here to)]

                 [:image
                  (merge
                   {:href (cond
                            (or (= [from-dir to-dir] [:east :west])
                                (= [from-dir to-dir] [:west :east]))
                            (str "assets/path_horizontal.svg")

                            (or (= [from-dir to-dir] [:south :north])
                                (= [from-dir to-dir] [:north :south]))
                            (str "assets/path_vertical.svg")

                            :else
                            (let [[x-dir y-dir] (sort-by #{:north :south} [from-dir to-dir])]
                              (str "assets/path_" (name y-dir) (name x-dir) ".svg")))
                    :x (dec (* sz x))
                    :y (dec (* sz y))
                    :width sz
                    :height (* 2 sz)})])))

       ;;end
       (let [end-dir (dir (last (butlast hovered-path)) (last hovered-path))
             [x y] (last hovered-path)]
         [:image
          {:href (case end-dir
                   :north "assets/path_end_up.svg"
                   :south "assets/path_end_down.svg"
                   :east "assets/path_end_right.svg"
                   :west "assets/path_end_left.svg")
           :x (dec (* sz x))
           :y (dec (* sz y))
           :width sz
           :height (* 2 sz)}])
       ])))

(defn svg-view
  [tiles game-mode game-mode-info current-player]
  [:div
   (let [width @(rf/subscribe [::s/width])
         height @(rf/subscribe [::s/height])]
     #_(p "coord of unit:" game-mode-info)
     (when (and width height)
       (into
        [:svg {:width (* (inc width) sz) :height (* (+ 2 height) sz)}]
        (concat
         (apply concat (mapv (fn [t] [:svg
                                      (->svg-image t)
                                      #_(->svg-unit t)]) tiles))
         (->svg-hover-path)
         (apply concat (mapv (fn [t] [:svg
                                      #_(->svg-image t)
                                      (->svg-unit t)]) tiles))
         [(->svg-cursor)]
         (map (fn [t] [->svg-interaction current-player game-mode game-mode-info t]) tiles)
         [[menu
           (= game-mode :unit-moved)
           (:to-coord game-mode-info)
           {:options ["wait" "fire" "load"]}]]))))])



(defn game-view
  []
  (let [tiles @(rf/subscribe [::s/y-sorted-tiles])
        game (dissoc @(rf/subscribe [::s/game]) :tiles)
        game-mode @(rf/subscribe [::s/game-mode])
        game-mode-info @(rf/subscribe [::s/game-mode-info])
        current-player @(rf/subscribe [::s/current-player-turn])]
    [:div {:style {:margin-top 20}}
     [:h3 {:style {:width "300px" :margin :auto}} "Advance Wars"]
     [:div "coord:" (pr-str (:hovered-coord game))]
     [:div {:style {:margin "20px"}} [svg-view tiles game-mode game-mode-info current-player]]
     [:div "Game:" [:pre (pr-str game)]]
     [:div (pp-str @(rf/subscribe [::s/hover-path]))]
     [:div (pp-str @(rf/subscribe [::s/hovered-tile]))]]))

(defn main!
  []
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
