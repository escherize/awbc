(ns awbc.view.membrane-tut
  (:require
    [membrane.basic-components :as basic]
    [membrane.component :as component
     :refer [defui defeffect]]
    [membrane.java2d :as java2d]
    [membrane.ui :as ui
     :refer [vertical-layout
             translate
             horizontal-layout
             button
             label
             with-color
             bounds
             spacer
             on]]))


(defn run!
  []
  (-> (let [app-state (atom {})
            checkbox (fn [path]
                       (let [checked? (get-in @app-state path)]
                         (on
                           :mouse-down
                           (fn [_]
                             (swap! app-state update :checkbox not)
                             nil)
                           (ui/label (if checked? "X" "O")))))]
        #(checkbox [:checkbox]))
      (java2d/run {:window-title "hi"
                   :window-start-width 300
                   :window-start-height 300
                   :window-start-x 500
                   :window-start-y 500})))


(comment
(ui/label "Hello World!")
;; label with specified font
(ui/label "Hello\nWorld!" (ui/font "Menlo" 22))

(ui/label "Hello World!" (ui/font nil 22))

  )


(run!)
