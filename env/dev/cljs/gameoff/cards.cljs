(ns gameoff.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [gameoff.core :as core]
            [gameoff.render :as render]
            [gameoff.signals :as signals]
            [devcards.core :as dc])
  (:require-macros
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(defcard-rg fresh-game
  (fn [game-state _] [core/reagent-renderer game-state])
  (atom {})
  {:inspect-data true})

(defonce signal-atom
  (let [a (atom 0)]
    (signals/map (fn [bah]
                   (reset! a bah))
                 (signals/tick 500))
    a))

(defcard signal-watch
  "Signal update an atom"
  (fn [data-atom _]
    @signal-atom)
  signal-atom
  {:inspect-data true})

(defonce atom-signal
  (let [a (atom 0)]
    (assoc (signals/tick 500)
           :value a)
    a))

(defcard signal-watch2
  "Signal atom replaced"
  (fn [data-atom _]
    @atom-signal)
  atom-signal
  {:inspect-data true})

(defonce observed-atom
  (let [a (atom 0)]
    (js/setInterval (fn [] (swap! observed-atom inc)) 1000)
    a))

(defcard atom-observing-card observed-atom)

(reagent/render [:div] (.getElementById js/document "app"))

;; remember to run 'lein figwheel devcards' and then browse to
;; http://localhost:3449/cards
