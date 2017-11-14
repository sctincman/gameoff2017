(ns gameoff.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [gameoff.core :as core]
            [gameoff.render.core :as render]
            [gameoff.signals :as signals]
            [gameoff.vector :as v]
            [devcards.core :as dc])
  (:require-macros
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(defcard-rg fresh-game
  (fn [game-state _] [core/reagent-renderer game-state])
  (atom {:test-cube {:position v/zero
                     :rotation v/zero
                     :renders {:base {:type :cube
                                      :geom :cube
                                      :material :cube}}}})
  {:inspect-data true})

(defonce signal-atom
  (let [a (atom 0)]
    (signals/map (fn [bah]
                   (reset! a bah))
                 (signals/tick 1000))
    a))

(defcard signal-watch
  "Signal update an atom"
  (fn [data-atom _]
    @signal-atom)
  signal-atom
  {:inspect-data true})

(defonce backing-atom
  (let [a (atom 0)
        out (signals/signal 0 :backing-atom a)]
    (signals/foldp inc 0 (signals/tick 1000) :out-signal out)
    a))

(defcard signal-watch2
  "Signal atom replaced"
  (fn [data-atom _]
    @backing-atom)
  backing-atom
  {:inspect-data true})

(defonce observed-atom
  (let [a (atom 0)]
    (js/setInterval (fn [] (swap! observed-atom inc)) 1000)
    a))

(defcard atom-observing-card observed-atom)

(reagent/render [:div] (.getElementById js/document "app"))

;; remember to run 'lein figwheel devcards' and then browse to
;; http://localhost:3449/cards
