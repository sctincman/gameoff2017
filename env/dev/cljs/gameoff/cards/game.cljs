(ns gameoff.cards.game
  (:require [reagent.core :as reagent :refer [atom]]
            [gameoff.core :as core]
            [gameoff.render.core :as render]
            [gameoff.signals :as signals]
            [gameoff.behavior :as behavior]
            [gameoff.physics :as physics]
            [devcards.core :as dc])
  (:require-macros
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(defcard-rg load-scene
  (fn [game-state _] [core/reagent-renderer game-state])
  (atom {:include "gltf/fox.gltf"
         :scene {:current-scene :Scene
                 :camera :Camera}
         :Fox (-> {:position [0.0 0.0 0.0]
                   :rotation [0.0 10.0 0.0]
                   :renders {}}
                  (behavior/player-movement {"w" :forward "s" :backward})
                  (physics/body 1.0 0.005))
         :camera {:position [0.0 0.0 200.0]
                  :renders {}}})
  {:inspect-data true})
