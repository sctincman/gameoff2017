(ns gameoff.cards.game
  (:require [reagent.core :as reagent :refer [atom]]
            [gameoff.core :as core]
            [gameoff.render.core :as render]
            [gameoff.signals :as signals]
            [gameoff.behavior :as behavior]
            [gameoff.physics :as physics]
            [gameoff.collision :as collision]
            [devcards.core :as dc])
  (:require-macros
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(defonce game-state
  (atom {:include "gltf/scene.gltf"
         :scene {:current-scene :Scene
                 :camera :PlayerCamera}
         :Cube {:collidable true}
         :BigCube {:collidable true}
         :Cylinder {:collidable true}
         :Fox (-> {:heading [0 1 0]
                   :up [0 0 -1]
                   :renders {}
                   :collidable true
                   :collision-handlers {:internal collision/wabb-solid-handler}}
                  (behavior/player-movement
                   {"w" :forward
                    "s" :backward
                    "a" :left
                    "d" :right
                    "q" :turn-left
                    "e" :turn-right})
                  (behavior/moveable)
                  (physics/body 1.0 0.005))}))

(defcard-rg load-scene
  (fn [game-state _] [core/reagent-renderer game-state])
  game-state
  {:inspect-data true})
