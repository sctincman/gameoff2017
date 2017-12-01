(ns gameoff.core
    (:require [reagent.core :as reagent]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [gameoff.render.core :as render]
              [gameoff.render.threejs.core :as render-backend]
              [gameoff.signals :as signals]
              [gameoff.behavior :as behavior]
              [gameoff.physics :as physics]
              [gameoff.collision :as collision]
              [gameoff.quaternion :as q]))

(def rotate-q (q/axis-angle->q [0 1 0] 0.2))

(defonce game-state
  (atom {:up [0 1 0]
         :include "gltf/scene.gltf"
         :scene {:current-scene :Scene
                 :camera :PlayerCamera}
         :Cube {:collision true}
         :BigCube {:collision true}
         :Cylinder {:collision true}
         :Ground {:collision true}
         :groups {:BigCube {:collision true}}
         :Fox (-> {:heading [0 1 0]
                   :up [0 0 -1]
                   :renders {}
                   :collision {:pre collision/clear-velocities
                               :internal collision/wabb-solid-handler}}
                  (behavior/player-movement
                   {"w" :forward
                    "s" :backward
                    "a" :left
                    "d" :right
                    "q" :turn-left
                    "e" :turn-right
                    " " :jump})
                  (behavior/moveable)
                  (physics/body 1.0 0.005))}))

(defn reagent-renderer [state-atom]
  (let [frame-signal (render/frames)
        world-base (signals/->Signal state-atom :world (atom {}))]
    (reagent/create-class
     {:display-name "threejs-canvas"
      :reagent-render
      (fn threejs-canvas-render []
        [:canvas.gameview])
      :component-did-mount
      (fn threejs-canvas-did-mount [this]
        (let [e (reagent/dom-node this)
              world-signal (signals/foldp
                            (fn step-world [world step]
                              (-> world
                                  (behavior/step step)
                                  (physics/step step)
                                  (collision/handle-collisions step)
                                  (render/render step)))
                            (swap! state-atom
                                   (fn [state]
                                     (-> state
                                         (behavior/add-world-commands {"b" :bounding-boxes-toggle})
                                         (render-backend/setup-scene e)
                                         (collision/add-space 15.0))))
                            (signals/dt frame-signal)
                            :out-signal world-base)]))
      :component-will-unmount
      (fn [this]
        (.cancelAnimationFrame js/window (get (deref (get frame-signal :properties))
                                              :request-id)))})))

;; -------------------------
;; Views

(defn home-page []
  [:div [reagent-renderer game-state]])

;; -------------------------
;; Routes

(def page (reagent/atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
