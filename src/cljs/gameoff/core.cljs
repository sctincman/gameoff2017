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
  (atom {:include "gltf/scene.gltf"
         :scene {:current-scene :Scene
                 :camera :PlayerCamera}
         :Fox (-> {:position [0.0 2.5 0]
                   :rotation [0.7071068286895752
                              0.0
                              0.0
                              0.7071068286895752]
                   :heading [0 1 0]
                   :up [0 0 -1]
                   :renders {}}
                  (behavior/player-movement
                   {"w" :forward
                    "s" :backward
                    "a" :left
                    "d" :right
                    "q" :turn-left
                    "e" :turn-right})
                  (behavior/moveable)
                  (physics/body 1.0 0.005))}))

(defn reagent-renderer [state-atom]
  (let [frame-signal (render/frames)
        world (signals/->Signal state-atom :world (atom {}))]
    (reagent/create-class
     {:display-name "threejs-canvas"
      :reagent-render
      (fn threejs-canvas-render []
        [:canvas.gameview])
      :component-did-mount
      (fn threejs-canvas-did-mount [this]
        (let [e (reagent/dom-node this)
              backend (render-backend/setup-scene
                       (render-backend/init-renderer e)
                       @state-atom)
              worldbah (signals/foldp (fn step-world [state step]
                                        (-> state
                                            (behavior/propagate step state)
                                            (into (comp
                                                   (map (fn [[id entity]]
                                                          (if (or (= id :test-cube)
                                                                  (= id :fox))
                                                            {id (update entity :rotation q/qmul rotate-q)}
                                                            {id entity})))
                                                   (behavior/propagate step)
                                                   (physics/propagate step)
                                                   (render/renderx backend
                                                                   (get-in state [:scene :camera] :camera)
                                                                   (get-in state [:scene :current-scene] :default)
                                                                   step))
                                                  state)))
                                      (swap! state-atom
                                             (fn [state]
                                               (-> state
                                                   (assoc :backend backend)
                                                   (behavior/add-world-commands {"b" :bounding-boxes-toggle})
                                                   (collision/add-space 15.0))))
                                      (signals/dt frame-signal)
                                      :out-signal world)]))
      :component-will-unmount
      (fn [this]
        (.cancelAnimationFrame js/window (get (deref (get frame-signal :properties))
                                              :request-id)))})))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Github Gameoff 2017"]
   [reagent-renderer game-state]])

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
