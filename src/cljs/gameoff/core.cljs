(ns gameoff.core
    (:require [reagent.core :as reagent]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [gameoff.render.core :as render]
              [gameoff.render.threejs.core :as render-backend]
              [gameoff.vector :as v]
              [gameoff.signals :as signals]
              [gameoff.behavior :as behavior]
              [gameoff.physics :as physics]))

(defonce game-state
  (atom {:include "gltf/fox.gltf"
         :scene {:current-scene :Scene
                 :camera :Camera}
         :Fox (-> {:position v/zero
                   :rotation (v/vector 0 10 0)
                   :heading (v/vector 0 0 -1)
                   :renders {}}
                  (behavior/player-movement {"w" :forward "s" :backward})
                  (physics/body 1.0 0.005))
         :camera {:position (v/vector 0 0 200)
                  :renders {}}}))

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
                                        (into state
                                              (comp
                                               (map (fn [[id entity]]
                                                      (if (or (= id :test-cube)
                                                              (= id :fox))
                                                        {id (update-in entity [:rotation :y] - 0.01)}
                                                        {id entity})))
                                               (behavior/propagate step)
                                               (physics/propagate step)
                                               (render/renderx backend
                                                               (get-in state [:scene :camera] :camera)
                                                               (get-in state [:scene :current-scene] :default)
                                                               step))
                                              state))
                                      (swap! state-atom assoc :backend backend)
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
