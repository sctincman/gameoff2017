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

(defonce game-state (atom {:test-cube {:position v/zero
                                       :rotation v/zero
                                       :renders [{:type :cube
                                                  :geom :cube
                                                  :material :cube}]}}))

(defn reagent-renderer [state-atom]
  (let [frame-signal (render/frames)
        world (signals/->Signal state-atom :world (atom {}))]
    (reagent/create-class
     {:display-name "threejs-canvas"
      :reagent-render
      (fn threejs-canvas-render []
        [:canvas])
      :component-did-mount
      (fn threejs-canvas-did-mount [this]
        (let [e (reagent/dom-node this)
              backend (render-backend/init-renderer @state-atom e)
              scenes (if (some? (:include @state-atom))
                       (render-backend/load-gltf (:include @state-atom))
                       (atom {}))
              bah    (swap! state-atom assoc :scenes scenes)
              worldbah (signals/foldp (fn animate [state step]
                                        (when-let [bahscene (get-in @scenes [:Scene :root])]
                                          (.add (:scene (:obj backend))
                                                bahscene)
                                          (swap! scenes assoc-in [:Scene :root] nil))
                                        (into state
                                              (comp
                                               (map (fn [[id entity]]
                                                      (if (or (= id :test-cube)
                                                              (= id :fox))
                                                        {id (update-in entity [:rotation :y] - 0.01)}
                                                        {id entity})))
                                               (behavior/propagate step)
                                               (physics/propagate step)
                                               (render/renderx (:obj backend)
                                                               (:camera backend)))
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
  [:div [:h2 "Welcome to gameoff"]
   [:div [:a {:href "/about"} "go to about page"]]
   [reagent-renderer game-state]])

(defn about-page []
  [:div [:h2 "About gameoff"]
   [:div [:a {:href "/"} "go to the home page"]]])

;; -------------------------
;; Routes

(def page (reagent/atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/about" []
  (reset! page #'about-page))

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
