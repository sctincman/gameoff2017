(ns gameoff.core
    (:require [reagent.core :as reagent]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [gameoff.render :as render]
              [gameoff.signals :as signals]))

(defonce game-state (atom {}))

(defn reagent-renderer [state-atom]
  (let [frame-signal (render/frames)]
    (reagent/create-class
     {:display-name "threejs-canvas"
      :reagent-render
      (fn threejs-canvas-render []
        [:canvas])
      :component-did-mount
      (fn threejs-canvas-did-mount [this]
        (let [e (reagent/dom-node this)
              state (render/init-renderer @state-atom e)
              world (signals/foldp (fn animate [state step]
                                     (let [mesh (get-in state [:backend :mesh])
                                           r (get-in state [:backend :renderer])
                                           scene (get-in state [:backend :scene])
                                           camera (get-in state [:backend :camera])]
                                       (aset mesh "rotation" "y" (+ 0.01 (.-y (.-rotation mesh))))
                                       (.render r scene camera)
                                       state))
                                   (reset! state-atom state)
                                   frame-signal
                                   :backing-atom state-atom)]))
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
