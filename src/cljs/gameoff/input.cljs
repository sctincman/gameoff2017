(ns gameoff.input
  (:require [gameoff.signals :as s]))

;; Optionally debounce? (eg, weird repeat behavior on Linux)
(defonce keyevents
  (let [out-signal (s/signal nil "keyboard")]
    (.addEventListener
     js/document
     "keydown"
     (fn [event]
       (s/propagate out-signal
                  {:key (.-key event),
                   :repeat (.-repeat event),
                   :press :down})))
    (.addEventListener
     js/document
     "keyup"
     (fn [event]
       (s/propagate out-signal
                  {:key (.-key event),
                   :repeat (.-repeat event),
                   :press :up})))
    out-signal))

(def ^:export keyboard
  "A signal generated from keyboard events. Events are maps with fields for key, repeat, and whether it is a keydown/keyup event."
  keyevents)
