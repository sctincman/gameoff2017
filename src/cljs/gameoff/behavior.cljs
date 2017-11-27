(ns gameoff.behavior
  (:require [gameoff.signals :as s]
            [gameoff.input :as input]
            [gameoff.physics :as physics]
            [clojure.core.matrix :as m]))

(defn ^:export behavioral? [entity]
  (some? (or (get entity :behaviors)
             (get entity :states))))

(defn ^:export add-behavior [entity behavior]
  (update entity :behavior (fn [behaviors]
                             (if (contains? entity :behavior)
                               (comp behavior behaviors)
                               behavior))))

(defn update-state [state entity delta-t world]
  (let [current-state (:state state)
        transition-list (get-in state [:table current-state])
        transition
        (first (filter (fn [{conds :conditions}]
                         (if (empty? conds)
                           true
                           (some true? (map #(when (fn? %)
                                               (% entity delta-t world))
                                            conds)))) 
                       transition-list))]
    (if (some? transition)
      (do
        (when-let [exit-handler (get-in state [:table current-state :exit])]
          (exit-handler entity delta-t world))
        (when-let [enter-handler (:enter transition)]
          (enter-handler entity delta-t world))
        (assoc state :state (:transition transition)))
      state)))

(defn update-states [entity delta-t world]
  (update entity :states (fn [states]
                           (map (fn [state]
                                  (update-state state entity delta-t world))
                                states))))

(defn ^:export propagate
  "Propagate AI behavior over time"
  ([entity delta-t world]
   (if (behavioral? entity)
     (-> entity
         (update-states delta-t world)
         ;#((get % :behavior) % delta-t world)
         )
     entity))
  ([delta-t]
   (fn [xform]
     (fn
       ([] (xform))
       ([world] (xform world))
       ([world entity]
        (let [[id automata] (first entity)]
          (xform world {id (propagate automata delta-t world)})))))))


(defn backward-command [entity delta-t world]
  (when-let [command (s/value (:commands entity))]
    (= :backward command)))

(defn forward-command [entity delta-t world]
  (when-let [command (s/value (:commands entity))]
    (= :forward command)))

(defn stop-command [entity delta-t world]
  (when-let [command (s/value (:commands entity))]
    (= :stop command)))

(def walking
  (let [backward-state {:conditions [backward-command]
                        :transition :walking-backward}
        forward-state {:conditions [forward-command]
                       :transition :walking-forward}
        standing-state {:conditions [stop-command]
                        :transition :standing}]
    {:state :walking-forward
     :table {:standing [forward-state backward-state]
             :walking-forward [standing-state backward-state]
             :walking-backward [standing-state forward-state]}}))

(defn ^:export moveable [entity]
  (update entity :states conj walking))

(defn update-fsm [entity delta-t world])

(defn ^:export player-movement
  "Given a keymap and entity, add input-driven movement component to entity, and returns updated entity."
  [entity keymap]
  (let [input-signal (s/map (fn [event]
                              (if-let [command (keymap (:key event))]
                                (if (= :down (:press event))
                                  command
                                  :stop)))
                            input/keyboard)]
    ;; check if exists?
    (-> entity
        (assoc :input keymap)
        (assoc :commands input-signal))))

(defn- follow*
  [target offset]
  (fn [entity dt world]
    (let [followee (get world target)]
      (if (and (some? (:position entity))
               (some? (:position followee)))
        ;;complicated behavior goes here
        (assoc entity :position (m/add (:position followee)
                                       offset))
        entity))))

(defn ^:export follow
  [entity target offset]
  ;; add position if it isn' there
  ;; add in behavior component
  (add-behavior entity (follow* target offset)))
