(ns gameoff.signals)

(defprotocol ^:export ISignal
  "A signal propogates values from a single producer to many consumers.
   Consumers register with watch (which mirrors `add-watch`). Producers
   use `propagate` to send a new value to consumers."
  (watch [this target handler] "Registers handler when signal value changes. `target` is a key used to identifier the consumer.")
  (unwatch [this target])
  (propagate [this value] "Pushs a new value to consumers.")
  (value [this] "Returns the current value of the signal."))

(defrecord Signal [value tag properties]
  ISignal
  (watch [this target handler]
    (add-watch value target
               (fn [key ref old new]
                 (handler key old new))))
  (unwatch [this target]
    (remove-watch value target))
  (propagate [this new-value]
    (reset! value new-value))
  (value [this]
    @value))

(defn ^:export signal
  "Creates a new, unbuffered `atom` based signal with an optional initial value (defaults to `nil`), and optional tag prefix."
  ([] (signal nil))
  ([init & {:keys [prefix backing-atom] :or {prefix "signal_" backing-atom (atom init)}}]
   (->Signal backing-atom (keyword (gensym prefix)) (atom {}))))

(defn ^:export fold
  "Transduction of input signal using transducer."
  [xform in-signal & {:keys [out-signal]
                      :or {out-signal (signal nil :prefix "fold")}}]
  (let [f (xform
           (fn
             ([] out-signal)
             ([result input]
              (propagate out-signal (unreduced input))
              input)
             ([result]
              (unwatch in-signal (:tag out-signal))
              result)))]
    (watch in-signal (:tag out-signal)
           (fn [target old-state new-state]
             (let [result (f old-state new-state)]
               (when (reduced? result)
                 (f result)))))
    (f)))

(defn ^:export foldp
  "Fold from past. Returns a new `Signal` that is the result of reduceing over an input `Signal` with the supplied function. `f` is a reducing function that takes 2 arguments: an accumulated state (current value of output `Signal`), and the current value of the input `Signal`. The return value becomes the new value of the output `Signal`.`init` is a starting value for the output `Signal`. `in-signal` is the input `Signal`. Returns the output `Signal`, allowing others to subscribe and consume from it."
  [f init in-signal & {:keys [out-signal]
                       :or {out-signal (signal init :prefix "foldp")}}]
  (let []
    (watch in-signal (:tag out-signal)
           (fn [target old-state new-state]
             (propagate out-signal (f (value out-signal) new-state))))
    out-signal))

;; expand to arbitrary amounts of in-signals when I can figure out how to handle the asynchronous nature...
;;; When one signal arrives, but others are unchanging, what is the behavior?
;;;; 1) Output new value with all signal inputs as-is (one changes changes out)
;;;; 2) Wait for all signals to change (must keep track of all signals...)
(defn- map*
  [f in-signal & {:keys [out-signal]
                  :or {out-signal (signal nil :prefix "map")}}]
  (watch in-signal (:tag out-signal)
         (fn [target old-state new-state]
           (propagate out-signal (f new-state))))
  out-signal)

(defn ^:export split
  "Splits signal by `pred`. Returns two signals, one for values that pass, and one for those that fail. Return value is a map {:true true-signal, :false false-signal}, that can be destructured."
  [pred in-signal & {:keys [true-signal false-signal]
                     :or {true-signal (signal nil :prefix "split-true")
                          false-signal (signal nil :prefix "split-false")}}]
  (watch in-signal (:tag true-signal)
         (fn [target old-state new-state]
           (if (pred new-state)
             (propagate true-signal new-state)
             (propagate false-signal new-state))))
  {:true true-signal, :false false-signal})

(defn- filter*
  [pred in-signal & {:keys [out-signal]
                     :or {out-signal (signal nil :prefix "filter")}}]
  (watch in-signal (:tag out-signal)
         (fn [target old-state new-state]
           (when (pred new-state)
             (propagate out-signal new-state))))
  out-signal)

(defn ^:export route
  "Given an input-signal, and sequence of predicates, returns a sequence of output-signals, each triggered when their predicate is true. Predicates are of arity 3, receving the target tag, old-state, and new-state from the input-signal, returning true if the signal should trigger the corresponding output-signal."
  [input-signal & rest-args]
  (let [routes (map (fn [pred]
                      (when (fn? pred)
                        {:signal (signal nil :prefix (str "route-" pred))
                         :pred pred}))
                    rest-args)]
    (watch input-signal (:tag (:signal (first routes)))
           (fn [target old-state new-state]
             (map (fn [{signal :signal pred :pred}]
                    (when (pred target old-state new-state)
                      (propagate signal new-state)))
                  routes)))
    routes))

(defn route*
  [input-signal & rest-args]
  (map (fn [pred]
         (when (fn? pred)
           (filter* pred input-signal)))
       rest-args))

(defn ^:export timed
  "Returns a signal that emits the time when the input-signal triggers."
  [trigger-signal & {:keys [out-signal]
                     :or {out-signal (signal (system-time) :prefix "timed")}}]
  (watch trigger-signal (:tag out-signal)
         (fn [target old-state new-state]
           (propagate out-signal (system-time))))
  out-signal)

(defn ^:export delta-time
  "Returns a signal that emits the change in a time-signal."
  [time-signal & {:keys [out-signal]
                  :or {out-signal (signal 0.0 :prefix "delta-t")}}]
  (watch time-signal (:tag out-signal)
         (fn [target old-state new-state]
           (propagate out-signal (- new-state old-state))))
  out-signal)

(defn ^:export dt
  "Derivate of in-signal wrt to time"
  [in-signal & {:keys [out-signal]
                :or {out-signal (signal nil :prefix "dt")}}]
  (delta-time (timed in-signal) :out-signal out-signal))

;;TODO handle to removeInterval...
(defn ^:export tick
  "Returns a signal that triggers every `delay` milliseconds. Defaults to 0 ms (as fast as possible). CLJS uses `setInterval` and is thus limited to a 10 ms minimum delay."
  [delay & {:keys [out-signal]
              :or {out-signal (signal (system-time) :prefix "time")}}]
  (js/setInterval
   (fn [] (propagate out-signal (system-time)))
   delay)
  out-signal)

(defn ^:export switch
  "Reverse route, switch on signal. Switch signal provides key for signal to watch"
  [switch-signal inputs & {:keys [out-signal]
                           :or {out-signal (signal nil :prefix "switch")}}]
  (let [init-switch (value switch-signal)]
    (when (some? init-switch)
      (watch (get inputs init-switch) (:tag out-signal)
             (fn [target old new]
               (propagate out-signal new))))
    (watch switch-signal (:tag out-signal)
           (fn [target old-switch new-switch]
             (unwatch (get inputs old-switch) (:tag out-signal))
             (watch (get inputs new-switch) (:tag out-signal)
                    (fn [target old new]
                      (propagate out-signal new)))))
    out-signal))

;; exports
(def ^:export map
  "Returns a new signal that is the application of `f` to all the input signals. Must match the arity of the number of input signal."
  map*)

(def ^:export filter
  "Returns a new signal that with values from `in-signal` that satisfy `pred`."
  filter*)
