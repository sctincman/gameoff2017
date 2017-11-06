(ns ^:figwheel-no-load gameoff.dev
  (:require
    [gameoff.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
