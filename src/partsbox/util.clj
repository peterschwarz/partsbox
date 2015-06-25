(ns partsbox.util
  (:import [java.util.concurrent TimeUnit]))

(defn delay-micros [micros]
  (.sleep TimeUnit/MICROSECONDS micros))
