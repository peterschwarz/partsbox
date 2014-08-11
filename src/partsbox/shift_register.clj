(ns partsbox.shift-register
  (:require [firmata.core :refer [set-digital]]))

(defrecord ShiftRegister [board latch-pin data-pin clock-pin bit-order])

(defn create-shift-register 
  ([board latch-pin data-pin clock-pin] (create-shift-register board latch-pin data-pin clock-pin :lsb-first))
  ([board latch-pin data-pin clock-pin bit-order] 
    {:pre [(or (= bit-order :lsb-first) (= bit-order :msb-first))]}
    (ShiftRegister. board latch-pin data-pin clock-pin bit-order)))

(defn shift-out
  "Sends a shift out to the board, for sending values to a shift register"
  [shift-register value]
  (let [board (:board shift-register)
        latch-pin (:latch-pin shift-register)
        data-pin (:data-pin shift-register)
        clock-pin (:clock-pin shift-register)
        is-lsb-first (= :lsb-first (:bit-order shift-register))]
  
    (set-digital board latch-pin :high)
  
    (doseq [i (range 8)]
      (let [shift-by (if is-lsb-first i (- 7 i))]
        (set-digital board data-pin (if (= 0 (bit-and value (bit-shift-left 1 shift-by))) :low :high)))
  
      (set-digital board clock-pin :high)
      (set-digital board clock-pin :low))
  
    (set-digital board latch-pin :low))

  shift-register)
