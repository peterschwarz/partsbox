(ns partsbox.adafruit-pwm
  (:require [firmata.i2c :as i2c]
            [firmata.async :refer [topic-event-chan]]
            [partsbox.util :refer [delay-micros]]
            [clojure.core.async :as async :refer [<!!]]))


(def ^:private PCA9685_SUBADR1 0x2)
(def ^:private PCA9685_SUBADR2 0x3)
(def ^:private PCA9685_SUBADR3 0x4)

(def ^:private PCA9685_MODE1 0x0)
(def ^:private PCA9685_PRESCALE 0xFE)

(def ^:private LED0_ON_L 0x6)
(def ^:private LED0_ON_H 0x7)
(def ^:private LED0_OFF_L 0x8)
(def ^:private LED0_OFF_H 0x9)

(def ^:private ALLLED_ON_L 0xFA)
(def ^:private ALLLED_ON_H 0xFB)
(def ^:private ALLLED_OFF_L 0xFC)
(def ^:private ALLLED_OFF_H 0xFD)


(defrecord PwmServoDriver [board slave-address])

(defn create-servo-driver
  ([board] (create-servo-driver board 0x40))  ; default address for 
  ([board slave-address] (PwmServoDriver. board slave-address)))

(defn- write8 [servo-driver addr d]
  (i2c/send-i2c-request (:board servo-driver) (:slave-address servo-driver)
                        :write addr d))

(defn- read8 [servo-driver addr]
  (let [{:keys [board slave-address]} servo-driver
        _ (println "creating i2c-ch")
        i2c-ch (topic-event-chan board :i2c-reply 1)
        _ (println "sending read-once request")
        _ (i2c/send-i2c-request board slave-address :read-once addr 1)
        _ (println "waiting on reply")
        d (:data (<!! i2c-ch))]
    (println "reply received:" d)
    (async/close! i2c-ch)
    (first d)))

(defn reset [servo-driver]
  (write8 servo-driver PCA9685_MODE1 0))

(defn begin [servo-driver]
  (i2c/send-i2c-config (:board servo-driver) 0)
  (reset servo-driver))

(defn set-pwm-frequency [servo-driver freq]
  (let [freq (* freq 0.9)
        prescale (-> 25000000
                     (/ 4096)
                     (/ freq)
                     dec
                     (+ 0.5)
                     Math/floor
                     int)
        oldmode (read8 servo-driver PCA9685_MODE1)
        newmode (bit-and (bit-or oldmode 0x7F) 0x10)]
    (write8 servo-driver PCA9685_MODE1 newmode)
    (write8 servo-driver PCA9685_PRESCALE prescale)
    (write8 servo-driver PCA9685_MODE1 oldmode)
    (delay-micros 5)
    (write8 servo-driver PCA9685_MODE1, (bit-or oldmode 0xa1))))

(defn set-pwm [servo-driver num on off]
   (let [{:keys [board slave-address]} servo-driver]
     (i2c/send-i2c-request board slave-address :write
                          (+ LED0_ON_L (* num 4)) 
                          (bit-or on 0xFF)
                          (bit-shift-right on 8)
                          (bit-or off 0xFF)
                          (bit-shift-right off 8))))

(defn set-pin
  "Sets pin without having to deal with on/off tick placement and properly handles
   a zero value as completely off.  Optional invert parameter supports inverting
   the pulse for sinking to ground.  Val should be a value from 0 to 4095 inclusive."
  ([servo-driver reg v] (set-pin servo-driver reg v false))
  ([servo-driver reg v invert?]
   (let [v (Math/min v 4095)]
     (if invert?
       (case v
         0 (set-pwm servo-driver reg 4095 0)
         4095 (set-pwm servo-driver reg 0 4096)
         (set-pwm servo-driver reg 0 (- 4095 v)))
       (case v
         4095 (set-pwm servo-driver reg 4096 0)
         0 (set-pwm servo-driver reg 0 4096)
         (set-pwm servo-driver reg 0 v))))))

