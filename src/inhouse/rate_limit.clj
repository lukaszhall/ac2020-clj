(ns inhouse.rate-limit
  (:require [mount.core :as mount :refer [args defstate]]
            [mount-up.core :as mu]))

;; Verbose logging for mount start/stop
;(mu/on-upndown :info mu/log :before)
;
;(defstate exec-time-ms
;          :start (atom (System/currentTimeMillis))
;          :stop nil)


(defn- exec-fn
  [f args]
  (let [v (apply f args)]
    {:fn-val v}))


(defn rate-limit
  [period-ms f]
  #_(println (type f))
  (let [exec-time-ms_ (atom 0)]

    (fn rate-limit-wrapper
      [& args]
      (let [[pre-swap post-swap] (swap-vals! exec-time-ms_
                                             (fn [exec-time-ms]
                                               (let [now-ms     (System/currentTimeMillis)
                                                     elapsed-ms (- now-ms exec-time-ms)]
                                                 (if (> elapsed-ms period-ms)
                                                   now-ms
                                                   exec-time-ms))))
            period-elapsed? (not= post-swap pre-swap)]
        (when period-elapsed?
          (exec-fn f args))))))





(comment

  (mount/start)
  #_=> {:started ["#'inhouse.rate-limit/rate-limit-time"]}

  (System/currentTimeMillis)
  #_=> 1667284549319


  (exec-fn inc 1)
  #_=> {:fn-val 1}

  (do
    (def rl-inc (rate-limit 1000 inc))

    [(rl-inc 4)
     (rl-inc 4)
     (do (Thread/sleep 1000)
         (rl-inc 4))])
  #_=> [{:fn-val 5} nil {:fn-val 5}]

  )