(ns logjam.framework.telemere
  "Log event capturing implementation for Telemere."
  {:added "0.3.0"}
  (:require
   [logjam.appender :as appender]
   [taoensso.telemere :as telemere]
   [taoensso.telemere.timbre :as timbre]))

(def ^:private log-levels
  "The Telemere level descriptors."
  (->> [:trace :debug :info :warn :error :fatal :report]
       (map-indexed #(assoc {:name %2
                             :category %2
                             :object %2}
                            :weight %1))))

(defn- extract-event-data [{:keys [data level msg_ uid ^java.time.Instant inst ^Throwable error thread file ^String ns line column ctx] :as sig}]
  (cond-> {:arguments data
           :id (java.util.UUID/randomUUID)
           :level level
           :logger (or (when (not-empty ns)
                         (let [sb (StringBuilder. ns)]
                           (when line
                             (.append sb \:)
                             (.append sb (int line)))
                           (when column
                             (.append sb \:)
                             (.append sb (int column)))
                           (.toString sb)))

                       (not-empty file))
           :message (or (not-empty (force msg_))
                        (some-> error .getMessage))
           :thread (or (:name thread)
                       (.getName (Thread/currentThread)))
           :timestamp (some-> inst .toEpochMilli)
           :mdc (or ctx sig {})}
    error (assoc :exception error)))

(defn- add-appender [framework appender]
  (telemere/add-handler!
   (:id @appender)
   (fn [signal]
     (swap! appender appender/add-event (extract-event-data signal))))
  framework)

(defn- log [_framework {:keys [arguments exception level message mdc]}]
  (timbre/with-context (or mdc {})
    (let [msg+ex (into []
                       (remove nil?)
                       [(not-empty message), exception])
          format? (seq arguments)
          vargs (if format?
                  (into msg+ex arguments)
                  msg+ex)]
      (timbre/log! level format? vargs))))

(defn- remove-appender [framework appender]
  (telemere/remove-handler! (:id @appender))
  framework)

(def framework
  "Telemere"
  {:add-appender-fn #'add-appender
   :id "telemere"
   :javadoc-url "https://cljdoc.org/d/com.taoensso/telemere/"
   :levels log-levels
   :log-fn #'log
   :name "Telemere"
   :remove-appender-fn #'remove-appender
   :website-url "https://github.com/taoensso/telemere"
   })
