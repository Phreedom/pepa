(ns pepa.printing.lpd
  (:require [com.stuartsierra.component :as component]

            [lpd.server :as lpd]
            [lpd.protocol :as lpd-protocol]

            [pepa.printing.common :as printing]
            [pepa.log :as log]
            [pepa.zeroconf :as zeroconf]))

(defn ^:private job-handler [lpd]
  (reify
    lpd-protocol/IPrintJobHandler
    (accept-job [_ queue job]
      (printing/accept-job lpd queue job
                           #(or (:source-filename %) (:banner-name %))))))

(defn ^:private lpd-server [lpd config]
  (lpd/make-server (assoc (select-keys config [:host :port])
                          :handler (job-handler lpd))))

(defrecord LPDPrinter [config db server]
  component/Lifecycle
  (start [lpd]
    (let [lpd-config (get-in config [:printing :lpd])]
      (if-not (:enable lpd-config)
        lpd
        (do
          (assert (< 0 (:port lpd-config) 65535))
          (log/info lpd "Starting LPD Server")
          (let [server (-> (lpd-server lpd lpd-config)
                           (lpd/start-server))]
            (assoc lpd
                   :server server))))))
  (stop [lpd]
    (when-let [server (:server lpd)]
      (log/info lpd "Stopping LPD Server")
      (lpd/stop-server server))
    (assoc lpd
           :mdns nil
           :server nil)))

(defn make-lpd-component []
  (map->LPDPrinter {}))

;;; Zeroconf Service Implementation

(defmethod zeroconf/service-info :lpd [module config]
  (let [name "Pepa DMS Printer"
        port (get-in config [:printing :lpd :port])
        queue "documents"]
    (assert (< 0 port 65535))
    {:type "_printer._tcp.local."
     :name name
     :port port
     :props {"pdl" "application/pdf,application/postscript"
             "rp" queue
             "txtvers" "1"
             "qtotal" "1" ;count of queues
             "ty" (str name " (Queue: " queue ")")}}))
