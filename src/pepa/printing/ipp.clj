(ns pepa.printing.ipp
  (:require [com.stuartsierra.component :as component]

            [ipp.server :as ipp]
            [ipp.protocol :as ipp-protocol]

            [pepa.printing.common :as printing]
            [pepa.log :as log]
            [pepa.zeroconf :as zeroconf]))

(defn ^:private job-handler [ipp]
  (reify
    ipp-protocol/IPrintJobHandler
    (accept-job [_ queue job]
      (printing/accept-job ipp queue job
                           #(or (:document-name %) (:job-name %))))))

(defn ^:private ipp-server [ipp config]
  (ipp/make-server (assoc (select-keys config [:host :port])
                          :queue "documents"
                          :name "Pepa DMS IPP Printer"
                          :handler (job-handler ipp))))

(defrecord IPPPrinter [config db server]
  component/Lifecycle
  (start [ipp]
    (let [ipp-config (get-in config [:printing :ipp])]
      (if-not (:enable ipp-config)
        ipp
        (do
          (assert (< 0 (:port ipp-config) 65535))
          (log/info ipp "Starting IPP Server")
          (let [server (-> (ipp-server ipp ipp-config)
                           (ipp/start-server))]
            (assoc ipp
                   :server server))))))
  (stop [ipp]
    (when-let [server (:server ipp)]
      (log/info ipp "Stopping IPP Server")
      (ipp/stop-server server))
    (assoc ipp
           :mdns nil
           :server nil)))

(defn make-ipp-component []
  (map->IPPPrinter {}))

;;; Zeroconf Service Implementation

(defmethod zeroconf/service-info :ipp [module config]
  (let [name "Pepa DMS IPP Printer"
        port (get-in config [:printing :ipp :port])
        queue "documents"]
    (assert (< 0 port 65535))
    {:type "_ipp._tcp.local."
     :name name
     :port port
     :props {"pdl" "application/pdf,application/postscript,image/urf"
             "URF" "DM3"; according to https://bugs.launchpad.net/ubuntu/+source/cups/+bug/1054495
             "Color" "T"
             "rp" queue
             "txtvers" "1"
             "ty" (str name " (Queue: " queue ")")}}))
