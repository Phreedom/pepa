(ns pepa.printing-ipp
  (:require [com.stuartsierra.component :as component]
            [clojure.java.io :as io]

            [ipp.server :as ipp]
            [ipp.protocol :as ipp-protocol]

            [pepa.db :as db]
            [pepa.model :as model]
            [pepa.log :as log]
            [pepa.util :as util]

            [pepa.zeroconf :as zeroconf])
  (:import java.lang.ProcessBuilder
           java.io.File))

(defn ^:private pdf? [bytes]
  (= "%PDF" (String. bytes 0 4)))

(defn ^:private ghostscript? [bytes]
  (= "%!" (String. bytes 0 2)))

(defn ^:private ps->pdf [input]
  (let [tmp-file (File/createTempFile "ps2pdf-output" ".pdf")
        process (-> ["ps2pdf" "-" (str tmp-file)]
                    (ProcessBuilder.)
                    (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)
                    (.start))
        process-input (.getOutputStream process)]
    (io/copy (io/input-stream input) process-input)
    (.close process-input)
    (let [exit-code (.waitFor process)]
      (if (zero? exit-code)
        tmp-file
        (throw (ex-info "Failed to run subprocess to extract meta-data."
                        {:exit-code exit-code}))))))

(defn ^:private job-handler [ipp]
  (reify
    ipp-protocol/IPrintJobHandler
    (accept-job [_ queue job]
      (log/info ipp "got job on queue" queue job)
      (db/with-transaction [db (:db ipp)]
        (let [name (or (:document-name job)
                       (:job-name job)
                       "Printed File")
              pdf (let [data (:data job)]
                    ;; Check magic bytes in the job data. 
                    (cond
                      (pdf? data) data
                      (ghostscript? data) (ps->pdf data)
                      true (do
                             (log/warn ipp "Couldn't determine file type of print job. Interpreting it as Postscript")
                             (ps->pdf data))))
              file-props {:content-type "application/pdf"
                          :name name
                          :origin (str "printer/" queue)
                          :data (util/slurp-bytes pdf)}
              file (model/store-file! db file-props)
              origin "printer"]
          ;; TODO: Allow printing to inbox
          (prn (String. (:data job) 0 100))
          (when-not false ; (model/inbox-origin? (:config ipp) origin)
            (log/info ipp "Creating Document for file" (:id file))
            (let [document (model/create-document! db {:title (:name file-props)
                                                       :file (:id file)})
                  tagging-config (get-in ipp [:config :tagging])]
              (log/info ipp "Auto-tagging document" document)
              (model/auto-tag! db document tagging-config
                               {:origin origin
                                :printing/queue (str "printer/" queue)}))))))))

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
                   :server server))
         ))))
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
