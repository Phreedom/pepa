(ns pepa.printing.common
  (:require [clojure.java.io :as io]

            [pepa.db :as db]
            [pepa.model :as model]
            [pepa.log :as log]
            [pepa.util :as util])

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

(defn ^:private get-pdf [job warn]
  (let [data (:data job)]
    ;; Check the magic bytes in the job data. 
    (cond
      (pdf? data) data
      (ghostscript? data) (ps->pdf data)
      :else (do
              (when warn
                (warn "Couldn't determine file type of print job. Interpreting it as Postscript"))
              (ps->pdf data)))))

(defn accept-job [component queue job get-job-name]
  (log/info component "got job on queue" queue job)
  (db/with-transaction [db (:db component)]
    (let [name (or (get-job-name job)
                   "Printed File")
          pdf (get-pdf job #(log/warn component %))
          file-props {:content-type "application/pdf"
                      :name name
                      :origin (str "printer/" queue)
                      :data (util/slurp-bytes pdf)}
          file (model/store-file! db file-props)
          origin "printer"]
      ;; TODO: Allow printing to inbox
      (when-not false ; (model/inbox-origin? (:config component) origin)
        (log/info component "Creating Document for file" (:id file))
        (let [document (model/create-document! db {:title (:name file-props)
                                                   :file  (:id file)})
              tagging-config (get-in component [:config :tagging])]
          (log/info component "Auto-tagging document" document)
          (model/auto-tag! db document tagging-config
                           {:origin origin
                            :printing/queue (str "printer/" queue)}))))))
