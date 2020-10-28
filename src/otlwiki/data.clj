(ns otlwiki.data
  (:require [clojure.string :as str]
            [otlwiki.outline :as otl]))

; Operations on outline-embedded data.
; This module is somewhat more opinionated that outline.clj.

(defn metadata-attribute
  "Return [:key val] if outline is metadata attribute, nil otherwise."
  [otl]
  (let
   [[key val] (otl/snip otl)
    key (when key (second (re-matches #"([a-z][a-z0-9\-]*):" key)))
    val (cond  ; Either inline value or indented value, not both
          (and (:head val) (not (seq (:body val))))
          (:head val),
          (and (not (:head val)) (seq (:body val)))
          (str/split-lines (with-out-str (otl/print-body val))),
          :else nil)]
    (when (and key val) [(keyword key) val])))

(defn metadata
  "Extract metadata from outline.

  Returns nil if there was no metadata."
  [otl]
  (when (:head otl)
    (->> (:body otl)
         (map metadata-attribute)
         (take-while some?)
         (into {}))))

(defn assoc-metadata
  "Return outline with old metadata replaced with new."
  [otl data]
  (let
   [otl (otl/outline (:head otl) (drop-while metadata-attribute (:body otl)))
    lines (map (fn [[k v]] (otl/attach (str (name k) ":") v)) data)]
    (otl/outline (:head otl) (concat lines (:body otl)))))
