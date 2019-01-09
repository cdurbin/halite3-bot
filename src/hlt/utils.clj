(ns hlt.utils
  "My utility functions."
  (:require
   [clj-time.core :as clj-time]
   [clj-time.format :as time-format]
   [clj-time.local :as local]
   [clojure.java.io :as clj-io]
   [cheshire.core :as json])
  (:gen-class))

(def bot-name "Durbinator-v187")

(def INFINITY 999999)

(defmacro defn-timed
  "Creates a function that logs how long it took to execute the body. It supports multiarity functions
  but only times how long the last listed arity version takes. This means it should be used with
  multiarity functions where it calls itself with the extra arguments."
  [fn-name & fn-tail]
  (let [fn-name-str (name fn-name)
        ns-str (str *ns*)
        ;; Extract the doc string from the function if present
        [doc-string fn-tail] (if (string? (first fn-tail))
                               [(first fn-tail) (next fn-tail)]
                               [nil fn-tail])
        ;; Wrap single arity functions in a list
        fn-tail (if (vector? (first fn-tail))
                  (list fn-tail)
                  fn-tail)
        ;; extract other arities defined in the function which will not be timed.
        other-arities (drop-last fn-tail)
        ;; extract the last arity definitions bindings and body
        [timed-arity-bindings & timed-arity-body] (last fn-tail)]
    `(defn ~fn-name
       ~@(when doc-string [doc-string])
       ~@other-arities
       (~timed-arity-bindings
         (let [start# (System/currentTimeMillis)]
           (try
             ~@timed-arity-body
             (finally
               (let [elapsed# (- (System/currentTimeMillis) start#)]
                 (log (format "%s/%s took %d ms." ~ns-str ~fn-name-str elapsed#))))))))))

(def asc
  "Sort in ascending order"
  compare)

(def desc
  "Sort in descending order"
   #(compare %2 %1))

(defn compare-by
  "Returns a comparator to be used when sorting.
  Example: (sort (compare-by :distance asc) coll)"
  [& key-cmp-pairs]
  (fn [x y]
      (loop [[k cmp & more] key-cmp-pairs]
         (let [result (cmp (k x) (k y))]
              (if (and (zero? result) more)
                  (recur more)
                  result)))))

(def logger
  (clj-io/writer (str bot-name ".log") :append false))

(def log-stuff (atom false))

(defn log
  "Logs the arguments to the log file, as if printed by println.
  Multiple arguments are separated by spaces."
  [& args]
  (when @log-stuff
    (binding [*out* logger]
      (let [time (time-format/unparse
                  (time-format/formatter "yyyy-MM-dd HH:mm:ss.SSS")
                  (clj-time/from-time-zone
                   (local/local-now)
                   (clj-time/time-zone-for-offset 0)))]
        (apply println (conj args time))))))


(def flog-file
  (clj-io/writer (str "flog" bot-name) :append false))

(defn init-flog
  []
  (binding [*out* flog-file]
    (println "[")))

(defn setup-logging
  "Sets up logging if required."
  [args]
  (when (= "-log" (first args))
    (reset! log-stuff true)
    (init-flog)))

(defn flog
  ([world location msg]
   (flog world location msg nil))
  ([world location msg color]
   (when @log-stuff
     (binding [*out* flog-file]
       (let [location (select-keys location [:x :y])
             line (if color
                    (json/generate-string (merge location
                                                 {:t (:turn world)
                                                  :msg msg}))
                                                  ; :color (name color)}))
                    (json/generate-string (merge location
                                                 {:t (:turn world)
                                                  :msg msg})))]
         (println line ","))))))

(defn flog-color
  ([world location msg]
   (flog world location msg nil))
  ([world location msg color]
   (when @log-stuff
     (binding [*out* flog-file]
       (let [location (select-keys location [:x :y])
             line (if color
                    (json/generate-string (merge location
                                                 {:t (:turn world)
                                                  :msg msg
                                                  :color (name color)}))
                    (json/generate-string (merge location
                                                 {:t (:turn world)
                                                  :msg msg})))]
         (println line ","))))))

(defn remove-item
  "Removes the item from a collection."
  [item coll]
  (remove #(= (:id item) (:id %)) coll))

(defn combine-cells
  "Combines two collections of cells (keeping the first one)."
  [new-cells cell-map]
  (reduce (fn [updated-cell-map cell]
            (assoc updated-cell-map (select-keys cell [:x :y]) cell))
          cell-map
          new-cells))
