(ns tlexanalysis.core
  [:require [clojure.java.io :as io]]
  [:require [clj-http.client :as client]]
  [:use [clojure.string :only (split)]]
  [:use [incanter.core :only (log $=)]])

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(def syn-dict
  (with-open [r (io/reader "tlex.data")]
    (into {} (map
              #(let [[w & s] (split % #" ")]
                 [w s])
              (line-seq r)))))



(defn google-ocurrences [& words]
  (let [
        quoted-words (map #(str "%22" % "%22" ) words)
        uri (apply str "http://www.google.com/search?q=" (interpose "+" quoted-words))
        {body :body} (client/get uri)
        str-ocurrences (second (re-find #"About ([\d,]+) results" body))]
    (Long. (apply str (split str-ocurrences #",")))))

(def google-N (google-ocurrences "a"))

(defn contexts [size words]
  (let [padded-seq (lazy-cat (repeat size nil) words (repeat size nil))
        context (fn [group] [(nth group size)
                             (into #{}
                                   (filter #(not (nil? %))
                                           (concat
                                            (take size group)
                                            (drop (inc size) group))))])]
    (map context (partition (inc (* size 2)) 1 padded-seq))))

(defn st [[word context]]
  (let [cf-w (google-ocurrences word)
        cf-wc (apply google-ocurrences word context)
        st-n ($= (log google-N / cf-w) * cf-wc)]
    {:cf-w cf-w
     :cf-wc cf-wc
     :st-n st-n}))

(defn st-seq [ctx-seq]
  (map (fn [ctx] [ctx (st ctx)]) ctx-seq))
    