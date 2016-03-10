(ns word-it.core
  (:gen-class)
  (:import (java.io FileNotFoundException)))

(require '[clojure.string :as str]
         '[clojure.data.generators :refer [weighted]]
         '[clojure.edn :as edn]
         '[clojure.core.match :refer [match]]
         '[me.raynes.fs :as fs]
         '[word-it.extensions :as ext])

(def profile-path (fs/expand-home "~/.wordit"))

(defn read-dict
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [lines (line-seq rdr)
          header (first lines)
          [from, to] (str/split header #" -> ")
          dict-lines (rest lines)
          trans (->> dict-lines
                     (map str/trim)
                     (filter not-empty)
                     (filter #(not (.startsWith % "#")))
                     (map #(str/split % #" - "))
                     (map (fn [line]
                            (->> line
                                 (map #(str/split % #","))
                                 (map #(map str/trim %))))))]
      {:from from, :to to, :dict (into [] trans)})))


(defn read-profile
  [filename]
  (edn/read-string (slurp filename)))


(defn write-profile [filename profile]
  (spit filename (prn-str profile)))


(defn rand-word [dict profile]
  (if
    (empty? profile)
    (rand-nth dict)
    (let [max-count (apply max (vals profile))
          weights (into {} (for
                             [[words trs] dict]
                             [[words trs] (* max-count (/ 1 (get profile words 1)))]))]
      (weighted weights))))

(defn ask
  [dicts lang-from lang-to profile]
  (let [[words, translations] (rand-word
                                (dicts lang-from)
                                (get-in profile [#{lang-from lang-to} lang-from] {}))
        transformation (get ext/transformations
                            [lang-from lang-to]
                            (constantly identity))
        transform (apply comp (map transformation words))
        translations (map transform translations)]
    (println (str lang-from, ":\t", (str/join ", " words)))
    (let [answer (do (print (str lang-to, ":\t"))
                     (flush)
                     (-> (read-line)
                         str/trim
                         transform))]
      (cond
        (some #(= answer %) translations) [:correct,
                                           (update-in profile
                                                      [#{lang-from lang-to} lang-from words]
                                                      (fnil inc 0))]
        (= answer "quit") [:quit,
                           profile]
        :else (do (println (str/join ", " translations)) [:wrong,
                                                          profile])))))


(defn -main [filename & args]
  (let [{from :from, to :to, dict :dict} (read-dict filename)
        dicts {from dict
               to   (map (fn [[from-words, to-words]] [to-words, from-words])
                         dict)}
        init-profile (try
                       (read-profile profile-path)
                       (catch FileNotFoundException _ {}))]
    (loop [[from-lang, lang-to] (shuffle [from, to])
           total 0
           wrong 0
           profile init-profile]
      (println)
      (match (ask dicts from-lang lang-to profile)
             [:correct new-profile] (recur (shuffle [from, to]) (inc total) wrong new-profile)
             [:wrong new-profile] (recur (shuffle [from, to]) (inc total) (inc wrong) new-profile)
             [:quit new-profile] (do (write-profile profile-path new-profile) (println "Total:" total "wrong:", wrong))))))