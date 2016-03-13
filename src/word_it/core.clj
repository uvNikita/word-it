(ns word-it.core
  (:gen-class)
  (:import (java.io FileNotFoundException)))

(require '[clojure.string :as str]
         '[clojure.data.generators :refer [weighted]]
         '[clojure.edn :as edn]
         '[clojure.core.match :refer [match]]
         '[clojure.core.async :as async :refer [<!, >!, <!!]]
         '[me.raynes.fs :as fs]
         '[word-it.extensions :as ext])

(def profile-path (fs/expand-home "~/.word-it"))

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
                     (map str/lower-case)
                     (filter #(not (.startsWith % "#")))
                     (map #(str/split % #" - "))
                     (map (fn [line]
                            (->> line
                                 (map #(str/split % #","))
                                 (map #(map str/trim %))
                                 (into [])))))]
      {:from from, :to to, :dict (into {} trans)})))


(defn read-profile
  [filename]
  (edn/read-string (slurp filename)))


(defn write-profile [filename profile]
  (spit filename (prn-str profile)))


(defn counter [ans-hist]
  (loop
    [total 0 wrong 0]
    (let [answer (<!! ans-hist)]
      (if (nil? answer)
        {:total total, :wrong wrong}
        (if (= :wrong (:result answer))
          (recur (inc total) (inc wrong))
          (recur (inc total) wrong))))))


(defn rand-word [dict profile]
  (if
    (empty? profile)
    (rand-nth (keys dict))
    (let [max-count (apply max (vals profile))
          weights (into {} (for
                             [words (keys dict)]
                             [words (* max-count (/ 1 (get profile words 1)))]))]
      (weighted weights))))


(defn word-picker [dicts profile]
  (let [words (async/chan 1)]
    (do
      (async/go
        (while true
          (let [[lang-from, lang-to] (shuffle (keys dicts))
                curr-profile (get-in @profile [#{(keys dicts)} lang-from] {})
                dict (dicts lang-from)]
            (>! words {:lang-from lang-from
                       :lang-to   lang-to
                       :word      (rand-word dict curr-profile)}))))
      words)))


(defn update-profile [profile {lang :lang, word :word}]
  (update-in profile [lang word] (fnil inc 0)))


(defn profile-updater [profile correct-words]
  (async/go-loop []
    (let [word (<! correct-words)]
      (when (some? word)
        (do
          (swap! profile update-profile word)
          (recur))))))


(defn ask
  [dicts words ans-hist]
  (async/go-loop []
    (let [word (<! words)]
      (when (some? word)
        (let [{word :word lang-from :lang-from lang-to :lang-to} word
              translation (get-in dicts [lang-from word])
              transformation (get ext/transformations
                                  [lang-from lang-to]
                                  (constantly identity))
              transform (apply comp (map transformation word))
              translation (map transform translation)]
          (println)
          (println (str lang-from, ":\t", (str/join ", " word)))
          (let [answer (do (print (str lang-to, ":\t"))
                           (flush)
                           (-> (read-line)
                               str/trim
                               transform))]
            (cond
              (some #(= answer %) translation) (do (>! ans-hist {:result :correct
                                                                 :lang   lang-from
                                                                 :word   word})
                                                   (recur))
              (= answer "quit") (async/close! ans-hist)
              :else (do (println (str/join ", " translation))
                        (>! ans-hist {:result :wrong
                                      :lang   lang-from
                                      :word   word})
                        (recur)))))))))


(defn -main [filename & args]
  (let [{from :from, to :to, dict :dict} (read-dict filename)
        dicts {from dict
               to   (into {} (map (fn [[from-words, to-words]] [to-words, from-words])
                                  dict))}
        full-profile (try
                       (read-profile profile-path)
                       (catch FileNotFoundException _ {}))
        lang-profile (full-profile #{from to})

        ans-hist-buffer 5
        ans-hist (async/chan ans-hist-buffer)
        ans-hist-mult (async/mult ans-hist)
        ans-hist-counter (async/tap ans-hist-mult (async/chan ans-hist-buffer))
        ans-hist-correct (async/tap
                           ans-hist-mult
                           (async/chan 5 (comp (filter #(= :correct (:result %)))
                                               (map #(select-keys % [:word :lang])))))

        profile (atom lang-profile)
        words (word-picker dicts profile)]
    (do (profile-updater profile ans-hist-correct)
        (ask dicts words ans-hist)
        (let [{total :total, wrong :wrong} (counter ans-hist-counter)]
          (do
            (write-profile profile-path (assoc full-profile #{from to} @profile))
            (println "Total:" total "wrong:", wrong)))
        )))

