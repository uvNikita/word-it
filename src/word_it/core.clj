(ns word-it.core
  (:gen-class))

(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn read-words
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [trans (->> (line-seq rdr)
                     (map str/trim)
                     (filter not-empty)
                     (map #(str/split % #" - ")))]
      (into [] trans)
      )))


(defn ask
  [dicts lang]
  (let [[word, translation] (rand-nth (dicts lang))
        [word-lang-name, trans-lang-name] (case lang
                                            :norsk ["norsk", "eng"]
                                            :eng ["eng", "norsk"])]
    (println (str word-lang-name, ":\t", word))
    (let [answer (do (print (str trans-lang-name, ":\t"))
                     (flush)
                     (str/trim (read-line)))]
      (cond
        (= answer translation) :correct
        (= answer "quit") :quit
        :else (do (println translation) :wrong)))))


(defn -main [filename & args]
  (let [ norsk-eng (read-words filename)
        dicts {:norsk norsk-eng
               :eng (map (fn [[norsk, eng]] [eng, norsk]) norsk-eng)}
        total (atom 0)
        wrong (atom 0)]
    (loop [lang (rand-nth [:norsk :eng])]
      (println)
      (case (ask dicts lang)
        :correct (do
                   (swap! total inc)
                   (recur (rand-nth [:norsk :eng])))
        :wrong (do
                 (swap! total inc)
                 (swap! wrong inc)
                 (recur (rand-nth [:norsk :eng])))
        :quit (println "Total:" @total "wrong:", @wrong)))))