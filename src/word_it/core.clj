(ns word-it.core
  (:gen-class))

(require '[clojure.string :as str])

(defn read-dict
  [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [trans (->> (line-seq rdr)
                     (map str/trim)
                     (filter not-empty)
                     (filter #(not (.startsWith % "#")))
                     (map #(str/split % #" - "))
                     (map (fn [line]
                            (->> line
                                 (map #(str/split % #","))
                                 (map #(map str/trim %))))))]
      (into [] trans))))


(defn ask
  [dicts lang]
  (let [[words, translations] (rand-nth (dicts lang))
        [word-lang-name, trans-lang-name] (case lang
                                            :norsk ["norsk", "eng"]
                                            :eng ["eng", "norsk"])]
    (println (str word-lang-name, ":\t", (str/join ", " words)))
    (let [answer (do (print (str trans-lang-name, ":\t"))
                     (flush)
                     (str/trim (read-line)))]
      (cond
        (some #(= answer %) translations) :correct
        (= answer "quit") :quit
        :else (do (println (str/join ", " translations)) :wrong)))))


(defn -main [filename & args]
  (let [norsk-eng (read-dict filename)
        dicts {:norsk norsk-eng
               :eng   (map (fn [[norsk, eng]] [eng, norsk]) norsk-eng)}
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