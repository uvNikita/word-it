(ns word-it.core
  (:gen-class))

(require '[clojure.string :as str])

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


(defn ask
  [dicts lang-from lang-to]
  (let [[words, translations] (rand-nth (dicts lang-from))]
    (println (str lang-from, ":\t", (str/join ", " words)))
    (let [answer (do (print (str lang-to, ":\t"))
                     (flush)
                     (str/trim (read-line)))]
      (cond
        (some #(= answer %) translations) :correct
        (= answer "quit") :quit
        :else (do (println (str/join ", " translations)) :wrong)))))


(defn -main [filename & args]
  (let [{from :from, to :to, norsk-eng :dict} (read-dict filename)
        dicts {from norsk-eng
               to   (map (fn [[norsk, eng]] [eng, norsk]) norsk-eng)}
        total (atom 0)
        wrong (atom 0)]
    (loop [[from-lang, lang-to] (shuffle [from, to])]
      (println)
      (case (ask dicts from-lang lang-to)
        :correct (do
                   (swap! total inc)
                   (recur (shuffle [from, to])))
        :wrong (do
                 (swap! total inc)
                 (swap! wrong inc)
                 (recur (shuffle [from, to])))
        :quit (println "Total:" @total "wrong:", @wrong)))))