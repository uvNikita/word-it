(ns word-it.core
  (:gen-class))

(require '[clojure.string :as str]
         '[word-it.extensions :as ext])

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
  (let [[words, translations] (rand-nth (dicts lang-from))
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
        (some #(= answer %) translations) :correct
        (= answer "quit") :quit
        :else (do (println (str/join ", " translations)) :wrong)))))


(defn -main [filename & args]
  (let [{from :from, to :to, dict :dict} (read-dict filename)
        dicts {from dict
               to   (map (fn [[from-words, to-words]] [to-words, from-words])
                         dict)}]
    (loop [[from-lang, lang-to] (shuffle [from, to])
           total 0
           wrong 0]
      (println)
      (case (ask dicts from-lang lang-to)
        :correct (recur (shuffle [from, to]) (inc total) wrong)
        :wrong (recur (shuffle [from, to]) (inc total) (inc wrong))
        :quit (println "Total:" total "wrong:", wrong)))))