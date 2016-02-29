(ns word-it.extensions
  (:require [clojure.string :as str]))


(def transformations
  {["norsk" "eng"] (fn [norsk]
                     (cond
                       (.startsWith norsk "å ") #(str/replace-first % "to " "")
                       (re-find #"en |et |ei " norsk) #(str/replace-first % #"a |the " "")
                       :else identity)
                     )
   ["eng" "norsk"] (fn [eng]
                     (cond
                       (.startsWith eng "to ") #(str/replace-first % "å " "")
                       :else identity)
                     )})
