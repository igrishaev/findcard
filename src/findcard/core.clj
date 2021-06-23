(ns findcard.core
  (:gen-class)
  (:require
   [clj-http.client :as client]
   [hickory.core :as h]
   [hickory.select :as s]
   [clojure.string :as str]))



#_
(def document
  (:body
   (client/get "https://www.e-katalog.ru/ek-list.php"
               {:query-params {"search_" "rtx 3060"}})))

#_
(def parsed-doc
  (h/parse document))

#_
(def doc-tree
  (h/as-hickory parsed-doc))

#_
(def nodes
  (s/select (s/class "model-short-block") doc-tree))

#_
(def vcard
  (s/select (s/class "model-short-title") (first nodes)))

#_
(def title
  (some-> vcard first :attrs :title))

#_
(def range-node
  (s/select (s/class "model-price-range") (first nodes)))


(defn show-prices [query]

  (let [document
        (:body
         (client/get "https://www.e-katalog.ru/ek-list.php"
                     {:query-params {"search_" query}}))

        parsed-doc
        (h/parse document)

        doc-tree
        (h/as-hickory parsed-doc)

        nodes
        (s/select (s/class "model-short-block") doc-tree)

        report
        (for [node nodes]

          (let [vcard
                (s/select (s/class "model-short-title") node)

                title
                (some-> vcard first :attrs :title)

                link
                (s/select (s/attr "data-url") node)

                url
                (str "https://www.e-katalog.ru"
                     (some-> link first :attrs :data-url))

                range-node
                (s/select (s/class "model-price-range") node)

                min-price
                (some-> range-node first :content second :content (nth 0) :content first
                        (str/replace #"[^0-9]" "")
                        Integer/parseInt)

                max-price
                (some-> range-node first :content second :content (nth 2) :content first
                        (str/replace #"[^0-9]" "")
                        Integer/parseInt)]

            {:title title
             :url url
             :min-price min-price
             :max-price max-price}))]

    (->> report
         (filter (every-pred :min-price :max-price))
         (sort-by :min-price))))


(defn render [data]

  (with-out-str

    (doseq [{:keys [title url min-price max-price]} data]

      (println)
      (println title)
      (println url)
      (println min-price " -- " max-price))))


(defn to-file [data]

  (let [html (render data)
        file (java.io.File/createTempFile "vcards" ".html")
        path (str file)]

    (spit path html)
    path))


(defn -main
  [& args]

  (let [queries ["rtx 3060" "rtx 2060" "gtx 1660 super"]]

    (doseq [query queries]

      (println)
      (println query)
      (println "-------------------")
      (println (render (show-prices query))))))
