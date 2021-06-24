(ns findcard.core
  (:gen-class)
  (:require

   [etaoin.api :as e]

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



(defn foo []
  (e/with-chrome {
                  :args ["--headless"
                         "--disable-gpu"
                         "--window-size=1920x1080"
                         "--enable-javascript"
                         "--user-agent=Mozilla: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101"

                         ]} driver


    (e/go driver "https://www.computeruniverse.net/ru/c/apparatnoe-obespechenie-i-komponenty/videokarty-nvidia?range%5Bprice_ag_floored%5D%5Bmin%5D=254&toggle%5Bdeliverydatenow%5D=true")

    (e/wait 3)

    (e/scroll-down driver 500)
    (e/wait 0.3)

    (e/scroll-down driver 500)
    (e/wait 0.3)

    (e/scroll-down driver 500)
    (e/wait 0.3)

    (let [html (e/get-source driver)

          _
          (spit "cu.html" html)

          parsed-doc
          (h/parse html)

          doc-tree
          (h/as-hickory parsed-doc)

          nodes
          (s/select (s/class "ais-Hits-item") doc-tree)

          _
          (def -nodes nodes)

          titles
          (for [node nodes]
            (some-> (s/select (s/class "c-productItem__head__name") node)
                    first
                    :content
                    first))

          prices
          (for [node nodes]
            (some-> (s/select (s/class "price--grey-alt") node)
                    first
                    :content
                    first
                    :content
                    first))

          hrefs
          (for [node nodes]
            (some-> (s/select (s/class "c-productItem__head__name") node)
                    first
                    :attrs
                    :href
                    ))
          ]


      [titles prices hrefs]
      )
    )
  )

;; ais-Hits-item
;; c-productItem__head__name
;; price price--blue-4xl flex
;; price price--grey-alt flex



(defn get-prices [query]

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


(defn -main
  [& args]

  (let [queries ["rtx 3060" "rtx 2060" "gtx 1660 super"]

        renders
        (doall
         (pmap (fn [query]
                 (render (get-prices query)))
               queries))]

    (doseq [[query render] (map vector queries renders)]

      (println)
      (println query)
      (println "-------------------")
      (println render)))

  (System/exit 0))
