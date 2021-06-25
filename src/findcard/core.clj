(ns findcard.core
  (:gen-class)
  (:require

   [etaoin.api :as e]

   [cheshire.core :as json]

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


(defn parse-price [line]
  (some-> line
          (str/replace #"[^0-9]" "")
          Integer/parseInt))


(defn parse-cu []
  (e/with-chrome {:size [1920 1080]
                  :args ["--headless"
                         "--disable-gpu"
                         "--enable-javascript"
                         "--user-agent=Mozilla: Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101"

                         ]} driver


    (e/go driver "https://www.computeruniverse.net/ru/c/apparatnoe-obespechenie-i-komponenty/videokarty-nvidia?range%5Bprice_ag_floored%5D%5Bmin%5D=254&toggle%5Bdeliverydatenow%5D=true")

    (e/wait 1)

    (dotimes [_ 20]
      (e/scroll-down driver 500)
      (e/wait 0.1))

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
                    ))]

      (for [[title price href] (map vector titles prices hrefs)]

        {:title title
         :min-price (parse-price price)
         :url (str "https://www.computeruniverse.net" href)}))))


;; ais-Hits-item
;; c-productItem__head__name
;; price price--blue-4xl flex
;; price price--grey-alt flex



(defn parse-e-katalog [query]

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
                        parse-price)

                max-price
                (some-> range-node first :content second :content (nth 2) :content first
                        parse-price)]

            {:title title
             :url url
             :min-price min-price
             :max-price max-price}))]

    (->> report
         (filter (every-pred :min-price :max-price))
         (sort-by :min-price))))



(defn parse-abc [query]

  (let [html
        (:body
         (client/get "https://voronezh.abc.ru/search/"
                     {:query-params {"query" query}}))

        _
        (spit "abc.html" html)

        parsed-doc
        (h/parse html)

        doc-tree
        (h/as-hickory parsed-doc)

        nodes
        (s/select (s/class "product-tile") doc-tree)

        _
        (def -abc-nodes nodes)]

    (for [node nodes]

      (let [a
            (-> (s/select (s/class "product-tile__title") node)
                first)

            title
            (-> a :attrs :title)

            url
            (str "https://voronezh.abc.ru"
                 (-> a :attrs :href))

            price
            (-> (s/select (s/class "product__price-current") node)
                   first
                   :content
                   first
                   parse-price)]

        {:title title
         :url url
         :min-price price}))))


(defn render [data]

  (with-out-str

    (doseq [{:keys [title url min-price max-price]} data]

      (println)
      (println title)
      (println url)
      (print min-price)

      (when max-price
        (print " -- " max-price))

      (println))))


(defn present [title rows]
  (println)
  (println title)
  (println "-------------------")
  (println (render rows)))



(defn -main
  [& args]

  (let [fut1 (future (parse-abc "rtx 3060"))
        fut2 (future (parse-abc "rtx 2060 super"))
        fut3 (future (parse-e-katalog "rtx 3060"))
        fut4 (future (parse-e-katalog "rtx 2060 super"))
        fut5 (future (parse-e-katalog "gtx 1660 super"))
        fut6 (future (parse-cu))]

    (present "ABC RTX 3060" @fut1)

    (present "ABC RTX 2060 Super" @fut2)

    (present "E-Katalog RTX 3060" @fut3)

    (present "E-Katalog RTX 2060 Super" @fut4)

    (present "E-Katalog GTX 1660 Super" @fut5)

    (present "Computer Universe" @fut6))

  (System/exit 0))
