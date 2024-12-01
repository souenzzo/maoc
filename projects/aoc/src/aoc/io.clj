(ns aoc.io
  (:require [clojure.string :as string])
  (:import (java.net CookieManager URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)))

(set! *warn-on-reflection* true)

(def *http-client
  (delay
    (let [cookie-handler (CookieManager.)]
      (.put cookie-handler (URI/create "https://adventofcode.com/")
        {"Set-Cookie" [(str "session="
                         (or (System/getenv "ADVENT_OF_COOKIE")
                           (throw (ex-info "Can't find ADVENT_OF_COOKIE env"
                                    {}))))]})
      (-> (HttpClient/newBuilder)
        (.cookieHandler cookie-handler)
        (.build)))))

(def input
  (memoize (fn [year day]
             (let [uri (URI. "https" nil "adventofcode.com" -1
                         (str "/" year "/day/" day "/input")
                         nil nil)]
               (-> @*http-client
                 (HttpClient/.send (.build (HttpRequest/newBuilder uri))
                   (HttpResponse$BodyHandlers/ofLines))
                 .body
                 stream-seq!)))))


(defn answer
  [year day level answer]
  (let [uri (URI. "https" nil "adventofcode.com" -1
              (str "/" year "/day/" day "/answer")
              nil nil)
        http-response (-> @*http-client
                        (HttpClient/.send (-> uri
                                            HttpRequest/newBuilder
                                            (.header "Content-Type" "application/x-www-form-urlencoded")
                                            (.POST (HttpRequest$BodyPublishers/ofString
                                                     (str "level=" level "&answer=" answer)))
                                            .build)
                          (HttpResponse$BodyHandlers/ofLines)))
        texts (stream-seq! (.body http-response))]

    (cond
      (some #(string/includes? % "That's not the right answer.")
        texts)
      :wrong!
      (some #(string/includes? % "You gave an answer too recently;")
        texts)
      :too-fast!
      (some #(string/includes? % "That's the right answer!")
        texts)
      :right!

      (some #(string/includes? % "You don't seem to be solving the right level.")
        texts)
      :already-submitted!

      :else {:headers (into {}
                        (map (fn [[k vs]]
                               [k (if (next vs)
                                    (vec vs)
                                    (first vs))]))
                        (.map (.headers http-response)))
             :body    texts
             :status  (.statusCode http-response)})))
