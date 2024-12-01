(ns aoc.io
  (:require [clojure.string :as string])
  (:import (java.net CookieHandler URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublisher HttpRequest$BodyPublishers HttpResponse$BodyHandlers)))

(set! *warn-on-reflection* true)

(def *http-client
  (delay
    (-> (HttpClient/newBuilder)
      (.cookieHandler (proxy [CookieHandler] []
                        (put [uri response-headers])
                        (get [uri request-headers]
                          (when (some-> uri str (string/starts-with? "https://adventofcode.com/"))
                            {"Cookie" [(str "session=" (System/getenv "ADVENT_OF_COOKIE"))]}))))
      (.build))))

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
                          (HttpResponse$BodyHandlers/ofLines)))]
    {:headers (into {}
                (map (fn [[k vs]]
                       [k (if (next vs)
                            (vec vs)
                            (first vs))]))
                (.map (.headers http-response)))
     :body    (.body http-response)
     :status  (.statusCode http-response)}))
