(ns img-palette.core
  (:require [img-palette.util :as u]
            [img-palette.k-means :as k]
            [clojure.java.io :as io]
            [api.pg.util :as pg]))

(defn space-distance
  "Distance between two points in 3d space."
  [x y]
  {:pre [(every? #(= (count %) 3) [x y])]}
  (Math/sqrt (apply + (map (comp #(Math/pow % 2) -) x y))))

(defn space-average
  "Average of multiple points in a 3d space."
  [& xs]
  {:pre [(every? #(= (count %) 3) xs)]}
  (apply map (fn [& cs] (/ (apply + cs) (count cs))) xs))

(defn rand-centroid
  "Generate a centroid from a bunch of points by taking the avg of 10 random points."
  [xs]
  (->> xs
       shuffle
       (take 10)
       (apply space-average)))

(defn gen-centroids
  "Generate n random centroids."
  [n xs]
  (repeatedly n #(rand-centroid xs)))

(defn kmeans-centroids
  ([data]
   (kmeans-centroids 3 data))
  ([n data]
   (->> data
        (gen-centroids n)
        ((k/k-groups space-distance space-average data))
        (map u/point->rgb))))

(defn img->palette
  "Take the source image and spit out the 3 primary colors."
  ([filename]
   (let [data (u/get-pixel-colors filename)]
     (kmeans-centroids 3 data)))
  ([filename dest-file]
   (let [data (u/get-pixel-colors filename)
         centroids (kmeans-centroids 3 data)]
     (u/spit-image centroids dest-file))))

(comment
  (img->palette "/home/tienson/Pictures/1.png")
  (img->palette "/Users/ghoseb/Desktop/nature.jpg" "/Users/ghoseb/Desktop/palette.png")
  )

;; id -> colors
(defonce palette (atom {}))
(defonce palette-path "/tmp/palette")

(defn save-data [] (spit palette-path (prn-str @palette)))

(defn load-data []
  (reset! palette
          (try (read-string (slurp palette-path))
               (catch Exception e
                 nil))))

(defn- get-palette
  [path file]
  (try
    (let [id (Integer/parseInt (re-find #"\d+" file))
          file (str path "/" file)
          colors (img->palette file)]
      (swap! palette assoc id colors)
      (save-data))
    (catch Exception e
      (prn "Error" file e))))

(defn get-n-mean-colors
  [n]
  (kmeans-centroids n (flatten (vals @palette))))

(defn -main
  [& args]
  (load-data)
  (let [path "/tmp/arts"
        files (->> (for [file (file-seq (io/file path))]
                     (.getName file))
                   (remove #(= "arts" %)))
        files files]
    (pmap (partial get-palette path) files)))

;;

(defn update-db
  []
  (let [db (:db environ-plus.core/env)]
    (doseq [[id colors] @palette]
      (let [source (api.db.art/get-raw db id :cover)
            colors (mapv vec colors)]
        (prn {:id id
              :colors colors
              :result (api.db.picture/update-by-source db {:source source
                                                           :colors colors})})))))
