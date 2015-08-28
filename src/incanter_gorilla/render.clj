(ns incanter-gorilla.render
  (:require [clojure.data.codec.base64 :as b64]
            [incanter.core :as incore]
            [gorilla-renderable.core :as render]))

(defn chart->byte-array
  [chart & {:keys [plot-size aspect-ratio]
            :or   {plot-size 500
                   aspect-ratio 1.618}}]
  (let [width (/ plot-size aspect-ratio)
        ba (java.io.ByteArrayOutputStream.)
        _ (org.jfree.chart.ChartUtilities/writeChartAsPNG ba chart plot-size width)]
    (.toByteArray ba)))


(defrecord ChartView [content opts])

(defn chart-view [content & opts] (ChartView. content opts))

(extend-type ChartView
  render/Renderable
  (render [self]
    (let [bytes (apply chart->byte-array (:content self) (:opts self))]
      {:type    :html
       :content (format "<img src=\"data:image/PNG;base64,%1$s\"/>" (String. (b64/encode bytes)))
       :value   (pr-str self)})))

(defn- list-like
  "util function used in render"
  [data value open close separator]
  {:type :list-like
   :open open
   :close close
   :separator separator
   :items data
   :value value})


 ;extend dataset type to show in gorilla-repl interface
(extend-type incanter.core.Dataset
    render/Renderable
    (render [self]
            (let [rendfn (fn [open close sep r] (list-like (map render/render r) (pr-str r) open close sep))
                  rows (map (partial rendfn "<tr><td>" "</td></tr>" "</td><td>") (incore/to-list self))
                  heading (if-let [cols (:column-names self)]
                            [(rendfn "<tr><th>" "</th></tr>" "</th><th>" cols)]
                            [])
                  body (list-like (concat heading rows) (pr-str self) "<center><table>" "</table></center>" "\n")]
              body)))
