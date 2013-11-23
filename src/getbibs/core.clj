(ns getbibs.core
  (import (java.net URL)
          (java.io ByteArrayInputStream))
  (:use [clj-xpath.core :only [$x $x:attrs $x:attrs* $x:text*]]))

(defmulti ^:private ->XML class)

(defmethod ->XML java.io.InputStream [str]
  (let [stf (javax.xml.transform.TransformerFactory/newInstance)
        dr  (javax.xml.transform.dom.DOMResult.)
        th  (doto (. stf newTransformerHandler)
              (.setResult dr))
        parser (doto (org.ccil.cowan.tagsoup.Parser.)
                 (.setFeature org.ccil.cowan.tagsoup.Parser/namespacesFeature false)
                 (.setContentHandler th))]
    (. parser (parse (org.xml.sax.InputSource. str)))
    (. dr getNode)))

(defmethod ->XML java.lang.String [str]
  (->XML (ByteArrayInputStream. (.getBytes str "UTF-8"))))

(defn- get-html [s]
  (let [url (URL. s)
        in   (.openStream url)]
    (slurp in)))

(defn- extract-pre [string]
  (re-seq #"<pre>.*" string))

(defn- parse-bibs [string]
  (->>
   (re-seq #"<a href=\"(http://dblp.uni-trier.de/rec/bibtex/[^\"]+)\">" string)
   (map second)
   (filter #(not (.endsWith %1 ".xml")))
   (map get-html)
   (map ->XML)
   (map #($x:text* "//pre" %1))
   flatten))

(defn get-bibs [source]
  "Return a lazy list of bibtex entries from the source.

Source has to be a URL string like 'http://www.dblp.org/pers/hc/d/Dijkstra:Edsger_W=.html'"
  (parse-bibs (get-html source)))

(defn print-bibs
  "Print all or [n] bibtex entries from the source.

Source has to be a URL string like 'http://www.dblp.org/pers/hc/d/Dijkstra:Edsger_W=.html'"
  ([source] (dorun (map println (get-bibs source))))
  ([source n] (dorun (map println (take n (get-bibs source))))))
