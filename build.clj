(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))

(defn ci
      [opts]
      (-> opts
          bb/run-tests))
