(ns kotoba.cam.materials-edn-test
  "JVM-only drift guard: the domain namespaces embed material/default-config
   tables as literals (to stay IO-free and portable, see kotoba.cam.stock /
   kotoba.cam.gcode docstrings). This test is the one place allowed to read
   the canonical resources/*.edn files and asserts they never drift from the
   embedded copies."
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [kotoba.cam.stock :as stock]
            [kotoba.cam.gcode :as gcode]))

(defn- read-edn-resource [path]
  (edn/read-string (slurp (io/resource path))))

(deftest materials-edn-matches-embedded-literal
  (is (= (read-edn-resource "kotoba/cam/materials.edn") stock/material-presets)))

(deftest gcode-defaults-edn-matches-embedded-literal
  (is (= (read-edn-resource "kotoba/cam/gcode-defaults.edn") gcode/default-config)))
