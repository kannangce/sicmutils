(ns sicmutils.util.scheme
  (:require [sicmutils.util.def :refer [apply-fn-times]]))

(defn car [x]
  (first x))

(defn cdr [x]
  (rest x))

(defn cddr [x]
  (apply-fn-times cdr x 2))

(defn cdddr [x]
  (apply-fn-times cdr x 3))

(defn cadr [x]
  (car (cdr x)))

(defn caddr [x]
  (car (cddr x)))

(defn cadddr [x]
  (car (cadddr x)))