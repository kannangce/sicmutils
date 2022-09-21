(ns sicmutils.solve.nsolve
  (:require [sicmutils.util.scheme :refer [car cadr caddr cadddr]]))


(defn make-solution
  [resid-eqs resid-vars substs tough]
  '(resid-eqs resid-vars substs tough))

(defn residual-equations [solution]
  (car solution))
(defn residual-variables [solution]
  (cadr solution))
(defn substitutions [solution]
  (caddr solution))
(defn tough-equations [solution]
  (cadddr solution))

(def *outstanding-contradictions*)
(def *solver-state*)

;TODO
(declare equation-expression)
(declare apply-substitutions-to-equation)
(declare solve-incremental)
;; TODO ends

;; Assumptions
;; Assuming ~0? as zero?
;; Assuming fluid-let as binding
;; Assumption ends

(defn correct-substitutions? [equations substitutions]
  (every? #(let [expr
                 (equation-expression
                   (apply-substitutions-to-equation %
                                                    substitutions))]
             (and (number? expr) (zero? expr)))
          equations))

(defn contradiction-failure [contradictions fail]
  (binding [*outstanding-contradictions* contradictions]
    (fail)))


(defn contradictory-equation? [eqn]
        (let [expr (equation-expression eqn)]
          (and (number? expr) (not (zero? expr)))))