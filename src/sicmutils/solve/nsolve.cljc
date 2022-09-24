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
;;; There may be two roots to a quadratic!
(def *root-premises* '())

;TODO
(declare equation-expression)
(declare apply-substitutions-to-equation)
(declare solve-incremental)
(declare flush-tautologies)
(declare next-substitutions)
(declare next-equations)
(declare contradictory-equation?)
(declare backsubstitute-equation)
(declare backsubstitute-substitution)
(declare isolatable?)
(declare make-substitution)
(declare equation-justifications)
(declare just-union)
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

(defn use-new-substitution [newsubst oldresids oldsubsts oldtough
                            succeed fail]
  (let [new-substitutions
        (cons newsubst (next-substitutions newsubst oldsubsts))
        new-equations
        (flush-tautologies (next-equations newsubst oldresids))
        new-tough
        (flush-tautologies (next-equations newsubst oldtough))]
    (let [contradictions
          (concat (filter contradictory-equation? new-equations)
                  (filter contradictory-equation? new-tough))]
      (cond (nil? contradictions)
            (succeed new-equations new-substitutions new-tough fail)
            :else
            (contradiction-failure contradictions fail)))))


(defn contradictory-equation? [eqn]
  (let [expr (equation-expression eqn)]
    (and (number? expr) (not (zero? expr)))))

(defn flush-tautologies [equations]
  (filter #(let [expr (equation-expression %)]
             (not (and (number? expr) (zero? expr))))
          equations))

(defn next-equations [substitution equations]
  (map #(backsubstitute-equation substitution %)
       equations))

(defn next-substitutions [new-substitution substitutions]
  (map #(backsubstitute-substitution new-substitution %)
       substitutions))

(defn isolate-var [var eqn succeed fail]
  ;; succeed = (lambda (new-substitution) ...)
  ;; fail    = (lambda () ...)
  (isolatable? var (equation-expression eqn)
               (fn [value fail]
                 ;;(pp `(isolate ,*root-premises* ,eqn ,value))
                 (succeed
                   (make-substitution var value
                                      (just-union *root-premises*
                                                  (equation-justifications eqn)))
                   fail))
               fail))

