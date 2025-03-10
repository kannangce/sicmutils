#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.numerical.quadrature.romberg
  (:require [sicmutils.numerical.quadrature.common :as qc :include-macros true]
            [sicmutils.numerical.quadrature.midpoint :as qm]
            [sicmutils.numerical.quadrature.trapezoid :as qt]
            [sicmutils.polynomial.richardson :as pr]))

;; ## Romberg's Method
;;
;; Romberg's method is a technique for estimating a definite integral over a
;; closed (or open) range $a, b$:
;;
;; $$\int_{a}^{b} f(x) dx$$
;;
;; By applying Richardson extrapolation (see `richardson.cljc`) to either the
;; Trapezoid method or the Midpoint method.
;;
;; The implementation of Richardson extrapolation in this library can be applied
;; to any methods; many of the numerical quadrature methods (Simpson, Simpson's
;; 3/8, Milne, Boole) involve a single step of Richardson extrapolation.
;;
;; Romberg integration goes all the way. A nice way to think about this
;; algorithm is this:
;;
;; - Generate a sequence of estimates of the definite integral using the
;;   Trapezoid or Midpoint methods on a geometrically increasing number of
;;   integration slices of width $h$. This gives you a sequence of $N$ points of
;;   the form $(h, A(h))$, where $A$ is the integral estimate.
;;
;; - Each time a new point becomes available, fit a polynomial of order $N-1$ to
;;   all $N$ points... and then extrapolate to $A(0)$, the magical area estimate
;;   where the width of each integration slice is 0.
;;
;; For a wonderful reference that builds up to the ideas of Richardson
;; extrapolation and Romberg integration, see Sussman's ["Abstraction in
;; Numerical
;; Methods"](https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2)
;;
;; References:
;;
;; - Press's Numerical Recipes (p134), Section 4.3 http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-3.pdf
;; - Numerical Recipes 4.4 for open-interval Romberg http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf
;; - Halfant & Sussman, ["Abstraction in Numerical
;;   Methods"](https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2).
;; - Wikipedia: https://en.wikipedia.org/wiki/Romberg%27s_method

(defn open-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the open interval $(a, b)$ by applying Richardson extrapolation to
  successive integral estimates from the Midpoint rule.

  Returns estimates formed by combining $n, 3n, 9n, ...$ slices, geometrically
  increasing by a factor of 3 with each estimate. This factor of 3 is because,
  internally, the Midpoint method is able to recycle old function evaluations
  through this factor of 3.

  Romberg integration converges quite fast by cancelling one error term in the
  taylor series expansion of $f$ with each examined term. If your function is
  /not/ smooth this may cause you trouble, and you may want to investigate a
  lower-order method.

  ### Optional arguments:

  If supplied, `:n` (default 1) specifies the initial number of slices to use."
  ([f a b] (open-sequence f a b {}))
  ([f a b {:keys [n] :or {n 1} :as opts}]
   {:pre [(number? n)]}
   (-> (qm/midpoint-sequence f a b opts)
       (pr/richardson-sequence 3 2 2))))

(defn closed-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the closed interval $[a, b]$ by applying Richardson extrapolation to
  successive integral estimates from the Trapezoid rule.

  Returns estimates formed by combining $n, 2n, 4n, ...$ slices, geometrically
  increasing by a factor of 2 with each estimate.

  Romberg integration converges quite fast by cancelling one error term in the
  taylor series expansion of $f$ with each examined term. If your function is
  /not/ smooth this may cause you trouble, and you may want to investigate a
  lower-order method.

  ### Optional arguments:

  If supplied, `:n` (default 1) specifies the initial number of slices to use."
  ([f a b] (closed-sequence f a b {}))
  ([f a b {:keys [n] :or {n 1} :as opts}]
   {:pre [(number? n)]}
   (-> (qt/trapezoid-sequence f a b opts)
       (pr/richardson-sequence 2 2 2))))

(defn romberg-sequence
  "Higher-level abstraction over `closed-sequence` and `open-sequence`. Identical
  to those functions (see their docstrings), but internally chooses either
  implementation based on the interval specified inside of `opts`.

  Defaults to the same behavior as `open-sequence`."
  ([f a b] (romberg-sequence f a b {}))
  ([f a b opts]
   (let [seq-fn (if (qc/closed?
                     (qc/interval opts))
                  closed-sequence
                  open-sequence)]
     (seq-fn f a b opts))))

(qc/defintegrator open-integral
  "Returns an estimate of the integral of `f` over the open interval $(a, b)$
  generated by applying Richardson extrapolation to successive integral
  estimates from the Midpoint rule.

  Considers $1, 3, 9 ... 3^n$ windows into $(a, b)$ for each successive
  estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See [[open-sequence]] for more information about Romberg integration, caveats
  that might apply when using this integration method and information on the
  optional args in `opts` that customize this function's behavior."
  :area-fn qm/single-midpoint
  :seq-fn open-sequence)

(qc/defintegrator closed-integral
  "Returns an estimate of the integral of `f` over the closed interval $[a, b]$
  generated by applying Richardson extrapolation to successive integral
  estimates from the Trapezoid rule.

  Considers $1, 2, 4 ... 2^n$ windows into $[a, b]$ for each successive
  estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See [[closed-sequence]] for more information about Romberg integration, caveats
  that might apply when using this integration method and information on the
  optional args in `opts` that customize this function's behavior."
  :area-fn qt/single-trapezoid
  :seq-fn closed-sequence)
