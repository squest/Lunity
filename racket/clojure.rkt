#lang racket

(require plot)

(define-syntax-rule (defn fname fbinding fbody)
  (define fname (λ fbinding fbody)))

(define-syntax-rule (def vname vbody)
  (define vname vbody))

(define-syntax (graf stx)
  (syntax-case stx ()
    [(graf fun)
     #'(plot (function fun)
             #:x-min -10 #:x-max 10
             #:y-min -10 #:y-max 10)]
    [(graf fun xpair)
     #'(plot (function fun)
             #:x-min (first xpair)
             #:x-max (second xpair)
             #:y-min -10 #:y-max 10)]
    [(graf fun xpair ypair)
     #'(plot (function fun)
             #:x-min (first xpair)
             #:x-max (second xpair)
             #:y-min (first ypair)
             #:y-max (second ypair))]))

(define-syntax (grafs stx)
  (syntax-case stx () 
    [(graf lst)
     #'(plot (list (axes) 
                   (function lst))
             #:x-min -10 #:x-max 10
             #:y-min -10 #:y-max 10)]
    [(graf l1 l2)
     #'(plot (list (axes)
                   (function l1) 
                   (function l2))
             #:x-min -10 #:x-max 10
             #:y-min -10 #:y-max 10)]
    [(graf l1 l2 l3)
     #'(plot (list (axes)
                   (function l1)
                   (function l2)
                   (function l3))
             #:x-min -10 #:x-max 10
             #:y-min -10 #:y-max 10)]))

(define-syntax (igraf stx)
  (syntax-case stx ()
    [(igraf l1 l2)
     #'(plot (list (axes)
                   (function-interval l1 l2))
             #:x-min -4 #:x-max 4
             #:y-min -10 #:y-max 10)]))

(define expn 
  (λ (x . ls) 
    (map (λ (m) (expt m x)) ls)))

(struct lazy-seq (hd tl))

(define-syntax-rule (lcons x xs)
  (lazy-seq x (λ () xs)))

(define (head lxs)
  (lazy-seq-hd lxs))

(define (tail lxs)
  (lazy-seq-tl lxs))

(define (force lxs)
  (define (all-tail lxs1)
    (let ([tmp ((lazy-seq-tl lxs1))])
      (if (lazy-seq? tmp)
          (cons (lazy-seq-hd tmp) (all-tail tmp))
          tmp)))
  (cons (lazy-seq-hd lxs) 
        (all-tail lxs)))

























