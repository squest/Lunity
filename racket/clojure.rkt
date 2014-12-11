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

(define (looper->> lxs res)
  (if (null? lxs)
      res
      (looper->> (rest lxs) 
                 (append (first lxs) (list res)))))

(define-syntax (->> stx)
  (syntax-case stx ()
    [(->> . lst)
     #'(foldl (λ (x y) (append y (list x)))
              (first lst) 
              (rest lst))]))

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

(define (dec n)
  (- n 1))

(define (inc n)
  (+ n 1))

(define (ltake n lxs)
  (if (= n 0)
      null
      (cons (head lxs)
            (ltake (dec n) ((tail lxs))))))

(define (lrange- start step)
  (lcons start (lrange- (+ start step) step)))

(define lrange
  (case-lambda 
    [() (lrange- 0 1)]
    [(start) (lrange- start 1)]
    [(start step) (lrange- start step)]))

(define (geo-seq start step)
  (lcons start (geo-seq (* start step) step)))

(define (ldrop n lxs)
  (if (zero? n)
      lxs
      (if (null? lxs)
          null
          (if (list? lxs)
              (drop lxs n)
              (ldrop (dec n)
                     ((tail lxs)))))))

(define (iterate f i)
  (lcons i (iterate f (f i))))

(define (take-while f lxs)
  (define (ltake-while lxs)
    (let ([tmp (head lxs)])
      (if (f tmp)
          (cons tmp (ltake-while ((tail lxs))))
          null)))
  (define (stake-while lxs)
    (if (null? lxs)
        null
        (let ([tmp (first lxs)])
          (if (f tmp)
              (cons tmp (stake-while (rest lxs)))
              null))))
  (if (list? lxs)
      (stake-while lxs)
      (ltake-while lxs)))

(define (drop-while f lxs)
  (define (ldrop-while lxs)
    (if (f (head lxs))
        (ldrop-while ((tail lxs)))
        lxs))
  (define (sdrop-while lxs)
    (if (null? lxs)
        null
        (if (f (first lxs))
            (sdrop-while (rest lxs))
            lxs)))
  (if (lazy-seq? lxs)
      (ldrop-while lxs)
      (sdrop-while lxs)))

(define (lmap f lxs)
  (lcons (f (head lxs)) (lmap f ((tail lxs)))))

(define (lfilter f lxs)
  (if (f (head lxs))
      (cons (head lxs) (lfilter f ((tail lxs))))
      (lfilter f ((tail lxs)))))

(define (keep f lxs)
  (define (lkeep lxs)
    (let ([tmp (f (head lxs))])
      (if tmp
          (lcons tmp (lkeep ((tail lxs))))
          (let ([tmp (drop-while (λ (x) (not (f x))) lxs)])
            (lcons (head tmp) (lkeep ((tail tmp))))))))
  (define (skeep lxs)
    (if (null? lxs)
        null
        (let ([tmp (f (first lxs))])
          (if tmp 
              (cons tmp (skeep (rest lxs)))
              (skeep (rest lxs))))))
  (if (lazy-seq? lxs)
      (lkeep lxs)
      (skeep lxs)))

(define (comp . funs)
  (define (looper x lst)
    (if (= 1 (length lst))
        ((first lst) x)
        (looper ((first lst) x) (rest lst))))
  (lambda (x) (looper x (reverse funs))))

(define (rc f . num)
  (lambda (x) (apply f (reverse (cons x (reverse num))))))

(define (lc f . num)
  (lambda (x) (apply f (cons x num))))

(define (juxt . lst)
  (define (looper x lxs res)
    (if (null? lxs)
        res
        (looper x (rest lxs) (cons ((first lxs) x) res))))
  (lambda (x) (reverse (looper x lst null))))

(define fibolist
  (case-lambda
    [() (lmap (juxt first third)
              (iterate 
               (λ (x) (list (+ (first x)
                               (second x))
                            (first x)
                            (inc (third x))))
               (list 1 1 1)))]
    [(i) (ltake i 
                (lmap (juxt first third)
                      (iterate 
                       (λ (x) (list (+ (first x)
                                       (second x))
                                    (first x)
                                    (inc (third x))))
                       (list 1 1 1))))]))

(define (take-while-by f g ls)
  (define (lver lxs)
    (if (null? lxs)
        null
        (if (f (g (first lxs)))
            (cons (first lxs) (lver (rest lxs)))
            lxs)))
  (define (sver lxs)
    (if (f (g (head lxs)))
        (cons (head lxs) (sver ((tail lxs))))
        null))
  (if (list? ls)
      (lver ls)
      (sver ls)))

(define (drop-while-by f g ls)
  (define (sver lxs)
    (if (null? lxs)
        null
        (if (f (g (first lxs)))
            (sver (rest lxs))
            lxs)))
  (define (lver lxs)
    (if (f (g (head lxs)))
        (lver ((tail lxs)))
        lxs))
  (if (list? ls)
      (sver ls)
      (lver ls)))

(define (sort-by f lst)
  (if (null? lst)
      null
      (append (sort-by f (filter (λ (x) (>= (f (first lst)) (f x)))
                                 (rest lst)))
              (list (first lst))
              (sort-by f (filter (λ (x) (< (f (first lst)) (f x)))
                                 (rest lst))))))












(define (negate f)
  (λ (x) (not (f x))))














































































