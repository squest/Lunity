#lang racket

(define (sum xs)
  (letrec [(f (fn (xs) (if (null? xs) 0 (+ (car xs) (f (cdr xs))))))
           (g (fn (xs) (if (null? xs) 0 (+ (car xs) (g ((cdr xs)))))))]
    (if (list? xs)
        (f xs)
        (g xs))))

(define (prod xs)
  (if (null? xs)
      1
      (let [(a (car xs))]
        (cond [(number? a) (* a (prod (cdr xs)))]
              [(list? a) (* (prod a) (prod (cdr xs)))]
              [#t [prod [cdr xs]]]))))


(define (combination n k)
  (define (fact a b)
    (prod (range a (+ 1 b))))
  (/ (fact (+ k 1) n) (fact 1 (- n k))))

(define (fact-range a b)
  (prod (range a (+ 1 b))))

(define (fact i)
  (fact-range 1 i))

(define (permutation n k)
  (fact-range (+ 1 k) n))

(define (inc i) (+ i 1))
(define (dec i) (- i 1))

(define ones (lambda () (cons 1 ones)))

(define (f x)
  (cons x (lambda () (f (inc x)))))

(define (get n xs)
  (if (= n 0)
      null
      (cons (car xs) (get (dec n) ((cdr xs))))))

(define (iterate f i)
  (cons i (lambda () (iterate f (f i)))))

(define (lmap f xs)
  (cons (f (car xs)) (lambda () (lmap f ((cdr xs))))))

(define (lfilter f xs)
  (if (null? xs)
      null
      (if (f (car xs))
          (cons (car xs) (fn () (lfilter f ((cdr xs)))))
          (lfilter f ((cdr xs))))))

(define (throw n xs)
  (if (zero? n)
      xs
      (throw (dec n) ((cdr xs)))))

(define-syntax-rule (def fname body)
  (define fname body))

(define-syntax-rule (fn fbinding body)
  (lambda fbinding body))

(define-syntax-rule (dik binding body)
  (let* binding body))

(define fibo
  (lmap first 
        (iterate (fn (x) 
                     (list (+ (first x) (second x))
                           (first x))) 
                 '(1 1))))

(define lrange
  (case-lambda 
    [(start) 
     (lrange start 1)]
    [(start step)
     (cons start (fn () (lrange (+ start step) step)))]))

(def (grange a r)
  (cons a (fn () (grange (* a r) r))))

(def (take-while f xs)
  (if (f (car xs))
      (cons (car xs) (take-while f ((cdr xs))))
      null))

(def (get-while f xs)
  (if (f (car xs))
      (cons (car xs) (fn () (get-while f ((cdr xs)))))
      null))

(def sisa remainder)
(def bagi quotient)

(def (jumlah-digit n)
  (if (< n 10)
      n
      (+ (sisa n 10) (jumlah-digit (bagi n 10)))))

(define (nth n xs)
  (letrec ([f (fn (n xs) (if (= n 0) (car xs) (f (dec n) (cdr xs))))]
           [g (fn (n xs) (if (= n 0) (car xs) (g (dec n) ((cdr xs)))))])
    (if (list? xs)
        (f n xs)
        (g n xs))))



























