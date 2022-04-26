
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (if (empty? xs)
      empty
      (cons (string-append (car xs) suffix) (string-append-map (cdr xs) suffix))))

(define (list-nth-mod xs n)
  (cond ((< n 0)  (error "list-nth-mod: negative number"))
        ((null? xs) (error "list-nth-mod: empty list"))
        (else (car (list-tail xs (remainder n (length xs)))))))

(define ones (lambda () (cons 1 ones)))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (x ss)
                (if (= n x)
                    null
                    (cons (car (ss))
                          (f (+ x 1) (cdr (ss))))))])
    (f 0 s)))

(define nats
  (letrec ([f (lambda (x)
                (cons x
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0)
                          (- x)
                          x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (cons x
                      (lambda () (f
                                  (if (eq? x "dog.jpg")
                                      "dan.jpg"
                                      "dog.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s)))
                   (stream-add-zero (cdr (s))))))

;; this is so coooool
(define (cycle-lists xs ys)
  (letrec  ([f (lambda (n) (cons (cons (list-nth-mod xs n) ; this is what I was missing
                                       (list-nth-mod ys n))
                                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (define (loop n)
    (if (>= n (vector-length vec))
        #f
        (letrec ([elem (vector-ref vec n)])
          (if (and (pair? elem) (equal? (car elem) v))
              elem
              (loop (+ n 1))))))
  (loop 0))

;; should return a function taking in a value v and returninng the same thing assoc returns;
;; should create a cache of length n; cache should be a vector of length n
;; created by the call to cached-assoc); it should be mutated;
;; 0. initialize cache to n #fs
;; 1. check cache for answer
;; if answer is there - just produce it
;; if answer is not there use (asoc xs v).
;; if result is not #f, (vector-set! cache position (assoc xs v)) before returning
;; you'll need a variable for holding the vector and a second variable to keep track of which slot you need to mutate it: after modifying cache, increment variable, or set it back to 0 (if (= index n))

(define (cached-assoc xs n)
  (lambda (v)
    (define (loop cache index)
      (if (vector-assoc v cache)
          (vector-assoc v cache)
          (if (assoc v xs)
              (begin
                (vector-set! cache index (assoc v xs))
                (set! index (if (= index (- n 1)) 0 (+ index 1)))
                (assoc v xs))
              #f)))
    (loop (make-vector n #f) 0)))

;; e1 e2 are expressions
;; while-less do are syntax
;; should:
;; 1. evaluate e1 exactly once
;; 2. evaluate e2 at least once
;; 3. keep doing (e2) until its result is not A NUMBER such that (> (e2) (e1))
;; if evaluation terminates, result is #t
;; should use a recursive thunk

;(define a 2)
; idk yet
(define (while-less e1 e2)
  (letrec ([f (lambda (res)
                (cons (< res e2) ;; if the result is a number < (e2), stop
                      (lambda ()(f res))))]) ;; otherwise keep on calling itself, but don't re-evaluate (e1)
    (lambda () (f e1)))
  (letrec ([loop (lambda (stream)
                   (let ([pr (stream)])
                     (if (car pr)
                         #t
                         (loop (cdr pr)))))])
    (loop (while-less e1 e2))))

