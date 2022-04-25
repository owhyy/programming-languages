
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
;; one way is this
(define (cycle-lists xs ys)
  (define (rec (n) (lambda () (rec (+ n 1)))))) ; (rec calls itself with n+1 inside a thunk)

(define (cycle-lists xs ys)
  (define (rec (n) (rec (+ n 1))))
  (letrec ([f (lambda (x y) (rec (+ n 1))])

(define (vector-assoc v vec)
  (define (loop n)
    (if (> n (vector-length vec))
      #f
      (letrec ([elem (vector-ref vec n)])
        (if (and (pair? elem) (equal? (car elem) v))
          elem
          (loop (+ n 1))))))
  (loop 0))

(define (cached-assoc xs n)
  (lambda (v) null))

;; dunno yet
;(define (while-less e1 e2) )
