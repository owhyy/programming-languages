
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

(define (stream-for-n-steps s n)
  null)

(define funny-number-stream (lambda () 0))

(define dan-then-dog (lambda () 0))

(define (stream-add-zero s)
  (lambda () null))

(define (cycle-lists xs ys)
  (lambda () null))

(define (vector-assoc v vec)
  #t)

(define (cached-assoc xs n)
  (lambda () null))

;; dunno yet
;(define (while-less e1 e2) )
