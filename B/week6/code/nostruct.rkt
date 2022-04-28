#lang racket

(define (funny-sum xs)
  (cond [(null? xs) 0]
        [(number? xs) (car xs) (+ (car xs) (funny-sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs))
                               (funny-sum (cdr xs)))))
