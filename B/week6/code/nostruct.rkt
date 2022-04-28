#lang racket

;; representation of expression interpreter
;; without using structs. expressions will be
;; lists

;; constructors
(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

;; helper functions to check type of expression
(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

;; getters
;; just better than using cadr, but same thing
;; better would be to check like (if (Const? e) (cadr e) (error ...))
(define (Const-int e) (cadr e))
(define (Negate-e e) (cadr e))
(define (Add-e1 e) (cadr e))
(define (Add-e2 e) (caddr e))
(define (Multiply-e1 e) (cadr e))
(define (Multiply-e2 e) (caddr e))

;; evaluator
;; how it happens: a expression in its 'lowest' form is a const, so to get the actual value - you call (Const-int) on it
;; the expressions from Negate or Add are evaluated to Const, and then Const-int is called on the Consts produced by them
;; finally, you convert them to a Const expression, after performing the right operation (+ or - or *...)
(define (eval-exp e)
  (cond [(Const? e) e] ;returns the exp, not the number
        [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add? e) (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                        [v2 (Const-int (eval-exp (Add-e2 e)))])
                    (Const (+ v1 v2)))]
        [(Multiply? e) (let ([v1 (Const-int (eval-exp (Multiply-e1 e)))]
                             [v2 (Const-int (eval-exp (Multiply-e2 e)))])
                         (Const (* v1 v2)))]
        [else (error "eval-exp expected an exp")]))

;; this is okay, but there is a better way, which defines the constructors, gettrs and helper functions for checking the type automatically(and returning errors too). see struct.rkt
