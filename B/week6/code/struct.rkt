#lang racket
;; solution using struct for simple expression interpreter

;; structs are better to use here, because they automatically define:
;; 1. constructors
;; 2. testers
;; 3. getters(extractors) which also test the types

;; since racket is a dinamically typed language, we
;; 1. can't specify that these are the only kinds of exp (anything may be an exp, whereas in static typing, const, negate, add and multiply are the ONLY kinds/types of exps)
;; 2. can't specify the types of the fields under the expressions (can't specify that const MUST hold numbers, negate MUST hold expressions); e under negate can be anything

(struct const (int) #:transparent) ; #:transparent makes repl print struct contents beautifully
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (const (- (const-int (eval-exp (negate-e e)))))]
        [(add? e) (let ([v1 (const-int (eval-exp (add-e1 e)))]
                        [v2 (const-int (eval-exp (add-e2 e)))])
                    (const (+ v1 v2)))]
        [(multiply? e) (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                             [v2 (const-int (eval-exp (multiply-e2 e)))])
                         (const (* v1 v2)))]
        [else (error "eval-exp expected an exp")]))
