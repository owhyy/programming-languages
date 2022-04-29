;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem 1

(define (racketlist->mupllist rl)
  (if (null? rl)
    (aunit)
    (apair (car rl)
           (racketlist->mupllist (cdr rl)))))

(define (mupllist->racketlist ml)
  (if (aunit? (eval-exp ml))
    null
    (cons (eval-exp (fst ml))
          (mupllist->racketlist (eval-exp (snd ml))))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)] ; if the environment is empty, it means that the variable wasn't added to the environment
        [(equal? (car (car env)) str) (cdr (car env))] ; if the variable is the first in the environment, produce its value
        [#t (envlookup (cdr env) str)])) ; otherwise, check for the rest of the variables in the environment

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) ; variables evaluate to their values
         (envlookup env (var-string e))]
        [(int? e) e] ; expressions evaluate to themselves
        [(aunit? e) (aunit)]
        [(closure? e) e]
        [(add? e) ; add evaluates to (int (+ v1 v2))
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (int (+ (int-num v1) (int-num v2))) ;; convert to an int
             (error "MUPL addition applied to non-number")))]
        [(fun? e) (closure env e)] ; functions evaluate to a closure holding the function and the current environment (a function is a closure)
        [(ifgreater? e) ; ifgreater evaluates to v3 if v1 > v2, and to v4 otherwise
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
             (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))
             (error "MUPL ifgreater arguments are not int")))]
        [(mlet? e) ; a mlet evaluates its body using an environment in which its mlet-var(the name) has the value of its first expression(mlet-e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (append (list (cons (mlet-var e) v)) env)))]
        [(call? e)
         (let ([call-function-closure (eval-under-env (call-funexp e) env)]
               [call-actual-parameter (eval-under-env (call-actual e) env)])
           ;; a call has 2 things:
           ;; - a function closure (its first argument)
           ;; - a actual parametes (its second argument)
           ;; a closure has two things:
           ;; - a environment
           ;; - a function
           ;; a function has three things:
           ;; - a name
           ;; - a formal parameter
           ;; - a body
           (if (not (closure? call-function-closure)) ; if the first argument of the call is not a closure
             (error "MUPL applied to non-closure") ; then error
             (letrec ([closure-environment (closure-env call-function-closure)]
                      [closure-function (closure-fun call-function-closure)]
                      [function-name (fun-nameopt closure-function)]
                      [function-formal-parameter (fun-formal closure-function)]
                      [function-body (fun-body closure-function)])
               (if function-name ; if the function's name is not #f, add it to the environment in which you evaluate it
                 (eval-under-env function-body (append (list (cons function-name call-function-closure) (cons function-formal-parameter call-actual-parameter)) closure-environment))
                 (eval-under-env function-body (append (list (cons function-formal-parameter call-actual-parameter)) closure-environment))))))] ; otherwise just evaluate it
        [(apair? e) ; a pair evaluates to a pair of the evaluated subexpressions
         (let ([e1 (eval-under-env (apair-e1 e) env)]
               [e2 (eval-under-env (apair-e2 e) env)])
           (apair e1 e2))]
        [(fst? e) ; fst evaluates to its first subexpression, if the result is a pair
         (let ([subexp (eval-under-env (fst-e e) env)])
           (if (apair? subexp)
             (apair-e1 subexp)
             (error "MUPL fst applied to a non-pair")))]
        [(snd? e) ; snd evaluates its second subexpression, if the result was a pair
         (let ([subexp (eval-under-env (snd-e e) env)])
           (if (apair? subexp)
             (apair-e2 subexp)
             (error "MUPL snd applied to a non-pair")))]
        [(isaunit? e); a isaunit evaluates to its subexpression, if the result is a unit, then the result is (int 1), otherwise, it's (int 0)
                       (let ([subexp (eval-under-env (isaunit-e e) env)])
                         (if (aunit? subexp)
                           (int 1)
                           (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

; should return a mupl experssion, that when run evaluates e1, and if the reuslt is (aunit) then evaluate e2, otherwise evaluate e3
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

; this basically needs evaluate e2 to an environment in which every string from lstlst is bound to its expression
; lstlst is a (list (cons "x" expression)

(define (mlet* lstlst e2)
  (if (null? lstlst)
    e2
    (mlet (caar lstlst)
          (cdar lstlst)
          (mlet* (cdr lstlst) e2))))

#| (append (list (cons (mlet-var e) v)) |#
;((x . #(struct:int 10)))((x . #(struct:int 10)) (y . #(struct:int 31)))((x . #(struct:int 10)) (y . #(struct:add #(struct:var x) #(struct:int 21))))

(define (ifeq e1 e2 e3 e4)
  (ifgreater
    (mlet "_x" e1
          (mlet "_y" e2
                (ifgreater (var "_x") (var "_y")
                           (int 1)
                           (int -1))))
    (int 0) e3 e4))

;; Problem 4

(define mupl-map
  (fun #f "fn"
       (fun "loop" "l"
            (ifaunit (var "l")
                     (aunit)
                     (apair (call (var "fn")   (fst (var "l")))
                            (call (var "loop") (snd (var "l"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i"
             (fun #f "xs"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "i")))) (var "xs"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
