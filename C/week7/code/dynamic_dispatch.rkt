#lang racket
; the whole thing with dynamic dispatch is that the parameter self is implicit

(struct obj (fields methods)) ; a object has (a list of) fields and (a list of) methods

; our fields are an immutable list of mutable pairs
; a pair (field) is (cons field_name(symbol) value)
; for tht we can define getters and setters, that return or mutate the field

; this will beused for finding the fields
(define (assoc-m v xs)
  (cond [(null? xs) #f]
        [(equal? v (mcar (car xs))) (car xs)]
        [#t (assoc-m v (cdr xs))]))

; if it finds the field in the fields of the object, it returns the value, else prints error
(define (get obj fld)
  (let ([pr (assoc-m fld (obj-fields obj))])
    (if pr
      (mcdr pr)
      (error "field not found"))))

; if it finds the field, it sets its value to v, else prints error
(define (set obj fld v)
  (let ([pr (assoc-m fld (obj-fields obj))])
    (if pr
      (set-mcdr! pr v)
      (error "field not found"))))

; takes a object, a message(the method) and a bunch of arguments to pass to it
(define (send obj msg . args)
  (let ([pr (asco msg (obj-methods ob))])
    (if pr
      ((cdr pr) obj args) ; call the method on the object, with the arguments passed
      (error "method not found" msg))))

; now we can define the make-point thingy
(define (make-point _x _y)
  (obj ; create obj
    (list (mcons 'x _x)  ; variable x, with value _x
          (mcons 'y _y)) ; variable y, with value _y
    (list (cons 'get-x (lambda (self args) (get self 'x))) ; here we need to use 'self', to mean that we're applying get to the ACTUAL OBJECT, not a sublcass or w/e of it; in dynamic dispatch, it is implicit, and performed automatically
          (cons 'get-y (lambda (self args) (get self 'y)))
          (cons 'set-x (lambda (self args) (set self 'x (car args))))
          (cons 'set-y (lambda (self args) (set self 'y (car args))))
          (cons 'distToOrigin
                (lambda (self args)
                  (let ([a (send self 'get-x)]
                        [b (send self 'get-y)])
                    (sqrt (+ (* a a) (* b b)))))))))

; now we create subclassing
(define (make-color-point _x _y _c)
  (let ([pt (make-point _x _y)])
    (obj
      (cons (mcons 'color _c)  ; add to the point's class's field the field color
            (obj-fields pt))
      (append (list            ; ado to the point's class's methods the get-color and set-color methods
                (cons 'get-color (lambda (self args) (get self 'color)))
                (cons 'set-color (lambda (self args) (set self 'color (car args)))))
              (obj-methods pt)))))

; and now we create make-polar-point to demonstrate dynamic dispatch
(define (make-polar-point _r _th)
  (let ([pt (make-point #f #f)])
    (obj
      (append (list (mcons 'r _r)
                    (mcons 'theta _theta))
              (obj-fields pt)) ; the other fields of point
      (append
        (list ('set-r-theta
               (lambda (self args)
                 (begin
                   (set self 'r (car args))
                   (set self 'theta (cadr args)))))
              (cons 'get-x (lambda (self args)
                             (let ([r (get self 'r)]
                                   [theta (get self 'theta)]
                                   (* r (cos theta))))))
              (cons 'get-y (lambda (self args)
                             (let ([r (get self 'r)]
                                   [theta (get self 'theta)]
                                   (* r (sin theta))))))
              (cons 'set-x (lambda (self args)
                             (let* ([a (car args)]
                                    [b (send self 'get-y)]
                                    [theta (atan (/ b a))]
                                    [r (sqrt (+ (* a a) (* b b)))])
                               (send self 'set-r-theta r theta))))
              (cons 'set-y (lambda (self args)
                             (let* ([b (car args)]
                                    [a (send self 'get-x)]
                                    [theta (atan (/ b a))]
                                    [r (sqrt (+ (* a a) (* b b)))])
                               (send self 'set-r-theta r theta)))))
        (obj-methods pt)))))

; polar-point is going to use its own get-y,. get-x, set-y and set-x, because send adds self to the environment, and will call the methods on it.
