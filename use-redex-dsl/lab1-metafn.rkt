#lang racket
(require redex)
(require "starter/common.rkt")
(require "starter/fv.rkt")
(require "starter/subtract.rkt")


(module+ test  ; ex1
  (test-equal (term (bv x))
              (term ()))
  (test-equal (term (bv (lambda (a) a)))
              (term (a)))
  (test-equal (term (bv (lambda (x) (y z x))))
              (term (x)))
  (test-equal (term (bv (lambda (a1 a2) (lambda (b) (a3 a4 a5)))))
              (term (a1 a2))))

; A variable x is bound in e_Lambda if x occurs in a lambda-parameter list in e_Lambda.
(define-metafunction Lambda
  bv : e -> (x ...)
  [(bv x) ()]
  [(bv (lambda (x ...) e))
   (subtract (x ...) x_bound ...)
   (where (x_bound ...) (bv e))]
  [(bv (e_head e_tail ...))  ; map bv e
   (x_head ... x_tail ... ...)
   (where (x_head ...) (bv e_head))
   (where ((x_tail ...) ...) ((bv e_tail) ...))])

(define-extended-language Env Lambda
  (e ::= .... natural)  ; exactly-4 dots: extending proto (absence: overriding)
  (env ::= ((x e) ...)))

(module+ test  ; ex2
  (test-equal (term (lookup x ((w 7) (x 8)))) (term 8)))

(define-metafunction Env
  lookup : x env -> e or boolean
  [(lookup x (_ ... (x e) _ ...))  e]
  [(lookup _ _) #false])

(define-metafunction Env
  let : env e -> e
  [(let ((x e_env) ...) e)
   ((lambda (x ...) e) e_env ...)])

(module+ test  ; ex3
  (test-equal
    (term
      (fv
        (let ((x (lambda (a b c) a))
              (y (lambda (x) x)))
          (x y non-local-var y y))))
    (term (non-local-var))))
