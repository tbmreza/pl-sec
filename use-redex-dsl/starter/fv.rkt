#lang racket
(require redex)
(require "common.rkt")
(require "subtract.rkt")

(provide fv)

; (fv e) computes the sequence of free variables of e
; a variable occurrence of x is free in e 
; if no (lambda (... x ...) ...) dominates its occurrence 
 
(module+ test
  (test-equal (term (fv x)) (term (x)))
  (test-equal (term (fv (lambda (x) x))) (term ()))
  (test-equal (term (fv (lambda (x) (y z x)))) (term (y z))))
 
(define-metafunction Lambda
  fv : e -> (x ...)
  [(fv x) (x)]
  [(fv (lambda (x ...) e))
   (subtract (x_e ...) x ...)
   (where (x_e ...) (fv e))]
  [(fv (e_f e_a ...))
   (x_f ... x_a ... ...)
   (where (x_f ...) (fv e_f))
   (where ((x_a ...) ...) ((fv e_a) ...))])
