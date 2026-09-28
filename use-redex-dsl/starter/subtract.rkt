#lang racket/base
(require redex)
(require "common.rkt")

(provide subtract)

(module+ test
  (test-equal (term (subtract (x y z x) x z)) (term (y))))
 
(define-metafunction Lambda
  subtract : (x ...) x ... -> (x ...)
  [(subtract (x ...)) (x ...)]
  [(subtract (x ...) x_1 x_2 ...)
   (subtract (subtract1 (x ...) x_1) x_2 ...)])
 
; (subtract1 (x ...) x_1) removes x_1  from (x ...)
(module+ test
  (test-equal (term (subtract1 (x y z x) x)) (term (y z))))
 
(define-metafunction Lambda
  subtract1 : (x ...) x -> (x ...)
  [(subtract1 (x_1 ... x x_2 ...) x)
   (x_1 ... x_2new ...)
   (where (x_2new ...) (subtract1 (x_2 ...) x))
   (where #false (in x (x_1 ...)))]
  [(subtract1 (x ...) x_1) (x ...)])
