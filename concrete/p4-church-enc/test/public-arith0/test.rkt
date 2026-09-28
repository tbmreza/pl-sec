#lang racket

(require "../../church-compile.rkt")

(define prog '(+ 1 (add1 (* 3 3))))

(define unchurch church->nat)

(define v (eval prog (make-base-namespace)))
(with-output-to-file "answer"
  (lambda ()
    (print v))
  #:exists 'replace)

(define compiled (church-compile prog))
(define cv-comp (eval compiled (make-base-namespace)))
(define v-comp (unchurch cv-comp))
(with-output-to-file "output"
  (lambda ()
    (print v-comp))
  #:exists 'replace)

