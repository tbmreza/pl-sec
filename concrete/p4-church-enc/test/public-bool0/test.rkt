#lang racket

(require "../../church-compile.rkt")

(define prog '#f)

; (define unchurch church->bool)
(define (unchurch cv-comp)
  ((church->bool cv-comp) (void)))

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

