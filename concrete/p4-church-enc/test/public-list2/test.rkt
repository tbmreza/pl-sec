#lang racket

(require "../../church-compile.rkt")

(define prog
  '(if (null? '())
       (let ([lst (cons '() (cons 3 '()))])
         (if (null? (car lst))
             (* 2 (car (cdr lst)))
             7))
       5))

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

