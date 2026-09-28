#lang racket

(require "../../church-compile.rkt")

(define prog
  '(let ([a 2] [b 3])
     (let ([b 5] [c b])
       (* a (* b c)))))

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

