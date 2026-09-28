#lang racket

(require "../../church-compile.rkt")

(define prog
  '(let* ([U (lambda (u) (u u))]
          [Yv (U (lambda (Y) (lambda (f) (f (lambda (x) (((U Y) f) x))))))]
          [len (Yv (lambda (len)
                     (lambda (lst)
                       (if (null? lst)
                           0
                           (add1 (len (cdr lst)))))))])
     (len (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons '() '())))))))))

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

