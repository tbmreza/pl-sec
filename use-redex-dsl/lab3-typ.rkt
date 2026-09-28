#lang racket
(require redex)
(require "starter/common.rkt")
(require "starter/tc-common.rkt")


(define-language TLambda
  (e ::=
     n
     +
     x
     (lambda ((x_!_ t) ...) e)
     (e e ...))
  (t ::=
     int
     (t ... -> t))
  (x ::= variable-not-otherwise-mentioned))
 
(define in-TLambda? (redex-match? TLambda e))
 
(define e1
  (term
    (lambda ([x int] [f (int -> int)])
      (+ (f (f x)) (f x)))))
(define e2
  (term
    (lambda ([x int] [f ((int -> int) -> int)])
      (f x))))
(define e3
  (term
    (lambda ([x int] [x (int -> int)])
      x)))
 
(module+ test
  (test-equal (in-TLambda? e1) #true)
  (test-equal (in-TLambda? e2) #true)
  (test-equal (in-TLambda? e3) #false))

(define-extended-language TLambda-tc TLambda
  (Γ ::= ((x t) ...)))
 
(module+ test
  (test-equal (judgment-holds (⊢ ()
                                 ,e1
                                 (int (int -> int) -> int)))
              #true)
  (test-equal (judgment-holds (⊢ () ,e2 t)) #false)
  (displayln  (judgment-holds (⊢ () ,e1 t) t))
  (displayln  (judgment-holds (⊢ () ,e2 t) t))
)
 
(define-judgment-form TLambda-tc
      #:mode (⊢ I I O)
  #:contract (⊢ Γ e t)

  [----------------------- "number"
   (⊢ Γ n int)]
 
  [----------------------- "+"
   (⊢ Γ + (int int -> int))]
 
  [----------------------- "variable"
   (⊢ Γ x (lookup Γ x))]
 
  [(⊢ (extend Γ (x_1 t_1) ...) e t)
   ------------------------------------------------- "lambda"
   (⊢ Γ (lambda ((x_1 t_1) ...) e) (t_1 ... -> t))]
 
  [(⊢ Γ e_1 (t_2 ... -> t))
   (⊢ Γ e_2 t_2) ...
   ------------------------------------------------- "application"
   (⊢ Γ (e_1 e_2 ...) t)])


; BROKEN REDUCTION

(define ->
  (reduction-relation
    TLambda
    ; #:domain e
    (--> e (lambda ((x int)) x))))

(traces ->
  (term
    (((lambda ((x (int -> int)))
        x)
      (lambda ((x int))
        x))
     1))
  ; #:pred (lambda (e) (judgment-holds (⊢ () ,e int)))  ; ?? panics
  )

; (redex-check TLambda
;              e
;              (implies (judgment-holds (⊢ () e int))
;                       (judgment-holds (⊢ () (eval-value e) int)))
;              #:attempts 3)


; Exercise 6. Develop a reduction system for which the trace expression from the lecture
; preserves types
; (module+ test
;   (traces ->
;           (term (((lambda ((x (int -> int))) x) (lambda ((x int)) x)) 1))
;           #:pred (lambda (e)
;                    (judgment-holds (⊢ () ,e int)))))
; Exercise 7. Extend TLambda with syntax for the following:
; additional numeric operators, say, multiplication, subtraction, and division;
;   let expressions;
;   Boolean constants plus strict and and or operators as well as a branching construct;
;   lists, specifically constructors and selectors (de-constructors);
;   explicitly recursive function definitions.
