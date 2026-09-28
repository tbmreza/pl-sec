#lang racket
(require redex)
(require "starter/common.rkt")


(define-extended-language Lambda-calculus Lambda
  (e ::= .... n)
  (n ::= natural)
  (v ::= (lambda (x ...) e))
  (C ::=  ; a context is an expression with one hole in lieu of a sub-expression 
     hole
     (e ... C e ...)
     (lambda (x_!_ ...) C)))

; (module+ test
;   (define Context? (redex-match? Lambda-calculus C))
;   (define C1 (term ((lambda (x y) x) hole 1)))
;   (define C2 (term ((lambda (x y) hole) 0 1)))
;   (test-equal (Context? C1) #true)
;   (test-equal (Context? C2) #true))
;
; (module+ test
;   (define-extended-language Lambda/n Lambda
;     (e ::= .... n)
;     (n ::= natural))
;   (define in-Lambda/n? (redex-match? Lambda/n e))
;
;   (define e1 (term (in-hole ,C1 1)))
;   (define e2 (term (in-hole ,C2 x)))
;  
;   (test-equal (in-Lambda/n? e1) #true)
;   (test-equal (in-Lambda/n? e2) #true))

; the λβ calculus, reductions only 
; (module+ test
;   ; does the one-step reduction reduce both β redexes? 
;   (test--> -->β
;            #:equiv =α/racket
;            (term ((lambda (x) ((lambda (y) y) x)) z))
;            (term ((lambda (x) x) z))
;            (term ((lambda (y) y) z)))
;  
;   ; does the full reduction relation reduce all redexes? 
;   (test-->> -->β
;             (term ((lambda (x y) (x 1 y 2))
;                    (lambda (a b c) a)
;                    3))
;             1)
;
;   ; (traces -->β
;   ;         (term ((lambda (x y)
;   ;                  ((lambda (f) (f (x 1 y 2)))
;   ;                   (lambda (w) 42)))
;   ;                ((lambda (x) x) (lambda (a b c) a))
;   ;                3)))
;   )

(define -->β
  (reduction-relation
    Lambda-calculus
    (--> (in-hole C ((lambda (x_1 ..._n) e) e_1 ..._n))
         (in-hole C (subst ([e_1 x_1] ...) e))
         β)))


(define lambda? (redex-match? Lambda-calculus e))

; (traces -->β  ; ok
;         (term ((lambda (x y)
;                  ((lambda (f) (f (x 1 y 2)))
;                   (lambda (w) 42)))
;                ((lambda (x) x) (lambda (a b c) a))
;                3)))

(define -->βv
  (reduction-relation
   Lambda-calculus
   (--> (in-hole C ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole C (subst ([v_1 x_1] ...) e)))))

; (traces -->βv  ; ok, fewer nodes
;         (term ((lambda (x y)
;                  ((lambda (f) (f (x 1 y 2)))
;                   (lambda (w) 42)))
;                ((lambda (x) x) (lambda (a b c) a))
;                3)))


; SEMANTICS

(define-extended-language Standard Lambda-calculus
  (v ::= n
     (lambda (x ...) e))
  (E ::=
     hole
     (v ... E e ...)))

(define t0
  (term
   ((lambda (x y) (x y))
    ((lambda (x) x) (lambda (x) x))
    ((lambda (x) x) 5))))
(define t0-one-step
  (term
   ((lambda (x y) (x y))
    (lambda (x) x)
    ((lambda (x) x) 5))))

(module+ test
  ; yields only one term, leftmost-outermost
  (test--> s->βv t0 t0-one-step)
  ; but the transitive closure drives it to 5
  (test-->> s->βv t0 5))

(define s->βv
  (reduction-relation
   Standard
   (--> (in-hole E ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole E (subst ((v_1 x_1) ...) e)))))

(module+ test
  (test-equal (term (eval-value ,t0)) 5)
  (test-equal (term (eval-value ,t0-one-step)) 5)

  (define t1
    (term ((lambda (x) x) (lambda (x) x))))
  (test-equal (lambda? t1) #true)
  (test-equal (redex-match? Standard e t1) #true)
  (test-equal (term (eval-value ,t1)) 'closure))

(define-metafunction Standard
  eval-value : e -> v or closure
  [(eval-value e) any_1 (where any_1 (run-value e))])

(define-metafunction Standard
  run-value : e -> v or closure
  [(run-value n) n]
  [(run-value v) closure]
  [(run-value e)
   (run-value e_again)
   ; (v) means that we expect s->βv to be a function 
   (where (e_again) ,(apply-reduction-relation s->βv (term e)))])


; REAL TEST

(define-namespace-anchor A)
(define N (namespace-anchor->namespace A))
; Lambda.e -> Number or 'closure or exn
(define (racket-evaluator t0)
  (define result
    (with-handlers ((exn:fail? values))
      (eval t0 N)))
  (cond
    [(number? result) result]
    [(procedure? result) (term closure)]
    [else (make-exn "hello world" (current-continuation-marks))]))

(define-metafunction Standard
  theorem:racket=eval-value : e -> boolean
  [(theorem:racket=eval-value e)
   ,(letrec ([rr (racket-evaluator (term e))]
             [vr (term (run-value e))])
      (cond
        [(and (exn? rr) (eq? (term stuck) vr))
         #true]
        [(exn? rr) #false]
        [(eq? (term stuck) vr) #false]
        [else (equal? vr rr)]))])

(module+ test
  (test-equal (term (theorem:racket=eval-value ,t0)) #true)
  (test-equal (term (theorem:racket=eval-value ,t0-one-step)) #true)
  (test-equal (term (theorem:racket=eval-value ,t1)) #true)
  )

(redex-check Standard e (term (theorem:racket=eval-value ,t0)))


; Exercise 4. Develop a βη reduction relation for Lambda-η.  ; β b*  η y*

(define-extended-language Lambda-η Lambda
  (e ::= .... n)
  (n ::= natural)
  (C ::=
     hole
     (e ... C e ...)
     (lambda (x_!_ ...) C))
  (v ::=
     n
     (lambda (x ...) e)))

(define -->βη
  ; (reduction-relation
  (extend-reduction-relation -->β
    Lambda-η
    (--> (in-hole C (lambda (x) (e_f x)))
         (in-hole C e_f)
         ; (where #true (in x (fv e_f)))  ; ?? metafn not evaluating
         η)))
; (apply-reduction-relation -->βη (term ((lambda (a b) (a b b)) 11 22)))  ; ok
(traces -->βη (term (lambda (a) ((lambda (f) f) a))))  ; ok
