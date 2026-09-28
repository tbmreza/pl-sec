#lang racket
(require redex)
(require "starter/common.rkt")


(define e0
  (term ((lambda (x) x) 0)))
 
(define e1
  (term ((lambda (x y) x) 1 2)))

(define-extended-language Lambda/v Lambda
  (e ::= .... n +)
  (n ::= integer)
  (v ::= n + (lambda (x ...) e)))

(define-extended-language CEK Lambda/v
  (ρ ::= ((x c) ...))
  (c ::= (v ρ))
  (k ::= ((app [c ...] ρ [e ...]) ...)))

; (define-metafunction PCFς
;     injς : M -> ς
;     [(injς M) ((injρ M) ())])
 
(module+ test
  ; (test-->> -->cek (inj e0) (term [0 () ()]))  ; ?? metafn?
  (test-->> -->cek (term [,e0 () ()]) (term [0 () ()]))
  (test-->> -->cek (term [,e1 () ()]) (term [1 () ()])))

(define -->cek
  (reduction-relation
   CEK
   #:domain (e ρ k)
   (--> [x
         ((x_1 c_1) ... (x (v ρ)) (x_2 c_2) ...)
         ((app any_v any_r any_e) ...)]
        [v
         ρ
         ((app any_v any_r any_e) ...)]
        CEK-lookup)
   (--> [(lambda (x ..._n) e)
         (any_c ...)
         ((app [c ..._n] ρ []) (app any_v any_r any_e) ...)]
        [e
         ([x c] ... any_c ...)
         ((app any_v any_r any_e) ...)]
        CEK-β_v)
   (--> [+
         ρ
         ((app [n_1 n_2] []) (app any_v any_r any_e) ...)]
        [,(+ (term n_1) (term n_2))
         ()
         ((app any_v any_r any_e) ...)]
        CEK-+)
   (--> [(e_1 ...)
         ρ
         (any_k ...)]
        [e_last
         ρ
         ((app () ρ (e_1others ...)) any_k ...)]
        (where (e_1others ... e_last) (e_1 ...))
        CEK-push)
   (--> [v
         ρ
         ((app (c_1 ...) ρ_stack (e ...)) any_k ...)]
        [e_last
         ρ_stack
         ((app ((v ρ) c_1 ...) ρ_stack (e_prefix ...)) any_k ...)]
        (where (e_prefix ... e_last) (e ...))
        CEK-switch)))
 
(module+ test
  (test-equal (term (eval-cek ,e0)) 0)
  (test-equal (term (eval-cek ,e1)) 1))
 
(define-metafunction Lambda/v
  eval-cek : e -> v or closure or stuck
  [(eval-cek e) (run-cek [e () ()])])
 
(define-metafunction CEK
  run-cek : (e ρ k) -> v or closure or stuck
  [(run-cek (n ρ ())) n]
  [(run-cek (v ρ ())) closure]
  [(run-cek any_1)
   (run-cek (e_again ρ_again k_again))
   (where ((e_again ρ_again k_again))
          ,(apply-reduction-relation -->cek (term any_1)))]
  [(run-cek any) stuck])
 
(traces -->cek (term [,e1 () ()]))

; (redex-check CEK (e ρ k) (term (theorem:racket=eval-cek (e ρ k))))  ; ?? goal
