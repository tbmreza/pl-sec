#lang racket/base

#| interpreting:  PCF (Plotkin 1977: PL for Computable Functions)
   using:         ?? |#

(require redex)
(require rackunit)

(define-term fact-5
  ((μ (fact : (num -> num))
      (λ ([n : num])
        (if0 n 1
             (* n (fact (pred n))))))
   5))


#| REDEX OPERATIONS NOT SPECIFIC TO LANGUAGE INTERPRETED |#

(define-language REDEX)

(define-judgment-form REDEX
      #:mode (lookup I I O)
  #:contract (lookup ((any any) ...) any any)
           [ (lookup (_ ... (any any_0) _ ...) any any_0) ])
; (test-judgment-holds (lookup ((x 1) (y 2) (x 3)) y 2))

(define-metafunction REDEX ext1
  : ((any any) ...) (any any) -> ((any any) ...)

  [ (ext1 (any_0 ... (any_k any_v0) any_1 ...) (any_k any_v1))
    (any_0 ... (any_k any_v1) any_1 ...)]

  [ (ext1 (any_0 ...) (any_k any_v1))
    ((any_k any_v1) any_0 ...)])

(define-metafunction REDEX ext
  : ((any any) ...) (any any) ... -> ((any any) ...)

  [ (ext any) any ]

  [ (ext any any_0 any_1 ...)
    (ext1 (ext any any_1 ...) any_0)])
; usage: (term (ext ((x 1) (y 2) (x 3))))

(define-relation REDEX unique  ; define bool-producing metafunction
  ⊆ (any ...)
  [ (unique (any_!_1 ...)) ])
(check-true (term (unique ((1 2) (2 1)))))


#| TYPING JUDGMENT |#

(define-language PCF
  (M ::=
     N O X L
     (μ (X : T) L)  ; ?? how to read T
     (M M ...)
     (if0 M M M))
  (X ::= variable-not-otherwise-mentioned)
  (L ::= (λ ([X : T] ...) M))
; (V ::= N O L)  ; denotable Values are number | primitive-operator | lambda-term
  (N ::= number)
  (O ::= O1 O2)
  (O1 ::= succ pred)  ; sub1, add1 unfortunately racket-syntax highlighted in pattern
  (O2 ::= + *)
  (T ::= num (T ... -> T)))

(define-extended-language PCFt PCF
  (Γ ::= ((X T) ...)))

(define-judgment-form PCFt
      #:mode (⊢ I I I O)
  #:contract (⊢ Γ M : T)

  [------------- NUM
   (⊢ Γ N : num)]

  [(lookup Γ X T)
   -------------- VAR
   (⊢ Γ X : T)]

  [----------------------- OP1
   (⊢ Γ O1 : (num -> num))]

  [--------------------------- OP2
   (⊢ Γ O2 : (num num -> num))]

  [(⊢ Γ M_1 : num)
   (⊢ Γ M_2 : T)
   (⊢ Γ M_3 : T)
   --------------------------- IF0
   (⊢ Γ (if0 M_1 M_2 M_3) : T)]

  [(⊢ (ext Γ (X T)) L : T)
   ----------------------- μ
   (⊢ Γ (μ (X : T) L) : T)]

  [(⊢ Γ M_0 : (T_1 ..._1 -> T))
   (⊢ Γ M_1 : T_1) ...
   ----------------------- APP
   (⊢ Γ (M_0 M_1 ..._1) : T)]

  [(unique (X ...))
   (⊢ (ext Γ (X T) ...) M : T_n)
   ------------------------------------------ λ
   (⊢ Γ (λ ([X : T] ...) M) : (T ... -> T_n))]

  )
;         #:contract (⊢  Γ  M                :  T)
(test-judgment-holds (⊢ () (λ ([x : num]) x) : (num -> num)))


#| CONCEPT: REDUCTION USING SUBST |#

(require redex-aam-tutorial/subst)
; (term (subst (x 5) (y 7) (+ x y))) ; '(+ 5 7)

(define-judgment-form PCF
      #:mode (δ I O)  ; primitive operation \delta
  #:contract (δ (O N ...) N)
  [(δ (+ N_0 N_1)  ,(+ (term N_0) (term N_1)))]
  [(δ (* N_0 N_1)  ,(* (term N_0) (term N_1)))]
  [(δ (pred N)     ,(sub1 (term N)))]
  [(δ (succ N)     ,(add1 (term N)))])
; [(δ (sub1 N)     ,(sub1 (term N)))]  ; sub1, add1 unfortunately racket-syntax highlighted in pattern

(define r (reduction-relation PCF #:domain M
   (--> (μ (X : T) M)
        (subst (X (μ (X : T) M)) M)
        μ)

   (--> ((λ ([X : T] ...) M_0) M ...)
        (subst (X M) ... M_0)
        β)

   (--> (O N_0 ...) N_1
        (judgment-holds (δ (O N_0 ...) N_1))
        δ)

   (--> (if0 0 M_1 M_2)
        M_1
        if-t)

   (--> (if0 N M_1 M_2) M_2
        (side-condition (not (zero? (term N))))
        if-f)))
(define -->r (compatible-closure r PCF M))
(check-equal?
  (apply-reduction-relation* -->r (term (pred ((λ ([x : num]) x) (succ 5)))))
  `(5))
; (traces -->r (term (pred ((λ ([x : num]) x) (succ 5)))))  ; paths to final state

; Declare normal order reduction (leftmost outermost).
(define-extended-language PCFn PCF
  ; New constructor E wrapping PCF's M.
  (E ::= hole
     (E M ...)
     (O V ... E M ...)
     (if0 E M M)))
; (define -->n (context-closure r PCFn E))
; (traces -->n (term ((λ ([x : num]) x) (succ 5))))  ; single deterministic path to final state


#| CONCEPT: INJECTION |#

; `Explicit substitution` is env based, substitution-free reduction system (name due to historical theory).
; We define PCFρ to demonstrate ??
(define-extended-language PCF⇓ PCF
  (V ::=
     N O
     (L ρ)
     ((μ (X : T) L) ρ))
  (ρ ::= ((X V) ...)))

; skipping (define-judgment-form PCF⇓

(define-extended-language PCFρ PCF⇓
  (C ::= V (M ρ) (if0 C C C) (C C ...))
  (E ::= hole (V ... E C ...) (if0 E C C)))

(define vρ (reduction-relation PCFρ #:domain C
  (--> ((if0 M ...) ρ)
       (if0 (M ρ) ...)
       ρ-if)

  (--> ((M ...) ρ)
       ((M ρ) ...)
       ρ-app)

  (--> (O ρ)
       O
       ρ-op)

  (--> (N ρ)
       N
       ρ-num)

  (--> (X ρ) V
       (judgment-holds (lookup ρ X V))
       ρ-x)

  (--> (((λ ([X : T] ...) M) ρ) V ...)
       (M (ext ρ (X V) ...))
       β)

  (--> ((name f ((μ (X_f : T_f) (λ ([X : T] ...) M)) ρ)) V ...)
       (M (ext ρ (X_f f) (X V) ...))
       rec-β)

  (--> (O V ...) V_1
       (judgment-holds (δ (O V ...) V_1))
       δ)

  (--> (if0 0 C_1 C_2)
       C_1
       if-t)

  (--> (if0 N C_1 C_2)
       C_2
       (side-condition (not (equal? 0 (term N))))
       if-f)))

(define-metafunction PCFρ injρ
  :  M      -> C
  [ (injρ M)  (M ())])

(define -->vρ (context-closure vρ PCFρ E))

; (apply-reduction-relation* -->vρ (term (injρ fact-5)))


#| CONCEPT: TRANSITION SYSTEM |#

(define-extended-language PCFς PCFρ
  (F ::= (V ... [] C ...) (if0 [] C C))
  (K ::= (F ...))
  (S ::= ; serious terms S ∩ V = ∅, C = S ∪ V
     (N ρ)
     (O ρ)
     (X ρ)
     ((M M ...) ρ)
     ((if0 M M M) ρ)
     (if0 C C C)
     (C C ...))
  (ς ::= (C K) V))

; PICKUP up until compatible-closure vs context-closure
; ?? traces diff with prev
