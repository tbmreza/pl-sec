#lang racket

(provide (all-defined-out))
(require rackunit)

#| interpreting:  lambda calculus
   using:         CESK* machine (tick/alloc: [ ] none [X] functions [ ] stacks)
                  fig.4 of Abstracting Abstract Machines paper |#

(define variable-name? string?)
(define church-encoded? list?)
(struct app (e0 e1))  ; would use cons if `(cons? (list 1 1 1))` weren't true

(struct clo (lam env))
(define (eval-clo lam-env)
  ; invariant: lambda has no non-local variables
  (clo-lam lam-env))

(struct ar (e r a) #:transparent)
(struct fn (l r a) #:transparent)

(struct S (expr env store addr time) #:transparent)
(define default-z 0)
(define default-env (hash))
(define default-store (hash default-z empty))

(define (inj e)
  (S e default-env default-store (addr default-z) (time default-z)))
(define (inj-with r σ e)
  (S e r σ (addr default-z) (time default-z)))


; CHURCH ENCODING

(define (church-encoded->procedure data) (eval data (make-base-namespace)))

(define ident `(lambda (x) x))
(define c2 `(lambda (f) (lambda (x) (f (f x)))))

(define (c->number c)
  (((church-encoded->procedure c) add1) 0))
(check-eq? (c->number c2) 2)

(define (cnat n)
  (define (h n acc)
    (match n
      [0  acc]
      [_  `(f ,(h (sub1 n) acc))]))
  `(lambda (f) (lambda (x) ,(h n 'x))))


; THE MACHINE

(define (lookup m k)
  (hash-ref m k #f))

(define (extend m k v)
  (hash-set m k v))

(struct time (z) #:transparent)
(struct addr (z) #:transparent)

(define (incr-time time-struct)
  (time (add1 (time-z time-struct))))
(define (tick state)
  (incr-time (S-time state)))
(define (alloc state)
  (addr (S-time state)))

(struct decapped (var body) #:transparent)
(define (decap lxe)
  (match lxe
    [`(lambda (,x) ,e)
      (define xe-string-pair (map (lambda (v) (format "~a" v))
                                  (list x e)))
      (apply decapped xe-string-pair)]))

(define (step s)
  (define state (match s
    [(S (? variable-name? e) r σ a t)  ; alternative tick/1 implementation takes t instead of s
     (let* ([e-addr  (lookup r e)]
            [d       (lookup σ e-addr)]
            [u       (tick s)])
       (S (clo-lam d) (clo-env d) σ a u))]

    [(S (? app? e) r σ a t)
     (let* ([e0  (app-e0 e)]
            [e1  (app-e1 e)]
            [b   (alloc s)]
            [k   (ar e1 r a)]
            [σ+  (extend σ b k)]
            [u   (tick s)])
       (S e0 r σ+ b u))]

    [(S v r σ a t)
     (match (lookup σ a)
       [(ar e r+ c)
        (let* ([b   (alloc s)]
               [k   (fn v r c)]
               [σ+  (extend σ b k)]
               [u   (tick s)])
          (S e r+ σ+ b u))]

       [(fn l r+ c)
        (let* ([xe   (decap l)]
               [x    (decapped-var xe)]
               [e    (decapped-body xe)]
               [b    (alloc s)]
               [r++  (extend r+ x b)]
               [σ+   (extend σ b (clo v r))]
               [u   (tick s)])
          (S e r++ σ+ c u))])]
    
    ))
  ; (displayln "\nstate:")(displayln state)
  state)

(define (until p f)
  (define (go x)
    (match x
      [(? p x)  x]
      [_        (go (f x))]))
  go)

(define (final? s)
  (match s
    [(S e _r σ a _t)
     (let* ([interpretable?  (not (app? e))]
            [store-a         (lookup σ a)]
            [mt?             (or (not store-a) (empty? store-a))])
       (and interpretable? mt?))])
  )

(define run (until final? step))

(define (interpret-control-str state)
  (match state
    [(S (? variable-name? var) r σ _a _t) #:when (hash-has-key? r var)
     (let* ([var-addr   (lookup r var)]
            [var-d      (lookup σ var-addr)]
            [var-value  (eval-clo var-d)])
       (c->number var-value))]

    [(S (? church-encoded? e) _r _σ _a _t)
     (c->number e)]

    [(S e _r _σ _a _t)
     (format "~a" e)]))


; -- TESTING

(check-equal?
  (let* ([start  (inj "not_in_env")]
         [final  (run start)])
    (interpret-control-str final))
  "not_in_env")

(check-equal?
  (let* ([r      (hash "x" 1001)]
         [σ      (hash 1001 (clo (cnat 2) default-env))]
         [final  (run (inj-with r σ "x"))]
         )
    (interpret-control-str final))
  2)

(check-eq?
  (let* ([start  (inj (app ident (cnat 11)))]
         [final  (run start)])
    (interpret-control-str final))
  11)

(check-eq?
  (let* ([start  (inj (app ident (app ident (app ident (cnat 21)))))]
         [final  (run start)])
    (interpret-control-str final))
  21)
