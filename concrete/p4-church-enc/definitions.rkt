#lang racket

(provide (all-defined-out))

(define (evalnew data)
  (eval data (make-base-namespace)))

(define (literal? v)
  (or (number? v) (member v `(#t #f '()))))

(define/contract (cnat n)
  (-> number? procedure?)
  (define (h n acc)
    (match n
      [0 acc]
      [_ `(f ,(h (sub1 n) acc))]))
  (evalnew `(lambda (f) (lambda (x) ,(h n 'x)))))

(define (unary? prim) (member prim '(add1 sub1 null? zero? not car cdr)))
(define (binary? prim) (member prim '(+ - * = cons)))

(define U (lambda (x) (x x)))
(define Y (lambda (g) ((lambda (f) (g (lambda (x) ((f f) x)))) (lambda (f) (g (lambda (x) ((f f) x)))))))

(define (SUCC cn)
  (lambda (f)
    (lambda (x) (f ((cn f) x)))))

(define (PLUS cn)
  (lambda (ck)
    (lambda (f)
      (lambda (x) ((cn f) ((ck f) x))))))

(define TRUE   (lambda (a) (lambda (_) a)))
(define FALSE  (lambda (_) (lambda (b) b)))

; One intuition (immediate observation of a simpler definition, not the one implemented below. The implementation below is called "wisdom tooth trick" for some intuition I can't work out yet.)
; of PRED is we start with a pair of zeros ((CONS c0) c0).
;
; If we input c0 (a lambda that *ignores* its outer-most parameter f), we take the first of our pair.
;
; If we input c1 (a lambda that applies f *once*), we take the second of previous step c0 as our new first; our new second succ of that new first;
;   and finally return the first of our pair.
;
; We increase the number of swapping-incrementing dance steps as we increment our input. (PRED k+1) leads (PRED k) by one because we do the dance one more time, by definition of church numerals.
(define PAIR  (λ (a) (λ (b) (λ (s) ((s a) b)))))
(define FST   (lambda (p) (p (lambda (a) (lambda (_) a)))))
(define SND   (lambda (p) (p (lambda (_) (lambda (b) b)))))
(define T     (λ (p) ((PAIR (SUCC (FST p))) (FST p))))
(define PRED  (λ (n) (SND ((n T) ((PAIR c0) c0)))))

(define MINUS (lambda (m) (lambda (n) ((n PRED) m))))

(define (MUL cn)
  (lambda (ck)
    (lambda (f)
      (lambda (x) ((cn (ck f)) x)))))

(define (POW cn)
  (lambda (ck)
    (lambda (f)
      (lambda (x) (((cn ck) f) x)))))

(define NOT    (λ (b) ((b FALSE) TRUE)))
(define ZERO?  (lambda (n)
                 ((n (lambda (_) FALSE)) TRUE)))
(define LEQ?   (lambda (m) (lambda (n) (ZERO? ((MINUS m) n)))))
(define AND    (lambda (p) (lambda (q) ((p q) p))))
(define EQ?    (lambda (m) (lambda (n) ((AND ((LEQ? m) n)) ((LEQ? n) m)))))

(define NULL? (λ (lst) ((lst (λ (_) (λ (_) FALSE))) (λ (_) TRUE))))

(define CONS (lambda (head)
               (lambda (tail)
                 (lambda (when-cons)
                   (lambda (_) ((when-cons head) tail))))))

(define VOID  (λ (void) void))
(define NIL   (λ (when-cons) (λ (when-nil) (when-nil VOID))))

(define ERROR (λ (_)
                ((λ (f) (f f)) (λ (f) (f f)))))

(define CAR (λ (lst)
              ((lst (λ (car) (λ (_cdr) car)))
               ERROR)))

(define CDR (λ (lst)
              ((lst (λ (_car) (λ (cdr) cdr)))
               ERROR)))

(define c0 (lambda (_) (lambda (x) x)))
(define c1 (lambda (f) (lambda (x) (f x))))
(define c2 (lambda (f) (lambda (x) (f (f x)))))
(define c3 (lambda (f) (lambda (x) (f (f (f x))))))
(define c4 (lambda (f) (lambda (x) (f (f (f (f x)))))))
(define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))
(define c7   ((PLUS c2) c5))
(define c9   ((PLUS c2) c7))
(define c10  ((MUL c2) c5))
(define c100 ((MUL ((MUL c2) c2)) ((MUL c5) c5)))
