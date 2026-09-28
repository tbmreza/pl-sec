#lang racket

(require "definitions.rkt" "church-compile.rkt" rackunit)

(check-true ((church->bool TRUE) '()))
(check-eq? 100 (church->nat c100))
(check-equal? (church->nat (cnat 3)) (church->nat (lambda (f) (lambda (x) (f (f (f x)))))))
(check-equal? (church->nat (SUCC (lambda (_) (lambda (x) x)))) (church->nat (lambda (f) (lambda (x) (f x)))))
(check-equal? (church->nat ((PLUS c2) c3)) 5)
(check-equal? (church->nat ((MUL c3) c3)) 9)
(check-equal? (church->nat ((POW c3) c2)) 8)
(check-equal? ((church->listof church->nat) (CAR ((CONS ((CONS c2) ((CONS c3) NIL))) NIL))) '(2 3))
(check-eq? (church->nat ((MINUS c4) c3)) 1)
(check-eq? 8 (church->nat (church-compile `(let ([b 2][* ,MUL]) (* b (* b 2))))))
(check-eq? 6 (church->nat (church-compile `(* 2 3))))
(check-eq? 2 (church->nat (church-compile `(if #t 2 3))))
(check-eq? 3 (church->nat (church-compile `(if #f 2 3))))
(check-eq? 4 (church->nat (church-compile `(let () (if #t 4 3)))))

; let* can invoke in els a lambda defined in previous square bracket.
(check-eq? 7 (church->nat (church-compile
  `(let* ([len  (lambda (ignore) 7)]
          [f    (lambda (x) (if #t (len x) 0))])
     (f 3)))))

; binary lambda body
(check-eq? 8 (church->nat (church-compile
  `(let* ([f0   (lambda (passed) (* 2 passed))]
          [f    (lambda (x) (if (zero? 9) 0 (f0 x)))])
     (f 4)))))

; triple curried if stmt
(check-eq? 4 (church->nat (church-compile
  `(let ([f (lambda (b) (lambda (thn) (lambda (els) (if b thn els))))])
     (((f #t) 4) 2)))))
(check-eq? 2 (church->nat (church-compile
  `(let ([f (lambda (b) (lambda (thn) (lambda (els) (if b thn els))))])
     (((f #f) 4) 2)))))

(check-eq? 4 (church->nat (church-compile
  `(let* ([fn  (lambda (j) (if (= j 3) 4 5))])
     (fn 3)))))
(check-eq? 5 (church->nat (church-compile
  `(let* ([fn  (lambda (j) (if (= j 3) 4 5))])
     (fn 0)))))

; lambda whose body is binary appl
(check-true ((church->bool (church-compile
  `(let* ([fn  (lambda (j) (= j 3))])
     (fn 3)))) (void)))
(check-false ((church->bool (church-compile
  `(let* ([fn  (lambda (j) (= j 3))])
     (fn 1)))) (void)))

; nested binop
(check-eq? 140 (church->nat (church-compile '(let ([f (lambda (a b c) (+ a (+ b c)))]) (f (f 0 0 5) (f 10 15 20) (f 25 30 35))))))
(check-eq? 6 (church->nat (church-compile '(let ([f (lambda (a b c) (+ a (+ b c)))]) (f 1 2 3)))))
(check-eq? 5 (church->nat (church-compile '(let ([f (lambda (b c) (+ b c))]) (f 2 3)))))

; lambda body of if
(check-eq? 1 (church->nat (church-compile '(let ([fn (lambda (a) (if (zero? a) 1 2))])  (fn 0)))))
(check-eq? 2 (church->nat (church-compile '(let ([fn (lambda (a) (if (zero? a) 1 2))])  (fn 9)))))

(check-eq? 7 (church->nat (church-compile '(let ([fn 7])  fn))))
(check-eq? 2 (church->nat (church-compile '(let ([fn (lambda (a) a)])  (fn 2)))))
(check-eq? 3 (church->nat (church-compile '(let ([v 3])  (if #t v 0)))))
(check-eq? 2 (church->nat (church-compile '(let ([fn (lambda (a) (if #t a 0))])  (fn 2)))))

(check-eq? 4 (church->nat (church-compile
  '(let* ([important 4][fn (lambda (a) important)])  (fn 2)))))

(check-eq? 3 (church->nat (church-compile '(let* ([x 3][y x])  y))))
(check-eq? 3 (church->nat (church-compile '(let* ([x 3][y x])  (if #t y 0)))))
(check-eq? 4 (church->nat (church-compile '(let* ([x 4][y 5])  (if (zero? 0) x y)))))
(check-eq? 5 (church->nat (church-compile '(let* ([x 4][y 5])  (if (zero? 9) x y)))))

; omega-safe if
(check-eq? 5 (church->nat (church-compile '(if (not #t) 3 (let ([U (lambda (u) (u u))]) 5)))))
(check-true ((church->bool ((church-compile '(lambda () #t)))) (void)))
(check-eq? 3 (church->nat (church-compile '(if (not #f) 3 (let ([U (lambda (u) (u u))]) (U U))))))
(check-eq? 6 (church->nat (church-compile '(if #t (let ([U (lambda (u) (u u))]) 6) 3))))

(check-eq? 720 (church->nat (church-compile
  `(let* ([U (lambda (u) (u u))]
          [fact (U (lambda (mk-fact)
                     (lambda (n)
                       (if (zero? n)
                           1
                           (* n ((U mk-fact) (- n 1)))))))])
     (fact 6)))))

(check-eq? 6 (church->nat (church-compile
  '(letrec ([len (lambda (lst)
                   (if (null? lst)
                       0
                       (add1 (len (cdr lst)))))])
     (len (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons '() '())))))))))))

(check-eq? 6 (church->nat (church-compile
      `(let ([lst (cons '() (cons 3 '()))])
         (if (null? (car lst))
             (* 2 (car (cdr lst)))
             7)))))

(check-eq? 6 (church->nat (church-compile
      `(if #t 6 0))))

(check-eq? 6 (church->nat (church-compile
      `(if #t (let () 6) 0))))

(check-eq? 6 (church->nat (church-compile
      `(if #t (let ([lst (cons '() (cons 3 '()))])
         (if (null? (car lst))
             (* 2 (car (cdr lst)))
             7)) 0))))

(check-eq? 1 (church->nat (church-compile
      `(if #t (let () (if #t 1 2)) 3))))
