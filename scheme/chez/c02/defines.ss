;;; Definitions

(load "../lib/tests.ss")

; (define var expr), if expr is a lambda expression:
; (1)
;(define var0
;   (lambda (var1 ... varn)
;       e1 e2 ...))
; =>
;(define (var0 var1 ... varn)
;   e1 e2 ...)
;
; (2)
;(define var0
;   (lambda varr
;       e1 e2 ...))
; =>
; (define (var0 . varr)
;   e1 e2 ...)
;
; (3)
;(define var0
;   (lambda (var1 ... varn . varr)
;       e1 e2 ...))
; =>
;(define (var0 var1 ... varn . varr)
;   e1 e2 ...)

(define doubler
    (lambda (f)
        (lambda (x) (f x x))))

(define double (doubler +))
(define double-cons (doubler cons))

(run-test double 13/2)
(run-test double-cons 'a)

;;; variables in lambda expression: proc2
(define proc1
    (lambda (x y)
        (proc2 y x)))

(define proc2 cons)

(run-test proc1 'a 'b)