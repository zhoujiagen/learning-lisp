;;; Syntactic Extension

(load "lib/tests.ss")

;;; (1) Keyword Bindins

;;; (2) Transformer: syntax-rules

(define-syntax and2
;(trace-define-syntax and2
    (syntax-rules ()
        [(_) #t]
        [(_ e) e]
        ; recursive
        [(_ e1 e2 e3 ...)
        (if e1 (and2 e2 e3 ...) #f)]))

; test
(let ([x 2])
    (display (and2 (not (= x 0)) (/ 1 x)))
    (newline))

(define-syntax or2
;(trace-define-syntax or2
    (syntax-rules ()
        [(_) #f]
        [(_ e) e]
        [(_ e1 e2 e3 ...)
        (let ([t e1]) (if t t (or e2 e3 ...)))]))

; test
(let ([x 2])
    (display (or2 (not (= x 0)) (/ 1 x)))
    (newline))

;;; (3) Transformer: syntax-case


(define-syntax or3
    (lambda (x)
        (syntax-case x ()
            [(_) #'#f]
            [(_ e) #'e]
            [(_ e1 e2 e3 ...)
            #'(let ([t e1]) (if t t (or e2 e3 ...)))])))

; test
(let ([x 2])
    (display (or3 (not (= x 0)) (/ 1 x)))
    (newline))

; syntax-case: a generalized version of syntax-rules
(define-syntax syntax-rules2
;(trace-define-syntax syntax-rules2
    (lambda (x)
        (syntax-case x ()
            [(_ (i ...) ((keyword . pattern) template) ...)
            #'(lambda (x)
                (syntax-case x (i ...)
                    [(_ . pattern) #'template] ...))])))
; test
(define-syntax or4
    (syntax-rules2 ()
        [(_) #f]
        [(_ e) e]
        [(_ e1 e2 e3 ...)
        (let ([t e1]) (if t t (or e2 e3 ...)))]))
(let ([x 2])
    (display (or4 (not (= x 0)) (/ 1 x)))
    (newline))

