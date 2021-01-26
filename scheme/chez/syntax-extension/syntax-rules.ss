;;; (syntax-rules (literal ...) clause ...)

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
    (display (and2 (not (= x 0)) (/ 1 x))) ; 1/2
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
    (display (or2 (not (= x 0)) (/ 1 x))) ; #t
    (newline))

(define-syntax cond2
    (syntax-rules (else)
        [(_ (else e1 e2 ...)) (begin e1 e2 ...)]
        [(_ (e0 e1 e2 ...)) (if e0 (begin e1 e2 ...))]
        [(_ (e0 e1 e2 ...) c1 c2 ...)
        (if e0 (begin e1 e2 ...) (cond2 c1 c2 ...))]))

; test
(let ([x 2])
    (cond2 
        [(<= x 1) (display "not greater than 1")]
        [(<= x 2) (display "not greater than 2")] ; this case
        [else (display "greater than 2")])
    (newline))

;;; (identifier-syntax tmpl)
;;; (identifier-syntax (id1 tmpl1) ((set! id2 e2) tmpl2))    

(let ()
    (define-syntax a (identifier-syntax car))
    (display (list (a '(1 2 3)) a)) ; (1 #<procedure car>)
    (newline))

(let ([lst (list 0)])
    (define-syntax a
        (identifier-syntax
            [id (car lst)]
            [(set! id e) (set-car! lst e)]))
    (let ([before a])
        (set! a 1)
        (display (list before a lst)) ; (0 1 (1))
        (newline)))