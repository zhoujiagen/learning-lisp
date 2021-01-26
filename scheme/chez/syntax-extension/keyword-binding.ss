
;;; (define-syntax keyword expr)
(define-syntax let*2
    (syntax-rules ()
        [(_ () b1 b2 ...) (let () b1 b2 ...)]
        [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
        (let ([i1 e1])
            (let*2 ([i2 e2] ...) b1 b2 ...))]))

;;; 内部定义的绑定(关键字或变量定义)在立即包裹体内可见
(let ()
    (define even?
        (lambda (x)
            (or (= x 0) (odd? (- x 1)))))
    (define-syntax odd?
        (syntax-rules ()
            [(_ x) (not (even? x))]))
    (display (even? 10))
    (newline))

(let ()
    (define-syntax bind-to-zero
        (syntax-rules ()
            ; 变量定义
            [(_ id) (define id 0)]))
    (bind-to-zero x)
    (display x) ; 0
    (newline))

;;; (let-syntex ((keyword expr) ...) form1 form2 ...)
;;; (letrec-syntax ((keyword expr) ...) form1 form2 ...)

(let ([f (lambda (x) (+ x 1))])
    (let-syntax ([f (syntax-rules ()
                        [(_ x) x])]
                [g (syntax-rules ()
                        [(_ x) (f x)])]) ; f: let
        (display (list (f 1) (g 1))) ; (1 2)
        (newline)))

(let ([f (lambda (x) (+ x 1))])
    (letrec-syntax ([f (syntax-rules ()
                        [(_ x) x])]
                [g (syntax-rules ()
                        [(_ x) (f x)])]) ; f: letrec-syntax
        (display (list (f 1) (g 1))) ; (1 1)
        (newline)))