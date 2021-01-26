
;;; (syntax-case expr (literal ...) clause ...)

;;; (syntax template)
;;; #'template
(define-syntax or2
    (lambda (x)
        (syntax-case x ()
            [(_) #'#f]
            [(_ e) #'e]
            [(_ e1 e2 e3 ...)
            #'(let ([t e1]) (if t t (or2 e2 e3 ...)))])))

; test
(let ([x 2])
    (display (or2 (not (= x 0)) (/ 1 x))) ; 1/2
    (newline))            

(define-syntax syntax-rules2
    (lambda (x)
        (syntax-case x ()
            [(_ (i ...) ((keyword . pattern) template) ...)
            #'(lambda (x)
                (syntax-case x (i ...)
                    [(_ . pattern) #'template] ...))])))

;;; (identifier? obj)
(define-syntax let2
    (lambda (x)
        (define ids?
            (lambda (lst)
                (or (null? lst)
                    (and (identifier? (car lst))
                        (ids? (cdr lst))))))
        (syntax-case x ()
            [(_ ((i e) ...) b1 b2 ...)
            (ids? #'(i ...)) ; fender
            #'((lambda (i ...) b1 b2 ...) e ...)])))

; test
(let2 ([x 1])
    (display x) ; 1
    (newline))

(let ([p (cons 0 #f)])
    (define-syntax pcar
        (lambda (x)
            (syntax-case x ()
                [_ (identifier? x) #'(car p)] ; fender: (identifier? x)
                [(_ e) #'(set-car! p e)])))
    (let ([a pcar]) ; 0
        (pcar 1) ; p: (1 #f)
        (display (list a pcar)) ; pcar: 1; result: (0 1)
        (newline)))

;;; (free-identifier=? identifier1 identifier2)
;;; (bound-identifier=? identifier1 identifier2)

(define-syntax cond2
    (lambda (x)
        (syntax-case x ()
            [(_ (e0 e1 e2 ...))
            (and (identifier? #'e0) (free-identifier=? #'e0 #'else))
            #'(begin e1 e2 ...)]
            [(_ (e0 e1 e2 ...)) #'(if e0 (begin e1 e2 ...))]
            [(_ (e0 e1 e2 ...) c1 c2 ...)
            #'(if e0 (begin e1 e2 ...) (cond2 c1 c2 ...))])))

(let ([x 2])
    (cond2 
        [(<= x 1) (display "not greater than 1")]
        [(<= x 2) (display "not greater than 2")] ; this case
        [else (display "greater than 2")])
    (newline))

(define-syntax let3
    (lambda (x)
        (define ids?
            (lambda (lst)
                (or (null? lst)
                    (and (identifier? (car lst)) (ids? (cdr lst))))))
        (define unique-ids?
            (lambda (lst)
                (or (null? lst)
                    (and (not (memp ; (rnrs lists)
                                (lambda (x) (bound-identifier=? x (car lst)))
                                (cdr lst)))
                        (unique-ids? (cdr lst))))))
        (syntax-case x ()
            [(_ ((i e) ...) b1 b2 ...)
            (and (ids? #'(i ...)) (unique-ids? #'(i ...)))
            #'((lambda (i ...) b1 b2 ...) e ...)])))

 ; test
(let3 ([x 2])
    (display x) ; 2
    (newline))           

;;; (with-syntax ((pattern expr) ...) body1 body2 ...)

(define-syntax with-syntax2
    (lambda (x)
        (syntax-case x ()
            [(_ ((p e) ...) b1 b2 ...)
            #'(syntax-case (list e ...) ()
            [(p ...) (let () b1 b2 ...)])])))

(define-syntax cond3
    (lambda (x)
        (syntax-case x ()
            [(_ c1 c2 ...)
            (let f ([c1 #'c1] [cmore #'(c2 ...)])
                (if (null? cmore)
                    (syntax-case c1 (else =>)
                        [(else e1 e2 ...) #'(begin e1 e2 ...)]
                        [(e0) #'(let ([t e0]) (if t t))]
                        [(e0 => e1) #'(let ([t e0]) (if t (e1 t)))]
                        [(e0 e1 e2 ...) #'(if e0 (begin e1 e2 ...))])
                    ; 局部模式绑定
                    (with-syntax ([rest (f (car cmore) (cdr cmore))])
                        (syntax-case c1 (=>)
                            [(e0) #'(let ([t e0]) (if t t rest))]
                            [(e0 => e1) #'(let ([t e0] (if t (e1 t) rest)))]
                            [(e0 e1 e2 ...)
                            #'(if e0 (begin e1 e2 ...) rest)]))))])))    


(let ([x 2])
    (cond3 
        [(<= x 1) (display "not greater than 1")]
        [(<= x 2) (display "not greater than 2")] ; this case
        [else (display "greater than 2")])
    (newline))                            
    
;;; (quasisyntax template ...)
;;; #`template
;;;
;;; (unsyntax template ...)
;;; #,template
;;;
;;; (unsyntax-splicing template ...)
;;; #,@template


;;; (make-variable-transformer procedure)


;;; (syntax->datum obj)
;;; (datum->syntax template-identifier obj)

;;; (generate-temporaries list)

