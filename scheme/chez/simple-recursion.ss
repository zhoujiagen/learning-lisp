;;; Simple Recursion

(load "lib/tests.ss")
(load "lib/math.ss")

(define length
    (lambda (lst)
        (if (null? lst)
            0
            (+ (length (cdr lst)) 1))))

(run-tests length '('() '(a) '(a b)))

(define list-copy
    (lambda (lst)
        (if (null? lst)
            '()
            (cons (car lst) (list-copy (cdr lst))))))

(run-tests length '('() '(a b c)))

(define memv
    (lambda (x lst)
        (cond 
        [(null? lst) #f]
        [(eqv? (car lst) x) lst]
        [else (memv x (cdr lst))])))

(let ([lst '(a b b d)])
    (run-test memv 'a lst)
    (run-test memv 'b lst)
    (run-test memv 'c lst))

(define remv
    (lambda (x lst)
        (cond
            [(null? lst) '()]
            [(eqv? (car lst) x) (remv x (cdr lst))]
            [else (cons (car lst) (remv x (cdr lst)))])))

; test
(let ([lst '(a b b d)])
    (run-test remv 'a lst)
    (run-test remv 'b lst)
    (run-test remv 'c lst))

(define tree-copy
    (lambda (tr)
        (if (not (pair? tr))
            tr
            (cons (tree-copy (car tr))
                (tree-copy (cdr tr))))))

(run-test tree-copy '((a . b) . c))

(define map1
    (lambda (p lst)
        (if (null? lst)
            '()
            (cons (p (car lst))
                (map1 p (cdr lst))))))

(run-test map1 abs '(1 -2 3 -4 5 -6) )