;;; record types
;;;
;;; define-record-type
;;; make-record-type-descriptor

(load "../lib/out.ss")

(define-record-type point
    (fields (mutable x) (immutable y)))

; default procedure
(let ([p (make-point 36 -17)])
    (displayln p)
    (displayln (point? p))              ; #t
    (displayln (point? '(cons 36 -17))) ; #f
    (displayln (point-x p))             ; 36
    (point-x-set! p (- (point-x p) 12)) 
    (displayln (point-x p))             ; 24   
    (displayln (point-y p)))            ; -17
; override the defaults
(define-record-type (point2 mkpoint ispoint?)
    (fields (mutable x x-val set-x-val!)
        (immutable y y-val)))

(define (f p)
    ; create a new type each time it is evaludated
    (define-record-type point (fields x y))
    (if (eq? p 'make) (make-point 3 4) (point? p)))
(let ()
    (displayln (f (f 'make)))) ; #f
(define (f p)
    ; override default generative behavior
    (define-record-type point (fields x y) (nongenerative))
    (if (eq? p 'make) (make-point 3 4) (point? p)))
(let ()
    (displayln (f (f 'make)))) ; #t

; distinct record types
(define (f)
    (define-record-type point (fields x y) (nongenerative))
    (make-point 3 4))
(define (g p)
    (define-record-type point (fields x y) (nongenerative))
    (point? p))
(let ()
    (displayln (g (f)))) ; #f

; record types with uid: RFC 4122 UUID
(define (f)
    (define-record-type point (fields x y) 
        (nongenerative really-the-same-point))
    (make-point 3 4))
(define (g p)
    (define-record-type point (fields x y) 
        (nongenerative really-the-same-point))
    (point? p))
(let ()
    (displayln (g (f)))) ; #t

; subtype
(let ()
    (define-record-type point (fields x y))
    ; cpoint is subtype of point
    (define-record-type cpoint (parent point) (fields color))
    (let ([cp (make-cpoint 3 4 'red)])
        (displayln (point? cp))                 ; #t
        (displayln (cpoint? (make-point 3 4)))  ; #f
        (displayln (point-x cp))                ; 3
        ; cpoint-x is not bound
        ;(displayln (cpoint-x cp))
        (displayln (cpoint-color cp))))         ; red

; override constructor
(let ()
    (define-record-type point
        (fields x y d)
        (protocol
            (lambda (new) ; primitive constructor
                (lambda (x y)
                    (new x y (sqrt (+ (* x x) (* y y))))))))
    (define-record-type cpoint
        (parent point)
        (fields color)
        (protocol
            (lambda (pargs->new)
                (lambda (c x y)
                    ((pargs->new x y) c)))))
    (let ([p (make-point 3 4)]
        [cp (make-cpoint 'red 3 4)])
        (displayln (point-x p))     ; 3
        (displayln (point-y p))     ; 4
        (displayln (point-x cp))    ; 3
        (displayln (point-y cp))    ; 4
        (displayln (point-d cp))    ; 5
        (displayln (cpoint-color cp)))) ; red