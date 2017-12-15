(define (not x) (if x #f #t))
(define (null? obj) (if (eqv obj '()) #t #f))
(define (list . objs) objs)
(define (id obj) obj)

(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g arg))))

(define zero? (curry = 0))
(define positive? (curry > 0))
(define negative? (curry < 0))
(define (odd? num) (= 1 (mod num 2)))
(define (even? num) (= 0 (mod num 2)))
