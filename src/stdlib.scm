(define (not x) (if x #f #t))
(define (null? obj) (if (eqv obj '()) #t #f))
(define (list . objs) objs)
(define (id obj) obj)
