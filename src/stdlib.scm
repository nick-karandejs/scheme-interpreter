(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
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

(define (foldr fun end lst)
  (if (null? lst)
    end
    (fun (car lst) (foldr fun end (cdr lst)))))

(define (foldl fun accum lst)
  (if (null? lst)
    accum
    (foldl fun (fun accum (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold fun init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold fun (fun init) pred))))

(define (sum . lst) (fold + 0 lst))

(define (product . lst) (fold * 1 lst))

(define (and . lst) (fold && #t lst))

(define (or . lst) (fold || #f lst))

(define (max first . rest) (fold (lambda (p n) (if (> n p) n p)) first rest))
(define (min first . rest) (fold (lambda (p n) (if (< n p) n p)) first rest))

(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))

(define (copy lst) (foldr cons '() lst))

(define (reverse lst) (fold (flip cons) '() lst))

(define (mem-helper pred op)
  (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))

(define (member obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (assoc obj alist) (fold (mem-helper (curry eqv? obj) car) #f lst))

(define (map fun lst) (foldr (lambda (x acc) (cons (fun x) acc)) '() lst))

(define (filter pred lst)
  (foldr (lambda (x acc) (if (pred x) (cons x acc) acc)) '() lst))
