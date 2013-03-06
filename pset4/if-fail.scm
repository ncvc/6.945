
(define (require p)
  (if (not p) (amb)))

(define (distinct l)
  (cond ((null? l) true)
  ((null? (cdr l)) true)
  ((member (car l) (cdr l)) false)
  (else (distinct (cdr l)))))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(if-fail
  (let ((x (an-element-of '(1 3 5))))
    (require (even? x))
    x)
  'all-odd)

(if-fail
  (let ((x (an-element-of '(1 3 5 8))))
    (require (even? x))
    x)
  'all-odd)