
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

(define count 0)
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))