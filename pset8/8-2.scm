#|
8.2.A
The procedure would not use lazy evaluation at all - lazy-fringe would just traverse the entire tree on the first call and return a stream of these elements.
|#

;;; 8.2.B
(define (lazy-fringe subtree)
  (define (walk subtree ans)
    (cond
      ((pair? subtree)
        (walk (car subtree)
              (lambda () (walk (cdr subtree) ans))))
      ((null? subtree)
        (ans))
      (else
        (cons-stream subtree (ans)))))
  (walk subtree (lambda () the-empty-stream)))
