; 1.4.a
(define (memo-wrapper-fib procname proc x)
  (let ((seen (assv x (eq-get proc 'old-values))))
    (if seen
      (cdr seen)
      (let ((old (eq-get proc 'old-values)) (v (proc x)))
        (eq-put! proc 'old-values 
          (if (eq? '() old)
            (list (cons x v))
            (list (cons x v) (car old))))
        (display (eq-get proc 'old-values))
        v))))

;Testing
; (eq-put! fib 'old-values '())
; (advise-unary fib memo-wrapper-fib)

; (show-time
;   (lambda ()
;   (fib 20)))


;1.4.b
(define (memo-wrapper-n-ary procname proc x)
  (let ((seen (assoc x (eq-get proc 'old-values))))
    (if seen
      (cdr seen)
  (let ((v (apply proc x)))
    (eq-put! proc 
       'old-values
       (cons (cons x v)
       (eq-get proc 'old-values)))
    v))))

;Testing
(define (test-proc x y)
  (cond
    ((<= x 0) y)
    ((<= y 0) x)
    (else
      (+ (test-proc (- x 1) y)
         (test-proc x (- y 1))))))

(define size 10)

(newline)
(display "No memo \n")
(display (test-proc size size))
(show-time
  (lambda ()
  (test-proc size size)))

(eq-put! test-proc 'old-values '())
(advise-n-ary test-proc memo-wrapper-n-ary)

(display "\nMemo \n")
(display (test-proc size size))
(show-time
  (lambda ()
  (test-proc size size)))
