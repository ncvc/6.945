(define-record-type future-record
  (make-future-record cont done callbacks)
  future?
  (cont      future-cont      set-future-cont!)
  (done      future-done      set-future-done!)
  (val       future-val       set-future-val!)
  (callbacks future-callbacks set-future-callbacks!))

#|
;; The original implementation did not use a list of future callbacks, opting instead for a single future callback per record. This made it so that only the most recent wait callback would be called until the future was finished.
(define (future procedure)
  (let ((record (make-future-record procedure #f '())))
    (define (new-callback val)
      (set-future-val! record val)
      (set-future-done! record #t)
      (for-each (lambda (callback) (callback)) (future-callbacks record)))

    (define do-it (alpha ()
      ((future-cont record) new-callback)))
    (do-it)
    record))


(define (wait future callback)
  (define (do-callback)
    (callback (future-val future)))

  (atomically (lambda()
    (if (future-done future)
      (do-callback)
      (set-future-callbacks! future (cons do-callback (future-callbacks future))))))))


(define fib
  (alpha (n c)
    (if (< n 2)
      (c n)
      (let ((xp (future (lambda (k) (fib (- n 1) k))))
            (yp (future (lambda (k) (fib (- n 2) k)))))
        (wait xp 
          (lambda (x)
            (wait yp
              (lambda (y)
                (c (+ x y))))))))))


(define (test-multi-wait)
  (let ((x (future (lambda (k) (k 1)))))
    (wait x (lambda (val) (pp val)))
    (wait x (lambda (val) (pp val)))
    (wait x (lambda (val) (pp val)))))
|#
