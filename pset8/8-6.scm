(define (make-threaded-filter generator)
  (let ((pipe (make-pipe)))
    (conspire:make-thread
      conspire:runnable
      (lambda ()
        (generator (pipe-writer pipe))))
    (pipe-reader pipe)))


#|
;;; Sample output

1 ]=> (with-time-sharing-conspiracy
      (lambda ()
        (tf-piped-same-fringe?
         '((a b) c ((d)) e (f ((g h))))
         '(a b c ((d) () e) (g (f ))))
        ))

;Value: #f

1 ]=> (with-time-sharing-conspiracy
      (lambda ()
        (tf-piped-same-fringe?
         '((a b) c ((d)) e (f ((g h))))
         '(a b c ((d) () e) (g (f (h)))))
        ))

;Value: #f

1 ]=> (with-time-sharing-conspiracy
      (lambda ()
        (tf-piped-same-fringe?
         '((a b) c ((d)) e (f ((g h))))
         '(a b c ((d) () e) (f (g (h)))))
        ))

;Value: #t
|#
