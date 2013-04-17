(define (make-threaded-filter generator)
  (let ((pipe (make-pipe)))
    (conspire:make-thread
      conspire:runnable
      (lambda ()
        (generator (pipe-writer pipe))))
    (pipe-reader pipe)))
