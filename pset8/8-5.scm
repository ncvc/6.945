(define (make-pipe)
  (list (conspire:make-lock) (queue:make)))


(define (pipe-reader pipe)
  (let ((lock (first pipe)) (queue (second pipe)))
    (lambda ()
      (conspire:switch-threads
        (lambda ()
          (not (queue:empty? queue))))

      (conspire:acquire-lock lock)
      (let ((item (queue:get-first queue)))
        (queue:delete-from-queue! queue item)
        (conspire:unlock lock)
        item))))


(define (pipe-writer pipe)
  (let ((lock (first pipe)) (queue (second pipe)))
    (lambda (item)
      (conspire:acquire-lock lock)
      (queue:add-to-end! queue item)
      (conspire:unlock lock))))
