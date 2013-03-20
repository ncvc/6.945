;;; 6.2

;; A
(define (snark-hunt tree)
  (define (snark-hunt-helper tree success)
    (if (symbol? tree)
      (if (eq? tree 'snark)
        (success #t)
        #f)
      (begin
        (for-each
          (lambda (subtree)
            (snark-hunt-helper subtree success))
          tree)
        #f)))
  (call-with-current-continuation (lambda (success) (snark-hunt-helper tree success))))

#|
Sample output:
1 ]=> (snark-hunt '(((a b c) d (e f)) g (((snark . "oops") h) (i . j))))
;Value: #t

1 ]=> (snark-hunt '(((a b c) d (e f)) g (((snarks 'fixed) h) (i j))))
;Value: #f

1 ]=> (snark-hunt '(((a b c) d (e f)) g (((snarks . "oops") h) (i . j))))
;The object (snarks . "oops"), passed as an argument to for-each, is not a list.
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
|#


;; B
(define (snark-hunt/instrumented tree)
  (define (snark-hunt-helper tree success)
    (pp "Called snark-hunt-helper")
    (if (symbol? tree)
      (if (eq? tree 'snark)
        (begin
          (pp "SUCCESS!")
          (success #t))
        #f)
      (begin
        (for-each
          (lambda (subtree)
            (snark-hunt-helper subtree success))
          tree)
        #f)))
  (call-with-current-continuation (lambda (success) (snark-hunt-helper tree success))))

#|
Strategy:
Simply print "SUCCESS!" each time the success continuation is called, and "Called snark-hunt-helper" when entering snark-hunt-helper. Since the only return values are either false or through the success continuation, if "SUCCESS!" is printed only once before returning #t, we know that only the original continuation was called on success.

Sample output:
1 ]=> (snark-hunt/instrumented '(((a b c) d (e f)) g (((snark . "oops") h) (i . j))))
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"Called snark-hunt-helper"
"SUCCESS!"
;Value: #t
|#
