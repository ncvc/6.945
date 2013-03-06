; 1.1.a
(define-syntax remove-advice-improved
  (syntax-rules ()
    ((remove-advice p)
     (begin (let ((old (eq-get p 'old-version)))
            (set! p
              (if (eq? old #f)
                p
                old)))
      'done))))

(define-syntax advise-unary-improved
  (syntax-rules ()
    ((advise-unary p wrapper)
     (define p
       (if (procedure-of-arity? p 1)
        (let ((saved-p p)
         (ewrapper wrapper))  ;Why rename wrapper? Think macro!
         (let ((new-p 
          (named-lambda (p x)
            (ewrapper 'p saved-p x))))
           (eq-put! new-p 'old-version saved-p)
           new-p))
        (begin
          (display "Non-unary procedure! Nothing advised.")
          p)
          )))))


; 1.1.b
(define-syntax advise-n-ary
  (syntax-rules ()
    ((advise-unary p wrapper)
     (define p
       (let ((saved-p p)
       (ewrapper wrapper))  ;Why rename wrapper? Think macro!
   (let ((new-p 
    (named-lambda (p . x)
      (ewrapper 'p saved-p x))))
     (eq-put! new-p 'old-version saved-p)
     new-p))))))

;Test code

; (display "\n1+1 = ")
; (display (+ 1 1))

; (display "\nadvise-n-ary +")

; (define plus-wrapper
;   (lambda (name oldf nums)
;      (if (eq? (car nums) 0)
;       (error "I hate zeroes" name)
;       (apply oldf nums))))

; (advise-n-ary + plus-wrapper)

; (display "\n1+1 = ")
; (display (+ 1 1))

; (display "\n2+1 = ")
; (display (+ 2 1))

; (display "\n0+1 = ")
; (display (+ 0 1))
