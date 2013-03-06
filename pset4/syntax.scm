;;; -*- Mode:Scheme -*- 

(declare (usual-integrations))

;;; Self-evaluating entities

(define (self-evaluating? exp)
  (or (number? exp)
      (eq? exp #t)
      (eq? exp #f)
      (string? exp)))	; Our prompt (viz., "EVAL==> ") is a string.

;;; Variables

(define (variable? exp) (symbol? exp))

(define (same-variable? var1 var2) (eq? var1 var2))  ;; Nice abstraction

;;; Special forms (in general)

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

;;; Quotations

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation quot) (cadr quot))

;;; Assignment--- SET!

(define (assignment? exp) (tagged-list? exp 'set!))
(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))

(define (assignment-variable assn) (cadr  assn))
(define (assignment-value    assn) (caddr assn))

;;; Definitions

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable defn)
  (if (variable? (cadr defn))			;;   (DEFINE  foo      ...)
      (cadr  defn)
      (caadr defn)))				;;   (DEFINE (foo ...) ...)

(define (definition-value defn)
  (if (variable? (cadr defn))			;;   (DEFINE  foo        ...)
      (caddr defn)
      (cons 'lambda				;;   (DEFINE (foo p...) b...)
            (cons (cdadr defn)			;; = (DEFINE  foo
                  (cddr  defn)))))		;;     (LAMBDA (p...) b...))

;;; LAMBDA expressions

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp)
  (let ((full-body (cddr lambda-exp)))
    (sequence->begin full-body)))


(define declaration? pair?)

(define (parameter-name var-decl)
  (if (pair? var-decl)
      (car var-decl)
      var-decl))

(define (lazy? var-decl)
  (and (pair? var-decl)
       (memq 'lazy (cdr var-decl))
       (not (memq 'memo (cdr var-decl)))))

(define (lazy-memo? var-decl)
  (and (pair? var-decl)
       (memq 'lazy (cdr var-decl))
       (memq 'memo (cdr var-decl))))

(define (sequence->begin seq)
  (cond ((null? seq) seq)
	((null? (cdr seq)) (car seq))
	((begin? (car seq)) seq)
	(else (make-begin seq))))

(define (make-begin exp) (cons 'begin exp))

;;; If conditionals

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'the-unspecified-value))

(define (make-if pred conseq alternative)
  (list 'IF pred conseq alternative))


;;; COND Conditionals

(define (cond? exp) (tagged-list? exp 'cond))

(define (clauses cndl) (cdr cndl))
(define (no-clauses? clauses) (null? clauses))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))
(define (else-clause? clause) (eq? (predicate clause) 'else))

(define (predicate clause) (car clause))

(define (actions clause)
  (sequence->begin (cdr clause)))

(define (cond->if cond-exp)
  (define (expand clauses)
    (cond ((no-clauses? clauses)
	   (list 'error "COND: no values matched"))
	  ((else-clause? (car clauses))
	   (if (no-clauses? (cdr clauses))
	       (actions (car clauses))
	       (error "else clause isn't last -- INTERP" exp)))
	  (else
	   (make-if (predicate (car clauses))
		    (actions (car clauses))
		    (expand (cdr clauses))))))
  (expand (clauses cond-exp)))


;;; BEGIN expressions (a.k.a. sequences)

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define no-more-exps? null?)		; for non-tail-recursive vers.

;;; LET expressions

(define (let? exp) (tagged-list? exp 'let))
(define (let-bound-variables let-exp)
  (map car (cadr let-exp)))
(define (let-values let-exp) (map cadr (cadr let-exp)))
(define (let-body let-exp) (sequence->begin (cddr let-exp)))
(define (let->combination let-exp)
  (let ((names (let-bound-variables let-exp))
  (values (let-values let-exp))
  (body (let-body let-exp)))
    (cons (list 'LAMBDA names body)
    values)))

;;; INFIX expressions

(define (infix? exp) (tagged-list? exp 'infix))
; (define (let-bound-variables let-exp)
;   (map car (cadr let-exp)))
; (define (let-values let-exp) (map cadr (cadr let-exp)))
; (define (let-body let-exp) (sequence->begin (cddr let-exp)))
(define (infix-get-parens exp i first-paren open-parens)
  (cond
    ((>= i (string-length exp))
      -1)
    ((char=? (string-ref exp i) #\()
      (infix-get-parens exp (+ i 1) (if (eq? first-paren -1) i first-paren) (+ open-parens 1)))
    ((char=? (string-ref exp i) #\))
      (if (eq? open-parens 1)
        (list first-paren i)
        (infix-get-parens exp (+ i 1) first-paren (- open-parens 1))))
    (else
      (infix-get-parens exp (+ i 1) first-paren open-parens))))

(define (infix-str->scheme infix-str)
  (let ((number (string->number infix-str))
        (parens (infix-get-parens infix-str 0 -1 0))
        (addition (string-search-forward "+" infix-str))
        (subtraction (string-search-forward "-" infix-str))
        (multiplication (string-search-forward "*" infix-str))
        (division (string-search-forward "/" infix-str))
        (exponentiation (string-search-forward "^" infix-str))
        (squareroot (string-search-forward "sqrt" infix-str)))
    (newline)
    (display infix-str)
    (newline)
    (display parens)
    (cond
      ((not (eq? number #f))
        (display "num")
        number)
      ((list? parens)
        (display "parens")
        (let ((i (car parens)) (j (cadr parens)))
          (list
            (infix-str->scheme (string-head infix-str i))
            (infix-str->scheme (substring infix-str (+ i 1) j))
            (string-tail infix-str (+ j 1)))))
      (addition
        (display "add")
        (list '+ (infix-str->scheme (string-head infix-str addition))
                 (infix-str->scheme (string-tail infix-str (+ addition 1)))))
      (subtraction
        (display "sub")
        (list '- (infix-str->scheme (string-head infix-str subtraction))
                 (infix-str->scheme (string-tail infix-str (+ subtraction 1)))))
      (multiplication
        (display "mult")
        (list '* (infix-str->scheme (string-head infix-str multiplication))
                 (infix-str->scheme (string-tail infix-str (+ multiplication 1)))))
      (division
        (display "div")
        (list '/ (infix-str->scheme (string-head infix-str division))
                 (infix-str->scheme (string-tail infix-str (+ division 1)))))
      (exponentiation
        (display "expt")
        (list 'expt (infix-str->scheme (string-head infix-str exponentiation))
                    (infix-str->scheme (string-tail infix-str (+ exponentiation 1)))))
      (squareroot
        (display "sqrt")
        (list 'sqrt (infix-str->scheme (string-tail infix-str (+ squareroot 4)))))
      (else
        (string->symbol infix-str)))))
  ; (let ((names (let-bound-variables let-exp))
  ; (values (let-values let-exp))
  ; (body (let-body let-exp)))
  ;   (cons (list 'LAMBDA names body)
  ;   values)))

(define (infix->scheme infix-exp)
  (display (infix-str->scheme (cadr infix-exp)))
  (newline)
  (newline)
  (newline)
  (infix-str->scheme (cadr infix-exp)))

;;; Procedure applications -- NO-ARGS? and LAST-OPERAND? added

(define (application? exp)
  (pair? exp))

(define (no-args? exp)				;; Added for tail recursion
  (and (pair? exp)
       (null? (cdr exp))))

(define (args-application? exp)			;; Changed from 5.2.1
  (and (pair? exp)
       (not (null? (cdr exp)))))


(define (operator app) (car app))
(define (operands app) (cdr app))

(define (last-operand? args)			;; Added for tail recursion
  (null? (cdr args)))

(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))

;;; Another special form that will be needed later.

(define (amb? exp)
  (and (pair? exp) (eq? (car exp) 'amb)))

(define (amb-alternatives exp) (cdr exp))



(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (if-fail-proc exp) (cadr exp))

(define (if-fail-failure exp) (caddr exp))
