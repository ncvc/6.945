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

(define (get-parens lst i first-paren open-parens)
  (cond
    ((>= i (length lst))
      -1)
    ((not (string? (list-ref lst i)))
      (get-parens lst (+ i 1) first-paren open-parens))
    ((string=? (list-ref lst i) "(")
      (get-parens lst (+ i 1) (if (eq? first-paren -1) i first-paren) (+ open-parens 1)))
    ((string=? (list-ref lst i) ")")
      (if (eq? open-parens 1)
        (list first-paren i)
        (get-parens lst (+ i 1) first-paren (- open-parens 1))))
    (else
      (get-parens lst (+ i 1) first-paren open-parens))))

(define (index-of lst i item)
  (cond
    ((eq? i (length lst))
      -1)
    ((not (string? (list-ref lst i)))
      (index-of lst (+ i 1) item))
    ((string=? item (list-ref lst i))
      i)
    (else
      (index-of lst (+ i 1) item))))

(define (split-list lst item)
  (let ((i (index-of lst 0 item)))
    (if (eq? i -1)
      '()
      (list (list-head lst i) (list-tail lst (+ i 1))))))

(define (infix-tokens->scheme infix-symbols)
  (let ((add (split-list infix-symbols "+"))
        (sub (split-list infix-symbols "-"))
        (mul (split-list infix-symbols "*"))
        (div (split-list infix-symbols "/"))
        (expo (split-list infix-symbols "^"))
        (sqroot (split-list infix-symbols "sqrt"))
        (parens (get-parens infix-symbols 0 -1 0)))
    (cond
      ((null? infix-symbols)
        '())
      ((eq? (length infix-symbols) 1)
        (let ((symbol (first infix-symbols)))
          (if (or (symbol? symbol) (list? symbol))
            symbol
            (let ((number (string->number symbol)))
              (if (not (eq? number #f))
                number
                (string->symbol (first infix-symbols)))))))
      ((list? parens)
        (let ((i (first parens)) (j (second parens)))
          (infix-tokens->scheme (append
            (list-head infix-symbols i)
            (list (infix-tokens->scheme (sublist infix-symbols (+ i 1) j)))
            (list-tail infix-symbols (+ j 1))))))
      ((not (null? add))
        (list '+ (infix-tokens->scheme (first add))
                 (infix-tokens->scheme (second add))))
      ((not (null? sub))
        (let ((first-arg (infix-tokens->scheme (first sub))))
          (if (null? first-arg)
            (list '- (infix-tokens->scheme (second sub)))
            (list '- (infix-tokens->scheme (first sub))
                     (infix-tokens->scheme (second sub))))))
      ((not (null? mul))
        (list '* (infix-tokens->scheme (first mul))
                 (infix-tokens->scheme (second mul))))
      ((not (null? div))
        (list '/ (infix-tokens->scheme (first div))
                 (infix-tokens->scheme (second div))))
      ((not (null? expo))
        (list 'expt (infix-tokens->scheme (first expo))
                    (infix-tokens->scheme (second expo))))
      ((not (null? sqroot))
        (list 'sqrt (infix-tokens->scheme (second sqroot)))))))

(define (infix-tokenizer infix-str)
  (token-helper infix-str 0
    (sort (append (map (lambda (i) (list i "+" 1)) (string-search-all "+" infix-str))
                  (map (lambda (i) (list i "-" 1)) (string-search-all "-" infix-str))
                  (map (lambda (i) (list i "*" 1)) (string-search-all "*" infix-str))
                  (map (lambda (i) (list i "/" 1)) (string-search-all "/" infix-str))
                  (map (lambda (i) (list i "(" 1)) (string-search-all "(" infix-str))
                  (map (lambda (i) (list i ")" 1)) (string-search-all ")" infix-str))
                  (map (lambda (i) (list i "^" 1)) (string-search-all "^" infix-str))
                  (map (lambda (i) (list i "sqrt" 4)) (string-search-all "sqrt(" infix-str)))
      (lambda (tuple1 tuple2) (< (first tuple1) (first tuple2))))))

(define (token-helper str start operator-list)
  (if (null? operator-list)
    (if (eq? start (string-length str))
      '()
      (list (string-tail str start)))
    (let ((operator-tuple (first operator-list)))
      (let ((operator-start-index (first operator-tuple))
            (operator-end-index (+ (first operator-tuple) (third operator-tuple)))
            (symbol (second operator-tuple)))
        (if (eq? start operator-start-index)
          (cons symbol (token-helper str operator-end-index (cdr operator-list)))
          (append
            (list (substring str start operator-start-index)
                  symbol)
            (token-helper str operator-end-index (cdr operator-list))))))))

(define (infix->scheme infix-exp)
  (infix-tokens->scheme (infix-tokenizer (second infix-exp))))

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
