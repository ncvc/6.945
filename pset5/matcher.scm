;;;; Matcher based on match combinators, CPH/GJS style.
;;;     Idea is in Hewitt's PhD thesis (1969).

(declare (usual-integrations))

;;; There are match procedures that can be applied to data items.  A
;;; match procedure either accepts or rejects the data it is applied
;;; to.  Match procedures can be combined to apply to compound data
;;; items.

;;; A match procedure takes a list containing a data item, a
;;; dictionary, and a success continuation.  The dictionary
;;; accumulates the assignments of match variables to values found in
;;; the data.  The success continuation takes two arguments: the new
;;; dictionary, and the number of items absorbed from the list by the
;;; match.  If a match procedure fails it returns #f.

;;; Primitive match procedures:

(define (match:eqv pattern-constant)
  (define (eqv-match data dictionary succeed)
    (and (eqv? data pattern-constant)
	 (succeed dictionary)))
  eqv-match)

(define (match:element variable restrictions)
  (define (ok? datum)
    (every (lambda (restriction)
	     (restriction datum))
	   restrictions))
  (define (element-match data dictionary succeed)
    (and (ok? data)
	 (let ((vcell (match:lookup variable dictionary)))
	   (if vcell
	       (and (equal? (match:value vcell) data)
		    (succeed dictionary))
	       (succeed (match:bind variable data dictionary))))))
  element-match)


;;; Support for the dictionary.

(define (match:bind variable data-object dictionary)
  (cons (list variable data-object) dictionary))

(define (match:lookup variable dictionary)
  (assq variable dictionary))

(define (match:value vcell)
  (cadr vcell))

;;; TODO match:segment need not search under two circumstances.  One
;;; is encoded here: if the variable's value is already known, no
;;; search is needed.  The other is if this segment variable is the
;;; last segment variable in its enclosing list matcher.  Then the
;;; list matcher can compute the exact quantity of things in the list
;;; that this variable must match (because if it matches any other
;;; number, the enclosing list match is sure to fail due to a length
;;; mismatch).  This extra optimization can save a factor linear in
;;; the length of the list being matched.  (In cases of complete tail
;;; position maybe even quadratic, because knowing that an unbound
;;; variable has to match all the available data can obviate a
;;; quadratic amount of work taking useless list-heads).

(define (match:segment variable)
  (define (segment-match data dictionary succeed)
    (and (list? data)
	 (let ((vcell (match:lookup variable dictionary)))
	   (if vcell
	       (let lp ((data data)
			(pattern (match:value vcell))
			(n 0))
		 (cond ((pair? pattern)
			(if (and (pair? data)
				 (equal? (car data) (car pattern)))
			    (lp (cdr data) (cdr pattern) (+ n 1))
			    #f))
		       ((not (null? pattern)) #f)
		       (else (succeed dictionary n))))
	       (let ((n (length data)))
		 (let lp ((i 0))
		   (if (<= i n)
		       (or (succeed (match:bind variable
						(list-head data i)
						dictionary)
				    i)
			   (lp (+ i 1)))
		       #f)))))))
  (segment-matcher! segment-match)
  segment-match)

(define (match:list . match-combinators)
  (define (list-match data dictionary succeed)
    (let lp ((data data)
       (matchers match-combinators)
       (dictionary dictionary))
      (define (try-element submatcher)
  (submatcher (car data) dictionary
    (lambda (new-dictionary)
      (lp (cdr data) (cdr matchers) new-dictionary))))
      (define (try-segment submatcher)
  (submatcher data dictionary
          (lambda (new-dictionary n)
      (if (> n (length data))
    (error "Matcher ate too much." n))
      (lp (list-tail data n) (cdr matchers) new-dictionary))))
      (cond ((pair? matchers)
       (if (segment-matcher? (car matchers))
     (try-segment (car matchers))
     (and (pair? data) (try-element (car matchers)))))
      ((pair? data) #f)
      ((null? data)
       (succeed dictionary))
      (else #f))))
  list-match)

(define (match:choice . match-combinators)
  (define (choice-match data dictionary succeed)
    (let lp ((data data)
             (matchers match-combinators)
             (dictionary dictionary))
      (cond
        ((pair? matchers)
          (let ((success ((car matchers) data dictionary succeed)))
            (if success
              success
              (lp data (cdr matchers) dictionary))))
        ((pair? data)
          #f)
        ((null? data)
          (succeed dictionary))
        (else
          #f))))
  choice-match)

(define named-bindings (make-eq-hash-table))

(define (match:pletrec bindings body)
  (define (pletrec-match data dictionary succeed)
    (define (bindings->dict hash-table bindings-list)
      (cond
        ((eq? (length bindings-list) 0)
          hash-table)
        (else
          (hash-table/put! hash-table (caar bindings-list) (cadar bindings-list))
          (bindings->dict hash-table (cdr bindings-list)))))

    (set! named-bindings (bindings->dict named-bindings bindings))

    ((match:->combinators body) data dictionary succeed))
  pletrec-match)

(define (match:ref pattern)
  (define (ref-match data dictionary succeed)
    ((match:->combinators (hash-table/get named-bindings (cadr pattern) 0)) data dictionary succeed))
  ref-match)


;;; Sticky notes

(define (segment-matcher! thing)
  (eq-put! thing 'segment-matcher #t))
(define (segment-matcher? thing)
  (eq-get thing 'segment-matcher))

;;; Syntax of matching is determined here.

(define (match:element? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?)))

(define (match:segment? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '??)))

(define (match:variable-name pattern) (cadr pattern))
(define (match:restrictions pattern) (cddr pattern))

(define (match:list? pattern)
  (and (list? pattern)
       (or (null? pattern)
	   (not (memq (car pattern) '(? ??))))))

(define (match:choice? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:choice)))

(define (match:pletrec? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:pletrec)))

(define (match:ref? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:ref)))

(define match:->combinators
  (make-generic-operator 1 'eqv match:eqv))

(defhandler match:->combinators
  (lambda (pattern)
    (match:element
     (match:variable-name pattern)
     (match:restrictions pattern)))
  match:element?)

(defhandler match:->combinators
  (lambda (pattern) (match:segment (match:variable-name pattern)))
  match:segment?)

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:list (map match:->combinators pattern)))
  match:list?)

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:choice (map match:->combinators pattern)))
  match:choice?)

(defhandler match:->combinators
  (lambda (pattern)
    (match:pletrec (cadr pattern) (caddr pattern)))
  match:pletrec?)

(defhandler match:->combinators
  (lambda (pattern)
    (match:ref pattern))
  match:ref?)

(define (matcher pattern)
  (let ((match-combinator (match:->combinators pattern)))
    (lambda (datum)
      (match-combinator datum '()
       (lambda (dictionary) dictionary)))))

#|
 ((match:->combinators '(a ((? b) 2 3) 1 c))
  '(a (1 2 3) 1 c)
  '()
   (lambda (x) `(succeed ,x)))
 ;Value: (succeed ((b 1)))

 ((match:->combinators '(a ((? b) 2 3) (? b) c))
  '(a (1 2 3) 2 c)
  '()
   (lambda (x) `(succeed ,x)))
 ;Value: #f

 ((match:->combinators '(a ((? b) 2 3) (? b) c))
  '(a (1 2 3) 1 c)
  '()
   (lambda (x) `(succeed ,x)))
 ;Value: (succeed ((b 1)))


 ((match:->combinators '(a (?? x) (?? y) (?? x) c))
  '(a b b b b b b c)
  '()
  (lambda (x)
    (pp `(succeed ,x))
    #f))
 (succeed ((y (b b b b b b)) (x ())))
 (succeed ((y (b b b b)) (x (b))))
 (succeed ((y (b b)) (x (b b))))
 (succeed ((y ()) (x (b b b))))
 ;Value: #f

 ((matcher '(a ((? b) 2 3) (? b) c))
  '(a (1 2 3) 1 c))
 ;Value: ((b 1))
|#

;;; Nice pattern inspection procedure that will be used by the
;;; pattern-directed invocation system.

(define (match:pattern-names pattern)
  (let loop ((pattern pattern) (names '()))
    (cond ((or (match:element? pattern)
               (match:segment? pattern))
           (let ((name
		  (match:variable-name pattern)))
             (if (memq name names)
                 names
                 (cons name names))))
          ((list? pattern)
           (let elt-loop
	       ((elts pattern) (names names))
             (if (pair? elts)
                 (elt-loop (cdr elts)
			   (loop (car elts) names))
                 names)))
          (else names))))

#|
 (match:pattern-names '((? a) (?? b)))
 ;Value: (b a)
|#
