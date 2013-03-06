;;;;    Generic sequence operator definitions

;;; First we declare the operators we want to be generic.
;;;  Each declaration specifies the arity (number of arguments)
;;;  and the default operation, if necessary.

(define sequence:null
  (make-generic-operator 1))


(define sequence:ref
  (make-generic-operator 2))

(define sequence:size
  (make-generic-operator 1))

(define sequence:type
  (make-generic-operator 1))

(define sequence:null?
  (make-generic-operator 1))

(define sequence:equal?
  (make-generic-operator 2))

(define sequence:set!
  (make-generic-operator 3))

(define sequence:subsequence
  (make-generic-operator 3))



;;; sequence:append takes multiple arguments.  It is defined in terms
;;; of a generic binary append that takes two sequences.

(define (sequence:append . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for append"))
  (let ((type? (sequence:type (car sequences))))
    (fold-right generic:binary-append
      (sequence:null (sequence:type (car sequences)))
      sequences)))

(define generic:binary-append (make-generic-operator 2))


;;; Operations implemented in the pset

(define (sequence:construct sequence-type . items)
  (generic:construct sequence-type items))

(define generic:construct (make-generic-operator 2))


(define (sequence:generate sequence-type n function)
  (generic:construct sequence-type (generate-list '() n function)))


(define generic:to-list (make-generic-operator 1))

(define (sequence:map function . sequences)
  (let ((list-sequences (map generic:to-list sequences)))
    (generic:construct (sequence:type (first sequences)) (apply map (cons function list-sequences)))))


(define (sequence:for-each function . sequences)
  (let ((list-sequences (map generic:to-list sequences)))
    (apply map (cons function list-sequences))))


(define (sequence:filter sequence predicate)
  (generic:construct (sequence:type sequence) (list-transform-positive (generic:to-list sequence) predicate)))


(define (list:get-index-helper sequence predicate i)
  (cond
    ((>= i (sequence:size sequence))
      #f)
    ((predicate (sequence:ref sequence i))
      i)
    (else
      (list:get-index-helper sequence predicate (+ i 1)))))

(define (sequence:get-index sequence predicate)
  (list:get-index-helper sequence predicate 0))


(define (sequence:get-element sequence predicate)
  (list-search-positive (generic:to-list sequence) predicate))


(define (sequence:fold-right function initial sequence)
  (fold-right function initial (generic:to-list sequence)))


(define (sequence:fold-left function initial sequence)
  (fold-left function initial (generic:to-list sequence)))


;;; Implementations of the generic operators.

(define (any? x) #t)
(define (constant val) (lambda (x) val))
(define (is-exactly val) (lambda (x) (eq? x val)))

(defhandler sequence:null (constant "")    (is-exactly string?))
(defhandler sequence:null (constant '())   (is-exactly list?))
(defhandler sequence:null (constant #())   (is-exactly vector?))

(defhandler sequence:ref string-ref         string? exact-integer?)
(defhandler sequence:ref list-ref           list?   exact-integer?)
(defhandler sequence:ref vector-ref         vector? exact-integer?)

(defhandler sequence:size string-length     string?)
(defhandler sequence:size length            list?)
(defhandler sequence:size vector-length     vector?)

(defhandler sequence:type (constant string?)     string?)
(defhandler sequence:type (constant list?)       list?)
(defhandler sequence:type (constant vector?)     vector?)


(define (vector-null? v) (= (vector-length v) 0))

(defhandler sequence:null? string-null?     string?)
(defhandler sequence:null? null?            list?)
(defhandler sequence:null? vector-null?     vector?)


;;; To assign to the ith element of a list:

(define (list-set! list i val)
  (cond ((null? list)
	 (error "List does not have enough elements" i))
	((= i 0) (set-car! list val))
	(else (list-set! (cdr list) (- i 1) val))))

(defhandler sequence:set! string-set!   string? exact-integer? any?)
(defhandler sequence:set! list-set!     list?   exact-integer? any?)
(defhandler sequence:set! vector-set!   vector? exact-integer? any?)


(defhandler sequence:subsequence
		  substring
		  string?  exact-integer?  exact-integer?)
(defhandler sequence:subsequence
		  sublist
		  list?  exact-integer?  exact-integer?)
(defhandler sequence:subsequence
		  subvector
		  vector?  exact-integer?  exact-integer?)


(define (vector-append v1 v2)
  (let ((n1 (vector-length v1))
	(n2 (vector-length v2)))
    (make-initialized-vector (+ n1 n2)
			     (lambda (i)
			       (if (< i n1)
				   (vector-ref v1 i)
				   (vector-ref v2 (- i n1)))))))


(define (compose-1st-arg f g)
  (lambda (x y) (f (g x) y)))

(define (compose-2nd-arg f g)
  (lambda (x y) (f x (g y))))

(define (string->vector string)
  (list->vector (string->list string)))

(define (vector->string vector)
  (list->string (vector->list vector)))

(defhandler generic:binary-append (compose-2nd-arg vector-append string->vector)  vector? string?)
(defhandler generic:binary-append (compose-2nd-arg vector-append list->vector)    vector? list?)
(defhandler generic:binary-append vector-append                                   vector? vector?)

(defhandler generic:binary-append (compose-2nd-arg append string->list)           list? string?)
(defhandler generic:binary-append append                                          list? list?)
(defhandler generic:binary-append (compose-2nd-arg append vector->list)           list? vector?)

(defhandler generic:binary-append string-append                                   string? string?)
(defhandler generic:binary-append (compose-2nd-arg string-append list->string)    string? list?)
(defhandler generic:binary-append (compose-2nd-arg string-append vector->string)  string? vector?)


;;; Operations implemented in the pset

(defhandler generic:construct (lambda (type items) (list->string items))  (is-exactly string?) list?)
(defhandler generic:construct (lambda (type items) items)                 (is-exactly list?)   list?)
(defhandler generic:construct (lambda (type items) (list->vector items))  (is-exactly vector?) list?)

(define (generate-list list n function)
  (if (= n -1)
    list
    (generate-list (cons (function n) list) (- n 1) function)))


(defhandler generic:to-list string->list          string?)
(defhandler generic:to-list (lambda (list) list)  list?)
(defhandler generic:to-list vector->list          vector?)



;;; Tests for operations implemented in the pset
(newline)
(display "2.1 Tests:")
(newline)
(newline)

(display "sequence:construct")
(newline)
(display (sequence:construct string? #\a #\b))
(newline)
(display (sequence:construct list? 1 2 3))
(newline)
(display (sequence:construct vector? 4 5 6))
(newline)

(display "sequence:generate")
(newline)
(display (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n)))))
(newline)
(display (sequence:generate list? 10 (lambda (n) n)))
(newline)
(display (sequence:generate vector? 10 (lambda (n) n)))
(newline)

(display "sequence:map")
(newline)
(display (sequence:map (lambda items (last items))
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n))))
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 49 n))))
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 50 n))))))
(newline)
(display (sequence:map (lambda items (fold-right + 0 items))
  (sequence:generate list? 10 (lambda (n) (+ 48 n)))
  (sequence:generate list? 10 (lambda (n) (+ 49 n)))
  (sequence:generate list? 10 (lambda (n) (+ 50 n)))))
(newline)
(display (sequence:map (lambda items (fold-right + 0 items))
  (sequence:generate vector? 10 (lambda (n) (+ 48 n)))
  (sequence:generate vector? 10 (lambda (n) (+ 49 n)))
  (sequence:generate vector? 10 (lambda (n) (+ 50 n)))))
(newline)

(display "sequence:for-each")
(newline)
(sequence:for-each (lambda x (display x))
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n))))
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 49 n))))
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 50 n)))))
(newline)
(sequence:for-each (lambda x (display x))
  (sequence:generate list? 10 (lambda (n) (+ 48 n)))
  (sequence:generate list? 10 (lambda (n) (+ 49 n)))
  (sequence:generate list? 10 (lambda (n) (+ 50 n))))
(newline)
(sequence:for-each (lambda x (display x))
  (sequence:generate vector? 10 (lambda (n) (+ 48 n)))
  (sequence:generate vector? 10 (lambda (n) (+ 49 n)))
  (sequence:generate vector? 10 (lambda (n) (+ 50 n))))
(newline)

(display "sequence:filter")
(newline)
(display (sequence:filter (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n)))) (lambda (x) (char=? x #\4))))
(newline)
(display (sequence:filter (sequence:generate list? 10 (lambda (n) (+ 48 n))) odd?))
(newline)
(display (sequence:filter (sequence:generate vector? 10 (lambda (n) (+ 48 n))) even?))
(newline)

(display "sequence:get-index")
(newline)
(display (sequence:get-index (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n)))) (lambda (x) (char=? x #\4))))
(newline)
(display (sequence:get-index (sequence:generate list? 10 (lambda (n) (+ 48 n))) odd?))
(newline)
(display (sequence:get-index (sequence:generate vector? 10 (lambda (n) (+ 48 n))) even?))
(newline)

(display "sequence:get-element")
(newline)
(display (sequence:get-element (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n)))) (lambda (x) (char=? x #\4))))
(newline)
(display (sequence:get-element (sequence:generate list? 10 (lambda (n) (+ 48 n))) odd?))
(newline)
(display (sequence:get-element (sequence:generate vector? 10 (lambda (n) (+ 48 n))) even?))
(newline)

(display "sequence:fold-right")
(newline)
(display (sequence:fold-right list #\S (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n))))))
(newline)
(display (sequence:fold-right / 1 (sequence:generate list? 10 (lambda (n) (+ 48 n)))))
(newline)
(display (sequence:fold-right + 0 (sequence:generate vector? 10 (lambda (n) (+ 48 n)))))
(newline)

(display "sequence:fold-left")
(newline)
(display (sequence:fold-left list #\S (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n))))))
(newline)
(display (sequence:fold-left / 1 (sequence:generate list? 10 (lambda (n) (+ 48 n)))))
(newline)
(display (sequence:fold-left + 0 (sequence:generate vector? 10 (lambda (n) (+ 48 n)))))
(newline)


(newline)
(display "2.2 Tests:")
(newline)
(newline)

(display "sequence:append")
(newline)
(display (sequence:append
  (sequence:generate list? 10 (lambda (n) (ascii->char (+ 48 n))))
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 49 n))))
  (sequence:generate vector? 10 (lambda (n) (ascii->char (+ 50 n))))))
(newline)
(display (sequence:append
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 48 n))))
  (sequence:generate list? 10 (lambda (n) (ascii->char (+ 49 n))))
  (sequence:generate vector? 10 (lambda (n) (ascii->char (+ 50 n))))))
(newline)
(display (sequence:append
  (sequence:generate vector? 10 (lambda (n) (ascii->char (+ 48 n))))
  (sequence:generate string? 10 (lambda (n) (ascii->char (+ 49 n))))
  (sequence:generate list? 10 (lambda (n) (ascii->char (+ 50 n))))))
(newline)

;;; 2.3 Answer
; I do not believe that this is a good idea, because the arguments could be ambiguous. For instance, if we had a sequence:contrived operation that took any number of <string? | list? | vector?> arguments, it would be impossible to tell whether the caller intended the first argument to specify the return format or not. Even though this is a contrived example, the introduction of this optional argument would make it difficult automatically unambiguously parse this extra argument, requiring much more code to actually implement generic operators.
; This could be fixed if scheme had named arguments, but possible solutions include requiring the type predicate for all operations or passing a special optional argument (one that is not used in any other context).
; It would also be possible to specify the output as a type that does not make sense in context. For example, if we append a few lists of functions, it would not make sense to get the output as a string. This is implicit in the idea, and the only way to avoid it is to disallow such nonsensical outputs.
; make-generic-helper would have to read the optional argument and pass it to the handler if necessary. 
; defhandler would have to accept an optional target selection argument.

;;; 2.4 Answer
; I believe this is a good idea. Generic operations with unspecified arity are fairly common, and it wouldn't be terribly difficult to have the first k-1 predicates examine the first k-1 arguments and the last (k) predicate examine the arguments in the range (k, n). Of course, the operation would require at least k arguments. I do not believe this would significantly initeract with problem 2.3 if both solutions are implemented correctly.
; make-generic-operator would take an additional optional parameter specifying that the operator has unspecified arity, and the "wrong number of arguments" check would take this into account. When traversing the tree, it would also need to ensure it will accept a branch that has unspecified arity when appropriate.
; defhandler would take into account the unspecified arity option in the "Incorrect operator arity" check

;;; 2.5 Answer
; A. Louis' implementation fails - ie. the invariant "(generic:less? x y) implies (not (generic:less? y x))" does not hold - on (generic:less? '((circular-list 1) 1) '(1 (circular-list 1))). This would causethe set to potentially allow duplicates in the set.
; B. Alyssa's suggestion is very verbose and does not effectively use the generic dispatch framework we have set up to reduce her time spent manually coding up each possibility. Also, she wrongly assumes that you need N^2 items in the dispatch table, as shown below, we only need 2*N items.
; C. Code below

(define (list<? list-1 list-2)
  (let ((len-1 (length list-1)) (len-2 (length list-2)))
    (cond ((< len-1 len-2) #t)
          ((> len-1 len-2) #f)
          ;; Invariant:  equal lengths
          (else
           (let prefix<? ((list-1 list-1) (list-2 list-2))
             (cond ((null? list-1) #f)  ; same
                   ((generic:less? (car list-1) (car list-2)) #t)
                   ((generic:less? (car list-2) (car list-1)) #f)
                   (else (prefix<? (cdr list-1) (cdr list-2)))))))))

(define generic:type-sort-score
  (make-generic-operator 1))

(define generic:type-sort
  (make-generic-operator 2))

(defhandler generic:type-sort-score  (constant 0)  null?)
(defhandler generic:type-sort-score  (constant 1)  boolean?)
(defhandler generic:type-sort-score  (constant 2)  char?)
(defhandler generic:type-sort-score  (constant 3)  number?)
(defhandler generic:type-sort-score  (constant 4)  symbol?)
(defhandler generic:type-sort-score  (constant 5)  string?)
(defhandler generic:type-sort-score  (constant 6)  vector?)
(defhandler generic:type-sort-score  (constant 7)  list?)

(define (compare operator)
  (lambda (x y) (operator x y)))

(defhandler generic:type-sort  (lambda (x y) #f)                                          null? null?)
(defhandler generic:type-sort  (lambda (x y) (and x (not y)))                             boolean? boolean?)
(defhandler generic:type-sort  (compare char<?)                                           char? char?)
(defhandler generic:type-sort  (compare <)                                                number? number?)
(defhandler generic:type-sort  (compare symbol<?)                                         symbol? symbol?)
(defhandler generic:type-sort  (compare string<?)                                         string? string?)
(defhandler generic:type-sort  (lambda (x y) (list<? (vector->list x) (vector->list x)))  vector? vector?)
(defhandler generic:type-sort  (compare list<?)                                           list? list?)

(define (generic:less? first second)
  (let ((first-score (generic:type-sort-score first))
        (second-score (generic:type-sort-score second)))
    (if (= first-score second-score)
      (generic:type-sort first second)
      (< first-score second-score))))

(newline)
(display "2.5.A Tests:")
(newline)
(newline)

(display "generic:less?")
(newline)
(display (generic:less? 3 1))
(newline)
(display (generic:less? #t "The entire text of Atlas Shrugged"))
(newline)
(display (generic:less? "I love parens!" "Tail recursion is best recursion!"))
(newline)
(display (generic:less? 'harveydentisaschemer #\v))
(newline)
(display (generic:less? '() #()))
(newline)
(display (generic:less? #() '()))
(newline)

;;; 2.6 Answers
; The main issue with dispatch on predicates is that each predicate must be evaluated each time we want to dispatch. Further, we have to try different sets of predicates until we get one that matches (albeit in a somewhat-efficient tree-walking mechanism).
;Tagged systems can be much more efficient, but only if we don't test the tags with predicates, since this would leave us in the same place as the current system (though with potentially cheaper predicates)! If we have a hash table mechanism to look up the tags, we can have constant-time dispatch instead of logarithmic like withthe predicate mechanism

; By avoiding a predicate-based system, however, we give up flexibility. For instance, maybe we want to have a special handler for division that performs some logging when trying to divide by 0. The predicate system makes this fairly simple - just check if the second argument equals 0. It is not possible to do this in a tagging system, however, unless the caller checks that the second argument does not equal 0 and tags the call manually, which defeats the purpose of generic dispatch.

; You could build a generic dispatch system with zero runtime overhead if the language already does some sort of generic dispatch in the background and you are able to take advantage of that in your generic dispatch framework. This would, of course, limit your system to the language's idiosyncrasies. The tagging system could have constant-time dispatch if implemented properly. The limiting factor is the time to look up the correct handler, so any system with such constant-time lookups can be made constant-time.