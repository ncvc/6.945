;;;; A Small Financial Example 

;;; First, we need a small database mechanism
;;;  Parent and child here do not refer to biological
;;;  things, but rather the relationships of parts
;;;  of a database.

(define (add-branch! parent child name)
  (eq-put! parent name child)
  (eq-put! child 'parent parent)
  (eq-put! child 'given-name name)
  'done)

(define (name-of thing)
  (let ((n (eq-get thing 'given-name)))
    (if n
	(let ((p (eq-get thing 'parent)))
	  (if p
	      (cons n (name-of p))
	      (list n)))
	(list (name thing)))))

;;; e.g. (thing-of Gaggle-salary gross-income Ben)

(define (thing-of name-path)
  (let lp ((path name-path))
    (cond ((= (length path) 1) (car path))
	  (else
	   (eq-get (lp (cdr path))
		   (car path))))))



;;; A financial entity has three cells

(define (make-financial-entity entity)
  (eq-put! entity 'kind-of-entity 'financial)

  (let-cells (gross-income expenses net-income)

    (add-branch! entity gross-income 'gross-income)
    (add-branch! entity net-income 'net-income)
    (add-branch! entity expenses 'expenses)

    (c:+ expenses net-income gross-income)
    'done
    ))

(define (financial-entity? thing)
  (eq? (eq-get thing 'kind-of-entity) 'financial))

(define (gross-income entity)
  (assert (financial-entity? entity))
  (eq-get entity 'gross-income))

(define (net-income entity)
  (assert (financial-entity? entity))
  (eq-get entity 'net-income))

(define (expenses entity)
  (assert (financial-entity? entity))
  (eq-get entity 'expenses))

(define (breakdown sum-node . part-names)
  (for-each
    (lambda (part-name)
      (let-cell part
        (add-branch! sum-node part part-name)))
    part-names)
  (c:id
    (let lp ((names part-names))
      (ce:+
        (eq-get sum-node (car names))
        (cond
          ((= (length names) 2)
            (eq-get sum-node (cadr names)))
          (else
            (lp (cdr names))))))
    sum-node)
  'done)

(define (combine-financial-entities compound . parts)
  (assert (every financial-entity? parts))
  (c:id
    (let lp ((p parts))
      (ce:+
        (gross-income (car p))
        (cond
          ((= (length p) 2)
            (gross-income (cadr p)))
          (else
            (lp (cdr p))))))
    (gross-income compound))
  (c:id
    (let lp ((p parts))
      (ce:+
        (net-income (car p))
        (cond
          ((= (length p) 2)
            (net-income (cadr p)))
          (else
            (lp (cdr p))))))
    (net-income compound))
  (c:id
    (let lp ((p parts))
      (ce:+
        (expenses (car p))
        (cond
          ((= (length p) 2)
            (expenses (cadr p)))
          (else
            (lp (cdr p))))))
    (expenses compound))
  'done)

#|
(initialize-scheduler)

(make-financial-entity 'Alyssa)
(make-financial-entity 'Ben)

;;; Ben and Alyssa are married
(make-financial-entity 'Ben-Alyssa)
(combine-financial-entities 'Ben-Alyssa 'Ben 'Alyssa)

;;; Ben and Alyssa file income tax jointly
(tell! (gross-income 'Ben-Alyssa) 427000 'IRS)

;;; Ben works at Gaggle as a software engineer.
(breakdown (gross-income 'Ben) 'Gaggle-salary 'investments)

;;; He gets paid alot to make good apps.
(tell! (thing-of '(Gaggle-salary gross-income Ben)) 200000 'Gaggle)

;;; Alyssa works as a PhD biochemist in big pharma.
(breakdown (gross-income 'Alyssa) 'GeneScam-salary 'investments)

;;; Biochemists are paid poorly.
(tell! (thing-of '(GeneScam-salary gross-income Alyssa)) 70000 'GeneScam)

(tell! (thing-of '(investments gross-income Alyssa))
       (make-interval 30000 40000) 'Alyssa)

(inquire (thing-of '(investments gross-income Ben)))
;Value: #(supported #[interval 117000 127000] (gaggle genescam alyssa irs))

;;; Ben is a tightwad
(tell! (thing-of '(expenses Ben)) (make-interval 10000 20000) 'Ben)

(inquire (thing-of '(net-income Ben)))
;Value: #(supported #[interval 297000 317000] (ben genescam alyssa irs))

;;; But Alyssa is not cheap.  She likes luxury.
(tell! (thing-of '(expenses Alyssa)) (make-interval 200000 215000) 'Alyssa)

(inquire (thing-of '(net-income Alyssa)))
;Value: #(supported #[interval -115000 -90000] (alyssa genescam))

;;; But they are doing OK anyway!
(inquire (thing-of '(net-income Ben-Alyssa)))
;Value: #(supported #[interval 192000 217000] (ben alyssa irs))

;;; Notice that this conclusion does not depend on the details, such
;;; as Gaggle or GeneScam!
|#
