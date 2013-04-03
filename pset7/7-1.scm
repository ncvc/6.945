(define (cell-name name property)
  (symbol name '- property))


(define (add-living-beyond-means name)
  (let ((cell-name (cell-name name 'living-beyond-means)))
    (define-cell cell-name)
    (eq-put! name 'living-beyond-means cell-name)
    (p:<
      (e:interval-high (eq-get name 'income))
      (e:interval-low (eq-get name 'expenses))
      cell-name)))


(define (add-profligate name)
  (let ((cell-name (cell-name name 'profligate)))
    (define-cell cell-name)
    (eq-put! name 'profligate cell-name)
    (p:and
      (e:id (eq-get name 'living-beyond-means))
      (e:or
        (e:id ((eq-path '(upper-middle-class income)) name))
        (e:id ((eq-path '(the-one-percent income)) name)))
      cell-name)))


(define (add-bmi name)
  (let ((cell-name (cell-name name 'bmi)))
    (define-cell cell-name)
    (eq-put! name 'bmi cell-name)
    (p:*
      (e:/
        (e:id (eq-get name 'weight))
        (e:square (eq-get name 'height)))
      703
      cell-name)))


(define (add-fat-cat name)
  (let ((cell-name (cell-name name 'fat-cat)))
    (define-cell cell-name)
    (eq-put! name 'fat-cat cell-name)
    (p:and
      (e:id ((eq-path '(obese bmi)) name))
      (e:id ((eq-path '(the-one-percent income)) name))
      cell-name)))


(define (attach-info names)
  (for-each
    (lambda (name)
      (for-each
        (lambda (property)
          (let ((cell-name (cell-name name property)))
            (define-cell cell-name)
            (eq-put! name property cell-name)))
        '(height weight income expenses))

      ((c:bins
        (named-ranges 'premise-height
          `(short         ,(make-interval 0 69))
          `(medium-height ,(make-interval 68 73))
          `(tall          ,(make-interval 72 96))))
        (eq-get name 'height))

      ((c:bins
        (named-ranges 'premise-weight
          `(small         ,(make-interval 0 160))
          `(medium-weight ,(make-interval 150 190))
          `(large         ,(make-interval 180 1000))))
        (eq-get name 'weight))

      ((c:bins
        (named-ranges 'premise-income
          `(lower-class         ,(make-interval 0 16000))
          `(working-class       ,(make-interval 16000 30000))
          `(lower-middle-class  ,(make-interval 30000 100000))
          `(upper-middle-class  ,(make-interval 100000 300000))
          `(the-one-percent     ,(make-interval 300000 999999999999))))
        (eq-get name 'income))

      ((c:bins
        (named-ranges 'premise-expense
          `(below-average  ,(make-interval 0 18300))
          `(above-average  ,(make-interval 18300 999999999999))))
        (eq-get name 'expenses))

      (add-living-beyond-means name)
      (add-profligate name)
      (add-bmi name)

      ((c:bins
        (named-ranges 'premise-bmi
          `(underweight ,(make-interval 0 18.5))
          `(normal      ,(make-interval 18.5 25))
          `(overweight  ,(make-interval 25 30))
          `(obese       ,(make-interval 30 99))))
        (eq-get name 'bmi))

      (add-fat-cat name))
    names))


(attach-info '(John Paul George Ringo))
(tell! (eq-get 'John 'income) (make-interval 99999997 99999998) 'ncvc-estimate)
(tell! (eq-get 'John 'expenses) (make-interval 99999999 999999999) 'ncvc-estimate)
(tell! (eq-get 'John 'weight) (make-interval 240 250) 'ncvc-estimate)
(tell! (eq-get 'John 'height) (make-interval 68 71) 'ncvc-estimate)

#|
;; Sample output
1 ]=> (inquire (eq-get 'John 'profligate))
;Value: #(value=#t,
   premises=(premise-income ncvc-estimate),
   informants=((and:p cell58 cell57)))

1 ]=> (inquire (eq-get 'John 'living-beyond-means))
;Value: #(value=#t,
   premises=(ncvc-estimate),
   informants=((<:p cell54 cell53)))

1 ]=> (inquire (eq-get 'John 'bmi))
;Value: #(value=#[interval 168720/5041 87875/2312],
   premises=(ncvc-estimate),
   informants=((*:p cell61 703)))

1 ]=> (inquire ((eq-path '(obese bmi)) 'John))
;Value: #(value=#t,
   premises=(ncvc-estimate premise-bmi),
   informants=((and:p cell83 cell81)))

1 ]=> (inquire (eq-get 'John 'fat-cat))
;Value: #(value=#t,
   premises=(premise-income ncvc-estimate premise-bmi),
   informants=((and:p cell85 cell84)))
|#
