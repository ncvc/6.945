(initialize-scheduler)

(make-financial-entity 'Alyssa)
(make-financial-entity 'Ben)
(make-financial-entity 'Harry)
(make-financial-entity 'Eva)

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

;;; Ben is a tightwad
(tell! (thing-of '(expenses Ben)) (make-interval 10000 20000) 'Ben)

;;; But Alyssa is not cheap.  She likes luxury.
(tell! (thing-of '(expenses Alyssa)) (make-interval 200000 215000) 'Alyssa)


;;; Harry and Eva are married.
(make-financial-entity 'Harry-Eva)
(combine-financial-entities 'Harry-Eva 'Harry 'Eva)
;;; Harry and Eva file income tax jointly
(tell! (gross-income 'Harry-Eva) 2027000 'IRS)


;;; Harry and Ben are members of a club
(make-financial-entity 'Harry-Ben)
(combine-financial-entities 'Harry-Ben 'Harry 'Ben)
;;; The Harry and Ben have a joint membership, which varies by year
(tell! (expenses 'Harry-Ben) (make-interval 5000 6000) 'membership-fees)


;;; Everyone is part of a neighborhood where donations are used to improve the facilities. They pool together a decent chunk of cash each year
(make-financial-entity 'All)
(combine-financial-entities 'All 'Harry 'Eva 'Ben 'Alyssa)
;;; The Harry and Ben have a joint membership, which varies by year
(tell! (expenses 'All) (make-interval 10000 12000) 'donations)

(pp (inquire (thing-of '(gross-income Eva))))
