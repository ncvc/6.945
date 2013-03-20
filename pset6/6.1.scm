; Part 1
(define yacht-puzzle
  (lambda ()
    (let ((Moore (list 'Mary-Ann 'Lorna))
          (Downing (list (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa) 'Melissa))
          (Hall (list (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa) 'Rosalind))
          (Hood (list 'Melissa 'Gabrielle))
          (Parker (list (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa) (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa))))
      (let ((people (list Moore Downing Hall Hood Parker)))
        (require (distinct? (map (lambda (person) (first person)) people)))
        (require (distinct? (map (lambda (person) (second person)) people)))
        (require (eq? (second (list-search-positive people (lambda (person) (eq? 'Gabrielle (first person))))) (first Parker)))
        (list (list 'Moore Moore)
              (list 'Downing Downing)
              (list 'Hall Hall)
              (list 'Hood Hood)
              (list 'Parker Parker))))))

;Solution:
; 1 ]=> (with-depth-first-schedule yacht-puzzle)
; ;Value 11: ((moore (mary-ann lorna)) (downing (lorna melissa)) (hall (gabrielle rosalind)) (hood (melissa gabrielle)) (parker (rosalind mary-ann)))

; Lorna's father is Downing


; Part 2
(define yacht-puzzle-less-info
  (lambda ()
    (let ((Moore (list (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa) 'Lorna))
          (Downing (list (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa) 'Melissa))
          (Hall (list (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa) 'Rosalind))
          (Hood (list 'Melissa 'Gabrielle))
          (Parker (list (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa) (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa))))
      (let ((people (list Moore Downing Hall Hood Parker)))
        (require (distinct? (map (lambda (person) (first person)) people)))
        (require (distinct? (map (lambda (person) (second person)) people)))
        (require (eq? (second (list-search-positive people (lambda (person) (eq? 'Gabrielle (first person))))) (first Parker)))
        (pp (list (list 'Moore Moore)
                  (list 'Downing Downing)
                  (list 'Hall Hall)
                  (list 'Hood Hood)
                  (list 'Parker Parker)))))
  (amb)))

;Solution:
; 1 ]=> (with-depth-first-schedule yacht-puzzle-less-info)
; ((moore (gabrielle lorna)) (downing (rosalind melissa))
;                            (hall (mary-ann rosalind))
;                            (hood (melissa gabrielle))
;                            (parker (lorna mary-ann)))
; ((moore (gabrielle lorna)) (downing (mary-ann melissa))
;                            (hall (rosalind rosalind))
;                            (hood (melissa gabrielle))
;                            (parker (lorna mary-ann)))
; ((moore (lorna lorna)) (downing (mary-ann melissa))
;                        (hall (gabrielle rosalind))
;                        (hood (melissa gabrielle))
;                        (parker (rosalind mary-ann)))
; ((moore (mary-ann lorna)) (downing (lorna melissa))
;                           (hall (gabrielle rosalind))
;                           (hood (melissa gabrielle))
;                           (parker (rosalind mary-ann)))
; ;Value: #f

; There are 4 possible solutions if we don't know that Mary Ann's last name is Moore.