; (with-depth-first-schedule yacht-puzzle)
(define yacht-puzzle
  (lambda ()
    (let ((Moore-daughter (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa)))
      (require (eq? Moore-daughter 'Mary-Ann))
      (require (not (eq? Moore-daughter 'Lorna)))
      (let ((Downing-daughter (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa)))
        (require (not (eq? Downing-daughter 'Melissa)))
        (let ((Hall-daughter (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa)))
          (require (not (eq? Hall-daughter 'Rosalind)))
          (let ((Hood-daughter (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa)))
            (require (eq? Hood-daughter 'Melissa))
            ; (require (not (eq? Hood-daughter 'Gabrielle)))  Rule was stated but not required
            (let ((Parker-daughter (amb 'Mary-Ann 'Gabrielle 'Lorna 'Rosalind 'Melissa)))
              ; (require (Parker-daughter))
              (require (distinct? (list Moore-daughter Downing-daughter Hall-daughter Hood-daughter Parker-daughter)))
              (pp (list (list 'Moore-daughter Moore-daughter)
                    (list 'Downing-daughter Downing-daughter)
                    (list 'Hall-daughter Hall-daughter)
                    (list 'Hood-daughter Hood-daughter)
                    (list 'Parker-daughter Parker-daughter))))))))
(amb)))

