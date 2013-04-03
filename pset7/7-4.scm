(define (range first last)
  (if (>= first last)
      '()
      (cons first (range (+ first 1) last))))


; Returns the index in the puzzle of the given coordinate
(define (get-index x y)
  (+ x (* 9 y)))


; Restrict the given list of cells to ensure none of them have the same value
(define (restrict-cells cells false-cell)
  (for-each (lambda (i)
      (for-each (lambda (j)
          (c:eq? (ce:id (list-ref cells i)) (ce:id (list-ref cells j)) false-cell))
        (range (+ i 1) 9)))
    (range 0 8)))



(define (->val cell)
  (let ((val (vector-second (inquire cell))))
    (if (number? val)
      val
      0)))

; Display the board
(define (display-board)
  (for-each (lambda (y)
      (for-each (lambda (x)
          (display (->val (eq-get x y)))
          (display " "))
        (range 0 9))
      (newline))
    (range 0 9))
  (newline))


(define (validate-board)
  (for-each (lambda (y)
      (for-each (lambda (x)
          (inquire (eq-get x y)))
        (range 0 9)))
    (range 0 9)))


(define (sudoku-checker board)
  (define-cell false-cell)
  (tell! false-cell #f 'false-cell)

  ; Fill in what we have initally
  (for-each (lambda (x)
      (for-each (lambda (y)
          (let ((cell (symbol x y)))
            (define-cell cell)
            (let ((val (list-ref board (get-index x y))))
              (if (= val 0)
                (tell! cell (make-interval 1 9) 'checker-interval)
                (tell! cell val 'checker-val)))
            (eq-put! x y cell)))
        (range 0 9)))
    (range 0 9))

  ; Display board
  (newline)
  (pp "Board")
  (display-board)

  (pp "Setting row constraints...")
  (for-each (lambda (x)
      (restrict-cells (map (lambda (y) (eq-get x y)) (range 0 9)) false-cell))
    (range 0 9))

  (pp "Setting column constraints...")
  (for-each (lambda (y)
      (restrict-cells (map (lambda (x) (eq-get x y)) (range 0 9)) false-cell))
    (range 0 9))

  (pp "Setting box constraints...")
  (for-each (lambda (x1)
      (for-each (lambda (y1)
          (restrict-cells
            (apply append
              (map (lambda (x2)
                  (map (lambda (y2)
                      (eq-get (+ x2 (* x1 3)) (+ y2 (* y1 3))))
                    (range 0 3)))
                (range 0 3)))
            false-cell))
        (range 0 3)))
    (range 0 3))

  (validate-board)
  (pp "Board is valid if no contradictions were shown"))


(define puzzle1
  (list
    0 0 0 2 0 0 0 0 0 ; Board is valid
    0 9 0 0 1 5 6 0 0
    7 6 0 0 0 0 3 0 0
    0 0 3 0 8 0 9 1 0
    0 0 5 4 0 0 0 0 0
    6 0 0 0 0 7 4 3 0
    9 5 0 0 0 6 0 0 0
    0 0 0 0 0 0 0 4 0
    0 3 0 0 0 0 0 0 2))

(define puzzle2
  (list
    0 0 3 0 2 0 6 0 0
    9 0 0 3 0 5 0 0 1
    0 0 1 8 0 6 4 0 0
    0 0 8 1 0 2 9 0 0
    7 0 0 0 0 0 0 0 8
    0 0 6 7 0 8 2 0 0
    0 0 2 6 0 9 5 0 0
    8 0 0 2 0 3 0 5 9 ; <-- 5 is a contradiction
    0 0 5 0 1 0 3 0 0))

(define puzzle3
  (list
    8 5 2 3 4 1 6 7 8 ; <-- 8 is a contradiction
    6 7 9 8 2 5 1 4 3
    4 3 1 6 7 9 5 8 2
    3 2 4 1 5 7 9 6 8
    9 8 6 2 3 4 7 5 1
    7 1 5 9 8 6 2 3 4
    1 6 8 5 9 3 4 2 7
    2 9 7 4 6 8 3 1 5
    5 4 3 7 1 2 8 9 0))


(sudoku-checker puzzle3)

#|
;puzzle1 output
"Board"
0 0 0 2 0 0 0 0 0 
0 9 0 0 1 5 6 0 0 
7 6 0 0 0 0 3 0 0 
0 0 3 0 8 0 9 1 0 
0 0 5 4 0 0 0 0 0 
6 0 0 0 0 7 4 3 0 
9 5 0 0 0 6 0 0 0 
0 0 0 0 0 0 0 4 0 
0 3 0 0 0 0 0 0 2 

"Setting row constraints..."
"Setting column constraints..."
"Setting box constraints..."
"Board is valid if no contradictions were shown"
;  ... done
;... done




;puzzle2 output
"Board"
0 0 3 0 2 0 6 0 0 
9 0 0 3 0 5 0 0 1 
0 0 1 8 0 6 4 0 0 
0 0 8 1 0 2 9 0 0 
7 0 0 0 0 0 0 0 8 
0 0 6 7 0 8 2 0 0 
0 0 2 6 0 9 5 0 0 
8 0 0 2 0 3 0 5 9 
0 0 5 0 1 0 3 0 0 

"Setting row constraints..."
"Setting column constraints..."
"Setting box constraints..."
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
"Board is valid if no contradictions were shown"
;  ... done
;... done




;puzzle3 output
"Board"
8 5 2 3 4 1 6 7 8 
6 7 9 8 2 5 1 4 3 
4 3 1 6 7 9 5 8 2 
3 2 4 1 5 7 9 6 8 
9 8 6 2 3 4 7 5 1 
7 1 5 9 8 6 2 3 4 
1 6 8 5 9 3 4 2 7 
2 9 7 4 6 8 3 1 5 
5 4 3 7 1 2 8 9 0 

"Setting row constraints..."
"Setting column constraints..."
"Setting box constraints..."
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
(contradiction (checker-val))
"Board is valid if no contradictions were shown"
|#
