(define (pb brd) ;function to print the board
  [if (null? brd)(display " ")(begin 
    (display (car brd))
    (newline)
    (pb (cdr brd)))])
(define (nim board players)
  (pb board)
  (play players)
  (display (car players))
)