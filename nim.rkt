(define (nim board players)
  (if (null? board)(error "The first argument should be a non-empty board")(pb board 0));display the board
  (if (not(eq?(length players)2)) (error "There should be exactly 2 players."))
  (play board players 0);"loop" to keep doing turns, starting with player 0.
)
(define (pb brd row) ;function to print the board, starting with row
  [if (null? brd)(display " ")(begin 
    (display (concat "Row " (number->string row) ": "))
    (display (car brd)) 
    (newline)
    (pb (cdr brd) (+ row 1)))])
(define (play board players who)
  (rem board (p-row (list-ref players who)) (p-num(list-ref players who)));remove the one they chose
)
(define (p-row player)(cond
   [(equal? player 'human)(display "Enter a Row:")(read)]
   [(equal? player 'random)()]
   [(equal? player 'smart)()]
   ))
(define (p-num player)(cond
   [(equal? player 'human)(display "Enter number to remove:")(read)]
   [(equal? player 'random)()]
   [(equal? player 'smart)()]
   ))
(define (rem lst row count); Removes count elements from the row'th sub-list of the list lst.
  '(not implemented)
  )
 
(nim '((X X) (X) (X X) (X)) '(human human)); FOR TESTING ONLY