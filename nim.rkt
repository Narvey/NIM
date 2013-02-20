(define (nim board players)
  (if (null? board)(error "The first argument should be a non-empty board"));check board
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
  (pb board 0);print the board
  (display "Player")(display who)(display "'s turn.")(newline)
  (play (rem board (p-row (list-ref players who)) (p-num(list-ref players who))) players (bitwise-xor who 1))
  ;remove the one they chose, then switch players (via the xor).
)
(define (p-row player)(cond   ; pick row function
   [(equal? player 'human)(display "Enter a Row: ")(read)]
   [(equal? player 'random)()]
   [(equal? player 'smart)()]
   ))
(define (p-num player)(cond   ; pick number to remove function
   [(equal? player 'human)(display "Enter number to remove: ")(read)]
   [(equal? player 'random)()]
   [(equal? player 'smart)()]
   ))
(define (rem lst row count); Removes count elements from the row'th sub-list of the list lst.
  (if(eqv? row 0)(cons (rm (car lst) count) (cdr lst))
   (cons (car lst) (rem (cdr lst) (- row 1) count))
  )
)
(define (rm lst count);takes a flat list and removes count X's
  (if (eqv? count 0) lst 
      (rm (remove 'X lst) (- count 1))
  )
)
;(map (lambda (x)(bitwise-xor 1 x)) '(7 1 2 3 5))

(nim '((X X X) (X X X X X X) (X X) (X)) '(human human)); FOR TESTING ONLY

#| XOR them all together with apply
if the result is not zero, then XOR with each element, to build another list. 
Subtract each XOR from each original element 
|#

;player who receives an empty board loses; check right before you prompt.

;loren.blaney@gmail.com  email source when done.