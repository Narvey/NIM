(require racket/list); for some functions such as flatten.

(define (nim board players)
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
  (if (null? (flatten board)) 
      (begin (display "Player ")(display (bitwise-xor who 1))(display " Won!")
             (bitwise-xor who 1)) ;if we have empty board, the other player won.
  (begin (display "Player ")(display who)(display "'s turn.")(newline)     
  (play (rem board (p-row (list-ref players who) board) (p-num (list-ref players who) board)) players (bitwise-xor who 1))
  ;remove the one they chose, then switch players (via the xor).
)))
(define (p-row player board)(cond   ; pick row function
   [(equal? player 'human)(display "Enter a Row: ")(read)]
   [(equal? player 'random)()]
   [(equal? player 'smart)()]
   [error "bad player type"]
   ))
(define (p-num player board)(cond   ; pick number to remove function
   [(equal? player 'human)(display "Enter number to remove: ")(read)]
   [(equal? player 'random)()]
   [(equal? player 'smart)()]
   [error "bad player type"]
   ))
(define (rem lst row count); Removes count elements from the row'th sub-list of the list lst.
  (cond [(eqv? row 0)(cons (rm (car lst) count) (cdr lst))]
        [(or (< row 0)(> row (length lst)))(display "Bad Row.\n")lst]
        [(> count (length (list-ref lst row))) (display "Not enough Sticks there.\n")lst]
        [cons (car lst) (rem (cdr lst) (- row 1) count)]
  )
)
(define (rm lst count);takes a flat list and removes count X's
  (if (eqv? count 0) lst 
      (rm (remove 'X lst) (- count 1))
  )
)
;(map (lambda (x)(bitwise-xor 1 x)) '(7 1 2 3 5))

;(nim '((X X X) (X X X X X X) (X X) (X)) '(human human))  ; FOR TESTING

#| XOR them all together with apply
if the result is not zero, then XOR with each element, to build another list. 
Subtract each XOR from each original element 
|#

;player who receives an empty board loses; check right before you prompt.

;loren.blaney@gmail.com  email source when done.