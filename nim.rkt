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
(define (play board players who) ; This function plays a turn (human or otherwise)
  (pb board 0);print the board
  (if (null? (flatten board)) 
      (begin (display "Player ")(display (bitwise-xor who 1))(display " Won!")
             (bitwise-xor who 1)) ;if we have empty board, the other player won.
  (begin (display "Player ")(display who)(display "'s turn.")(newline)     
  (play (rem board (pick (list-ref players who) board)) players (bitwise-xor who 1))
  ;remove the one they chose, then switch players (via the xor).
)))

;;;;;;;;;AUXILARY FUNCTIONS THAT MAY HAVE EXTERNAL USEFULNESS;;;;;;;;;;
(define (numify board);replaces each sub-list with it's length.
  (if (null? board)board
      (cons (length (car board)) (numify (cdr board)))
  )
)


(define (maxat lst); returns the index of the maximum value in lst.
  (define (mx l i m) (if (eq? m (car lst)) i;we found it!
                         (begin (newline)
                         (mx (cdr l)(+ i 1)m))
                    ))
  (mx lst 0 (apply max lst))
)

(maxat (list 3 4 56 3 4 2 6))
;;;;;;;;;AUXILARY FUNCTIONS FOR THE pick FUNCTION;;;;;;;;;;
(define (p-row player board); function to prompt for a row
  (display "Enter a Row: ")(read)
)
(define (smart board) ; makes an optimal move given a board.  If there is no optimal move, just does
  (list (map (apply bitwise-xor board))
  )
)
(define (smart-row board) ;takes a numify'ed board and determines the ideal row.
  (map (lambda (x)(bitwise-xor (apply bitwise-xor board) x)) board)
)
(define (pick player board); pick number to remove, combine it with picked row.
  (cond [(equal? player 'human)(list(p-row board)(begin(display "Enter number to remove: ")(read)))]
        ;makes a move from the user-supplied row, then number to remove.
        
        [(equal? player 'random)(rn (random (length board)))];randomly picks row, then calls rn to pick num.
        [(equal? player 'smart)()]
        [error "bad player type"] 
))

(define (rm lst count);takes a flat list and removes count X's (helper for rem)
  (if (eqv? count 0) lst 
      (rm (remove 'X lst) (- count 1))
  )
)
(define (rem lst move); Removes the move from the list lst.
  (cond [(or(< (car move) 0)(> (car move) (-(length lst)1)))
            (display "Bad Row.\n")(rem lst (pick 'human lst))];try again
        [(> (cadr move) (length (list-ref lst (car move))))
            (display "Not enough Sticks there.\n")(rem lst (pick 'human lst))];try again
        [(eqv? (car move) 0) (cons (rm (car lst) (cadr move)) (cdr lst))]
        [(cons (car lst) (rem (cdr lst) (list(- (car move) 1) (cadr move))))]
  )
)


(define board '((X X X) (X X X X X X) (X X) (X)))

#| XOR them all together with apply
if the result is not zero, then XOR with each element, to build another list. 
Subtract each XOR from each original element 
|#

;loren.blaney@gmail.com  email source when done.
