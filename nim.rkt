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
  (printf "----------\n")
  (pb board 0);print the board
  (if (null? (flatten board))(begin (printf "Player ~a Won!" (bitwise-xor who 1))
             (bitwise-xor who 1)) ;if we have empty board, the other player won.
  (begin (printf "Player ~a's turn.\n" who)    
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
  (define (mx l i m) (if (eq? m (car l)) i;we found it!
                         (mx (cdr l)(1+ i)m);dig down further looking...
                    ))
  (mx lst 0 (apply max lst));start looking at index 0.
)
(define (list- a b);listwise subtraction. Subtracts each element in b from the corresponding element in b.
  (if (null? b)a ; they don't need to be the same size
      (if (null? a)(map - b)
          (cons (- (car a)(car b))(list- (cdr a)(cdr b)))
          ))
)

;;;;;;;;;AUXILARY FUNCTIONS FOR THE pick FUNCTION;;;;;;;;;;
(define (prm string); function to prompt for stuff.
  (printf "Enter ~s: " string)(noalpha(read))
)
(define (noalpha char);makes sure the read thing is a number
  (if (number? char) char (prm "a number, PLEASE"))
)
(define (rn row board);makes a random move, given a random row and the board.
  (if (null? (list-ref board row))(rn (random (length board)) board);reject empty row.
      (list row (1+(random (length(list-ref board row))))) ;make a move in non-empty row.
  )
)
(define (smart board) ; makes an optimal move given a board.  If there is no optimal move, tries random move in fist row.
  (if (eqv? 0(apply max (smart-row (numify board))))(rn 0 board)
  (list (maxat (smart-row (numify board)))(apply max (smart-row (numify board)))))
)
(define (smart-row board) ;takes a numify'ed board and makes a list whose max is the ideal row.
  (list- board(map (lambda (x)(bitwise-xor (apply bitwise-xor board) x)) board))
)
;;;;;;
(define (pick player board); Make a move based on player type.
  (cond [(equal? player 'human)(list(prm "a Row")(prm "number to remove: "))]
        ;makes a move from the user-supplied row, then number to remove.
        
        [(equal? player 'random)(rn (random (length board))board)];randomly picks row, then calls rn to pick num.
        [(equal? player 'smart)(smart board)];call the AI algorithm.
        [error "bad player type"] 
))

(define (rm lst count);takes a flat list and removes count X's (helper for rem)
  (if (eqv? count 0) lst 
      (rm (remove 'X lst) (- count 1))
  )
)
(define (rem lst move); Removes the move from the list lst.
  (cond [(or(< (car move) 0)(> (car move) (-(length lst)1)))
            (display "Bad Row.\n")(rem lst (pick 'human lst))];and try again
        [(> (cadr move) (length (list-ref lst (car move))))
            (display "Not enough Sticks there.\n")(rem lst (pick 'human lst))];and try again
        [(< (cadr move) 1)(display "Must remove at least 1 stick.\n")(rem lst (pick 'human lst))];and try again
        [(eqv? (car move) 0) (cons (rm (car lst) (cadr move)) (cdr lst))];base case
        [(cons (car lst) (rem (cdr lst) (list(- (car move) 1) (cadr move))))]
  )
)


(define board '((X X X X X X) (X X X X X) (X) (X X)(X X X)));start board

;loren.blaney@gmail.com  email source when done.
