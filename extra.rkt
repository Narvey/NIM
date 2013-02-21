;;;;;;;;;AUXILIARY FUNCTIONS THAT MAY HAVE EXTERNAL USEFULNESS;;;;;;;;;;
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