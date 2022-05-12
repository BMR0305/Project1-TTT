#lang racket


;Function to change the game vector to a matrix game
;params i board N M: i is a counter, board is the game vector, N is the number of columns and M the number of rows
(define (moves-into-matrix i board N M)
  (cond
    ((equal? i M) empty)
    (else (cons (drop (take board (* i N)) (* (- i 1) N)) (moves-into-matrix (+ i 1) board N M) ))
    )
  )

;Function to define the horizontal winner lines
;params matrix current_row current_col: matrx is the game, current_row and current_col are is the positon in the matrix to be checked
(define (try-horizontal matrix current_row current_col)
  (cond
    ((>= (+ 2 current_col) (length (list-ref matrix 0)) ) empty)
    (else (list (+ (* (length (list-ref matrix 0)) current_row) current_col)
                (+ 1 (+ (* (length (list-ref matrix 0)) current_row) current_col))
                (+ 2 (+ (* (length (list-ref matrix 0)) current_row) current_col))))
   )
  )

;Function to define the vertical winner lines
;params matrix current_row current_col: matrx is the game, current_row and current_col are is the positon in the matrix to be checked
(define (try-vertical matrix current_row current_col)
  (cond
    ((>= (+ 2 current_row) (length matrix)) empty) 
    (else (list (+ (* (length (list-ref matrix 0)) current_row) current_col)
                (+ (length (list-ref matrix 0)) (+ (* (length (list-ref matrix 0)) current_row) current_col))
                (+ (* 2 (length (list-ref matrix 0))) (+ (* (length (list-ref matrix 0)) current_row) current_col))))
   )
  )

;Function to define the diagonal winner lines
;params matrix current_row current_col: matrx is the game, current_row and current_col are is the positon in the matrix to be checked
(define (try-diagonal_1 matrix current_row current_col)
  (cond
    ((or (>= (+ 2 current_row) (length matrix))(>= (+ 2 current_col) (length (list-ref matrix 0)))) empty) 
    (else (list (+ (* (length (list-ref matrix 0)) current_row) current_col)
                (+ 1 (length (list-ref matrix 0)) (+ (* (length (list-ref matrix 0)) current_row) current_col))
                (+ 2 (* 2 (length (list-ref matrix 0))) (+ (* (length (list-ref matrix 0)) current_row) current_col))))
   )
  
  )

;Function to define the diagonal winner lines
;params matrix current_row current_col: matrx is the game, current_row and current_col are is the positon in the matrix to be checked
(define (try-diagonal_2 matrix current_row current_col)
  (cond
    ((or (>= (+ 2 current_row) (length matrix))(< (- current_col 2) 0)) empty) 
    (else (list (+ (* (length (list-ref matrix 0)) current_row) current_col)
                (- (+ (length (list-ref matrix 0)) (+ (* (length (list-ref matrix 0)) current_row) current_col)) 1)
                (- (+ (* 2 (length (list-ref matrix 0))) (+ (* (length (list-ref matrix 0)) current_row) current_col)) 2)))
   )
  
  )

;Function to traverse the columns of the matrix in the given row and return the winner lines
;params matrix current_row current_col: matrix of the game, current_row and current_col is the position that is been traverse
(define (traverseMatrix-aux matrix current_row current_col)
  (cond
    ((equal? current_col (length (list-ref matrix 0))) (traverseMatrix matrix (+ 1 current_row) 0))
    (else (append (list (try-horizontal matrix current_row current_col) (try-vertical matrix current_row current_col) (try-diagonal_1 matrix current_row current_col)
                        (try-diagonal_2 matrix current_row current_col)) (traverseMatrix-aux matrix current_row (+ 1 current_col))))
    )
  )


;Function to traverse the rows of the matrix in the given row
;params matrix current_row current_col: matrix of the game, current_row and current_col is the position that is been traverse
(define (traverseMatrix matrix current_row current_col)
  (cond
    ((equal? current_row (length matrix)) empty)
    (else (traverseMatrix-aux matrix current_row current_col))
    )
  )

;Function to define a list of the winner lines
;params N M: the columns and rows of the matrix of the game
(define (lines-list N M) 
  (filter pair? (traverseMatrix
 (append (list (take (vector->list (make-vector (* N M) 1)) N)) (moves-into-matrix 2 (vector->list (make-vector (* N M) 2)) N M)
  (list (drop (vector->list (make-vector (* N M) 3)) (* (- M 1) N))))
  0 0))
 )
 
;Function to create an empty game
;params N M: dimensions of the game
(define (make-empty-game N M)
   (make-vector (* N M) empty))


;Function to count how many times a player has played in the game
;params a-game symb: a-game is the game where to be checked and symb is the symbol that needs to be counted
(define (count-plays a-game symb)
  (vector-count (lambda (s) (equal? s symb)) a-game))


;Function to determine the next player to play in the game assuming that the human always start (X)
;params a-game: the gamew where to be checked
(define (determine-next-to-play a-game)
  (if (> (count-plays a-game 'X) (count-plays a-game 'O)) 'O 'X))

;Function to makes a new vecto based in the input vector and replaces the element at pos by v before returning it.
;params vec pos v: vec is a vector used as a base, pos is the position that needs to be need changed and v is the element that needs to be inserted
(define (vector-copy-and-replace vec pos v)
  (vector-append (vector-take vec pos) (vector v) (vector-drop vec (+ pos 1))))

;Function to create a copy of the game with symb at pos.
;params a-game symb pos: a-game is the state of the game, symb is the symbol that needs to be placed and pos is the position where the symbol needs to be placed
(define (game-after-replacing a-game symb pos)
  (vector-copy-and-replace a-game pos symb))

  
;Function to create the list of games generated by replacing symb at the positions in the pos-list. Function candidates
;params a-game symb pos-list: a-game is the state of the game, symb is the symbol that needs to be placed and pos-list is the list of posible positions for the symbol
(define (list-games-after-replacing a-game symb pos-list )
  (cond
    ((empty? pos-list) null)
    (else (cons (game-after-replacing a-game symb (car pos-list)) (list-games-after-replacing a-game symb (cdr pos-list)) ))
    )
  )

;Function that detects if the adjacents positions of a positio on the matrix of the game are not empty
;params current-row current-col a-game N M: current-row and current-col represent the current position that is been checked, N and M represent the dimensions of the game and
;a-game is the state of the game
(define (check-adjacents current-row current-col a-game N M)
  (cond
    ((or
      (and (< (+ 1 current-col) N) (symbol? (list-ref (list-ref a-game current-row) (+ 1 current-col))))                     
      (and (>= (- current-col 1) 0) (symbol? (list-ref (list-ref a-game current-row) (- current-col 1))))
      (and (< (+ 1 current-row) M) (symbol? (list-ref (list-ref a-game (+ 1 current-row)) current-col)))
      (and (>= (- current-row 1) 0) (symbol? (list-ref (list-ref a-game (- current-row 1)) current-col)))
      (and (>= (- current-row 1) 0) (>= (- current-col 1) 0) (symbol? (list-ref (list-ref a-game (- current-row 1)) (- current-col 1))))
      (and (>= (- current-row 1) 0) (< (+ 1 current-col) N) (symbol? (list-ref (list-ref a-game (- current-row 1)) (+ current-col 1))))
      (and (< (+ 1 current-row) M) (>= (- current-col 1) 0) (symbol? (list-ref (list-ref a-game (+ current-row 1)) (- current-col 1))))
      (and (< (+ 1 current-row) M) (< (+ 1 current-col) N) (symbol? (list-ref (list-ref a-game (+ current-row 1)) (+ current-col 1))))
         )#t)
    (else #f)
    )
  )


;Function that detects if a position in the matrix of the game is viable to be checked, traversing in the columns of a certain row
;params current-row current-col a-game N M: current-row and current-col represent the current position that is been checked, N and M represent the dimensions of the game and
;a-game is the state of the game
(define (viable-pos-aux current-row current-col a-game N M)
  (cond
    ((equal? current-col N) (viable-pos (+ 1 current-row) 0 a-game N M))
    ((empty? (list-ref (list-ref a-game current-row) current-col)) (if (check-adjacents current-row current-col a-game N M)
                                                                     (cons (+ (* N current-row) current-col)(viable-pos-aux current-row (+ current-col 1) a-game N M) )
                                                                     (viable-pos-aux current-row (+ current-col 1) a-game N M)
                                                                     )
                                                                   ) 
    (else (viable-pos-aux current-row (+ current-col 1) a-game N M))
    )
  )

;Function that detects if a position in the matrix of the game is viable to be checked, traversing in the rows of the game
;params current-row current-col a-game N M: current-row and current-col represent the current position that is been checked, N and M represent the dimensions of the game and
;a-game is the state of the game
(define (viable-pos current-row current-col a-game N M)
  (cond
    ((equal? current-row M) empty)
    (else (viable-pos-aux current-row current-col a-game N M))
    )

  )


;Function to create a list with all the possible games a player can generate in his next move. Function candidates
;params a-game symb N M : a-game is the state of the game, symb is the symbol that needs to be replace in the positions, N and M are the dimensions of the game
(define (derive-games-for-player a-game symb N M)
  (list-games-after-replacing a-game symb
                              (viable-pos 0 0
                                          (append (list (take (vector->list a-game) N)) (moves-into-matrix 2 (vector->list a-game) N M)
                                                  (list (drop (vector->list a-game) (* (- M 1) N)))) N M)
   )

  
  ;(list-games-after-replacing a-game symb (filter (lambda (i) (empty? (vector-ref a-game i))) (range (* N M))))
 )


;Function that  Enumerates all games that may be derived from the specified game in a single turn. Function candidates
;params game N M: game represents the state of the game and N and M are the dimensions
(define (derive-games game N M)
  (derive-games-for-player game (determine-next-to-play game) N M))


;Function to check if the positions a, b, and c of the vector are equal symbols.
;params a-vector a b c: a-vector is the vector that represents the game, a b c are the positions to be checked
(define (three-equal-symbols? a-vector a b c)
  (and (symbol? (vector-ref a-vector a))
       (equal? (vector-ref a-vector a) (vector-ref a-vector b))
       (equal? (vector-ref a-vector b) (vector-ref a-vector c))))

; Either returns the symbol of the outcome or empty if no one won.
; Evaluates the game for the specified list of triplets.

;Function to evaluate if there has been an outcome using the winner lines, if there has been one it returns the winner. Function solution
;params a-game triplet-list current-result: a-game is the state of the game, triplet-list is the list of winner lines and current result is the result found for each game evaluated
(define (game-evaluate-triplets a-game triplet-list current-result)
  (cond
    ((empty? triplet-list) current-result)
  (else(if (empty? current-result)
        (if (three-equal-symbols? a-game (first (car triplet-list)) (second (car triplet-list)) (third (car triplet-list)))
          (game-evaluate-triplets a-game (cdr triplet-list) (vector-ref a-game (first (car triplet-list))))
          (game-evaluate-triplets a-game (cdr triplet-list) empty))
        current-result))))


;Function that evaluates the game for results in all valid direction, if there is a winner returns the symbol of the winner, if tehre is a tie it returns the tie symbol or it
;returns empty if no winner is found. Function solution
;params a-game N M: a-game is the state of the game and N and M are the dimensions of the matrix of the game
(define (game-evaluate a-game N M)
    (cond
      [(and (empty? (game-evaluate-triplets a-game (lines-list N M) empty)) (or (game-full? a-game) (> N 3) (> M 3))) 'T]
      [else (game-evaluate-triplets a-game (lines-list N M) empty)]))

;Auxiliary function that helps in the process of assigning the score of the posbile games
;params derive-game N M: derive game is the state of the derived game and N and M are the dimensions of the matrix of the game
(define (evaluate-derive-games derive-game N M)
  (cond
    ((empty? derive-game) null)
    (else (cons (* 0.9 (game-score (car derive-game) N M)) (evaluate-derive-games (cdr derive-game) N M) ))
    )
  )

;Function that evaluates all the posibles games and assigns a score to each one. Function factibilidad
;params a-game N M: a-game is the state of the gmae and N and M are the dimensions of the matrix of the game
(define (game-score a-game N M) 
  (cond
    [(equal? (game-evaluate a-game N M) 'X) 100.0]
    [(equal? (game-evaluate a-game N M) 'T) 0.0]
    [(empty? (game-evaluate a-game N M)) (selector-from-player_IA (determine-next-to-play a-game) (evaluate-derive-games (derive-games a-game N M) N M))]
    [else -100.0]))


;Function that checks if one of the players have won the game
;params a-game N M: a-game is the state of the game and N and M are the dimensions of the matrix of the game
(define (game-won? a-game N M)
  (or (equal? 'X (game-evaluate a-game N M)) (equal? 'O (game-evaluate a-game N M))))

; 

;Function that R¿returns whether or not there are no more empty spaces on the board.
;params  a-game: a-game is the state of the game
(define (game-full? a-game)
  (zero? (vector-count empty? a-game)))


;Function that returns whether or not the game is finished. function object
;params a-game N M: a-game is the state of the game and N and M are the dimensions of the matrix of the game
(define (game-finished? a-game N M)
  ; For the game to be finished, there must be a winner or no more moves left.
  (or (game-won? a-game N M) (game-full? a-game)))

;Function to select the argument of a list that has highest score
;params max score: max is the higher score and score is a list of all scores
(define (argmax_IA max scores)
  (cond
    ((empty? scores) max)
    ((> (car scores) max) (argmax_IA (car scores) (cdr scores)))
    (else (argmax_IA max (cdr scores)))))

;Function to select the argument of a list that has lowest score
;params min score: min is the lowest score and score is a list of all scores
(define (argmin_IA min scores)
  (cond
    ((empty? scores) min)
    ((< (car scores) min) (argmin_IA (car scores) (cdr scores)))
    (else (argmin_IA min (cdr scores)))))


;Function to select the best next move, returns the appropriate game score selector for a player. Function select
;params player scores: player is the actual symbol and scores is a list of all scores
(define (selector-from-player_IA player scores)
  (cond
    [(equal? player 'X) (argmax_IA 0.0 scores)]
    [else (argmin_IA 1000.0 scores)]))

;Function to select the argument of a list that has highest score
;params max score N M: max is the higher score, score is a list of all scores and N M are de dimensions of the matrix
(define (argmax_P max scores N M)
  (cond
    ((empty? scores) max)
    ((> (game-score(car scores) N M) (game-score max N M)) (argmax_P (car scores) (cdr scores) N M))
    (else (argmax_P max (cdr scores) N M))))

;Function to select the argument of a list that has lowest score
;params max score N M: max is the lowest score, score is a list of all scores and N M are de dimensions of the matrix
(define (argmin_P min scores N M)
  (cond
    ((empty? scores) min)
    ((< (game-score(car scores) N M) (game-score min N M)) (argmin_P (car scores) (cdr scores) N M))
    (else (argmin_P min (cdr scores) N M))))


;Function to select the appropiate game score selector for a player. Function select
;params player scores: player is the actual symbol, scores is a list of all scores and N M are de dimensions of the matrix
(define (selector-from-player_P player scores N M)
  (cond
    [(equal? player 'X) (argmax_P (car scores) scores N M)]
    [else (argmin_P (car scores) scores N M)]))


;Function to execute the play made by the computer
;params a-game N M: a-game is the state of the game and N M are de dimensions of the matrix
(define (play a-game N M)
  (cond
    ((not (game-finished? a-game N M)) (selector-from-player_P (determine-next-to-play a-game)  (derive-games a-game N M) N M))
    (else a-game)))              
    
    
;This are the function to provide to the GUI
(provide vector-copy-and-replace
         game-score
         game-finished?
         game-evaluate
         make-empty-game
         play)

