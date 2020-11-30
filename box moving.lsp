; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 


; Helper function for goal-test
; Check whether there's any box on a certain row r
(defun findBoxR (row)
	; if empty, then not found box
	(cond ((not row) nil) 
		(t (or (isBox (car row)) (findBoxR (cdr row))))
		)
	)


; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
	; if s empty, then there's no box found on any row
    (cond ((not s) t) 
  	; if there's box found on any row, then s is not goal state
  	(t (and (not (findBoxR (car s))) (goal-test (cdr s)))
  ))
  );end defun



; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; Helper function for get-square
; It extracts a certain row with index "row" from state S
; Arguments: S is the input state, row is the index of the row we want to extract
(defun extractRow (S row)
	; if row less than 0 (out of scope), return wall 
	(cond ((< row 0) wall)
		; if nothing left in S, return wall
		((not S) wall)
		; if zeroth row, return first element of S
		((= row 0) (car S))
		(t (extractRow (cdr S) (- row 1)))
	)
   )

; Helper function for get-square
; It extracts a certain element from a row
; Arguments: r is the input row, col is the column the element is in
(defun extractElementFromRow (r col)
	; if nothing left in the row r, return wall
	(cond ((not r) wall)
		; if col less than 0, return wall
		((< col 0) wall)
		; if zeroth column, return first element of r
		((= col 0) (car r))
		(t (extractElementFromRow (cdr r) (- col 1)))
	)
   )


; Helper function for next-states
; It returns the integer content of state S at square (row,col)
; Arguments: row is the row number, col is the column number
(defun get-square (S row col)
	; if fail to extract row r, return wall
	(cond ((equal (extractRow S row) wall) wall)
		(t (extractElementFromRow (extractRow S row) col))
	 )
	)

; Helper function for set-square
; It sets a certain element of the input row to value v
; Arguments: r is the input row, col is the column number, v is the value to be set
(defun setRow (r col v)
	; if column number is 0, set the first element to be v
	(cond ((= col 0) (cons v (cdr r)))
		(t (cons (car r) (setRow (cdr r) (- col 1) v)))
	 )
	)

; Helper function for next-states
; It sets a certain square to value v
; Arguments: S is the input state, row is the row number, col is the column number, v is the value
(defun set-square (S row col v)
	; if the row number is 0, modify first row and combine it back
	(cond ((= row 0) (cons (setRow (car S) col v) (cdr S)))
		(t (cons (car S) (set-square (cdr S) (- row 1) col v)))
	 )
	)

; Helper function for next-states
; It explores all directions and returns the corresponding result states
; Arguments: S is the input state, D is the direction
(defun try-move (S D)
	(let* ((position (getKeeperPosition S 0)) (KeeperRow (second position)) (KeeperCol (first position)))
		(cond   
		    ; if moving UP
			((equal D 'UP) 
				; UP1 is the object above keeper, UP2 is the object above UP1
				(let ((UP1 (get-square S (- KeeperRow 1) KeeperCol)) (UP2 (get-square S (- KeeperRow 2) KeeperCol)))
					(cond
						; if there's wall UP
						((isWall UP1) nil)
						; if there's box(boxStar) UP and a wall right above the box
						((and (or (isBox UP1) (isBoxStar UP1)) (isWall UP2)) nil)
						; if there are two boxes(boxStars) UP
						((and (or (isBox UP1) (isBoxStar UP1)) (or (isBox UP2) (isBoxStar UP2))) nil)
						; if UP is blank
						((isBlank UP1) (set-square (set-square S (- KeeperRow 1) KeeperCol keeper) KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)))
						; if UP is box
						((isBox UP1) (cond 
							; if UP2 is blank
							((isBlank UP2) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) (- KeeperRow 1) KeeperCol keeper) (- KeeperRow 2) KeeperCol box))
							; if UP2 is star
							((isStar UP2) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) (- KeeperRow 1) KeeperCol keeper) (- KeeperRow 2) KeeperCol boxstar))
							)
						)
						; if UP is star
						((isStar UP1) (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) (- KeeperRow 1) KeeperCol keeperstar))
						; if UP is boxstar
						((isBoxStar UP1) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) (- KeeperRow 1) KeeperCol keeperstar) (- KeeperRow 2) KeeperCol box))
				    )
				)
			)

			; if moving DOWN
			((equal D 'DOWN)
				; DOWN1 is the object down the keeper, DOWN2 is the object down DOWN1
				(let ((DOWN1 (get-square S (+ KeeperRow 1) KeeperCol)) (DOWN2 (get-square S (+ KeeperRow 2) KeeperCol)))
					(cond
						; if there's wall DOWN
						((isWall DOWN1) nil)
						; if there's box(boxStar) DOWN and a wall right below the box
						((and (or (isBox DOWN1) (isBoxStar DOWN1)) (isWall DOWN2)) nil)
						; if there are two boxes(boxStars) DOWN
						((and (or (isBox DOWN1) (isBoxStar DOWN1)) (or (isBox DOWN2) (isBoxStar DOWN2))) nil)
						; if DOWN is blank
						((isBlank DOWN1) (set-square (set-square S (+ KeeperRow 1) KeeperCol keeper) KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)))
						; if DOWN is box
						((isBox DOWN1) (cond 
							; if DOWN2 is blank
							((isBlank DOWN2) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) (+ KeeperRow 1) KeeperCol keeper) (+ KeeperRow 2) KeeperCol box))
							; if DOWN2 is star
							((isStar DOWN2) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) (+ KeeperRow 1) KeeperCol keeper) (+ KeeperRow 2) KeeperCol boxstar))
							)
						)
						; if DOWN is star
						((isStar DOWN1) (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) (+ KeeperRow 1) KeeperCol keeperstar))
						; if DOWN is boxstar
						((isBoxStar DOWN1) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) (+ KeeperRow 1) KeeperCol keeperstar) (+ KeeperRow 2) KeeperCol box))
				    )
				)
			)

			; if moving LEFT
			((equal D 'LEFT)
				; LEFT1 is the object LEFT the keeper, LEFT2 is the object left to LEFT1
				(let ((LEFT1 (get-square S KeeperRow (- KeeperCol 1))) (LEFT2 (get-square S KeeperRow (- KeeperCol 2))))
					(cond
						; if there's wall LEFT
						((isWall LEFT1) nil)
						; if there's box(boxStar) LEFT and a wall left to the box
						((and (or (isBox LEFT1) (isBoxStar LEFT1)) (isWall LEFT2)) nil)
						; if there are two boxes(boxStars) LEFT
						((and (or (isBox LEFT1) (isBoxStar LEFT1)) (or (isBox LEFT2) (isBoxStar LEFT2))) nil)
						; if LEFT is blank
						((isBlank LEFT1) (set-square (set-square S KeeperRow (- KeeperCol 1) keeper) KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)))
						; if LEFT is box
						((isBox LEFT1) (cond 
							; if LEFT2 is blank
							((isBlank LEFT2) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) KeeperRow (- KeeperCol 1) keeper) KeeperRow (- KeeperCol 2) box))
							; if LEFT2 is star
							((isStar LEFT2) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) KeeperRow (- KeeperCol 1) keeper) KeeperRow (- KeeperCol 2) boxstar))
							)
						)
						; if LEFT is star
						((isStar LEFT1) (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) KeeperRow (- KeeperCol 1) keeperstar))
						; if LEFT is boxstar
						((isBoxStar LEFT1) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) KeeperRow (- KeeperCol 1) keeperstar) KeeperRow (- KeeperCol 2) box))
				    )
				)
			)

			; if moving RIGHT
			((equal D 'RIGHT)
				; RIGHT1 is the object RIGHT the keeper, RIGHT2 is the object RIGHT to RIGHT1
				(let ((RIGHT1 (get-square S KeeperRow (+ KeeperCol 1))) (RIGHT2 (get-square S KeeperRow (+ KeeperCol 2))))
					(cond
						; if there's wall right
						((isWall RIGHT1) nil)
						; if there's box(boxStar) RIGHT and a wall at right to the box
						((and (or (isBox RIGHT1) (isBoxStar RIGHT1)) (isWall RIGHT2)) nil)
						; if there are two boxes(boxStars) at right
						((and (or (isBox RIGHT1) (isBoxStar RIGHT1)) (or (isBox RIGHT2) (isBoxStar RIGHT2))) nil)
						; if RIGHT is blank
						((isBlank RIGHT1) (set-square (set-square S KeeperRow (+ KeeperCol 1) keeper) KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)))
						; if RIGHT is box
						((isBox RIGHT1) (cond 
							; if RIGHT2 is blank
							((isBlank RIGHT2) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) KeeperRow (+ KeeperCol 1) keeper) KeeperRow (+ KeeperCol 2) box))
							; if RIGHT2 is star
							((isStar RIGHT2) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) KeeperRow (+ KeeperCol 1) keeper) KeeperRow (+ KeeperCol 2) boxstar))
							)
						)
						; if RIGHT is star
						((isStar RIGHT1) (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) KeeperRow (+ KeeperCol 1) keeperstar))
						; if RIGHT is boxstar
						((isBoxStar RIGHT1) (set-square (set-square (set-square S KeeperRow KeeperCol (if (isKeeperStar (get-square S KeeperRow KeeperCol)) star blank)) KeeperRow (+ KeeperCol 1) keeperstar) KeeperRow (+ KeeperCol 2) box))
				    )
				)
			)

		)
	  )
	)



; It returns all successors of S
(defun next-states (S)
  ; (let* ((pos (getKeeperPosition s 0))
	 ; (x (car pos))
	 ; (y (cadr pos))
	 ; ;x and y are now the coordinate of the keeper in s.
	 ; (result (list (try-move s 'UP) (try-move s 'RIGHT) (try-move s 'DOWN) (try-move s 'LEFT))))
  ;   (cleanUpList result);end
  ;  );end let
   (cleanUpList (list (try-move S 'UP) (try-move S 'RIGHT) (try-move S 'DOWN) (try-move S 'LEFT)))
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; Helper function for h1
; It returns the number of misplaced boxes in a certain row
; Arguments: r is the input row
(defun misplacedInRow (r)
	(cond 
		((not r) 0)
		((atom r) (if (equal r box) 
			1
			0))
		(t (+ (misplacedInRow (car r)) (misplacedInRow (cdr r))))
		)
	)
 

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun h1 (s)
	(cond
		((not s) 0)
		; if only one list left
		((atom (car s)) (misplacedInRow s))
		(t (+ (misplacedInRow (car s)) (h1 (cdr s))))
	)

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Helper function for h904925677
; Find the Manhattan distance between two squares
(defun distance (firstpt secondpt)
	(+ (abs (- (first firstpt) (first secondpt))) (abs (- (second firstpt) (second secondpt))))
	)

; Helper function for h904925677
; Find the shortest distance between a coordinate and a list of coordinates
; Arguments: Coord is one square, CoordList is a list of squares
(defun shortestDis (Coord CoordList)
	(cond 
		((not CoordList) 1000)
		; if there's only one element in CoordList
		((= (length CoordList) 1) (distance Coord (car CoordList)))
		((atom (car CoordList)) (distance Coord CoordList))
		; if there are more than one element
		(t (if (< (shortestDis Coord (car CoordList)) (shortestDis Coord (cdr CoordList)))
			(shortestDis Coord (car CoordList))
			(shortestDis Coord (cdr CoordList))
		   )
		)
	)
   )


; Helper function for h904925677
; Coordinates are in the form (c r)
; Arguments: boxList is a list of boxes, GoalList is a list of goals
(defun sumDis (boxList GoalList)
	(cond
		((or (not boxList) (not GoalList)) 0)
		; if there's one element in boxList
		((= (length boxList) 1) (shortestDis (car boxList) GoalList))
		((atom (car boxList)) (shortestDis boxList GoalList))
		; if there are more than 1 element in boxList
		(t (+ (sumDis (car boxList) GoalList) (sumDis (cdr boxList) GoalList)))
   )
)

; Helper function for getGoalList
; Get all goals in a row r
; Arguments: r is the input row
(defun getGoalListInRow (r row col)
	(cond
		((not r) ())
		; if only one element in row r
		((atom r) (if (or (isStar r) (isKeeperStar r)) (list col row) ()))
		(t (append (if (not (getGoalListInRow (car r) row col)) 
			nil
			(list (getGoalListInRow (car r) row col)))
			(getGoalListInRow (cdr r) row (+ col 1)))
		   )
		)
	)


; Helper function for h904925677
; Get a coordinate list of goals from a state S
; Arguments: S is the input state
(defun getGoalList (S row col)
	(cond
		((not S) ())
		; if only one row in S
		((atom (car S)) (getGoalListInRow S row col))
		((= (length S) 1) (getGoalListInRow (car S) row col))
		(t (cleanUpList (append (getGoalListInRow (car S) row col) (getGoalList (cdr S) (+ row 1) col))))
	)
   )

; Helper function for getBoxList
; Get the all boxes in a row r
; Arguments: r is the input row
(defun getBoxListInRow (r row col)
	(cond
		((not r) ())
		; if only one element in row r
		((atom r) (if (isBox r) (list col row) ()))
		(t (append (if (not (getBoxListInRow (car r) row col)) 
			nil
			(list (getBoxListInRow (car r) row col)))
			(getBoxListInRow (cdr r) row (+ col 1)))
		   )
		)
	)


; Helper function for h904925677
; Get all boxes from a state S
; Arguments: S is the input state
(defun getBoxList (S row col)
	(cond
		((not S) ())
		; if there's only one row in S
		((atom (car S)) (getBoxListInRow S row col))
		((= (length S) 1) (getBoxListInRow (car S) row col))
		(t (cleanUpList (append (getBoxListInRow (car S) row col) (getBoxList (cdr S) (+ row 1) col))))
	)
   )

; Helper function for boxBlocked
; Test whether x is a blocker: either wall, box, or boxstar
(defun isBlocker (x)
	(or (isWall x) (isBox x) (isBoxStar x))
	)

; Helper function for h904925677
; Determine if a box is unable to move
; Arguments: B is coordinate of box, S is state
(defun boxBlocked (B S)
	(let ((col (first B)) (row (second B)))
		(cond
			((not B) nil)
			; UP and LEFT
			((and (isBlocker (get-square S (+ row 1) col)) (isBlocker (get-square S row (- col 1)))) t)
			; UP and RIGHT
			((and (isBlocker (get-square S (+ row 1) col)) (isBlocker (get-square S row (+ col 1)))) t)
			; DOWN and LEFT
			((and (isBlocker (get-square S (- row 1) col)) (isBlocker (get-square S row (- col 1)))) t)
			; DOWN and RIGHT
			((and (isBlocker (get-square S (- row 1) col)) (isBlocker (get-square S row (+ col 1)))) t)
			(t nil)
		)
	)
   )

; Helper function for h904925677
; Test if any box is blocked in state S
(defun anyBlocked (S Boxes)
	(cond
		((not Boxes) nil)
		((atom (car Boxes)) (boxBlocked Boxes S))
		((= (length Boxes) 1) (boxBlocked (car Boxes) S))
		((or (anyBlocked S (car Boxes)) (anyBlocked S (cdr Boxes))) t)
		(t nil)
		)
   )

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h904925677 (S)
	(let* ( (boxlist (getBoxList S 0 0)) )
		(cond
			; if S is goal state
			((goal-test S) 0)
			; if any box is blocked
			((anyBlocked S boxlist) 100)
			(t (sumDis boxlist (getGoalList S 0 0)))
		)
	)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))


;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
