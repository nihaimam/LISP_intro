;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                  ;;
;; Project 3: THE 8-PUZZLE REPRESENTATION                                                           ;;
;;                                                                                                  ;;
;; Name     : Niha Imam                                                                             ;;
;; Date     : April 14, 2020                                                                        ;;
;;                                                                                                  ;;
;; report and graphs included at the end of the file                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun make-initial-state (initial-puzzle-situation)
  "Makes an initial state with a given puzzle situation.
   The puzzle situation is simply a list of 9 numbers.
   So to create an initial state with the puzzle
     2 7 4
     9 8 3
     1 5 6
   ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))"
  (cons (concatenate 'simple-vector initial-puzzle-situation
		     (list (position 9 initial-puzzle-situation))) nil))


(defun create-random-state (num-moves)
  "Generates a random state by starting with the
   canonical correct puzzle and making NUM-MOVES random moves.
   Since these are random moves, it could well undo previous
   moves, so the 'randomness' of the puzzle is <= num-moves"
  (let ((puzzle #(1 2 3 4 5 6 7 8 9 8)))
    (dotimes (x num-moves)
      (let ((moves (elt *valid-moves* (empty-slot puzzle))))
	(setf puzzle (make-move (elt moves (random (length moves))) puzzle))))
    (build-state puzzle nil)))


(defmacro depth (state)
  "Returns the number of moves from the initial state 
   required to get to this STATE"
  `(1- (length ,state)))


(defmacro puzzle-from-state (state)
  "Returns the puzzle (an array of 10 integers) from STATE"
  `(car ,state))


(defmacro previous-state (state)
  "Returns the previous state that got us to this STATE"
  `(cdr ,state))


(defmacro empty-slot (puzzle)
  "Returns the position of the empty slot in PUZZLE"
  `(elt ,puzzle 9))


(defun swap (pos1 pos2 puzzle)
  "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.
   If POS1 or POS2 is empty, slot 9 is updated appropriately."
  (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
    (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
    (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
    (if (= (elt puz pos1) 9) (setf (empty-slot puz) pos1)  ;; update if pos1 is 9
        (if (= (elt puz pos2) 9) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 9
    puz))


(defparameter *valid-moves*
  #((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7))
  "A vector, for each empty slot position, of all the valid moves that can be made.
   The moves are arranged in lists.")


(defmacro foreach-valid-move ((move puzzle) &rest body)
  "Iterates over each valid move in PUZZLE, setting
   MOVE to that move, then executing BODY.  Implicitly
   declares MOVE in a let, so you don't have to."
  `(dolist (,move (elt *valid-moves* (empty-slot ,puzzle)))
     ,@body))


(defun make-move (move puzzle)
  "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
   If the move is illegal, nil is returned.  Note that this is a PUZZLE,
   NOT A STATE.  You'll need to build a state from it if you want to."
  (let ((moves (elt *valid-moves* (empty-slot puzzle))))
    (when (find move moves) (swap move (empty-slot puzzle) puzzle))))


(defmacro build-state (puzzle previous-state)
  "Builds a state from a new puzzle situation and a previous state"
  `(cons ,puzzle ,previous-state))


(defmacro foreach-position ((pos puzzle) &rest body)
  "Iterates over each position in PUZZLE, setting POS to the
   tile number at that position, then executing BODY. Implicitly
   declares POS in a let, so you don't have to."
  (let ((x (gensym)))
    `(let (,pos) (dotimes (,x 9) (setf ,pos (elt ,puzzle ,x))
			  ,@body))))


(defun print-puzzle (puzzle)
  "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
  (let (lis)
    (foreach-position (pos puzzle)
		      (if (= pos 9) (push #\space lis) (push pos lis)))
    (apply #'format t "~%~A~A~A~%~A~A~A~%~A~A~A" (reverse lis)))
  puzzle)


(defun print-solution (goal-state)
  "Starting with the initial state and ending up with GOAL-STATE,
   prints a series of puzzle positions showing how to get 
   from one state to the other.  If goal-state is 'FAILED then
   simply prints out a failure message"
  ;; first let's define a recursive printer function
  (labels ((print-solution-h (state)
	     (print-puzzle (puzzle-from-state state)) (terpri)
	     (when (previous-state state) (print-solution-h (previous-state state)))))
    ;; now let's reverse our state list and call it on that
    (if (equalp goal-state 'failed)
	(format t "~%Failed to find a solution")
	(progn
	  (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
	  (print-solution-h (reverse goal-state))))))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;;


(defun general-search (initial-state goal-test enqueueing-function &optional (maximum-iterations nil))
  "Starting at INITIAL-STATE, searches for a state which passes the GOAL-TEST
   function.  Uses a priority queue and a history list of previously-visited states.
   Enqueueing in the queue is done by the provided ENQUEUEING-FUNCTION.  Prints 
   out the number of iterations required to discover the goal state.  Returns the 
   discovered goal state, else returns the symbol 'FAILED if the entire search 
   space was searched and no goal state was found, or if MAXIMUM-ITERATIONS is 
   exceeded.  If maximum-iterations is set to nil, then there is no maximum number
   of iterations."

  (let* ((queue (make-empty-queue)) ;make an empty queue
	 (hist (make-hash-table :test #'equalp)) ;make an empty history hash
	 (iter 0) ;keep track of iterations
	 (state initial-state) ;set the initial state
	 (puzz (puzzle-from-state state)) ;set puzzle from state
	 (chld (puzzle-from-state state))) ; set child puzzle from state

    ;enqueue the initial state using the designated enqueueing fuction
    (funcall enqueueing-function state queue)
    ;add the initial puzzle to hash history
    (setf (gethash puzz hist) puzz)

    ;loop through the queue
    (loop
       do
	 (progn
	   ;increment the iteration
	   (incf iter)
	   ;if max iteration is given and current iteration is greater
	   (if (and maximum-iterations (> iter maximum-iterations))
	       ;algorithm failed to find the goal solution
	       (return-from general-search "FAILURE"))
	   ;set the state to the state at the front of the queue
	   (setf state (remove-front queue))
	   ;if the state is our goal state we have found the solution
	   (if (funcall goal-test (car state))
	       (progn
		 ;print the number of iteration required tofind the goal
		 (format t "~%SUCCESS~%Iteration Required: ~d" iter)
		 (return-from general-search state)))
	   ;if not goal state then create puzzle from state
	   (setf puzz (puzzle-from-state state))
	   ;for every valid move
	   (dotimes (idx (length (aref *valid-moves* (empty-slot puzz))))
	     ;create a child using one valid move
	     (setf chld (make-move (nth idx (aref *valid-moves* (empty-slot puzz))) puzz))
	     ;if the child is not is the history hash (not in hash so not in queue)
	     (if (not (gethash chld hist))
		 (progn
		   ;add the child STATE to the queue
		   (funcall enqueueing-function (build-state chld state) queue)
		   ;add the child PUZZLE to the history hash
		   (setf (gethash chld hist) chld))))))))


#|
(defun in-history (puzzle history)
  "helper to check if puzzle is in history"
  (let ((answer nil))
    (dotimes (i (length history))
      (if (equalp (nth i history) puzzle)
	  (setf answer T)))
  (return-from in-history answer)))
|#


(defun goal-p (state)
  "Returns T if state is a goal state, else NIL.  Our goal test."
  ;empty slot should be at end so create a random state with no moves
  (return-from goal-p (equalp state (car (create-random-state 0)))))


(defun dfs-enqueuer (state queue)
  "Enqueues in depth-first order"
  ;add the state to the end of the queue (LIFO)
  (enqueue-at-end queue state))


(defun bfs-enqueuer (state queue)
  "Enqueues in breadth-first order"
  ;add the state to the beginning of the queue (LIFO)
  (if (empty-queue? queue)
      ;if queue is empty set the cdr to null and add to queue
      (progn
	(setf (cdr state) nil)
	(enqueue-at-front queue state))
      ;if queue is not empty set the cdr to the first item in queue and add to queue
      (progn
	(setf (cdr state) (car (queue-front queue)))
	(enqueue-at-front queue state))))                                         


(defparameter *points*
  #((0 0) (0 1) (0 2)  (1 0) (1 1) (1 2)  (2 0) (2 1) (2 2))
  "A position vector to tell which tile belong in which postion.")


(defun nyc (state)
  "helper to calculate manhattan distance"
  (let ((puzz (puzzle-from-state state))
	(sum 0))
    ;using the puzzle from state sum up the distance
    (dotimes (i 9)
      (if (/= (elt puzz i) 9)
	  (setf sum (+ sum (x+y-distance (elt *points* i) (elt *points* (- (elt puzz i) 1)))))))
    (return-from nyc sum)))


(defun manhattan-enqueuer (state queue)
  "Enqueues by manhattan distance"
  ;add the state to the queue using the manhattan distance
  (enqueue-by-priority queue #'nyc state))


(defun num-out (state)
  "helper for num out enqueuer"
  (let ((puzz (puzzle-from-state state))
	(ctr 0))
    ;using the puzzle from state check how many at not in the correct position
    (dotimes (i 10)
      (if (not (equal (aref puzz i) (+ i 1)))
	  (setf ctr (+ ctr 1))))
    (return-from num-out ctr)))


(defun num-out-enqueuer (state queue)
  "Enqueues by number of tiles out of place"
  ;add the state to the queue using the number of tiles out of place
  (enqueue-by-priority queue #'num-out state))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;;



#|

;;; The five test examples.

;;; Solves in 4 moves:
(setf s1 (make-initial-state '(
9 2 3
1 4 6
7 5 8)))

;;; Solves in 8 moves:
(setf s2 (make-initial-state '(
2 4 3
1 5 6
9 7 8)))

;;; Solves in 16 moves:
(setf s3 (make-initial-state '(
2 3 9
5 4 8
1 6 7)))

;;; Solves in 24 moves:
(setf s4 (make-initial-state '(
1 8 9
3 2 4
6 5 7)))

;;; easy or hard to solve?  Why?
(setf s5 (make-initial-state '(
9 2 3
4 5 6
7 8 1)))



|_enqueuer__|s2 # of iterations|s2 # of iterations|s3 # of iterations|s4 # of iterations|s5 # of iterations|
|           |                  |                  |                  |                  |                  |
|    bfs    |  181251-Failure  |  100027-Failure  |   44298-Failure  |   28216-Failure  |  >20000-Failure  |
|    dfs    |        29        |       219        |       9582       |  123945-Failure  |  >20000-Failure  |
|  num-out  |         5        |       111        |        509       |       789        |  >20000-Failure  |
| manhattan |         5        |        11        |        267       |       233        |  >20000-Failure  | 
|___________|__________________|__________________|__________________|__________________|__________________|



DISCLAIMER - some might be wring i had to type them one by one !
manhattan-enqueuer solution of s1
Solution requires 4 moves
 23       123       123       123       123
146  -->   46  -->  4 6  -->  456  -->  456
758       758       758       7 8       78


manhattan-enqueuer solution of s2
Solution requires 10 moves
243       243       243       243       243
156  -->  156  -->  156  -->  15   -->  1 5  -->
 78       7 8       78        786       786

2 3        23       123       123       123       123
145  -->  145  -->   45  -->  4 5  -->  45   -->  456 
786       786       786       786       786       78


manhattan-enqueuer solution of s3
Solution requires 40 moves:
23        2 3        23       523       523
548  -->  548  -->  548  -->   48  -->  4 8  -->  
167       167       167       167       167

523       523       523       523       523
4 8  -->  468  -->  468  -->  46   -->  4 6  -->  
167       1 7       17        178       178

5 3        53       453       453       453
426  -->  426  -->   26  -->  126  -->  126  -->  
178       178       178        78       7 8

453       453       453       4 3       43
126  -->  12   -->  1 2  -->  152  -->  152  -->  
 78       786       786       786       786

432       432       4 2       42        425
15   -->  1 5  -->  135  -->  135  -->  13   -->  
786       786       786       786       786

425       425        25       2 5       25
1 3  -->   13  -->  413  -->  413  -->  413  -->  
786       786       786       786       786

253       253       253       253       253
41   -->  416  -->  416  -->  416  -->   16  -->  
786       78        7 8        78       478

253       2 3        23       123       123
1 6  -->  156  -->  156  -->   56  -->  456  -->
478       478       478       478        78

123       123
456  -->  456
7 8       78

manhattan-enqueuer solution of s4
Solution requires 98 moves:
18        1 8       128       128       128
324  -->  324  -->  3 4  -->   34  -->  634
657       657       657       657        57

128       128       128       128       128
634  -->  634  -->  63   -->  6 3  -->   63
5 7       57        574       574       574

128       128       128       128       128
563  -->  563  -->  563  -->  56   -->  5 6
 74       7 4       74        743       743

1 8       18        186       186       186
526  -->  526  -->  52   -->  523  -->  523
743       743       743       74        7 4

186       1 6       16        163       163
5 3  -->  583  -->  583  -->  58   -->  584
724       724       724       724       72

163       163       163       16        1 6
584  -->  5 4  -->  54   -->  543  -->  543
7 2       782       782       782       782

146       146        46       4 6       46
5 3  -->   53  -->  153  -->  153  -->  153
782       782       782       782       782

463       463       463       463       4 3
15   -->  152  -->  152  -->  1 2  -->  162
782       78        7 8       758       758

43        432       432       432       432
162  -->  16   -->  1 6  -->  156  -->  156
758       758       758       7 8       78

432       43        4 3       453       453
15   -->  152  -->  152  -->  1 2  -->  12 
786       786       786       786       786

453       453       453       4 3        43
126  -->  126  -->  1 6  -->  156  -->  156
78        7 8       728       728       728

143       143       143       143       143
 56  -->  5 6  -->  526  -->  526  -->  52
728       728       7 8       78        786

143       1 3       13        132       132
5 2  -->  542  -->  542  -->  54   -->  546
786       786       786       786       78

132       132       132      132       132
546  -->  5 6  -->   56  --> 756  -->  756 
7 8       748       748       48       4 8

132       132       13        1 3       153
756  -->  75   -->  752  -->  752  -->  7 2
48        486       486       486       486

153       153       153       153       153
72   -->  726  -->  726  -->  726  -->   26
486       48        4 8        48       748

153       153       153       153       153
2 6  -->  246  -->  246  -->  24   -->  2 4
748       7 8       78        786       786

153        53       5 3       523       523
 24  -->  124  -->  124  -->  1 4  -->  14
786       786       786       786       786

52        5 2        52       152       152
143  -->  143  -->  143  -->   43  -->  4 3
786       786       786       786       786

1 2       12        123       123
453  -->  453  -->  45   -->  456
786       786       786       78



Report

This was a fun one, made me think a bit. The neural networks project kind of killed me but this one has 
revived me. It may be due to the fact that I started earlier but who can tell really. When I started 
this project, I was lost. I hated bfs and dfs and trees in general in 310 so I thought this project 
would be the death of me but as it turns out it was actually fun. 

After thinking and sitting around for two days I started the project by implementing the bfs enqueuer 
and the dfs enqueuer. Which required me to go and read the implementation of enqueue at end and enqueue 
and beginning. I realized that enqueue at end sets up the cdr of anything being added to the list but 
enqueue at front does not. This way for dfs-enqueuer all I had to do was enqueue the state to the queue 
and everything else would be taken care of. For bfs-enqueuer I had to set up the cdr of the state to point 
to the front of the queue and then enqueue the function.

Next were the Manhattan and the num out enqueuers. For then I didn’t have to do much just enqueue them 
using helper functions to find the key. To find the Manhattan distance initial I was taking the taking 
the index and dividing (taking the ceiling) it by 3 and then taking the tile number at the index and 
dividing (taking the ceiling) it by and then subtracting them for the x value and then doing the same 
but using od instead of divide for the y value. This way worked but there were some cases where it didn’t 
really work. So, I utilized the x+y-distance to find the distance from their point to their actual point. 
I did this by setting up a vector of points and for each position I took the value of point[position] 
and of point[true position] and found the distance. This method works better and handles all the test cases.

Then it was time to work on goal and general search. For goal I created a random new state with 0 
random moves so it’s the goal state and then I return whether or not it is equal to the incoming state.

For general search I tried to follow the pseudo code that the professor had given us. I would take 
the state, queue and history, get the children of state and if they not in history add them and then do 
the entire process over again. Since in the beginning I was using a history list I had a helper function 
to help me check if the history was in the list which killed my runtime. So, I added a hash table. But 
then I hit another speed bump, the hash table would differentiate between keys even if they were same, 
so I found that using equalp is better. 

In the end I found out most the solutions but most of the bfs ones took more than 20000 which for the 
purposes of this project is considered a failure. This project was a fun one because it’s interesting 
to see greedy algorithms are work in front of you. The num out and the Manhattan enqueuers had more 
success because they put the best child on top but the dfs and bfs don’t, so it takes long. I am still 
surprised why my dfs work but not bfs, but I couldn’t understand it.

For example, five is a seemingly innocent example but it is the hardest. For Manhattan distance example 5 
is hard because even if you move one tile the distance increases so the optimal distance it can get is 
with the problem given. Even with num-out for example 5 there are only 2 tiles out of place if you move 
one tie there would be 3 tiles out of place and that makes it harder to solve the problem.

|#


