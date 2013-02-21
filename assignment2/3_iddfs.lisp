;;; Alden Quimby
;;; adq2101

;;; breadth and depth first search implementation

;; node accessor functions:
(defun State (node) (first node)) ; 8 puzzle state: ((row1) (row2) (row3) (r c))
(defun Operator (node) (second node)) 
(defun Parent (node) (third node))

;; trace solution when we hit goal state
(defun Trace-solution (node) 
	(cond 
		((null node) 
			(print "Solution:") 
			nil)
		(t 
			(Trace-solution (Parent node))
			(if (not (null (Operator node)))
				(print (Operator node)))))
) ;defun

;; convert 8 puzzle state to string
(defun State-to-string (state)  
	(let ((cstate (append (copy-list (first state)) (copy-list (second state)) (copy-list (third state))))
          (returned-string "S"))
  		(loop
		   	(if (null cstate) (return returned-string))
		   	(setf returned-string
				(concatenate 'string returned-string (subseq "012345678" (first cstate) (1+ (first cstate)))))
		   	(setf cstate (rest cstate))
   		) ;loop
  	) ;let
) ;defun

;; compare states by looking at string representation
(defun State-equal (state1 state2)
	(equal 
		(State-to-string state1) 
		(State-to-string state2))
) ;defun

;; swap two states
(defun Swap (state from-row from-col to-row to-col)
	(let ((temp nil))
	 	;simple swap
 		(setf temp 
 			(nth from-col (nth from-row state))) 
		(setf (nth from-col (nth from-row state))
			(nth to-col (nth to-row state)))
		(setf (nth to-col (nth to-row state)) 
			temp)
		;update new location of blank
		(setf (fourth state)
			(list to-row to-col))
		;return state
		state)
) ;defun

;; move the blank NORTH 
(defun North (state)
	(let ((blank-row (first (fourth state)))
		  (blank-col (second (fourth state))))
		(cond
			;make sure we can move
			((= blank-row 0) nil)
			;subtract one from blank-row
			((Swap
				(copy-tree state)
				blank-row
				blank-col
				(- blank-row 1)
				blank-col))
		) ;cond
	) ;let
) ;defun

;; move the blank SOUTH
(defun South (state)
	(let ((blank-row (first (fourth state)))
		  (blank-col (second (fourth state))))
		(cond
			;make sure we can move
			((= blank-row 2) nil)
			;add one to blank-row
			((Swap
				(copy-tree state)
				blank-row
				blank-col
				(+ blank-row 1)
				blank-col))
		) ;cond
	) ;let
) ;defun

;; move the blank EAST
(defun East (state)
	(let ((blank-row (first (fourth state)))
		  (blank-col (second (fourth state))))
		(cond
			;make sure we can move
			((= blank-col 2) nil)
			;add one to blank-col
			((Swap
				(copy-tree state)
				blank-row
				blank-col
				blank-row
				(+ blank-col 1)))
		) ;cond
	) ;let
) ;defun

;; move the blank WEST
(defun West (state)
	(let ((blank-row (first (fourth state)))
		  (blank-col (second (fourth state))))
		(cond
			;make sure we can move
			((= blank-col 0) nil)
			;subtract one from blank-col
			((Swap
				(copy-tree state)
				blank-row
				blank-col
				blank-row
				(- blank-col 1)))
		) ;cond
	) ;let
) ;defun

;; get successor nodes by applying all operators to node
(defun Successors (node) 
	(let ((son-nodes nil) 
		  (son-state nil)
		  (state (State node)))
		;apply all operators to node
		(setf son-state (North state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (list son-state 'NORTH node)))))
		(setf son-state (South state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (list son-state 'SOUTH node)))))
		(setf son-state (East state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (list son-state 'EAST node)))))
		(setf son-state (West state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (list son-state 'WEST node)))))
		;and return son-nodes
		son-nodes)
) ;defun 

;; depth bounded dfs
(defun Depth-bounded-dfs (node sg sons depth)
	(let ((daughters nil))
		;did we reach the goal state?
		(if (State-equal (state node) sg) 
			(return-from Depth-bounded-dfs (Trace-solution node)))
		;did we reach the bottom of the search space without finding goal?
		(if (= depth 1)
			(return-from Depth-bounded-dfs nil))
		;get child nodes
   		(setf daughters (funcall sons node)) 
		(loop
			;if no daughters, failed
			(if (null daughters) 
				(return-from Depth-bounded-dfs nil))
			;recurse with shallower depth
 			(if (Depth-bounded-dfs
                   (pop daughters)
                   sg
                   sons
                   (- depth 1))
	 			;found a solution!
	         	(return-from Depth-bounded-dfs t))
		) ;loop
	) ;let
) ;defun

;; iterative deepening dfs
(defun Iterative-deepening-dfs (s0 sg sons depth increment)
	(print (format nil "depth: ~a" depth))
	;try depth bounded dfs at this depth
	(if (not (Depth-bounded-dfs
				(list s0 nil nil)
				sg
				sons
				depth))
		;try again, but go deeper by increment
		(Iterative-deepening-dfs
			s0
			sg
			sons
			(+ depth increment)
			increment))
) ;defun

;; main program
(loop 
	;get start state
	(print "Tell me the start state (0 for default):") 
	(setf state (read)) 
	(setf SI state) 
	(if (equal 0 SI)
		(setf SI '((1 2 3) (4 0 6) (7 5 8) (1 1))))
	;get goal state
	(print "Tell me the goal state (0 for default):") 
	(setf state (read)) 
	(setf SG state) 
	(if (equal 0 SG)
		(setf SG '((1 2 3) (4 5 6) (7 8 0) (2 2))))

	(Iterative-deepening-dfs SI SG 'Successors 14 2)
) ;loop
