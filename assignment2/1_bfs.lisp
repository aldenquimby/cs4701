;;; Alden Quimby
;;; adq2101

;; node accessor functions:
(defun State (node) (first node)) 
(defun Operator (node) (second node)) 
(defun Parent (node) (third node))

;; convert 8 puzzle state to string
;; 8 puzzle state: ( ( row1 ) ( row2 ) ( row3 ) (r c) )
(defun state8-to-string (state)  
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
(defun state-equal (state1 state2)
	(equal (state8-to-string state1) (state8-to-string state2)))

;TODO: hash states to check for visited
;; convert an 8-puzzle state to a symbol
;(defun state-to-symbol (state)
;   (intern (state-to-string state)))
;(setf state '((7 8 0) (1 2 3) (4 5 6)))
;(setf (get (state-to-symbol state) 'Visited) T)
;(if (get (state-to-symbol state) 'Visited)
;	(print "indeed I have..."))

;; trace solution when we hit goal state
(defun trace-solution (node) 
	(cond 
		((null node) 
			(print "Beginning of solution") 
			nil) 
		(t 
			(trace-solution (Parent node)) 
			(print (Operator node)))
	) ;closes cond
) ;closes defun 

;; subtract open-and-closed-nodes from new-nodes
(defun DIFF (new-nodes open-and-closed-nodes) 
	(let ((return-nodes nil))
		(do 
			;initialize loop variable 
			((nodes new-nodes (rest nodes)))
			;exit test 
			((null nodes) return-nodes) 
			;body of this loop is another loop 
			(do 
				;initialize loop 
				((scan-nodes open-and-closed-nodes (rest scan-nodes))) 
				;exit test
				((null scan-nodes (push node return-nodes))) 
				;body test if state of node appears on open or closed list 
				(cond 
					((state-equal (State (first nodes)) (State (first scan-nodes))) 
						;break from the inner loop
						(return nil)))  
			) ;closes inner do
		;if the inner loop doesn't end prematurely, the exit test will update "return-nodes" 
		) ;closes outer do 
		;return cleaned up daughters
		return-nodes) 
) ;closes defun 

;; 8 puzzle, swap two states
(defun swap8 (state from-row from-col to-row to-col)
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

;; 8 puzzle, move the blank NORTH 
(defun North8 (state)
	(let ((newstate nil))
		(cond
			((= (first (fourth state)) 0) 
				nil)
			(t
				(setf newstate
					(swap8 (copy-tree state) 
					(first (fourth state))   ;from row
					(second (fourth state))  ;from column
					(decf (first (fourth state))) ;to row
					(second (fourth state))))) ;to column
		) ;cond
	) ;let
) ;defun

;; 8 puzzle, move the blank SOUTH
(defun South8 (state) 
	(let ((newstate nil))
		(cond
			((= (first (fourth state)) 2) 
				nil)
			(t
				(setf newstate
					(swap8 (copy-tree state) 
					(first (fourth state))   ;from row
					(second (fourth state))  ;from column
					(incf (first (fourth state))) ;to row
					(second (fourth state))))) ;to column
		) ;cond
	) ;let
) ;defun

;; 8 puzzle, move the blank EAST
(defun East8 (state) 
	(let ((newstate nil))
		(cond
			((= (second (fourth state)) 2) 
				nil)
			(t
				(setf newstate
					(swap8 (copy-tree state) 
					(first (fourth state))   ;from row
					(second (fourth state))  ;from column
					(first (fourth state))   ;to row
					(incf (second (fourth state)))))) ;to column
		) ;cond
	) ;let
) ;defun

;; 8 puzzle, move the blank WEST
(defun West8 (state) 
	(let ((newstate nil))
		(cond
			((= (second (fourth state)) 0) 
				nil)
			(t
				(setf newstate
					(swap8 (copy-tree state) 
					(first (fourth state))   ;from row
					(second (fourth state))  ;from column
					(first (fourth state))   ;to row
					(decf (second (fourth state)))))) ;to column
		) ;cond
	) ;let
) ;defun

;; get successor nodes by applying all operators to node
(defun successor-function (node) 
	(let ((son-nodes nil) 
		  (son-states nil)
		  (state (first node))) 
		;apply problem dependent operator 1 
		(setf son-states (North8 state)) 
		(setf son-nodes 
			(mapcar son-states 
				'(lambda(son-state) (list son-state 'NORTH node))))
		;repeat for each operator
		(setf son-states (South8 state)) 
		(setf son-nodes 
			(append son-nodes 
				(mapcar son-states 
					'(lambda(son-state) (list son-state 'SOUTH node)))))
		(setf son-states (East8 state))
		(setf son-nodes 
			(append son-nodes 
				(mapcar son-states 
					'(lambda(son-state) (list son-state 'EAST node)))))
		(setf son-states (West8 state)) 
		(setf son-nodes 
			(append son-nodes 
				(mapcar son-states 
					'(lambda(son-state) (list son-state 'WEST node)))))
		;and return son-nodes
		son-nodes)
) ;closes defun 

;; breadth first search
(defun bfs (s0 sg sons)
	(let ((open (list (list s0 nil nil))) ;1. put S0 on OPEN
		  (closed nil)
		  (n nil)
		  (daughters nil))
		(loop 
			;2. if OPEN is empty, EXIT FAIL
			(if (null open) (return 'fail)) 
			;3.1. let N = pop first from OPEN
			(setf n (pop open)) 
			;3.2 push N onto CLOSED
			(push n closed) 
			;3.3. if state(N) == Sg, EXIT SUCCESS
			(if (state-equal (State n) sg) 
				(print "Great. I found a solution. Here it is:") 
				(return (trace-solution n)))
			;4.1. let DAUGHTERS be nodes of all operators applied to N
			(setf daughters (apply sons n)) 
			;4.2. remove previously explored states from DAUGHTERS
			(setf daughters (DIFF daughters (append open closed))) 
			;4.3. add DAUGHTERS to end of OPEN, b/c bfs uses a queue
			(setf open (append open daughters)) 
			;5. loop back to 2
		) ;closes loop 
	) ;closes let 
) ;closes defun

; program that tries out BFS
(loop 
	(print "Please tell me the starting position:") 
	(setf state (read)) 
	(setf SI state) 
	(print "Please tell me the goal state you wish to find:") 
	(setf goal (read)) 
	(setf SG goal)
	(bfs SI SG 'successor-function) 
) ;end of loop
