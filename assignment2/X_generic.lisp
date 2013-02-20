;;; Alden Quimby
;;; adq2101

;; node accessor functions:
(Defun State (node) (first node))
(Defun Operator (node) (second node))
(Defun G-hat (node) (third node))
(Defun H-hat (node) (fourth node))
(Defun Parent (node) (fifth node))

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

;; debugger
(defun Print-nodes (nodes title)
	(print title)
	(dolist (x nodes) 
		(print (format NIL "Node: ~a" (State-to-string (State x)))))
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

(defun Calculate-G-hat(state parent-node)
	(+ (G-hat parent-node) 1)
) ;defun

(defun Calculate-H-hat(state parent-node)
	(+ 0 0)
) ;defun

(defun Create-node (state operator parent)
	(list 
		state 
		operator 
		(Calculate-G-hat state parent) 
		(Calculate-H-hat state parent) 
		parent)
) ;defun

;; get successor nodes by applying all operators to node
(defun Sucessors (node) 
	(let ((son-nodes nil) 
		  (son-state nil)
		  (state (State node)))
		;create nodes by applying each operator
		(setf son-state (North state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state 'NORTH node)))))
		(setf son-state (South state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state 'SOUTH node)))))
		(setf son-state (East state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state 'EAST node)))))
		(setf son-state (West state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state 'WEST node)))))
		;and return son-nodes
		son-nodes)
) ;defun 

;; depth first = sort open list by negative G-hat
(defun dfs(s0 sg successors)
	(best-first-search s0 sg successors 
		#'(lambda (node) (- 0 (G-hat node)))
	)
)

;; breadth first = sort open list by G-hat 
(defun bfs(s0 sg successors)
	(best-first-search s0 sg successors 
		#'(lambda (node) (G-hat node))
	)
)

;; generic search
(defun best-first-search(s0 sg successors open-sort-key)
	(let ((open (list (list s0 nil 0 0 nil))) ;1. put S0 on OPEN
		  (closed nil)
		  (n nil)
		  (daughters nil))
		(loop 
			;2. if OPEN is empty, EXIT FAIL
			(if (null open) (return 'fail)) 
			;3.1. let N = pop first from OPEN
			(setf open (sort open #'< :key open-sort-key))
			(setf n (pop open))
			;3.2. push N onto CLOSED
			(push n closed) 
			;3.3. if state(N) == Sg, EXIT SUCCESS
			(if (State-equal (State n) sg) 
				(return (Trace-solution n)))
			;4.1. let DAUGHTERS be nodes of all operators applied to N
			(setf daughters (funcall successors n))

			(dolist (node daughters)
				(cond
					((not (null (find-if #'(lambda (i) (State-equal (State i) (State node))) open)))
						; in the open list
						; do nothing
					)
					((not (null (find-if #'(lambda (i) (State-equal (State i) (State node))) closed)))
						; in the closed list
						; do nothing
					)
					(t
						; not in either open or closed
						(setf open (append open (list node))))
				) ;cond
			) ;dolist
		) ;loop 
	) ;let 
) ;defun

;; main program
(loop 
	
	(print "Please tell me the starting position:") 
	(setf state (read)) 
	(setf SI state) 
	(if (equal 'test SI)
		(setf SI '((1 2 3) (4 0 6) (7 5 8) (1 1))))
	
	(print "Please tell me the goal position:") 
	(setf state (read)) 
	(setf SG state) 
	(if (equal 'test SG)
		(setf SG '((1 2 3) (4 5 6) (7 8 0) (2 2))))
	
	(print "Choose a search type:")
	(print "(1) bfs = breadth first")
	(print "(2) dfs = depth first")
	(print "(3) ucs = uniform cost")
	(setf type (read))
	(cond
		((equal 1 type)
			(bfs SI SG 'Sucessors))
		((equal 2 type)
			(dfs SI SG 'Sucessors))
		((equal 3 type)
			(bfs SI SG 'Sucessors))
		((print "unhandled case"))
	) ;cond

) ;loop
