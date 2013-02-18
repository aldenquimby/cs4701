;;; Alden Quimby
;;; adq2101

;; node accessor functions:
(defun State (node) (first node)) 
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

;; debugger
(defun Print-nodes (nodes title)
	(print title)
	(dolist (x nodes) 
		(print (format NIL "Node: ~a" (State-to-string (State x)))))
)

;;;;;;;;;;;;;;; SPECIFIC TO 8 PUZZLE ;;;;;;;;;;;;;;;

;; state accessor functions:
(defun Blank (state) (fourth state))

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
;; 8 puzzle state: ((row1) (row2) (row3) (r c))
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

;;;;;;;;;;;; GENERAL TO 8/15 PUZZLE ;;;;;;;;;;;

;; compare states by looking at string representation
(defun State-equal (state1 state2)
	(equal (State-to-string state1) (State-to-string state2))
) ;defun

;; subtract open-and-closed-nodes from new-nodes
(defun Diff (new existing)
	(set-difference 
		new 
		existing 
		:test #'(lambda (a b) (State-equal (State a) (State b))))
) ;defun

;; move the blank NORTH 
(defun North (state)
	(let ((blank-row (first (Blank state)))
		  (blank-col (second (Blank state))))
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
	(let ((blank-row (first (Blank state)))
		  (blank-col (second (Blank state))))
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
	(let ((blank-row (first (Blank state)))
		  (blank-col (second (Blank state))))
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
	(let ((blank-row (first (Blank state)))
		  (blank-col (second (Blank state))))
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
(defun Sucessors (node) 
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

;;;;;;;;;;;;; UNIFORM COST SEARCH ;;;;;;;;;;;;;;;

;; uniform cost search
(defun ucs (s0 sg successors)
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
			(if (State-equal (State n) sg) 
				(return (Trace-solution n)))
			;4.1. let DAUGHTERS be nodes of all operators applied to N
			(setf daughters (funcall successors n))

			;	foreach (var m in sucessors(n))
			;	{
			;		if m is not OPEN or CLOSED
			;			add m to OPEN
			;		if m is OPEN
			;			if g(m) > g(n) + c(n,m)
			;				kill this node
			;	}

			(dolist (node daughters)
				(cond
					((not (null (find-if #'(lambda (i) (State-equal (State i) (State node))) open)))
						; in the open list

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
	(setf SG '((1 2 3) (4 5 6) (7 8 0) (2 2)))
	(bfs SI SG 'Sucessors)
) ;end of loop
