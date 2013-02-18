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
			(print "Solution:") 
			nil)
		(t 
			(trace-solution (Parent node))
			(if (not (null (Operator node)))
				(print (Operator node)))))
) ;defun

;; subtract open-and-closed-nodes from new-nodes
(defun DIFF (new existing)
	(set-difference 
		new 
		existing 
		:test #'(lambda (a b) (state-equal (State a) (State b))))
) ;defun

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
	(cond
		((= (first (fourth state)) 0) 
			nil)
		((swap8 
			(copy-tree state) 
			(first (fourth state))   ;from row
			(second (fourth state))  ;from column
			(- (first (fourth state)) 1) ;to row
			(second (fourth state))))) ;to column
) ;defun

;; 8 puzzle, move the blank SOUTH
(defun South8 (state)
	(cond
		((= (first (fourth state)) 2) 
			nil)
		((swap8 
			(copy-tree state) 
			(first (fourth state))   ;from row
			(second (fourth state))  ;from column
			(+ (first (fourth state)) 1) ;to row
			(second (fourth state)))) ;to column
	) ;cond
) ;defun

;; 8 puzzle, move the blank EAST
(defun East8 (state)
	(cond
		((= (second (fourth state)) 2) 
			nil)
		((swap8 
			(copy-tree state) 
			(first (fourth state))   ;from row
			(second (fourth state))  ;from column
			(first (fourth state))   ;to row
			(+ (second (fourth state)) 1))) ;to column
	) ;cond
) ;defun

;; 8 puzzle, move the blank WEST
(defun West8 (state)
	(cond
		((= (second (fourth state)) 0) 
			nil)
		((swap8 
			(copy-tree state) 
			(first (fourth state))   ;from row
			(second (fourth state))  ;from column
			(first (fourth state))   ;to row
			(- (second (fourth state)) 1))) ;to column
	) ;cond
) ;defun

;; get successor nodes by applying all operators to node
(defun successor-function (node) 
	(let ((son-nodes nil) 
		  (son-state nil)
		  (state (State node)))

		;(print "expanding node")
		;(print node)

		(setf son-state (North8 state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (list son-state 'NORTH node)))))

		(setf son-state (South8 state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (list son-state 'SOUTH node)))))

		(setf son-state (East8 state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (list son-state 'EAST node)))))

		(setf son-state (West8 state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (list son-state 'WEST node)))))

		;(print-nodes son-nodes "son-nodes")

		;and return son-nodes
		son-nodes)
) ;defun 

(defun print-nodes (nodes title)
	(print title)
	(dolist (x nodes) 
		(print (format NIL "Node: ~a" (state8-to-string (State x)))))
)

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
				(return (trace-solution n)))
			;4.1. let DAUGHTERS be nodes of all operators applied to N
			(setf daughters (successor-function n))
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
	(setf SG '((1 2 3) (4 5 6) (7 8 0) (2 2)))
	(bfs SI SG 'successor-function)
) ;end of loop
