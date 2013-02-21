;;; Alden Quimby
;;; adq2101

;; node accessor functions:
(defun State (node) (first node)) 
(defun Operator (node) (second node)) 
(defun G-hat (node) (third node))
(defun Parent (node) (fourth node))

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
		(Print-fmt "Node: ~a" (State-to-string (State x))))
)

;; print helper
(defun Print-fmt fmt val
	(print 
		(format NIL fmt val))
)

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

;; compare states by looking at string representation
(defun State-equal (state1 state2)
	(equal 
		(State-to-string state1) 
		(State-to-string state2))
) ;defun

;; returns sublist starting at node that has state equal to arg state
(defun Find-state (state nodes)
	(if (null nodes)
		nil ;return nil if exhausted
     (if (State-equal state (State (first nodes)))
         nodes ;return where we found it
         (Find-state (rest nodes)))) ;keep looking
) ;defun

;; optimized state copier, only copies what's necessary
(defun Copy-state (state copy-first copy-second copy-third)
	(let ((part1 nil)
		  (part2 nil)
		  (part3 nil)
		  (part4 (fourth state)))
		; copy necessary components
		(if copy-first
			(setf part1 (copy-tree (first state)))
			(setf part1 (first state)))
		(if copy-second
			(setf part2 (copy-tree (second state)))
			(setf part2 (second state)))
		(if copy-third
			(setf part3 (copy-tree (third state)))
			(setf part3 (third state)))
		; return copied state
		(list part1 part2 part3 part4)
	) ;let
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
		  (blank-col (second (fourth state)))
		  (copied-state nil))
		;make sure we can move
		(if (= blank-row 0)
			(return-from North nil))
		;only copy parts we need to
		(if (= blank-row 2)
			(setf copied-state 
				(Copy-state state nil t t))
			(setf copied-state 
				(Copy-state state t t nil))
		) ;if
		;subtract one from blank-row
		(Swap
			copied-state
			blank-row
			blank-col
			(- blank-row 1)
			blank-col)
	) ;let
) ;defun

;; move the blank SOUTH
(defun South (state)
	(let ((blank-row (first (fourth state)))
		  (blank-col (second (fourth state)))
		  (copied-state nil))
		;make sure we can move
		(if (= blank-row 2)
			(return-from South nil))
		;only copy parts we need to
		(if (= blank-row 0)
			(setf copied-state 
				(Copy-state state t t nil))
			(setf copied-state 
				(Copy-state state nil t t))
		) ;if
		;add one from blank-row
		(Swap
			copied-state
			blank-row
			blank-col
			(+ blank-row 1)
			blank-col)
	) ;let
) ;defun

;; move the blank EAST
(defun East (state)
	(let ((blank-row (first (fourth state)))
		  (blank-col (second (fourth state)))
		  (copied-state nil))
		;make sure we can move
		(if (= blank-col 2)
			(return-from East nil))
		;only copy parts we need to
		(cond
			((= blank-row 0)
				(setf copied-state 
					(Copy-state state t nil nil)))
			((= blank-row 1)
				(setf copied-state 
					(Copy-state state nil t nil)))
			((= blank-row 2)
				(setf copied-state 
					(Copy-state state nil nil t)))
		) ;cond
		;add one from blank-row
		(Swap
			copied-state
			blank-row
			blank-col
			blank-row
			(+ blank-col 1))
	) ;let
) ;defun

;; move the blank WEST
(defun West (state)
	(let ((blank-row (first (fourth state)))
		  (blank-col (second (fourth state)))
		  (copied-state nil))
		;make sure we can move
		(if (= blank-col 0)
			(return-from West nil))
		;only copy parts we need to
		(cond
			((= blank-row 0)
				(setf copied-state 
					(Copy-state state t nil nil)))
			((= blank-row 1)
				(setf copied-state 
					(Copy-state state nil t nil)))
			((= blank-row 2)
				(setf copied-state 
					(Copy-state state nil nil t)))
		) ;cond
		;add one from blank-row
		(Swap
			copied-state
			blank-row
			blank-col
			blank-row
			(- blank-col 1))
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

;; UNIFORM COST SEARCH
(defun ucs (s0 sg sons)
	(let ((open (list (list s0 nil 0 nil))) ;1. put S0 on OPEN
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
			(setf daughters (funcall sons n))

			;4.2. 
			(dolist (m daughters)
				(let ((found-closed-m (Find-state (State m) closed))
					  (found-open-m (Find-state (State m) open)))
					(cond
						; if in closed list, do nothing
						((not (null found-closed-m)))
						; if in open list, compare g-hat
						((not (null found-open-m))
							; if new child's g-hat is less than previous
							; trash old child, destructively replace with new child
							(if (< (g-hat m) (g-hat (first found-open-m)))
								(setf (first found-open-m) m)))
						; if not in open or closed, add to open
						((push m open))
					) ;cond
				) ;let
			) ;dolist

			; sort open list form min to max g-hat
			(setf open
				(sort open 
					#'(lambda(node1 node2) (< (G-hat node1) (G-hat node2)))))
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
	(bfs SI SG 'Successors)
) ;end of loop
