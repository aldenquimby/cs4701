;;; Alden Quimby
;;; adq2101

;; node accessor functions:
(defun State (node) (first node)) 
(defun Operator (node) (second node)) 
(defun G-hat (node) (third node))
(defun H-hat (node) (fourth node))
(defun Parent (node) (fifth node))

;; node helper functions:
(defun F-hat (node) (+ (G-hat node) (H-hat node)))

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

;; convert state to string
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
         (Find-state state (rest nodes)))) ;keep looking
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

;; cost between two states 
(defun Cost (from-state to-state)
	; just do breadth first search for now
	1
) ;defun

;; manhattan distance between two points
(defun Manhattan-distance (pt1 pt2)
	(+
		(abs (- (first pt1) (first pt2)))
		(abs (- (second pt1) (second pt2))))
) ;defun

;; heuristic described in class: sum of manhattan distances off
(defun Manhattan-distance-heuristic (state goal)
	(let ((cur nil)
		  (sum 0)
		  (correct-place nil)
		  (distance-off 0)
		  (goal-hash (make-hash-table)))

		; hash location of each item in goal state
		(loop for row-num in '(0 1 2) do
			(loop for col-num in '(0 1 2) do
				(setf cur (nth col-num (nth row-num goal)))
				(setf (gethash cur goal-hash) (list row-num col-num))
			) ;loop
		) ;loop

		; sum the manhattan distances off from goal state
		(loop for row-num in '(0 1 2) do
			(loop for col-num in '(0 1 2) do
				(setf cur (nth col-num (nth row-num state)))
				(setf correct-place (gethash cur goal-hash))
				(setf distance-off (Manhattan-distance correct-place (list row-num col-num)))
				(setf sum (+ sum distance-off))
			) ;loop
		) ;loop

		; return sum
		sum

	) ;let
) ;defun

;; estimated cost from state to goal
(defun My-heuristic (state goal)
	; TODO: do it!
	(Manhattan-distance-heuristic state goal)
) ;defun

;; creates new node, setting appropriate g-hat and h-hat
(defun Create-node (state operator parent goal)
	(list 
		state 
		operator
		(+ (G-hat parent) (Cost (State parent) state))
		(My-heuristic state goal)
		parent)
) ;defun

;; get successor nodes by applying all operators to node
(defun Successors (node goal) 
	(let ((son-nodes nil) 
		  (son-state nil)
		  (state (State node)))
		;create nodes by applying each operator
		(setf son-state (North state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state 'NORTH node goal)))))
		(setf son-state (South state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state 'SOUTH node goal)))))
		(setf son-state (East state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state 'EAST node goal)))))
		(setf son-state (West state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state 'WEST node goal)))))
		;and return son-nodes
		son-nodes)
) ;defun 

;; astar sorts open list by min F-hat
(defun astar-open-sort-func (node1 node2)
	(< 
		(F-hat node1) 
		(F-hat node2))
) ;defun

;; A* SEARCH
(defun astar-search (s0 sg sons)
	(generic-search s0 sg sons 
		'astar-open-sort-func)
) ;defun

;; generic search
(defun generic-search (s0 sg sons open-sort-func)
	(let ((open (list (list s0 nil 0 (My-heuristic s0 sg) nil))) ;1. put S0 on OPEN
		  (closed nil)
		  (n nil)
		  (daughters nil))
		(loop 
			;2. if OPEN is empty, EXIT FAIL
			(if (null open) (return 'fail)) 
			;3.0. sort open list
			(setf open (sort open 'astar-open-sort-func))
			;3.1. let N = pop first from OPEN
			(setf n (pop open)) 
			;3.2 push N onto CLOSED
			(push n closed) 
			;3.3. if state(N) == Sg, EXIT SUCCESS
			(if (State-equal (State n) sg) 
				(return (Trace-solution n)))
			;4.0. loop through all successors of N
			(setf daughters (funcall sons n sg))
			(dolist (m daughters)
				(let ((found-closed-m (Find-state (State m) closed))
					  (found-open-m (Find-state (State m) open)))
					(cond
						; 4.1. if in open list
						((not (null found-open-m))
							; reset g-hat and parent of m on open if new m is better
							(when (< (G-hat m) (G-hat (first found-open-m)))
								(setf (first found-open-m) m)))
						; 4.2. if in closed list
						((not (null found-closed-m))
							(when (< (G-hat m) (G-hat (first found-closed-m)))
								; put new m on open
								(push m open)
								; remove old m from closed
								(setf closed 
									(remove-if #'(lambda (closed-node) (equal closed-node (first found-closed-m))) closed))))
						; 4.3. else not on open or closed
						((push m open))
					) ;cond
				) ;let
			) ;dolist
		) ;loop 
	) ;let 
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
	; do search
	(astar-search SI SG 'Successors)
) ;loop
