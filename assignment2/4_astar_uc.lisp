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

;; state accessor functions:
(defun Dimensions (state) (length (first state)))

;; recreate list of operators used to solve
(defun Trace-solution-helper (node result)
	(cond
		((null node)
			result)
		(t
			(if (not (null (Operator node)))
				(push (Operator node) result))
			(Trace-solution-helper (Parent node) result))
	) ;cond
) ;defun

;; trace solution when we hit goal state
(defun Trace-solution (node open closed algorithm num-generated num-prev-generated)
	(let ((solution nil)
		  (trace (Trace-solution-helper node nil))) ;;(list (list nil 0) 0 0 0 0)))
		(push (length closed) solution)
		(push (length open) solution)
		(push num-prev-generated solution)
		(push num-generated solution)
		(push (list trace (length trace)) solution)
		(princ (format nil "~C~a:" #\linefeed algorithm))
		(print solution)
		nil
	) ;let
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

;; creates new node, setting appropriate g-hat and h-hat
(defun Create-node (state operator parent goal heuristic-func)
	(let ((g-hat 0) (h-hat 0))
		(if (not (null parent))
			(setf g-hat (+ (G-hat parent) (Cost (State parent) state))))
		(if (not (null heuristic-func))
			(setf h-hat (funcall heuristic-func state goal)))
		; return node
		(list state operator g-hat h-hat parent)
	) ;let
) ;defun

;; get successor nodes by applying all operators to node
(defun Successors (node goal heuristic-func) 
	(let ((son-nodes nil) 
		  (son-state nil)
		  (state (State node)))
		;create nodes by applying each operator
		(setf son-state (North state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state "N" node goal heuristic-func)))))
		(setf son-state (South state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state "S" node goal heuristic-func)))))
		(setf son-state (East state))
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state "E" node goal heuristic-func)))))
		(setf son-state (West state)) 
		(if (not (null son-state))
			(setf son-nodes 
				(append son-nodes 
					(list (Create-node son-state "W" node goal heuristic-func)))))
		;and return son-nodes
		son-nodes)
) ;defun 

;;;;;;;;;;;;;;;;;;; HEURISTICS ;;;;;;;;;;;;;;;;;;;;;;

;; manhattan distance between two points
(defun Manhattan-distance (pt1 pt2)
	(+
		(abs (- (first pt1) (first pt2)))
		(abs (- (second pt1) (second pt2))))
) ;defun

;; heuristic described in class: sum of manhattan distances off
(defun Manhattan-heuristic (state goal)
	(let ((cur nil)
		  (sum 0)
		  (correct-place nil)
		  (distance-off 0)
		  (max-row-col (- (Dimensions state) 1))
		  (goal-hash (make-hash-table)))

		; hash location of each item in goal state
		(loop for row-num from 0 to max-row-col do
			(loop for col-num from 0 to max-row-col do
				(setf cur (nth col-num (nth row-num goal)))
				(setf (gethash cur goal-hash) (list row-num col-num))
			) ;loop
		) ;loop

		; sum the manhattan distances off from goal state
		(loop for row-num from 0 to max-row-col do
			(loop for col-num from 0 to max-row-col do
				(setf cur (nth col-num (nth row-num state)))
				; skip the blank
				(when (not (= 0 cur))
					(setf correct-place (gethash cur goal-hash))
					(setf distance-off (Manhattan-distance correct-place (list row-num col-num)))
					(setf sum (+ sum distance-off)))
			) ;loop
		) ;loop

		; return sum
		sum

	) ;let
) ;defun

;; heuristic that adds linear conflicts to manhattan distance
(defun My-heuristic (state goal)
	(let ((cur nil)
		  (sum 0)
		  (correct-place nil)
		  (distance-off 0)
		  (conflicts-hash (make-hash-table :test 'equal))
		  (max-row-col (- (Dimensions state) 1))
		  (goal-hash (make-hash-table)))

		; hash location of each item in goal state
		(loop for row-num from 0 to max-row-col do
			(loop for col-num from 0 to max-row-col do
				(setf cur (nth col-num (nth row-num goal)))
				(setf (gethash cur goal-hash) (list row-num col-num))
			) ;loop
		) ;loop

		(loop for row-num from 0 to max-row-col do
			(loop for col-num from 0 to max-row-col do
				(setf cur (nth col-num (nth row-num state)))
				; skip the blank
				(when (not (= 0 cur))
					; add manahattan distance
					(setf correct-place (gethash cur goal-hash))
					(setf distance-off (Manhattan-distance correct-place (list row-num col-num)))
					(incf sum distance-off)
					; check for possible conflicts
					(cond
						; is it in the correct row?
						((= row-num (first correct-place))
							; and in the wrong column?
							(if (not (= col-num (second correct-place)))
								(push (list col-num (second correct-place)) (gethash (format nil "R~a" row-num) conflicts-hash))))
						; is it in the correct column?
						((= col-num (second correct-place))
							(push (list row-num (first correct-place)) (gethash (format nil "C~a" col-num) conflicts-hash)))
					) ;cond
				)
			) ;loop
		) ;loop

		; add linear conflicts
		(loop for conflicts being the hash-values of conflicts-hash do
			(let ((conflict (pop conflicts)))
				(dolist (other conflicts)
					(when (and (> (first other) (first conflict))
					 		 (< (second other) (second conflict)))
						(incf sum 2))
					(when (and (< (first other) (first conflict))
					 		 (> (second other) (second conflict)))
						(incf sum 2))
				) ;dolist
			) ;let
		) ;loop

		; return sum
		sum

	) ;let
) ;defun

;;;;;;;;;;;;;;;;; SEARCHES ;;;;;;;;;;;;;;;;;;;;;;;;

;; uniform cost sorts open by min G-hat
(defun uc-sort (node1 node2) (< (G-hat node1) (G-hat node2)))

;; astar sorts open list by min F-hat
(defun astar-sort (node1 node2) (< (F-hat node1) (F-hat node2)))

;; greedy sorts by min H-hat
(defun greedy-sort (node1 node2) (< (H-hat node1) (H-hat node2)))

;; UNIFORM COST SEARCH
(defun uc-search (s0 sg sons)
	(generic-search s0 sg sons
		'uc-sort nil 'UCS)
) ;defun

;; A* SEARCH
(defun astar-search (s0 sg sons heuristic-func)
	(generic-search s0 sg sons 
		'astar-sort heuristic-func 'A*)
) ;defun

;; GREEDY SEARCH
(defun greedy-search (s0 sg sons heuristic-func)
	(generic-search s0 sg sons
		'greedy-sort heuristic-func 'GREEDY)
) ;defun

;;;;;;;;;;;;;;; GENERIC SEARCH ;;;;;;;;;;;;;;;;;

;; generic search
(defun generic-search (s0 sg sons open-sort-func heuristic-func algorithm)
	(let ((open (list (Create-node s0 nil nil sg heuristic-func))) ;1. put S0 on OPEN
		  (closed nil)
		  (n nil)
		  (daughters nil)
		  (num-generated 0)
		  (num-prev-generated 0))
		(loop 
			;2. if OPEN is empty, EXIT FAIL
			(if (null open) (return 'fail)) 
			;3.0. sort open list
			(setf open (sort open open-sort-func))
			;3.1. let N = pop first from OPEN
			(setf n (pop open)) 
			;3.2 push N onto CLOSED
			(push n closed) 
			;3.3. if state(N) == Sg, EXIT SUCCESS
			(if (State-equal (State n) sg) 
				(return (Trace-solution n open closed algorithm num-generated num-prev-generated)))
			;4.0. loop through all successors of N
			(setf daughters (funcall sons n sg heuristic-func))
			
			;INSTRUMENT
			(incf num-generated (length daughters))
			;END INSTRUMENT

			(dolist (m daughters)
				(let ((found-closed-m (Find-state (State m) closed))
					  (found-open-m (Find-state (State m) open)))
					(cond

						; 4.1. if in open list
						((not (null found-open-m))

							;INSTRUMENT
							(incf num-prev-generated)
							;END INSTRUMENT

							; reset g-hat and parent of m on open if new m is better
							(when (< (G-hat m) (G-hat (first found-open-m)))
								(setf (first found-open-m) m))
						) ;case

						; 4.2. if in closed list
						((not (null found-closed-m))

							;INSTRUMENT
							(incf num-prev-generated)
							;END INSTRUMENT

							(when (< (G-hat m) (G-hat (first found-closed-m)))
								; put new m on open
								(push m open)
								; remove old m from closed
								(setf closed 
									(remove-if #'(lambda (closed-node) (equal closed-node (first found-closed-m))) closed)))
						) ;case
						
						; 4.3. else not on open or closed
						((push m open))

					) ;cond
				) ;let
			) ;dolist
		) ;loop 
	) ;let 
) ;defun

;;;;;;;;;;;;;;; MAIN SEARCH LOOP ;;;;;;;;;;;;;;

;; main program
(loop 
	;get start state
	(princ (format nil "~C" #\linefeed))
	(print "Tell me the start state (0 easy, 1 med, 2 hard):")
	(setf state (read)) 
	(setf SI state) 
	(if (equal 0 SI)
		(setf SI '((1 2 3) (4 0 6) (7 5 8) (1 1))))
	(if (equal 1 SI)
		(setf SI '((0 5 2) (1 4 6) (7 3 8) (0 0))))
	(if (equal 2 SI)
		(setf SI '((4 1 6) (2 0 3) (5 7 8) (1 1))))
	;get goal state
	(print "Tell me the goal state (0 for default):") 
	(setf state (read)) 
	(setf SG state) 
	(if (equal 0 SG)
		(setf SG '((1 2 3) (4 5 6) (7 8 0) (2 2))))
	;get heuristic
	(print "Choose a heuristic")
	(print "(0) Manhattan Distance")
	(print "(1) My Linear Conflict Heuristic")
	(setf type (read))
	(setf h-func nil)
	(cond
		((equal 0 type)
			(setf h-func 'Manhattan-heuristic))
		((equal 1 type)
			(setf h-func 'My-heuristic))
		((print "unhandled case"))
	) ;cond
	;get search type	
	(print "Choose a search type:")
	(print "(0) All Searches")
	(print "(1) Uniform Cost")
	(print "(2) A*")
	(print "(3) Greedy")
	(setf type (read))

	(princ (format nil "solution format:~C" #\linefeed))
	(princ (format nil "((steps num-steps) num-generated num-prev-generated num-open num-closed)~C" #\linefeed))

	(cond
		((equal 0 type)
			(uc-search SI SG 'Successors)
			(astar-search SI SG 'Successors h-func)
			(greedy-search SI SG 'Successors h-func))
		((equal 1 type)
			(uc-search SI SG 'Successors))
		((equal 2 type)
			(astar-search SI SG 'Successors h-func))
		((equal 3 type)
			(greedy-search SI SG 'Successors h-func))
		((print "unhandled case"))
	) ;cond
) ;loop
