;;; Alden Quimby
;;; adq2101

;; node accessor functions:
(defun State (node) (first node)) 
(defun Operator (node) (second node)) 
(defun Parent (node) (third node))
(defun Depth (node) (fourth node))

;; state comparison
(defun state-equal (state1 state2)
	NIL)

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

;; get successor nodes by applying all operators to node
(defun successor-function (node) 
	(let ((son-nodes nil) 
		  (son-states nil)
		  (state (first node))) 
		;apply problem dependent operator 1 
		(setf son-states (operator-ONE state)) 
		(setf son-nodes 
			(mapcar son-states 
				'(lambda(son-state) (list son-state 'ONE node)))) ;here we create a list of nodes!
		;repeat for each operator
		(setf son-states (operator-TWO state))
		(setf son-nodes 
			(append son-nodes 
				(mapcar son-states 
					'(lambda(son-state) (list son-state 'TWO node)))))
		(setf son-states (operator-THREE state)) 
		(setf son-nodes 
			(append son-nodes 
				(mapcar son-states 
					'(lambda(son-state) (list son-state 'THREE node)))))
		(setf son-states (operator-FOUR state)) 
		(setf son-nodes 
			(append son-nodes 
				(mapcar son-states 
					'(lambda(son-state) (list son-state 'FOUR node)))))
		;and return son-nodes
		son-nodes)
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

;; breadth first search
(defun dfs (s0 sg sons)
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
			;4.3. add DAUGHTERS to front of OPEN, b/c bfs uses a stack
			(setf open (append daughters open))
			;5. loop back to 2
		) ;closes loop 
	) ;closes let 
) ;closes defun


;; 8-PUZZLE

(setf s0 '((1 3 nil) (2 6 4) (5 8 7)))

;; operator-ONE = move blank "NORTH"
(defun operator-ONE (parent-state) ;move blank "NORTH" 
	(let ((state (copy parent-state))) 
		(cond 
			((member nil (first state)) 
				nil) 
			((member nil (second state)) 
				(cond 
					((null (caadr state)) 
						(replace (caadr state) (caar state))
						(replace (caar state) nil) 
						state) 
					((null (caaddr state))
						(replace (caaddr state) (cadar state)) 
						(replace (cadar state) nil) 
						state) 
					((null (caadddr state))
						(replace (caadddr state) (caddar state)) 
						(replace (caddar state) nil) 
						state)))
			((member nil (caddr state))
		
				;TODO fill this in

				)
		) ;closes cond
	) ;closes let
) ;closes defun 

;; operator-TWO = move blank "EAST"
(defun operator-TWO (state) 
	;define it intelligently! 
)

;; operator-THREE = move blank "SOUTH"
(defun operator-THREE (state) 
	;define it intelligently! 
)

;; operator-FOUR = move blank "WEST"
(defun operator-FOUR (state) 
	;define it intelligently! 
)


(defun dfs-depth-bounded (s0 sg sons depth)

	(if (< (Depth n) depth-bound) 
		; some stuff here
	)

	; more code here
)

(loop 
	(print "Please tell me the starting position:") 
	(setf state (read)) 
	(setf SI state) 
	(print "Please tell me the goal state you wish to find:") 
	(setf goal (read)) 
	(setf SG goal)
	(bfs SI SG 'successor-function) 
) ;end of loop
