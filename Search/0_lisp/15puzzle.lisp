
;; convert 15 puzzle state to string
;; 15 puzzle state: ( ( row1 ) ( row2 ) ( row3 ) ( row4 ) (r c) )
(defun state15-to-string (state)  
	(let ((cstate (append (copy-list (first state)) (copy-list (second state)) (copy-list (third state)) (copy-list (fourth state))))
          (returned-string "S"))
  		(loop
		   (if (null cstate) (return returned-string))
		   (setf returned-string
		         (concatenate 'string returned-string (subseq "00010203040506070809101112131415" (* 2 (first cstate)) (+ 2 (* 2 (first cstate))))))
		   (setf cstate (rest cstate))
   		) ;loop
  	) ;let
) ;defun


; swap two states
(defun swap15 (state from-row from-col to-row to-col)
	(let ((temp nil))
	 	;simple swap
 		(setf temp 
 			(nth from-col (nth from-row state))) 
		(setf (nth from-col (nth from-row state))
			(nth to-col (nth to-row state)))
		(setf (nth to-col (nth to-row state)) 
			temp)
		;update new location of blank
		(setf (fifth state) 
			(list to-row to-col))
		;return state
		state)
) ;defun


; 15-puzzle, move the blank NORTH 
(defun North15 (state)
	(let ((newstate nil))
		(cond
			;blank can't move north
			((<= (first (fifth state)) 0) 
				nil)
			(t
				(setf newstate
					(swap15 (copy-tree state) 
					(first (fifth state))   ;from row
					(second (fifth state))  ;from column
					(decf (first (fifth state))) ;to row
					(second (fifth state))))) ;to column
		) ;cond
	) ;let
) ;defun
