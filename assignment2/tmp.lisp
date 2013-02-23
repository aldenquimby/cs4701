
;; state accessor functions:
(defun Dimensions (state) (length (first state)))

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
	(print "calculating heuristic")
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
						(print "adding extra")
						(incf sum 2))
					(when (and (< (first other) (first conflict))
					 		 (> (second other) (second conflict)))
						(print "adding extra")
						(incf sum 2))
				) ;dolist
			) ;let
		) ;loop

		; return sum
		sum

	) ;let
) ;defun


;; main program
(loop 
	;get start state
	(print "Tell me the start state:")
	(setf SI (read))
	(setf SG '((1 2 3) (4 5 6) (7 8 0) (2 2)))

	(print "manhattan")
	(print (Manhattan-heuristic SI SG))
	(print "mine")
	(print (My-heuristic SI SG))
) ;loop
