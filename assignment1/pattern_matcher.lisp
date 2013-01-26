;;; pattern_matcher.lisp
;;; a simple pattern matcher

(defun match-pattern (pattern data)
	(let ((no-match t) (match-no-vars NIL) (match-vars NIL))
		(if (no-match)
			NIL
			(if (match-no-vars)
				t
				'(a b c)))))

