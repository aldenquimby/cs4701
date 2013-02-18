;;; Alden Quimby
;;; adq2101
;;; 
;;; matcher.lisp
;;; A simple pattern matcher that uses recursion.
;;; 
;;; Basic idea: Match the current pattern symbol with the current
;;; data symbol. If they match, recurse with the rest of pattern and rest
;;; of data. Handle * by recursing twice because * could represent 0 or
;;; more symbols. Handle pattern variables by recursing and creating
;;; an association list. When necessary, merge association lists.
;;;


;; pattern matching algorithm
(defun match (pattern data)
	(cond
		; BASE CASE: both empty, return true
		((and (null pattern) (null data)) t)
		; BASE CASE: pattern empty, return NIL
		((null pattern) NIL)
		; BASE CASE: data empty, return true if pattern contains all *s
		((null data) 
			(if (every #'(lambda (p) (equal p '*)) pattern) t NIL))
		; RECURSION
		((let ((p (car pattern)) (d (car data)))
			(cond
				; if p and d are equal or p is ?, recurse
				((or (equal p d) (equal p '?)) (match (cdr pattern) (cdr data)))
				; else if p is *, recurse twice and merge lists
				((equal p '*)
					(let ((noData (match (cdr pattern) data))
						 (withData (match pattern (cdr data))))
						(cond
							; if either result is NIL, return the other
							((null noData) withData)
							((null withData) noData)
							; if either result is t, return the other
							((equal noData t) withData)
							((equal withData t) noData)
							; if we already have a list of association lists, just append
							((listp (caar noData)) (append noData (list withData)))
							((listp (caar withData)) (append (list noData) withData))
							; both are association lists, make a list of association lists
							((list noData withData)))))
				; else if p starts with "?", recurse and add to list
				((and (symbolp p) (equal (subseq (symbol-name p) 0 1) "?"))
					(let ((subResult (match (cdr pattern) (cdr data)))
						 (newAssoc (list (list p d))))
						(cond
							((null subResult) NIL)
							((equal subResult t) newAssoc)
							; subResult is an association list, need to check that subAssoc matches (p d)
							((let ((subAssoc (find-if #'(lambda (al) (equal (car al) p)) subResult)))
								(cond
									((null subAssoc) (append subResult newAssoc))
									((equal (cadr subAssoc) d) subResult)
									(NIL)))))))
				; else if p and d are lists, recurse into them
				((and (listp p) (listp d)) (match p d))
				; else no match
				(NIL))))))


;; individual test run, prints results
(defun test-one-match (pattern data output)
	(print (format NIL "pattern: ~a" pattern))
	(print (format NIL "data:    ~a" data))
	(let ((result (match pattern data)))
		(if (equal result output)
			(print "TEST PASSED")
			(progn
				(print "**********************************")
				(print "************ FAILED **************")
				(print "**********************************")
				(print (format NIL "expected: ~a" output))
				(print (format NIL "result:   ~a" result))))))


;; test driver, thoroughly test match algorithm
(defun test-match ()
	(test-one-match '() '() t)
	(test-one-match '(ai) '(ai) t)
	(test-one-match '(ai cs) '(ai cs) t)
	(test-one-match '(cs ai) '(ai cs) NIL)
	(test-one-match '(1 2 3 0) '(1 2 3 4 0) NIL)
	(test-one-match '(? mudd) '(seely mudd) t)
	(test-one-match '(?first ?middle mudd) '(seely w mudd) '((?middle w) (?first seely)))
	(test-one-match '(? ?x ? ?y ?) '(Warren Buffet Is A Good Man) NIL)
	(test-one-match '(School Of Engineering and Applied Science) '(School Of Engineering) NIL)
	(test-one-match '(* School Of Engineering and Applied Science) '(The Fu Foundation School Of Engineering and Applied Science) t)
	(test-one-match '(The * School Of Engineering and Applied Science) '(The Fu Foundation School Of Engineering and Applied Science) t)
	(test-one-match '(The * School Of Engineering and Applied Science) '(The School Of Engineering and Applied Science) t)
	(test-one-match '(* 3 ?x 4 *) '(3 5 4) '((?x 5)))
	(test-one-match '(?x (1 2) ?y (4 5)) '(c (1 2) d (4 5)) '((?y d)(?x c)))
	(test-one-match '(?y ?z (c v)) '(8 gh (c v) ) '((?z gh)(?y 8)))
	(test-one-match '(((get) me) out) '(get (me (out))) NIL)
	(test-one-match '(A * B) '(A A A A A B) t)
	(test-one-match '(?x * ?y) '(A A A A Applied B) '((?y b)(?x a)))
	(test-one-match '(? ? ?) '(a (a b) ((a b c))) t)
	(test-one-match '(a * ?x *) '(a b c d) '(((?x b)) ((?x c)) ((?x d))))
	(test-one-match '(?x 2 (?x)) '(1 2 (1)) '((?x 1)))
	(test-one-match '(?x 2 (?x)) '(3 2 (1)) NIL)
	(test-one-match '(1 (* ?x *)) '(1 (a (b c) d)) '(((?x a)) ((?x (b c))) ((?x d))))

	(test-one-match '(((?X (?Y (?Z ((?W))) (?R))))) '(((1 (2 (3 ((4))) (1))))) '((?X 1) (?Y 2) (?Z 3) (?W 4) (?R 1)))
	(test-one-match '(?A (* ?B *)) '(1 (2 3 4)) '(((?B 2) (?A 1)) ((?B 3) (?A 1)) ((?B 4) (?A 1))))
	(test-one-match '(?A (* ?A *)) '(3 (2 3 4)) '((?A 3)))
)


;; run all tests
(test-match)
(setf tmp (read))