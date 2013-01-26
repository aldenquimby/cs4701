;;; pattern_matcher.lisp
;;; a simple pattern matcher

(defun match (pattern data)

	;; base case: pattern is empty
	(when (= 0 (length pattern))
		(if (= 0 (length data))
			t
			NIL))

	(let ((p (first pattern)) (d (first data)))
		
		(cond
			; if equal at this position, recurse
			((equal p d)
				(match (rest pattern) (rest data)))
			
			; else if p is symbol, check for "?", "*", "?x"
			((symbolp p)
				(cond
					; if ?, recurse					
					((equal p '?)
						(match (rest pattern) (rest data)))

					; else if *, recurse twice and merge results
					((equal p '*)
						(let ((noData (match (rest pattern) data))
							 (withData (match pattern (rest data))))

							(cond
								((equal noData NIL)
									withData)
								((equal withData NIL)
									noData)
								((and (equal noData t) (equal withData t))
									t)
								((equal noData t)
									withData)
								((equal withData t)
									noData)
								()
							)


							; do stuff here

							))

					; else if starts-with ?, recurse and add to list
					((equal (subseq (symbol-name p) 0 1) "?")
						(let ((subResult (match (rest pattern) (rest data)))
							 (newAssoc (list p d)))

							(cond
								((equal subResult NIL)
									NIL)
								((equal subResult t)
									(list newAssoc))
								((push newAssoc subResult)))))

					; else no match
					(NIL)))

			; else no match
			(NIL)))) 






(defun test-match ()
	(test-one-match '() '(), t)
	(test-one-match '(ai) '(ai), t)
	(test-one-match '(ai cs) '(ai cs), t)
	(test-one-match '(cs ai) '(ai cs), NIL)
	(test-one-match '(1 2 3 0) '(1 2 3 4 0), NIL)
	(test-one-match '(? mudd) '(seely mudd), t)
	(test-one-match '(?first ?middle mudd) '(seely w mudd), '((?middle w) (?first seely)))
	(test-one-match '(? ?x ? ?y ?) '(Warren Buffet Is A Good Man), NIL)
	(test-one-match '(School Of Engineering and Applied Science) '(School Of Engineering), NIL)
	(test-one-match '(* School Of Engineering and Applied Science) '(The Fu Foundation School Of Engineering and Applied Science), t)
	(test-one-match '(The * School Of Engineering and Applied Science) '(The Fu Foundation School Of Engineering and Applied Science), t)
	(test-one-match '(The * School Of Engineering and Applied Science) '(The School Of Engineering and Applied Science), t)
	(test-one-match '(* 3 ?x 4 *) '(3 5 4), '((?x 5)))
	(test-one-match '( ?x (1 2) ?y (4 5)) '(c (1 2) d (4 5)), '((?y d)(?x c)))
	(test-one-match '(?y ?z (c v)) '(8 gh (c v) ), '((?z gh)(?y 8)))
	(test-one-match '(((get) me) out) '(get (me (out))), NIL)
	(test-one-match '(A * B) '(A A A A A B), t)
	(test-one-match '(?x * ?y) '(A A A A A B), '((?y b)(?x a)))
	(test-one-match '(a * ?x *) '(a b c d), '(((?x b)) ((?x c)) ((?x d))))
	"DONE!")

(defun test-one-match (pattern, data, output)
	
	; log the inputs
	(print (format NIL "pattern: ~a" pattern))
	(print (format NIL "data:    ~a" data))

	; check the output
	(let ((result (match pattern data)))
		(if (equal result output)
			(print "TEST PASSED")
			(progn
				(print "******** TEST FAILED ********")
				(print (format NIL "expected: ~a" output))
				(print (format NIL "result:   ~a" result)))))

	; skip a line
	(print ""))


