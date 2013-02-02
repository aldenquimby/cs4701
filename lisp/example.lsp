; example.lsp

(defun rpower-of-two (n)
  (cond ((= n 0) 1)
        (t (* 2 (rpower-of-two (- n 1))))))