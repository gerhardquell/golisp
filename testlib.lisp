; testlib.lisp – kleine Bibliothek
(defun quadrat (n) (* n n))
(defun kubik (n) (* n n n))
(defun inc (n) (+ n 1))
(defun dec (n) (- n 1))

; TCO-Test: tiefe Rekursion
(defun countdown (n) (if (= n 0) "done" (countdown (- n 1))))
(println (countdown 1000000))
