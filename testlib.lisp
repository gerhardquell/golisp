; testlib.lisp – kleine Bibliothek
(defun quadrat (n) (* n n))
(defun kubik (n) (* n n n))
(defun inc (n) (+ n 1))
(defun dec (n) (- n 1))

; TCO-Test: tiefe Rekursion
(defun countdown (n) (if (= n 0) "done" (countdown (- n 1))))
(println (countdown 1000000))

; gensym-Demo: swap!-Makro ohne Variablenkonflikt
(defmacro swap! (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (begin (set! ,a ,b) (set! ,b ,tmp)))))

(define x 10)
(define y 20)
(swap! x y)
(println x)   ; soll 20 ausgeben
(println y)   ; soll 10 ausgeben
