;; ********************************************************************
;; lib/swank/swank.lisp – SWANK protocol handlers for GoLisp.
;; Autor    : Gerhard Quell - gquell@skequell.de
;; CoAutor  : claude sonnet 4.6
;; Copyright: 2026 Gerhard Quell - SKEQuell
;; Erstellt : 20260618
;; ********************************************************************

;; Redirect REPL output to Emacs.
(set! print swank-print)
(set! println swank-println)

(defun swank-dispatch (msg)
  (case (car msg)
    ((:emacs-rex)
     (let ((form (cadr msg))
           (pkg (caddr msg))
           (thread (cadddr msg))
           (id (car (cdr (cdr (cdr (cdr msg)))))))
       (handle-emacs-rex form pkg thread id)))
    (else (list (list :return (list :abort "unhandled message") 0)))))

(defun handle-emacs-rex (form pkg thread id)
  (let ((op (car form)))
    (cond
      ((equal? op 'swank:connection-info)
       (swank:connection-info id))
      ((equal? op 'swank:swank-require)
       (swank:swank-require id))
      ((equal? op 'swank:init-presentations)
       (swank:ok-nil id))
      ((equal? op 'swank:autodoc)
       ;; SLIME: (unless (eq doc :not-available) ...). Kein Arglist
       ;; verfuegbar -> :not-available, Formatierung/insert uebersprungen.
       (list (list :return (list :ok (list :not-available nil)) id)))
      ;; swank-repl contrib nutzt eigenes Package-Prefix
      ((equal? op 'swank-repl:create-repl)
       (swank:create-repl id))
      ((equal? op 'swank-repl:listener-eval)
       (swank:listener-eval (cadr form) id))
      ;; Legacy-Prefix (Manuelle Tests)
      ((equal? op 'swank:create-repl)
       (swank:create-repl id))
      ((equal? op 'swank:listener-eval)
       (swank:listener-eval (cadr form) id))
      ;; Unbekannte Ops: graceful leere Liste statt :abort. SLIME-Contribs
      ;; degradieren sauber; :abort wuerfe Sync-Eval-Fehler in Emacs.
      (else
       (swank:ok-nil id)))))

;; Generic OK-Stub: liefert echte leere Liste () als :ok-Wert.
;; Viele SLIME-Ops (autodoc, init-*) erwarten Liste, kein String.
(defun swank:ok-nil (id)
  (list (list :return (list :ok (list)) id)))

(defun swank:connection-info (id)
  (list (list :return
              (list :ok
                    (list :pid 0
                          :style :spawn
                          :encoding (list :coding-systems (list "utf-8-unix"))
                          :implementation (list :type "GoLisp"
                                                :version "0.2"
                                                :program "golisp")
                          :machine (list :instance "unknown")
                          :package (list :name "USER" :prompt "USER")
                          :features (list)
                          :version "0.2"))
              id)))

(defun swank:create-repl (id)
  (list (list :return (list :ok (list "USER" "USER")) id)
        (list :new-package "USER" "USER")))

;; Stub: keine Contribs implementiert. SLIME akzeptiert leere Liste
;; (geladene Module), Connect laeuft durch.
(defun swank:swank-require (id)
  (list (list :return (list :ok (list)) id)))

(defun swank:listener-eval (string id)
  (catch
    (let ((forms (swank--read-all string)))
      (let ((events (swank--eval-forms forms (list))))
        (append events (list (list :return (list :ok (list)) id)))))
    (lambda (err)
      (list (list :return (list :abort (swank--value-string err)) id)))))

;; Wertet alle Formen, sammelt (:write-string "<wert>\n" :repl-result)
;; Events. eval ist Spezialform, daher Wrapper als echte FUNC.
(defun swank--eval1 (form) (eval form))

(defun swank--eval-forms (forms acc)
  (if (null? forms)
      acc
      (let ((result (swank--eval1 (car forms))))
        (swank--eval-forms
          (cdr forms)
          (append acc (list (list :write-string
                                  (string-append (swank--value-string result) "\n")
                                  :repl-result)))))))
