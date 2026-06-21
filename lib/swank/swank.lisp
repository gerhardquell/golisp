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
       (swank:autodoc form id))
      ((equal? op 'swank:operator-arglist)
       (swank:operator-arglist (cadr form) id))
      ((equal? op 'swank:swank-macroexpand-1)
       (swank:macroexpand-1 (cadr form) id))
      ((equal? op 'swank:swank-macroexpand)
       (swank:macroexpand-full (cadr form) id))
      ((equal? op 'swank:swank-macroexpand-all)
       (swank:macroexpand-full (cadr form) id))
      ;; SLIMEs eigene expand-Familie (C-c C-m default). Wie macroexpand,
      ;; aber immer String-Return (sonst char-or-string-p nil in Emacs).
      ((equal? op 'swank:swank-expand-1)
       (swank:macroexpand-1 (cadr form) id))
      ((equal? op 'swank:swank-expand)
       (swank:macroexpand-full (cadr form) id))
      ;; swank-repl contrib nutzt eigenes Package-Prefix
      ((equal? op 'swank-repl:create-repl)
       (swank:create-repl id))
      ((equal? op 'swank-repl:listener-eval)
       (swank:listener-eval (cadr form) id))
      ((equal? op 'swank:simple-completions)
       (swank:simple-completions (cadr form) id))
      ((equal? op 'swank:completions)
       (swank:completions (cadr form) id))
      ((equal? op 'swank:load-file)
       (swank:load-file (cadr form) id))
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

;; swank:simple-completions (prefix pkg) -> (:ok (matching-strings...)).
;; SLIME nutzt completion-table-dynamic, erwartet Liste von Strings.
(defun swank:simple-completions (prefix id)
  (let ((matches (swank--filter-prefix prefix (swank--symbols) (list))))
    (list (list :return (list :ok matches) id))))

(defun swank--filter-prefix (prefix syms acc)
  (if (null? syms)
      acc
      (let ((s (car syms)))
        (swank--filter-prefix
          prefix
          (cdr syms)
          (if (swank--prefix? prefix s) (append acc (list s)) acc)))))

(defun swank--prefix? (prefix s)
  (if (> (string-length prefix) (string-length s))
      ()
      (equal? prefix (substring s 0 (string-length prefix)))))

;; swank:completions (prefix pkg) -> (:ok ((name) (name)...)).
;; swank-c-p-c Contrib: Client destrukturiert (symbol-name classification
;; symbol) pro Element; fehlende = nil. Also 1-Element-Liste pro Match.
(defun swank:completions (prefix id)
  (let ((matches (swank--filter-prefix prefix (swank--symbols) (list))))
    (list (list :return (list :ok (swank--wrap-each matches (list))) id))))

(defun swank--wrap-each (lst acc)
  (if (null? lst)
      acc
      (swank--wrap-each (cdr lst) (append acc (list (list (car lst)))))))

;; swank:operator-arglist (name pkg) -> (:ok "(name args)") | (:ok ()).
;; C-c C-d C-a / company-docsig.
(defun swank:operator-arglist (name id)
  (let ((al (swank--arglist name)))
    (list (list :return (list :ok al) id))))

;; swank:autodoc (raw-form :print-right-margin N) -> (:ok (string cache-p)).
;; Vereinfacht: Operator aus raw-form, Arglist zeigen (ohne Highlighting
;; des aktuellen Args). Built-in FUNC -> :not-available.
(defun swank:autodoc (form id)
  (let* ((quoted (cadr form))
         (rawform (cadr quoted))
         (op (car rawform)))
    (let ((al (swank--arglist op)))
      (if (null? al)
          (list (list :return (list :ok (list :not-available nil)) id))
          (list (list :return (list :ok (list al nil)) id))))))

;; swank:swank-macroexpand-1 (string) -> (:ok "<expanded>").
;; C-c C-m. Eine Expansion via GoLisp macroexpand-Spezialform.
(defun swank:macroexpand-1 (string id)
  (catch
    (let ((form (read string)))
      (let ((expanded (macroexpand form)))
        (list (list :return (list :ok (swank--value-string expanded)) id))))
    (lambda (err)
      (list (list :return (list :abort (swank--value-string err)) id)))))

;; swank:swank-macroexpand / -all (string) -> (:ok "<expanded>").
;; Wiederhole macroexpand bis stabil. (Echtes macroexpand-all rekursiv in
;; alle Subformen ist noch offen; v1 expandiert Top-Level wiederholt.)
(defun swank:macroexpand-full (string id)
  (catch
    (let ((form (read string)))
      (let ((expanded (swank--expand-top form)))
        (list (list :return (list :ok (swank--value-string expanded)) id))))
    (lambda (err)
      (list (list :return (list :abort (swank--value-string err)) id)))))

(defun swank--expand-top (form)
  (let ((expanded (macroexpand form)))
    (if (equal? expanded form)
        form
        (swank--expand-top expanded))))

;; swank:load-file (filename) -> (:ok "<result>"). C-c C-l in Emacs.
;; Nutzt GoLisp load-Spezialform.
(defun swank:load-file (filename id)
  (catch
    (let ((result (eval (list (quote load) filename))))
      (list (list :return (list :ok (swank--value-string result)) id)))
    (lambda (err)
      (list (list :return (list :abort (swank--value-string err)) id)))))

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
