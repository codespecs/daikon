;; (in-package medic)

;;; Medic-specific code for invariant detection.

;; To instrument the Medic package, do something like this:
;;   From $inv/medic/data, start Lisp (via  M-x run-lisp, or, better, outside Emacs via just  lisp )
;;   (load "../code/main") (in-package medic) (load-all) (load-domains)
;;   ;; Either one of these:
;;     (regress-dtrace)
;;     ;; these are useful when running a few problems on many machines:
;;     (regress-dtrace 0 5)
;;     (regress-dtrace 5 10)
;;     (regress-dtrace 10 15)
;;     (regress-dtrace 15 20)
;;     (regress-dtrace 20 25)
;;     (regress-dtrace 25 30)
;;     (regress-dtrace 30)
;;   ;; or follow these directions:
;;   ;; One of these:
;;     (dtrace-file-per-problem)
;;     ;; or, wrap this around a computation to put data trace in one big file:
;;     (init-data-trace) ... (finish-data-trace)
;;   ;; Maybe one or more of these:
;;     (setq *run-program-timeout* 3600) ; one hour
;;     (setq *cnf-solver* 'walksat)
;;     (setq *dont-solve* t)
;;     (medic-clean-directory)
;;     (medic-clean-directory nil '("*.info" "*.cnf-size"))
;;   ;; One of these:
;;     (multi-flags-regress)
;;     (multi-flags-regress (subseq *regression-problems* 0 5))
;;     (multi-flags-regress (subseq *regression-problems* 5 10))
;;     (multi-flags-regress (subseq *regression-problems* 10 15))
;;     (multi-flags-regress (subseq *regression-problems* 15 20))
;;     (multi-flags-regress (subseq *regression-problems* 20 25))
;;     (multi-flags-regress (subseq *regression-problems* 25 30))
;;     (multi-flags-regress (subseq *regression-problems* 30))
;;   (ext:quit)
;;   Copy file dtrace to elsewhere

(defun regress-dtrace (&optional (start 0) (end (length *regression-problems*)))
  (dtrace-file-per-problem)
  (setq *run-program-timeout* 3600)
  (multi-flags-regress (subseq *regression-problems* start end)))

;; Also see section in the Medic README about only generating CNF.

;; data/ATT-LOGISTICS1-crsn-6-simple.cnf failed -- I'm not sure why

;; TINY-crse-4.cnf failed
;; (set-flags "crse")
;; Then these are OK:
;;    (solve-find-problem 'tiny)
;;    (solve-find-problem 'tiny 1 1)
;; But these are no good:
;;    (solve-find-problem 'tiny 1 2)
;;    (solve-find-problem 'tiny 1 4)
;;    (solve-find-problem 'tiny 4 4)

;;; Types

(defun op-parameters-length (x) (length (op-parameters x)))
(defun op-formals-length (x) (length (op-formals x)))
(defun op-formal-types-length (x) (length (op-formal-types x)))
(defun op-precondition-length (x) (length (op-precondition x)))
(defun op-effect-length (x) (length (op-effect x)))
(defun op-obj-lst-length (x) (length (op-obj-lst x)))
(defun op-bindings-length (x) (length (op-bindings x)))

(defun sprop-args-length (x) (length (sprop-args x)))

(defvar *medic-inv-types-accessors*
  '((operator op-parameters-length op-formals-length op-formal-types-length
	      op-precondition-length op-effect-length op-obj-lst-length
	      op-bindings-length)
    (sprop sprop-args-length))
  "Append this to *inv-types-accessors*.")


;;; Solving planning problems

;; Perhaps omit MAX-STEPS.
(defun dtrace-file-name (experimentnum problemname &optional max-steps)
  (declare (ignore experimentnum))
  (format nil "~a.dtrace" (file-name-header problemname max-steps)))

(defun dtrace-solve-problem-start-function (prob max-steps start-steps experimentnum)
  (declare (ignore start-steps))
  (let ((probname (cond ((symbolp prob)
			 prob)
			((problem-p prob)
			 (problem-name prob))
			(t
			 (error "Expected symbol or problem structure" prob)))))
    (init-data-trace
     (dtrace-file-name experimentnum probname max-steps))))

(defun dtrace-solve-problem-end-function (prob max-steps start-steps experimentnum)
  (declare (ignore prob max-steps start-steps experimentnum))
  (finish-data-trace))


(defun dtrace-file-per-problem ()
  (setq solve-problem-start-function 'dtrace-solve-problem-start-function)
  (setq solve-problem-end-function 'dtrace-solve-problem-end-function))
