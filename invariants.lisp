;; (in-package medic)

;; Utilities for finding invariants

;; To instrument the Medic package, do something like this:
;;   From $inv/medic/data, start Lisp (via  M-x run-lisp, or, better, outside Emacs via just  lisp )
;;   (load "../code/main") (in-package medic) (load-all) (load-domains)
;;   ;; Either one of these:
;;     (regress-invariants)
;;     ;; these are useful when running a few problems on many machines:
;;     (regress-invariants 0 5)
;;     (regress-invariants 5 10)
;;     (regress-invariants 10 15)
;;     (regress-invariants 15 20)
;;     (regress-invariants 20 25)
;;     (regress-invariants 25 30)
;;     (regress-invariants 30)
;;   ;; or follow these directions:
;;   ;; One of these:
;;     (invariants-file-per-problem)
;;     ;; or, wrap this around a computation to put invariants in one big file:
;;     (init-check-for-invariants) ... (finish-check-for-invariants)
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
;;   Copy invariants.raw to elsewhere

(defun regress-invariants (&optional (start 0) (end (length *regression-problems*)))
  (invariants-file-per-problem)
  (setq *run-program-timeout* 3600)
  (multi-flags-regress (subseq *regression-problems* start end)))

;; Also see section in README about only generating CNF.

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types
;;;

(defun op-parameters-length (x) (length (op-parameters x)))
(defun op-formals-length (x) (length (op-formals x)))
(defun op-formal-types-length (x) (length (op-formal-types x)))
(defun op-precondition-length (x) (length (op-precondition x)))
(defun op-effect-length (x) (length (op-effect x)))
(defun op-obj-lst-length (x) (length (op-obj-lst x)))
(defun op-bindings-length (x) (length (op-bindings x)))

(defun sprop-args-length (x) (length (sprop-args x)))

(defun null-or-int->int (x) (or x 0))	; how well does this work???

(defun array-dim-1 (a) (first (array-dimensions a)))
(defun array-dim-2 (a) (second (array-dimensions a)))
(defun array-dim-3 (a) (third (array-dimensions a)))

;; So far, only numeric accessors
(defvar *inv-types-accessors*
  '(;; (int identity)
    (integer identity)
    (list length)
    (operator op-parameters-length op-formals-length op-formal-types-length
	      op-precondition-length op-effect-length op-obj-lst-length
	      op-bindings-length)
    (sprop sprop-args-length)
    ;; ((or null int) null-or-int->int)
    ((or integer null) null-or-int->int)
    ((or null integer) null-or-int->int)
    (alist length)
    (hash-table hash-table-count)
    ;; I should be able to do much better than this.
    ((array integer 1) array-dim-1)
    ((array integer 2) array-dim-1 array-dim-2)
    ((array integer 3) array-dim-1 array-dim-2 array-dim-3)
    ((array character 1) array-dim-1)
    ((array character 2) array-dim-1 array-dim-2)
    ((array character 3) array-dim-1 array-dim-2 array-dim-3)
    ))

;; Returns either a symbol or a (funcallable-function . print-repr) pair.
;; Ought to be able to cope with recursive calls (eg a list of elements,
;; each of which requires some special accessor).
(defun accessors-for-type (type)
  (or (cdr (assoc type *inv-types-accessors* :test #'equal))
      (and (listp type)
	   (cond
	    ((and (= 3 (length type))
		  (eq 'list (first type))
		  (eq 'integer (second type))
		  (integerp (third type)))
	     ;; (list type n)
	     (loop for i from 0 to (- (third type) 1)
		   collect (cons `(function (lambda (l) (nth ,i l)))
				 (format nil "[~d]" i))))
	    ((eq 'list-elements (first type))
	     ;; (list-elements type type type)
	     (loop for i from 0 to (- (length type) 2)
		   nconc (if (eq 'integer (nth (1+ i) type))
			     (list (cons `(function (lambda (l) (nth ,i l)))
					 (format nil "[~d]" i)))
			   '())))
;; 	    ((and (= 3 (length type))
;; 		  (eq 'array (first type))
;; 		  (eq 'integer (second type))
;; 		  (integerp (third type))
;; 		  (= 1 (third type)))
;; 	     ;; (array integer 1)
;; 	     (loop for i from 0 to (- (third type 1))
;; 		   (collect
;; 		    ...)))
	    (t
	     nil)))))
;; (accessors-for-type 'list)
;; (accessors-for-type 'sprop)
;; (accessors-for-type 'integer)
;; (accessors-for-type '(list integer 3))
;; (accessors-for-type '(list-elements foo integer integer bar integer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check for invariants (actually just output to a file)
;;;


(defvar *inv-output-stream* nil)

(defun init-check-for-invariants (&optional filename)
  ;; output-stream-p returns false if the argument is a closed stream.
  (assert (or (null *inv-output-stream*) (not (output-stream-p *inv-output-stream*))))
  (setq *inv-output-stream* (open (or filename "invariants.raw")
				  :direction :output
				  :if-exists :rename)))

(defun finish-check-for-invariants ()
  (close *inv-output-stream*)
  (setq *inv-output-stream* nil))

(defmacro check-for-invariants (function-name &rest type-lists)
  (assert (every #'listp type-lists))
  (assert (every #'(lambda (type-list) (accessors-for-type (car type-list))) type-lists))
  (if (and (listp function-name)
	   (eq 'quote (car function-name)))
      (setq function-name (second function-name)))
  `(when *inv-output-stream*
     (format *inv-output-stream* ,(format nil "~a~~%" function-name))
     ,@(loop for type-list in type-lists
	     nconc (let ((type (car type-list)))
		     (loop for var in (cdr type-list) ; actually expressions
			   nconc (let ((var-sans-spaces (nsubstitute #\_ #\space (format nil "~s" var))))
				   (loop for function in (accessors-for-type type)
					 collect (cond
						  ((eq function 'identity)
						   `(format *inv-output-stream*
							    ,(format nil "~a~c~~s~~%" var-sans-spaces #\tab)
							    ,var))
						  ((and (consp function)
							(atom (cdr function)))
						   `(format *inv-output-stream*
							    ,(format nil "~a.~a~c~~s~~%" var-sans-spaces (cdr function) #\tab)
							    (funcall ,(car function) ,var)))
						  (t
						   `(format *inv-output-stream*
							    ,(format nil "~a.~a~c~~s~~%" var-sans-spaces function #\tab)
							    (,function ,var)))))))))
     ;; used when all the above was on one line
     ;; (format *inv-output-stream* "~%")
     ))

;; (macroexpand '(check-for-invariants split-argnum-sprop (integer argnum) (operator op)))
;; (macroexpand '(check-for-invariants non-matching-var-sprops (sprop occurrence) (list fluent-args fluent-occurrences) (alist var-argnum-alist)))
;; (macroexpand '(CHECK-FOR-INVARIANTS '|P180-15.1.1:::END| ((ARRAY INTEGER 1) B) (INTEGER N I S)))
;; ;; (macroexpand '(check-for-invariants find-plan-1 ((list integer 3) cnf-size) (list-elements nil integer integer integer integer integer integer)))


;; Perhaps omit MAX-STEPS.
(defun invariants-file-name (experimentnum problemname &optional max-steps)
  (declare (ignore experimentnum))
  (format nil "~a.inv" (file-name-header problemname max-steps)))

(defun invariants-solve-problem-start-function (prob max-steps start-steps experimentnum)
  (declare (ignore start-steps))
  (let ((probname (cond ((symbolp prob)
			 prob)
			((problem-p prob)
			 (problem-name prob))
			(t
			 (error "Expected symbol or problem structure" prob)))))
    (init-check-for-invariants
     (invariants-file-name experimentnum probname max-steps))))


(defun invariants-solve-problem-end-function (prob max-steps start-steps experimentnum)
  (declare (ignore prob max-steps start-steps experimentnum))
  (finish-check-for-invariants))


(defun invariants-file-per-problem ()
  (setq solve-problem-start-function 'invariants-solve-problem-start-function)
  (setq solve-problem-end-function 'invariants-solve-problem-end-function))


(defmacro with-invariants-log (filename &rest body)
  `(progn
     (init-check-for-invariants ,filename)
     (unwind-protect
	 ,@body
       (finish-check-for-invariants))))
