;; (in-package medic)

;; Utilities for writing data traces

;; The `write-to-data-trace' macro creates a trace file.  Gries-style Lisp
;; programs can be automatically instrumented -- the calls to
;; `write-to-data-trace' are inserted by the `instrument' function found in
;; gries-helper.lisp.  Given a file of Gries-style Lisp functions,
;; `instrument' produces a new file of instrumented Lisp code which can be
;; compiled and run.

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
    ;; These used to include array_dim_1, array_dim_2, etc.
    ((array integer 1) (identity . "[]"))
    ((array integer 2) (identity . "[][]"))
    ((array integer 3) (identity . "[][][]"))
    ((array character 1) (identity . "[]"))
    ((array character 2) (identity . "[][]"))
    ((array character 3) (identity . "[][][]"))
    ))

;; Returns either a symbol or a (funcallable-function . print-repr) pair,
;; where PRINT-REPR often starts with a period, as in ".size".
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
;; (accessors-for-type '(array integer 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write data values to a file
;;;


(defvar *dtrace-output-stream* nil)

(defun init-data-trace (&optional filename)
  ;; output-stream-p returns false if the argument is a closed stream.
  (assert (or (null *dtrace-output-stream*) (not (output-stream-p *dtrace-output-stream*))))
  (setq *dtrace-output-stream* (open (or filename "dtrace")
				  :direction :output
				  :if-exists :rename)))

(defun finish-data-trace ()
  (close *dtrace-output-stream*)
  (setq *dtrace-output-stream* nil))

;; Used to be called check-for-invariants; but that is not what it does.
;; Maybe someday we'll use a version that actually does an online check.
(defmacro write-to-data-trace (function-name function-parameters &rest type-lists)
  (assert (every #'listp type-lists))
  (assert (every #'(lambda (type-list) (accessors-for-type (car type-list))) type-lists))
  (if (and (listp function-name)
	   (eq 'quote (car function-name)))
      (setq function-name (second function-name)))
  `(when *dtrace-output-stream*
     (let ((*print-pretty* nil))
       (format *dtrace-output-stream*
	       ,(format nil "~a~a~~%" function-name
			(mapcar (lambda (var)
				  (if (some #'(lambda (type-list)
					       (and (member var (cdr type-list))
						    (listp (car type-list))
						    (eq 'array (caar type-list))))
					   type-lists)
				      (format nil "~a[]" var)
				    var))
				function-parameters)))
       ,@(loop for type-list in type-lists
	       nconc (let ((type (car type-list)))
		       (loop for var in (cdr type-list) ; actually expressions
			     nconc (let ((var-sans-spaces (nsubstitute #\_ #\space (format nil "~s" var))))
				     (loop for function in (accessors-for-type type)
					   collect (cond
						    ((eq function 'identity)
						     `(format *dtrace-output-stream*
							      ,(format nil "~a~c~~s~~%" var-sans-spaces #\tab)
							      ,var))
						    ((and (consp function)
							  (atom (cdr function)))
						     `(format *dtrace-output-stream*
							      ,(format nil "~a~a~c~~s~~%" var-sans-spaces (cdr function) #\tab)
							      ,(if (eq (car function) 'identity)
								   var
								 `(funcall ,(car function) ,var))))
						    (t
						     `(format *dtrace-output-stream*
							      ,(format nil "~a.~a~c~~s~~%" var-sans-spaces function #\tab)
							      (,function ,var)))))))))
       ;; used when all the above was on one line
       ;; (format *dtrace-output-stream* "~%")
       )))

;; (macroexpand '(write-to-data-trace split-argnum-sprop (params) (integer argnum) (operator op)))
;; (macroexpand '(write-to-data-trace non-matching-var-sprops (params) (sprop occurrence) (list fluent-args fluent-occurrences) (alist var-argnum-alist)))
;; (macroexpand '(WRITE-TO-DATA-TRACE '|P180-15.1.1:::END| (b n) ((ARRAY INTEGER 1) B) (INTEGER N I S)))
;; ;; (macroexpand '(write-to-data-trace find-plan-1 (params) ((list integer 3) cnf-size) (list-elements nil integer integer integer integer integer integer)))


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


(defmacro with-data-trace (filename &rest body)
  `(progn
     (init-data-trace ,filename)
     (unwind-protect
	 ,@body
       (finish-data-trace))))
