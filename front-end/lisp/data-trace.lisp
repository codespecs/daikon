;; (in-package medic)

;; Utilities for writing data traces

;; The `write-to-data-trace' macro creates a trace file.  Gries-style Lisp
;; programs can be automatically instrumented -- the calls to
;; `write-to-data-trace' are inserted by the `instrument' function found in
;; gries-helper.lisp.  Given a file of Gries-style Lisp functions,
;; `instrument' produces a new file of instrumented Lisp code which can be
;; compiled and run.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types
;;;

(defun null-or-int->int (x) (or x 0))	; how well does this work???

(defun array-dim-1 (a) (first (array-dimensions a)))
(defun array-dim-2 (a) (second (array-dimensions a)))
(defun array-dim-3 (a) (third (array-dimensions a)))

;; Like identity, but not quite
(defun inv-format-array-character-1 (s)
  (format nil "~s" s))
(defun inv-format-array-integer-1 (a)
  (let ((raw (format nil "~s" a)))
    (subseq raw 2 (1- (length raw)))))


;; So far, only numeric accessors
(defvar *inv-types-accessors*
  '(;; (int identity)
    (integer identity)
    (list length)
    ;; ((or null int) null-or-int->int)
    ((or integer null) null-or-int->int)
    ((or null integer) null-or-int->int)
    (alist length)
    (hash-table hash-table-count)
    ;; These used to include array_dim_1, array_dim_2, etc.
    (array identity)
    ;; ((array integer 1) (identity . "[]"))
    ;; ((array integer 2) (identity . "[][]"))
    ;; ((array integer 3) (identity . "[][][]"))
    ;; ((array character 1) (identity . "[]"))
    ;; ((array character 2) (identity . "[][]"))
    ;; ((array character 3) (identity . "[][][]"))
    ((array integer 1) inv-format-array-integer-1)
    ((array integer 2) identity)
    ((array integer 3) identity)
    ((array character 1) inv-format-array-character-1)
    ((array character 2) identity)
    ((array character 3) identity)
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

(defvar *trace-pretty-type-names*
  '((integer . "int")
    ((array integer 1) . "int")))

(defun type-name-format (type)
  (cond ((eq type 'integer)
	 "int")
	((eq type 'character)
	 "char")
	((and (listp type)
	      (eq (car type) 'array))
	 (let ((result (type-name-format (second type))))
	   (loop for i from 1 to (third type)
		 do (setq result (string-append result "[]")))
	   result))
	(t
	 (format nil "~a" type))))


(defun lackwit-type-format (var)
  (let ((array-type (assoc var *lackwit-arrays*)))
    ;; (format t "~a is array? ~a    ~a~%" var array-type *lackwit-arrays*)
    (if array-type
	(let ((result (format nil "~a" (lackwit-var-representative
					(nth 1 array-type)))))
	  (loop for index-var in (cddr array-type)
		do (setq result
			 (string-append result
					(format nil "[~a]"
						(lackwit-var-representative
						 index-var)))))
	  result)
      (lackwit-var-representative var))))


(defun write-data-trace-declaration (ostream program-point-name var+type-list)
  (format ostream "DECLARE~%~a~%" program-point-name)
  ;; (format ostream "*lackwit-env* = ~s~%" *lackwit-env*)
  ;; I could do all this in one tricky format line, too.
  (loop for var+type in var+type-list
	do (let* ((var (car var+type))
		  (type (cdr var+type)))
	     (format *decl-output-stream* "~a~%~a~%~a~%"
		     var
		     (type-name-format type)
		     (lackwit-type-format var))))
  (format ostream "~%")
  )

(defmacro write-to-data-trace (program-point-name var+type-list)
  (assert (every #'consp var+type-list))
  (assert (every #'(lambda (var+type) (accessors-for-type (cdr var+type))) var+type-list))
  (if (and (listp program-point-name)
	   (eq 'quote (car program-point-name)))
      (setq program-point-name (second program-point-name)))
  `(when *dtrace-output-stream*
     (let ((*print-pretty* nil))
       (format *dtrace-output-stream*
	       ,(format nil "~a~~%" program-point-name))
       ,@(loop for var+type in var+type-list
	       nconc (let* ((var (car var+type)) ; actually expressions
			    (var-modified (symbol-append var '-modified))
			    ;; These could be computed once and let-bound,
			    ;; for the case when there are multiple accessors.
			    (modified-time-expr-for-get `(get ',program-point-name ',var-modified 0))
			    (modified-time-expr-for-set `(get ',program-point-name ',var-modified))
			    (modified-bit-expr `(if (> ,var-modified ,modified-time-expr-for-get) 1 0))
			    (type (cdr var+type))
			    (var-sans-spaces (nsubstitute #\_ #\space (format nil "~s" var))))
		       (append
			(loop for function in (accessors-for-type type)
			      collect (cond
				       ((eq function 'identity)
					`(format *dtrace-output-stream*
						 ,(format nil "~a~c~~s~c~~s~~%" var-sans-spaces #\newline #\newline)
						 ,var
						 ,modified-bit-expr))
				       ((member function '(inv-format-array-integer-1
							   inv-format-array-character-1))
					`(format *dtrace-output-stream*
						 ;; use of "~~a" instead of "~~s"
						 ,(format nil "~a~c~~a~c~~s~~%" var-sans-spaces #\newline #\newline)
						 (,function ,var)
						 ,modified-bit-expr))
				       ((and (consp function)
					     (atom (cdr function)))
					`(format *dtrace-output-stream*
						 ,(format nil "~a~a~c~~s~c~~s~~%" var-sans-spaces (cdr function) #\newline #\newline)
						 ,(if (eq (car function) 'identity)
						      var
						    `(funcall ,(car function) ,var))
						 ,modified-bit-expr))
				       (t
					`(format *dtrace-output-stream*
						 ,(format nil "~a.~a~c~~s~c~~s~~%" var-sans-spaces function #\newline #\newline)
						 (,function ,var)
						 ,modified-bit-expr))))
			;; `((setq ,var-modified 0))
			;; This doesn't really need to be done unconditionally,
			;; but it's better than redoing the test, I guess!
			`((setf ,modified-time-expr-for-set ,var-modified))
			)))
       (format *dtrace-output-stream* "~%")
       )))
;; (macroexpand '(WRITE-TO-DATA-TRACE |P173-14.3:::BEGIN| ((X . INTEGER) (Y . INTEGER))))

;;; Old version that used type-lists, which merge adjacent variables of the
;;; same type.  The added complexity isn't worth the small potential
;;; increase in efficiency.
;; Used to be called check-for-invariants; but that is not what it does.
;; Maybe someday we'll use a version that actually does an online check.
;; (defmacro write-to-data-trace (program-point-name &rest type-lists)
;;   (assert (every #'listp type-lists))
;;   (assert (every #'(lambda (type-list) (accessors-for-type (car type-list))) type-lists))
;;   (if (and (listp program-point-name)
;; 	   (eq 'quote (car program-point-name)))
;;       (setq program-point-name (second program-point-name)))
;;   `(when *dtrace-output-stream*
;;      (let ((*print-pretty* nil))
;;        (format *dtrace-output-stream*
;; 	       ,(format nil "~a~~%" program-point-name))
;;        ,@(loop for type-list in type-lists
;; 	       nconc (let ((type (car type-list)))
;; 		       (loop for var in (cdr type-list) ; actually expressions
;; 			     nconc (let ((var-sans-spaces (nsubstitute #\_ #\space (format nil "~s" var))))
;; 				     (loop for function in (accessors-for-type type)
;; 					   collect (cond
;; 						    ((eq function 'identity)
;; 						     `(format *dtrace-output-stream*
;; 							      ,(format nil "~a~c~~s~~%" var-sans-spaces #\tab)
;; 							      ,var))
;; 						    ((and (consp function)
;; 							  (atom (cdr function)))
;; 						     `(format *dtrace-output-stream*
;; 							      ,(format nil "~a~a~c~~s~~%" var-sans-spaces (cdr function) #\tab)
;; 							      ,(if (eq (car function) 'identity)
;; 								   var
;; 								 `(funcall ,(car function) ,var))))
;; 						    (t
;; 						     `(format *dtrace-output-stream*
;; 							      ,(format nil "~a.~a~c~~s~~%" var-sans-spaces function #\tab)
;; 							      (,function ,var)))))))))
;;        ;; used when all the above was on one line
;;        ;; (format *dtrace-output-stream* "~%")
;;        )))

;; (macroexpand '(write-to-data-trace split-argnum-sprop (params) (integer argnum) (operator op)))
;; (macroexpand '(write-to-data-trace non-matching-var-sprops (params) (sprop occurrence) (list fluent-args fluent-occurrences) (alist var-argnum-alist)))
;; (macroexpand '(WRITE-TO-DATA-TRACE '|P180-15.1.1:::END| (b n) ((ARRAY INTEGER 1) B) (INTEGER N I S)))
;; ;; (macroexpand '(write-to-data-trace find-plan-1 (params) ((list integer 3) cnf-size) (list-elements nil integer integer integer integer integer integer)))


(defmacro with-data-trace (filename &rest body)
  `(progn
     (init-data-trace ,filename)
     (unwind-protect
	 ,@body
       (finish-data-trace))))
