;;; instrument.lisp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

(defun progn-body (stmt)
  "Return the progn body of STMT (which need not be a progn)."
  ;; This avoid extraneous "progn" (aesthetic fix only)
  (if (eq 'progn (car stmt))
      (cdr stmt)
    (list stmt)))

(defmacro string-append (&rest args)
  `(concatenate 'string ,@args))
(defun string-append-function (&rest args)
  (apply #'concatenate 'string args))

(defun symbol-append (&rest args)
  (intern (apply #'concatenate 'string (mapcar #'string args))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instrumentation
;;;

(defvar *decl-output-stream*)

(defvar doing-orig-parameters nil
  "Non-nil if original parameter values should be remembered and output at
function exit time; nil if the instrumentation engine will regenerate that
information from other data in the data trace file.")

(defun instrument (form)
  "Insert calls to `write-to-data-trace' in FORM, which is a Lisp expression.
Write declaration information to *decl-output-stream*."
  (lackwit-infer form)
  (instrument-internal form nil nil nil nil))

(defun instrument-internal (form fn-name params bound-vars lastp)
  "Instrument FORM, which appears in function FN-NAME.
BOUND-VARS is a list of the bound variables in this scope.
LASTP is non-nil if this is the last form (the return value) in the function body."
  ;; This function should not call instrument, only instrument-internal.

  ;; Should make good choices about where to insert the instrumentation and
  ;; which variables to instrument.

  (let ((head (if (consp form) (car form) 'form-has-no-head)))
    (cond ((eq head 'defun)
	   (assert (null bound-vars))
	   (assert (not fn-name))
	   (assert (not lastp))
	   (setq fn-name (second form))
	   (let* ((params (third form))
		  (body (cdddr form))

		  ;; A list of the parameters that are modified in the
		  ;; function body.  For these, we'll remember the original
		  ;; value.
		  (params-modified (and doing-orig-parameters
					(reverse
					 (intersection params
						       (modified-variables form)))))
		  (params-orig (mapcar #'(lambda (p)
					   (pcl::symbol-append 'orig- p))
				       params-modified))

		  ;; I could just use the cdrs of the declare statement's
		  ;; (type ...) forms in an argument to
		  ;; write-to-data-trace, but I want to make sure every
		  ;; variable appears, so destruct them.

		  (decl (and (eq 'declare (first (first body)))
			     (first body)))
		  (vars+types (and decl (declare->vars+types decl)))
		  (vars+types-orig (mapcar
				    #'(lambda (pm)
					(let ((v+t (assoc pm vars+types)))
					  (cons (pcl::symbol-append 'orig-
								    (car v+t))
						(cdr v+t))))
				    params-modified))

		  ;; Bind the original values, so I can compare against
		  ;; them.  (Maybe do that for all forms that
		  ;; potentially modify a variable?)

		  (body-sans-last-stmt
		   (mapcar #'(lambda (stmt)
			       (instrument-internal stmt
				 fn-name params
				 (append vars+types vars+types-orig) nil))
			   (if decl
			       (cdr (butlast body))
			     (butlast body))))
		  (last-stmt (instrument-internal (car (last body))
			       fn-name params
			       (append vars+types vars+types-orig) t))
		  (result-body
		   (append body-sans-last-stmt (progn-body last-stmt)))
		  (wrapped-result-body
		   (if params-modified
		       `((let ,(mapcar #'(lambda (orig mod)
					   `(,orig (copy ,mod)))
				       params-orig params-modified)
			   ,@result-body))
		     result-body)))
	     ;; Could also assert that no more types are declared (because I
	     ;; will check for invariants over everything declared...).
	     (assert (every #'(lambda (v) (assoc v vars+types)) params))

	     `(defun ,fn-name ,params
		,@(if decl (list decl) '())
		,(make-write-to-data-trace
		  (pcl::symbol-append fn-name ":::BEGIN") vars+types)
		,@wrapped-result-body)))

	  ((eq head 'let)
	   (let* ((bindings (second form))
		  (body (cddr form))
		  (decl (and (eq 'declare (first (first body)))
			     (first body)))
		  (vars+types (and decl (declare->vars+types decl)))
		  (new-vars (mapcar #'(lambda (binding)
					(if (symbolp binding)
					    binding
					  (car binding)))
				    bindings))
		  (result-sans-last-stmt
		   `(let ,bindings
		      ,@(if decl (list decl) '())
		      ,@(mapcar #'(lambda (stmt)
				    (instrument-internal stmt
				      fn-name params
				      (append bound-vars vars+types) nil))
				(if decl
				    (cdr (butlast body))
				  (butlast body)))))
		  (last-stmt (instrument-internal (car (last body))
			       fn-name params
			       (append bound-vars vars+types) lastp)))
	     ;; Could also assert that no more types are declared (because I
	     ;; will check for invariants over everything declared...).
	     (assert (every #'(lambda (v)
				(or (do-od-matched-symbol-p v)
				    (assoc v vars+types)))
			    new-vars))

	     (append result-sans-last-stmt (progn-body last-stmt))))

	  ((eq head 'progn)
	   (let* ((body (cdr form))
		  (result-sans-last-stmt
		   `(progn
		      ,@(mapcar #'(lambda (stmt)
				    (instrument-internal stmt
				      fn-name params bound-vars nil))
				(butlast body))))
		  (last-stmt (instrument-internal (car (last body))
			       fn-name params bound-vars t)))
	     (append result-sans-last-stmt (progn-body last-stmt))))

	  ;; PROBLEM with do-od: I need to insert something in the
	  ;; macroexpanded version.

	  ((eq head 'loop)
	   (if (and (eq (second form) 'while)
		    (do-od-matched-symbol-p (third form))
		    (eq (fourth form) 'do)
		    (= 5 (length form)))
	       `(loop while ,(third form) do
		      (progn
			,(make-write-to-data-trace
			  (gensym (string-append (symbol-name fn-name) ":::LOOP-"))
			  bound-vars)
			,(instrument-internal (fifth form)
			   fn-name params bound-vars lastp)))
	     (let ((do-position (position 'do form)))
	       (if (eq do-position (- (length form) 2))
		   `(,@(butlast form)
		       ,(instrument-internal (car (last form))
			  fn-name params bound-vars lastp))
		 (if lastp
		     ;; Is this right?  Should it be prog1?
		     `(progn
			,(instrument-internal form
			   fn-name params bound-vars nil)
			,(make-write-to-data-trace
			  (pcl::symbol-append fn-name ":::END")
			  bound-vars))
		   form)))))

	  ((eq head 'do-od)
	   (instrument-internal (macroexpand form)
	     fn-name params bound-vars lastp))

	  ;; 	  ;; This is cheating:  it tells me where to insert invariants.
	  ;; 	  ((eq head 'pre)
	  ;; 	   (make-write-to-data-trace
	  ;; 	     (pcl::symbol-append fn-name ":::PRE")
	  ;; 	     bound-vars))
	  ;; 	  ((eq head 'post)
	  ;; 	   (make-write-to-data-trace
	  ;; 	     (pcl::symbol-append fn-name ":::POST")
	  ;; 	     bound-vars))
	  ;; 	  ((eq head 'inv)
	  ;; 	   (make-write-to-data-trace
	  ;; 	     (gensym (pcl::symbol-append fn-name ":::INV"))
	  ;; 	     bound-vars))

	  (t
	   (if lastp
	       `(progn
		  ,(instrument-internal form
		     fn-name params bound-vars nil)
		  ,(make-write-to-data-trace
		    (pcl::symbol-append fn-name ":::END")
		    bound-vars))
	     form)))))

(defun make-write-to-data-trace (marker bound-vars)
  "BOUND-VARS is a list of (VAR . TYPE) conses."
  (write-data-trace-declaration *decl-output-stream* marker bound-vars)
  `(write-to-data-trace
    ,marker				; no quote around this
    ,bound-vars))
;; (make-write-to-data-trace 'foo '((x . integer)))
;; (make-write-to-data-trace 'foo '((x . integer) (y . integer) (z . integer)))
;; (make-write-to-data-trace 'foo '((x . integer) (y . array)))
;; (make-write-to-data-trace 'foo '((x . integer) (y . array) (z . integer)))

;;; Old implementation that merged adjacent variables of the same type.
;;; The added complexity isn't worth the small potential increase in efficiency.
;; (defun make-write-to-data-trace (marker bound-vars)
;;   "BOUND-VARS is a list of (VAR . TYPE) conses."
;;   `(write-to-data-trace
;;     ,marker				; no quote around this
;;     ,@(let ((result '())
;; 	    (prev-type nil)
;; 	    (prev-vars '()))
;; 	(loop for v+t in bound-vars
;; 	      do (let ((var (car v+t))
;; 		       (type (cdr v+t)))
;; 		   (if (equal type prev-type)
;; 		       (setq prev-vars (cons var prev-vars))
;; 		     (progn
;; 		       (if prev-type
;; 			   (setq result (cons (cons prev-type (reverse prev-vars))
;; 					      result)))
;; 		       (setq prev-type type
;; 			     prev-vars (list var))))))
;; 	(if prev-type
;; 	    (setq result (cons (cons prev-type (reverse prev-vars))
;; 			       result)))
;; 	(reverse result))))
;; (make-write-to-data-trace 'foo '((x . integer)))
;; (make-write-to-data-trace 'foo '((x . integer) (y . integer) (z . integer)))
;; (make-write-to-data-trace 'foo '((x . integer) (y . array)))
;; (make-write-to-data-trace 'foo '((x . integer) (y . array) (z . integer)))


(defun declare->vars+types (form)
  (and (eq 'declare (first form))
       (loop for decl in (cdr form)
	     append (if (eq 'type (first decl))
			(let ((type (second decl)))
			  (mapcar #'(lambda (v)
				      (cons v type))
				  (cddr decl)))))))
;; (declare->vars+types '(declare (type integer x) (type (array integer 2) b) (type integer m n)))


;; could add lots of stuff here:  *f (incf,decf), push, set, more
(defun modified-variables (form)
  (cond ((not (consp form))
	 '())
	((memq (car form) '(setq psetq setf psetf))
	 (let ((vars-and-values (cdr form))
	       (result '()))
	   (loop while vars-and-values
		 do (setq result (cons (place->var (car vars-and-values))
				       result)
			  vars-and-values (cddr vars-and-values)))
	   result))
	((eq (car form) 'swap)
	 (mapcar #'place->var (cdr form)))
	(t
	 (append (modified-variables (car form))
		 (modified-variables (cdr form))))))

(defun place->var (place)
  (if (symbolp place)
      place
    (let ((place-head (car place)))
      (cond ((eq place-head 'aref)
	     (second place))
	    (t
	     (error "can't cope with place ~s" place))))))


(defun instrument-file (ifilename ofilename &optional declfilename)
  (with-open-file (istream ifilename :direction :input)
    (with-open-file (ostream ofilename :direction :output
			     :if-exists :rename)
      (with-open-file (declstream (or declfilename
				      (merge-pathnames
				       (make-pathname :type "decls")
				       ofilename))
				  :direction :output
				  :if-exists :rename)
	(setq *decl-output-stream* declstream)
	(format *decl-output-stream* "# Declarations for ~s.~%"
		(namestring (truename ifilename)))
	;; There must be a better way to output the date!
	(multiple-value-bind
	    (sec min hour date month year weekday dstp time-zone)
	    (decode-universal-time (get-universal-time))
	  (declare (ignore weekday dstp time-zone))
	  (format *decl-output-stream* "# Written ~s ~a ~s ~2,'0d:~2,'0d:~2,'0d~%~%"
		  date (aref ext::abbrev-month-table month) year hour min sec))
	(let ((eof nil)
	      (eof-marker (gensym)))
	  (loop while (not eof)
		do (let ((form (read istream nil eof-marker)))
		     (if (eq form eof-marker)
			 (setq eof t)
		       (progn
			 (print (instrument form) ostream)
			 (terpri ostream)))))))))
  (compile-file ofilename))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lackwit-style type inference
;;;

;; This implementation is purely local and non-polymorphic.
;; It is intended to at least work on the Gries programs.

;; Using a global variable is a hack.
(defvar *lackwit-env*
  '()
  "A list of lists of variables.
The variables in each sublist have the same Lackwit type.")

(defvar *lackwit-seen-procedures*
  '())

(defvar *lackwit-arrays*
  '()
  "A list of variables in *lackwit-env* of array type.
Each is a list of (array elt-type index-type ...).")

(defun array-derived-vars (array indices)
  (or (assoc array *lackwit-arrays*)
      (let ((derived-vars (list array)))
	;; (format t "Adding vars ~s ~s~%" array-index-type-var array-element-type-var)
	(let ((array-element-type-var (symbol-append array "-element")))
	  (lackwit-add-var-maybe array-element-type-var)
	  (push array-element-type-var derived-vars))
	(loop for i from 1 to indices
	      do (let ((index-type-var
			(symbol-append array "-index" (format nil "~d" i))))
		   (lackwit-add-var-maybe index-type-var)
		   (push index-type-var derived-vars)))
	(setq derived-vars (nreverse derived-vars))
	(push derived-vars *lackwit-arrays*)
	derived-vars)))


(defun lackwit-infer (form)
  "Determine Lackwit-style types for variables bound in Lisp expression FORM."
  (setq *lackwit-env* nil)
  (setq *lackwit-arrays* nil)
  (lackwit-infer-internal form)
  (lackwit-merge-array-vars)
  *lackwit-env*
  'this-is-the-return-value-from-lackwit-infer)


;; I should use union-find, but this is easier to implement and adequate
;; for small programs.
;; nil is the type for literals.
(defun lackwit-add-var (v)
  ;; Can't yet deal with shadowed variables.
  (assert (not (lackwit-var-representative v)))
  ;; (format t "lackwit-add-var ~s ~s~%" v *lackwit-env*)
  (push (list v) *lackwit-env*)
  ;; (format t "lackwit-add-var (post ~s) ~s~%" v *lackwit-env*)
)
(defun lackwit-add-var-maybe (v)
  (if (not (lackwit-var-representative v))
      ;; "push" wasn't modifying *lackwit-env* in place; I'm not sure why.
      (push (list v) *lackwit-env*)))
(defun lackwit-add-vars (vars)
  (setq *lackwit-env* (nconc (mapcar #'list vars) *lackwit-env*)))
(defun lackwit-var-representative (v)
  "Return the representative for this variable.
That's a list, not a single variable."
  (and (not (numberp v))
       (some #'(lambda (l) (and (member v l) l)) *lackwit-env*)))
(defun lackwit-merge-vars (v1 v2)
  ;; returns the new type
  ;; (format t "lackwit-merge-vars: ~s ~s ~s~%" v1 v2 *lackwit-env*)
  (if (and v1 v2 (not (eq v1 v2)) (not (or (numberp v1) (numberp v2))))
      (let ((r1 (lackwit-var-representative v1))
	    (r2 (lackwit-var-representative v2)))
	(assert (and r1 r2))
	(if (not (eq r1 r2))
	    (progn
	      (setq *lackwit-env* (delete r2 *lackwit-env*))
	      (nconc r1 r2)
	      ;; (format t "after lackwit-merge-vars: ~s ~s ~s~%" r1 r2 *lackwit-env*)
	))
	v1)))

(defun lackwit-merge-array-vars ()
  "For array variables that have been merged, merge all their derived types.
The derived types are the types of their elements and indices."
  (loop for arrayvars in (remove-duplicates
			  (mapcar #'lackwit-var-representative
				  (mapcar #'car *lackwit-arrays*))
			  :test #'equal)
	do (if (> (length arrayvars) 1)
	       (let ((derivee-lists (mapcar #'(lambda (var)
						(assoc var *lackwit-arrays*))
					    arrayvars)))
		 (if (some #'not derivee-lists)
		     ;; Some argument was not known to be an array (was
		     ;; never operated on in array context); find some
		     ;; argument that is known to be an array.
		     (let ((indices (- (length (some #'identity derivee-lists)) 2)))
		       (setq derivee-lists
			     (and (> indices 0)
				  (mapcar #'(lambda (arrayvar)
					      (array-derived-vars arrayvar indices))
					  arrayvars)))))
		 (if derivee-lists
		     (progn
		       (assert (apply #'= (mapcar #'length derivee-lists)))
		       (let ((repr1 (car derivee-lists)))
			 (loop for repr2 in (cdr derivee-lists)
			       do (mapcar #'lackwit-merge-vars
					  (cdr repr1) (cdr repr2))))))))))


(defun lackwit-infer-body (forms)
  (loop for f in (butlast forms)
	do (lackwit-infer-internal f))
  (lackwit-infer-internal (car (last forms))))

(defun lackwit-infer-internal (form)
  (if (atom form)
      (car (lackwit-var-representative form))
    (let ((head (first form)))
      (cond
            ;; don't need inv and bound; they're buried inside do-od
            ((member head '(;; Common Lisp
			    declare
			    ;; Gries Lisp
			    pre post skip swap inv))
	     nil)
	    ((eq head 'defun)
	     (let ((fn-name (second form))
		   (params (third form))
		   (body (cdddr form)))
	       (push fn-name *lackwit-seen-procedures*)
	       (lackwit-add-vars params)
	       (lackwit-infer-body body)))
	    ;; since we don't allow repeated variable names,
	    ;; let and let* are identical.
	    ((member head '(let let*))
	     (let ((bindings (second form))
		   (body (cddr form)))
	       (loop for binding in bindings
		     do (if (and (listp binding) (not (null (cdr binding))))
			    (let* ((var (first binding))
				   (val (second binding))
				   (val-type (lackwit-infer-internal val)))
			      (lackwit-add-var var)
			      (lackwit-merge-vars var val-type))))
	       (lackwit-infer-body body)))
	    ((eq head 'progn)
	     (let ((body (cdr form)))
	       (lackwit-infer-body body)))
	    ((eq head 'loop)
	     (cond
	      ((and (>= (length form) 5)
		    (eq 'while (nth 1 form))
		    (eq 'do (nth 3 form)))
	       (let ((pred (nth 2 form))
		     (body (nthcdr 4 form)))
		 (lackwit-infer-internal pred)
		 (lackwit-infer-body body)))
	      ((and (>= (length form) 9)
		    (eq 'for (nth 1 form))
		    (eq 'from (nth 3 form))
		    (eq 'to (nth 5 form))
		    (eq 'do (nth 7 form)))
	       (let* ((var (nth 2 form))
		      (limit1 (nth 4 form))
		      (limit2 (nth 6 form))
		      (body (nthcdr 8 form))
		      (limit1-type (lackwit-infer-internal limit1))
		      (limit2-type (lackwit-infer-internal limit2))
		      (limit-type (lackwit-merge-vars limit1-type limit2-type)))
		 (lackwit-add-var var)
		 (lackwit-merge-vars var limit-type)
		 (lackwit-infer-body body)))
	      (t
	       (error "Deal with 'loop"))))
	    ((member head '(do-od if-fi cond))
	     (let ((clauses (if (eq head 'do-od) (cdddr form) (cdr form))))
	       (loop for clause in clauses
		     do (let ((predicate (car clause))
			      (clause-body (cdr clause)))
			  (lackwit-infer-internal predicate)
			  (lackwit-infer-body clause-body)))))
	    ((eq head 'if)
	     (let* ((pred (nth 1 form))
		    (then-expr (nth 2 form))
		    (else-body (nthcdr 3 form))
		    (then-type (lackwit-infer-internal then-expr))
		    (else-type (lackwit-infer-body else-body)))
	       (lackwit-infer-internal pred)
	       (lackwit-merge-vars then-type else-type)))
	    ((member head '(setq psetq setf psetf))
	     (let ((last-type nil)
		   (vars-and-values (cdr form)))
	       (loop while vars-and-values
		     do (let* ((var (first vars-and-values))
			       (value (second vars-and-values))
			       (value-type (lackwit-infer-internal value)))
			  (if (symbolp var)
			      (assert (lackwit-var-representative var))
			    (setq var (lackwit-infer-internal var)))
			  (lackwit-merge-vars var value-type)
			  (setq vars-and-values (cddr vars-and-values))
			  (setq last-type var)))
	       last-type))
	    ;; These functions return something of the same type as their
	    ;; arguments, whose types are all merged.
	    ((member head '(+ - = /= > >= < <= min max 1-
			      copy-array
			      ;; It's arguable whether these should be
			      ;; here.  I'll put them in for now.
			      * / mod floor))
	     (let* ((types (delete nil (mapcar #'lackwit-infer-internal (cdr form))))
		    (types-len (length types)))
	       ;; (format t "~s types: ~s~%" form types)
	       (if (not (null types-len))
		   (progn
		     (loop for typ in (cdr types)
			   do (lackwit-merge-vars (car types) typ))
		     (car types)))))
	    ((eq head 'aref)
	     ;; (format t "aref: ~s~%" form)
	     (assert (>= (length form) 3))
	     (let* ((array (second form))
		    (indices (cddr form))
		    ;; derived-vars is:  (array array-element array-index1 ...)
		    (derived-vars (array-derived-vars array (length indices))))
	       ;; Alternately to this assert, at the end I could fix up all
	       ;; the arrays that should have been merged; that wouldn't be
	       ;; too hard.
	       (assert (symbolp array))
	       (assert (= (length indices) (- (length derived-vars) 2)))
	       (loop for index-type-var in (cdr (cdr derived-vars))
		     for index-expr in indices
		     do (lackwit-merge-vars
			 index-type-var (lackwit-infer-internal index-expr)))
	       ;; return value:  array element type
	       (nth 1 derived-vars)))

	    ((member head '(and or not))
	     ;; Note that we do not merge the types of the arguments to
	     ;; these operators, because of common usage.
	     (lackwit-infer-body (cdr form)))
	    ;; Deal with all other function calls
	    (t
	     (if (not (or (member head '(tagbody random-range with-data-trace))
			  (member head *lackwit-seen-procedures*)))
		 (format t "Taking default action for ~s~%" form))
	     (lackwit-infer-body (cdr form))
	     ;; Could keep info about types for functions and return that here.
	     nil)))))



;; Local Variables:
;; eval: (put 'instrument-internal 'lisp-indent-function 1)
;; End:
