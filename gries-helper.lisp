;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

;; These come first because some are macros.

(defmacro skip ()
  nil)

;; Watch out:  the arguments can be evaluated more than once.
(defmacro swap (place1 place2)
  `(psetf ,place1 ,place2 ,place2 ,place1))
;; (macroexpand '(swap a b))
;; (macroexpand '(swap i (aref b i)))

(defmacro if-fi (&rest clauses)
  `(cond ,@clauses
	 (t (error "no test matched in if-fi"))))
;; (macroexpand '(if-fi (a b) (c d) (e f)))

(defmacro do-od ((inv-symbol invariant) (bound-symbol bound) &rest clauses)
  (declare (ignore invariant bound))
  (assert (eq inv-symbol 'inv))
  (assert (eq bound-symbol 'bound))
  ;; (intern (symbol-name ...)):  yuck, but it does eliminate unsightly
  ;; "#:" prefix in printed representation.
  (let ((do-od-matched-var (intern (symbol-name (gensym "DO-OD-MATCHED-")))))
    `(let ((,do-od-matched-var t))
       (loop while ,do-od-matched-var
	     do (cond ,@clauses
		      (t (setq ,do-od-matched-var nil)))))))
;; (macroexpand '(do-od (inv foo) (bound (- n i)) ((/= i n) body1) ((/= i n2) body2)))
(defun do-od-matched-symbol-p (s)
  (let ((name (symbol-name s)))
    (and (> (length name) 14)
	 (equal "DO-OD-MATCHED-" (subseq name 0 14)))))


(defmacro pre (&rest condition)
  (declare (ignore condition))
  nil)

(defmacro post (&rest condition)
  (declare (ignore condition))
  nil)

(defmacro inv (&rest condition)
  (declare (ignore condition))
  nil)


(defun copy-array (a)
  "Shallow-copy an array."
  (let ((result (make-array (array-dimensions a))))
    (loop for i from 0 to (- (array-total-size a) 1)
	  do (setf (row-major-aref result i) (row-major-aref a i)))
    result))

(defun copy (x)
  (cond ((symbolp x) x)
	((numberp x) x)
	((listp x) (copy-list x))
	((arrayp x) (copy-array x))
	(t (error "don't know how to copy"))))


(defmacro string-append (&rest args)
  `(concatenate 'string ,@args))

(defun random-range (min max)
  (+ (random (- max min -1)) min))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instrumentation
;;;

(defun instrument (form)
  "Insert calls to `check-assertion' in the body."
  (instrument-internal form nil nil nil))

(defun instrument-internal (form fn-name bound-vars lastp)
  ;; This function should not call instrument, only instrument-internal.

  ;; Should make good choices about where to insert the instrumentation and
  ;; which variables to instrument.
  ;; Also, perhaps only compare certain pairs of variables (when one or the
  ;; other is new or has changed) with one another, not all variables.

  (let ((head (if (consp form) (car form) 'nothing-in-particular)))
    (cond ((eq head 'defun)
	   (if (not (null bound-vars))
	       (error "bound variables while processing defun"))
	   (if fn-name
	       (error "function name while processing defun")
	     (setq fn-name (second form)))
	   (if lastp
	       (error "last while processing defun"))
	   (let* ((params (third form))
		  (body (cdddr form))

		  (params-modified (intersection params
						 (modified-variables form)))
		  (params-orig (mapcar #'(lambda (p)
					   (pcl::symbol-append 'orig- p))
				       params-modified))

		  ;; I could just use the cdrs of the (type ...) forms in
		  ;; an argument to check-for-invariants, but I want to
		  ;; make sure every variable appears, so destruct them.

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
		  ;; them.  Maybe only do that if they can be modified
		  ;; somewhere.  Maybe do that for all forms that
		  ;; potentially modify a variable.

		  (body-sans-last-stmt
		   ;; I could imagine adding instrumentation between
		   ;; every pair of body forms.  Is that worthwhile??
		   ;; (I don't have invariants at every location...)
		   (mapcar #'(lambda (stmt)
			       (instrument-internal stmt
				 fn-name (append vars+types vars+types-orig) nil))
			   (if decl
			       (cdr (butlast body))
			     (butlast body))))
		  (last-stmt (instrument-internal (car (last body))
			       fn-name (append vars+types vars+types-orig) t))
		  ;; Avoid extraneous "progn" (aesthetic fix only)
		  (result-body
		   (append body-sans-last-stmt
			   (if (eq 'progn (car last-stmt))
			       (cdr last-stmt)
			     (list last-stmt))))
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
		,(make-check-for-invariants
		  (pcl::symbol-append fn-name ":::BEGIN")
		  vars+types)
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
				      fn-name (append bound-vars vars+types) nil))
				(if decl
				    (cdr (butlast body))
				  (butlast body)))))
		  (last-stmt (instrument-internal (car (last body))
			       fn-name (append bound-vars vars+types) lastp)))
	     ;; Could also assert that no more types are declared (because I
	     ;; will check for invariants over everything declared...).
	     (assert (every #'(lambda (v)
				(or (do-od-matched-symbol-p v)
				    (assoc v vars+types)))
			    new-vars))

	     (append result-sans-last-stmt
		     (if (eq 'progn (car last-stmt))
			 (cdr last-stmt)
		       (list last-stmt)))))

	  ((eq head 'progn)
	   (let* ((body (cdr form))
		  (result-sans-last-stmt
		   `(progn
		      ,@(mapcar #'(lambda (stmt)
				    (instrument-internal stmt
				      fn-name bound-vars nil))
				(butlast body))))
		  (last-stmt (instrument-internal (car (last body))
			       fn-name bound-vars t)))
	     (append result-sans-last-stmt
		     (if (eq 'progn (car last-stmt))
			 (cdr last-stmt)
		       (list last-stmt)))))

	  ;; PROBLEM with do-od: I need to insert something in the
	  ;; macroexpanded version.

	  ((eq head 'loop)
	   (if (and (eq (second form) 'while)
		    (do-od-matched-symbol-p (third form))
		    (eq (fourth form) 'do)
		    (= 5 (length form)))
	       `(loop while ,(third form) do
		      (progn
			,(make-check-for-invariants
			  (gensym (string-append (symbol-name fn-name) ":::LOOP-"))
			  bound-vars)
			,(instrument-internal (fifth form)
			   fn-name bound-vars lastp)))
	     (let ((do-position (position 'do form)))
	       (if (eq do-position (- (length form) 2))
		   `(,@(butlast form)
		       ,(instrument-internal (car (last form))
			  fn-name bound-vars lastp))
		 (if lastp
		     `(progn
			,(instrument-internal form
			   fn-name bound-vars nil)
			,(make-check-for-invariants
			  (pcl::symbol-append fn-name ":::END")
			  bound-vars))
		   form)))))

	  ((eq head 'do-od)
	   (instrument-internal (macroexpand form)
	     fn-name bound-vars lastp))

	  ;; 	  ;; This is cheating:  it tells me where to insert invariants.
	  ;; 	  ((eq head 'pre)
	  ;; 	   (make-check-for-invariants
	  ;; 	     (pcl::symbol-append fn-name ":::PRE")
	  ;; 	     bound-vars))
	  ;; 	  ((eq head 'post)
	  ;; 	   (make-check-for-invariants
	  ;; 	     (pcl::symbol-append fn-name ":::POST")
	  ;; 	     bound-vars))
	  ;; 	  ((eq head 'inv)
	  ;; 	   (make-check-for-invariants
	  ;; 	     (gensym (pcl::symbol-append fn-name ":::INV"))
	  ;; 	     bound-vars))

	  (t
	   (if lastp
	       `(progn
		  ,(instrument-internal form
		     fn-name bound-vars nil)
		  ,(make-check-for-invariants
		    (pcl::symbol-append fn-name ":::END")
		    bound-vars))
	     form)))))

(defun make-check-for-invariants (marker bound-vars)
  "BOUND-VARS is a list of (VAR . TYPE) conses."
  `(check-for-invariants
    ,marker				; no quote around this
    ,@(let ((result '())
	    (prev-type nil)
	    (prev-vars '()))
	(loop for v+t in bound-vars
	      do (let ((var (car v+t))
		       (type (cdr v+t)))
		   (if (equal type prev-type)
		       (setq prev-vars (cons var prev-vars))
		     (progn
		       (if prev-type
			   (setq result (cons (cons prev-type (reverse prev-vars))
					      result)))
		       (setq prev-type type
			     prev-vars (list var))))))
	(if prev-type
	    (setq result (cons (cons prev-type (reverse prev-vars))
			       result)))
	(reverse result))))
;; (make-check-for-invariants 'foo '((x . integer)))
;; (make-check-for-invariants 'foo '((x . integer) (y . integer) (z . integer)))
;; (make-check-for-invariants 'foo '((x . integer) (y . array)))
;; (make-check-for-invariants 'foo '((x . integer) (y . array) (z . integer)))


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


(defun instrument-file (ifilename ofilename)
  (with-open-file (istream ifilename :direction :input)
    (with-open-file (ostream ofilename :direction :output
			     :if-exists :supersede)
      (let ((eof nil)
	    (eof-marker (gensym)))
	(loop while (not eof)
	      do (let ((form (read istream nil eof-marker)))
		   (if (eq form eof-marker)
		       (setq eof t)
		     (progn
		       (print (instrument form) ostream)
		       (terpri ostream))))))))
  (compile-file ofilename))


;; Local Variables:
;; eval: (put 'instrument-internal 'lisp-indent-function 1)
;; End:
