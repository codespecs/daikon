;; gries-helper.lisp

;; The programs in Gries's _The Science of Programming_ are written in a
;; variant of Dijkstra's guarded command language.

;; The semantics of the Gries-style Lisp code is as follows:
;;  * if-fi:  nondeterministically choose one of the true guards and execute
;;    its body.  Err if no guard is true.
;;  * do-od:  while any guards are true, nondeterministically choose one of
;;    the true guards and execute its body.  Exits when no guard is true
;;    (which might be on the first iteration, so no body would be executed).
;;  * skip:  no-op; useful in if-fi bodies
;;  * pre, post, inv:  ignored and run and compile time; these are Gries' pre-
;;    and post-conditions, and other invariants he notes in comments, and they
;;    represent the goal.  For instance, for the four p177* programs, the goal
;;    is to determine that, at both the beginning and end of the function, j =
;;    k (mod 10).  This is successfully discovered, along with other
;;    properties as well.  (Actually, it occurs to me now that the stronger
;;    statement j = k mod 10 would be preferable to j = k (mod 10); that
;;    should be trivial to fix.)
;;  * psetf:  parallel assignment:  compute the values before assigning to the
;;    variables (this is built into Common Lisp)
;;  * swap:  swap values of two variables, transliterated from (psetf x y y
;;    x).  This is one of my few departures from Gries' style.  I also used
;;    setf instead of psetf where appropriate (Gries has only psetf, no setf),
;;    again because I found the code much easier to understand when I knew
;;    whether the parallelism was being exploited or not.  These changes have
;;    no effect on the run-time behavior, which is all that really matters:
;;    they're just different ways of expressing the same single statement.
;;  * declare:  Lisp type annotations added by me to simplify my job; implicit
;;    in the code


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gries-style syntax
;;;

;; These come first because some are macros.

(defmacro skip ()
  "The no-op operation in the Gries language."
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for tests of Gries functions
;;;

(defun random-range (min max)
  (+ (random (- max min -1)) min))

;; This implementation is completely unreasonable in its speed.
;; There must be a closed-form solution for this I could use instead.
(defun random-harmonic (divisor)
  "Return a random integer greater than or equal to 1.
Value 1 has probability 1/DIVISOR; 2 has probability 1/(2*divisor);
3 has probability 1/(3*divisor); and so forth."
  (do ((i 1 (+ i 1)))
      ((zerop (random (* divisor i))) i)))
;; Testing
;; (mapcar #'random-harmonic '(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))

(defun random-harmonic-signed (divisor)
  "Like random-harmonic, but can produce negative, positive, or zero values."
  (let ((pos-num (random-harmonic divisor)))
    (* (if (zerop (random 2)) 1 -1)
       (1- pos-num))))

(defun random-exponential (scale)
  "Return a random non-negative integer.
Value i has probability (e^-i)/SCALE (I think)."
  (floor (* (- scale) (log (random 1.0)))))
;; Testing
;; (mapcar #'random-exponential '(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))

(defun random-exponential-signed (ratio)
  "Like random-exponential, but can produce negative, positive, or zero values."
  (let ((pos-num (random-exponential ratio)))
    (if (zerop (random 2))
	pos-num
      (- pos-num))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

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
