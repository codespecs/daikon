;; Programs from "The Science of Programming", by David Gries.

;; Massage the programs:
;;  * list all the variables, for convenience

;; Conventions:  any forms in a single setq can all be evaluated in
;; parallel or sequentially.  A psetf requires parallel execution.  When
;; one setq follows another, all of the first must be executed before any
;; of the second.


;; Should already have loaded gries-helper.lisp, which defines macros and such.


;; (load "invariants.lisp")
;; (load "gries-helper.lisp")
;; (load "gries.lisp")

;; (instrument-file "gries.lisp" "gries-instrumented.lisp")
;; (compile-file "gries-instrumented.lisp")

;; (compile-file "invariants.lisp")
;; (compile-file "gries-helper.lisp")
;; (compile-file "gries.lisp")

;; (load "invariants")
;; (load "gries-helper")
;; ;; (load "gries")
;; (load "gries-instrumented")

;; (test-all)
;; (test-p173-14.3)
;; (test-p176)
;; (test-p177-14.8)

;; Python:
;; import invariants
;; reload(invariants)
;; invariants.read_invs('p173-14.3.inv')
;; invariants.all_numeric_invariants()


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 14: Programming as a goal-oriented activity
;;;

;; page 173, program (14.3):
;; compute max of two numbers
(defun p173-14.3 (x y)
  (declare (type integer x y))
  (pre true)
  ;; z is a new variable
  (let (z)
    (declare (type (or integer null) z))	; yuck
    (if-fi ((>= x y) (setq z x))
	   ((>= y x) (setq z y)))
    ;; equivalently, (and (>= z x) (>= z y) (or (= z x) (= z y)))
    (post (= (z (max x y))))))

(defun test-p173-14.3 ()
  (with-invariants-log "p173-14.3.inv"
    (loop for i from 0 to 100
	  do (p173-14.3 (random-range -100 100) (random-range -100 100)))))

;; page 176:
;; set x to smaller of inputs, y to larger
(defun p176 (x y)
  (declare (type integer x y))
  (pre (and (= x orig-x) (= y orig-y)))
  (if-fi ((<= x y) (skip))
	 ((<= y x) (swap x y)))
  (post (and (<= x y) (or (and (= x orig-x) (= y orig-y))
			  (and (= y orig-x) (= x orig-y))))))

(defun test-p176 ()
  (with-invariants-log "p176.inv"
    (loop for i from 0 to 100
	  do (p176 (random-range -100 100) (random-range -100 100)))))


;;; increment k under the invariance j = k mod 10

;; page 177, program (14.8):
;; increment k under the invariance j = k mod 10
(defun p177-14.8 (j k)
  (declare (type integer j k))
  (pre (= j (mod k 10)))
  (if-fi ((< j 9) (setq k (+ k 1) j (+ j 1)))
	 ((= j 9) (setq k (+ k 1) j 0)))
  (post (= j (mod k 10))))

(defun test-p177-14.8 ()
  (with-invariants-log "p177-14.8.inv"
    (loop for i from 0 to 100
	  do (let ((k (random-range -100 100)))
	       (p177-14.8 (mod k 10) k)))))

;; page 177, program (14.9):
;; increment k under the invariance j = k mod 10
(defun p177-14.9 (j k)
  (declare (type integer j k))
  (pre (= j (mod k 10)))
  (if-fi ((< j 9) (setq k (+ k 1) j (+ j 1)))
	 ((>= j 9) (setq k (+ k 1) j 0)))
  (post (= j (mod k 10))))

(defun test-p177-14.9 ()
  (with-invariants-log "p177-14.9.inv"
    (loop for i from 0 to 100
	  do (let ((k (random-range -100 100)))
	       (p177-14.9 (mod k 10) k)))))

;; page 177, near bottom of page:
;; increment k under the invariance j = k mod 10
(defun p177-1 (j k)
  (declare (type integer j k))
  (pre (= j (mod k 10)))
  (if-fi ((and (<= 0 j) (< j 9))
	  (setq k (+ k 1) j (+ j 1)))
	 ((>= j 9)
	  (setq k (+ k 1) j 0)))
  (post (= j (mod k 10))))

(defun test-p177-1 ()
  (with-invariants-log "p177-1.inv"
    (loop for i from 0 to 100
	  do (let ((k (random-range -100 100)))
	       (p177-1 (mod k 10) k)))))

;; page 177, very bottom of page:
;; increment k under the invariance j = k mod 10
(defun p177-2 (j k)
  (declare (type integer j k))
  (pre (= j (mod k 10)))
  (setq k (+ k 1))
  (if-fi ((< j 9) (setq j (+ j 1)))
	 ((= j 9) (setq j 0)))
  (post (= j (mod k 10))))

(defun test-p177-2 ()
  (with-invariants-log "p177-2.inv"
    (loop for i from 0 to 100
	  do (let ((k (random-range -100 100)))
	       (p177-2 (mod k 10) k)))))

;;; end of: increment k under the invariance j = k mod 10

;; page 178, exercise 1(b); answer on page 342:
;; set x to abs(x)
(defun p178-1b (x)
  (declare (type integer x))
  (pre (= x orig-x))
  (if-fi ((>= x 0) (skip))
	 ((<= x 0) (setq x (- x))))
  ;; equivalently, (or (and (>= orig-x 0) (= x orig-x))
  ;;                   (and (<= orig-x 0) (= x (- orig-x))))
  (post (= x (abs orig-x))))

(defun test-p178-1b ()
  (with-invariants-log "p178-1b.inv"
    (loop for i from 0 to 100
	  do (p178-1b (random-range -100 100)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 15: Loops
;;;

;; page 180, program (15.1.1):
;; s = sum(b[0..n-1])
(defun p180-15.1.1 (b n)
  (declare (type (array integer 1) b)
	   (type integer n))
  (pre (>= n 0))
  (let ((i 0) (s 0))
    (declare (type integer i s))
    (do-od (inv (and (<= 0 i) (<= i n) (= s (sum j (0 <= j < i) (aref b j)))))
	   (bound (- n i))
	   ((/= i n) (psetq i (+ i 1) s (+ s (aref b i)))))
    (post (= s (sum (0 <= j < n) (aref b j))))))

(defun test-p180-15.1.1 ()
  (with-invariants-log "p180-15.1.1.inv"
    (loop for i from 0 to 100
	  do (let* ((n (random-range 7 13))
		    (b (make-array n)))
	       (loop for j from 0 to (- n 1)
		     do (setf (aref b j) (random-range -100 100)))
	       (p180-15.1.1 b n)))))

;;; This one isn't stated formally, but via pictures and "x not here".
;; page 183, program (15.1.7):
;; search multidimensional array b[0..m-1][0..n-1] for value x; set indices i, j
(defun p183-15.1.7 (x b m n)
  (declare (type integer x)
	   (type (array integer 2) b)
	   (type integer m n))
  (pre (and (> m 0) (> n 0)))				; not given in text
  (let ((i 0) (j 0))
    (declare (type integer i j))
    (do-od (inv (and (<= 0 i) (<= i m) (<= 0 j) (< j n)
		     (not (x in b[0..i-1][0..n-1]))
		     (not (x in b[i][0..j-1]))))
	   (bound (- (* (- m i) n) j))
	   ((and (/= i m) (/= x (aref b i j)))
	    (if-fi ((< j (- n 1)) (setq j (+ j 1)))
		   ((= j (- n 1)) (setq i (+ i 1) j 0)))))
    (post (or (and (<= 0 i) (< i m) (<= 0 j) (< j n) (= x (aref b i j)))
	      (and (= i m) (not (in x b)))))))

;; page 184, exercise 3:
;; set x to the smallest value in b[0..n-1]
(defun p184-3 (b n)
  (declare (type (array integer 1) b)
	   (type integer n))
  (pre (< 0 n))
  (let ((i 1) (x (aref b 0)))
    (declare (type integer i x))
    (do-od (inv (and (<= 1 i) (<= i n)
		     (<= x b[0..i-1]) (exists (j) (and (<= 0 j) (< j i) (= x (aref b j))))))
	   (bound (- n i))
	   ((/= i n)
	    (if-fi ((>= x (aref b i)) (psetq i (+ i 1) x (aref b i)))
		   ((<= x (aref b i)) (setq i (+ i 1))))))
    (post (and (<= x b[0..n-1]) (exists (j) (and (<= 0 j) (< j n) (= x (aref b j))))))))

;;; Perm isn't formally defined, and number is a mess; perhaps skip this one.
;; page 187, top:
;; sort the four integer inputs q0, q1, q2, q3
(defun p187 (q0 q1 q2 q3)
  (declare (type integer q0 q1 q2 q3))
  (pre (and (= q0 q0-orig) (= q1 q1-orig) (= q2 q2-orig) (= q3 q3-orig)))
  (do-od (inv (perm-4 q0 q1 q2 q3 q0-orig q1-orig q2-orig q3-orig))
	 (bound (number (i j) (and (<= 0 i) (< i j 4) (> qi qj))))
	 ((> q0 q1) (swap q0 q1))
	 ((> q1 q2) (swap q1 q2))
	 ((> q2 q3) (swap q2 q3)))
  (post (and (<= q0 q1 q2 q3)
	     (perm-4 q0 q1 q2 q3 q0-orig q1-orig q2-orig q3-orig))))


;;; This one isn't stated formally, but via pictures and "x not here".
;; page 189, program (15.2.4):
;; search possibly degenerate (empty) multidimensional array
;; b[0..m-1][0..n-1] for value x; set indices i, j
(defun p189-15.2.4 (x b m n)
  (declare (type integer x)
	   (type (array integer 2) b)
	   (type integer m n))
  (pre (and (<= 0 m) (<= 0 n)))
  (let ((i 0) (j 0))
    (declare (type integer i j))
    (do-od (inv (and (<= 0 i m) (<= 0 j n)
		     (not (x in b[0..i-1][0..n-1]))
		     (not (x in b[i][0..j-1]))))
	   (bound (- (* (- m i) n) j))
	   ((and (/= i m) (/= j n) (/= x (aref b i j)))
	    (setq j (+ j 1)))
	   ((and (/= i m) (= j n) (/= x (aref b i j)))
	    (setq i (+ i 1) j 0)))
    (post (or (and (<= 0 i) (< i m) (<= 0 j) (< j n) (= x (aref b i j)))
	      (and (= i m) (not (in x b)))))))


;;; This one is "bad" style:  it uses while loops and goto.  Perhaps not
;;; worth expanding the language I cope with to cover it.  Its loop
;;; invariants and bounds are probably the same as just above, p189-15.2.4.
;; page 190, program (15.2.5)
;; search possibly degenerate (empty) multidimensional array
;; b[0..m-1][0..n-1] for value x; set indices i, j
(defun p190-15.2.5 (x b m n)
  (declare (type integer x)
	   (type (array integer 2) b)
	   (type integer m n))
  (pre (and (<= 0 m) (<= 0 n)))
  (let ((i 0) (j 0))
    (declare (type integer i j))
    (tagbody
     (loop while (/= i m)
	   do (loop while (/= j n)
		    do (if (= x (aref b i j))
			   (go loopexit)
			 (setq j (+ j 1))))
	   (setq i (+ i 1) j 0))
     loopexit
     (post (or (and (<= 0 i) (< i m) (<= 0 j) (< j n) (= x (aref b i j)))
	       (and (= i m) (not (in x b))))))))


;; page 191, exercise 2:
;; set x and y to the greatest common divisor (gcd x y)
(defun p191-2 (orig-x orig-y)
  (declare (type integer orig-x orig-y))
  (pre (and (> orig-x 0) (> orig-y 0)))
  (let ((x orig-x) (y orig-y))
    (declare (type integer x y))
    (do-od (inv (and (< x 0) (< y 0) (= (gcd x y) (gcd orig-x orig-y))))
	   (bound (+ x y))
	   ((> x y) (setq x (- x y)))
	   ((> y x) (setq y (- y x))))
    (post (and (< 0 x) (< y 0) (= (gcd x y) (gcd orig-x orig-y)) (= x y (gcd x y))))))


;;; This one isn't stated formally, but via "the input line" and concatenation
;; page 192, exercise 5:
;; strip off first word of input
(defun p192-2 (b j s)
  (declare (type (array character 1) b)
	   (type integer j)
	   (type (array character 1) s))
  (pre (and (<= 0 j) (< j 80)
	    (array-equal (string-append b[j..79] the-input-lines) (string-append w "-" rest))))
  (let ((tt 0))
    (declare (type integer tt))
    (do-od (inv (and (<= 0 j 80) (<= 0 v (length w))
		     (array-equal (string-append s[0..v-1] b[j..79]) (string-append w "-" rest))))
	   (bound (+ (* 2 (length w)) (* -2 v) j))
	   ((and (/= j 80) (not (equal (aref b j) #\space)))
	    (psetf tt (+ tt 1) (aref s (+ tt 1)) (aref b j) j (+ j 1)))
	   ((= j 80)
	    (gries-read b)
	    (setq j 0)))
    (pre (and (<= 0 j) (< j 80)
	      (array-equal s[0..length_of_w-1] w)
	      (array-equal (string-append b[j..79] the-input-lines) (string-append "-" rest))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chapter 16: Developing invariants
;;;

;; page 196, program (16.2.2):
;; set a to the approximate (integer) square root of n
(defun p196-16.2.2 (n)
  (declare (type integer n))
  (pre (<= 0 n))
  (let ((a 0))
    (declare (type integer a))
    (do-od (inv (<= 0 (* a a) n))
	   (bound (- (ceil (sqrt n)) a))
	   ((<= (* (+ a 1) (+ a 1)) n)
	    (setq a (+ a 1))))
    (post (and (<= 0 (* a a) n) (< n (* (+ a 1) (+ a 1)))))))


;;; Probably eliminate all but one of these p197-16.2.5 programs

;; page 197, program (16.2.5)
;; linear search:  x is in b[0..m-1]; set i to the first index containing x
(defun p197-16.2.5 (x b m)
  (declare (type integer x)
	   (type (array integer 1) b)
	   (type integer m))
  (pre (and (< 0 m) (in x b[0..m-1])))
  (let ((i 0))
    (declare (type integer i))
    (do-od (inv (and (<= 0 i) (< i m)
		     (forall j (suchthat (and (<= 0 j) (< j i))) (/= x (aref b j)))))
	   (bound ???)
	   ((/= x (aref b i)) (setq i (+ i 1))))
    (post (and (<= 0 i) (< i m) (not (in x b[0..i-1])) (= x (aref b i))))))


;; page 197, program (16.2.5), expanded postcondition noted in middle of page
;; linear search:  x is in b[0..m-1]; set i to the first index containing x
(defun p197-16.2.5-altpost (x b m)
  (declare (type integer x)
	   (type (array integer 1) b)
	   (type integer m))
  (pre (and (< 0 m) (in x b[0..m-1])))
  (let ((i 0))
    (declare (type integer i))
    (do-od (inv (and (<= 0 i) (< i m)
		     (forall j (suchthat (and (<= 0 j) (< j i))) (/= x (aref b j)))))
	   (bound ???)
	   ((/= x (aref b i)) (setq i (+ i 1))))
    (post (and (<= 0 i) (< i m)
	       (forall j (suchthat (and (<= 0 j) (< j i))) (/= x (aref b j)))
	       (= x (aref b i))))))

;; page 197, program (16.2.5)
;; fixed loop invariant noted on page 198, 16.2.6 (but it's true
;; throughout, not just in the loop:  because the loop doesn't change, that
;; isn't interesting).
;; linear search:  x is in b[0..m-1]; set i to the first index containing x
(defun p197-16.2.5 (x b m)
  (declare (type integer x)
	   (type (array integer 1) b)
	   (type integer m))
  (pre (and (< 0 m) (in x b[0..m-1])))
  (let ((i 0))
    (declare (type integer i))
    (do-od (inv (and (<= 0 i) (< i m)
		     (forall j (suchthat (and (<= 0 j) (< j i))) (/= x (aref b j)))))
	   (bound ???)
	   ((/= x (aref b i)) (setq i (+ i 1))))
    (post (and (<= 0 i) (< i m) (not (in x b[0..i-1])) (= x (aref b i))))))


;; page 197, program (16.2.5)
;; fixed loop invariant noted on page 198, 16.2.6
;; fixed loop invariant noted on page 198, 16.2.6 (but it's true
;; throughout, not just in the loop:  because the loop doesn't change, that
;; isn't interesting).
;; expanded postcondition noted in middle of page
;; linear search:  x is in b[0..m-1]; set i to the first index containing x
(defun p197-16.2.5-altpost (x b m)
  (declare (type integer x)
	   (type (array integer 1) b)
	   (type integer m))
  (pre (and (< 0 m) (in x b[0..m-1])))
  (let ((i 0))
    (declare (type integer i))
    (do-od (inv (and (<= 0 i) (< i m)
		     (forall j (suchthat (and (<= 0 j) (< j i))) (/= x (aref b j)))
		     (in x b[0..m-1])))
	   (bound ???)
	   ((/= x (aref b i)) (setq i (+ i 1))))
    (post (and (<= 0 i) (< i m)
	       (forall j (suchthat (and (<= 0 j) (< j i))) (/= x (aref b j)))
	       (= x (aref b i))))))



;; page 197, program (16.2.5), expanded postcondition noted in middle of page
;; linear search:  x is in b[0..m-1]; set i to the first index containing x
(defun p197-16.2.5-alt2 (x b m)
  (declare (type integer x)
	   (type (array integer 1) b)
	   (type integer m))
  (pre (and (< 0 m) (in x b[0..m-1])))
  (let ((i 0))
    (declare (type integer i))
    (do-od (inv (and (<= 0 i) (< i m)
		     (forall j (suchthat (and (<= 0 j) (< j i))) (/= x (aref b j)))
		     (in x b[0..m-1])))
	   (bound ???)
	   ((/= x (aref b i)) (setq i (+ i 1))))
    (post (and (<= 0 i) (< i m)
	       (forall j (suchthat (and (<= 0 j) (< j i))) (/= x (aref b j)))
	       (= x (aref b i))))))


;; page 199, exercise 2
;; given integer n, find largest power of 2 that is at most n
(defun p199-2 (n)
  (declare (type integer n))
  (pre (< 0 n))
  (let ((i 1))
    (declare (type integer i))
    (do-od (inv (and (< 0 i) (<= i n) (exists p (= (exp 2 p) i))))
	   (bound (- n i))
	   ((<= (* 2 i) n) (setq i (* 2 i))))
    (post (and (<= 0 i n) (< n (* 2 i)) (exists p (= (exp 2 p) i))))))


;; page 199, exercise 4
;; two-dimensional search:  find indices of x, which occurs in b
(defun p199-4 (x b m n)
  (declare (type integer x)
	   (type (array integer 2) b)
	   (type integer m n))
  (pre (in x b[0..m-1][0..n-1]))
  (let ((i 0) (j 0))
    (declare (type integer i j))
    (do-od (inv (and (<= 0 i) (< i m) (<= 0 j) (< j n)
		     (not (in x b[0..i-1][0..n-1]))
		     (not (in x b[i][0..j-1]))
		     (in x b)))
	   (bound (- (* (- m i) n) j))
	   ((and (/= x (aref b i j)) (/= j (- n 1)))
	    (setq j (+ j 1)))
	   ((and (/= x (aref b i j)) (= j (- n 1)))
	    (setq i (+ i 1) j 0)))
    (post (and (<= 0 i) (< i m) (<= 0 j) (< j n)
	       (= x (aref b i j))
	       (not (in x b[0..i-1][0..n-1]))
	       (not (in x b[i][0..j-1]))))))


;; page 202, program (16.3.7)
;; set a to the floor of the square root of n
(defun p202-16.3.7 (n)
  (declare (type integer n))
  (pre (< 0 n))				; not given in text
  (let ((a 0) (b (+ n 1)))
    (declare (type integer a b))
    (do-od (inv (and (< a b) (<= b (+ n 1))
		     (<= (* a a) n) (< n (* b b))))
	   (bound (+ b (-a) 1))
	   ((/= (+ a 1) b)
	    (let ((d (floor (/ (+ a b) 2))))
	      (declare (type integer d))
	      (if-fi ((<= (* d d) n) (setq a d))
		     ((> (* d d) n) (setq b d))))))
    (post (and (<= (* a a) n) (< n (* (+ a 1) (+ a 1)))))))


;; page 204, program (16.3.11)
;; store in p the length of the longest plateau of array b[0..n-1]
(defun p204-p16.3.11 (b n)
  (declare (type (array integer 1) b)
	   (type integer n))
  (pre ???)
  (let ((i 1) (p 1))
    (declare (type integer i p))
    (do-od (inv (and (<= 1 i n)
		     (exists k (suchthat (<= 0 k (- i p))) (= (aref b k) (aref b (+ k p -1))))
		     (forall k (suchthat (<= 0 k (- i p 1))) (/= (aref b k) (aref b (+ k p))))))
	   (bound (- n 1))
	   ((/= i n)
	    (if-fi ((/= (aref b i) (aref b (- i p))) (setq i (+ i 1)))
		   ((= (aref b i) (aref b (- i p))) (setq i (+ i 1) p (+ p 1))))))
    ;; postcondition: "p is the length of the longest plateau of b[0..n-1]"
    (post (and (exists k (suchthat (<= 0 k (- n p))) (= (aref b k) (aref b (+ k p -1))))
	       (forall k (suchthat (<= 0 k (- n p 1))) (/= (aref b k) (aref b (+ k p))))))))


;; page 205, exercise 4, part a
;; given x and fixed, ordered (by <=) array b[1..n] satisfying b[1] <= x < b[n],
;; find where x belongs in the array
(defun p205-4a (x b n)
  (declare (type integer x)
	   (type (array integer 1) b)
	   (type integer n))
  (pre ???)
  (let ((i 1) (j n))
    (declare (type integer i j))
    (do-od (inv (and (<= 1 i) (< i j) (<= j n) (<= (aref b i) x) (< x (aref b j))))
	   (bound (log (- j i)))
	   ((/= (+ i 1) j)
	    (let ((e (floor (/ (+ i j) 2))))
	      (declare (type integer e))
	      (if-fi ((<= (aref b e) x) (setq i e))
		     ((> (aref b e) x) (setq j e))))))
    (post (and (<= 1 i) (< i n) (<= (aref b i) x) (< x (aref b (+ i 1)))))))


;; page 205, exercise 4, part b
;; incorporate the above in a program with no restriction on the value x
(defun p205-4b (x b n)
  (declare (type integer x)
	   (type (array integer 1) b)
	   (type integer n))
  (pre ???)
  (let ((i 0) (j (+ n 1)))
    (declare (type integer i j))
    (do-od (inv (and (<= 0 i) (< i j) (<= j (+ n 1)) (<= (aref b i) x) (< x (aref b j))))
	   (bound (log (- j i)))
	   ((/= (+ i 1) j)
	    (let ((e (floor (/ (+ i j) 2))))
	      (declare (type integer e))
	      (inv (<= 1 e n))
	      (if-fi ((<= (aref b e) x) (setq i e))
		     ((> (aref b e) x) (setq j e))))))
    (post (or (and (= i 0) (< x (aref b 1)))
	      (and (<= 1 i) (< i n) (<= (aref b i) x) (< x (aref b (+ i 1))))
	      (and (= i n) (<= (aref b n) x))))))


;; page 206, exercise 10
;; find the length of the longest plateau of b[0..n-1]
(defun p210-10 (b n)
  (declare (type (array integer 1) b)
	   (type integer n))
  (pre ???)
  (let ((i 0) (p 0))
    (declare (type integer i p))
    (do-od (inv (and (<= 0 i n)
		     (exists k (suchthat (<= 0 k (- i p))) (= (aref b k) (aref b (+ k p -1))))
		     (forall k (suchthat (<= 0 k (- i p 1))) (/= (aref b k) (aref b (+ k p))))
		     (or (= i 0)
			 (= i n)
			 (/= (aref b (- i 1) (aref b i))))))
	   (bound (- n i))
	   ((/= i n)
	    (let ((j (+ i 1)))
	      (declare (type integer j))
	      (do-od (inv (b[i..j-1] are all equal))
		     (bound (- n j))
		     ((and (/= j n) (= (aref b j) (aref b i)))
		      (setq j (+ j 1))))
	      (setq p (max p (- j i)))
	      (setq i j))))
    ;; postcondition: "p is the length of the longest plateau of b[0..n-1]"
    (post (and (exists k (suchthat (<= 0 k (- n p))) (= (aref b k) (aref b (+ k p -1))))
	       (forall k (suchthat (<= 0 k (- n p 1))) (/= (aref b k) (aref b (+ k p))))))))


;;; This one is hopelessly fuzzy; for instance, the invariant and bound depend
;;; on the (unknown) final values of i, j, and k.
;; page 209, program (16.4.3)
;; "The welfare crook": find an element in all of arrays f, g, and h
(defun p209-16.4.3 (f g h)
  (declare (type (array integer 1) f g h))
  ;; The arrays are sorted, but the program invariants omit this fact (it
  ;; is mentioned in the text and the program depends on it).
  (pre true)
  (let ((i 0) (j 0) (k 0))
    (declare (type integer i j k))
    (do-od (inv (and (<= 0 i) (<= 0 j) (<= 0 k)))
	   (bound ???)
	   ((or (< (aref f i) (aref g j)) (< (aref f i) (aref h k))) (setq i (+ i 1)))
	   ((or (< (aref g j) (aref h k)) (< (aref g j) (aref f i))) (setq j (+ j 1)))
	   ((or (< (aref h k) (aref f i)) (< (aref h k) (aref g j))) (setq k (+ k 1))))
    (post (= (aref f i) (aref g j) (aref h k)))	; and these are the least such indices
    ))


;;; This one is hopelessly fuzzy; for instance, the invariant and bound depend
;;; on the (unknown) final values of i, j, and k.
;; page 210, program (16.4.5)
;; "The welfare crook": find an element in all of arrays f, g, and h
(defun p210-16.4.5 (f g h)
  (declare (type (array integer 1) f g h))
  ;; The arrays are sorted, but the program invariants omit this fact (it
  ;; is mentioned in the text and the program depends on it).
  (pre true)
  (let ((i 0) (j 0) (k 0))
    (declare (type integer i j k))
    (do-od (inv (and (<= 0 i) (<= 0 j) (<= 0 k)))
	   (bound ???)
	   ((< (aref f i) (aref g j)) (setq i (+ i 1)))
	   ((< (aref g j) (aref h k)) (setq j (+ j 1)))
	   ((< (aref h k) (aref f i)) (setq k (+ k 1))))
    (post (= (aref f i) (aref g j) (aref h k)))	; and these are the least such indices
    ))

;; page 212, program (16.5.1)
;; given n>=0, p>=0 and array b[0..n-1], add p*i to each element of b
(defun p212-16.5.1 (n p orig-b)
  (declare (type integer n p)
	   (type (array integer 1) orig-b))
  (pre (and (>= n 0) (>= p 0)))		; also each element of b = orig-b (duh)
  (let ((b (copy-array orig-b)))
    (declare (type (array integer 1) b))
    (let ((j 0))
      (declare (type integer j))
      (do-od (inv (and (<= 0 j n)
		       (forall i (suchthat (and (<= 0 i) (< i j))) (= (aref b i) (+ (aref orig-b i) (* p i))))
		       (forall i (suchthat (and (<= j i) (< i n))) (= (aref b i) (aref orig-b i)))))
	     (bound ???)
	     ((/= j n) (psetf j (+ j 1) (aref b j) (+ (aref b j) (* p j)))))
      (post (forall i (suchthat (and (<= 0 i) (< i n))) (= (aref b i) (+ orig-b (* p i))))))))


;;; This one doesn't have formal pre- and post- conditions (but I should
;;; derive them; I'm just being lazy to skip this).
;; page 213, program (16.5.3)
;; swap non-overlapping sections b[i..i+n-1] and b[j..j+n-1] of b[0..m-1]
(defun p213-16.5.3 (b m i j n)
  (declare (type (array integer 1) b)
	   (type integer m i j n))
  (pre (and (<= 0 i) (< i m) (<= 0 j) (< j m)
	    (< (+ i n -1) m) (< (+ j n -1) m)
	    ;; non-overlapping
	    (or (< (+ i n -1) j) (< (+ j n -1) i))))
  (let ((k 0))
    (declare (type integer k))
    (do-od (inv <<complicated>>)
	   (bound (- n k))
	   ((/= k n) (psetf k (+ k 1) (aref b (+ i k)) (aref b (+ j k)) (aref b (+ j k)) (aref b (+ i k)))))
    (post <<complicated>>)))


;; page 214, exercise 4
;; permute the values of the array to achieve quicksort-like partition
(defun partition (orig-b m n)
  (declare (type (array integer 1) orig-b)
	   (type integer m n))
  (let ((b (copy-array orig-b)))
    (declare (type (array integer 1) b))
    (let ((x (aref b m))
	  (q (+ m 1))
	  (p (- n 1)))
      (declare (type integer x q p))
      (do-od (inv (and (< m q) (<= q (+ p 1) n) (= x (aref b 1))
		       <<complicated>>))
	     (bound (+ p (- q) 1))
	     ((<= q p)
	      (if-fi ((<= (aref b q) x) (setq q (+ q 1)))
		     ((> (aref b p) x) (setq p (- p 1)))
		     ((and (> (aref b q) x) (>= x (aref b p)))
		      (swap (aref b q) (aref b p))
		      (setq q (+ q 1) p (- p 1))))))
      (inv (and (= p (- q 1))
		(<= b[m+1..p] (aref b m))
		(> b[p+1..n-1] (aref b m))
		(= (aref b m) (aref orig-b m) x)))
      (setf (aref b m) (aref b p))
      (post (and (<= m p) (< p n) (perm b orig-b)
		 <<complicated>>)))))


;;; exercise 7 (on page 215) is to write formal pre- and post-conditions
;;; for this exercise; so skip over it for the nonce.  I'm only doing what
;;; the book presents.
;; page 215, exercise 6
;; link reversal


;; page 215, exercise 8
;; saddleback search:  x occurs in b[0..m-1][0..n-1], and each row and column
;; of b is ordered by <=.  Find the location (aref b i j) of x.
(defun p215-8 (x b m n)
  (declare (type integer x)
	   (type (array integer 2) b)
	   (type integer m n))
  (pre (in x b[0..m-1][0..n-1]))
  (let ((i 0) (p (- m 1)) (q 0) (j (- n 1)))
    (declare (type integer i p q j))
    (do-od (inv (and (<= 0 i p) (< p m) (<= 0 q j) (< j n) (in x b[i..p][q..j])))
	   (bound ???)
	   ((< (aref b i j) x) (setq i (+ i 1)))
	   ((> (aref b p q) x) (setq p (- p 1)))
	   ((< (aref b p q) x) (setq q (+ q 1)))
	   ((> (aref b i j) x) (setq j (- j 1))))
    (post (and (<= 0 i) (< i m) (<= 0 j) (< j n) (= x (aref b i j))))))


;; page 215, exercise 8 (simplified final solution
;; saddleback search:  x occurs in b[0..m-1][0..n-1], and each row and column
;; of b is ordered by <=.  Find the location (aref b i j) of x.
(defun p215-8alt (x b m n)
  (declare (type integer x)
	   (type (array integer 2) b)
	   (type integer m n))
  (pre (in x b[0..m-1][0..n-1]))
  (let ((i 0) (j (- n 1)))
    (declare (type integer i j))
    (do-od (inv (and (<= 0 i p) (< p m) (<= 0 q j) (< j n) (in x b[i..p][q..j])))
	   (bound ???)
	   ((< (aref b i j) x) (setq i (+ i 1)))
	   ((> (aref b i j) x) (setq j (- j 1))))
    (post (and (<= 0 i) (< i m) (<= 0 j) (< j n) (= x (aref b i j))))))


;;; doesn't give formal pre/post conditions; so I should come up with them
;; page 224 (no program number)
;; swap two (maybe unequal-length) sections of an array.
;; Note: the "swapequals" procedure is (16.5.3), page 213.
;; Also dothe recursive version


;; page 224, program (18.1.3) is basically just page 191, exercise 2.



;; page 228, program (18.2.5)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing
;;;

(defun test-all ()
  (test-p173-14.3)
  (test-p176)
  (test-p177-14.8)
  (test-p177-14.9)
  (test-p177-1)
  (test-p177-2)
  (test-p178-1b)
  (test-p180-15.1.1)
  )
