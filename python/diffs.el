;; Process output of diff_files, or more specifically all_fns_size_diff.

(require 'util-mde)			; for count-matches-regexp

(defun remove-diffs-gubbish ()
  (interactive)

  (let ((fn-begin-regexp
	 (concat "===========================================================================\n"
		 ".*:::\\(BEGIN\\|END\\).*\n"
		 "\\(RATIONALIZING INCOMPATIBLE VAR_INFOS\n\\)?"
		 "[0-9]+ var_infos:\n")))
    (goto-char (point-min))
    (re-search-forward fn-begin-regexp)
    (delete-region (point-min) (point))
    (while (re-search-forward fn-begin-regexp nil t)
      (replace-match "")))
  (goto-char (point-min))
  (replace-string ">>> >>> ... " "" nil)

  (goto-char (point-min))
  (if (re-search-forward
       (concat "SUMMARY\n"
	       "Identical unary invariants: \\([0-9]+\\)\n"
	       "Differing unary invariants: \\([0-9]+\\)\n"
	       "Identical binary invariants: \\([0-9]+\\)\n"
	       "Differing binary invariants: \\([0-9]+\\)")
       nil t)
      (setq id-unary (string-to-int (match-string 1))
	    diff-unary-initial (string-to-int (match-string 2))
	    id-binary (string-to-int (match-string 3))
	    diff-binary-intial (string-to-int (match-string 4)))
    (setq id-unary nil
	  diff-unary nil
	  id-binary nil
	  diff-binary nil))
  ;; (list id-unary diff-unary id-binary diff-binary)
  ;; (+ diff-unary diff-binary)

  (replace-match "")
  (goto-char (point-min))
  (delete-matching-lines "\\(Identical\\|Differing\\) \\(u\\|bi\\)nary invariants:")


  (if (search-forward "UNARY DIFFS" nil t)
      (delete-region (match-beginning 0) (point-max)))

  (goto-char (point-min))
  (while (re-search-forward "\n\\([ \t]\\)" nil t)
    (replace-match "//\\1"))

  ;; When I count lines at this point, they do equal (+ diff-unary diff-binary).

  ;; Eliminate comparisons between pointers and numbers.
  (goto-char (point-min))
  (delete-matching-lines "[0-9][0-9][0-9][0-9][0-9][0-9]")

  ;; (delete-matching-lines "Different numeric difference (subtraction) (\\(Different\\|Missing\\) \\(maximum\\|minimum\\))//")
  (delete-matching-lines "Different \\(sum\\|numeric difference (subtraction)\\) ([^()]*)//")


  (setq uninteresting-re "\\(\\(Different\\|Missing\\) \\(maximum\\|minimum\\)\\|Different \\(non\\)?modulus\\|Different small number of values\\)")

  (setq diff-unary-uninteresting (count-matches-regexp (concat "unary: " uninteresting-re "\\(, " uninteresting-re "\\)*//")))
  (setq diff-unary-missing (count-matches-regexp "unary: \\(one is unconstrained\\)"))
  (setq diff-unary-interesting (count-matches-regexp "unary: \\(different invariants over\\|Equality difference for\\)"))
  (setq diff-unary-final (- (count-matches-regexp "unary:") diff-unary-missing))
  (if (not (= diff-unary-final (+ diff-unary-uninteresting diff-unary-interesting)))
      (error "Missed a unary invariant: %d %d %d" diff-unary-final diff-unary-uninteresting diff-unary-interesting))


  (setq diff-binary-uninteresting (count-matches-regexp (concat "binary: " uninteresting-re "\\(, " uninteresting-re "\\)*//")))
  (setq diff-binary-missing (count-matches-regexp "binary: \\(\\(First\\|Second\\) group contains invariant\\|one is unconstrained\\)"))
  ;; plus all the others
  (setq diff-binary-final (- (count-matches-regexp "binary:") diff-binary-missing))
  ;; (setq diff-binary-interesting (count-matches-regexp "binary: \\(Equality difference for\\)"))
  (setq diff-binary-interesting (- diff-binary-final (+ diff-binary-uninteresting)))

  (goto-char (point-min))
  (insert (format "Identical unary invariants:     %d\n" id-unary))
  (insert (format "Missing unary invariants:       %d\n" diff-unary-missing))
  (insert (format "Differing unary invariants:     %d\n" diff-unary-final))
  (insert (format "Differing unary interesting:     %d\n" diff-unary-interesting))
  (insert (format "Differing unary uninteresting:   %d\n" diff-unary-uninteresting))

  (insert (format "Identical binary invariants:    %d\n" id-binary))
  (insert (format "Missing binary invariants:      %d\n" diff-binary-missing))
  (insert (format "Differing binary invariants:    %d\n" diff-binary-final))
  (insert (format "Differing binary interesting:    %d\n" diff-binary-interesting))
  (insert (format "Differing binary uninteresting:  %d\n" diff-binary-uninteresting))



)

;; call like this:  (diff-summary "/projects/se/people/mernst/diffs")
(defun diff-summary (dir)
  (let ((files (directory-files dir t "\.diff$"))
	(result nil))
    (while files
      (let ((file (car files)))
	(find-file file)
	(revert-buffer t t)
	(remove-diffs-gubbish)
	(setq result (cons (cons file (buffer-substring (point-min) (point))) result))
	(setq files (cdr files))))
    (pop-to-buffer (get-buffer-create "*inv-diff-summary*"))
    (erase-buffer)
    (setq result (reverse result))
    (while result
      (insert (car (car result)) "\n"
	      (cdr (car result)) "\n\n")
      (setq result (cdr result)))))







;; Some sample non-three-line formatted invariant diffs:

;; One is unconstrained, the other is not
;;   pat[0..argc] 	(760 values)
;; 	All sequence elements: *every*element* in [0..126] 	(120 values)
;;   pat[0..argc] unconstrained 	(811 values)
;; One is unconstrained, the other is not
;;   pat[0..argc-1] 	(482 values)
;; 	All sequence elements: *every*element* in [0..126] 	(119 values)
;;   pat[0..argc-1] unconstrained 	(510 values)
;; One is unconstrained, the other is not
;;   sub[0..argc] 	(1100 values)
;; 	All sequence elements: *every*element* <= 126 	(100 values)
;;   sub[0..argc] unconstrained 	(1206 values)
;; One is unconstrained, the other is not
;;   sub[0..argc-1] 	(1083 values)
;; 	All sequence elements: *every*element* in [-1..126] 	(99 values)
;;   sub[0..argc-1] unconstrained 	(1188 values)
;; 
;; First group contains invariant: argc <= sub[0..argc] + 3 	justified 	(100 values)
;; First group contains invariant: pat[0..*argv-1] <= argc + 123 	justified 	(100 values)
;; First group contains invariant: argc <= max(pat[0..argc]) - 33 	justified 	(292 values)
;; First group contains invariant: argc <= sum(pat[0..argc-1]) + 3 	justified 	(73 values)
;; First group contains invariant: -123 <= argc - min(pat[0..argc-1]) <= -30 	justified 	(64 values)
;; First group contains invariant: argc <= max(pat[0..argc-1]) - 33 	justified 	(220 values)
;; First group contains invariant: -97 <= argc - sum(sub[0..argc]) <= 3 	justified 	(77 values)
;; First group contains invariant: -123 <= argc - min(sub[0..argc]) <= -30 	justified 	(56 values)
;; First group contains invariant: argc <= max(sub[0..argc]) + 7 	justified 	(388 values)
;; First group contains invariant: argc != sum(sub[0..argc-1]) 	(72 values)
;; First group contains invariant: -123 <= argc - min(sub[0..argc-1]) <= 4 	justified 	(100 values)
;; First group contains invariant: argc <= max(sub[0..argc-1]) + 6 	justified 	(318 values)
;; 
;; Second group contains invariant: -1 <= sum(sub[0..result]) - min(sub[0..result]) <= 126 	justified 	(1009 values)
;; Second group contains invariant: max(sub[0..result]) <= sum(sub[0..result]) + 1 	justified 	(1009 values)
;; Second group contains invariant: -126 <= min(sub[0..result]) - max(sub[0..result]) <= 0 	justified 	(1009 values)
;; 
;; Different invariants over all elements = (Missing minimum)
;;   sub_100_orig[0..j-1] 	(91 values)
;; 	All sequence elements: *every*element* in [-107..127] 	(26 values)
;;   sub_100_orig[0..j-1] 	(94 values)
;; 	All sequence elements: *every*element* <= 127 	(35 values)
;; 
;; Different invariants over all elements = (Missing minimum)
;;   pat_100_orig[0..junk-1] 	(8 values)
;; 	Per sequence elements equal
;; 	All sequence elements: *every*element* in [0..99] 	(7 values)
;;   pat_100_orig[0..junk-1] 	(9 values)
;; 	Per sequence elements equal
;; 	All sequence elements: *every*element* <= 99 	(8 values)
