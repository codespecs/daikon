;; This file removes trailing whitespace from source code; the trailing
;; whitespace serves no purpose.

;; TODO: See Emacs function delete-trailing-whitespace instead.
;; Also see library whitespace.el, which is part of the Emacs distribution.


(defun add-whitespace-hooks ()
  (add-hook 'write-contents-hooks 'maybe-remove-trailing-whitespace))

(add-hook 'c-mode-hook 'add-whitespace-hooks)
(add-hook 'java-mode-hook 'add-whitespace-hooks)
(add-hook 'makefile-mode-hook 'add-whitespace-hooks)

;; Customize if desired.
(defvar remove-trailing-whitespace-ignore-regexps
  (list
   ;; Emacs source files
   "mernst/emacs/x?lisp/"
   "emacs[-/][0-9]+\.[0-9]+\\(\.[0-9]+\\)?/\\(lisp\\|src\\)/"
   "viewmail/lisp/"
   ;; Javac compiler
   "annotations/\\(vendor-\\)?compiler/"
   ;; ASM bytecode manipulation library
   "annotations/asmx?/"
   "org/objectweb/asm/"
   ;; FreePastry
   "pastry/src/"
   ;; Valgrind (part of Kvasir)
   "valgrind-3/"
   )
  "List of regular expressions.  If any of them match a file name, then
trailing whitespace is not removed from the file.
These are typically source files whose style I shouldn't modify, because
they are maintained by someone else, and I wish to minimize differences/patches."
)

(defun remove-trailing-whitespace-ignored-filename (filename)
  "Return t if FILENAME should not have trailing whitespace removed."
  (let ((match nil)
	(regexps remove-trailing-whitespace-ignore-regexps))
    (while regexps
      (let ((regexp (car regexps)))
	(setq regexps (cdr regexps))
	(if (string-match regexp filename)
	    (setq match t
		  regexps nil))))
    match))

(defun maybe-remove-trailing-whitespace ()
  "Remove trailing whitespace and newlines from all lines in the file,
unless the file is maintained by someone else."
  (if (not (and (buffer-file-name)
		(remove-trailing-whitespace-ignored-filename (buffer-file-name))))
      (progn
	(remove-trailing-whitespace)
	(remove-trailing-newlines))))

(defun remove-trailing-whitespace ()
  "Remove trailing whitespace from all lines in the file.  Does not modify point."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; Don't use this; in some modes (eg C mode), it removes blank lines.
    ;; Also, don't use replace-regexp, as it messages "Done."
    ;; (replace-regexp "\\s +$" "" nil)
    (while (re-search-forward "[\t ]+$" nil t)
      ;; Don't remove whitespace immediately following a comment starter.
      (let ((match-begin (match-beginning 0)))
	(if (or (= match-begin (point-min))
		(not (= ?< (char-syntax (char-after (1- match-begin))))))
	    (replace-match "")))))
  ;; Return nil so this can be used as a write-{file,contents}-hook
  nil)

(defun remove-trailing-newlines ()
  "Remove trailing blank lines from the file.  Does not modify point."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "\\([^\n]\n\\)\n+\\'" nil t)
	(replace-match "\\1")))
  ;; Return nil so this can be used as a write-{file,contents}-hook
  nil)
