(if (not (getenv "INV"))
    (error "Environment variable $INV is not defined."))
(setq load-path (cons (substitute-in-file-name "${INV}/emacs/")
                      load-path))

;;; Update timestamps when writing files.
(add-hook 'write-file-hooks 'time-stamp)

;;; Ediff customizations
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; no multiframe
(setq-default ediff-ignore-similar-regions t)   ; ignore whitespace differences
(setq ediff-whitespace " \n\t\f\r\240") ; by default omits \r, \240, etc.

;;; PCL-CVS
(if (not (fboundp 'cvs-update))
    (if (= 20 emacs-major-version)
        (autoload 'cvs-update "pcl-cvs" nil t) ; Emacs 20
      (autoload 'cvs-update "pcvs" nil t))) ; Emacs 21

;;; AUC TeX
;; Key features:
;;  * When editing a LaTeX file, do "C-c C-c" to do the next appropriate
;;    action (LaTeX, BibTeX, View (xdvi), File (dvips)
;;  * After running latex, do "C-c `" to step through the errors.
;; You can get the full manual in the usual way:  C-h i d m AUCTeX RET
(setq load-path (cons (expand-file-name "~mernst/emacs/auctex-11.13")
		      load-path))
(if (not (featurep 'tex-site))
    (progn
      (require 'tex-site)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java and C mode
;;;

(load "c-set-basic-offset")
(load "remove-trailing-whitespace")
(defun unset-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'java-mode-hook 'unset-indent-tabs-mode)
(add-hook 'c-mode-hook 'unset-indent-tabs-mode)
(add-hook 'perl-mode-hook 'unset-indent-tabs-mode)
(add-hook 'cperl-mode-hook 'unset-indent-tabs-mode)

(add-to-list 'auto-mode-alist '("\\.jpp\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.java\\.goal\\'" . java-mode))


;;; Hiding debugging statements in Java code
;; (Be careful when editing near the ellipsis ("...") that indicates hidden
;; text.)

(defvar java-debug-hiding-retain-if
  '(java-mode ("if \(.*\.isLoggable *(.*) \\({\\)" 1) "}" "/[*/]"
	     nil
	     hs-c-like-adjust-block-beginning)
  "Leaves the \"if (debug.isLoggable(Level.FINE)) ...\" line visible.")
(defvar java-debug-hiding-hide-if
  '(java-mode ("\\(\n\\)[ \t]*if \(\\(debug.*\\|.*\.isLoggable *(.*\\)) *{?$" 1) "}" "/[*/]"
		  forward-2-sexps-then-forward-statement
		  hs-dont-adjust-block-beginning)
  "Elides even the \"if (debug.isLoggable(Level.FINE)) ...\" line.")

(defvar hs-all-hidden nil "Current state of hideshow for toggling all.")

(defun hide-debugging-statements ()
  "Toggle hiding debugging statements in Java code.
This uses hideshow, which see."
  (interactive)
  (require 'hideshow)
  (add-to-list 'hs-special-modes-alist java-debug-hiding-hide-if)
  (setq hs-hide-comments-when-hiding-all nil)
  (hs-minor-mode 1)
  (setq hs-all-hidden (not hs-all-hidden))
  (if hs-all-hidden
      (progn
	(hs-hide-all)
	(message "Debugging blocks are hidden."))
    (progn
      (hs-show-all)
      (message "Debugging blocks are shown (non-hidden)."))))

(defun forward-2-sexps-then-forward-statement (arg)
  "Move forward two sexps, then move forward one (or ARG) statements.
The statement may be compound or simple."
  (forward-sexp 2)
  (if (or (not (= arg 1)) (looking-at " *{"))
      (forward-sexp arg)
    (c-end-of-statement)))

(defun hs-dont-adjust-block-beginning (initial)
  "Adjust INITIAL, the buffer position after `hs-block-start-regexp'.
Actually, point is never moved; a new position is returned that is
the end of the C-function header.  This adjustment function is meant
to be assigned to `hs-adjust-block-beginning' for Java-debug mode.
This particular function doesn't adjust the block beginning at all."
  (point))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Daikon tags table and manual
;;;

(if (not (fboundp 'float-time))
    (defun float-time ()
      (let ((time (current-time)))
        (+ (* 65536.0 (car time))
           (cadr time)
           (* .000001 (caddr time))))))
;; Testing: (list (float-time) (float-time-2))

(defun daikon-tags-table ()
  "Use the Daikon TAGS table.
Remake it first if it is more than a week old."
  (interactive)
  (let* ((tags-file (substitute-in-file-name "$inv/java/TAGS"))
         (tags-file-exists (file-exists-p tags-file)))
    (if (or (not tags-file-exists)
            ;; TAGS file is at least one week old
            (let* ((tags-file-modtime-ints (nth 5 (file-attributes tags-file)))
                   (tags-file-modtime (+ (* 65536.0
                                            (car tags-file-modtime-ints))
                                         (cadr tags-file-modtime-ints))))
              (> (float-time) (+ tags-file-modtime (* 60 60 24 7)))))
        (let ((default-directory (substitute-in-file-name "$inv/java/"))
              (verb (if tags-file-exists "Updating" "Making")))
          (message "%s the Daikon tags table..." verb)
          (call-process "make" nil nil nil "tags")
          (message "%s the Daikon tags table...done" verb)))
    (visit-tags-table tags-file)))
(fset 'tags-table-daikon 'daikon-tags-table)

(defun daikon-developer-info ()
  "Browse the Daikon developer manual, using Info."
  (interactive)
  (daikon-info "developer"))

(defun daikon-info (&optional basename)
  "Browse the Daikon manual (or developer manual), using Info."
  (interactive)
  (if (not basename)
      (setq basename "daikon"))
  (let* ((dir (substitute-in-file-name "$inv/doc/"))
	 (infofile (concat dir basename ".info"))
	 (texinfofile (concat dir basename ".texinfo"))
	 (remake (or (not (file-exists-p infofile))
		     (and (file-newer-than-file-p texinfofile infofile)
			  (y-or-n-p (concat basename ".info is out of date; re-make it? "))))))
    (if remake
        (let ((default-directory dir))
          (call-process "make" nil nil nil "info")
          (sit-for 0 500)               ; let the filesystem find the new file
          ;; The above was synchronous and minimal;
          ;; the below is asynchronous and maximal.
          (daikon-remake-manual t)))
    (let* ((info-buffer (get-buffer "*info*"))
           (info-already-visiting
            (and (buffer-live-p info-buffer)
		 Info-current-file
                 (with-current-buffer info-buffer
                   (save-match-data
                     (string-match (concat "/" basename ".info$")
				   Info-current-file))))))
      (if info-already-visiting
          (if (not remake)
              (pop-to-buffer info-buffer)
            ;; Guarantee that we get the new contents by moving away (to
            ;; a different file) then back to this one.
            ;; "(Info-directory) (Info-last)" seems not to work (leaves me
            ;; at Info-directory), so do the work myself.
            (let (file node)
              (with-current-buffer info-buffer
                (setq file Info-current-file
                      node Info-current-node))
              (Info-directory)
              (Info-find-node file node)))
        (info infofile)))))

(defun daikon-remake-manual (&optional force)
  "Remake the Daikon manual.
Does nothing if a compilation is already running unless `force' is non-nil."
  (interactive)
  (require 'compile)
  (if (or force (not (compilation-is-running)))
      (let ((default-directory (substitute-in-file-name "$inv/doc/")))
        (save-some-buffers (not compilation-ask-about-save) nil)
        (compile-internal "make -k " "No more errors" "make-daikon-info"))))

(defun compilation-is-running ()
  (let ((compilation-is-running nil))
    (let ((name-of-mode "Compilation") outbuf name-function)
      ;; from `compile-internal' (with minmal indentation and other changes)
      (setq outbuf
            (get-buffer-create
             (funcall (or name-function compilation-buffer-name-function
                          (function (lambda (mode)
                                      (concat "*" (downcase mode) "*"))))
                      name-of-mode)))
      (set-buffer outbuf)
      (let ((comp-proc (get-buffer-process (current-buffer))))
        (if comp-proc
            (if (or (not (eq (process-status comp-proc) 'run))
                    (setq compilation-is-running t))
                nil))))
    compilation-is-running))
