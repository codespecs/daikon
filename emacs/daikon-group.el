(setq load-path (cons (substitute-in-file-name "${INV}/emacs/")
		      load-path))

;;; Java and C mode
(load "c-set-basic-offset")
(load "remove-trailing-whitespace")
(defun unset-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'java-mode-hook 'unset-indent-tabs-mode)
(add-hook 'c-mode-hook 'unset-indent-tabs-mode)

(add-to-list 'auto-mode-alist '("\\.jpp\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.java\\.goal\\'" . java-mode))

;;; Ediff customizations
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; no multiframe
(setq-default ediff-ignore-similar-regions t)	; ignore whitespace differences
(setq ediff-whitespace " \n\t\f\r\240")	; by default omits \r, \240, etc.

;;; PCL-CVS
(if (not (fboundp 'cvs-update))
    (if (= 20 emacs-major-version)
	(autoload 'cvs-update "pcl-cvs" nil t) ; Emacs 20
      (autoload 'cvs-update "pcvs" nil t))) ; Emacs 21

;;; Daikon tags table and manual

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

(defun daikon-info ()
  "Browse the Daikon manual, using Info."
  (interactive)
  (let ((remake (and (file-newer-than-file-p
		      (substitute-in-file-name "$inv/doc/daikon.texinfo")
		      (substitute-in-file-name "$inv/doc/daikon.info"))
		     (y-or-n-p "daikon.info is out of date; re-make it? "))))
    (if remake
	(let ((default-directory (substitute-in-file-name "$inv/doc/")))
	  (call-process "make" nil nil nil "info")
	  (sit-for 0 500)		; let the filesystem find the new file
	  ;; The above was synchronous and minimal;
	  ;; the below is asynchronous and maximal.
	  (daikon-remake-manual t)))
    (let* ((info-buffer (get-buffer "*info*"))
	   (info-visiting-daikon
	    (and (buffer-live-p info-buffer)
		 (with-current-buffer info-buffer
		   (save-match-data
		     (string-match "/daikon.info$" Info-current-file))))))
      (if info-visiting-daikon
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
	(info (substitute-in-file-name "$inv/doc/daikon.info"))))))

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
