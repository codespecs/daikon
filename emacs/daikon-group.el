(setq load-path (cons (substitute-in-file-name "${INV}/emacs/")
		      load-path))

(load "c-set-basic-offset")
(load "remove-trailing-whitespace")

(add-to-list 'auto-mode-alist '("\\.jpp\\'" . java-mode))

(if (not (fboundp 'cvs-update))
    (if (= 20 emacs-major-version)
	(autoload 'cvs-update "pcl-cvs" nil t) ; Emacs 20
      (autoload 'cvs-update "pcvs" nil t))) ; Emacs 21

