(setq load-path (cons (substitute-in-file-name "${INV}/emacs/")
		      load-path))

(load "c-set-basic-offset")
(load "remove-trailing-whitespace")
(defun unset-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'java-mode-hook 'unset-indent-tabs-mode)
(add-hook 'c-mode-hook 'unset-indent-tabs-mode)

(add-to-list 'auto-mode-alist '("\\.jpp\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.java\\.goal\\'" . java-mode))

(if (not (fboundp 'cvs-update))
    (if (= 20 emacs-major-version)
	(autoload 'cvs-update "pcl-cvs" nil t) ; Emacs 20
      (autoload 'cvs-update "pcvs" nil t))) ; Emacs 21

