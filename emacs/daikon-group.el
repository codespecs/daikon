(setq load-path (cons (substitute-in-file-name "${INV}/emacs/")
		      load-path))

(load "c-set-basic-offset")
(load "remove-trailing-whitespace")

(add-to-list 'auto-mode-alist '("\\.jpp\\'" . java-mode))

(autoload 'cvs-update "pcl-cvs" nil t)

