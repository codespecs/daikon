(setq load-path (cons (substitute-in-file-name "${INV}/emacs/")
		      load-path))

(load "c-set-basic-offset")
(load "remove-trailing-whitespace")

(autoload 'cvs-update "pcl-cvs" nil t)

