;; Adjust this directory as necessary
(setq load-path (cons (substitute-in-file-name "${INV}/emacs/")
		      load-path))

(load "c-set-basic-offset")
(autoload 'cvs-update "pcl-cvs" nil t)
