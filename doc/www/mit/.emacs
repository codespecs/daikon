;; Adjust this directory as necessary
(setq load-path (cons (expand-file-name "~/research/invariants/emacs/")
		      loaad-path))

(load "c-set-basic-offset")
(autoload 'cvs-update "pcl-cvs" nil t)
