;;; load-all.lisp

;; Convenience file that loads all Lisp files

(defun load-all ()
  (load "gries-helper")	; definitions for the Gries-style syntax
  (load "instrument")	; instrumenter
  (load "data-trace")	; code for writing to logs
)

(defun compile-all ()
  (compile-file "gries-helper.lisp")
  (compile-file "instrument.lisp")
  (compile-file "data-trace.lisp")
  (values))

(load-all)
