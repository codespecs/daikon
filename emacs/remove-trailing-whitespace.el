;; This file removes trailing whitespace from source code; the trailing
;; whitespace serves no purpose.

(add-hook 'c-mode-hook 'maybe-remove-trailing-whitespace)
(add-hook 'java-mode-hook 'maybe-remove-trailing-whitespace)

;; Customize the tests if desired.
(defun maybe-remove-trailing-whitespace ()
  "Remove trailing whitespace from all lines in the file,
unless the file is maintained by someone else."
  (if (not (and (buffer-file-name)
		(emacs-source-file-p (buffer-file-name))))
      (remove-trailing-whitespace)))

(defun emacs-source-file-p (filename)
  "Return t if FILENAME is an Emacs source file."
  (or (string-match "mernst/emacs/x?lisp/" filename)
      (string-match "emacs[-/][0-9]+\.[0-9]+\\(\.[0-9]+\\)?/\\(lisp\\|src\\)/" filename)
      ;; (string-match "local/src/emacs-19" (buffer-file-name))
      ;; (string-match "lib/emacs/local-lisp/w3" (buffer-file-name)))
      ))
