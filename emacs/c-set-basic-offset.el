;; This code automatically sets the value of c-basic-offset to be
;; consistent with the file being edited.  You can place it in your .emacs
;; file or load it from your .emacs file.

(add-hook 'c-mode-hook 'c-set-basic-offset)
(add-hook 'java-mode-hook 'c-set-basic-offset)

; from util-mde.el
;; I haven't tested this one, but it is probably faster than Potter's version.
;; From: huttar@hp750.itg.ti.com (Lars Huttar)
(defun looking-back-at (regexp)
  "t when text before point matches regular expression REGEXP."
  (save-excursion
    (let ((old-point (point))
          (found (re-search-backward regexp (point-min) t)))
      (and found (= (match-end 0) old-point)))))

(defvar c-basic-offset-default 2
  "Default value used by `c-set-basic-offset'.
If nil, use the current value of `c-basic-offset' as the default.")
(defun c-set-basic-offset ()
  "If possible set `c-basic-offset' to correspond to text in the buffer.
`c-basic-offset' is used by C, C++, Java, and related modes.
To use, put in your .emacs file:  \(add-hook 'c-mode-hook 'c-set-basic-offset\)"
  (make-local-variable 'c-basic-offset)
  (if c-basic-offset-default
      (setq c-basic-offset c-basic-offset-default))
  (save-excursion
    (goto-char (point-min))
    (while (forward-comment 1)
      ;; nothing to do
      )
    ;; This isn't quite right:  I might find indentation in a C++ initializer
    ;;   Foo::Foo(int arg1) : slot1(arg1),
    ;;                      : slot2(NULL)
    ;; so I should insist that the indentation be found in the body starting with the "{".
    ;; Insist on space around brace to avoid finding @link{...} in a comment.
    (if (re-search-forward "\\([ \t\n\r]{[ \t\n\r]\\|{$\\)" nil t)
	(progn
	  (while (forward-comment 1)
	    ;; nothing to do
	    )
	  ;; forward-comment actually brings us all the way to non-whitespace
	  (beginning-of-line)
	  ;; This isn't quite right:  it could match in comments.  Perhaps demand
	  ;; a match for c-Java-defun-prompt-regexp or some other keywords.
	  ;; Forbid a trailing colon to avoid matching labels, which have special
	  ;; indentation.
	  (if (re-search-forward "^\\([ \t]+\\)[^ \t\n\r][^\n\r/*].*[^:\n\r]$" nil t)
	      (progn
		(goto-char (match-end 1))
		(if (looking-back-at "^\t+")
		    (setq c-basic-offset 8))
		;; sanity check
		(if (<= (current-column) 8)
		    (setq c-basic-offset (current-column))))))))
  (message "Set c-basic-offset to %d" c-basic-offset))
