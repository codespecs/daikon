;;;
;;; Emacs minor mode for the Daikon Tree GUI for Java.
;;;
;;; Requires JDE (http://jde.sunsite.dk) and of course
;;; Daikon (http://pag.lcs.mit.edu/daikon).
;;;
;;; Contact gjay@ucsd.edu for bug reports, questions, etc.
;;;

;;;; ************************************************************************
;;;; Copyright © 2001 The Regents of the University of California.
;;;; All rights reserved.
;;;; This code is part of the UCSD Daikon Context GUI and has been authored
;;;; by Gregory Jay and William Griswold.
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following
;;;; conditions are met:
;;;;
;;;; 1. Redistributions of source code must retain the above copyright
;;;;	notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;	notice, this list of conditions and the following disclaimer in the
;;;;	documentation and/or other materials provided with the distribution.
;;;; 3. Neither the name of the University nor the names of its contributors
;;;;	may be used to endorse or promote products derived from this software
;;;;	without specific prior written permission.
;;;;
;;;; For permission to use this software for commercial purposes, contact
;;;; William G. Griswold (wgg@cs.ucsd.edu) or send U.S. Mail to:
;;;; William G. Griswold
;;;; Department of Computer Science and Engineering, 0114
;;;; University of California, San Diego
;;;; La Jolla, CA 92093-0114
;;;; ************************************************************************

(require 'jde)

(defgroup daikon-context-gui nil
  "Mode to display invariants for the current method in a separate window."
  :group 'tools)

(defcustom daikon-context-gui-project-root-directory nil
  "*Root of the project source tree being compiled for Daikon.
If nil, taken to be directory in which Daikon Context GUI is launched."
  :type 'string
  :group 'daikon-context-gui)

(defvar daikon-context-gui-started nil
  "Determines if the Daikon Context GUI has been started.")

(defun daikon-context-gui-start-gui ()
  (interactive)
  "Start up the Daikon Context GUI."
  ;(if (not (eq major-mode 'jde-mode))
  ;    (jde-mode))
  (when (not daikon-context-gui-started)
    (setq startup-path (expand-file-name (or daikon-context-gui-project-root-directory
					     default-directory)))
    (case major-mode
     ((jde-mode) ; used to say java-mode here, too -- possible?
      (bsh-eval (concat "daikon.gui.treeGUI.InvariantsGUI.start(\"" startup-path
		      "\");")))
     ((c-mode)
      (bsh-eval (concat "daikon.gui.treeGUI.InvariantsGUI.start(\"" startup-path
		      "\");"))))
    (setq daikon-context-gui-started t)))

(defun daikon-context-gui-end-gui ()
  "Terminate the Daikon Context GUI."
  (when daikon-context-gui-started
    (setq daikon-context-gui-started nil)
    (bsh-eval "daikon.gui.treeGUI.InvariantsGUI.stop();")
  )
)

(defun daikon-context-gui-end ()
  (interactive)
  (daikon-context-gui -1)
)



;;; Borrowed in part from jde-which-method-update.
(defun daikon-context-gui-update ()
  "Update the Daikon Context GUI with the current class/method, or just class if no method."
  (case major-mode
   ((jde-mode) ; used to say java-mode here, too -- possible?
    (let ((class-method-args (daikon-context-gui-get-method-at-point)))
      ;;(message "daikon-context-gui-update class-method-args = %s" class-method-args)
      (if class-method-args
	  (daikon-context-gui-update-with-method class-method-args)
	(daikon-context-gui-update-with-class
	  (car (jde-parse-get-innermost-class-at-point))))))
   ((c-mode)
    (daikon-context-gui-update-with-method
      (list "std" (cadr (jde-parse-get-method-at-point)) nil)))  ; 'class' always "std"
   (otherwise ; whenever you're in another buffer
    nil)))





;; Sends the class information to the Context GUI for display
(defun daikon-context-gui-update-with-class (class)
  "Update the Daikon Context GUI using only class information."
  (when daikon-context-gui-started

    (bsh-eval
     (message (concat "daikon.gui.treeGUI.InvariantsGUI.setSelection(\""
		      (jde-parse-get-package-name) "."
		      class "\");")))))

;; Sends the method information to the Context GUI for display.
(defun daikon-context-gui-update-with-method (class-method-args)
  (when daikon-context-gui-started
    (let ((class   (car class-method-args))
	  (method (cadr class-method-args))
	  (args  (caddr class-method-args)))
      (bsh-eval (message
		 (concat "daikon.gui.treeGUI.InvariantsGUI.setSelection(\""
			 (jde-parse-get-package-name) "."
			 class
			 "\", \""  method "\", \"" args "\");"))))))

;; This code was taken from JDK's get method at point function from
;; version jde-2.2.7beta11 jde-which-method.el but edited such that i can
;; get the arguments of the functions to send to the GUI. This is needed for
;; method overloading.
(defun daikon-context-gui-get-method-at-point (&optional position)
  "Gets the method at POSITION, if specified, otherwise at point.
Returns a list of three strings, (CLASS_NAME METHOD_NAME ARGS), if the
specified position is in a method; otherwise, returns nil."
  ;; Define an internal function that recursively searches a class
  ;; and its subclasses for a method containing point.
  (flet ((search-class
	  (class pos)
	  (let* ((class-name       (semantic-token-name class))
		 (class-parts      (semantic-token-type-parts class))
		 (class-subclasses (semantic-find-nonterminal-by-token 'type class-parts))
		 (class-methods    (semantic-find-nonterminal-by-token 'function class-parts)))

	      ;; Is point in a method of a subclass of this class?
	      (loop for subclass in class-subclasses do
		(search-class subclass pos))

	      ;; Is point in any of the methods of this class?
	      (loop for method in class-methods do
		    (setq arglist (car (cdr (cdr (cdr method)))))
		    (setq args "")
		    (setq arg_string "")
		    (while arglist
			(setq arg_type (caddr (car arglist)))
			(when arg_type
			  (setq arg_string (format "%s" arg_type))
			  (setq args (concat args arg_string " ")))
			(setq arglist (cdr arglist)))
		    (setq class-string (format "%s" class-name))
		    (setq method-string (format "%s" (car method)))
		    (let* ((method-name  (semantic-token-name method))
			 (method-start (semantic-token-start method))
			 (method-end   (semantic-token-end method)))
		      (when (and (>= pos method-start)
				 (<= pos method-end))
			(throw 'found
			       (list class-string method-string args))))))))

    (let* ((pos (or position (point)))
	   (tokens (semantic-bovinate-toplevel))
	   (classes (semantic-find-nonterminal-by-token 'type tokens)))
      (catch 'found
	(loop for class in classes
	  do (search-class class pos))))))

;; Timer to update the Context GUI.
(defvar daikon-context-gui-idle-timer nil
  "Runs the necessary Daikon Context GUI functions at idle points.")

;;;
;;; Minor Mode Definition.
;;;
(defvar daikon-context-gui-map nil
  "Keymap for Daikon Context GUI minor mode.")

;; Sets key to stop the minor mode.
(when (not daikon-context-gui-map)
  (setq daikon-context-gui-map (make-sparse-keymap))
  (define-key daikon-context-gui-map "\C-cd" 'daikon-context-gui))

(defvar daikon-context-gui nil
  "Mode variable for Daikon Context GUI minor mode.
Non-nil if minor mode is active, nil if minor mode is not active.")
(make-variable-buffer-local 'daikon-context-gui)

(defun daikon-context-gui (&optional arg)
  "Enable or disable the Daikon Context GUI minor mode.
This minor mode shows Daikon invariants for Java in a GUI.
\\{daikon-context-gui-map}"
  (interactive)
  (setq daikon-context-gui
	(if (null arg)
	    (not daikon-context-gui)
	  (> (prefix-numeric-value arg) 0)))
  (if daikon-context-gui
      ;; GUI is being enabled
      (progn
       (add-hook 'jde-mode-hook 'daikon-context-gui)
       (daikon-context-gui-start-gui)
       (add-hook 'jde-entering-java-buffer-hook 'daikon-context-gui-update)
       (unless daikon-context-gui-idle-timer
	 (setq daikon-context-gui-idle-timer
	       (run-with-idle-timer .25 t 'daikon-context-gui-update))))
    ;; GUI is being disabled
    (progn
      (remove-hook 'jde-entering-java-buffer-hook 'daikon-context-gui-update)
      (cancel-timer daikon-context-gui-idle-timer)
      (setq daikon-context-gui-idle-timer nil)
      (daikon-context-gui-end-gui))))

;;; Updates minor mode list and modeline
(when (not (assq 'daikon-context-gui minor-mode-alist))
  (setq minor-mode-alist
	(cons '(daikon-context-gui " Daikon") minor-mode-alist))
  (setq minor-mode-map-alist
	(cons (cons 'daikon-context-gui daikon-context-gui-map) minor-mode-map-alist)))

;; Arguments to the c-*-of-* functions seem to be required by Emacs 20.
(defun c-name-of-enclosing-function ()
  "Return the name of the function containing point, or nil
if point is not in a function."
  (save-excursion
    (beginning-of-line)
    (let ((orig-point (point)))
      (c-end-of-defun 1)
      (c-beginning-of-defun 1)
      (if (= (point) (point-min))
          nil
        (let ((bod (point)))            ; beginning of defun
          (c-beginning-of-statement 1)
          (if (< orig-point (point))
              nil
            (if (re-search-forward "\\b\\([_A-Za-z0-9]+\\)\\s-*(" bod t)
                (match-string 1)
              (progn
                (message "c-name-of-enclosing-function got confused")
                nil))))))))

;; Here is some test code.
(defun message-c-name-of-enclosing-function ()
  (if (eq major-mode 'c-mode)
      (message "%s" (c-name-of-enclosing-function))))
;; (add-hook 'post-command-hook 'message-c-name-of-enclosing-function)
;; ;; To undo:
;; (remove-hook 'post-command-hook 'message-c-name-of-enclosing-function)

(provide 'daikon-context-gui)
