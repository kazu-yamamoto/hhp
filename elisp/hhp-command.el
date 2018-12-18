;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-command.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Apr 13, 2010

;;; Code:

(require 'hhp-process)
(require 'hhp-check)

(defun hhp-insert-template ()
  (interactive)
  (cond
   ((bobp)
    (hhp-insert-module-template))
   ((hhp-check-overlay-at (point))
    (hhp-check-insert-from-warning))
   (t
    (message "Nothing to be done"))))

(defun hhp-insert-module-template ()
  (let* ((fullname (file-name-sans-extension (buffer-file-name)))
	 (rootdir (hhp-get-project-root))
	 (len (length rootdir))
	 (name (substring fullname (1+ len)))
	 (file (file-name-sans-extension (buffer-name)))
	 (case-fold-search nil)
	 (mod (if (string-match "^[A-Z]" name)
		  (hhp-replace-character name ?/ ?.)
		(if (string-match "^[a-z]" file)
		    "Main"
		  file))))
    (while (looking-at "^{-#")
      (forward-line))
    (insert "module " mod " where\n")))

;; (defun hhp-capitalize (str)
;;   (let ((ret (copy-sequence str)))
;;     (aset ret 0 (upcase (aref ret 0)))
;;     ret))

(defun hhp-sort-lines (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line
		   (lambda ()
		     (re-search-forward "^import\\( *qualified\\)? *" nil t)
		     nil)
		   'end-of-line))
      (hhp-merge-lines))))

(defun hhp-merge-lines ()
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (not (eolp))
      ;; qualified modlues are not merged at this moment.
      ;; fixme if it is improper.
      (if (looking-at "^import *\\([A-Z][^ \n]+\\) *(\\(.*\\))$")
	  (let ((mod (match-string-no-properties 1))
		(syms (match-string-no-properties 2))
		(beg (point)))
	    (forward-line)
	    (hhp-merge-line beg mod syms))
	(forward-line)))))

(defun hhp-merge-line (beg mod syms)
  (let ((regex (concat "^import *" (regexp-quote mod) " *(\\(.*\\))$"))
	duplicated)
    (while (looking-at regex)
      (setq duplicated t)
      (setq syms (concat syms ", " (match-string-no-properties 1)))
      (forward-line))
    (when duplicated
      (delete-region beg (point))
      (insert "import " mod " (" syms ")\n"))))

(defun hhp-save-buffer ()
  (interactive)
  ;; fixme: better way then saving?
  (if hhp-check-command ;; hlint
      (if (buffer-modified-p)
	  (call-interactively 'save-buffer))
    (unless buffer-read-only
      (set-buffer-modified-p t)
      (call-interactively 'save-buffer)))
  (hhp-check-syntax))

(provide 'hhp-command)
