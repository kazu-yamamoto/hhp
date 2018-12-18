;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-ins-mod.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 27, 2011

(require 'hhp-process)

;;; Code:

(defun hhp-insert-module ()
  (interactive)
  (let* ((expr0 (hhp-things-at-point))
	 (expr (hhp-read-expression expr0)))
    (hhp-ins-mod expr)))

(defvar hhp-preferred-modules '("Control.Applicative"
				"Data.ByteString"
				"Data.Text"
				"Text.Parsec"
				"System.FilePath"
				"System.Directory"))

(defun hhp-reorder-modules (mods)
  (catch 'loop
    (dolist (pmod hhp-preferred-modules)
      (if (member pmod mods)
	  (throw 'loop (cons pmod (delete pmod mods)))))
    mods))

(defun hhp-ins-mod (expr)
  (let (prefix fun mods)
    (if (not (string-match "^\\([^.]+\\)\\\.\\([^.]+\\)$" expr))
	(setq fun expr)
      (setq prefix (match-string 1 expr))
      (setq fun (match-string 2 expr)))
    (setq mods (hhp-reorder-modules (hhp-function-to-modules fun)))
    (if (null mods)
	(message "No module guessed")
      (let* ((key (or prefix fun))
	     (fmt (concat "Module name for \"" key "\" (%s): "))
	     (mod (hhp-completing-read fmt mods)))
	(save-excursion
	  (hhp-goto-module-position)
	  (if prefix
	      (insert-before-markers "import qualified " mod " as " prefix "\n")
	    (insert-before-markers "import " mod " (" (hhp-enclose expr) ")\n")))))))

(defun hhp-completing-read (fmt lst)
  (let* ((def (car lst))
	 (prompt (format fmt def))
	 (inp (completing-read prompt lst)))
    (if (string= inp "") def inp)))

(defun hhp-goto-module-position ()
  (goto-char (point-max))
  (if (re-search-backward "^import" nil t)
      (hhp-goto-empty-line)
    (if (not (re-search-backward "^module" nil t))
	(goto-char (point-min))
      (hhp-goto-empty-line)
      (forward-line)
      (unless (eolp)
	;; save-excursion is not proper due to insert-before-markers.
	(let ((beg (point)))
	  (insert-before-markers "\n")
	  (goto-char beg))))))

(defun hhp-goto-empty-line ()
  (unless (re-search-forward "^$" nil t)
    (forward-line)))

(defun hhp-function-to-modules (fun)
  (let ((cmd (format "find %s\n" fun)))
    (hhp-sync-process cmd)))

(provide 'hhp-ins-mod)
