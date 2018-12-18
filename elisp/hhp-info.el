;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-info.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 15, 2010

;;; Code:

(require 'hhp-func)
(require 'hhp-process)

(defun hhp-show-info (&optional ask)
  (interactive "P")
  (let* ((expr0 (hhp-things-at-point))
	 (expr (if (or ask (not expr0)) (hhp-read-expression expr0) expr0))
	 (info (hhp-get-info expr)))
    (when info
      (hhp-display
       nil
       (lambda () (insert info))))))

(defun hhp-get-info (expr)
  (let* ((file (buffer-file-name))
	 (cmd (format "info %s %s\n" file expr)))
    (hhp-sync-process cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; type
;;;

(defvar hhp-type-overlay nil)

(make-variable-buffer-local 'hhp-type-overlay)

(defun hhp-type-set-ix (n)
  (overlay-put hhp-type-overlay 'ix n))

(defun hhp-type-get-ix ()
  (overlay-get hhp-type-overlay 'ix))

(defun hhp-type-set-point (pos)
  (overlay-put hhp-type-overlay 'pos pos))

(defun hhp-type-get-point ()
  (overlay-get hhp-type-overlay 'pos))

(defun hhp-type-set-types (types)
  (overlay-put hhp-type-overlay 'types types))

(defun hhp-type-get-types ()
  (overlay-get hhp-type-overlay 'types))

(hhp-defstruct tinfo beg-line beg-column end-line end-column info)

(defun hhp-type-init ()
  (setq hhp-type-overlay (make-overlay 0 0))
  (overlay-put hhp-type-overlay 'face 'region)
  (hhp-type-clear-overlay)
  (setq after-change-functions
	(cons 'hhp-type-clear-overlay after-change-functions))
  (add-hook 'post-command-hook 'hhp-type-post-command-hook))

(defun hhp-type-clear-overlay (&optional beg end len)
  (when (overlayp hhp-type-overlay)
    (hhp-type-set-ix 0)
    (hhp-type-set-point 0)
    (move-overlay hhp-type-overlay 0 0)))

(defun hhp-type-post-command-hook ()
  (when (and (eq major-mode 'haskell-mode)
	     (overlayp hhp-type-overlay)
	     (/= (hhp-type-get-point) (point)))
    (hhp-type-clear-overlay)))

(defun hhp-show-type ()
  (interactive)
  (let ((buf (current-buffer))
	(tinfos (hhp-type-get-tinfos)))
    (if (null tinfos)
	(progn
	  (hhp-type-clear-overlay)
	  (message "Cannot guess type"))
      (let* ((tinfo (nth (hhp-type-get-ix) tinfos))
	     (type (hhp-tinfo-get-info tinfo))
	     (beg-line (hhp-tinfo-get-beg-line tinfo))
	     (beg-column (hhp-tinfo-get-beg-column tinfo))
	     (end-line (hhp-tinfo-get-end-line tinfo))
	     (end-column (hhp-tinfo-get-end-column tinfo))
	     (left (hhp-get-pos buf beg-line beg-column))
	     (right (hhp-get-pos buf end-line end-column)))
	(move-overlay hhp-type-overlay (- left 1) (- right 1) buf)
	(message type)))))

(defun hhp-type-get-tinfos ()
  (if (= (hhp-type-get-point) (point))
      (hhp-type-set-ix
       (mod (1+ (hhp-type-get-ix)) (length (hhp-type-get-types))))
    (let ((types (hhp-type-obtain-tinfos)))
      (if (not (listp types)) ;; main does not exist in Main
	  (hhp-type-set-types nil)
	(hhp-type-set-types types)
	(hhp-type-set-point (point))
	(hhp-type-set-ix 0))))
  (hhp-type-get-types))

(defun hhp-type-obtain-tinfos ()
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (1+ (current-column))))
	 (file (buffer-file-name))
	 (cmd (format "type %s %s %s\n" file ln cn)))
    (hhp-sync-process cmd nil 'hhp-type-fix-string)))

(defun hhp-type-fix-string ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "[Char]" nil t)
      (replace-match "String"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expanding Template Haskell
;;;

(defun hhp-expand-th ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (cmds (list "expand" file "-b" "\n"))
	 (source (hhp-run-hhp cmds)))
    (when source
      (hhp-display
       'fontify
       (lambda () (insert source))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun hhp-get-pos (buf line col)
  (save-excursion
    (set-buffer buf)
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char col)
    (point)))

(defun hhp-read-expression (default)
  (if default
      (let ((prompt (format "Expression (%s): " default)))
	(read-string prompt default nil))
    (read-string "Expression: ")))

(defun hhp-find-module-name ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^module[ ]+\\([^ \n]+\\)" nil t)
	(match-string-no-properties 1))))

(provide 'hhp-info)
