;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-doc.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

(require 'hhp-func)
(require 'hhp-comp)
(require 'hhp-info)

;;; Code:

(defun hhp-browse-document (&optional haskell-org)
  (interactive "P")
  (let ((mod0 (hhp-extract-module))
	(expr0 (hhp-things-at-point))
	pkg-ver-path mod expr info)
    (if (or mod0 (not expr0))
	(setq mod (hhp-read-module-name mod0))
      (setq expr (hhp-read-expression expr0))
      (setq info (hhp-get-info expr0))
      (setq mod (hhp-extact-module-from-info info)))
    (setq pkg-ver-path (hhp-resolve-document-path mod))
    (if (and pkg-ver-path mod)
	(hhp-display-document pkg-ver-path mod haskell-org expr)
      (message "No document found"))))

(hhp-defstruct pkg-ver-path pkg ver path)

(defun hhp-resolve-document-path (mod)
  (with-temp-buffer
    (hhp-call-process hhpc-command nil t nil "doc" mod)
    (goto-char (point-min))
    (when (looking-at "^\\([^ ]+\\)-\\([0-9]*\\(\\.[0-9]+\\)*\\) \\(.*\\)$")
      (hhp-make-pkg-ver-path
       :pkg (match-string-no-properties 1)
       :ver (match-string-no-properties 2)
       :path (match-string-no-properties 4)))))

(defconst hhp-doc-local-format "file://%s/%s.html")
(defconst hhp-doc-hackage-format
  "http://hackage.haskell.org/packages/archive/%s/%s/doc/html/%s.html")

(defun hhp-display-document (pkg-ver-path mod haskell-org &optional symbol)
  (let* ((mod- (hhp-replace-character mod ?. ?-))
	 (pkg  (hhp-pkg-ver-path-get-pkg pkg-ver-path))
	 (ver  (hhp-pkg-ver-path-get-ver pkg-ver-path))
	 (path (hhp-pkg-ver-path-get-path pkg-ver-path))
	 (pkg-with-ver (format "%s-%s" pkg ver))
	 (local (format hhp-doc-local-format path mod-))
	 (remote (format hhp-doc-hackage-format pkg ver mod-))
	 (file (format "%s/%s.html" path mod-))
	 (url0 (if (or haskell-org (not (file-exists-p file))) remote local))
	 (url (if symbol (hhp-add-anchor url0 symbol) url0)))
    ;; Mac's "open" removes the anchor from "file://", sigh.
    (browse-url url)))

(defun hhp-add-anchor (url symbol)
  (let ((case-fold-search nil))
    (if (string-match "^[A-Z]" symbol)
	(concat url "#t:" symbol)
      (if (string-match "^[a-z]" symbol)
	  (concat url "#v:" symbol)
	(concat url "#v:" (hhp-url-encode symbol))))))

(defun hhp-url-encode (symbol)
  (let ((len (length symbol))
	(i 0)
	acc)
    (while (< i len)
      (hhp-add acc (format "-%d-" (aref symbol i)))
      (setq i (1+ i)))
    (apply 'concat (nreverse acc))))

(defun hhp-extact-module-from-info (info)
  (when (string-match "\`\\([^']+\\)'" info)
    (match-string 1 info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hhp-input-map nil)

(unless hhp-input-map
  (setq hhp-input-map
	(if (boundp 'minibuffer-local-map)
	    (copy-keymap minibuffer-local-map)
	  (make-sparse-keymap)))
  (define-key hhp-input-map "\t" 'hhp-complete))

(defun hhp-read-module-name (def)
  (read-from-minibuffer "Module name: " def hhp-input-map))

(defun hhp-read-expression (def)
  (read-from-minibuffer "Expression: " def hhp-input-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-extract-module ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\(import\\|module\\) +\\(qualified +\\)?\\([^ (\n]+\\)")
	(match-string-no-properties 3))))

(provide 'hhp-doc)
