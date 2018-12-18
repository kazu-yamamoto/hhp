;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-check.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  9, 2014

;;; Code:

(require 'hhp-func)
(require 'hhp-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stolen from flymake.el
(defface hhp-face-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "orangered"))
    (t
     :inherit error))
  "Face used for marking error lines."
  :group 'ghc)

(defface hhp-face-warn
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "gold"))
    (t
     :inherit warning))
  "Face used for marking warning lines."
  :group 'ghc)

(defvar hhp-check-error-fringe (propertize "!" 'display '(left-fringe exclamation-mark)))

(defvar hhp-check-warning-fringe (propertize "?" 'display '(left-fringe question-mark)))

(defvar hhp-display-error nil
  "*An action to display errors/warnings for 'M-n' and 'M-p:

nil            does not display errors/warnings.
'minibuffer    displays errors/warnings in the minibuffer.
'other-buffer  displays errors/warnings in the other buffer.
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-check-syntax ()
  (interactive)
  (hhp-with-process (hhp-check-send)
		    'hhp-check-callback
		    (lambda () (setq mode-line-process " -:-"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(hhp-defstruct hilit-info file line msg err)

(defun hhp-check-send ()
  (let ((file (buffer-file-name)))
    (if hhp-check-command
	(let ((opts (hhp-haskell-list-of-string hhp-hlint-options)))
	  (if opts
	      (format "lint %s %s\n" opts file)
	    (format "lint %s\n" file)))
      (format "check %s\n" file))))

(defun hhp-haskell-list-of-string (los)
  (when los
    (concat "["
	    (mapconcat (lambda (x) (concat "\"" x "\"")) los ", ")
	    "]")))

(defun hhp-check-callback (status)
  (cond
   ((eq status 'ok)
    (let* ((errs (hhp-read-lisp-this-buffer))
	   (infos (hhp-to-info errs)))
      (cond
       (infos
	(let ((file hhp-process-original-file)
	      (buf hhp-process-original-buffer))
	  (hhp-check-highlight-original-buffer file buf infos)))
       (t
	(hhp-with-current-buffer hhp-process-original-buffer
	  (remove-overlays (point-min) (point-max) 'hhp-check t))))
      (hhp-with-current-buffer hhp-process-original-buffer
	(let ((len (length infos)))
	  (if (= len 0)
	      (setq mode-line-process "")
	    (let* ((errs (hhp-filter 'hhp-hilit-info-get-err infos))
		   (elen (length errs))
		   (wlen (- len elen)))
	      (setq mode-line-process (format " %d:%d" elen wlen))))))))
   (t
    (hhp-with-current-buffer hhp-process-original-buffer
      (setq mode-line-process " failed")))))

(defun hhp-to-info (errs)
  ;; [^\t] to include \n.
  (let ((regex "^\\([^\n]*\\):\\([0-9]+\\):\\([0-9]+\\): *\\([^\t]+\\)")
	info infos)
    (dolist (err errs (nreverse infos))
      (when (string-match regex err)
	(let* ((file (expand-file-name (match-string 1 err))) ;; for Windows
	       (line (string-to-number (match-string 2 err)))
	       ;; don't take column to make multiple same errors to a single.
	       (msg (match-string 4 err))
	       (wrn (string-match "^Warning" msg))
	       (info (hhp-make-hilit-info
		      :file file
		      :line line
		      :msg  msg
		      :err  (not wrn))))
	  (unless (member info infos)
	    (hhp-add infos info)))))))

(defun hhp-check-highlight-original-buffer (ofile buf infos)
  (hhp-with-current-buffer buf
    (remove-overlays (point-min) (point-max) 'hhp-check t)
    (save-excursion
      (goto-char (point-min))
      (dolist (info infos)
	(let ((line (hhp-hilit-info-get-line info))
	      (msg  (hhp-hilit-info-get-msg  info))
	      (file (hhp-hilit-info-get-file info))
	      (err  (hhp-hilit-info-get-err  info))
	      beg end ovl)
	  ;; FIXME: This is the Shlemiel painter's algorithm.
	  ;; If this is a bottleneck for a large code, let's fix.
	  (goto-char (point-min))
	  (cond
	   ((string= ofile file)
	    (forward-line (1- line))
	    (while (eq (char-after) 32) (forward-char))
	    (setq beg (point))
	    (forward-line)
	    (setq end (1- (point))))
	   (t
	    (setq beg (point))
	    (forward-line)
	    (setq end (point))))
	  (setq ovl (make-overlay beg end))
	  (overlay-put ovl 'hhp-check t)
	  (overlay-put ovl 'hhp-file file)
	  (overlay-put ovl 'hhp-msg msg)
	  (overlay-put ovl 'help-echo msg)
	  (let ((fringe (if err hhp-check-error-fringe hhp-check-warning-fringe))
		(face (if err 'hhp-face-error 'hhp-face-warn)))
	    (overlay-put ovl 'before-string fringe)
	    (overlay-put ovl 'face face)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-overlay-p (ovl)
  (overlay-get ovl 'hhp-check))

(defun hhp-check-overlay-at (p)
  (hhp-filter 'hhp-overlay-p (overlays-at p)))

(hhp-defstruct file-msgs file msgs)

(defun hhp-get-errors-over-warnings ()
  (let ((ovls (hhp-check-overlay-at (point))))
    (when ovls
      (let ((msgs (mapcar (lambda (ovl) (overlay-get ovl 'hhp-msg)) ovls))
	    (file (overlay-get (car ovls) 'hhp-file))
	    errs wrns)
	(dolist (msg msgs)
	  (if (string-match "^Warning" msg)
	      (hhp-add wrns msg)
	    (hhp-add errs msg)))
	(hhp-make-file-msgs :file file :msgs (nconc errs wrns))))))

(defun hhp-display-errors ()
  (interactive)
  (let ((file-msgs (hhp-get-errors-over-warnings)))
    (if (null file-msgs)
	(message "No errors or warnings")
      (let ((file (hhp-file-msgs-get-file file-msgs))
	    (msgs (hhp-file-msgs-get-msgs file-msgs)))
	(hhp-display
	 nil
	 (lambda ()
	   (insert file "\n\n")
	   (mapc (lambda (x) (insert x "\n\n")) msgs)))))))

(defun hhp-display-errors-to-minibuf ()
  (let ((file-msgs (hhp-get-errors-over-warnings)))
    (if (null file-msgs)
	(message "No errors or warnings")
      (let* ((file (hhp-file-msgs-get-file file-msgs))
	     (msgs (hhp-file-msgs-get-msgs file-msgs))
	     (errmsg (mapconcat 'identity msgs "\n"))
	     (buffile buffer-file-name))
        (if (string-equal buffile file)
            (message "%s" errmsg)
          (message "%s\n\n%s" file errmsg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-goto-prev-error ()
  (interactive)
  (let* ((here (point))
         (ovls0 (hhp-check-overlay-at here))
         (end (if ovls0 (overlay-start (car ovls0)) here))
         (ovls1 (overlays-in (point-min) end))
         (ovls2 (hhp-filter (lambda (ovl) (overlay-get ovl 'hhp-check)) ovls1))
         (pnts (mapcar 'overlay-start ovls2)))
    (if pnts (goto-char (apply 'max pnts))))
  (cond
   ((eq hhp-display-error 'minibuffer) (hhp-display-errors-to-minibuf))
   ((eq hhp-display-error 'other-buffer) (hhp-display-errors))))

(defun hhp-goto-next-error ()
  (interactive)
  (let* ((here (point))
         (ovls0 (hhp-check-overlay-at here))
         (beg (if ovls0 (overlay-end (car ovls0)) here))
         (ovls1 (overlays-in beg (point-max)))
         (ovls2 (hhp-filter (lambda (ovl) (overlay-get ovl 'hhp-check)) ovls1))
         (pnts (mapcar 'overlay-start ovls2)))
    (if pnts (goto-char (apply 'min pnts))))
  (cond
   ((eq hhp-display-error 'minibuffer) (hhp-display-errors-to-minibuf))
   ((eq hhp-display-error 'other-buffer) (hhp-display-errors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-check-insert-from-warning ()
  (interactive)
  (dolist (data (mapcar (lambda (ovl) (overlay-get ovl 'hhp-msg)) (hhp-check-overlay-at (point))))
    (save-excursion
      (cond
       ((string-match "Inferred type: \\|no type signature:" data)
	(beginning-of-line)
	(insert-before-markers (hhp-extract-type data) "\n"))
       ((string-match "lacks an accompanying binding" data)
	(beginning-of-line)
	(when (looking-at "^\\([^ ]+\\) *::")
	  (save-match-data
	    (forward-line)
	    (if (not (bolp)) (insert "\n")))
	  (insert (match-string 1) " = undefined\n")))
       ;; GHC 7.8 uses Unicode for single-quotes.
       ((string-match "Not in scope: type constructor or class .\\([^\n]+\\)." data)
	(let ((sym (match-string 1 data)))
	  (hhp-ins-mod sym)))
       ((string-match "Not in scope: data constructor .\\([^\n]+\\)." data)
	;; if the type of data constructor, it would be nice.
	(let ((sym (match-string 1 data)))
	  (hhp-ins-mod sym)))
       ((string-match "\n[ ]+.\\([^ ]+\\). is a data constructor of .\\([^\n]+\\).\n" data)
	(let* ((old (match-string 1 data))
	       (type-const (match-string 2 data))
	       (new (format "%s(%s)" type-const old)))
	  (hhp-check-replace old new)))
       ((string-match "Not in scope: .\\([^\n]+\\)." data)
	(let ((sym (match-string 1 data)))
	  (if (or (string-match "\\." sym) ;; qualified
		  (y-or-n-p (format "Import module for %s?" sym)))
	      (hhp-ins-mod sym)
	    (unless (re-search-forward "^$" nil t)
	      (goto-char (point-max))
	      (insert "\n"))
	    (insert "\n" (hhp-enclose sym) " = undefined\n"))))
       ((string-match "Pattern match(es) are non-exhaustive" data)
	(let* ((fn (hhp-get-function-name))
	       (arity (hhp-get-function-arity fn)))
	  (hhp-insert-underscore fn arity)))
       ((string-match "Found:\n[ ]+\\([^\t]+\\)\nWhy not:\n[ ]+\\([^\t]+\\)" data)
	(let ((old (match-string 1 data))
	      (new (match-string 2 data)))
	  (hhp-check-replace old new)))
       (t
	(message "Nothing was done"))))))

(defun hhp-check-replace (old new)
  (beginning-of-line)
  (when (search-forward old nil t)
    (let ((end (point)))
      (search-backward old nil t)
      (delete-region (point) end))
    (insert new)))

(defun hhp-extract-type (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (re-search-forward "Inferred type: \\|no type signature:\\( \\|\n +\\)?" nil t)
      (delete-region (point-min) (point)))
    (when (re-search-forward " forall [^.]+\\." nil t)
      (replace-match ""))
    (while (re-search-forward "\n +" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "\\[Char\\]" nil t)
      (replace-match "String"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun hhp-get-function-name ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\([^ ]+\\) ")
      (match-string 1))))

(defun hhp-get-function-arity (fn)
  (when fn
    (save-excursion
      (let ((regex (format "^%s *::" (regexp-quote fn))))
	(when (re-search-backward regex nil t)
	  (hhp-get-function-arity0))))))

(defun hhp-get-function-arity0 ()
  (let ((end (save-excursion (end-of-line) (point)))
	(arity 0))
    (while (search-forward "->" end t)
      (setq arity (1+ arity)))
    arity))

(defun hhp-insert-underscore (fn ar)
  (when fn
    (let ((arity (or ar 1)))
      (save-excursion
	(goto-char (point-max))
	(re-search-backward (format "^%s *::" (regexp-quote fn)))
	(forward-line)
	(re-search-forward "^$" nil t)
	(insert fn)
	(dotimes (i arity)
	  (insert " _"))
	(insert  " = error \"" fn "\"\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-jump-file ()
  (interactive)
  (let* ((ovl (car (hhp-check-overlay-at 1)))
	 (file (if ovl (overlay-get ovl 'hhp-file))))
    (if file (find-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hhp-hlint-options nil "*Hlint options")

(defvar hhp-check-command nil)

(defun hhp-toggle-check-command ()
  (interactive)
  (setq hhp-check-command (not hhp-check-command))
  (if hhp-check-command
      (message "Syntax check with hlint")
    (message "Syntax check with GHC")))

(provide 'hhp-check)
