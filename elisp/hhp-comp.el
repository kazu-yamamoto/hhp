;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-comp.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

;;; Code:

(require 'hhp-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

(defvar hhp-idle-timer-interval 30
 "*Period of idle timer in second. When timeout, the names of
unloaded modules are loaded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constants
;;;

;; must be sorted
(defconst hhp-reserved-keyword-for-bol '("class" "data" "default" "import" "infix" "infixl" "infixr" "instance" "main" "module" "newtype" "type"))

;; must be sorted
(defconst hhp-reserved-keyword '("case" "deriving" "do" "else" "if" "in" "let" "module" "of" "then" "where"))

(defconst hhp-extra-keywords '("ByteString" "Text"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local Variables
;;;

(defvar hhp-window-configuration nil)

(mapc 'make-variable-buffer-local
      '(hhp-window-configuration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar hhp-module-names nil)   ;; completion for "import"
(defvar hhp-merged-keyword nil) ;; completion for type/func/...
(defvar hhp-language-extensions nil)
(defvar hhp-option-flags nil)
(defvar hhp-pragma-names '("LANGUAGE" "OPTIONS_GHC" "INCLUDE" "WARNING" "DEPRECATED" "INLINE" "NOINLINE" "ANN" "LINE" "RULES" "SPECIALIZE" "UNPACK" "SOURCE"))

(defconst hhp-keyword-prefix "hhp-keyword-")
(defvar hhp-keyword-Prelude nil)
(defvar hhp-keyword-Control.Applicative nil)
(defvar hhp-keyword-Control.Exception nil)
(defvar hhp-keyword-Control.Monad nil)
(defvar hhp-keyword-Data.Char nil)
(defvar hhp-keyword-Data.List nil)
(defvar hhp-keyword-Data.Maybe nil)
(defvar hhp-keyword-System.IO nil)

(defvar hhp-loaded-module nil)

(defun hhp-comp-init ()
  (let* ((syms '(hhp-module-names
		 hhp-language-extensions
		 hhp-option-flags
		 ;; hard coded in main.hs
		 hhp-keyword-Prelude
		 hhp-keyword-Control.Applicative
		 hhp-keyword-Control.Exception
		 hhp-keyword-Control.Monad
		 hhp-keyword-Data.Char
		 hhp-keyword-Data.List
		 hhp-keyword-Data.Maybe
		 hhp-keyword-System.IO))
	 (vals (hhp-boot (length syms))))
    (hhp-set syms vals))
  (hhp-add hhp-module-names "qualified")
  (hhp-add hhp-module-names "hiding")
  ;; hard coded in main.hs
  (hhp-merge-keywords '("Prelude"
			"Control.Applicative"
			"Control.Exception"
			"Control.Monad"
			"Data.Char"
			"Data.List"
			"Data.Maybe"
			"System.IO")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing command
;;;

(defun hhp-boot (n)
  (prog2
      (message "Initializing...")
      (hhp-sync-process "boot\n" n)
    (message "Initializing...done")))

(defun hhp-load-modules (mods)
  (if mods
      (mapcar 'hhp-load-module mods)
    (message "No new modules")
    nil))

(defun hhp-load-module (mod)
  (prog2
      (message "Loading symbols for %s..." mod)
      (hhp-sync-process (format "browse %s\n" mod))
    (message "Loading symbols for %s...done" mod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion
;;;

(defvar hhp-completion-buffer-name "*Completions*")

(defun hhp-complete ()
  (interactive)
  (if (hhp-should-scroll)
      (hhp-scroll-completion-buffer)
    (hhp-try-complete)))

(defun hhp-should-scroll ()
  (let ((window (hhp-completion-window)))
    (and (eq last-command this-command)
	 window (window-live-p window) (window-buffer window)
	 (buffer-name (window-buffer window)))))

(defun hhp-scroll-completion-buffer ()
  (let ((window (hhp-completion-window)))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
	  (set-window-start window (point-min))
	(save-selected-window
	  (select-window window)
	  (scroll-up))))))

(defun hhp-completion-window ()
  (get-buffer-window hhp-completion-buffer-name 0))

(defun hhp-try-complete ()
  (let* ((end (point))
	 (symbols (hhp-select-completion-symbol))
	 (beg (hhp-completion-start-point))
	 (pattern (buffer-substring-no-properties beg end))
	 (completion (try-completion pattern symbols)))
    (cond
     ((eq completion t) ;; completed
      ) ;; do nothing
     ((null completion) ;; no completions
      (ding))
     ((not (string= pattern completion)) ;; ???
      (delete-region beg end)
      (insert completion)
      (hhp-reset-window-configuration))
     (t ;; multiple completions
      (let* ((list0 (all-completions pattern symbols))
	     (list (sort list0 'string<)))
	(if (= (length list) 1)
	    (hhp-reset-window-configuration)
	  (hhp-save-window-configuration)
	  (with-output-to-temp-buffer hhp-completion-buffer-name
	    (display-completion-list list))))))))

(defun hhp-save-window-configuration ()
  (unless (get-buffer-window hhp-completion-buffer-name)
    (setq hhp-window-configuration (current-window-configuration))))

(defun hhp-reset-window-configuration ()
  (when hhp-window-configuration
    (set-window-configuration hhp-window-configuration)
    (setq hhp-window-configuration nil)))

(defun hhp-module-completion-p ()
  (or (minibufferp)
      (let ((end (point)))
	(save-excursion
	  (beginning-of-line)
	  (and (looking-at "import ")
	       (not (search-forward "(" end t)))))
      (save-excursion
	(beginning-of-line)
	(looking-at " +module "))))

(defun hhp-select-completion-symbol ()
  (cond
   ((hhp-module-completion-p)
    hhp-module-names)
   ((save-excursion
      (beginning-of-line)
      (looking-at "{-# LANGUAGE "))
    hhp-language-extensions)
   ((save-excursion
      (beginning-of-line)
      (looking-at "{-# OPTIONS_GHC "))
    hhp-option-flags)
   ((save-excursion
      (beginning-of-line)
      (looking-at "{-# "))
    hhp-pragma-names)
   ((or (bolp)
	(let ((end (point)))
	  (save-excursion
	    (beginning-of-line)
	    (not (search-forward " " end t)))))
    hhp-reserved-keyword-for-bol)
   (t hhp-merged-keyword)))

(defun hhp-completion-start-point ()
  (save-excursion
    (let ((beg (save-excursion (beginning-of-line) (point)))
	  (regex (if (hhp-module-completion-p) "[ (,`]" "[\[ (,`.]")))
      (if (re-search-backward regex beg t)
	  (1+ (point))
	beg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading keywords
;;;

(defun hhp-import-module ()
  (interactive)
  (hhp-load-module-buffer))

(defun hhp-unloaded-modules (mods)
  (hhp-filter (lambda (mod)
		(and (member mod hhp-module-names)
		     (not (member mod hhp-loaded-module))))
	      mods))

(defun hhp-load-module-buffer ()
  (hhp-load-merge-modules (hhp-gather-import-modules-buffer)))

(defun hhp-load-merge-modules (mods)
  (let* ((umods (hhp-unloaded-modules mods))
	 (syms (mapcar 'hhp-module-symbol umods))
	 (names (hhp-load-modules umods)))
    (hhp-set syms names)
    (hhp-merge-keywords umods)))

(defun hhp-merge-keywords (mods)
  (setq hhp-loaded-module (append mods hhp-loaded-module))
  (let* ((modkeys (mapcar 'hhp-module-keyword hhp-loaded-module))
	 (keywords (cons hhp-extra-keywords (cons hhp-reserved-keyword modkeys)))
	 (uniq-sorted (sort (hhp-uniq-lol keywords) 'string<)))
    (setq hhp-merged-keyword uniq-sorted)))

(defun hhp-module-symbol (mod)
  (intern (concat hhp-keyword-prefix mod)))

(defun hhp-module-keyword (mod)
  (symbol-value (hhp-module-symbol mod)))

(defun hhp-gather-import-modules-buffer ()
  (let (ret)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^import\\( *qualified\\)? +\\([^\n ]+\\)" nil t)
	(hhp-add ret (match-string-no-properties 2))
	(forward-line)))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Background Idle Timer
;;;

(provide 'hhp-comp)
