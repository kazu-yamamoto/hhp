;;; hhp.el --- hhp front-end for haskell-mode

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009
;; Revised:

;; Put the following code to your "~/.emacs".
;;
;; (autoload 'hhp-init "hhp" nil t)
;; (autoload 'hhp-debug "hhp" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (hhp-init)))
;;
;; Or if you wish to display error each goto next/prev error,
;; set hhp-display-error valiable.
;;
;; (setq hhp-display-error 'minibuffer) ; to minibuffer
;; ; (setq hhp-display-error 'other-buffer) ; to other-buffer

;;

;;; Code:

;; defvar-local was introduced in 24.3
(let* ((major 24)
       (minor 3))
  (if (or (< emacs-major-version major)
	  (and (= emacs-major-version major)
	       (< emacs-minor-version minor)))
      (error "hhp requires at least Emacs %d.%d" major minor)))

(defconst hhp-version "0.0.2")

;; (eval-when-compile
;;  (require 'haskell-mode))

(require 'hhp-comp)
(require 'hhp-doc)
(require 'hhp-info)
(require 'hhp-check)
(require 'hhp-command)
(require 'hhp-ins-mod)
(require 'hhp-indent)
(require 'dabbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

(defun hhp-find-C-h ()
  (or
   (when keyboard-translate-table
     (aref keyboard-translate-table ?\C-h))
   ?\C-h))

(defvar hhp-completion-key  "\e\t")
(defvar hhp-document-key    "\e\C-d")
(defvar hhp-import-key      "\e\C-m")
(defvar hhp-previous-key    "\ep")
(defvar hhp-next-key        "\en")
(defvar hhp-help-key        "\e?")
(defvar hhp-insert-key      "\et")
(defvar hhp-sort-key        "\es")
(defvar hhp-type-key        "\C-c\C-t")
(defvar hhp-info-key        "\C-c\C-i")
(defvar hhp-toggle-key      "\C-c\C-c")
(defvar hhp-jump-key        "\C-c\C-j")
(defvar hhp-module-key      "\C-c\C-m")
(defvar hhp-expand-key      "\C-c\C-e")
(defvar hhp-kill-key        "\C-c\C-k")
(defvar hhp-hoogle-key      (format "\C-c%c" (hhp-find-C-h)))
(defvar hhp-shallower-key   "\C-c<")
(defvar hhp-deeper-key      "\C-c>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar hhp-initialized nil)

;;;###autoload
(defun hhp-init ()
  (hhp-abbrev-init)
  (hhp-type-init)
  (unless hhp-initialized
    (define-key haskell-mode-map hhp-completion-key  'hhp-complete)
    (define-key haskell-mode-map hhp-document-key    'hhp-browse-document)
    (define-key haskell-mode-map hhp-type-key        'hhp-show-type)
    (define-key haskell-mode-map hhp-info-key        'hhp-show-info)
    (define-key haskell-mode-map hhp-expand-key      'hhp-expand-th)
    (define-key haskell-mode-map hhp-import-key      'hhp-import-module)
    (define-key haskell-mode-map hhp-previous-key    'hhp-goto-prev-error)
    (define-key haskell-mode-map hhp-next-key        'hhp-goto-next-error)
    (define-key haskell-mode-map hhp-help-key        'hhp-display-errors)
    (define-key haskell-mode-map hhp-insert-key      'hhp-insert-template)
    (define-key haskell-mode-map hhp-sort-key        'hhp-sort-lines)
    (define-key haskell-mode-map [remap save-buffer] 'hhp-save-buffer)
    (define-key haskell-mode-map hhp-toggle-key      'hhp-toggle-check-command)
    (define-key haskell-mode-map hhp-jump-key        'hhp-jump-file)
    (define-key haskell-mode-map hhp-module-key      'hhp-insert-module)
    (define-key haskell-mode-map hhp-kill-key        'hhp-kill-process)
    (define-key haskell-mode-map hhp-hoogle-key      'haskell-hoogle)
    (define-key haskell-mode-map hhp-shallower-key   'hhp-make-indent-shallower)
    (define-key haskell-mode-map hhp-deeper-key      'hhp-make-indent-deeper)
    (hhp-comp-init)
    (setq hhp-initialized t))
  (hhp-import-module)
  (hhp-check-syntax))

(defun hhp-abbrev-init ()
  (set (make-local-variable 'dabbrev-case-fold-search) nil))

;;;###autoload
(defun hhp-debug ()
  (interactive)
  (let ((el-path (locate-file "hhp.el" load-path))
	(ghc-path (executable-find "ghc")) ;; FIXME
	(hhpc-path (executable-find hhpc-command))
	(hhpi-path (executable-find hhpi-command))
	(el-ver hhp-version)
	(ghc-ver (hhp-run-hhp '("--version") "ghc"))
	(hhp-ver (hhp-run-hhp '("version")))
	(hhpi-ver (hhp-run-hhp '("version") hhpi-command))
	(path (getenv "PATH")))
    (switch-to-buffer (get-buffer-create "**HHP Debug**"))
    (erase-buffer)
    (insert "Path: check if you are using intended programs.\n")
    (insert (format "\t  hhp.el path: %s\n" el-path))
    (insert (format "\t    hhpc path: %s\n" hhpc-path))
    (insert (format "\t    hhpi path: %s\n" hhpi-path))
    (insert (format "\t     ghc path: %s\n" ghc-path))
    (insert "\nVersion: all versions must be the same.\n")
    (insert (format "\thhp.el version %s\n" el-ver))
    (insert (format "\t  %s\n" hhp-ver))
    (insert (format "\t  %s\n" hhpi-ver))
    (insert (format "\t%s\n"  ghc-ver))
    (insert "\nEnvironment variables:\n")
    (insert (format "\tPATH=%s\n" path))))

(provide 'hhp)
