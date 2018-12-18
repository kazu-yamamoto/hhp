;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-func.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

;;; Code:

(defvar hhpc-command "hhpc"
  "*The command name of \"hhpc\"")

(defvar hhp-ghc-options nil "*GHC options")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-replace-character (string from to)
  "Replace characters equal to FROM to TO in STRING."
  (let ((ret (copy-sequence string)))
    (dotimes (cnt (length ret) ret)
      (if (char-equal (aref ret cnt) from)
	  (aset ret cnt to)))))

(defun hhp-replace-character-buffer (from-c to-c)
  (let ((from (char-to-string from-c))
	(to (char-to-string to-c)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward from nil t)
	(replace-match to)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro hhp-add (sym val)
  `(setq ,sym (cons ,val ,sym)))

(defun hhp-set (vars vals)
  (dolist (var vars)
    (if var (set var (car vals))) ;; var can be nil to skip
    (setq vals (cdr vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-filter (pred lst)
  (let (ret)
    (dolist (x lst (reverse ret))
      (if (funcall pred x) (hhp-add ret x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-uniq-lol (lol)
  (let ((hash (make-hash-table :test 'equal))
	ret)
    (dolist (lst lol)
      (dolist (key lst)
	(puthash key key hash)))
    (maphash (lambda (key val) (hhp-add ret key)) hash)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-read-lisp (func)
  (with-temp-buffer
    (funcall func)
    (hhp-read-lisp-this-buffer)))

;; OK/NG are ignored.
(defun hhp-read-lisp-this-buffer ()
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
	(read (current-buffer))
      (error ()))))

(defun hhp-read-lisp-list-this-buffer (n)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
	(let ((m (set-marker (make-marker) 1 (current-buffer)))
	      ret)
	  (dotimes (i n (nreverse ret))
	    (hhp-add ret (read m))))
      (error ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-mapconcat (func list)
  (apply 'append (mapcar func list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-things-at-point ()
  (thing-at-point 'sexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-keyword-number-pair (spec)
  (let ((len (length spec)) key ret)
    (dotimes (i len (nreverse ret))
      (setq key (intern (concat ":" (symbol-name (car spec)))))
      (setq ret (cons (cons key i) ret))
      (setq spec (cdr spec)))))

(defmacro hhp-defstruct (type &rest spec)
  `(progn
     (hhp-defstruct-constructor ,type ,@spec)
     (hhp-defstruct-s/getter ,type ,@spec)))

(defmacro hhp-defstruct-constructor (type &rest spec)
  `(defun ,(intern (concat "hhp-make-" (symbol-name type))) (&rest args)
     (let* ((alist (quote ,(hhp-keyword-number-pair spec)))
	    (struct (make-list (length alist) nil))
	    key val key-num)
       (while args ;; cannot use dolist
	 (setq key  (car args))
	 (setq args (cdr args))
	 (setq val  (car args))
	 (setq args (cdr args))
	 (unless (keywordp key)
	   (error "'%s' is not a keyword" key))
	 (setq key-num (assoc key alist))
	 (if key-num
	     (setcar (nthcdr (cdr key-num) struct) val)
	   (error "'%s' is unknown" key)))
       struct)))

(defmacro hhp-defstruct-s/getter (type &rest spec)
  `(let* ((type-name (symbol-name ',type))
	  (keys ',spec)
	  (len (length keys))
	  member-name setter getter)
     (dotimes (i len)
       (setq member-name (symbol-name (car keys)))
       (setq setter (intern (format "hhp-%s-set-%s" type-name member-name)))
       (fset setter (list 'lambda '(struct value) (list 'setcar (list 'nthcdr i 'struct) 'value) 'struct))
       (setq getter (intern (format "hhp-%s-get-%s" type-name member-name)))
       (fset getter (list 'lambda '(struct) (list 'nth i 'struct)))
       (setq keys (cdr keys)))))

(defun hhp-make-ghc-options ()
  (hhp-mapconcat (lambda (x) (list "-g" x)) hhp-ghc-options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst hhp-error-buffer-name "*HHP Info*")

(defun hhp-display (fontify ins-func)
  (let ((buf hhp-error-buffer-name))
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (erase-buffer)
        (funcall ins-func)
        (goto-char (point-min))
        (if (not fontify)
            (font-lock-mode -1)
          (haskell-font-lock-defaults-create)
	  (turn-on-font-lock)))
      (display-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-run-hhp (cmds &optional prog)
  (let ((target (or prog hhpc-command)))
    (hhp-executable-find target
      (let ((cdir default-directory))
	(with-temp-buffer
	  (cd cdir)
	  (apply 'hhp-call-process target nil t nil
		 (append (hhp-make-ghc-options) cmds))
	  (buffer-substring (point-min) (1- (point-max))))))))

(defmacro hhp-executable-find (cmd &rest body)
  ;; (declare (indent 1))
  `(if (not (executable-find ,cmd))
       (message "\"%s\" not found" ,cmd)
     ,@body))

(put 'hhp-executable-find 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hhp-debug nil)

(defvar hhp-debug-buffer "*HHP Debug*")

(defmacro hhp-with-debug-buffer (&rest body)
  `(with-current-buffer (set-buffer (get-buffer-create hhp-debug-buffer))
     (goto-char (point-max))
     ,@body))

(defun hhp-call-process (cmd x y z &rest args)
  (apply 'call-process cmd x y z args)
  (when hhp-debug
    (let ((cbuf (current-buffer)))
      (hhp-with-debug-buffer
       (insert (format "%% %s %s\n" cmd (mapconcat 'identity args " ")))
       (insert-buffer-substring cbuf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-enclose (expr)
  (let ((case-fold-search nil))
    (if (string-match "^[a-zA-Z0-9_]" expr)
	expr
      (concat "(" expr ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro hhp-with-current-buffer (buf &rest body)
  ;; (declare (indent 1))
  `(if (buffer-live-p ,buf)
       (with-current-buffer ,buf
	 ,@body)))

(provide 'hhp-func)
