;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-process.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  9, 2014

;;; Code:

(require 'hhp-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hhp-process-running nil)

(defvar-local hhp-process-process-name nil)
(defvar-local hhp-process-original-buffer nil)
(defvar-local hhp-process-original-file nil)
(defvar-local hhp-process-callback nil)
(defvar-local hhp-process-hook nil)

(defvar hhpi-command "hhpi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-get-project-root ()
  (hhp-run-hhp '("root")))

(defun hhp-with-process (cmd callback &optional hook1 hook2)
  (unless hhp-process-process-name
    (setq hhp-process-process-name (hhp-get-project-root)))
  (when (and hhp-process-process-name (not hhp-process-running))
    (setq hhp-process-running t)
    (if hook1 (funcall hook1))
    (let* ((cbuf (current-buffer))
	   (name hhp-process-process-name)
	   (buf (get-buffer-create (concat " hhpi:" name)))
	   (file (buffer-file-name))
	   (cpro (get-process name)))
      (hhp-with-current-buffer buf
        (setq hhp-process-original-buffer cbuf)
	(setq hhp-process-original-file file)
	(setq hhp-process-callback callback)
	(setq hhp-process-hook hook2)
	(erase-buffer)
	(let ((pro (hhp-get-process cpro name buf)))
	  (process-send-string pro cmd)
	  (when hhp-debug
	    (hhp-with-debug-buffer
	     (insert (format "%% %s" cmd))))
	  pro)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-get-process (cpro name buf)
  (cond
   ((not cpro)
    (hhp-start-process name buf))
   ((not (eq (process-status cpro) 'run))
    (delete-process cpro)
    (hhp-start-process name buf))
   (t cpro)))

(defun hhp-start-process (name buf)
  (let* ((opts (append '("-b" "\n" "-l") (hhp-make-ghc-options)))
	 (pro (apply 'start-file-process name buf hhpi-command opts)))
    (set-process-filter pro 'hhp-process-filter)
    (set-process-sentinel pro 'hhp-process-sentinel)
    (set-process-query-on-exit-flag pro nil)
    pro))

(defun hhp-process-filter (process string)
  (let ((pbuf (process-buffer process)))
    (if (not (get-buffer pbuf))
	(setq hhp-process-running nil) ;; just in case
      (hhp-with-current-buffer (process-buffer process)
        (goto-char (point-max))
	(insert string)
	(forward-line -1)
	(cond
	 ((looking-at "^OK$")
	  (if hhp-process-hook (funcall hhp-process-hook))
	  (goto-char (point-min))
	  (funcall hhp-process-callback 'ok)
	  (when hhp-debug
	    (let ((cbuf (current-buffer)))
	      (hhp-with-debug-buffer
	       (insert-buffer-substring cbuf))))
	  (setq hhp-process-running nil))
	 ((looking-at "^NG ")
	  (funcall hhp-process-callback 'ng)
	  (when hhp-debug
	    (let ((cbuf (current-buffer)))
	      (hhp-with-debug-buffer
	       (insert-buffer-substring cbuf))))
	  (setq hhp-process-running nil)))))))

(defun hhp-process-sentinel (process event)
  (setq hhp-process-running nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar hhp-process-rendezvous nil)
(defvar hhp-process-num-of-results nil)
(defvar hhp-process-results nil)

(defun hhp-sync-process (cmd &optional n hook)
  (unless hhp-process-running
    (setq hhp-process-rendezvous nil)
    (setq hhp-process-results nil)
    (setq hhp-process-num-of-results (or n 1))
    (let ((pro (hhp-with-process cmd 'hhp-process-callback nil hook)))
      (condition-case nil
	  (while (null hhp-process-rendezvous)
	    ;; 0.01 is too fast for Emacs 24.4.
	    ;; (sit-for 0.1 t) may get stuck when tooltip is displayed.
	    (sit-for 0.1)
	    ;; (discard-input) avoids getting stuck.
	    (discard-input))
	(quit
	 (setq hhp-process-running nil))))
    hhp-process-results))

(defun hhp-process-callback (status)
  (cond
   ((eq status 'ok)
    (let* ((n hhp-process-num-of-results)
	   (ret (if (= n 1)
		    (hhp-read-lisp-this-buffer)
		  (hhp-read-lisp-list-this-buffer n))))
      (setq hhp-process-results ret)))
   (t
    (setq hhp-process-results nil)))
  (setq hhp-process-num-of-results nil)
  (setq hhp-process-rendezvous t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hhp-kill-process ()
  (interactive)
  (let* ((name hhp-process-process-name)
	 (cpro (if name (get-process name))))
    (if (not cpro)
	(message "No process")
      (delete-process cpro)
      (message "A process was killed"))))

(provide 'hhp-process)
