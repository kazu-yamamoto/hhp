;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hhp-indent.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb 28, 2012

;;; Code:

(defvar hhp-indent-offset 4)

(defun hhp-make-indent-shallower (beg end)
  (interactive "r")
  (indent-rigidly (region-beginning) (region-end) (- hhp-indent-offset)))

(defun hhp-make-indent-deeper (beg end)
  (interactive "r")
  (indent-rigidly (region-beginning) (region-end) hhp-indent-offset))

(provide 'hhp-indent)
