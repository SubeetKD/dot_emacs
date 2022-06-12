;; My custom functions

;; Window functions
(defun subeet/split-windown-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun subeet/split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))


(defun subeet/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
		   (assoc ?_ register-alist))
	  (jump-to-register ?_)
	(progn
	  (window-configuration-to-register ?_)
	  (delete-other-window))))
