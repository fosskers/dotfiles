;;; autoload/util.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro message! (patt &rest args)
  "Like `message', but prefix the message with the name of the calling function."
  `(message "%s: %s" this-command (format ,patt ,@args)))

;;;###autoload
(defun colin/insert-date ()
  "Insert the DateTime at `point'."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S %Z")))

;;;###autoload
(defun colin/window-go-home ()
  "Returns a buffer in a torn-off frame to another.
Does nothing if there is only one frame open."
  (interactive)
  (let ((buffer (current-buffer))
        (this-frame (window-frame))
        (that-frame (next-frame)))
    (unless (eq this-frame that-frame)
      (let ((window (frame-root-window that-frame)))
        (when window
          (evil-quit) ; Closes the window and its frame if it was the last one.
          (split-window window nil 'left nil)
          (set-window-buffer window buffer))))))

