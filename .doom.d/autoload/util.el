;;; autoload/util.el -*- lexical-binding: t; -*-

;;;###autoload
(defmacro message! (patt &rest args)
  "Like `message', but prefix the message with the name of the calling function."
  `(message "%s: %s" this-command (format ,patt ,@args)))

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

;;;###autoload
(defun colin/lisp-object-to-number (item)
  "Try to yield a number from ITEM, depending on its type.
Also knows how to parse times, which it converts into a float.

a -> Maybe Number"
  (cond ((numberp item) item)
        ((stringp item) (let ((trimmed (string-trim item)))
                          (or (let ((str (ignore-errors (read trimmed))))
                                (when (numberp str) str))
                              (colin/parse-time trimmed))))))

;;;###autoload
(defun colin/parse-time (time)
  "Try to parse a time string into a float.
The ones-digit represents hours.

String -> Maybe Float"
  (when-let* ((parsed (parse-time-string time))
              (hours (nth 2 parsed))
              (minutes (nth 1 parsed)))
    (+ hours (/ minutes 60.0))))

;;;###autoload
(defun colin/filter-non-nil (items)
  "Remove all elements from ITEMS that are nil.

[Maybe a] -> [a]
"
  (seq-filter #'identity items))

;;;###autoload
(defun colin/mu4e-delete-lock ()
  "Delete a stuck xapian lock."
  (interactive)
  (delete-file "/home/colin/.cache/mu/xapian/flintlock")
  (message "Xapian lock deleted."))
