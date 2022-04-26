;;; autoload/editing.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/copy-and-comment-region ()
  "Copy the current region above itself and comment it out."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (copy-region-as-kill start end)
    (comment-region start end)
    (newline)
    (yank)
    (+format/buffer)))
