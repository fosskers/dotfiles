;;; autoload/term.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/vterm-kill-all ()
  "Kill all open `*vterm*' buffers."
  (interactive)
  (dolist (buffer (colin/vterm-buffers))
    (set-buffer buffer)
    ;; Kill any process that was running.
    (vterm-send-C-c)
    ;; C-d only works if at the beginning of the input line.
    (vterm-send-backspace)
    ;; Kill the vterm.
    (vterm-send-C-d)))

;;;###autoload
(defun colin/vterm-buffers ()
  "All currently open `*vterm*' buffers."
  (doom-matching-buffers "^\\*vterm\\*"))

;;;###autoload
(defun colin/vterm-kill-window-on-exit (buffer _event)
  "Kill the entire window when a `vterm' process exits.
Does nothing if there is only one window left.

The arguments are the way they are because of the requirements of
`vterm-exit-functions', to which this function here is passed as
a hook."
  (let ((window-count (length (window-list))))
    (when (> window-count 1)
      (delete-window (get-buffer-window buffer)))))

;;;###autoload
(defun colin/terminal-over-there ()
  "Split the window vertically and either open an existing
`*vterm*' buffer or open a new terminal there."
  (interactive)
  (let ((terms (colin/vterm-buffers)))
    (if terms
        (progn
          (+evil/window-vsplit-and-follow)
          (switch-to-buffer (car terms)))
      (colin/new-terminal-over-there))))

;;;###autoload
(defun colin/new-terminal-over-there ()
  "Split the window vertically and open a new terminal there.

Returns the created buffer."
  (interactive)
  (+evil/window-vsplit-and-follow)
  (+vterm/here nil))

;;;###autoload
(defun colin/new-terminal-down-there ()
  "Split the window horizontally and open a new terminal there.

Returns the created buffer."
  (interactive)
  (+evil/window-split-and-follow)
  (+vterm/here nil))

;;;###autoload
(defun colin/in-terminal (command)
  "Open a new terminal on the right and run a command in it."
  (interactive)
  (let ((buffer (current-buffer)))
    (colin/in-terminal-stay command)
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun colin/in-terminal-stay (command)
  "Like `colin/in-terminal', but stays in the opened buffer."
  (interactive)
  (colin/new-terminal-over-there)
  (vterm-send-string command)
  (vterm-send-return))
