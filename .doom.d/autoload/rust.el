;;; autoload/rust.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/trunk ()
  "Load a `trunk' session in a terminal."
  (interactive)
  (when (+rust-cargo-project-p)
    (colin/in-terminal "trunk serve --open")))

;;;###autoload
(defun colin/seed ()
  "When invoked from a Seed project, serve the server and open 'cargo watch'.
Also runs a 'sass --watch' process if it detects a main `.scss'
file."
  (interactive)
  (let ((buffer (current-buffer)))
    ;; TODO Consider `+vterm/toggle'.
    (colin/new-terminal-over-there)
    (vterm-send-string "cargo make serve")
    (vterm-send-return)
    (colin/new-terminal-down-there)
    (vterm-send-string "cargo make watch")
    (vterm-send-return)
    (when-let ((project-root (doom-project-root)))
      (colin/seed-watch-scss project-root))
    (balance-windows)
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun colin/seed-watch ()
  "Open a 'cargo make watch' for every sub library that has a `Makefile.toml'.
Also opens a top level 'cargo watch' and 'sass --watch' for every `.scss' file it can find."
  (interactive)
  (when-let* ((project-root (doom-project-root))
              (make-files (directory-files-recursively project-root "^Makefile.toml$"))
              (dirs (mapcar (lambda (file) (file-name-directory file)) make-files))
              (first (car dirs))
              (buffer (current-buffer)))
    (colin/cargo-watch)
    (colin/in-terminal-stay (format "cd %s; cargo make watch" first))
    (tear-off-window nil)
    (dolist (dir (cdr dirs))
      (colin/in-terminal (format "cd %s; cargo make watch" dir)))
    (colin/seed-watch-scss project-root)
    (switch-to-buffer-other-frame buffer)))

;;;###autoload
(defun colin/seed-watch-scss (root)
  "Open a 'sass --watch' session for every `.scss' found from the given ROOT downwards."
  (interactive "fPath: ")
  (let ((files (directory-files-recursively root "[.]scss$")))
    (dolist (scss files)
      (let ((css (file-name-with-extension scss "css")))
        (colin/in-terminal (format "sass --watch %s %s" scss css))))))

;;;###autoload
(defun colin/cargo-watch ()
  "Open a 'cargo watch' session, if possible."
  (interactive)
  (when (+rust-cargo-project-p)
    (colin/in-terminal "cargo watch -c -q")))