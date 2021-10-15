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
(defun colin/seed-build ()
  "Build every subproject in which a Makefile.toml is detected."
  (interactive)
  (when-let* ((project-root (doom-project-root))
              (make-files (directory-files-recursively project-root "^Makefile.toml$" nil #'colin/descend-into-dir nil))
              (dirs (mapcar (lambda (file) (file-name-directory file)) make-files)))
    (dolist (dir dirs)
      (colin/in-terminal (format "cd %s; cargo make build; exit" dir)))
    (colin/seed-build-scss project-root)))

;;;###autoload
(defun colin/seed-watch ()
  "Open a 'cargo make watch' for every sub library that has a `Makefile.toml'.
Also opens a top level 'cargo watch' and 'sass --watch' for every `.scss' file it can find."
  (interactive)
  (when-let* ((project-root (doom-project-root))
              (make-files (directory-files-recursively project-root "^Makefile.toml$" nil #'colin/descend-into-dir nil))
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

(defun colin/seed-build-scss (root)
  "Convert to css every scss file found from ROOT downwards."
  (interactive "fPath: ")
  (let ((files (directory-files-recursively root "[.]scss$" nil #'colin/descend-into-dir nil)))
    (dolist (scss files)
      (let ((css (file-name-with-extension scss "css")))
        (colin/in-terminal (format "sass %s %s; exit" scss css))))))

;;;###autoload
(defun colin/seed-watch-scss (root)
  "Open a 'sass --watch' session for every `.scss' found from the given ROOT downwards."
  (interactive "fPath: ")
  (let ((files (directory-files-recursively root "[.]scss$" nil #'colin/descend-into-dir nil)))
    (dolist (scss files)
      (let ((css (file-name-with-extension scss "css")))
        (colin/in-terminal (format "sass --watch %s %s" scss css))))))

(defun colin/descend-into-dir (directory)
  "Should this DIRECTORY be descended into?
Excludes things like git and build directories."
  (let ((base (file-name-base directory))
        (bads '("target" "node_modules")))
    (not (or (equal ?. (seq-first base))
             (member base bads)))))

;;;###autoload
(defun colin/cargo-watch ()
  "Open a 'cargo watch' session, if possible."
  (interactive)
  (when (+rust-cargo-project-p)
    (colin/in-terminal "cargo watch -c -q")))
