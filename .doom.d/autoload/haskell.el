;;; autoload/haskell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun colin/haskell-project-cabal-file ()
  "The `.cabal' file from the project root, if present."
  (interactive)
  (when-let* ((root (doom-project-root))
              (files (directory-files root)))
    (car (seq-filter (lambda (file) (string= "cabal" (file-name-extension file))) files))))

;;;###autoload
(defun colin/ghcid ()
  "Open `ghcid' within a Haskell project, if possible."
  (interactive)
  (when (colin/haskell-project-cabal-file)
    (colin/in-terminal "ghcid")))
