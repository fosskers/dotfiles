;;; autoload.el -*- lexical-binding: t; -*-

(defmacro message! (patt &rest args)
  "Like `message', but prefix the message with the name of the calling function."
  `(message "%s: %s" this-command (format ,patt ,@args)))

;;;###autoload
(defun colin/vterm-kill-all ()
  "Kill all open `*vterm*' buffers."
  (interactive)
  (let ((buffers (colin/vterm-buffers)))
    (dolist (buffer buffers)
      (set-buffer buffer)
      ;; Kill any process that was running.
      (vterm-send-C-c)
      ;; C-d only works if at the beginning of the input line.
      (vterm-send-backspace)
      ;; Kill the vterm.
      (vterm-send-C-d))))

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
    (colin/new-terminal-over-there)
    (vterm-send-string command)
    (vterm-send-return)
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun colin/insert-date ()
  "Insert the DateTime at `point'."
  (interactive)
  (let ((time (format-time-string "%Y-%m-%d %H:%M:%S %Z")))
    (insert time)))

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

;; TODO Remove once upstreamed.
;;;###autoload
(defun colin/new-workspace-named (name)
  "Create a new workspace with the given NAME."
  (interactive "sWorkspace Name: ")
  (+workspace/new name))

;;;###autoload
(defun colin/kin-graph (kanji)
  "Produce a `kanji-net' graph based on KANJI and open it in a new buffer.

You can pass as many Kanji as you want as a single string. Spaces
aren't necessary, but will be accounted for on kin's end."
  (interactive "sKanji: ")
  (message! "You gave: %s" kanji)
  (let* ((outpath "/tmp/graph.png")
         (res (doom-call-process "kin" "graph" kanji "--output" outpath)))
    (when (= 0 (car res))
      (find-file-read-only outpath))))

;;;###autoload
(defun colin/upwork-earnings (rate hours usd-to-cad)
  "Estimate earnings for an hourly job on Upwork.

Given an hourly RATE and the HOURS to be worked, projects a final
pay amount with the Upwork cuts taken off. Applies a USD-TO-CAD
conversion rate at the end."
  (let* ((gross (* rate hours))
         (usd (cond ((<= gross 500.0) (* gross 0.8))
                    ((<= gross 10000.0) (+ (* 0.8 500)
                                           (* 0.9 (- gross 500.0))))
                    (t (+ (* 0.8 500)
                          (* 0.9 9500)
                          (* 0.95 (- gross 10000.0)))))))
    (round (* usd usd-to-cad))))

;;;###autoload
(defun colin/trunk ()
  "Load a `trunk' session in a terminal."
  (interactive)
  (when (+rust-cargo-project-p)
    (colin/in-terminal "trunk serve --open")))

;;;###autoload
(defun colin/seed ()
  "When invoked from a Seed project, serve the server and open `cargo watch'.
Also runs a `sass --watch' process if it detects a main `.scss' file."
  (interactive)
  (let ((buffer (current-buffer)))
    ;; TODO Consider `+vterm/toggle'.
    (colin/new-terminal-over-there)
    (vterm-send-string "cargo make serve")
    (vterm-send-return)
    (colin/new-terminal-down-there)
    (vterm-send-string "cargo make watch")
    (vterm-send-return)
    ;; TODO Considering `dolist' over every `.scss' it can find.
    (when-let* ((css-files (colin/seed--css))
                (scss (colin/seed--scss-file css-files))
                (css (file-name-with-extension scss "css"))
                (cmd (format "sass --watch assets/css/%s assets/css/%s" scss css)))
      (colin/new-terminal-down-there)
      (vterm-send-string cmd)
      (vterm-send-return))
    (balance-windows)
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun colin/seed--scss-file (files)
  "Given some FILES, extract the first `.scss' file it can find."
  (car (-filter (lambda (file) (string= "scss" (file-name-extension file)))
                files)))

;;;###autoload
(defun colin/seed--css ()
  "The contents of `<project-root>/assets/css/', if it exists.
Returns nil otherwise."
  (when-let* ((project-root (doom-project-root))
              (css (doom-path project-root "assets" "css")))
    (when (file-exists-p css)
      (directory-files css))))

;;;###autoload
(defun colin/cargo-watch ()
  "Open a `cargo watch' session, if possible."
  (interactive)
  (when (+rust-cargo-project-p)
    (colin/in-terminal "cargo watch -c -q")))

;;;###autoload
(defun colin/haskell-project-cabal-file ()
  "The `.cabal' file from the project root, if present."
  (interactive)
  (when-let* ((root (doom-project-root))
              (files (directory-files root)))
    (car (-filter (lambda (file) (string= "cabal" (file-name-extension file))) files))))

;;;###autoload
(defun colin/ghcid ()
  "Open `ghcid' within a Haskell project, if possible."
  (interactive)
  (when (colin/haskell-project-cabal-file)
    (colin/in-terminal "ghcid")))

;;;###autoload
(defun colin/hledger-transfers (income liabilities)
  "Automate monthly transfers, given some INCOME and LIABILITIES."
  (interactive "nIncome: \nnMastercard: ")
  (when-let* ((buffer (find-file-noselect hledger-jfile))
              (raw (colin/hledger-transfers-raw income liabilities)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n")
      (insert raw)
      (save-buffer))))

;;;###autoload
(defun colin/hledger-transfers-raw (income liabilities)
  "Given a month's INCOME and its LIABILITIES, produce a valid Hledger transaction string."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (tax (* income 0.25))
         (take-home (- income tax))
         (donation (* take-home 0.1))
         (tfsa (* take-home 0.18)))
    (concat (format "%s Monthly Transfers\n" today)
            (format "    assets:bs:sav:tax          %.2f C\n" tax)
            (format "    assets:bs:sav:donation     %.2f C\n" donation)
            (format "    assets:bs:sav:japan\n")
            (format "    assets:qtrade:tfsa         %.2f C\n" tfsa)
            (format "    liabilities:bs:mastercard  %.2f C\n" liabilities)
            (format "    assets:bs:chequing         %.2f C" (* -1.0 income)))))

;; --- DEBUGGING --- ;;

;;;###autoload
(defun bug/switch-frame ()
  "Reproduce the frame switching bug on Wayland."
  (interactive)
  (let* ((orig-buffer (current-buffer))
         (orig-window (get-buffer-window orig-buffer))
         (orig-frame (window-frame orig-window))
         (split-window (split-window-right))
         (split-buffer (window-buffer split-window)))
    (switch-to-buffer-other-window split-buffer)
    (switch-to-buffer "*scratch*")
    (tear-off-window nil)
    (let* ((torn-window (get-buffer-window))
           (torn-frame (window-frame))
           (selected (selected-frame))
           (next (next-frame)))
      (message "Original Frame: %s" orig-frame)
      (message "Torn Frame: %s" torn-frame)
      (message "Selected Frame: %s" selected)
      (message "Next Frame: %s" next)
      (when (window-live-p orig-window)
        (message "Original window is live."))
      ;; (select-frame-set-input-focus orig-frame)
      (select-window orig-window))))
;; (select-frame-set-input-focus orig-frame))))
;; (select-frame-set-input-focus next))))
;; (switch-to-buffer-other-frame nil))))
;; (switch-to-buffer-other-frame orig-buffer))))
;; (princ orig-frame)
;; (princ split-frame)
;; (princ (frame-list))))
;; (switch-to-buffer-other-frame orig-buffer)))
