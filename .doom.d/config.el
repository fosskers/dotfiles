;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Colin Woodbury"
      user-mail-address "colin@fosskers.ca")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; --- KEY BINDINGS --- ;;
(after! evil
  (map! :n "l" #'evil-insert
        :n "L" #'evil-insert-line
        :nv "h" #'evil-backward-char
        :nv "i" #'evil-forward-char
        :nv "n" #'evil-next-line
        :nv "e" #'evil-previous-line
        :nv "k" #'evil-forward-word-end
        :n "N" #'evil-join
        :n "j" #'evil-ex-search-next))

(after! magit
  (map! :map magit-mode-map
        :n "n" #'magit-next-line
        :n "e" #'magit-previous-line))

(map! :map evil-org-agenda-mode-map
      :m "n" #'org-agenda-next-line
      :m "e" #'org-agenda-previous-line
      :m "d" #'org-agenda-day-view
      :m "w" #'org-agenda-week-view)

;; To restore Spacemacs-like window switching.
(map! :leader "1" #'winum-select-window-1
      :leader "2" #'winum-select-window-2
      :leader "3" #'winum-select-window-3
      :leader "4" #'winum-select-window-4)

;; A quicker way to the Agenda view I want.
(map! :leader "a" #'org-agenda-list)

;; Easy clocking in.
(map! :leader "I" #'org-mru-clock-in)

;; Easy code commenting.
(map! :leader "C" #'comment-line)

;; Easy opening terminals.
(map! :leader "T" #'colin/terminal-over-there
      :leader "V" #'colin/new-terminal-over-there
      :leader "S" #'colin/new-terminal-down-there)

(map! :leader "w G" #'colin/window-go-home)

;; Flycheck bindings from Spacemacs.
(map! :leader "e n" #'flycheck-next-error
      :leader "e N" #'flycheck-previous-error)

;; --- ORG MODE --- ;;
(setq org-directory "~/sync/org/"
      org-roam-directory "/home/colin/sync/org-roam"
      org-agenda-files '("/home/colin/sync/colin.org"
                         "/home/colin/contracting/upwork.org"
                         "/home/colin/code/haskell/real-world-software-dev/course.org"
                         "/home/colin/sync/japan/japan.org"))

(after! org
  (setq org-todo-keywords '("TODO" "STARTED" "DONE")
        org-log-done 'time
        org-agenda-span 7
        org-agenda-start-on-weekday 0
        org-agenda-start-day "-Sun"
        org-hide-emphasis-markers t
        org-mru-clock-files #'org-agenda-files
        org-modules '(ol-bibtex org-habit))
  (org-wild-notifier-mode)
  (add-hook 'org-mode-hook #'org-appear-mode)
  (set-popup-rule! "^\\*Org Agenda" :side 'right :size 0.5))

(after! org-wild-notifier
  (setq org-wild-notifier-keyword-whitelist '()))

(after! org-tree-slide
  (map! :map org-tree-slide-mode-map
        "<f9>" #'org-tree-slide-move-previous-tree
        "<f10>" #'org-tree-slide-move-next-tree)
  (setq org-tree-slide-activate-message "発表開始"
        org-tree-slide-deactivate-message "発表終了"
        org-tree-slide-modeline-display nil))
;; (org-tree-slide-presentation-profile))

(after! org-roam
  (setq org-roam-verbose t))

;; --- MAGIT --- ;;
(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-traditional))

;; --- PROGRAMMING --- ;;
(after! haskell-mode
  (setq haskell-stylish-on-save t))

(after! web-mode
  (set-formatter! 'html-tidy
    '("prettier"
      "--parser" "html"
      "--loglevel" "silent"
      "--no-bracket-spacing"
      "--jsx-bracket-same-line")))

;; Unbreak LSP
(after! lsp-ui
  (setq lsp-ui-doc-enable nil))

;; --- FINANCE --- ;;
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(after! hledger-mode
  (setq hledger-jfile "/home/colin/sync/life/finances/finances.journal"))

;; --- MISC. --- ;;
(setq alert-default-style 'notifications)
(setq +format-on-save-enabled-modes '(not c-mode))

;; --- CUSTOM FUNCTIONS --- ;;

(defun colin/terminal-over-there ()
  "Split the window vertically and either open an existing
`*vterm*' buffer or open a new terminal there."
  (interactive)
  (let ((term (colin/vterm-buffers)))
    (if term
        (progn
          (+evil/window-vsplit-and-follow)
          (switch-to-buffer (car term)))
      (colin/new-terminal-over-there))))

(defun colin/new-terminal-over-there ()
  "Split the window vertically and open a new terminal there."
  (interactive)
  (+evil/window-vsplit-and-follow)
  (+vterm/here nil))

(defun colin/new-terminal-down-there ()
  "Split the window horizontally and open a new terminal there."
  (interactive)
  (+evil/window-split-and-follow)
  (+vterm/here nil))

(defun colin/vterm-buffers ()
  "All currently open `*vterm*' buffers."
  (doom-matching-buffers "^\\*vterm\\*"))

(defun colin/insert-date ()
  "Insert the DateTime at `point'."
  (interactive)
  (let ((time (format-time-string "%Y-%m-%d %H:%M:%S %Z")))
    (insert time)))

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

(defun colin/kin-graph (kanji)
  "Produce a `kanji-net' graph based on KANJI and open it in a new
buffer."
  (interactive "sKanji: ")
  (message "You gave: %s" kanji)
  (let* ((data "/home/colin/code/rust/kanji-net/data.json")
         (outpath "/tmp/graph.png")
         (res (doom-call-process "kin" "--data" data "graph" kanji "--output" outpath)))
    (when (= 0 (car res))
      (find-file-read-only outpath))))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
