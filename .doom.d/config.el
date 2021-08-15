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
(setq doom-unicode-font (font-spec :family "Julia Mono"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; --- KEY BINDINGS --- ;;

(map! :after evil
      :n "l" #'evil-insert
      :n "L" #'evil-insert-line
      :nv "h" #'evil-backward-char
      :nv "i" #'evil-forward-char
      :nv "n" #'evil-next-line
      :nv "e" #'evil-previous-line
      :nv "k" #'evil-forward-word-end
      :n "N" #'evil-join
      :n "j" #'evil-ex-search-next)

(map! :map magit-mode-map
      :after magit
      :n "n" #'magit-next-line
      :n "e" #'magit-previous-line)

(map! :map evil-org-agenda-mode-map
      :m "n" #'org-agenda-next-line
      :m "e" #'org-agenda-previous-line
      :m "d" #'org-agenda-day-view
      :m "w" #'org-agenda-week-view
      :m "z" #'org-agenda-view-mode-dispatch)

;; To restore Spacemacs-like window switching.
(map! :leader "1" #'winum-select-window-1
      :leader "2" #'winum-select-window-2
      :leader "3" #'winum-select-window-3
      :leader "4" #'winum-select-window-4
      :leader "5" #'winum-select-window-5
      :leader "6" #'winum-select-window-6)

;; A quicker way to the Agenda view I want.
(map! :leader "a" #'org-agenda-list)

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
                         "/home/colin/sync/org/2021.org"
                         "/home/colin/sync/org/coding.org"
                         "/home/colin/sync/org/sysadmin.org"
                         "/home/colin/contracting/upwork.org"
                         "/home/colin/code/haskell/real-world-software-dev/course.org"
                         "/home/colin/sync/japan/japan.org"))

(after! org
  (setq org-todo-keywords '("TODO" "STARTED" "DONE")
        org-log-done 'time
        org-agenda-span 7
        org-agenda-start-on-weekday 0
        org-agenda-start-day nil
        org-hide-emphasis-markers t
        org-hugo-base-dir "/home/colin/code/hugo"
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

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; --- MAGIT --- ;;

(after! magit
  (setq magit-display-buffer-function #'magit-display-buffer-traditional))

(after! forge
  (setq forge-topic-list-limit '(60 . -1)))

;; --- PROGRAMMING --- ;;

(setq +format-on-save-enabled-modes '(not c-mode))

(after! haskell-mode
  (setq haskell-stylish-on-save t))

(after! lsp-haskell
  (setq lsp-haskell-formatting-provider "stylish-haskell"))

(after! lsp-rust
  (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]))

(after! web-mode
  (set-formatter! 'html-tidy
    '("prettier"
      "--parser" "html"
      "--loglevel" "silent"
      "--no-bracket-spacing"
      "--jsx-bracket-same-line")))

;; (after! lsp-mode
;;   (setq lsp-headerline-breadcrumb-enable t))

;; --- VTERM --- ;;

(after! vterm
  (add-hook 'vterm-exit-functions #'colin/vterm-kill-window-on-exit))

;; --- IRC --- ;;

(after! circe
  (set-irc-server! "irc.libera.chat"
                   `(:tls t
                     :port 6697
                     :nick "fosskers"
                     :sasl-username "fosskers"
                     :sasl-password ,(+pass-get-secret "irc/libera.chat")
                     :channels ("#systemcrafters" "#archlinux" "#archlinux-aur")))
  (add-hook 'circe-mode-hook #'enable-circe-display-images)
  (add-hook 'circe-mode-hook #'disable-circe-new-day-notifier))

;; --- FINANCE --- ;;

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(after! hledger-mode
  (setq hledger-jfile "/home/colin/sync/life/finances/finances.journal"))

;; --- EMAIL --- ;;

(set-email-account! "fosskers.ca"
                    '((mu4e-sent-folder . "/fosskers.ca/Sent")
                      (mu4e-drafts-folder . "/fosskers.ca/Drafts")
                      (mu4e-trash-folder . "/fosskers.ca/Trash")
                      (mu4e-refile-folder . "/fosskers.ca/Archive")
                      (smtpmail-smtp-user . "colin@fosskers.ca"))
                    t)

;; --- MISC. --- ;;

(setq alert-default-style 'notifications)

(require 'streak)
(streak-mode)

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
