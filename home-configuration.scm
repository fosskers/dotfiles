(use-modules
 (gnu home)
 (gnu home services)
 (gnu home services shells)
 (gnu packages)
 (gnu packages base)
 (gnu services)
 (guix gexp))

(define colin-locales
  (make-glibc-utf8-locales
   glibc
   #:locales '("en_US")
   #:name "glibc-us-utf8-locales"))

;; NOTE 2023-09-24 swaylock-effects cannot be used as it somehow can't
;; communicate with the underlying system's password manager in order to
;; actually unlock. I learned this the hard way and had to do a hard restart.
(define my-packages
  '(;; --- Applications --- ;;
    ;; "krita"  ; pulls a lot of KDE
    "libreoffice"
    "sway"
    ;; --- Programming --- ;;
    "clojure-tools"
    "fennel"
    "fnlfmt"
    "sbcl"
    ;; --- System Tools --- ;;
    "foot"
    "git"
    "git:send-email"
    "grimshot"
    "htop"
    "i3status"
    "mpv"
    "ncdu"
    "ripgrep"
    "swaybg"
    "yt-dlp"
    "zsh"
    "zsh-autosuggestions"
    "zsh-completions"
    "zsh-syntax-highlighting"
    ;; --- Wayland --- ;;
    "mako"
    ;; Pinned to 5.x for Calibre
    "qtwayland@5"
    ;; --- Misc --- ;;
    ;; For git to work.
    "nss-certs"))

(home-environment
 (packages
  (cons colin-locales
        (specifications->packages my-packages)))

 (services
  (list
   (service
    home-zsh-service-type
    (home-zsh-configuration
     (zprofile (list (local-file "/home/colin/dotfiles/zsh/.zprofile" "zprofile")))
     (zshenv   (list (local-file "/home/colin/dotfiles/zsh/.zshenv" "zshenv")))
     (zshrc    (list (local-file "/home/colin/dotfiles/zsh/.zshrc" "zshrc")))
     (environment-variables
      '(("EDITOR" . "emacs")
        ("GTK_THEME" . "Adwaita:dark")
        ("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale/")
        ("JAVA_HOME" . "/usr/lib/jvm/default")
        ("LANG" . "en_US.UTF-8")
        ("LC_ALL" . "en_US.UTF-8")
        ("LEDGER_FILE" . "/home/colin/sync/life/finances/finances.journal")
        ("MOZ_ENABLE_WAYLAND" . "1")
        ("SDL_VIDEODRIVER" . "wayland")
        ("_JAVA_AWT_WM_NONREPARENTING" . "1")))))
   (simple-service
    'dotfiles
    home-xdg-configuration-files-service-type
    `(("aura.toml"       ,(local-file "/home/colin/dotfiles/aura.toml"))
      ("fcitx5/config"   ,(local-file "/home/colin/dotfiles/fcitx5/config"))
      ("foot/foot.ini"   ,(local-file "/home/colin/dotfiles/foot/foot.ini"))
      ("i3status/config" ,(local-file "/home/colin/dotfiles/i3status/config"))
      ("sway/config"     ,(local-file "/home/colin/dotfiles/sway/config"))
      ("swaylock/config" ,(local-file "/home/colin/dotfiles/swaylock/config"))
      ("swaynag/config"  ,(local-file "/home/colin/dotfiles/swaynag/config"))
      ("wofi/style.css"  ,(local-file "/home/colin/dotfiles/wofi/style.css")))))))

