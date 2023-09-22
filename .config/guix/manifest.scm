(use-modules (guix packages)
             (gnu packages base))

(define colin-locales
  (make-glibc-utf8-locales
   glibc
   #:locales '("en_US")
   #:name "glibc-us-utf8-locales"))

(concatenate-manifests
 (list
  (specifications->manifest
   (list
    ;; --- Applications --- ;;
    ;; "krita"
    "libreoffice"
    "sway"
    ;; --- Programming --- ;;
    "clojure"
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
    ;; Pinned to 5.x for Calibre
    "qtwayland@5"
    ;; --- Misc --- ;;
    ;; For git to work.
    "nss-certs"))
  (packages->manifest (list colin-locales))))
