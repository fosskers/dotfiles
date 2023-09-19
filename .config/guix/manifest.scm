(use-modules (guix packages)
             (gnu packages base))

(define colin-locales
  (make-glibc-utf8-locales
   glibc
   #:locales '("en_US")
   #:name "glibc-us-utf8-locales"))

(concatenate-manifests
 (list (specifications->manifest
        (list
         ;; --- Applications --- ;;
         "calibre"
         "handbrake"
         "krita"
         "libreoffice"
         "sway"
         "transmission:gui"
         ;; --- Programming --- ;;
         "clojure"
         "sbcl"
         ;; --- System Tools --- ;;
         "foot"
         "git"
         "git:send-email"
         "htop"
         "ncdu"
         "ripgrep"
         "yt-dlp"
         ;; --- Wayland --- ;;
         ;; Pinned to 5.x for Calibre
         "qtwayland@5"))
       (packages->manifest (list colin-locales))))
