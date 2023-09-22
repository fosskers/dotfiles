;; The start of beautiful Luakit config in Fennel!

(print "[       colin] Loading Fennel config...")

(local modes (require :modes))
(local adblock (require :adblock))
(local select (require :select))

;; --- Keybindings --- ;;

(modes.remap_binds "normal" [["n" "j" false]
                             ["e" "k" false]
                             ["N" "J" false]
                             ["E" "K" false]])

;; --- Adblocking --- ;;

(adblock.load false "easylist.txt")
(adblock.load false "easyprivacy.txt")
(adblock.load false "fanboy-annoyance.txt")

;; --- Follow Mode --- ;;

(fn select.label_maker [s] (s.interleave "arst" "neio"))

;; --- Appearance --- ;;

;; --- ;;

(print "[       colin] Fennel config complete.")
