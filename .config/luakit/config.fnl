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

;; (tset select "label_maker" (fn [] (interleave "arst" "neio")))

;; --- ;;

(print "[       colin] Fennel config complete.")
