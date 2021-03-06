;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;;(package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;;(package! builtin-package :recipe (:nonrecursive t))
;;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;;(package! builtin-package :pin "1a2b3c4d5e")

;; v1.2.4 tag
(package! org-roam :pin "9065f6a")

;; v3.1.1
(package! magit :pin "143d95cced1ee793106d16da3a182dcc2dd01e88")

;; v0.2.0
(package! forge :pin "551e515")

;; (unpin! lsp-mode)
;; (unpin! lsp-ui)
;; (unpin! all-the-icons)

(package! streak
  :recipe (:host github :repo "fosskers/streak"))

(package! org-tree-slide
  :recipe (:host github :repo "takaxp/org-tree-slide"))

(package! org-wild-notifier
  :recipe (:host github :repo "akhramov/org-wild-notifier.el"))

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"))

(package! org-mru-clock
  :recipe (:host github :repo "unhammer/org-mru-clock"))

(package! hledger-mode
  :recipe (:host github :repo "narendraj9/hledger-mode"))

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;;(unpin! pinned-package)
;; ...or multiple packages
;;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;;(unpin! t)
