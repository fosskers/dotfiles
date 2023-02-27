set -x CHROME_EXECUTABLE google-chrome-stable
set -x EDITOR emacs
set -x GTK_THEME "Adwaita:dark"
set -x GUILE /home/colin/code/guile/guile/meta/guile
set -x GUILE_LOAD_PATH /home/colin/code/guile/home
set -x JAVA_HOME /usr/lib/jvm/default
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'
set -x LEDGER_FILE '/home/colin/sync/life/finances/finances.journal'
set -x MOZ_ENABLE_WAYLAND 1
set -x PATH /bin /usr/local/bin /usr/bin /usr/sbin /sbin '/home/colin/.local/bin' /usr/bin/core_perl /home/colin/code/go/bin '/home/colin/.cargo/bin/' '/home/colin/.local/npm/node_modules/.bin' '/home/colin/.deno/bin' '/home/colin/.emacs.d/bin' '/home/colin/.ghcup/bin' '/home/colin/.chicken/bin' /home/colin/code/flutter/flutter/bin '/home/colin/.config/guix/current/bin'
set -x PKG_CONFIG_ALLOW_CROSS 1
set -x SDL_VIDEODRIVER wayland
set -x _JAVA_AWT_WM_NONREPARENTING 1

# set -x _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
# set -x GOPATH /home/colin/code/go
# set -x GO111MODULE on
# set -x CGO_ENABLED 1
# set -x CHICKEN_INSTALL_REPOSITORY /home/colin/.chicken/lib
# set -x CHICKEN_REPOSITORY_PATH /usr/lib/chicken/11 /home/colin/.chicken/lib
# set -x CHICKEN_INSTALL_PREFIX /home/colin/.chicken

# A higher file descriptor limit
# ulimit -Sn 20000

# Allow me to type Esperanto!
# setxkbmap -option compose:ralt
# setxkbmap -option ctrl:nocaps

function la
    exa -laah
end

function hi
    hledger is -VMA -b 2022-09 --pretty-tables $argv
end

function hb
    hledger bs -V --tree --pretty-tables $argv
end

function hc
    hledger cf -VMT -b 2022-09 $argv
end

function hbud
    hledger b -MV -b 2020-02 -e 2020-05 --budget --depth 3
end

function c
    clear
    ls
end

function mkcd
    mkdir $argv
    cd $argv
end

# GIT COMMANDS
function gd
    git diff $argv
end

function gl
    git log --graph --show-signature
end

starship init fish | source
