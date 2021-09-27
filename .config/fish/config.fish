set -x PATH /bin /usr/local/bin /usr/bin /usr/sbin /sbin '/home/colin/.local/bin' /usr/bin/core_perl /home/colin/code/go/bin '/home/colin/.cargo/bin/' '/home/colin/.local/npm/node_modules/.bin' '/home/colin/.deno/bin' '/home/colin/.emacs.d/bin' '/home/colin/.ghcup/bin'

set -x PKG_CONFIG_ALLOW_CROSS 1
set -x EDITOR emacs
set -x JAVA_HOME /usr/lib/jvm/default
set -x DOOMDIR '/home/colin/dotfiles/.doom.d'

# Wayland
set -x MOZ_ENABLE_WAYLAND 1
# set -x QT_QPA_PLATFORM 'wayland-egl'
set -x SDL_VIDEODRIVER wayland
# set -x SDL_VIDEODRIVER x11

# Golang
set -x GOPATH /home/colin/code/go
set -x GO111MODULE on
set -x CGO_ENABLED 1

# HLedger
set -x LEDGER_FILE '/home/colin/sync/life/finances/finances.journal'

# Temporary
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'

# A higher file descriptor limit
ulimit -Sn 20000

# Allow me to type Esperanto!
setxkbmap -option compose:ralt
setxkbmap -option ctrl:nocaps

function la
    exa -laah
end

function hi
    hledger is -VMA -b 2021-7 --pretty-tables $argv
end

function hb
    hledger bs -V --tree --pretty-tables $argv
end

function hc
    hledger cf -VMT -b 2021-02 $argv
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
