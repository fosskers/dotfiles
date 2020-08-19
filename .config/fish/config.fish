set -x PATH '/bin' '/usr/local/bin' '/usr/bin' '/usr/sbin' '/sbin' '/home/colin/.local/bin' '/usr/bin/core_perl' '/home/colin/.nix-profile/bin' '/home/colin/code/go/bin' '/home/colin/code/ruby/gems/bin' '/home/colin/.cargo/bin/'

set -x PKG_CONFIG_ALLOW_CROSS 1

set -x GEM_HOME '/home/colin/code/ruby/gems'

# Golang
set -x GOPATH '/home/colin/code/go'
set -x GO111MODULE 'auto'
set -x CGO_ENABLED 1

set -x EDITOR "emacs"

set -x JAVA_HOME '/usr/lib/jvm/default'

set -x SPARK_HOME '/opt/apache-spark'

# HLedger
set -x LEDGER_FILE '/home/colin/sync/life/finances/finances.journal'

# Nix
set -x NIX_PATH 'nixpkgs=/home/colin/.nix-defexpr/channels/nixpkgs'
set -x NIX_SSL_CERT_FILE '/etc/ssl/certs/ca-certificates.crt'

# Temporary
set -x LANG 'en_US.UTF-8'
set -x LC_ALL 'en_US.UTF-8'

# A higher file descriptor limit
ulimit -Sn 20000

# Allow me to type Esperanto!
setxkbmap -option compose:ralt
setxkbmap -option ctrl:nocaps

function hi
    hledger is -VMA -b 2020 $argv
end

function hb
    hledger bs -V --tree $argv
end

function hc
    hledger cf -VQT -b 2020 $argv
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
