#!/bin/sh

# Japanese input via ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus

# Set the background
/home/colin/.cargo/bin/setwall random /home/colin/backgrounds/

# Set compose key for UTF-8 input
setxkbmap -option compose:ralt
# CapsLock -> Ctrl
setxkbmap -option ctrl:nocaps

# Grab terminal colour settings, etc.
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# Make the top row of keyboard buttons work
xbindkeys -p

# For switching to Japanese input.
ibus-daemon -drx

# Position monitors. Fails gracefully if the other monitor isn't present.
xrandr --output eDP1 --mode 1920x1080 --rate 60 --output HDMI2 --auto --right-of eDP1

# Configure the Wacom tablet, if present.
wacom

# Launch window manager.
exec i3
