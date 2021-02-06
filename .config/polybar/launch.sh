#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

if type "xrandr"; then
    for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=$m polybar --reload top &
    done
else
    polybar --reload top &
fi
