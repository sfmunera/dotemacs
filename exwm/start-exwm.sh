#!/bin/sh

# Set the screen DPI
xrdb ~/.emacs.d/exwm/Xresources

compton &

# Enable screen locking on suspend
xss-lock -- slock &

# start EXWM session
exec dbus-launch --exit-with-session emacs -mm --debug-init
