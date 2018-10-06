#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

########################################
# key bindings
########################################

# makes Caps another Ctrl
setxkbmap -option 'caps:escape'

# when both Shift buttons are pressed simultaneously, they turn Caps
# Lock on. it can be switched off by pressing Shift
setxkbmap -option 'shift:both_capslock_cancel'

exec i3
