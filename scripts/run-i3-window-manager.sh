#!/bin/bash
export DISPLAY=localhost:0.0
#full screen 768x1024
Xvnc -geometry 768x915  -depth 16 -SecurityTypes None &

# Disable X key repeat after we may assume X11 is running
sleep 3
#xset -display localhost:0.0 r rate 500 10
xset -r

#Emacs started from herbstluftwm autostart under ~/config/herbsluftwm
#emacs --geometry=53x76

# Start Herbstluftwm
#awesome 2>/home/chris/.awe.err.log > /home/chris/.awe.run.log &
#fluxbox &
#emacs &
i3 &
#/usr/local/bin/Esetroot /home/chris/.fluxbox/wallpaper.jpg &

# Exploring emacs -as-is frame size
#(frame-parameter (selected-frame) 'width)
#(frame-parameter (selected-frame) 'height)
