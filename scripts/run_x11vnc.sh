#!/bin/bash
sudo x11vnc -solid -forever -noipv6 -nopw -listen localhost
#export X11VNC_SHM_DEBUG
#sudo x11vnc -solid -forever -noshm -noipv6 -listen localhost