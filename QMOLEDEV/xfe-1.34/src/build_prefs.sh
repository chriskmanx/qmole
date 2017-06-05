#!/bin/bash
g++ -DLOCALEDIR="/usr/local/share/locale" -DHAVE_CONFIG_H -I. -I.. -I. -I.. -I../intl -I/usr/local/include -I/usr/local/include/freetype2 -I/usr/local/include/freetype2 -I/usr/local/include  -I/usr/local/include/fox-1.6 -DHAVE_XFT_H -DSTARTUP_NOTIFICATION -MT Preferences.o -MD -MP -MF .deps/Preferences.Tpo -c -o Preferences.o Preferences.cpp
