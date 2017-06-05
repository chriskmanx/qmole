#!/bin/bash
g++ -DLOCALEDIR=\"/usr/local/share/locale\" -DHAVE_CONFIG_H -I. -I..  -I. -I.. -I../intl -I/usr/local/include -I/usr/local/include/freetype2 -I/usr/local/include/freetype2 -I/usr/local/include   -I/usr/local/include/fox-1.6 -DHAVE_XFT_H -DSTARTUP_NOTIFICATION -MT icons.o -MD -MP -MF .deps/icons.Tpo -c -o icons.o icons.cpp
