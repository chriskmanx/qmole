#!/bin/bash
gcc -I /opt/local/include -L /opt/X11/lib/ -o qmole-msg main.c -lX11

