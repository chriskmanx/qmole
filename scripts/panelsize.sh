#!/bin/bash
xwininfo -root -tree | grep panel | awk '{print $7, $8}'

