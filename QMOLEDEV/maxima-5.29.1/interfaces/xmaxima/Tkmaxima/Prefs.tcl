# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Prefs.tcl,v 1.4 2002-09-13 17:42:21 mikeclarkson Exp $
#
proc resetMaximaFont { w } {
    global maxima_default

    $w config -font [xHMmapFont font:fixed:normal:r:[expr $maxima_default(fontAdjust) + 3]]
}




