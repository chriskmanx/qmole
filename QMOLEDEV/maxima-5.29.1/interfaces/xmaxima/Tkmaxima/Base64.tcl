# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Base64.tcl,v 1.3 2002-09-14 17:25:34 mikeclarkson Exp $
#
###### Base64.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################
# aaaaaabb bbbbcccc ccdddddd

proc tobase64 { binary } {
    set ll [string length $binary ]
    set n [binary scan  $binary "c*" ans]
    lappend ans 0 0 0
    foreach { x y z } $ans {
	#	puts "$x $y $z $n"
	catch {
	    append new  \
		    [char64 [expr {(($x & 255)>>2) }]][char64 \
		    [expr {((($x & 3)<<4) | (($y >> 4) & 15))}]][char64 \
		    [expr {((($y & 15)<<2) | (($z &255) >> 6))}]][char64 \
		    [expr {($z & 63) }]]
	
	}
    }
    return $new
}

proc char64 { x } {
    string range "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" $x $x
}

## endsource base64.tcl
