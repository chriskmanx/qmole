# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Plotting.tcl,v 1.3 2002-09-07 23:20:49 mikeclarkson Exp $
#
###### plotting.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################


global axisGray
if { "[winfo screenvisual .]" == "staticgray" } {
    set axisGray black
} else  {
    set axisGray gray60
}

global writefile
set writefile  "Save"
# make printing be by ftp'ing a file..

if {[catch { set doExit }] } { set doExit ""}


## endsource plotting.tcl
