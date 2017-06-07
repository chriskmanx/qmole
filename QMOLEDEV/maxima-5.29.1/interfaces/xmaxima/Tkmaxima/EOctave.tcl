# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: EOctave.tcl,v 1.3 2011-03-19 23:17:20 villate Exp $
#
###### EOctave.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################


#
#-----------------------------------------------------------------
#
# insertResult_octave --  insert result RES, in text window W,
# into RESULTRANGE.  The command which was sent to octave came
# from THISRANGE.   For plots if a resultRANGE is missing,
# we use a space just after the end of the line of THISRANGE.
# checks if this is plotdata, and if so makes plot win for it.
#
#  Results: none
#
#  Side Effects:  inserts in text or graph in window W.
#
#----------------------------------------------------------------
#

proc insertResult_octave {  w thisRange resultRange res } {
    #puts "res=$res"
    if { [regexp "\{plot\[23\]d" $res] || [regexp "\{plotdf" $res] \
             || [regexp "\{scene" $res] } {
	#puts "its a plot"
	set name [plotWindowName $w [lindex $res 0]]
	set tem [setDesiredDims $w $name $thisRange ]
	eval plot2dData $name $res [getDimensions $w $name]
	ShowPlotWindow $w $name  $thisRange $resultRange $tem
	return 0
    } elseif { "$resultRange" != "" } {
	insertResult $w $resultRange $res
    }
    return 0
}


## endsource eoctave.tcl
