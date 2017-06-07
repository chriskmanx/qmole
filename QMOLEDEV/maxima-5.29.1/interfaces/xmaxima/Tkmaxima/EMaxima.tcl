# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: EMaxima.tcl,v 1.4 2011-03-19 23:16:41 villate Exp $
#
###### EMaxima.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################


#
#-----------------------------------------------------------------
#
# insertResult_maxima --  insert result RES, in text window W,
# into RESULTRANGE.  The command which was sent to maxima came
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

proc insertResult_maxima {  w thisRange resultRange res } {
    set program maxima

    #    puts <lengthres=[llength $res],thisRange=$thisRange,resultRange=$resultRange>

    if { 0 == [string compare "$res" "cant connect"] } {
	bgerror [concat [mc "unable to call"] "$program"]
    }
    if { [regexp "\{plot\[23\]d" $res] || [regexp "\{plotdf" $res] \
             || [regexp "\{scene" $res] } {
	#puts "its a plot"
	set name [plotWindowName $w [lindex $res 0]]
	eval plot2dData $name $res [getDimensions $w $name]
	set desired [setDesiredDims $w $name $thisRange ]
	ShowPlotWindow $w $name  $thisRange $resultRange $desired
	return 0
    }

    if { "$resultRange" != "" }   {
	set name $w.plot[oset $w counter [expr {1 + [oget $w counter]}]]
	insertResult $w $resultRange $res
	
    }
    return 0
}

## endsource emaxima.tcl
