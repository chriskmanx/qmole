# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Readdata.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
###### Readdata.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################



#
#-----------------------------------------------------------------
#
# readDataTilEof --  read data from CHANNEL appending to VAR
# allowing no more than TIMEOUT milliseconds between reads.
#
#  Results:  1 on success, and -1 if it fails or times out.
#
#  Side Effects: CHANNEL will be closed and the global variable VAR will
#  be set..
#
#----------------------------------------------------------------
#
proc readDataTilEof { channel var timeout } {
    global readDataDone_ _readDataData
    global readDataDone_
    upvar 1 $var variable
    set _readDataData ""
    set readDataDone_ 0
    set $var ""

    set after_id [after $timeout "set readDataDone_ -1"]
    fconfigure $channel -blocking 0
    fileevent $channel readable \
	    [list readDataTilEof1 $channel _readDataData $timeout $after_id]

    myVwait readDataDone_
    after cancel $after_id
    catch { close $channel}
    set res $readDataDone_
    if {$res > 0 } { append variable $_readDataData }
    return $res
}

proc readDataTilEof1 { channel var timeout after_id} {
    global readDataDone_  $var

    set new  [read $channel]
    append $var $new

    if { [eof $channel] } {
	set readDataDone_ 1
	close $channel
    } else {
	after cancel $after_id
        after $timeout "set readDataDone_ -1"
    }
}


## endsource Readdata.tcl
