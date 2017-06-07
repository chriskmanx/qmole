# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: String.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
###### String.tcl ######


#
#-----------------------------------------------------------------
#
# trimSpace --  If a STRING contains no embedded newlines, then remove
#  the surrounding whitespace, otherwise just remove trailing whitespace.
#
#  Results:
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc trimSpace { ans } {
    if { ![regexp "\n" $ans] } {
	set ans [string trim $ans "\n \t"]
    } elseif { [regexp "^\[\n\t \](\[^\n\]+)\[\n\t \]\$" $ans junk item] } {
	set ans [string trim $ans "\n \t"]
    } else {
	# set ans [string range $ans 0 [expr {[string length $ans] - 2}]]
	# try to make multiline things start with ans
	set ans \n[string trimleft $ans \n]
    }
    return $ans

    if { [regexp "^\[\n\t \]*(\[^\n\]+)\[\n\t \]*\$" $ans junk item] } {
	set ans [string trim $ans "\n \t"]
    } elseif { [regexp "\n" $ans] } {
	set ans [string trim $ans "\n \t"]
	return "\n$ans"
    }    else {
	set ans [string trimright $ans "\n \t"]
    }
    return $ans
}


#
#-----------------------------------------------------------------
#
# genword --  make a string by copying STRING a total of COUNT times
#
#  Results:string
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc genword { string count } {
    set ans ""
    while { [incr count -1] >= 0 } { append ans $string }
    return $ans
}

## endsource string.tcl
