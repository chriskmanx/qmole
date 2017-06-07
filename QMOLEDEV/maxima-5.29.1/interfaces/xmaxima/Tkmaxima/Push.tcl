# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Push.tcl,v 1.4 2003-01-22 02:59:02 mikeclarkson Exp $
#
###### push.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################



#
#-----------------------------------------------------------------
#
# pushl --  push VALUE onto a stack stored under KEY
#
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#

global __pushl_ar
proc pushl { val key  } {
    global __pushl_ar
    append __pushl_ar($key) " [list $val]"
}


#
#-----------------------------------------------------------------
#
# peekl --  if a value has been pushl'd under KEY return the
# last value otherwise return DEFAULT.   If M is supplied, get the
# M'th one pushed... M == 1 is the last one pushed.
#  Results:  a previously pushed value or DEFAULT
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc peekl {key default {m 1}} {
    global __pushl_ar
    if {![info exists __pushl_ar($key)]} {
	return $default
    } elseif { [catch { set val [set __pushl_ar($key) ] } ] } {
	return $default
    } else {
	set n [llength $val]
	if { $m > 0 && $m <= $n } {
	    return [lindex $val [incr n -$m]]
	} else {
	    return $default
	}
    }
}



#
#-----------------------------------------------------------------
#
# popl --  pop off  last value stored under KEY, or else return DFLT
#
#  Results: last VALUE stored or DEFAULT
#
#  Side Effects: List stored under KEY becomes one shorter
#
#----------------------------------------------------------------
#
proc popl { key  dflt} {
    global __pushl_ar

    if { [catch { set val [set __pushl_ar($key) ] } ] } {
	return $dflt
    } else {
	set n [llength $val]
	set result [lindex $val [incr n -1]]
	
	if { $n > 0 } {
	    set __pushl_ar($key) [lrange $val 0 [expr {$n -1}]]
	} else {
	    unset __pushl_ar($key)
	}
	return $result
    }
}


#
#-----------------------------------------------------------------
#
# clearl --  clear the list stored under KEY
#
#  Result: none
#
#  Side Effects:  clear the list stored under KEY
#
#----------------------------------------------------------------
#
proc clearl { key } {
    global __pushl_ar
    catch { unset __pushl_ar($key) }
}



## endsource push.tcl
