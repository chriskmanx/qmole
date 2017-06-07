# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Macros.tcl,v 1.6 2006-06-29 13:09:58 villate Exp $
#
###### Macros.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################
#
#-----------------------------------------------------------------------
# desetq lis1 lis2 -- sets the values for several variables
#
# Result: each variable name in LIS1 is defined with a value in LIS2
#-----------------------------------------------------------------------
#
proc desetq {lis1 lis2} {
    set i 0
    foreach v $lis1 {
	uplevel 1 set $v [list [lindex $lis2 $i]]
	incr i
    }
}
######  Options parsing functions ######################################
#
#-----------------------------------------------------------------------
# assoc key lis args -- find value of option KEY in options list LIS
#
# Result: value of option or first element of list ARGS
#-----------------------------------------------------------------------
#
proc assoc { key lis args } {
    foreach { k val } $lis { if { "$k" == "$key" } { return $val} }
    return [lindex $args 0]
}
#
#-----------------------------------------------------------------------
# delassoc key lis -- remove option KEY from options list LIS
#
# Result: an options list without option KEY
#-----------------------------------------------------------------------
#
proc delassoc { key lis } {
    set new {}
    foreach { k val } $lis {
	if { "$k" != "$key" } { lappend new $k $val}
    }
    return $new
}
#
#-----------------------------------------------------------------------
# putassoc key lis value -- set VALUE for option KEY in options list LIS
#
# Result: an option list with KEY set to VALUE
#-----------------------------------------------------------------------
#
proc putassoc {key lis value } {
    set done 0
    foreach { k val } $lis {
	if { "$k" == "$key" } {
	    set done 1
	    set val $value
	}
	lappend new $k $val
    }
    if { !$done } { lappend new $key $value }
    return $new
}
######  End options parsing functions #################################
#
#-----------------------------------------------------------------------
# intersect lis1 lis2 -- find common elements of two lists
#
# Result: a list of values found in LIS1 and LIS2
#-----------------------------------------------------------------------
#
proc intersect { lis1 lis2 } {
    set new ""
    foreach v $lis1 { set there($v) 1 }
    foreach v $lis2 { if { [info exists there($v)] } { lappend new $v }}
    return $new
}
#
#-----------------------------------------------------------------------
# ldelete item lis --  remove all copies of ITEM from LIS
#
# Result: new list without ITEM
#-----------------------------------------------------------------------
#
proc ldelete { item list } {
    while { [set ind [lsearch $list $item]] >= 0  } {
	set list [concat [lrange $list 0 [expr {$ind -1}]] [lrange $list [expr {$ind +1}] end]]
    }
    return $list
}
#
#-----------------------------------------------------------------------
# apply f a1 .. am [list u1 .. un] -- apply a function with arguments
#        A1 .. Am and all the elements U1 .. Un in a list
#
# Result: command f is evaluated, in the scope from where APPLY was issued
#-----------------------------------------------------------------------
#
proc apply {f args } {
    set lis1 [lrange $args 0 [expr {[llength $args] -2}]]
    foreach v [lindex $args end] { lappend lis1 $v}
    set lis1 [linsert $lis1  0 $f]
    uplevel 1 $lis1
}
## endsource macros.tcl
