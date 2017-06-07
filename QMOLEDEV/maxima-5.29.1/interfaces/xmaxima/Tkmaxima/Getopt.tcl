# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Getopt.tcl,v 1.4 2004-10-13 12:08:57 vvzhy Exp $
#
###### Getopt.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

#####sample option list.   Error will be signalled if "Required" option
##### not given.
#set dfplotOptions {
#    {xdot Required {specifies dx/dt = xdot.  eg -xdot "x+y+sin(x)^2"} }
#    {ydot Required {specifies dy/dt = ydot.  eg -ydot "x-y^2+exp(x)"} }
#    {xradius 10 "Width in x direction of the x values" }
#    {yradius 10 "Height in y direction of the y values"}
#}


#
#-----------------------------------------------------------------
#
# optLoc --  if $usearray is not 0, then the OPTION is stored
# in a hashtable, otherwise in the variable whose name is the
# same as OPTION.
#  Results: a form which when 'set' will allow storing value.
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc optLoc { op ar }  {
    #  puts "$ar,[lindex $op 0]"
    # puts "return=$ar\([lindex $op 0]\)"
    if { "$ar" == 0 } {
	return [lindex $op 0]
    } else {
	#puts "$ar\([lindex $op 0]\)"
	return "$ar\([lindex $op 0]\)"
    }

}


#
#-----------------------------------------------------------------
#
# getOptions --  given OPTLIST a specification for the options taken,
# parse the alternating keyword1 value1 keyword2 value2 options_supplied
# to make sure they are allowed, and not just typos, and to supply defaults
# for ones not given.   Give an error message listing options.
# a specification is  { varname default_value "doc string" }
# and optlist, is a list of these.   the key should be -varname
#
#  -debug 1 "means print the values on standard out"
#  -allowOtherKeys 1 "dont signal an error if -option is supplied but not in
#                     the list"
#  -usearray "should give a NAME, so that options are stored in NAME(OPTION)
#  -setdefaults "if not 0 (default is 1) do `set OPTION dflt' for all options"
# If a key is specified twice eg.  -key1 val1 -key1 val2, then the first
# value val1 will be used
#  Results:
#
#  Side Effects: set the values in the callers environment
#
#----------------------------------------------------------------
#

proc getOptions { optlist options_supplied args } {
    # global  getOptionSpecs

    set ar [assoc -usearray $args 0]
    set help [assoc -help $args ""]
    if { "$ar" != "0" } { global $ar }
    set debug [assoc -debug $args 0]
    set allowOtherKeys [assoc -allowOtherKeys $args 0]
    set setdefaults [assoc -setdefaults $args 1]
    set supplied ""

    foreach {key val } $options_supplied {
	if { [info exists already($key)] } { continue }
	set already($key) 1
	set found 0
	foreach op $optlist {
	    if { "$key" == "-[lindex $op 0]" } {
		uplevel 1 set [optLoc $op $ar] [list $val]
		
		append supplied " [lindex $op 0]"
		set found 1
		break
	    }
	}
	set caller global

	if { $found == 0  && !$allowOtherKeys } {
	    catch {set caller [lindex [info level -1] 0]}
	    error [concat "`$caller'" [mc "does not take the key"] "`$key':\n[optionHelpMessage $optlist]\n$help"]

	}
    }
    foreach op $optlist {
	if { [lsearch $supplied [lindex $op 0]] < 0 } {
	    if { "[lindex $op 1]" == "Required" } {
		catch {set caller [lindex [info level -1] 0]}	
		error [concat "`-[lindex $op 0]'" [mc "is required option for"] "`$caller':\n[optionHelpMessage $optlist]"]
	    }
	    if { $setdefaults }  {

		uplevel 1 set [optLoc $op $ar] [list [lindex $op 1]]
	    }
	}
	# for debugging see them.
	# if { $debug } {   uplevel 1 puts "[optLoc $op $ar]=\$[optLoc $op $ar]"}
	if { $debug } {   puts "[optLoc $op $ar]=[safeValue [optLoc $op $ar] 2]"}
	
    }
}

proc getOptionDefault { key optionList } {
    foreach v $optionList {
	if { "[lindex $v 0]" == "$key" } { return [lindex $v 1]}
    }
    return ""
}

proc assq {key list {dflt ""}} {
    foreach v $list { if { "[lindex $v 0]" == "$key" } { return $v }}
    return $dflt
}

proc safeValue { loc level} {
    if { ![catch { set me [uplevel $level set $loc] } ] } {
	return $me
    }  else {
	return "`unset'"
    }
}



proc optionFirstItems { lis } {
    set ans ""
    foreach v $lis { append ans " [list [lindex $v 0]]" }
    return $ans
}

proc optionHelpMessage { optlist } {
    set msg ""
    foreach op $optlist  {	
	append msg \
		" -[lindex $op 0] \[ [lindex $op 1] \] --[lindex $op 2]\n"
    }
    return $msg
}


#
#-----------------------------------------------------------------
#
# setSplittingOptionsRest --  takes ARGLIST and splits it into
# two lists, the first part it stores in KEYPAIRS and the second in REST
#
#
#  Results: none
#
#
#  Side Effects: sets the variables in the local frame passed to KEYPAIRS
#
#----------------------------------------------------------------
#	
proc setSplittingOptionsRest {  keypairs rest arglist } {
    upvar 1 $keypairs keys
    upvar 1 $rest res
    set i 0
    while { 1 } {
	if { $i >= [llength $arglist] } { break }
	if { "[string range [lindex $arglist $i] 0 0]" == "-" } {
	    incr i 2
	} else {
	    break
	}
    }
    set keys [lrange $arglist 0 [expr $i -1]]
    set res [lrange $arglist  $i end]
}



## endsource getopt.tcl
