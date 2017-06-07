# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: EHref.tcl,v 1.3 2004-10-13 12:08:57 vvzhy Exp $
#
###### EHref.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################


#
#-----------------------------------------------------------------
#
# eval_href --  Follow a link to another om document
#
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#

proc obsoleteeval_href { program w this nextResult} {
    set arg ""
    foreach v [$w tag names [lindex $this 0]] {
	if { [string first "\{ThrefArg" $v] == 0 } {
	    set arg $v
	    break
	}
    }
    set arglist [getTargTags $w $this]
    if { [llength $arglist] != 1 } {
	return -code error -errorinfo  [concat "[llength $arglist]" [mc "args to href.  Wanted 1, got:"] "$arglist"]
    }
    puts "arglist=$arglist"

    set arg [lindex $arglist 0]
    puts "arg=$arg"
    set list [lrange $arg 1 end]
    set doc [assoc -src $list ""]

    set searchregexp [assoc -searchregexp $list ""]
    set search [assoc -search $list ""]

    puts "doc=$doc"

    if { "$doc" != "" } {
	puts "       OpenMathOpenUrl $doc -commandpanel [omPanel $w]"
	OpenMathOpenUrl $doc -commandpanel [omPanel $w]
    }
    makeLocal  [omPanel $w] textwin
    set ind ""
    if { "$searchregexp" != "" } {
	set ind [ $textwin search -regexp -- $searchregexp 1.0]
    } elseif { "$search" != "" } {
	set ind [ $textwin search -exact -- $search 1.0]
    }
    if { "$ind" != "" } {
	$textwin yview $ind
    }
    return 0
}



## endsource ehref.tcl
