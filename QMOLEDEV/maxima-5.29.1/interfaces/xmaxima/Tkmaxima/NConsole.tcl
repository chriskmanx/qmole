# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: NConsole.tcl,v 1.10 2006-12-04 10:45:46 villate Exp $
#
###### NConsole.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################


proc mkConsole { fr program } {
    catch { destroy $fr }
    global NCtextHelp
    frame $fr
    pack $fr -expand 1 -fill both
    set w $fr.text
    label [set msg $fr.label]  -height 1 -relief sunken \
	    -textvariable maxima_priv(load_rate)

    oset $w program $program
    oset $w prompt "% "
    text $w
    bind $w <Configure> "resizeSubPlotWindows $w %w %h"

    $w tag configure input -foreground blue
    # -relief sunken -borderwidth 1
    bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

    global maxima_priv

    if { ![regexp -- $maxima_priv(sticky) input] } {
	append maxima_priv(sticky) {|^input$}
    }

    CNinsertPrompt $w
    $w mark gravity lastStart left
    pack $w -side top -expand 1 -fill both
    pack $msg -side bottom -expand 0 -fill x
    raise $w
}

proc CNinterrupt { w } {
    sendInterrupt [oget $w program]
}

proc CNclearinput { w } {
    if { [$w compare lastStart < insert] } {
	pushl "[saveText $w lastStart insert ]" killRing
	$w delete lastStart insert
    }
}


proc CNinsertPrompt { w } {
    set prompt [oget $w prompt]
    $w mark set insert end
    if { [$w compare insert > "insert linestart"] } {
	$w insert insert \n
    }
    $w insert insert "$prompt" prompt
    $w mark set lastStart [$w index "end -1char"]
    $w see end
    #puts [$w dump -all "[$w index end] -2 lines" end]
}

proc CNeval { w } {
    linkLocal $w inputs inputIndex
    set prev ""
    if { [$w compare insert < lastStart] } {
	set this [thisRange $w input insert]
	if { "$this" != "" } {
	    set code [eval $w get $this]
	    set prev [string trimright [$w get lastStart end] \n]

	    $w delete lastStart end
	    $w insert lastStart $code input
	}
    }
    set expr [string trimright [$w get lastStart end] \n]
    $w tag add input lastStart end
    lappend inputs $expr
    set inputIndex [expr {[llength $inputs] - 1}]
    set tag ""
    #puts "sendind <$expr>"
    set res [sendOneWait [oget $w program] $expr]
    message "received result"

    if { "$res" != "" } {
	if { ![regexp \n $res] } {
	    set tag center
	    set res "\n $res"
	}

	if { [regexp "\{plot\[23\]d" $res] } {
	    linkLocal $w counter
	    #puts res=$res
	    if { ![info exists counter] } {set counter 0}
	    set name $w.plot[oset $w counter [expr {1 + [oget $w counter]}]]
	    eval plot2dData $name $res [getDimensions $w $name]
	    set e [$w index end]
	    set view [ShowPlotWindow $w $name  "$e $e" "$e $e"  ""]
	    append view " -1 line"
	} else {
	    $w insert end $res "$tag result"
	}

    }
    CNinsertPrompt $w
    if { [info exists view] } {$w yview $view }
    if { "$prev" != "" }  {$w insert insert $prev}
}

proc CNpreviousInput { w direction } {
    linkLocal $w  inputIndex matching
    if { [catch {makeLocal $w inputs}] } { return }
    if { [$w compare insert < lastStart ] } { return }

    set last [lindex [peekLastCommand $w] 1]
    if {  ("[lindex $last 2]" != "ALT_p" && "[lindex $last 2]" != "ALT_n") || \
	      ![info exists inputIndex] } {
	set inputIndex [expr {$direction < 0 ? [llength $inputs] : -1}]
	set matching [string trim [$w get lastStart end] " \n"]
    }
    if {[info exists $matching]} {
	lappend inputs $matching
    }
    set n [llength $inputs]
    set j 0
    set matchRegexp "[quoteForRegexp $matching]"
    while {[incr j] <= $n } {
	set inputIndex [expr {($inputIndex + $direction+ $n)%$n}]
	# [string match "$matching*" [lindex $inputs $inputIndex]]
    	if { [regexp -- $matchRegexp [lindex $inputs $inputIndex]] } {
	    $w delete lastStart end
	    $w insert insert [lindex $inputs $inputIndex]
	    $w see insert
	    break
	}
    }
}

proc CNblinkMatchingParen { win ch } {
    $win tag delete blink
    if { [string first $ch "\}\)\]"] >= 0 } {
	set tem [$win get "@0,0" "insert"]
	set ind [matchingParen $tem]
	if { "$ind" != "" } {
	    set ind [expr {[string length $tem] - $ind}]
	    catch { after cancel [oget $win blinkAfter] }
	    set i [$win index "insert -$ind chars"]
	    $win tag add blink "$i" "$i +1char"
	    $win tag configure blink -foreground red
	    oset $win blinkAfter [after 1000 $win tag delete blink]
	}
    }
}


#
#-----------------------------------------------------------------
#
# matchingParen --  Return index of STRING which a close paren
#   would match if added to end.
#  Results: index
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc matchingParen { s1  } {
    set s $s1
    set ind [string length $s]
    set count -1
    regsub -all "\[\01\02\]" $s x s
    regsub -all "\[\]\}\)\]" $s "\01c" s
    regsub -all "\[\[\{\(\]" $s "\01o" s
    set lis [split $s \01]
    set n [llength $lis]
    while { [incr n -1] > 0 } {
	set v [lindex $lis $n]
	incr ind -[string length $v]
	set c [string index $v 0]
	if { "$c" == "c" } {
	    incr count -1
	} else {
	    incr count
	}
	if { $count == 0 } {
	    return $ind
	}
    }
}

## endsource nconsole.tcl
