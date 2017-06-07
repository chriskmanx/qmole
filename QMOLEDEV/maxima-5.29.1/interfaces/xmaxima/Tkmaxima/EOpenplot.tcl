# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: EOpenplot.tcl,v 1.7 2011-03-19 23:15:44 villate Exp $
#
###### EOpenplot.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################



#
#-----------------------------------------------------------------
#
# eval_openplot --  invoke OPENPLOT on the substring of Window given
# by thisRange, and substitute the result into resultRange, if the
# latter is not the empty list.   If it is, then the window is placed
# on the next line from this command.
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#

proc eval_openplot { program w thisRange resultRange } {


    set tem [eval $w get $thisRange]
    lappend tem -windowname $name
    foreach v [getDimensions $w $name] { lappend tem $v }
    set allowed "plot2d plotdf plot3d scene"
    set f [lindex $tem 0]
    if { [lsearch $allowed $f] >= 0 } {
	apply $f [lrange $tem 1 end]
	ShowPlotWindow $w $name $thisRange $resultRange $desired
    } else {
	error [concat "$f" [mc "not allowed, only"] "{$allowed}"]
    }
    set name  [plotWindowName $w $f]
    set desired [setDesiredDims $w $name $thisRange ]
    return 0
}


#
#-----------------------------------------------------------------
#
# plotWindowName --  checks preferences to see if separate or multiple
#  or nontoplevel windows are desired, and chooses a name accordingly.
#  in the first two cases it also assures that the toplevel window exists.
#
#  Results: window name
#
#  Side Effects:  possibly make a new toplevel window.
#
#----------------------------------------------------------------
#
proc plotWindowName { w command } {
    upvar #0 maxima_default(plotwindow) plot
    upvar #0 maxima_priv(plot,count) count
    set name ""

    if { "$command" == "scene" } {
        set name ".plotfr"	
        if { [winfo exists $name ] } {
            after cancel PlayStep $name.plot.menubar.play
            vtkCommand DeleteAllObjects
            destroy $name
            clearLocal $name.plot
        }
        toplevel $name
        if { "[info proc setIcon]" != "" } {
            after 1000 setIcon $name
        }
    } else {
        if { ![info exists plot] || "$plot" == "embedded" } {
            linkLocal $w counter
            if { ![info exists counter] } {set counter 0}
            return $w.plot[incr counter]
        }
        set name ".plotfr"	
        if { "$plot" == "multiple" } {
            if { ![info exists count] } {
                set count 1
            } else {
                incr count
            }
            append name $count
        }
        if { ![winfo exists $name ] } {
            toplevel $name
            set h [expr {round ([winfo screenheight $name]*.6) }]
            set wid [expr round ($h * 1.2) ]
            set r1 [expr {round(10+rand()*30)} ]
            set r2 [expr {round(10+rand()*30)} ]
            wm geometry $name ${wid}x${h}+${r1}+${r2}
            if { "[info proc setIcon]" != "" } {
                after 1000 setIcon $name
            }   
        }
    }
    append name .plot
    return $name
}


proc whereToPutPlot { w thisRange resultRange } {
    if { "$resultRange" != "" } {
	eval $w  delete $resultRange
	set at [lindex $resultRange 0]
	$w insert $at " " { Tresult}
	set at [$w index "$at + 1char"]
    } else {
	set at "[lindex $thisRange 1] lineend + 1 chars"
    }
    return $at
}


proc setDesiredDims { w name range } {
    #puts "setDesiredDims  $w $name $range"
    foreach v [getTagsMatching $w "^(width|height):" $range] {
	set tem [split $v :]
	lappend ans [lindex $tem 0]Desired [lindex $tem 1]
    }
    if { [info exists ans] } {
	oarraySet $name $ans
	return $ans
    }
    return ""
}

proc getDimensions { w name } {
    # puts "getDimensions  $w $name"
    set parent [winfo parent $w]
    set scrollwidth 15
    catch { set scrollwidth [ [winfo parent $parent].scroll cget -scrollwidth] }
    set width [winfo width $w]
    set height [winfo height $w]
    #set width [getPercentDim [oget $name widthDesired] width $w]
    catch {set width [getPercentDim [oget $name widthDesired] width $w] }
    catch {set height [getPercentDim [oget $name heightDesired] height $w] }

    set width [expr {round ($width-4) }]
    set height [expr {round ($height-4)}]
    #puts "using width,height=$width,$height"

    if { $width <0 } {
	set width [expr {[oget $parent width] - 2*$scrollwidth}]
	set height [expr {round(.85*[oget $parent height])}]
    }
    return " -width $width -height $height"
}


proc insertResult_openplot {w args } {
    puts "insert=[$w index insert]" 
}

proc ShowPlotWindow { w name thisRange resultRange desired } {
    if { "[winfo toplevel $w]" != "[winfo toplevel $name]" } {
	$name config -relief sunken -borderwidth 2
	pack $name -expand 1 -fill both
	raise [winfo toplevel $name ]
	return
    }
    oarraySet $name $desired
    set at [whereToPutPlot $w $thisRange $resultRange]
    set col [lindex [split $at .] 1]

    if { $col > 0 } {
	$w insert $at "\n \n" "$name"
	set at [$w index "$at +1char"]
    }
    # compute where we will try to display.
    # try to leave top of window where it is, but if not
    # scroll lines up just the amount necessary to make the
    # window visible.

    set h1 [winfo height $w]
    set h2 [oget  $name height]
    set begin [$w index @0,0]
    set ind $at
    set dl [$w dlineinfo $ind]
    set y0 [lindex $dl 1]
    set prev ""
    if { "$y0" != "" } {
	while { [$w compare $begin <= $ind] } {
	    set dl [$w dlineinfo $ind]

	    if { "$dl" == "" } { break }
	    if { $y0 - [lindex $dl 1] + $h2 +5 < $h1  } {
		set prev $ind
		set ind [$w index "$ind - 1 line" ]
	    } else {
		break
	    }
	}
    }

    bind $name <Destroy> "catch {$w yview [$w index @0,0] } ; eval $w delete \[$w tag  nextrange $name 0.0 \]"

    if { "$prev" != "" } { set ind $prev }
    $w insert $at " " "$name center"
    $w window create  $at+1char -window $name
    $w tag add "center $name" $at "$at+2char"
    update
    $w yview $ind
    # somehow the single button click gets run positioning the cursor
    # near where the
    after 1 $w mark set insert [$w index insert]
    return $ind
}

## endsource eopenplot.tcl
