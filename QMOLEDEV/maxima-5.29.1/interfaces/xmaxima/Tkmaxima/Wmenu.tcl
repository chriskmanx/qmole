# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Wmenu.tcl,v 1.8 2004-10-13 12:08:58 vvzhy Exp $
#
###### wmenu.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

# implement a menu bar without toplevel windows.
# wet

proc wmenubar { name  } {
    if { "[string index $name 0]" == "." } {
	frame $name
	# puts "rename $name $name-orig"
	rename $name $name-orig
	set top [winfo toplevel $name]
	oset $top helpwin ""
	proc $name { option args } "wmenubarInternal $name \$option \$args"
	set parent [winfo parent $name]
	# maybe change this to do traversal toward side leaving on..
	oset $name items ""
    } else {
	error [mc "needs a window name arg"]
    }
}


proc eswitch { key lis } {
    foreach {k act} $lis { lappend allowd $k}
    lappend lis default [concat [mc "error"] "$key" [mc "must be one of:"] "$allowd"]
    uplevel 1 switch -- $key  [list  $lis]
}

proc ogetr { win var dflt } {
    set w $win
    while { 1 } {
	if { 0 == [catch { set val [oget $w $var] }] } {
	    return $val
	}
	global [oarray $w]
	# puts w=$w,[array get [oarray $w]]
	set w [winfo parent $w]
	if { "$w" == "" } {return $dflt}
    }
}

proc deleteHelp { win } {
    #mike FIXME: This is being called even if show_balloons = 0
    linkLocal $win helpPending
    if { [info exists  helpPending] } {
	after cancel $helpPending
	unset helpPending
    }
    set top [winfo toplevel $win]
    set helpwin [oget $top helpwin]
    if {$helpwin != "" && [winfo exists $helpwin]} {
	place forget $helpwin
    }
}

proc setHelp {win  help args } {
    # set c [ogetr $win c "cant"]
    if { "$help" == "" } {set help [concat [mc "This is a menu window"] "$win"]}
    set enter ""
    set exit ""
    if  { [catch { set current [$win cget -relief] } ] || "$current" \
	    != "flat" } {
	set enter ""
	set exit ""
    } else {
	set enter "$win configure -relief raised" ;
	set exit "$win configure -relief $current"
    }
    # puts "current=$current"

    bind $win <Enter> "$enter; showHelp $win  {$help} $args"
    bind $win <Leave> "$exit; deleteHelp $win"
}


#
#-----------------------------------------------------------------
#
# showHelp --  for WINDOW show a HELP message using ANCHOR positions.
#  WINDOW may be a window or a rectangle specifier: x,y,wid,height
#  ANCHOR positions may be either n,w,e,s,nw,ne,se,sw,center or
#  one of these followed by two floating point numbers indicating
#  the fraction of the width and height of the window one is away from
#  the upper left x,y of the window.
#  Results: none
#
#  Side Effects: display a window.
#
#----------------------------------------------------------------
#
proc showHelp { win help args } {
    global show_balloons helpwin
    if { $show_balloons == 0 } {
	#mike FIXME: $win is a list not a window
	set top [winfo toplevel [lindex $win 0]]
	set helpwin [oget $top helpwin]
	if {$helpwin != "" && [winfo exists $helpwin]} {
	    place forget $helpwin 
	}
	return
    }
    linkLocal [lindex $win 0] helpPending
    #mike FIXME: $win is a list not a window - needs an eval
    set helpPending [after 1000 [list showHelp1 $win $help $args]]
}

proc showHelp1 { win help args } {
    global  tk_version
    set top [winfo toplevel [lindex $win 0]]
    #    set anchors $args
    #    append anchors "  w  e s ne n sw nw"
    #    set anchors " nw"
    #    set anchors "w e n {nw .2 1.2} {ne .8 1.2} s se"
    #     set anchors "w e n {nw .2 1.2} {ne .8 1.2} s se"
    set anchors "sw w e n {nw .2 1.2} {ne .8 1.2} s se"
    makeLocal $top helpwin
    if { "$helpwin" == "" } {
	set tt $top
	if { "$tt" == "." } {set tt ""}
	set helpwin $tt.balloonhelpwin
	if { ![winfo exists $helpwin] } {
	
	    label $helpwin -width 0 -height 0  -borderwidth 1 \
		    -background beige -padx 4 -pady 4 -justify left
	}
	$helpwin config -relief solid
	
	oset $top helpwin $helpwin
    }
    if { [string first _eval $help ] == 0 } {
	catch { set help [eval [concat list [lindex $help 1]]]}
    }

    $helpwin configure -text $help \
	-wraplength [expr {round(.34 * [winfo width $top])}]
    global anchorPositions
    if { [llength $win] == 5 } {
	desetq "win wx wy wxdim wydim" $win
    }  else {
	set wx [expr {[winfo rootx $win ] - [winfo rootx $top]}]
	set wy [expr {[winfo rooty $win ] - [winfo rooty $top]}]
	set wxdim [winfo width $win]
	set wydim [winfo height $win]
    }
    set nxdim [winfo reqwidth $helpwin]
    set nydim [winfo reqheight $helpwin]
    set topxdim  [winfo width $top]
    set topydim  [winfo height $top]
    global anchorPositions
    foreach an $anchors {
	if {[llength $an] == 3} {
	    desetq "an rx ry" $an
	} else {
	    desetq "rx ry" [lsublis { {0 1.1 } {1 -.1}} $anchorPositions($an)]
	}
	# puts "rx=$rx,ry=$ry"
	set yoff [expr { $ry > 1 ? 8 : $ry < 0 ? -8 : 0 } ]
	desetq "x y" [getPlaceCoords 0 $yoff $rx $ry $an $wx $wy $wxdim $wydim $nxdim $nydim]
	# puts "for $win $an rx=$rx,ry=$ry x=$x,y=$y :[expr {$x >5}],[expr {$y > 5}],[expr {$x+$nxdim < $topxdim}],[expr {$y +$nydim < $topydim}]"
	if { $x > 5 && $y > 5 && $x+$nxdim < $topxdim && \
		$y +$nydim < $topydim } {
	    place forget $helpwin

    	    place $helpwin -x $x -y $y -anchor nw
	    after idle raise $helpwin
	    return
	}
    }
}

proc wmenubarInternal { win  option  lis } {
    # puts "{wmenubarInternal $win $option $lis}"
    set key [lindex $lis 0]
    set lis [lrange $lis 1 end]
    eswitch $option {
	add {
	    set parent [winfo parent $win]
	    if { "$parent" == "."} {set parent ""}
	    set men [assoc -menu $lis $parent.item[llength [oget $win items]]]
 	    bindAltForUnderline $key "wmenuPost $key"
	    frame $men -relief raised -borderwidth 2p
	    setHelp $key [assoc -help $lis] n nw ne
	    rename $men $men-orig
	    set body "wmenuInternal $key \$option \$args"
	    proc $men {option args } $body
	    pack $key -in $win -side left -expand 0 -fill both
	    global [oarray $win]
	    lappend [oloc $win items] $key
	    oset $key menu $men
	    oset $men items ""
	    oset $key parent $win
	    bind $key <Button-1>  {wmenuPost %W}
	    return $men
	}
	configure {
	    return [eval $win-orig configure $key $lis]

	}
	invoke {
	    set w [lindex [oget $win items] $key]
	    wmenuPost $w
	}
	cget {
	    return [eval $win cget $key $lis]
	}
    }
}

proc getSomeOpts { opts lis } {
    set answer ""
    foreach {ke val } $lis {
	if { [lsearch $opts $ke] >= 0  } {
	    lappend answer $ke $val
	}
    }
    return $answer
}

proc excludeSomeOpts { opts lis } {
    set answer ""
    foreach {ke val } $lis {
	if { [lsearch $opts $ke] < 0  } {
	    lappend answer $ke $val
	}
    }
    return $answer
}

proc lsublis { subs lis } {
    foreach v $subs {
	set key [lindex $v 0]
	while { [set i [lsearch $lis $key]] >= 0 } {
	    if { [llength $v] > 1 } {
		set lis [lreplace $lis $i $i [lindex $v 1]]
	    } else {
		set lis [lreplace $lis $i $i]
	    }
	}
    }
    return $lis
}

proc wmenuInternal {win option  olist } {
    set key [lindex $olist 0]
    set lis [lrange $olist 1 end]
    makeLocal $win menu parent
    makeLocal $menu items
    eswitch $option {
	add {
	    if { [catch {set counter [oget $menu counter] }] }   {
		set counter 0
	    }
	    oset $menu counter [incr counter]
	    # set new to be the new menu item window
	    # set com to be the command for 'invoke' to invoke.
	    set opts [excludeSomeOpts "-textvariable -image -label -underline -help" $lis]
	    set labopts [lsublis {{-label -text}} \
		    [getSomeOpts "-image -label -textvariable -underline" $lis]]
	    append labopts " -justify left -anchor w -padx 2"
	    eswitch $key {
		radio {
		    set new $menu.fr$counter
		    frame $new -borderwidth 1
		    # puts "new=$new"
		    apply label $new.label $labopts
		    pack $new.label -side left -fill x
		    set opts [lsublis {{-radiovariable -textvariable}} $opts]
		    apply radiobutton $new.radio $opts
		    pack $new.radio -side right -anchor e
		    set com "$new.radio invoke"
		}
		check {
		    set new $menu.fr$counter
		    frame $new -borderwidth 1
		    # puts "new=$new"
		    apply label $new.label $labopts
		    pack $new.label -side left
		    set opts [lsublis {{-checkvariable -textvariable}} $opts]
		    apply checkbutton $new.check $opts
		    pack $new.check -side right
		    # puts "$var --> $val"
		    set com "$new.check invoke"
		}
		command {
		    set com [assoc -command $lis]
		    set new $menu.fr$counter
		    frame $new -borderwidth 1
		    apply label $new.label $labopts
		    pack $new.label -in $new -side left
		    # puts "bind $new.label <Button-1> $com"
		    bind $new.label <Button-1> $com
		    bind $new <Button-1> $com
		}
		window {
		    set new [assoc -window $lis]
		    set com [assoc -command $lis list]
		}
		entry {
		    set new $menu.fr$counter
		    frame $new -borderwidth 1
		    apply label $new.label $labopts
		    set opts [lsublis {{-entryvariable -textvariable}} $opts]
		    apply entry $new.entry $opts
		    pack $new.label -side top -in $new -anchor w
		    pack $new.entry  -side top -in $new
		    set com "focus $new.entry"
		}
		separator {
		    set new $menu.sep$counter
		    frame $new -height 4
		    propagate $new 0
		    set com ""
		}
		
	    }
	    bindAltForUnderline $new.label "$menu invoke $new"
	    pack $new -in $menu -side top -fill both -expand 0
	    oset $menu items [lappend items $new]
	    oset $menu command$new $com
	    setHelp $new [assoc -help $lis] w e
	    return $new
	}
	configure {
	    return [eval $win configure $key $lis]
	}
	invoke {
	    makeLocal $menu items
	    if { ![winfo exists $key] }  {
		# it is an index
		set key [lindex $items $key]
	    }
	    eval [oget $menu command$key]
	    return
	}
	post {
	
	    place $menu -anchor nw -relx 0 -rely 0 -bordermode outside -in $win
	    bind $menu <Leave> "place forget $menu"
	    focus $menu
	    #bind $menu <FocusIn> "puts focus in"
	    #bind $menu <FocusOut> "puts {leave for focus  menu}"
	    raise $menu
	}
    }
}

proc wmenuPost { win } {
    makeLocal $win parent menu
    bind $menu <Leave> "place forget $menu"
    place $menu -anchor nw -relx 0 -rely 1.0 -bordermode outside -in $win
    raise $menu
}

proc bindAltForUnderline { item command } {
    set ind -1
    catch { set ind [$item cget -underline] }
    if { $ind >= 0 } {
	set letter [string index [$item cget -text] $ind]
	set to [winfo toplevel $item]
	bind $to <Alt-Key-$letter> $command
    }
}

proc showSomeEvents { win } {
    foreach v { Enter FocusIn FocusOut Visibility Leave} {  
	bind $win <$v> "puts {$win $v %x %y}"
    }
}

global anchorPositions
array set anchorPositions {
    n {.5 0} nw { 0 0 } se {1 1} e {1 .5} center {.5 .5}
    s { .5 1} sw { 0 1} w { 0 .5} ne { 0 1}
}

proc getPlaceCoords { x y relx rely anchor xIn yIn xdimIn ydimIn xdim ydim } {
    global anchorPositions

    # puts "xIn=$xIn,yIn=$yIn,xdimIn=$xdimIn,ydimIn=$ydimIn,xdim=$xdim,ydim=$ydim"
    set x1 [expr {$x + $xIn+$relx * $xdimIn}]
    set y1 [expr {$y + $yIn+$rely * $ydimIn}]
    desetq "fx1 fy1" $anchorPositions($anchor)
    set atx [expr {$x1 - $fx1*$xdim}]
    set aty [expr {$y1 - $fy1*$ydim}]

    return [list $atx $aty]
}

## endsource wmenu.tcl
