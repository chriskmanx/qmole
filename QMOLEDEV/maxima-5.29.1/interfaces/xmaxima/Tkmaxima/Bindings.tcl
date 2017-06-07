# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Bindings.tcl,v 1.10 2007-03-23 00:05:06 villate Exp $
#
###### Bindings.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

global NCtextHelp
set NCtextHelp [mc "
Bindings:
<Return>    This sends the current expression (ie where the insert
            cursor is)  for evaluation.
<Linefeed>  (Control-j) This inserts a newline, and is useful
            for entering multiline input.
<Control-k> Kills the current line and puts it in kill ring.
            Successive control-k's append their output together.
<Control-y> Yank out the last kill, Meta-y cycles through previous
            kills.
<Control-g> Interrupts the current computation.
<Alt-p>     Gets the previous input in the history of inputs; if the
            first input is reached, it proceeds to the last input. If
            some characters are written before clicking on Alt-p, only
            history items containing those characters will be considered.
<Alt-n>     Gets the next input in the history of inputs; if the end is
            reached, it proceeds to the first input. If some characters
            are written before clicking on Alt-n, only history items
            containing those characters will be considered.
<Alt-s>     Print again the Maxima input prompt.
"]

proc vMAXSetCNTextBindings {w} {
    # Prevent deleting output fields with BackSpace key
    bind CNtext <Key-BackSpace> {
	if {[lsearch [%W tag names [%W index insert-1c]] output] >= 0} break
    }

    # Disable default keyboard bindings in output fields 
    bind CNtext <Key> {
	if {[lsearch [%W tag names [%W index insert]] output] >= 0} break
    }

    # Keep only default bindings for the cursor movement keys
    foreach Key {<Left> <Right> <Up> <Down> <Next> <Prior> <Home> <End>
	<Shift-Left> <Shift-Right> <Shift-Up> <Shift-Down> <Shift-Home>
	<Shift-End> <Control-Shift-Home> <Control-Shift-End>
	<Control-a> <Control-b> <Control-e> <Control-f> <Control-n> <Control-p>
	<Control-Home> <Control-End> <Meta-less> <Meta-greater> <Meta-b>
	<Meta-f>} {
	bind CNtext $Key "# nothing"
    }
	
    # The "Return" key is bound to command evaluation, except in output tags
    bind CNtext <Return> {
	if {[lsearch [%W tag names [%W index insert]] output] >= 0} {
	    break
	} else {
	    CMeval %W
	    break
	}
    }

    # Special keys (see NCtextHelp above for explanation)
    bind CNtext <Control-g> "CMinterrupt %W "
    bind CNtext <Control-G> "CMinterrupt %W "
    bind CNtext <Control-u> "CNclearinput %W "
    bind CNtext <Control-U> "CNclearinput %W "
    bind CNtext "\)"  "CNblinkMatchingParen %W %A"
    bind CNtext "\]"  "CNblinkMatchingParen %W %A"
    bind CNtext "\}"  "CNblinkMatchingParen %W %A"
    bind CNtext <Control-j> "tkTextInsert %W %A ; openMathAnyKey %W %K  %A"
    bind CNtext <Control-J> "tkTextInsert %W %A ; openMathAnyKey %W %K  %A"
    bind CNtext <Alt-p>  "CNpreviousInput $w -1"
    bind CNtext <Alt-P>  "CNpreviousInput $w -1"
    bind CNtext <Alt-n>  "CNpreviousInput $w 1"
    bind CNtext <Alt-N>  "CNpreviousInput $w 1"
    bind CNtext <Alt-s>  {sendMaxima %W ":s\n" }
    bind CNtext <Alt-S>  {sendMaxima %W ":s\n" }
    bind CNtext <Control-Key-c>  {tk_textCopy %W ;break}
    bind CNtext <Control-Key-C>  {tk_textCopy %W ;break}
    bind CNtext <Control-Key-x>  {tk_textCut %W ;break}
    bind CNtext <Control-Key-X>  {tk_textCut %W ;break}
    bind CNtext <Control-Key-v>  {tk_textPaste %W ;break}
    bind CNtext <Control-Key-V>  {tk_textPaste %W ;break}
}


global maxima_priv
set maxima_priv(doublek) 0

bind OpenMathText <Control-Key-k><Control-Key-k> {
    set maxima_priv(doublek) 1
}
bind OpenMathText <Control-Key-K><Control-Key-K> {
    set maxima_priv(doublek) 1
}

global maxima_priv
if {0} {
    # xmaxima should not be binding the Text class
    if {! [info exists maxima_priv(bindings_added) ] } {
	bind Text <Control-Key-k> "+openMathControlK %W"
	bind Text <B3-Motion> [bind Text <B2-Motion>]
	bind Text <Button-3> [bind Text <Button-2>]

	set maxima_priv(bindings_added) 1
    }
} else {
    bind OpenMathText <Control-Key-k> "+openMathControlK %W"
    bind OpenMathText <Control-Key-K> "+openMathControlK %W"
}


#mike - I'm decreeing windows Cut/Copy/Paste conventions for
# keybindings, and will preobably reserve Alt-key for menu shortcuts.

bind OpenMathText <Control-Key-y> "OpenMathYank %W 0; break"
bind OpenMathText <Alt-Key-y> "OpenMathYank %W 1; break"
bind OpenMathText <Meta-Key-y> "OpenMathYank %W 1; break"

# put the clipboard paste on Control-Shift-y
# event add <<Paste>> <Control-Shift-y>

# Copy
bind OpenMathText <Alt-Key-w> {
    pushCommand %W SaveSelection ""
    if { "[selection own -displayof %W]" == "%W"} {
	pushl [saveText %W sel.first sel.last] killRing
	selection clear -displayof %W
    }
}

bind OpenMathText <Key> {openMathAnyKey %W %K %A}
bind OpenMathText <Alt-Key> {openMathAnyKey %W %K ALT_%A}

# stop the double button click word selection in openMathText..
bind OpenMathText <Double-Button-1> { break; }
bind OpenMathText <Control-c><Key-e> {doInvoke %W insert ; break}

# ok - mark
bind OpenMathText <Control-Key-space> {
    pushCommand %W SetAnchor ""
    %W mark set anchor insert 
}

#
#-----------------------------------------------------------------
#
# binding --   push the current selection on the killRing, and
# if there is no selection, push the region between the anchor and
# the point.
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#

bind OpenMathText <Control-Key-w> {
    pushCommand %W OpenMathTextCut ""
    # in the first case the <<Cut>> event on Text will delete the selection.
    if { [catch { pushl [saveText %W sel.first sel.last] killRing } ] } {
	catch {
	    set range [getRange %W anchor insert]
	    pushl [eval saveText %W $range] killRing
	    eval %W delete $range
	}
    }
}



proc openMathAnyKey { win keysym s  } {
    # puts "$win `$keysym' `$s'"
    if { "$s" != "" } {
	pushCommand $win openMathAnyKey [list $win  $keysym $s]
    }

    if { "$s" != "" && [doInsertp [$win tag names insert]]
	 && ("$s" == "$keysym"  || [regexp -- "\[\n\t \]" "$s" junk] )} {
	setModifiedFlag $win insert
    }
}

#mike this code is impenetrable:
proc OpenMathYank {win level} {
    global maxima_priv
    #puts "doing OpenMathYank $win $level"
    if { $level == 0 } {
	set maxima_priv(currentwin) $win
	pushCommand $win OpenMathYank [list $win $level]
	set maxima_priv(point) insert
	$win mark set beforeyank insert
	$win mark gravity beforeyank left
	eval [peekl killRing "" ]
    } elseif { ![info exists maxima_priv(lastcom,$win)]} {
	#mike this case was not forseen in the code below and
	# it always occurs on the first Yank if nothing has benn Killed
    } elseif { [catch {
	set last $maxima_priv(lastcom,$win)
	set m [lindex [lindex $last 1] 1]
	incr m
	if { [lindex $last 0] == "OpenMathYank" && \
		"$maxima_priv(currentwin)" == "$win" && \
		"$maxima_priv(point)" == "insert"} {
	    set doit 1
	} else {
	    #mike the following was missing, and its 
	    # lack was obscurred by the catch
  	    set doit 0
	}
    } err] || "$doit" == "0"} {
	pushCommand $win Error "" 
    } else {
	set res [peekl killRing _none_ [expr {$m + 1}]]
	if { "$res" == "_none_" } {
	    # this will cause to cycle
	    set m 0
	} else {
	    $win delete beforeyank insert
	    eval $res
	}
	pushCommand $win OpenMathYank [list $win $m]
    }
    catch {$win see insert}
}

proc saveText { win args } {

    if {[catch {$win index [lindex $args 1 ]} endregion]} {return ""}

    set tags [ldelete sel  [$win tag names]]
    set prev [lindex $args 0]

    if { "$prev" == "" } {set prev 0.0 }
    if { "$endregion" == "" } {set endregion end}

    set allar($prev) 1
    set allar($endregion) 1
    foreach v $tags {
	set ranges [tagRanges $win $v  $prev $endregion]
	foreach {begin end} $ranges {
	    lappend start($begin) $v
	    lappend stop($end) $v
	    set allar($begin) 1
	    set allar($end) 1
	
	}
    }
    proc __comp { a b} " return  \[$win compare \$a > \$b \] "
    set all [lsort -command __comp [array names allar]]
    set result ""
    foreach v $all {
	append result "Tins [list [array names currentTags]] [quoteBraces [$win get $prev $v]]\n"
	set prev $v


	if { [info exists start($v)] } {

	    foreach u $start($v) { set currentTags($u) 1}
	}

	if { [info exists stop($v)] } {

	    foreach u $stop($v) { unset currentTags($u) }
	}



	#puts -nonewline "..deleting{$stop($v)} giving {$currentTags}"

	# puts ">>"

    }
    return $result
}



proc openMathControlK { win } {
    global maxima_priv
    if { $maxima_priv(doublek) != 0 } {
	set now [popl killRing ""]
    } else {
	set now ""
    }
    set maxima_priv(doublek) 0
    if { [$win compare insert == "insert lineend" ]  } {
	if { [$win compare insert < end] } {
	    append now "\nTins {[ldelete sel [$win tag names insert]]} {\n}"
	} } else {
	    append now "\n[saveText $win insert {insert lineend}]"
	}
    pushl $now killRing
}


