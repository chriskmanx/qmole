# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: OpenMath.tcl,v 1.17 2011-03-15 01:13:22 villate Exp $
#
proc genSample { x n } {
    set sample $x
    set m 1
    while { 1 } {
	if { $m >= $n } { return $sample }
    	if { [set tem [expr {2*$m}]] <= $n } {
	    append sample $sample
	    set m $tem
	} else {
	    return [append sample [genSample $x [expr {$n - $m}]]]
	}
    }
}


# font measuring is very slow so we cache the result of measuring a line
# of x's.
proc fontMeasure { font size } {
    global  maxima_priv
    set ll $maxima_priv(linelength)
    if { ![catch {set answer [set $maxima_priv($font,$size,$ll)]} ] } { return $answer}
    set sample [genSample x $ll]
    set  maxima_priv($font,$size,$ll)  [font measure [list $font $size] $sample]
    return $maxima_priv($font,$size,$ll)
}

proc getDefaultFontSize { width } {
    global fixedFont
    set answer "10 480"
    catch {
	set wid1 [fontMeasure $fixedFont 10]
	set guess [expr {round($width/double($wid1) * 10.0)}]
	while { [fontMeasure $fixedFont $guess] < $width && $guess <= 14 } {
	    incr guess
	}
	incr guess -1
	while { [fontMeasure $fixedFont $guess] > $width } { incr guess -1 }
	set answer   [list $guess [fontMeasure $fixedFont $guess]]
    }
    return $answer

}

proc getMaxDimensions { } {
    global embed_args
    set dims "800 600"
    if { [catch { set dims "$embed_args(width) $embed_args(height)" } ] } {
	set dims "[expr round(.85* [winfo screenwidth .])] [expr round(.9* [winfo screenheight .])]"} else {
	    set dims "[getPercentDim [lindex $dims 0] width .] [getPercentDim [lindex $dims 1] height .]"
	}
    return $dims
}

proc getPercentDim { dim direction win } {
    if { [regexp {([0-9]+)%} $dim junk val] } {
	set dim [winfo $direction $win]
	catch { set dim [expr {round($val * $dim * .01)}] }
	return $dim
    }
    return $dim
}

proc computeTextWinDimensions { win width height } {
    # leave room for scroll bar
    global fixedFont maxima_priv
    # desetq "fsize wid" [getDefaultFontSize [expr {$width -15}]]
    set wid $width
    # set fixedFont [xHMmapFont font:fixed:normal:r:3]
    set fsize [xHMfontPointSize $fixedFont]

    set lh [expr {$fsize +1}]
    catch {   set lh [font metrics $fixedFont -linespace] }
    oset $win fixedFont $fixedFont
    oset $win fontSize $fsize
    oset $win width $width
    oset $win width_chars $maxima_priv(linelength)
    set hei [expr {round($height/$lh)}]
    oset $win height_chars $hei
    oset $win height [expr {$hei * $lh}]
    oset $win lineheight $lh
}



proc setFontOptions { fsize }     {
    global maxima_priv

    global _fixed_default _prop_default fontSize
    set helvetica $_prop_default
    set courier $_fixed_default

    global buttonfont entryfont labelfont fixedtextfont
    set  buttonfont [font create -family $helvetica -size $fsize]
    set  labelfont [font create -family $helvetica -size $fsize]
    set  fixedtextfont [font create -family $courier -size $fsize]
    set  entryfont [font create -family $courier -size $fsize]

return

    if { $fsize > 10 } { set fsize 12 }
    if { $fsize == 8 } { set entrysize 10 } else {set entrysize $fsize }
    #puts "fsize=$fsize"
    catch {
	#mike FIXME: these are broken for windows
	set  buttonfont [font create -family Helvetica -size $fsize]
	set  labelfont [font create -family helvetica -size $fsize]
	set  fixedtextfont [font create -family courier -size $fsize]
	set  entryfont [font create -family courier -size $fsize]

	#mike: maxima should not be playing with these
	# option add *Button.font $buttonfont
	# option add *Label.font $labelfont
	# option add *Entry.font $entryfont

	option add  *Dialog.msg.wrapLength 500

    }

}
proc omPanel { w args } {
    global buttonfont entryfont labelfont maxima_priv

    set top [winfo toplevel $w]
    linkLocal $top omPanel
    if { [info exists omPanel] } {return $omPanel }

    set top [winfo parent $w]
    #
    if { "$top" == "." } { set top ""}	
    set win $top.textcommands
    set omPanel $win
    makeLocal $w fontSize
    setFontOptions $fontSize

    global [oarray $top.textcommands]
    set menubar $top.textcommands
    if { [winfo exists $menubar] } {
	return $menubar
    }
    oset $win history ""
    oset $win historyIndex 0
    wmenubar $menubar
    pack $menubar -side top -expand 1 -fill x -anchor nw

    button $win.back -image ::img::previous -text [mc Back] -relief flat \
        -width 30 -height 30 -command "OpenMathMoveHistory $win -1"
    button $win.forward -image ::img::next -text [mc Forward] \
	-relief flat -width 30 -height 30 -command "OpenMathMoveHistory $win 1"
    pack $win.back $win.forward -side left -expand 0

    global location
    if {1} {
	menubutton $win.url -image ::img::track -text Url: -relief flat \
            -width 30 -height 30 
	menu $win.url.m -tearoff 0 \
	    -postcommand [list vMaxOMUrlPostCommand $win $win.url.m]
	$win.url configure -menu $win.url.m
	pack $win.url -side left -fill both -expand 0
	proc vMaxOMUrlPostCommand {win m} {
	    $m delete 0 end
	    foreach v [oget $win history] {
		set url [oget $v location]
		$m add command -label $url \
		    -command [list OpenMathOpenUrl $url -commandpanel  $win]
		
	    }
	}
    } else {
	#mike slate the old histroy list for demolition
	button $win.loclabel -text " Url:" \
	    -command "OpenMathOpenUrl \[$win.location get\] -commandpanel  $win"
	setHelp $win.loclabel [mc {Fetch the URL or FILE indicated in the entry box. \
				   A local file is something like file:/home/wfs/foo.om, and a URL \
				   begins with http.}]

	pack $win.loclabel -side left -fill x -expand 0
    }

    entry $win.location -textvariable [oloc $win location] -width 40
    setHelp $win.location [mc {Address of the current document.  You may modify it and type Enter, to fetch a new document.}]
    bind $win.location <Key-Return> "OpenMathOpenUrl \[$win.location get\] -commandpanel  $win"
    pack $win.location  -side left -fill x -expand 1
    label $win.locspace -text " "
    pack $win.locspace -side left -fill x -expand 0

    oset $win history ""
    pack $win -side top -expand 1 -fill x

    oset $win status $maxima_priv(cStatusWindow)
    return $win
}

proc forgetCurrent { win } {
    makeLocal $win history historyIndex
    set i 0
    if { [llength $history] > 1 } {
	set w [lindex $history $historyIndex]
	set history [lreplace $history $historyIndex $historyIndex]
	# might have caused two identical ones to be next to each other
	if { "[lindex $history $historyIndex]" == "[lindex $history [expr {$historyIndex -1 }]]" } {
	    set history [lreplace $history $historyIndex $historyIndex]
	    set i -1
	}
	if { [lsearch  $history $w] < 0 } {
	    after 2000 "destroy $w"
	}
	oset $win history $history
	OpenMathMoveHistory $win $i
    }
}

proc omDoStop { win } {
    global maxima_priv
    set st $maxima_priv(cStatusWindow)
    set var [$st.scale cget -variable]
    if { [regexp {sock[0-9]+} $var sock] } {
	oset $sock done -1
	if { ![catch { close $sock} ] } {
	
	    append maxima_priv(load_rate) "--aborted"
	}
    }
}





#
#-----------------------------------------------------------------
#
# setTypeForEval --  insert special editing of options, into MENU for PROGRAM
#
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc setTypeForEval { menu program } {
    global maxima_priv
    #puts "$menu program"
    set slaves [pack slaves $menu.program ]
    set men $menu.program.$program
    if { [llength $slaves] > 0 } {eval pack forget $slaves}
    if { ![catch { set options $maxima_priv(options,$program) } ] } {
	if { ![winfo exists $menu.program.$program] } {
	    #puts "options=$options"
	    # puts "there"

	    ### set up to add menu items to a new frame
	    set key $menu.program

	    frame $men
	    rename $men $men-orig
	    set body "wmenuInternal $key \$option \$args"
	    oset $menu.program menu $men
	    oset $men items ""
	    oset $key parent $menu
	    proc $men {option args } $body
	
	    ##### end
	

	    foreach v $options {
		desetq "key dflt help" $v
		
		if { [catch { set maxima_priv(options,$program,$key)} ] } {
		    set maxima_priv(options,$program,$key) $dflt
		}
		switch [lindex $v 3] {
		    boolean {
			$men add check -label $key -variable maxima_priv(options,$program,$key) -help [concat $program option -$key: $help] -onvalue 1 -offvalue 0
		    }
		    default {
            		$men add entry -label "$key:" -entryvariable maxima_priv(options,$program,$key) -help [concat $program option -$key: $help]

		    }

		}

		
		#		label $new.label -text $key:
		#		entry $new.entry  -textvariable maxima_priv(options,$program,$key)
		#		pack $new.label $new.entry -side top -anchor w -fill x
		#		pack $new -fill x
		#		setHelp $new [concat $program option -$v: $help]

	    }
	}
	
    }
    catch { pack $men}

}


#
#-----------------------------------------------------------------
#
# getGlobalOptions --  Convert the current global options for program,
# to an option list:  -key1 value1 -key2 value2 ..
#
#  Results: the option list
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc getGlobalOptions { program } {
    global maxima_priv
    set ans ""
    if { ![catch { set options $maxima_priv(options,$program) } ] } {
	foreach v $options {
	    set key [lindex $v 0]
	    set dflt [lindex $v 1]
	    if { ![catch { set val $maxima_priv(options,$program,$key) }] } {
		if { "$val" != "$dflt" } {
		    lappend ans -$key $val
		}
	    }
	}
    }
    return $ans
}


#
#-----------------------------------------------------------------
#
# setGlobalOptions --  set the current global values of the options for PROGRAM
# according to the values specified in OPTIONLIST.   If a value is not specified
# use the value supplied in the defaults: $maxima_priv(options,$program)
#
#  Results:  none
#
#  Side Effects: the entries maxima_priv(options,$program,$key) are changed
#  for each $key which is an option for program.
#
#----------------------------------------------------------------
#
proc setGlobalOptions { program list } {
    global maxima_priv
    if { [catch { set options $maxima_priv(options,$program) } ] } {
	foreach  v $options {
	    set key [lindex $v 0]
	    set dflt [lindex $v 1]
	    set $maxima_priv(options,$program,$key) \
		[assoc -$key $list $dflt]
	}
    }
}

proc toggleEditBar  {win} {
    makeLocal $win showEditBar editbar
    if { [winfo viewable $editbar] }  {
	pack forget $editbar
	oset $win showEditBar "show edit bar"
    } else {
	pack $editbar -in $win -side bottom -expand 1 -fill x
	oset $win showEditBar "hide edit bar"
    }
}


proc getPrefixed { prefix  tags } {
    set i [lsearch $tags ${prefix}*]
    if { $i >= 0 } {
	return [string range [lindex $tags $i] [string length $prefix] end]
    } else {
	return ""
    }
}

proc programFromTags {tags} {
    if {[lsearch $tags Teval ] < 0 } {
	return ""
    }
    return [getPrefixed program: $tags]
}

proc saveToFile { commandPanel label file } {
    makeLocal $commandPanel textwin
    $label configure -relief sunken
    set lab [$label cget -text]

    # save just as text
    set text [$textwin get 0.0 end]

    if { [catch { set fi [open $file w] } err] } {
	return -code error \
	    [M [mc "Could not open file %s\n%s"] \
		 [file native $file] $err]
    }
    puts $fi $text
    close $fi
    $label configure -relief raised -text [concat [mc "wrote"] "$file"]
    after 1200 [list $label configure -text $lab]
}

if { [catch { package require Safesock } ] } {
    catch { policy  home }
    # catch {  policy outside }

}


proc mkOpenMath { win  } {
    global    maxima_priv

    set w $win
    if {[winfo exists $w]} {catch {destroy $w}}
    if { [catch { package require Safesock } ] } {
	# policy network home
	catch {  policy  outside }
    }
    desetq "width height" [getMaxDimensions]
    computeTextWinDimensions $win $width $height

    makeLocal $win fontSize width_chars height_chars fixedFont
    set font $fixedFont

    # puts "fontSize=$fontSize"
    frame $w
    set commandPanel [omPanel $w ]
    oset $w commandPanel $commandPanel
    set prevwindow ""

    catch { set prevwindow [oget $commandPanel textwin] }

    oset $commandPanel textwin $w.text

    # pack $commandPanel -in $w -side top -fill x -pady 2m
    # raise  $commandPanel

    text $w.text -yscrollcommand "$w.scroll set" \
	-selectbackground "#808080" \
	-width $width_chars  -height $height_chars -font $font -wrap word
    bind $w.text <Configure> "resizeSubPlotWindows $w.text %w %h"
    set maxima_priv(currentwin) $w.text
    set maxima_priv(point) end

    $w.text tag bind "currenteval" <Leave> "$w.text tag remove currenteval 0.0 end ; addTagSameRange %W Teval currenteval @%x,%y;"
    $w.text tag config "currenteval" -foreground red
    $w.text tag bind Teval <Double-Button-1> {doInvoke %W @%x,%y }
    $w.text tag bind Teval <Enter> {addTagSameRange %W Teval currenteval @%x,%y; textShowHelp %W Teval @%x,%y [mc "Double clicking (with the left mouse button), in the marked region will cause evaluation. "]}
    $w.text tag bind Teval <Leave> {deleteHelp %W}
    $w.text tag config hrule -font {Courier 1} -background black
    $w.text mark set insert 0.0

    # try "#d0d0d0" or "#ffffd0" or yellow

    $w.text tag configure Teval -foreground blue -font $font  -border 1 -lmargin1 20



    $w.text tag configure bold -font [xHMmapFont font:propor:bold:r:3] -lmargin1 15
    $w.text tag configure plain -font [xHMmapFont font:propor:bold:r:3] -lmargin1 10
    $w.text tag configure Tresult -font [xHMmapFont font:fixed:bold:r:3] -lmargin1 10
    $w.text tag configure Tmodified -font [xHMmapFont font:fixed:normal:r:3] -background pink -relief sunken -border 1
    $w.text tag configure Thref -font [xHMmapFont font:fixed:normal:r:3]  -foreground blue  -relief flat

    set lh [oget $win lineheight]
    $w.text tag configure sub -offset [expr {-round($lh*.6) }]
    $w.text tag configure sup -offset [expr {round($lh*.6) }]


    oset $w.text counter 0
    # allow some openmath text bindings to take precedence
    bindtags $w.text "OpenMathText [bindtags $w.text]"
    scrollbar $w.scroll -command "$w.text yview"

    pack $w.scroll -side right -fill y
    pack $w.text -expand 1  -fill both
    pack $w -expand 1 -fill both

    if {[winfo exists $prevwindow] } { pack forget [winfo parent $prevwindow] }
    return  $w.text

}

#source emaxima.tcl
#source egp.tcl

# Create bindings for tags.

# set ActiveTags {
#   gap-eval
#   gap-eval-insert
#   octave-eval
#   octave-eval-insert
#   face-jump-to-bkmark
#   xlsp-eval
#   xlsp-eval-insert
#   gcl-eval
#   gcl-eval-insert
#   emacs-lisp-eval
#   emacs-lisp-eval-insert
#   mma-eval
#   mma-eval-insert
#   Splus-eval
#   Splus-eval-insert
#   gp-eval
#   gp-eval-insert
#   maple-eval
#   maple-eval-insert
#   shell-eval-region
#   gnuplot-eval
#   xplot-eval
#   maxima-eval
#   maxima-eval-insert
#   dfplot-eval
#   book-shell-eval-insert
#   book-image-insert
#   book-postscript-insert
#   book-tex-math-mode
#   book-elisp-eval
#   book-shell-eval
#  }

global evalPrograms
# add in Toctave, Topenplot, Thref etc... ie ones with eval_* defined
foreach v [info proc insertResult_*] {
    lappend evalPrograms [string range $v 13 end]
}


#
#-----------------------------------------------------------------
#
# defaultInsertMode --  each program can have a default insert mode.
#  If the insert method is not noted specifically then it uses the default.
#  maxima and gp have default to insert.
#  Results: 0 or 1
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc defaultInsertMode { program } {
    global maxima_priv
    if { [catch {  set dflt [getOptionDefault doinsert $maxima_priv(options,$program)]} ] } { return 1}

    if { "$dflt" == "" } {set dflt  1}
    return $dflt
}

proc doInsertp { tags } {
    set program [programFromTags $tags]
    # puts "program=$program," ; flush stdout
    return [getEvalArg -doinsert $tags [defaultInsertMode [programName $program]]]
}


#
#-----------------------------------------------------------------
#
# doInvoke --  invoked when user clicks on WINDOW at INDEX
# this will either call the program whose tag is in the list of
# tags at this point, on the expression which is highlighted for this
# or else call the special code in eval_$program if the latter exists.
#  Results: none
#
#  Side Effects: The modified result of the insert field will be cleared,
#  and the value there will be changed.
#----------------------------------------------------------------
#
proc doInvoke { w index } {
    global evalPrograms MathServer
    set tags [$w tag names $index]

    $w tag delete sel

    set program [programFromTags $tags]
    if { "$program" == "" } {
	return
    }
    # puts "base=[oget $w baseprogram],w=$w"
    set res [resolveURL $program [oget $w baseprogram]]
    # puts "program=$program,baseprogram[oget $w baseprogram],res=$res"

    set MathServer "[assoc server $res [lindex $MathServer 0]] \
	   [assoc port $res [lindex $MathServer 1]]"
    set this [thisRange $w  program:$program $index]
    # puts "this=$this"

    set nextResult ""
    set doinsert [doInsertp $tags]
    # puts "doinsert=$doinsert"

    if { $doinsert} {
	set name [getPrefixed name: $tags]
	if { "$name" != "" } {
	    set nextResult [$w tag nextrange result:$name [lindex $this 1]]
	    if { 0 == [llength $nextResult] } {
		error [concat [mc "No result field with"] "name=$name"]
	    }
	} else {
	    set next [$w tag nextrange Teval [lindex $this 1]]
	    set nextResult [$w tag nextrange Tresult [lindex $this 1]]
	    if {
		[llength $nextResult] == 0
		||    ([llength $next] !=0
		       &&  [$w  compare [lindex $nextResult 0] > [lindex $next 0]] )
	    } {
		$w insert "[lindex $this 1]+1 char" " " "Tresult"
		set nextResult [$w tag nextrange Tresult [lindex $this 1]]
		# error "no place to put result"
	    }
	}
	if { "$nextResult" != "" } {
	    eval $w  tag add Tmodified $nextResult
	}
    }
    set prog [programName $program]
    if { [info proc eval_$prog] != "" } {
	if {[eval_$prog $program $w $this $nextResult] != 0 }  {
	    error [mc "Failed to eval region"]
	}
    } else {
	global err
	if { [catch { sendOneInsertTextWin $program [eval $w get $this] $w $this $nextResult} err ] && [regexp "Can't connect" $err ]} {
	    global maxima_default
	    set now [encodeURL [oget $w baseprogram] ]
	    set tem [ldelete $now $maxima_default(defaultservers)]
	    if { [tk_dialog .jil 0 "$err: connect to one of $tem?" "" 0 change "keep $now"] == 0 } {
		set maxima_default(defaultservers)  $tem
		oset $w baseprogram [decodeURL [getBaseprogram]]
		doInvoke $w $index
		return
	    } else {
		return
	    }

	}
    }


}

proc getEvalArg { key names {dflt ""} } {

    foreach v $names {
	if { "[string range $v 0 5]" == "Targs "} {
	    return [assoc $key [lrange $v 1 end] $dflt]
	}
    }
    return $dflt
}


#
#-----------------------------------------------------------------
#
# setModifiedFlag --  add the Tmodified tag to the next Tresult field
#  after the current expression.
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc setModifiedFlag { win index } {
    if { [lsearch [$win tag names $index] Teval] >= 0 } {
	set next [$win tag nextrange Tresult $index]
	if { "$next" != "" } {
	    eval $win  tag add Tmodified $next
	}
    }
}


#
#-----------------------------------------------------------------
#
# insertResult --  replace RESULTRANGE of the text buffer by VALUE,
#  and clear the Tmodified tag if there is one.
#  most eval_$program programs will call this to insert their result.
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc insertResult { w resultRange value } {
    set tags [$w tag names [lindex $resultRange 0]]
    set value [xHMuntabify $value]
    # append a newline to a multiline result that has no newline after it.
    if { [regexp "\n.*\[^\n]\$" $value ] } {append value "\n"}
    eval $w delete $resultRange
    # dont lose the whole thing!!
    if { "$value" == "" } { set value " "}
    $w insert [lindex $resultRange 0] $value  [ldelete Tmodified $tags]
}




#
#-----------------------------------------------------------------
#
# addPreloads --  Tack any preloads or preevals on to the
#  command.
#  Results: the new COMMAND
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc addPreloads {command program win this } {
    set preload [getTagsMatching $win ^pre(load|eval):* $this]
    if { "$preload" != "" &&  ![preeval $program $preload] } {
	if { [regexp \{pre(load|eval):(.*)\} $preload junk op url] ||
	     [regexp pre(load|eval):(.*) $preload junk op url]} {
	    if { "$op" == "load" } {
		set res [HMgetURL $win $url type]
		append res $command
		set command $res
	    } else {
		append url $command
		set command $url
	    }

	}
    }
    return $command
}


#
#-----------------------------------------------------------------
#
# sendOneInsertTextWin --  send PROGRAM the COMMAND for insertion
# in the text window WIN at RANGE.   There may be a program specific
# insertResult_maxima, .. in which case this does the job.   It
# is also passed the field of where the command came from.
# We mark these fields with a tag, since they may get moved by typing
# before the result comes back.   The com:* tags also provide omDoAbort
# with the program names that are currently active, so that it can abort.
#  Results:
#
#  Side Effects: until the evaluation succeeds the tags
#  res:pdata($PROGRAM,result,$i) and a similar com: indicate the
#  result field, and the command field.
#
#----------------------------------------------------------------
#
proc sendOneInsertTextWin { program command win this range} {
    set eval [getTagsMatching $win ^eval(sub|):* $this]
    if { "$eval" != "" } {
	if { [regexp \{eval(sub|):(.*)\}  $eval junk op val ]  } {
	    if { "$op" == "sub" } {
		regsub -all "\\&" $val $command val
	    }
	    set command $val
	}
    }
    set command [addPreloads $command $program $win $this ]

    # puts "preload=$preload,command:$command"
    set loc [sendOneDoCommand $program $command "sendOneInsertTextWin1 $win $program "]
    if { "$range" != "" } {
	$win tag add res:$loc [lindex $range 0] [lindex $range 1]
    }
    $win tag add com:$loc [lindex $this 0] [lindex $this 1]
}

proc sendOneInsertTextWin1 { win program location } {
    #puts "entering trace:sendOneInsertTextWin1 $win $location"
    #flush stdout
    message "received result"
    set resultRange [$win tag nextrange res:$location 0.0]
    set this [$win tag nextrange com:$location 0.0]
    $win tag delete res:$location com:$location
    #    if { "$resultRange" == ""} {
    #	puts "somebody removed result place for $location"
    #	return ""
    #    }

    if {[info command insertResult_[programName $program]] != "" } {
	insertResult_[programName $program] \
	    $win $this $resultRange \
 	    [uplevel "#0" set $location]
    } else {
	insertResult $win $resultRange [uplevel "#0" set $location]
    }
    uplevel "#0" unset $location
}


proc xHMuntabify { s } {
    set lis [split $s \n]
    set ans [lindex $lis 0]
    foreach v [lrange $lis 1 end] {
	append ans \n[xHMuntabifyLine $v]
    }
    return $ans
}

proc xHMuntabifyLine { s } {
    set l [split $s \t]
    set ans [lindex $l 0]
    set rest [lrange $l 1 end]
    foreach w $rest {
	set n [expr {[string length $ans]%8}]
	append ans [string range "        " $n end]
	append ans $w
    }
    return $ans
}


#
#-----------------------------------------------------------------
#
# textBbox --  Compute the bounding box of a range of characters
# starting at IND1 and running to IND2.
#
#  Results: return "x y width height" where x, y are the coordinates
#  of the upper left corner.
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc textBbox { win ind1 ind2 } {

    foreach i { 1 2 } {
	set ind [eval $win index [set ind$i]]
	set ind$i $ind
	set line$i [lindex [split $ind .] 0]
	if { [catch {desetq "x$i y$i xdim$i ydim$i" [eval $win bbox $ind]}] } {
	    # not visible
	    return ""}
    }
    if { $line1 == $line2 } {
	return "$x1 $y1 [expr {$x2-$x1+$xdim2}] [expr {$y2-$y1+$ydim2}]"
    } else {
	set xrange "$x1 $x2+$xdim2"
	set yrange "$y1 $y2+$ydim2"
	
	for { set j $line1 } { $j < $line2 } { incr j } {
	    desetq "x y xdim ydim" [$win dlineinfo $j.0]
	    set xrange [minMax $xrange $x [expr {$x + $xdim}]]
	    set yrange [minMax $yrange $y [expr {$y + $ydim}]]
	}
	desetq "x y xdim ydim" [$win dlineinfo $line2.0]
	set xrange [minMax $xrange $x [expr {$x + $xdim}]]
	set yrange [minMax $yrange [expr {$y + $ydim}]]
	desetq "x1 x2 y1 y2" "$xrange $yrange"
	return "$x1 $y1 [expr {$x2 - $x1}] [expr {$y2 - $y1}]"

    }
}

proc textShowHelp { win tag index msg } {
    set this [thisRange $win $tag $index]
    if { "$this" == "" } { return }
    set tags [$win tag names $index]
    if { "$tag" == "Teval" } {
	set program [programFromTags $tags]
	if { "$program" != ""} {
	    set msg [string trimright $msg ". "]
	    append msg [M [mc " by %s."] "$program"]
	}
	if { [doInsertp $tags] } {
	    append msg [mc " The result will be inserted."]
	}
	if { "[getPrefixed name: $tags]" != "" } {
	    append msg [concat [mc "  The result field is named"] "`[getPrefixed name: $tags]'."]
	}
    }
    if { [catch { desetq "x y wid hei" [eval textBbox $win  $this] } ] } {
	# cant get position
	return ""
    }
    set top [winfo toplevel $win]

    set x [expr {$x + [winfo rootx $win] - [winfo rootx $top]}]
    set y [expr {$y + [winfo rooty $win] - [winfo rooty $top]}]

    #puts "showHelp $win $x $y $wid $hei"
    #mike FIXME: $arg1 is a list not a window
    showHelp "$win $x $y $wid $hei" $msg
}

proc getTagsMatching { win regexp range } {
    foreach ind $range {
	foreach v [$win tag names $ind] {
	    if { [regexp -- $regexp $v] } {
		set there($v) 1
	    }
	}
    }
    set dump [eval $win dump -tag $range]
    set i 1
    set ll [llength $dump]
    while { $i < $ll } {
	set v [lindex $dump $i]
	if { [regexp -- $regexp $v] } {
	    set there($v) 1
	}
	incr i 3
    }
    return [array names there]
}

proc markForProgram { w args } {
    global evalTags
    set win [omPanel $w]
    set program [assoc -program $args [oget $win currentProgram]]
    set range [assoc -range $args [$w tag nextrange sel 0.0]]
    if { "$range" == ""} {
	return ""
    }
    set tags [assoc -tags $args ""]
    if { "$tags" == ""} {
	set tags [list Teval program:$program ]
	set opts [getGlobalOptions [programName $program]]
	if { "$opts" != ""} {  lappend tags [concat Targs $opts] }
    }
    # puts "tags=$tags"
    eval $w tag remove Teval $range
    foreach v [getTagsMatching $w "^Targs |^program:" $range] {
	eval $w tag remove [list $v] $range
    }
    foreach v $tags {eval $w tag add [list $v] $range}
    set insert [doInsertp $tags]
    if { $insert } {
	set nextResult [$w tag nextrange Tresult [lindex $range 1]]
	set next [$w tag nextrange Teval [lindex $range 1]]
	if { [llength $nextResult] == 0 ||
	     ([llength $next] !=0)
	     &&  [$w compare [lindex $nextResult 0] > [lindex $next 0]] } {

	    set templates [list " yields " " evaluates to "  \
			       " returns " " produces " " gives "]
	    $w mark set tmp [lindex $range 1]	

	    $w insert tmp [lindex $templates [expr {[clock clicks]%[llength $templates]}]] plain
	    $w insert tmp RESULT {Tresult Tmodified}
	    $w insert tmp " "  {plain}
	} else {
	    apply $w tag add Tmodified $nextResult
	}

    }
}

## endsource preamble.tcl

