# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Printops.tcl,v 1.11 2006-08-24 07:03:23 vvzhy Exp $
#
###### Printops.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

### FIXME: fix a4 size !
global paperSizes printOptions
set paperSizes {{letter 8.5 11} { A4 8.5 11} {legal 8.5 13}}

set printOptions {
    { landscape 0 "Non zero means use landscape mode in printing"}
    { papersize letter "letter, legal or A4"}
    { hoffset 0.5 "Left margin for printing"}
    { voffset 0.5 "Top margin for printing"}
    { xticks 20 "Rough number of ticks on x axis"}
    { yticks 20 "Rough number of ticks on y axis"}
#   { title "" "Title"}
    { psfilename "~/sdfplot.ps" "Postscript filename"}
    { centeronpage 1 ""}
}

# proc getPageOffsets { widthbyheight} {
#     global printOption paperSizes
#     puts "wbh=$widthbyheight"
#     set pwid 8.5
#     set phei 11.0

#     foreach v $paperSizes {
# 	if { "[lindex $v 0]" == "$printOption(papersize)" } {
# 	    set pwid [lindex $v 1]
# 	    set phei [lindex $v 2]
# 	}
#     }
#     set wid [expr {$pwid - 2* $printOption(hoffset)}]
#     set hei [expr {$phei - 2* $printOption(voffset)}]
# #    if { $printOption(landscape) } {set widthbyheight [expr  {1.0 /$widthbyheight}]}
# #    set w $wid ; set hei $wid ; set wid $w

#     puts "pw=$wid,ph=$hei,w/h=$widthbyheight,hh=[expr {$hei * $widthbyheight}], ww=[expr {$wid / $widthbyheight}]"

#     set fac   $widthbyheight
#     puts "fac=$fac"
#     if { $fac * $hei < $wid } {
    # 	set iwid [expr {$fac *$hei}]
# 	set ihei $hei

#     } else {
    # 	set ihei [expr {$wid / $fac}]

# 	set iwid $wid

#     }

#     if { $printOption(landscape) } { set fac1 [expr {1/$fac}] }
#     if { $wid/$hei > $fac } {
# 	set ihei $hei
    # 	set iwid   [expr {$hei / $fac }]

#     } else {
# 	 set iwid $wid
    # 	 set ihei [expr {$wid * $fac }]
#     }

#     #-pagex = left margin (whether landscape or not)
#     #-pagey = right margin (whether landscape or not)
#     #-pagewidth becomes vertical height if landscape
#     #-pageheight becomes horiz width if landscape

#     set xoff [expr {($pwid-$iwid)/2.0}]
#     set yoff [expr  {($phei-$ihei)/2.0}]

#     if { $printOption(landscape) } {
# 	set h $ihei
# 	set ihei $iwid
# 	set iwid $h
#     }

#     puts "phei=$phei,ihei=$ihei,yoff=$yoff,voff=$printOption(voffset)"
#     set ans "-pagex [set xoff]i -pagey [set yoff]i \
# 	    -pagewidth [set iwid]i -pageheight [set ihei]i"
#     set ans "-pagex [set xoff]i -pagey [set yoff]i \
# 	    -pagewidth [set iwid]i -pageheight [set ihei]i"
#     return $ans
# }

proc swap { a b } {
    set me [uplevel 1 set $b]
    uplevel 1 set $b \[set $a\]
    uplevel 1 set $a [list $me]
}

proc getPageOffsets { widthbyheight} {
    global printOption paperSizes
    #puts "wbh=$widthbyheight"
    set pwid 8.5
    set phei 11.0

    foreach v $paperSizes {
	if { "[lindex $v 0]" == "$printOption(papersize)" } {
	    set pwid [lindex $v 1]
	    set phei [lindex $v 2]
	}
    }
    set wid [expr {$pwid - 2* $printOption(hoffset)}]
    set hei [expr {$phei - 2* $printOption(voffset)}]
    if { $printOption(landscape) } {
	swap wid hei
	#	swap pwid phei
    }
    if { $wid / $hei  < $widthbyheight  } {
	# width dominates
	set iwid $wid
	set ihei [expr {$wid / $widthbyheight }]
	append opts " -pagewidth [set wid]i"
    } else {
	set ihei $hei
	set iwid [expr {$hei * $widthbyheight }]
	append opts " -pageheight [set hei]i"
    }

    #-pagex = left margin (whether landscape or not)
    #-pagey = right margin (whether landscape or not)
    #-pagewidth becomes vertical height if landscape
    #-pageheight becomes horiz width if landscape

    append opts " -pagex [expr {$pwid / 2.0}]i -pagey [expr {$phei / 2.0}]i "

    if { $printOption(landscape) } {
	append opts " -rotate $printOption(landscape)"
    }
    return $opts
}

global printOption
set printOption(setupDone) 0

proc getEnv { name } {
    global env
    if { [catch { set tem $env($name) } ] } { return "" }
    return $tem
}
proc setPrintOptions { lis } {
    global browser_version
    global printOptions printOption printSetUpDone
    if { !$printOption(setupDone) } {
	set printOption(setupDone) 1
	getOptions $printOptions $lis -allowOtherKeys 1 \
		-setdefaults [catch { source [getEnv HOME]/.printOptions }] -usearray printOption
    }
}

proc mkentryPr { w var text buttonFont }  {
    set fr $w ; frame $fr
    uplevel 1 append topack [list " $fr"]
    label $fr.lab1
    label $fr.lab -text "$text:" -font $buttonFont -width 0
    entry $fr.e -width 20 -textvariable $var -font $buttonFont
    pack $fr.lab1 -side left -expand 1 -fill x
    pack $fr.lab -side left
    pack $fr.e -side right -padx 3 -fill x
}


proc mkPrintDialog { name args } {
    global printSet argv env printOptions printOption printSetUpDone paperSizes buttonfont

    set canv [assoc -canvas $args ]
    set buttonFont [assoc -buttonfont $args $buttonfont]
    catch { destroy $name }
    set dismiss "destroy $name"
    if { "$canv" == "" } {
	catch {destroy $name}
	toplevel $name
	wm geometry $name -0+20

    } else {
        $canv delete printoptions
        set name [winfo parent $canv].printoptions
	# set name $canv.fr1
        catch {destroy $name}
	frame $name -borderwidth 2 -relief raised
	
	set item [$canv create window [$canv canvasx 10] [$canv canvasy  10] -window $name -anchor nw -tags printoptions]
        $canv raise printoptions
	set dismiss "$canv delete $item; destroy $name "
    }

    frame $name.fr

    set w $name.fr
    label $w.msg  -wraplength 600 -justify left -text [mc "Encapsulated PostScript File Options"] -font $buttonFont
    pack $w
    pack $w.msg
    set wb $w.buttons
    frame $wb
    pack $wb -side left -fill x -pady 2m
    set topack ""
    catch { set printOption(psfilename) \
	    [file nativename $printOption(psfilename)]}
    set win [winfo parent $canv]
    button $wb.save -text [mc "Save"] -font $buttonFont -command "destroy $name; writePostscript $win; $canv delete printoptions"
    button $wb.cancel -text [mc "Cancel"] -font $buttonFont -command "destroy $name ; $canv delete printoptions"
    set writefile "Save"
    mkentryPr  $wb.psfilename printOption(psfilename) [mc "Postscript filename"] $buttonFont
    mkentryPr  $wb.hoffset printOption(hoffset) [mc "Left margin (inches)"] $buttonFont
    mkentryPr  $wb.voffset printOption(voffset) [mc "Top margin (inches)"] $buttonFont
    eval pack $topack -expand 1

    foreach v  $paperSizes {
	set papersize [lindex $v 0]
        set lower [string tolower $papersize]
        radiobutton $wb.$lower -text [lindex $v 0] -variable printOption(papersize) \
		-value [lindex $v 0] -font $buttonFont -highlightthickness 0
	pack $wb.$lower -pady 2 -anchor w -fill x
    }

    checkbutton $wb.b1 -text [mc "Center on Page"] -variable printOption(centeronpage) -relief flat -font $buttonFont
    checkbutton $wb.b2 -text [mc "Landscape Mode"] -variable printOption(landscape) -relief flat -font $buttonFont
    pack $wb.b1 $wb.b2

    frame $w.grid
    pack $w.grid -expand yes -fill both -padx 1 -pady 1
    pack $wb.save $wb.cancel
    grid rowconfig    $w.grid 0 -weight 1 -minsize 0
    grid columnconfig $w.grid 0 -weight 1 -minsize 0
}

proc markToPrint { win tag title } {
    # puts "$win $tag"
    # bind $win <1> "bindBeginDrag $win %x %y $tag [list $title]"
    pushBind $win <1> "$win delete printrectangle ; popBind $win <1>"
    pushBind $win <1> "bindBeginDrag $win %x %y $tag [list $title]; popBind $win <1>"
}

proc bindBeginDrag { win x y tag title } {
    $win delete $tag printrectangle
    set beginRect "[$win canvasx $x] [$win canvasy $y]"
    set it1 [eval $win create rectangle $beginRect $beginRect -tags $tag -width 3]
    set old [bind $win <B1-Motion>]
    set new "eval $win coords $it1 \
	    $beginRect \[$win canvasx %x\] \[$win canvasy %y\]; \
	    "
    if { "$old" == "$new" } {set old ""}
    #mike FIXME: rip this out
    bind $win <B1-Motion> $new
    bind $win <ButtonRelease-1> "bind $win <B1-Motion> [list $old];\
	    bind $win <ButtonRelease-1> {} ; unbindAdjustWidth $win $tag [list $title];"
}

proc unbindAdjustWidth { canv tag title } {
    set win [winfo parent $canv]
    global printOption

    set it [$canv find withtag $tag]
    set co1 [$canv coords $tag]
    set co [$canv coords $it]
    # if { "$co" != "$co1" } {puts differ,$co1,$co}
    desetq "x1 y1 x2 y2" $co
    set center [expr { ($x1+$x2 )/2}]
    set h [expr {$y2 - $y1}]
    set it [$canv find withtag $tag]
    set new [$canv create rectangle $x1 $y1 $x2 $y2 -outline white -width [expr {$h* .04}] -tags [concat $tag bigger] ]

    # puts "<marginTicks $canv $x1 $y1 $x2 $y2 printrectangle>"
    marginTicks $canv [storx$win $x1] [story$win $y2] [storx$win $x2] [story$win $y1] "printrectangle marginticks"
    desetq "a1 b1 a2 b2" [$canv bbox $new]
    set textit [$canv create text $center [expr {$y1 - $h *.03}] \
	    -font [font create -family Courier -size 14 -weight bold] -text "$title" \
	    -anchor s -tags [concat $tag bigger title]]

    set bb [$canv bbox $textit]
    $canv create rectangle $a1 [lindex $bb 1]  $a2 [expr {$y1 - 0.02 * $h}]  -tags $tag -fill white -outline {}
    $canv itemconfig $it -width [expr {$h *.002}]
    $canv raise $it
    $canv raise $textit
    $canv raise marginticks
    if { $printOption(domargin) == 0 } {
	$canv delete marginticks
    }

    $canv create text [expr {($a1 + $a2)/2.0}] [expr {$y2 + .01*$h  }] -anchor nw -tag $tag
    # puts h=$h

}


proc getPSBbox  { } {
    set fi [open /home/wfs/sdfplot.ps r]
    set me [read $fi 500]
    regexp {BoundingBox: (-*[0-9]+) (-*[0-9]+) (-*[0-9]+) (-*[0-9]+)} $me junk x1 y1 x2 y2
    set w [expr {72 * 8.5}]
    set h [expr {72 * 11}]
    # puts "hei=[expr {$y2-$y1}],tm=[expr {$h - $y2}],bm=$y1"
    # puts "wid=[expr {$x2-$x1}],lm=$x1,rm=[expr {$w - $x2}]"
    # puts "hei=[expr {($y2-$y1)/72.0}],tm=[expr {($h - $y2)/72.0}],bm=([expr {$y1/72.0}])"
    #puts "wid=[expr {($x2-$x1)/72.0}],lm=([expr {$x1/72.0}]),rm=[expr {($w - $x2)/72.0}]"
    close $fi
}


## endsource printops.tcl
