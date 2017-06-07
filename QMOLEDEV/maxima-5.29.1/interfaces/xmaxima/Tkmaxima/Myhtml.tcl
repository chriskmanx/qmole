# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Myhtml.tcl,v 1.15 2006-10-01 23:58:29 villate Exp $
#
###### Myhtml.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

# parsing routines for html
# try to be compatible from calling level with the package by stephen uhler.
# to use:
#  set html [exec cat /home/wfs/tclet/server/sample.html] ; xHMinit_win .t ; xHMset_state .t url sample.html ; xHMparse_html $html "xHMrender .t" ;     array set wvar $args
# source myhtml.tcl ; catch {destroy .t } ; text .t ;  set html [exec cat /home/wfs/tclet/server/sample.html] ; xHMinit_win .t ; xHMset_state .t url sample.html ; xHMparse_html $html "xHMrender .t"

proc testit { file } {
    global xHMpriv
    source myhtml.tcl
    catch {destroy .t }
    foreach {k val} [array get xHMpriv geom*] {unset xHMpriv($k) }
    frame .t
    text .t.text
    set t .t.text
    set html [exec cat $file]
    xHMinit_win $t
    xHMset_state $t url $file
    xHMparse_html $html "xHMrender $t"
    pack .t
    pack $t
    raise .
}

#
#     xHMparse_html $html "xHMrender .t"
# you can change the state of the parse engine by using
#    xHMset_state .t key1 val1 key2 val2...

#########

#  the HTML tags:

# becomes

# idea: some tags like font,indent,link have only one per but the tag
# varies..  others have a constant tag... eg 'strike' 'underline' ...
# or fill.  You cant have
# and are either on or off...
# have pushConstantTag win tag
# have popConstantTag win tag
# have pushNamedTag win name tag
# have popNamedTag win name tag   :sets current to be this one and pushes previous..
# and these maintain things so that
# [array names xHMtaglist$win] should provide the taglist to do

proc xHMpushConstantTag { win tag } {
    upvar #0 xHMtaglist$win taglist
    if { [catch {incr taglist($tag) } ] } {
	set taglist($tag) 1 }
}

proc xHMpopConstantTag {win tag} {
    upvar #0 xHMtaglist$win taglist
    catch {
	set i [incr  taglist($tag) -1]
	if { $i <= 0 } {unset taglist($tag) }
    }
}

proc xHMpushNamedTag {win name tag} {
     upvar #0 xHMvar$win wvar
    #puts "push $win <$name> <$tag>"
    if { [catch { set now [lindex [set wvar($name)] end] }] } {
	set now "" }
    lappend wvar($name) $tag
}

proc xHMpopNamedTag {win name} {
    upvar #0 xHMvar$win wvar
    set v [set wvar($name)]
    set now [lindex $v end]
    catch { set v [lreplace $v end end] }
    set wvar($name) $v
    return $now
}

proc xHMgetNamedTag {win tag } {
    upvar #0 xHMvar$win wvar
    set res ""
    catch  { set res [lindex $win($tag) end] }
    return $res
}

proc xHMpushAindent { win i } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist
    set n [incr wvar(indent) $i]
    # puts "taglist:[array names taglist ]"
    unset taglist(indent:[expr {$n - $i}])
    set taglist(indent:$n) 1
}

proc xHMpopAindent { win i } {
    upvar #0 xHMtaglist$win taglist
    upvar #0 xHMvar$win wvar
    set n 0
    set n [set wvar(indent)]

    unset taglist(indent:$n)
    set n [expr {$n - $i}]
    if { $n < 0 } { set n 0 }
    set wvar(indent) $n
    set taglist(indent:$n) 1

}

# font and indent wil


#
 #-----------------------------------------------------------------
 #
 # defTag --  creates an executable scripts to invoke when the TAG
 #  or /TAG are encountered.
 #     -alter  takes a list of key1 val1 key2 val2
 #         generally these are pushed onto stacks for TAG and popped for /TAG
 #         the value of xHMtaglist$win  should get altered
 #     -before  set the prefix for text inserted for TAG
 #     -after   set the prefix for text inserted for /TAG
 #     -body   additional body to use for TAG
 #     -sbody   additional body to use for the /TAG
 #  The variables { tag  params text }  are bound when
 #  the BODY is evaluated.   Thus for example $text would get the
 #  text following the tag, and
 # 	set paramList [xHMsplitParams $params]
 #  could be used to decode the params.
 #
 #  Results: none
 #
 #  Side Effects: saves the script in xHMtag array under TAG and /TAG
 #
 #----------------------------------------------------------------
#
proc defTag { htag args } {
    global xHMtag
    foreach {key val } $args { set $key $val }
    if { [info exists -alter] } {
	foreach { key tag } ${-alter} {
	    if { [string match A* $key] } {
		append body "\nxHMpush$key \$win $tag"
		append sbody "\nxHMpop$key \$win $tag"
	    } elseif { [string match C* $key] } {
		append body "\nxHMpushConstantTag \$win $tag"
		append sbody "\nxHMpopConstantTag \$win $tag"
	    } else {
		append body "\nxHMpushNamedTag \$win $key $tag"
		append sbody "\nxHMpopNamedTag \$win $key"
	    }
	}
	array set toalter ${-alter}
	foreach prop { family size weight style} {
	    if { [info exists toalter($prop)] } { append fontprops " $prop"}
	}
	catch {
	    append body "\nxHMalterFont \$win $fontprops"
	    append sbody "\nxHMalterFont \$win $fontprops"
	}
    }
    catch { append body \n${-body} }
    catch { append sbody \n${-sbody} }
    catch { append body "\nset prefix \"[slashNewline ${-before}]\"" }
    catch {append sbody "\nset prefix \"[slashNewline ${-after}]\""  }
    catch { set xHMtag($htag) $body }
    catch { set xHMtag(/$htag) $sbody }
}

proc slashNewline { s } {
    regsub -all "\n" $s "\\n" s
    return $s
}

# netscape uses fonts in the following progression.
# we will have the font labels looking like:
#  font:propor:normal:r:4   to indicate size 4
# In an application if the user sets the default
# nfont:nfamily:nweight:nstyle:nsize
# where nfamily is in {propor,fixed}
# where nweight is in {normal,bold}
# where nstyle  is in {i,r}
# where nsize   is in {1,2,3,4,5,6,7}
# then we map the label to a particular font....
# propor-->times
# fixed->courier

# set the font to be what it would map to for X.
proc xHMsetFont { win fonttag  } {
    upvar #0 xHMvar$win wvar
    set fo [xHMmapFont $fonttag]
    set wvar($fonttag) 1
    $win tag config $fonttag -font $fo
}


#convert a fonttag into an actual font specifier, using preferences.
# mapping propor,fixed to font families, and dobing size adjusting based
# on font type.
 proc xHMmapFont {  fonttag } {
    # font:family:weight:style:size
    global maxima_default xHMfonts
    if { [info exists xHMfonts($fonttag) ] } {
	return $xHMfonts($fonttag)
    } else {
	set xHMfonts($fonttag) [set fo [font create]]
	xHMconfigFont $fonttag
	return $fo
	
    }
 }

 proc xHMconfigFont {  fonttag } {
    # font:family:weight:style:size
    global maxima_default xHMfonts

    set font $xHMfonts($fonttag)
    set s [split $fonttag :]
    if {[llength $s] < "2"} {
	error [concat [mc "Internal font error:"] "$fonttag '$xHMfonts($fonttag)'"]
    }
    set fam [lindex $s 1]
    #puts "fam=$fam,fonttag=$fonttag,s=$s"
    if { "$fam" == "" } {
	set fam propor
    }
    set si [expr {$maxima_default($fam,adjust) + [lindex $s 4]}]
    #set si [lindex $s 4]
    set si [expr {($si < 1 ? 1 : ($si > 8 ? 8 : $si))}]
    set elt [lindex $s 1]
    if {![info exists maxima_default($fam)]} {
	error [concat [mc "Internal font error:"] "'$fam'"]
    }
    set family $maxima_default($fam)
    set weight [lindex $s 2]
    set slant [lindex $s 3]
    if { "$slant" == "i" } { 
	set slant italic
    } else {
	set slant roman
    }
    #puts "font config $font -family $family -size $maxima_default($fam,$si) -slant $slant -weight $weight"
    global tcl_platform
    if { "$tcl_platform(platform)" == "unix" } {
	set usePixel "-"
    } else {
	set usePixel ""
    }
    font config $font -family $family -size $usePixel$maxima_default($fam,$si) -slant $slant -weight $weight
    return
 }

 ### the following resets all the fonts
 ### for any windows now that font objects are interned

 proc xHMresetFonts { win } {
     global xHMfonts
     foreach v [array names xHMfonts] {
	 xHMconfigFont $v
     }
 }

proc xHMfontPointSize { string } {
    #mike FIXME: hard coded font name and $string is ignored
    set si [font config $string -size]
    return [expr { $si < 0 ? - $si : $si }]
}




proc xHMalterFont {win args } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist

#    puts "font:$args,[array get wvar *]"
    foreach v {family weight style size adjust}  {
	set $v [lindex $wvar($v) end]
    }

    set si $size
    if { [catch { set si [expr {$si + $adjust}] }] } {
	# puts "too many pops"
	return
    }
    set font font:$family:$weight:$style:$si
    if { ![catch { set fo $wvar(font) }] } {
	catch { unset taglist($fo) } }
#    puts "font=$font, wvar=[array get wvar fon*]"
    set  wvar(font) $font
    if { ![info exists wvar($font)] } {
	xHMsetFont $win $font }
    set taglist($font) 1
	
   # return "-*-$family-$weight-$style-normal-*-*-${size}0-*-*-*-*-*-*"
}

proc xHMsplitParams { param } {
    if { "$param" == "" } { return ""}
   set reg "(\[^= \t\n\]+)\[ \t\n]*((=\[ \t\n]*((\"(\[^\"\]*)\")|('(\[^'\]*)')|(\[^ \t\n\]*)))|(\[ \t\n\])|\$)"

   # set sub "{1=\\1,2=\\2,3=\\3,4=\\4,5=\\5,6=\\6,7==\\7,8=\\8,9=\\9}"
   # regsub -all $reg $param $sub  joe
   # puts joe=$joe

    set sub "\\1\\6\\8\\9"
    regsub -all $reg $param $sub  joe
    foreach { dummy key val } [lreplace [split $joe ] end end]  { lappend new [string tolower $key] $val}
    return $new
}

proc xHMextract_param {paramList  key args} {
    foreach { k val } $paramList {
	if { "$k" == "$key" } {
	    uplevel 1 set $key [list $val]
	return 1}}
	if { "$args" != "" } {
	    uplevel 1 set $key  [list [lindex $args 0] ]
	}
	return 0
    }

global xHMtag
if {[info exists xHMtag]} {catch {unset xHMtag}}

defTag a -alter {Cdoaref doaref} -body xHMdo_a  -sbody xHMdo_/a
defTag b -alter {weight bold }
defTag -body xHMdo_body
defTag br -before "\n"
defTag center -alter {Ccenter center}
defTag cite -alter {style i}
defTag code -alter {family fixed}
defTag dd -before "\n" -after "\n"
defTag dfn -alter {style i}
defTag dt -before "\n"
defTag em -alter {style i}
defTag h1 -alter {size 7 weight bold} -body {xHMassureNewlines 1} -after "\n"
defTag h2 -alter {size 6} -body {xHMassureNewlines 1} -after "\n"
defTag h3 -alter {size 6} -body {xHMassureNewlines 1} -after "\n"
defTag h4 -alter {size 5} -body {xHMassureNewlines 1} -after "\n"
defTag h5 -alter {size 4} -before "\n" -after "\n"
defTag h6 -alter {size 3 style i} -before "\n" -after "\n"
defTag i -alter {style i}
defTag img -body xHMdo_img

defTag kbd -alter {family fixed weight bold}
defTag li -body xHMdo_li

defTag dl  -body xHMlistEnter -sbody xHMlistExit
defTag dir  -body xHMlistEnter -sbody xHMlistExit
defTag menu -body xHMlistEnter -sbody xHMlistExit
defTag ol  -body {
    xHMlistEnter
    set wvar(listindex$wvar(indent)) 0} -sbody {
	xHMlistExit }	

defTag title  -body {wm title [winfo toplevel $win] $text ; set text ""} -sbody {list }
defTag ul -alter {Aindent 1} -body { xHMlistEnter
  set paramList [xHMsplitParams $params]
  set _iii -1
  if { [xHMextract_param $paramList type ""] } {
      set _iii [lsearch {disc circle square} $type]
  }
  if { $_iii < 0 } {
      set _iii [expr {($wvar(indent)/2 > 3 ? 3 : $wvar(indent)/2) -1 }]
     if { $_iii < 0 } { set _iii 0}
  }
  # push an index which will say disc, circle or square.
  xHMpushNamedTag $win ultype $_iii
}  -sbody { xHMlistExit ; catch { xHMpopNamedTag $win ultype }}


#defTag p -before "\n\n" -sbody {}
#defTag p -before "\n\n" -sbody {}
defTag p -before "\n" -body { xHMassureNewlines 1 } -sbody { xHMassureNewlines 1 }
defTag blockquote -before "\n\n" -after "\n"
defTag pre -alter {family fixed Cnowrap nowrap} -before "\n" /pre "\n"
defTag samp -alter {family fixed}
defTag strike -alter {Cstrike strike}
defTag strong -alter {weight bold}
defTag sup -alter {Csup sup}
defTag sub -alter {Csub sub}

defTag tt -alter {family fixed}
defTag u -alter {Cunderline underline}

defTag hrx  -body { $win insert $wvar(W_insert) "\n" ;
     $win insert $wvar(W_insert) "\n" hrule
    } -sbody {}
defTag hr -before \n  -body {
     $win insert $wvar(W_insert) "                  " underline
    } -sbody {}

defTag var -alter {style i}

defTag hmstart -alter {	family propor   weight normal   style r   size 3
	list list
        adjust 0 } -body { set wvar(counter) 0 }

defTag font -body {
    set paramList [xHMsplitParams $params]
    xHMpushNamedTag $win adjust [assoc size $paramList 0]
    xHMalterFont $win adjust
    }  -sbody {
	xHMpopNamedTag $win adjust
	xHMalterFont $win adjust
    }

proc notyet { args } {
    puts [concat [mc "not yet"] "$args"]
}

defTag isindex -body xHMdo_isindex -sbody {}
defTag meta -body list -sbody list
defTag form  -before "\n" -after "\n"  -body {
    global xHMpriv
    set xHMpriv(form) [gensym form]
    upvar #0 $xHMpriv(form) form
    set paramList [xHMsplitParams $params]
    #puts "paramList=$paramList"
    if { [xHMextract_param $paramList action ""] } {
	set form(action) $action
    }
    xHMextract_param $paramList method "get"
    set form(method) $method

  } -sbody { global xHMpriv ;
    if { [info exists xHMpriv(form) ] } {
	upvar #0 $xHMpriv(form) form
	#puts form=$xHMpriv(form)
	#puts "form values=[array get form]"

	if { ![info exists form(f_has_submit)] } {
	    set params ""
	    xHMtextInsert $win "\n"
	    xHMdo_input submit
	}
	unset xHMpriv(form)
     }
    }
defTag input -body xHMdo_input
defTag select -body "xHMdo_input select" -sbody {
#    puts wvar=[array get wvar f_in_select]
    #catch {
    global xHMpriv
    upvar #0 $xHMpriv(form) form
    puts "\[array get wvar f_in_select*]=[array get wvar f_in_select*]"
    set na [lindex $wvar(f_in_select) 0]
     	
    set w $form(f_select,$na)
    foreach v [lrange $wvar(f_in_select) 1 end] {
	$w.list insert end $v
    }
    xHMresetListbox $w $wvar(f_selected,$na)
    append form(f_reset) " ; xHMresetListbox $w [list $wvar(f_selected,$na)]"
    #puts $w
    if { [winfo exists ${w}label] } {
	#puts "have label $w and ${w}label"
	bind  ${w}label <1> "place $w -anchor center -relx 0 -rely 1.0 -bordermode outside -in ${w}label ; raise $w"
	bind  $w <Leave> "xHMresetListbox $w \[$w.list curselection\] ; place forget $w"
    }
    if { [$w.list cget -height] > 0  && [llength $wvar(f_select_values)] > [$w.list cget -height] } {
	scrollbar $w.scroll -orient v -command "$w.list yview" -takefocus 0
	$w.list configure -yscrollcommand "$w.scroll set"
	pack $w.scroll -side right -fill y
    }

    set form(f_select_list,$na) $wvar(f_select_values)
    if { [catch { unset wvar(f_selected,$na) }] } { puts "failed= unset wvar(f_selected,$na)"}
    if { [catch  { unset wvar(f_select_values) }] } { puts "failed=unset wvar(f_select_values)"}
    #}
}

proc   xHMresetListbox  { w selected } {
    $w.list selection clear 0 end
    foreach v $selected { $w.list selection set $v}
    set i 0
    if { [llength $selected] > 0 } {
	set i [lindex $selected 0]
    }
    if { [winfo exists ${w}label] } {
	${w}label configure -text [$w.list get $i]
    }
}

defTag textarea -body "xHMdo_input textarea"
proc configColor { args } {
    set color [lindex $args end]
    if { [catch { eval $args } ] } {
	set color [lindex $args end]
	set args [lreplace $args end end "#$color"]
	catch { eval $args }
    }
}


defTag html -body "list " -sbody "list "
defTag head -body "list " -sbody "list "
defTag body -body {
    #puts "<body $params> $text"
     set paramList [xHMsplitParams $params]
    if { [xHMextract_param $paramList bgcolor ""] } {
	configColor $win config -background $bgcolor
	configColor $win tag  config hrule -font {courier 2} -background $bgcolor
    }
    if { [xHMextract_param $paramList baseprogram ] } {
        oset $win baseprogram [resolveURL $baseprogram [oget $win baseprogram]]
	oset $win baseprogram [decodeURL $baseprogram]
    }


    set _text $text
    if { [xHMextract_param $paramList text ""] } {
	 configColor $win config -foreground $text
    }
    set text ${_text}
    foreach {ll tag} {evalrelief Teval resultrelief  Tresult aevalrelief currenteval resultmodifiedrelief Tmodified }  {
	if { [xHMextract_param $paramList $ll ""] } {
	    $win tag configure $tag -relief [set $ll]
	}
    }

    foreach {ll tag} {bgeval Teval bgresult Tresult bgresultmodified Tmodified bgaeval currenteval}  {
	if { [xHMextract_param $paramList $ll ""] } {
	      configColor $win tag configure $tag -background [set $ll]
	}
    }
    foreach {ll tag} {link href alink currenthrefforeground eval Teval result Tresult resultmodified Tmodified aeval currenteval}  {
	if { [xHMextract_param $paramList $ll ""] } {
	configColor $win tag configure $tag -foreground [set $ll]
	}
    }
   } -sbody "list "

defTag base -body {       set paramList [xHMsplitParams $params]
   if { [xHMextract_param $paramList href ""] } {
       set wvar(baseurl) $href
      #xHMset_state $win baseurl $href
       oset $win baseurl $href
   }
  }



defTag option -body { set text [string trimright $text]
       set paramList [xHMsplitParams $params]
       xHMextract_param $paramList value $text
       lappend wvar(f_select_values) $value
       lappend wvar(f_in_select) $text
       if { [xHMextract_param $paramList selected] } {
	   #puts "hi==wvar(f_selected,[lindex $wvar(f_in_select) 0])"
	   lappend wvar(f_selected,[lindex $wvar(f_in_select) 0]) [expr {[llength $wvar(f_in_select)] -2}]
       }
       set text ""
}

global xHMpriv
set xHMpriv(counter) 0


#
 #-----------------------------------------------------------------
 #
 # ldelete --  remove all copies of ITEM from LIST
 #
 #  Results: new list without item
 #
 #  Side Effects:
 #
 #----------------------------------------------------------------
#
proc ldelete { item list } {
    while { [set i [lsearch $list $item]] >= 0} {
	set list [lreplace $list $i $i]
    }
    return $list
}
if { ![info exists _gensymCounter] } {set _gensymCounter  0}
proc gensym { name } {
    global _gensymCounter
    incr _gensymCounter
    set var ${name}_${_gensymCounter}
    catch { uplevel "#0"  unset $var}
    return $var
}

proc xHMdo_input {{type ""}} {
    global xHMpriv
    if { ![info exists xHMpriv(form)] } {
	set xHMpriv(form) [gensym form]
    }
    upvar 1 win win
    upvar #0 $xHMpriv(form) form
    upvar #0 xHMvar$win wvar
    upvar 1 params params
    set form(url) $wvar(url)

    set paramList [xHMsplitParams $params]

    set w $win.input[incr wvar(counter)]
#    bindtags $w [ldelete maxlength [bindtags $w]]
    xHMextract_param $paramList name ""
   if { "$type" == "" } {
    xHMextract_param $paramList type text
   }
    xHMextract_param $paramList value ""
    set value  [xHMconvert_ampersand $value]
    switch -regexp -- $type {
	{text$|password|int$|string} {
	    xHMextract_param $paramList size 20
	    entry $w -width $size
	    if { "$type" == "password" } { $w config -show * }
	    if { [xHMextract_param $paramList maxlength] } {
		bindtags $w [concat [bindtags $w] maxlength]
		bind maxlength <KeyPress> "xHMdeleteTooLong $win %W"
		
		set wvar($w,maxlength) $maxlength
	    }

	    $w insert end $value

	    append form(f_reset) " ; $w delete 0 end ; $w insert end [list $value] "
	    set form(f_submit,$name) "$w get"
	}
	select {
	    xHMextract_param $paramList size 1
	    xHMextract_param $paramList mode single
	    set lis $w
	    if { $size == 1 } {
		set w ${w}label
		label $w -relief raised
	    }
	    frame $lis
	    listbox $lis.list  -selectmode $mode -width 0 -exportselection 0 -height [expr {$size > 1 ? $size : 0}]
	    pack $lis.list -side left

	    # will contain list "window value1 value2 value3 .."
	    # added to by <option>
	    set wvar(f_selected,$name) ""
	    set form(f_select,$name) $lis
	    set wvar(f_in_select) $name
	    set wvar(f_select_values) $name
	    # throw away any text after select
	    set text ""
	
	}
	textarea {
	    upvar 1 text text
	    xHMextract_param $paramList cols 30
	    xHMextract_param $paramList rows 5
	    catch {
	      frame $w
	      puts "w=$w"
	    scrollbar $w.yscroll -command "$w.text yview" -orient v
	    text $w.text -height $rows -width $cols -wrap none \
		    -yscrollcommand "$w.yscroll set"  -padx 2 -pady 2
	     $w.text insert 0.0 $text
	
	    set text ""
	    pack $w.text
	    set form(f_submit,$name) "$w.text get 0.0 end"
	    append form(f_reset) " ; $w.text delete 0.0 end ; $w.text insert end [list $text]"
	} errm ;
	    puts errm=$errm;
	
	}
	image {

	    xHMextract_param $paramList width 0
	    xHMextract_param $paramList height 0
	    xHMextract_param $paramList src "broken.ppm"
	    set form(f_has_submit) 1
	    catch { set base $wvar(url) ; set base $wvar(baseurl) }
	    label $w -image [xHMgetImage $win $src $base $width $height] \
		    -background [$win cget -background]
	    bind $w <ButtonRelease-1>   "xHMdoSubmit $w $xHMpriv(form) {$name.x %x $name.y %y}"
	    bind $w <Return> "xHMdoSubmit $w $xHMpriv(form) {$name.x 0 $name.y 0}"
	    bind $w <Leave> "$w configure -relief raised"
	
	    }
	radio {

	    if { [catch { set var $form(radio,$name) } ] } {
		set var [set form(radio,$name) [gensym radio_value]]
	    }
	    radiobutton $w -variable $var -value $value -text " "
	    if { [xHMextract_param $paramList checked] } {
		append form(f_reset) "; $w select"
		$w select
		
	    } else {
		append form(f_reset) "; $w deselect"
		$w deselect
		
	    }

	    set form(f_submit,$name) "uplevel #0 set $var"

	}
	checkbox {
	    ######### to do fix this..failed: http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Forms/example-4.html
	    if { [catch { set var $form(checkbox,$name) } ] } {
		set var [set form(checkbox,$name) [gensym checkbox_value]]
	    }
	    xHMextract_param $paramList value on
	    checkbutton $w -on $value -variable $var -off _dontsubmit_ \
		    -text " "

	    set form(f_submit,$name) "uplevel #0 set $var"
	
	    if { [xHMextract_param $paramList checked] } {
		append form(f_reset) " ; $w select"
		$w select;
	    } else {
		$w deselect
		append form(f_reset) " ; $w deselect"
	    }

	}
	hidden {
	    set form(f_submit,$name) "list  [list $value]"
	    set w ""
	}
	reset {
	    if { "$value" == "" } {set value "Reset"}
	    button $w -text $value -command "xHMdoReset $xHMpriv(form)"

	}
	submit {
	    set form(f_has_submit) 1
	    if { "$value" == "" } { set value "Submit Query" }
	    if { "$name" != "" } {
		button $w -text $value -command [list xHMdoSubmit $w $xHMpriv(form) [list $name $value]]
	    } else {
		button $w -text $value -command "xHMdoSubmit $w $xHMpriv(form) [list {}]"
	    }
	
	}
    }
#    if { [info exists form(f_submit,$name)] } {
#	lappend form(f_tosubmit) $name
#    }
    #dputs "type=$type,w=$w"
    #dputs "form(reset)=$form(f_reset)"
    if { "$w" != "" } {
	#catch { puts "class=[winfo class $w]" }
	if { [catch {   $win window create $wvar(W_insert) -window $w -align bottom -padx 1 -pady 1 } ] } {
	    puts [concat "$w" [mc "bad window"] "?"]
	}
	
	### todo handle focus of forms.. with tabbing.
	
    }

}

proc xHMsetSubmitPosition { formvar name x y } {
    upvar #0 $formvar form
    set form(f_submit,$name.x) "list $x"
    set form(f_submit,$name.y) "list $y"
}



proc xHMdoReset { formVar } {
    upvar #0 $formVar form
    eval $form(f_reset)
}
proc xHMdoSubmit { w formVar nameVals } {
    upvar #0 $formVar form
    set ans ""
    set win [omPanel $w]
    foreach { name value } $nameVals {
	puts "value=$value--><[xHMencode_get $value]>"
	if { "$name" != "" } { append ans "&$name=[xHMencode_get $value]"}
    }

#    foreach name $form(f_tosubmit) {
#	set val [eval $form(f_submit,$name)]
#	if { "$val" != "_dontsubmit_" } {
#	    append ans "&$name=[xHMencode_get $val]"
#	}
#    }
    set n [string length f_submit,]
    foreach {name value}  [array get form f_submit,* ] {
	 puts "form submit:[array get form f_submit,*]"
	set val [eval $value]
	puts "name=$name,val=$val-->[xHMencode_get $val]"
	if { "$val" != "_dontsubmit_" } {
	append ans "&[string range $name $n end]=[xHMencode_get $val]"
	}
    }
    # do the select listboxes:

    foreach { name w } [array get form f_select,*] {
	set name [string range $name [string length f_select,] end]
	
	set values [lrange $form(f_select_list,$name) 1 end]
	set ans1 ""

	foreach v [$w.list curselection] {
	    lappend ans1 [lindex $values $v]
	}
	puts w=$w.list,name=$name,ans1=$ans1,
	set ans1 [join $ans1 " "]
	append ans "&$name=[xHMencode_get $ans1]"
    }
    #puts ans=$ans
    #puts form=[array get form]
    set action $form(action)
    if { "[string tolower $form(method)]" == "get" } {
	xHMfindUrl $win $form(method) $form(action)?[string range $ans 1 end]
    } else {
	xHMfindUrl $win $form(method) $form(action) [string range $ans 1 end]
}
		}

proc xHMfindUrl { win method  url { body "" }} {
    #puts "$win,$method,$url,$body"
    set method "[string tolower $method]"
    if { "$method" == "get" } {
	OpenMathOpenUrl $url -commandpanel $win
    } elseif { "$method" == "post" } {
	if { "$body" == "" } {set body " "}
	OpenMathOpenUrl $url -commandpanel $win -post $body
    }
}

proc xHMdeleteTooLong { win w } {
    upvar #0 xHMvar$win wvar
    catch { $w delete $wvar($w,maxlength) end }
    #puts $wvar($w,maxlength)
}

proc xHMconvert_ampersand { text } {
    if {![regexp & $text]} {return $text}
    regsub  -all {([[\\])|(&((#([0-9][0-9]?[0-9]?))|([a-zA-Z]+));?)} $text {[xHM_do1 \\\1  \5 : \6]} tmp
    return [subst -novariables $tmp]
}

proc xHM_do1 { a b {c xx} } {
    global isoLatin1
   if { "$a" == " " } {
      if { "$b" == ":" } {
	  #set result ?
	  if { [catch { set result $isoLatin1($c) }] } {
	     return "&$c"
	  }
	  return $result
      } else {
	  return [format %c $b]
      }
   } else {
       return [string index $a 0]
   }
}

proc xHMdo_li {} {
    uplevel 1 {
	set i $wvar(indent)
	set taglist(listindex) 1
	set text [string trimleft $text]
	if { ![catch { incr wvar(listindex$i) }] } {
	    xHMpopAindent $win 1
	    xHMtextInsert $win "\n\t$wvar(listindex$i).\t"
	    xHMpushAindent $win 1
	} else {
	    set ii 0
	    catch { set ii [lindex $wvar(ultype) end] }
	    xHMpopAindent $win 1
	    xHMtextInsert $win "\n\t"
	    xHMinsertBullet $win $ii
	    xHMtextInsert $win "\t"
	    xHMpushAindent $win 1
	}
    unset  taglist(listindex)
 }
}

proc xHMinsertBullet { win i } {
    global xHMulBMPdata xHMpriv
    upvar #0 xHMvar$win wvar
    set fg [$win cget -foreground]
    set image ""
    if {[catch { set image $xHMpriv(ul,$fg,$i) }] } {
	catch { set image [set xHMpriv(ul,$fg,$i) [image create bitmap -data [lindex $xHMulBMPdata $i] -foreground $fg]] }
    }
    # if we cant get the image, or cant insert it fall back to
    # inserting a simple character
    if { "$image" == "" || [catch { $win  image create $wvar(W_insert) -image $image } ] } {
	if { $i > 2 } { set i 2}
	$win tag configure listindex -foreground red		
	xHMtextInsert $win [string range "oo*" $i $i]
    }
}

defTag th -body list
defTag td -body list -after "\t\t\t\t"
defTag tr -body list -after "\n"





proc xHMdo_a  {} {
   uplevel 1  {
       set paramList [xHMsplitParams $params]
       if { [xHMextract_param $paramList href] } {
	   # in case they forget </a>
	   foreach v [array names taglist h:*] {
	       unset taglist($v)
	   }
	   $win tag bind h:$href <Enter> "HMdoaref enter $win %x %y"
	   $win tag bind h:$href <Leave> "HMdoaref leave $win %x %y"
	   $win tag bind h:$href <1> "HMdoaref click $win %x %y"
	   set taglist(h:$href) 1
	   set taglist(href) 1
	
       }
       if { [xHMextract_param $paramList name] } {
	   $win mark set anchor:$name "$wvar(W_insert) -1 chars"
	   $win mark gravity anchor:$name left
	   }
       }
}

proc xHMdo_/a  {} {
    uplevel 1 {
	foreach v [array names taglist h:*] { unset taglist($v) }
	catch {unset taglist(href)}
    }
}

proc xHMdo_body { win } {
    global xHMOptions
    upvar 1 params params
    upvar #0 xHMvar$win wvar
    set paramList [xHMsplitParams $params]
    foreach {key val } $paramList {
	catch { $win config -$key $val }
	set wvar(option,$key) $val
    }
}

proc xHMdo_img {} {
    upvar 1 params params
    upvar 1 wvar wvar
    upvar 1 taglist taglist
    upvar 1 win win
    set paramList [xHMsplitParams $params]

    xHMextract_param $paramList align bottom
    xHMextract_param $paramList border 1
    xHMextract_param $paramList width 0
    xHMextract_param $paramList height 0
    xHMextract_param $paramList src ""
#    xHMextract_param $paramList alt <image:[file tail $src]>
    xHMextract_param $paramList alt <image:$src>
    #puts "img:$src,$alt,$width,$height"
    if { [lsearch {bottom top center} $align ] < 0 } { set align bottom}
	set w $win.fr[incr wvar(counter)]
    set base ""
    set bg [$win cget -background]

    catch { set base $wvar(url) ; set base $wvar(baseurl) }
    if { [catch { set im [xHMgetImage $win $src $base $width $height] }] } {
	error "dont get here now"
	frame $w -width $width -height $height -background $bg
	label $w.label -text $alt -background $bg
	if { $width && $height } { pack  propagate  $w 0 }
	pack $w.label -fill both -expand 1
    } else {
	if { $wvar(measure) >= 0 } {
	    incr wvar(measure) [image width $image]
	}
	label $w -image $im -background $bg
	bind $w <Enter> [list set maxima_priv(load_rate) "$alt" ]
	bind $w <Leave> [list set maxima_priv(load_rate) ""  ]

    }
    catch { $w configure -border $border}
    set href [lindex [array names taglist h:*] 0]
    if { "$href" != "" }  {
	bind $w <1> "OpenMathOpenUrl [string range $href 2 end] \
			-commandpanel [omPanel $win]"
    }
    foreach v [array names taglist] { $win tag add $v $wvar(W_insert)}
    $win window create $wvar(W_insert) -window $w -align $align -padx 1 -pady 1


## to do add links for call backs
}

# return an image object..
proc xHMgetImage {win src baseurl width height } {
#     puts "$win,$src,$baseurl,$width,$height"
#     puts "getImage [resolveURL $src [decodeURL $baseurl]] $width $height"
    return [getImage [resolveURL $src [decodeURL $baseurl]] $width $height]
}

proc xHMget { url } {
}

proc xHMlistEnter {} 	{
    uplevel 1 {
	xHMassureNewlines [expr {($wvar(indent) < 2 ?  1 : 0)}]
	set _ii [expr {(($wvar(indent) <= 0  ) ? 2 : 1)}]
	xHMpushAindent $win $_ii
	catch { unset wvar(listindex$wvar(indent))}
    }
}

proc xHMlistExit {} 	{
    uplevel 1 {
	set _ii [expr {($wvar(indent) <= 2) ? 2 : 1}]
	xHMpopAindent $win $_ii
	xHMassureNewlines [expr {($wvar(indent) < 2 ?  1 : 0)}]
	
    }
}

proc dupString { s n } {
    set ans ""
    while { [incr n -1] >= 0 } { append ans $s }
    return $ans
}

### to do fix this to see how many blank lines there are at our insert
### point and to insert ones to make up.
proc xHMassureNewlines { n } {

    uplevel 1 set _n $n
    uplevel 1 {
	set _have 0
	foreach _v [lrange [split [$win get "$wvar(W_insert)-4char" $wvar(W_insert)] \n] 1 end] {
	    if { [string trim "$_v"  " "] == "" } {
		incr _have
	    } else {
		set _have 0
	    }
	}
#    set _have  [$win  compare $wvar(W_insert) == "$wvar(W_insert) linestart"]
	xHMtextInsert $win [dupString "\n" [expr {$_n - $_have}]]
    }
}

proc xHMsetDefaultPreferences {} {
    global maxima_default tcl_platform

    if { "$tcl_platform(platform)" == "unix" } {
	set pairs {  1 8
	    2 10
	    3 12
	    4 14
	    5 18
	    6 24
	    7 24
	    8 34	
	}
    } else {
	set pairs {  1 6
	    2 8
	    3 8
	    4 10
	    5 12
	    6 14
	    7 16
	    8 18	
	}
    }

    foreach fam {propor fixed} {
	foreach {n si} $pairs { set maxima_default($fam,$n) $si}
    }
    set maxima_default(propor,adjust) [expr {$maxima_default(adjust) + 0}]
    set maxima_default(fixed,adjust) [expr {$maxima_default(adjust)  + 0}]
    array set maxima_default { propor arial fixed courier  indentwidth .7 }
}

xHMsetDefaultPreferences
catch { source ~/.xmaximarc }

proc dputs {x} {
    puts $x ; flush stdout
}

proc xHMinit_state { win args } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist
    global maxima_default
    array set saveme [array get wvar W_*]
    catch { unset wvar}
        catch { unset taglist}
    array set wvar {
	family propor   weight normal   style r   size 3
	list list
	indent 0
	adjust 0
	measure -1
	W_insert insert
	W_update 15
    }
    array set wvar [array get saveme]
    array set taglist {indent:0 1}

}

proc xHMrender { win tag  params text } {
    global xHMtag
    upvar #0 xHMtaglist$win taglist
    upvar #0 xHMvar$win wvar
    set prefix ""

    set tag [string tolower $tag]
    # the following will go in a catch after debugging:
    #dputs "doing <$tag>"
    #dputs text=<<$text>>
    # puts "xHMtag($tag)=[set xHMtag($tag)]"


   # eval [set xHMtag($tag)]
    if { [info exists xHMtag($tag)] } {
	# if { [catch { eval [set xHMtag($tag)] }] } { puts [concat [mc "error evaling tag:"] "$tag"] }
	eval [set xHMtag($tag)]
    } else {
	if { [string match "!--*" $tag] } { list} else {
	#puts "undefined $tag: puts comment:$text"
    }
		}


    if { [regexp & $text] }  {
       set text [xHMconvert_ampersand $text]
    }

    #dputs "nowrap=[info exists taglist(nowrap)]"
    if { ![info exists taglist(nowrap)] } {
	regsub -all "\[ \t\r\n\]+" $text " " text
	if { "$prefix" != "" } { set text [string trimleft $text] }
    }
    xHMtextInsert $win $prefix$text
}

# make a copy of it.
proc xHMrender_orig [info args xHMrender] [info body xHMrender]


proc xHMtextInsert { win text } {
    global xHMtaglist$win
    upvar #0 xHMvar$win wvar
    # dputs "$win insert $wvar(W_insert) [list $text] [list [array names xHMtaglist$win ]]"
    # we calculate the longest unbroken line...
    if { 0 && $wvar(measure) >= 0 } {
	# puts "hi"
	set fo [xHMmapFont  $wvar(font)]
	set lis [split $text \n]
	set ll [font measure $fo [lindex $lis 0]]
	incr wvar(measure) $ll
	foreach vv [lrange $lis 1 end] {
	    maxIn wvar(maxwidth) $wvar(measure)
	    set wvar(measure)   [font measure $fo $vv]
	}
	maxIn wvar(maxwidth) $wvar(measure)
    }
    $win insert $wvar(W_insert) $text [array names xHMtaglist$win ]
}

proc xHMset_state { win args } {
    upvar #0 xHMvar$win wvar

    array set wvar $args

}

proc toPixelWidth { dim win } {
    if { [regexp {([.0-9]+)c} $dim junk d] } {
	return [expr {round($d*[winfo screenwidth $win] /(.1*[winfo screenmmwidth $win]))}] } else {
		return $dim}
    }
	

proc xHMinit_win { win } {
    upvar #0 xHMvar$win wvar
    global maxima_default
    # global xHMvar$win
   # catch { unset xHMvar$win }
    xHMinit_state $win
    $win config -font [xHMmapFont font:fixed:normal:r:3]
    catch { eval destroy [winfo children $win] }
    set iwidth [toPixelWidth  [set maxima_default(indentwidth)]c $win]
    # puts iwidth=$iwidth
    for { set i 0 } { $i < 12 } { incr i } {
	set half [expr {$iwidth/2.0 }]
	set w [expr {$i * $iwidth}]
	$win tag configure indent:$i -lmargin1 ${w} -lmargin2 ${w} -tabs \
		"[expr {$w + $half}] [expr {$w + 2*$half}]"
    }
   # $win tag bind doaref <Enter> "HMdoaref enter $win %x %y"
   # $win tag bind doaref <Leave> "HMdoaref leave $win %x %y"
   # $win tag bind doaref <1> "HMdoaref click $win %x %y"

    $win tag configure indent:0 -lmargin1 ${half} -lmargin2 ${half} -tabs "${half} [expr {2 * $half}]"
    $win tag configure href -borderwidth 2 -foreground blue -underline 1

    $win tag configure nowrap -wrap none
    $win tag configure rindent -rmargin $iwidth
    $win tag configure strike -overstrike 1

    $win tag configure underline -underline 1
    $win tag configure center -justify center
    $win configure -wrap word
}

global HMdefaultOptions
set HMdefaultOptions {
    {atagforeground blue "foreground for <a href=...>  tags"}
    {currenthrefforeground red "foreground of current <a href=..> tags"}
    {foreground black "foreground"}
    {background white "background "}
    {atagbackground blue "background for <a href=...>  tags" }
}

foreach v $HMdefaultOptions {set HMOption([lindex $v 0]) [lindex $v 1] }

proc xHMwget { win key dflt } {
    upvar #0 xHMvar$win wvar
    if { [info exists wvar($key)] } {
	return $wvar($key)
    } else {
	return $dflt
}
		}

proc HMdoaref { action win x y } {
    global HMOption
    set tags [$win tag names  @$x,$y ]
    set i [lsearch $tags h:*]
    set tag [lindex $tags $i]
    set reference [string range [lindex $tags $i] 2 end]
    # puts "$action $x $y"do_a
    switch -- $action {
	enter {
	    if { $i >= 0  }  {
		set ranges [$win tag ranges $tag]
		eval $win tag add currenthref $ranges
		textShowHelp $win currenthref @$x,$y [concat [mc "Click to follow link to"] "$reference"]

		$win tag bind $tag <Leave> "deleteHelp $win ;$win tag remove currenthref $ranges"
		$win tag  config currenthref -foreground [xHMwget $win option,atagforeground $HMOption(currenthrefforeground)] }
	    }
	click {
	    if { $i>= 0 } {
		global [oarray $win]
		if { [info exists [oloc $win dontopen]] } {
		    unset [oloc $win dontopen]
		} else {
		    oset $win dontopen 1
		    OpenMathOpenUrl $reference \
			    -commandpanel [omPanel $win]
		    catch {  unset [oloc $win dontopen] }
		}
		    return
	    }

	}
	    leave {
		
		$win tag delete currenthref
	    }
	}
    }

proc xHMdo_isindex {} {
    uplevel 1 {
	set paramList [xHMsplitParams $params]
	xHMextract_param $paramList prompt [mc " Enter search keywords: "]
	xHMtextInsert $win $prompt
	set w $win.entry[incr wvar(counter)]
	entry $w
	# puts "wvar=[array get wvar]"
        $win window create $wvar(W_insert) -window $w  -padx 1 -pady 1
	bind $w <Return> "xHMget $wvar(url)?\[xHMencode_get \[$w get\]\]"
    }
}

# encode a string where
#  " " --> "+"
#  "\n" --> "%0d%0a"
#  [a-zA-Z0-9] --> self
#   c --> [format %.2x $c]

# make a list of all characters, to get char code from char.
global xHMallchars
set xHMallchars ""
for { set i 1} { $i <256 } {incr i } { append xHMallchars [format %c $i] }

proc xHMhexChar { c } {
    global xHMallchars
    set i [string first $c $xHMallchars]
    return %[format %.2x [expr {$i + 1}]]
}

# "ISO 8879-1986//ENTITIES Added Latin 1 substitutions
array set isoLatin1 {
    	AElig \xc6 	Aacute \xc1 	Acirc \xc2 	Agrave \xc0
	Aring \xc5 	Atilde \xc3 	Auml \xc4 	Ccedil \xc7
	ETH \xd0 	Eacute \xc9 	Ecirc \xca 	Egrave \xc8
	Euml \xcb 	Iacute \xcd 	Icirc \xce 	Igrave \xcc
	Iuml \xcf 	Ntilde \xd1 	Oacute \xd3 	Ocirc \xd4
	Ograve \xd2 	Oslash \xd8 	Otilde \xd5 	Ouml \xd6
	THORN \xde 	Uacute \xda 	Ucirc \xdb 	Ugrave \xd9
	Uuml \xdc 	Yacute \xdd 	aacute \xe1 	acirc \xe2
	acute \xb4 	aelig \xe6 	agrave \xe0 	amp \x26
	aring \xe5 	atilde \xe3 	auml \xe4 	brvbar \xa6
	cb \x7d 	ccedil \xe7 	cedil \xb8 	cent \xa2
	copy \xa9 	curren \xa4 	deg \xb0 	divide \xf7
	eacute \xe9 	ecirc \xea 	egrave \xe8 	eth \xf0
	euml \xeb 	frac12 \xbd 	frac14 \xbc 	frac34 \xbe
	gt \x3e 	hibar \xaf 	iacute \xed 	icirc \xee
	iexcl \xa1 	igrave \xec 	iquest \xbf 	iuml \xef
	laquo \xab 	lt \x3c 	micro \xb5 	middot \xb7
	nbsp \xa0 	not \xac 	ntilde \xf1 	oacute \xf3
	ob \x7b 	ocirc \xf4 	ograve \xf2 	ordf \xaa
	ordm \xba 	oslash \xf8 	otilde \xf5 	ouml \xf6
	para \xb6 	plusmn \xb1 	pound \xa3 	quot \x22
	raquo \xbb 	reg \xae 	sect \xa7 	shy \xad
	sup1 \xb9 	sup2 \xb2 	sup3 \xb3 	szlig \xdf
	thorn \xfe 	times \xd7 	uacute \xfa 	ucirc \xfb
	ugrave \xf9 	uml \xa8 	uuml \xfc 	yacute \xfd
	yen \xa5 	yuml \xff
}

proc xHMencode_get { str } {
    regsub -all "\[^a-zA-Z0-9\]" $str "\[xHMencode_get1 {x&x}]" str
    regsub -all "{x(\[{}\])x}" $str \{\\\\\\1x\} str
    return [subst  -novariables -nobackslashes $str ]
}

proc xHMencode_get1 { s } {
    set c [string index $s 1]
    switch -- $c {
	\n  { return %0d%0a }
	" " { return + }
	default { return [xHMhexChar $c ]}
    }
}


proc HexDecode { me }  {
    regsub -all {\+} $me " "  me
  if { [regexp % $me] } {
     regsub -all {\[} $me {[dec1 5b]} me
    regsub -all {%([0-9A-Fa-f][0-9A-Fa-f])} $me {[dec1 \1]}  me
    subst -nobackslashes -novariables $me
 } else {
		return $me }
}
proc dec1 { s } {
    if { [scan  $s %x d] } {
	format %c $d
    } else {
	error [concat [mc "cant decode hex"] "$s"]
    }
}




#
 #-----------------------------------------------------------------
 #
 # xHMparse_html --  takes HTML containing valid html code, and
 #  converts it into a sequence of calls to CMD.   These
 #  CMD should take 4 arguments:
 #     tagname slash tagArguments followingText
 #  where slash is {} or {/} depending on whether the TAGNAME was
 #  prefixed with a '/'.   The tagAguments are not parsed: eg
 #  <foo bil=good joe> hi there <next> this is
 #  would turn into
 #  $CMD {foo} {} {bil=good joe} {hi there}
 #  $CMD {next} {} {}   {this is..}
 #  We have tried to stay call compatible with a similar command
 #  written by Stephen Uhler.   Our handling of all the tags is different
 #  however.
 #
 #  Results: none
 #
 #  Side Effects: the sequence of $CMD is evald.
 #
 #----------------------------------------------------------------
#
proc xHMparse_html {html {cmd HMtest_parse} {firstTag hmstart}} {
    #dputs "beginning parse"

     global meee ; set meee $html;
     regsub -all {(['\"])\./\.\.} $html {\1..} html 
     regsub -- "^.*<!DOCTYPE\[^>\]*>" $html {} html
     regsub -all -- "--(\[ \t\n\]*)>" $html "\001\\1\002" html
     regsub -all -- "<--(\[^\001\]*)\001(\[^\002\]*)\002" $html \
	 {\&lt;--\1--\2\&gt;} html
     regsub -all -- "<!--\[^\001\]*\001(\[^\002\]*)\002"  $html {} html

     regsub -all \} <$firstTag>\n$html\n</$firstTag> {\&cb;} html
     #dputs "beginning parse1"
     regsub -all \{ $html {\&ob;} html
     # prevent getting \} \{ or \\n in a braces expression.
     regsub -all "\\\\(\[\n<>])" $html "\\&#92;\\1" html
     #regsub -all "<(/?)(\[^ \t\n\r>]+)\[ \t\n\r\]*(\[^>]*)>" $html \
	 "\}\n$cmd {\\2} {\\1} {\\3} \{" html
     regsub -all "<(\[^ \t\n\r>]+)\[ \t\n\r\]*(\[^>]*)>" $html \
	 "\}\n$cmd {\\1}  {\\2} \{" html
     # puts "<html=$html>"
     #dputs "beginning end splitparse1"

     #dputs "list {$html}"
     eval "list {$html}"

}

proc myPost { win menu } {
    bind $menu <Leave> "place forget $menu"
    place $menu -anchor center -relx 0 -rely 1.0 -bordermode outside -in $win
    raise $menu
}
## endsource myhtml.tcl
