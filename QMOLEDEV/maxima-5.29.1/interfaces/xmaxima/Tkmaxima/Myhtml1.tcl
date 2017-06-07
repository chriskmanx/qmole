# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Myhtml1.tcl,v 1.6 2002-09-14 17:25:34 mikeclarkson Exp $
#
###### Myhtml1.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

defTag eval -alter {family fixed Cnowrap nowrap adjust 0} \
    -body {
	set paramList [xHMsplitParams $params]
	if { [xHMextract_param $paramList program ""] } {
	    set wvar(evalPushed) "Teval"
	    xHMpushConstantTag $win Teval
	    foreach { k val } $paramList {
		if { "$k" == "doinsert" } {
		    set doinsert $val
		    if { "$doinsert" != "[defaultInsertMode $program]" } {
			lappend wvar(evalPushed) [list Targs -doinsert $doinsert]
			xHMpushConstantTag $win [list Targs -doinsert $doinsert]
		    }
		} else {
		    set tem "$k:$val"
		    xHMpushConstantTag $win $tem
		    lappend wvar(evalPushed) $tem
		}
	    }
	}
    }  -sbody {
	catch {foreach v $wvar(evalPushed) { xHMpopConstantTag $win $v } }
    }


defTag result  -alter {family fixed  weight bold  adjust 0} -body {
    set paramList [xHMsplitParams $params]
    set wvar(resultPushed) Tresult
    set taglist(Tresult) 1
    if { [xHMextract_param $paramList modified ""] } {
	lappend wvar(resultPushed) Tmodified
	set taglist(Tmodified) 1
    }
    if { [xHMextract_param $paramList name ""] } {
	lappend wvar(resultPushed) result:$name
	set taglist(result:$name) 1
    }
}   -sbody {
    catch {foreach v $wvar(resultPushed) { xHMpopConstantTag $win $v } }
}

defTag netmath -body {
    set paramList [xHMsplitParams $params]
    catch {
	if { [xHMextract_param $paramList version ""] } {
	    global maxima_priv
	    if { [clock scan $version] > [clock scan $maxima_priv(date)] } {

		xHMextract_param $paramList oldversion ""
		append oldversion $text
		set text $oldversion
	    }
	}
	#  swallow the following text if the browser is netmath enabled...
	if { [xHMextract_param $paramList swallow] } {
	    set text ""
	}

    }
}

defTag math -body {
    set paramList [xHMsplitParams $params]
    upvar #0 xHMtaglist$win taglist
    global xHMpriv 	 maxima_default
    set pre {$}
    if { [xHMextract_param $paramList display] } {
	set pre {$\displaystyle}
	xHMassureNewlines 1
    }

    set wc $win.c[incr xHMpriv(counter)]
    canvas $wc 		-background [$win cget -background] -highlightthickness 0 -borderwidth 0

    set si [expr {[lindex $wvar(size) end] + [lindex $wvar(adjust) end]}]
    if { [xHMextract_param $paramList size] } {
	catch { incr si $size }
    }
    set si [expr {($si < 1 ? 1 : ($si > 7 ? 7 : $si))}]

    set ptsize $maxima_default([lindex $wvar(family) end],$si)
    if { [regexp & $text] }  {
	set text [xHMconvert_ampersand $text]
    }
    if { [catch { set it [ $wc create stext 0 0 \
			       -anchor nw -stext "$pre $text \$" -pointsize $ptsize \
			      ] } ]  } {
	xHMpushConstantTag $win "center"
	xHMtextInsert $win $text
	xHMpopConstantTag $win "center"
	set text ""
	destroy $wc
    } else {
	set text ""
	set dims [$wc bbox $it]
	$wc config -width [lindex $dims 2] -height [lindex $dims 3]
	xHMpushConstantTag $win "center"
	xHMtextInsert $win " "
	$win window create $wvar(W_insert) -window $wc  -padx 1 -pady 1
	xHMpopConstantTag $win "center"
    }
} -sbody {list }

proc getDim { dim  max } {
    if { [regexp {([0-9.]+)%$} $dim junk amt] } {
	return [expr {round($amt * .01 * $max) }]
    } elseif { $dim  < 0 } {
	return $max
    } else {
	return $dim
    }
}

defTag embed  -body {
    set paramList [xHMsplitParams $params]
    xHMextract_param $paramList width -1
    # allow for things like 50%
    set width [getDim [set width] [expr {.95 * [winfo width $win]}]]
    xHMextract_param $paramList height -1
    set height [getDim [set height] [expr {.95 * [winfo height $win]}]]
    set ewin [makeEmbedWin $win $width $height]
    $win window create $wvar(W_insert) -window $ewin  -padx 1 -pady 1
    set slave [oget $ewin slave]
    if { [    xHMextract_param $paramList src] } {
	set data [HMgetURL $win $src text]
	interp eval $slave $data
    }
} -sbody {}


proc HMgetURL {  textwin url type } {
    set currentUrl [oget $textwin currentUrl]
    catch { set currentUrl [decodeURL [oget $textwin baseurl]] }
    set new [resolveURL $url $currentUrl ]
    return [uplevel 1 getURL [list $new] type]
}


## endsource "myhtml1.tcl"
