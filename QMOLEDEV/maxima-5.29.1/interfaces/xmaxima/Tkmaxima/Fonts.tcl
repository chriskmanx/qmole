# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Fonts.tcl,v 1.8 2006-06-27 13:33:42 villate Exp $
#

# set font {Courier 8}
global maxima_priv fontCourier8 fixedFont fontSize
set maxima_priv(fixedFont) Courier

# Pick a default font size in pixels
set _screenheight [winfo screenheight .]
# if {$_screenheight < 500} {
#     # for 640x480
#     set _pixel 10
#     set _point 8
#     # Pick a default borderwidth which smaller
#     set _bd 1
# } elseif {$_screenheight < 700} {
#     # for 800x600
#     set _pixel 12
#     set _point 8
# } elseif {$_screenheight < 800} {
#     # for 1024x768
#     set _pixel 12
#     set _point 8
# } elseif {$_screenheight < 1100} {
#     # for 1200x1000
#     set _pixel 14
#     set _point 10
# } else {
#     set _pixel 18
#     set _point 12
# }

# ZW: this actually produces better result
set _pixel 10
set _point 8
set fontSize $_pixel

# setup defaults depending on the OS and Window Manager
# Really should do another version for mono
switch -exact -- $tcl_platform(platform) {
    windows {
	if {$tcl_platform(osVersion) < 5} {
	    set _prop_default {MS Sans Serif}
	} else {
	    set _prop_default Tahoma
	}
	set _fixed_default {Courier New}
    }
    default {
	set _prop_default {Bitstream Vera Sans}
	set _fixed_default {Bitstream Vera Sans Mono}
    }
}
# make sure these fonts are installed
set _allowed [string tolow [font families]]
foreach font [list $_prop_default "MS Sans Serif" Tahoma Arial Helvetica \
		 fixed system] {
    if {[lsearch -exact $_allowed [string tolow $font]] > -1} {
	set _prop_default $font
	break
    }
}
foreach font [list $_fixed_default Courier fixed system] {
    if {[lsearch -exact $_allowed [string tolow $font]] > -1} {
	set _fixed_default $font
	break
    }
}
set fontCourier8 [list $_fixed_default $_pixel]
set fixedFont [font create -family $_fixed_default -size $_pixel]
set buttonfont [font create -family $_prop_default -size $_pixel]


global maxima_default
set maxima_default(adjust) 0
# I think this is too crude and wont work with WM schemes
if {0} {
    catch {
	set width_ [expr {.9 * [winfo screenwidth .]}]
	if { [winfo width .] >= 500 } {
	    set width_ [winfo width .]
	}
	set maxima_default(adjust) [expr {
					  $width_<= 640 ? -1 :
					  $width_<= 800 ? 0 :
					  1 } ]
	unset width_
    }
}



######### font choosing utilities #########
global tcl_platform
global isFixedp

if { "$tcl_platform(platform)" == "unix" } {
    array set isFixedp {
	fixed 1 {fangsong ti} 1 {clearlyu alternate glyphs} 0 lucidatypewriter 1 charter 0 lucidabright 0 times 0 ming 1 {lucidux sans} 0 {open look glyph} 0 {song ti} 1 newspaper 0 helvetica 0 {open look cursor} 1 li 1 mincho 1 {clearlyu ligature} 0 {clearlyu pua} 0 {lucidux mono} 1 courier 1 clearlyu 0 utopia 0 lucida 0 nil 1 clean 1 terminal 1 kai 1 gothic 1 cursor 0 symbol 0 {clearlyu arabic extra} 0 {lucidux serif} 0 {new century schoolbook} 0 song 1
    }
}

proc getFontFamilies { fixed } {
    global isFixedp
    foreach font  [font families] {
	if { ![info exists isFixedp($font)] } {
	    set isFixedp($font) [font metrics [list $font] -fixed]
	}
	if { $isFixedp($font) == $fixed } {
	    lappend answer $font
	}
    }
    return [lsort $answer]
}


