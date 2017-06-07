# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Gui.tcl,v 1.8 2011-03-21 09:17:42 villate Exp $
#

object_class MAXGui {

    method __init__ {} {
	global tcl_platform maxima_priv
        
    }
    
    method install {fr} {
	global tcl_platform maxima_priv maxima_default
        
	if {$tcl_platform(platform) == "windows" && \
		[info commands winico] != ""} {
	    set file [file join \
			  $maxima_priv(maxima_xmaximadir) \
			  max.ico]
	    if {[file isfile $file]} {
		set ico [winico createfrom $file]
		winico setwindow . $ico
	    }
	}
        
	if {[winfo exists $fr]} {catch { destroy $fr }}

        
	######## make status panel....
	set st .status
	frame $st
        
	set maxima_priv(cStatusWindow) $st
	label $st.rate -width 35 -bd 1 -relief sunken \
	    -justify left \
	    -textvariable maxima_priv(load_rate) -anchor w
	scale $st.scale -showvalue 0 -length 200 \
	    -orient horizontal
	pack $st.rate -side left -fill x -expand 1 -anchor w
	pack $st.scale -side left
	pack $st -side bottom -fill x -anchor w
	set maxima_priv(cStatusLabel) $st.rate

        # Creates the browser in a separate window
	toplevel .browser
	wm title .browser [mc {Xmaxima: browser}]
	OpenMathOpenUrl $maxima_priv(firstUrl) -toplevel .browser
	set maxima_priv(cBrowser) .browse
        
        # Adds the menubar and the toolbar to the browser
	vMAXAddBrowserMenu .browser

 	# Adds the toolbar to the Maxima console
	vMAXAddSystemBar

	frame $fr
	pack $fr -expand 1 -fill both -side top

	set w $fr.text

	clearLocal $w
	oset $w heightDesired 80%
	set maxima_priv(maximaWindow) $w

	closeMaxima $w
	clearLocal $w

	# oset $w program $program
	oset $w prompt "% "
	if {[winfo exists $w]} {catch { destroy $w }}

	frame $fr.bottom -height 2
	$fr.bottom config -cursor double_arrow
	bind  $fr.bottom <B1-Motion> "changeSize $w %Y"
	pack $fr.bottom -side bottom -fill x

	text $w -yscrollcommand "$fr.scroll set" \
	    	-selectbackground yellow -selectforeground blue
	set maxima_priv($w,inputTag) input
#	resetMaximaFont $w
	scrollbar $fr.scroll -command "$w yview"
	pack $fr.scroll -side right -fill y
	pack $fr.text -expand 1 -fill both -side left

	$w mark set lastStart end
	$w mark gravity lastStart left
	bind $w <Configure> "resizeSubPlotWindows $w %w %h; resizeMaxima $w %w %h"

	$w configure -background white
	$w configure -foreground "#008600"
        $w tag configure input -foreground blue
	$w tag configure output -foreground black

	# binding order will be: window bindings, CNtext bindings,
	# OpenMathText bindings and default bindings (usually Text . all)
	# CNtext ans OpenMathText bindings are set up in Bindings.tcl
	bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

	if {![regexp -- input $maxima_priv(sticky)] } {
	    append maxima_priv(sticky) {|^input$}
	}
	set maxima_priv(cConsoleText) $fr.text
        
        vMAXSetCNTextBindings $w
        wm protocol . WM_DELETE_WINDOW [list tkmaxima exit $fr.text]

        # Sets up the console size and font
        $fr.text configure \
            -height $maxima_default(iConsoleHeight) \
            -width $maxima_default(iConsoleWidth)
        font configure ConsoleFont \
            -family [lindex $maxima_default(ConsoleFont) 0] \
            -size [lindex $maxima_default(ConsoleFont) 1]
        $fr.text configure -font ConsoleFont

 	# Adds the menu bar to the Maxima console
	vMAXAddSystemMenu $fr $fr.text

	wm deiconify .
 	return $w
    }

    method status {mess} {
	global maxima_priv
	set maxima_priv(load_rate) $mess
	$maxima_priv(cStatusLabel) configure -text $mess
    }

}

