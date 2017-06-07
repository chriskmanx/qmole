# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Tkmaxima.tcl,v 1.7 2011-03-21 09:17:17 villate Exp $
#

#mike The following files are prepended, and could be sourced instead.
# The only problem about sourcing them is that the way of finding
# the directory they're in may differ in a wrapped executable.
# Note that the order of required files may be important.

# Source Tkmaxima/Constants.tcl 	;# required - must not be autoloaded
# Source Tkmaxima/Cygwin.tcl 		;# required - must not be autoloaded
# Source Tkmaxima/Preamble.tcl 		;# required - must not be autoloaded
# Source Tkmaxima/Readdata.tcl 		;# can be autoloaded
# Source Tkmaxima/Getdata1.tcl 		;# can be autoloaded
# Source Tkmaxima/Macros.tcl 		;# can be autoloaded
# Source Tkmaxima/Proxy.tcl 		;# can be autoloaded
# Source Tkmaxima/Send-some.tcl 	;# sets global variables
# Source Tkmaxima/Plotting.tcl 		;# sets global variables
# Source Tkmaxima/Fonts.tcl 		;# sets global variables
# Source Tkmaxima/Private.tcl 		;# can be autoloaded
# Source Tkmaxima/Getopt.tcl 		;# can be autoloaded
# Source Tkmaxima/Parse.tcl 		;# sets global variables
# Source Tkmaxima/Textinsert.tcl 	;# can be autoloaded
# Source Tkmaxima/Printops.tcl 		;# can be autoloaded
# Source Tkmaxima/Push.tcl 		;# can be autoloaded
# Source Tkmaxima/Plotconf.tcl 		;# can be autoloaded
# Source Tkmaxima/Adams.tcl 		;# can be autoloaded
# Source Tkmaxima/Rk.tcl 		;# can be autoloaded
# Source Tkmaxima/Plotdf.tcl 		;# can be autoloaded
# Source Tkmaxima/Plot2d.tcl 		;# defined globals
# Source Tkmaxima/Matrix.tcl 		;# can be autoloaded
# Source Tkmaxima/Plot3d.tcl 		;# defined globals
# Source Tkmaxima/NPlot3d.tcl 		;# can be autoloaded
# Source Tkmaxima/EOctave.tcl 		;# can be autoloaded
# Source Tkmaxima/EOpenplot.tcl 	;# can be autoloaded
# Source Tkmaxima/EMaxima.tcl 		;# can be autoloaded
# Source Tkmaxima/EHref.tcl 		;# can be autoloaded
# Source Tkmaxima/Browser.tcl 		;# defines globals
# Source Tkmaxima/Bindings.tcl 		;# defines bindings
# Source Tkmaxima/Wmenu.tcl 		;# can be autoloaded
# Source Tkmaxima/Tryftp2.tcl 		;# can be autoloaded
# Source Tkmaxima/Myhtml.tcl 		;# defines globals and tags
# Source Tkmaxima/Myhtml1.tcl 		;# can be autoloaded
# Source Tkmaxima/Base64.tcl 		;# can be autoloaded
# Source Tkmaxima/Bitmaps.tcl 		;# defines globals
# Source Tkmaxima/Tryembed.tcl 		;# defines globals?
# Source Tkmaxima/OpenMath.tcl 		;# active
# Source Tkmaxima/NConsole.tcl 		;# can be autoloaded
# Source Tkmaxima/String.tcl 		;# can be autoloaded
# Source Tkmaxima/Prefs.tcl 		;# can be autoloaded
# Source Tkmaxima/RunMaxima.tcl		;# can be autoloaded

# Source Tkmaxima/Menu.tcl
# Source Tkmaxima/Paths.tcl
# Source Tkmaxima/Gui.tcl
# Source Tkmaxima/Tkmaxima.tcl

proc vMaxUsage {script {error {}}} {
    set msg [mc "$error\n\nUsage: $script \[options\] \[filenames\]

Known options:
   -help, -h: Display this message
   -url site: Start browser at site 
   -use-version ver, -u ver: Launch maxima version ver
   -lisp flavor, -l flavor: Use lisp implementation flavor
"]
    tk_messageBox -type ok -icon info -title "Usage" -message $msg -parent .
    exit
}

proc lMaxInitSetOpts {} {
    global maxima_priv argv argv0
    set maxima_priv(opts) {}
    set maxima_priv(plotfile) {}
    set state key
    foreach arg $argv {
	switch -- $state {
	    key {
		switch -regexp -- $arg {
		    {^-h(elp)?$}       {vMaxUsage $argv0}
		    {^-url$}           {set state url}
		    {^-u(se-version)?$} {set state version}
		    {^-l(isp)?$}       {set state lisp}
		    {^--$}             {set state noopts}
		    {^-.*}             {vMaxUsage $argv0 "Unknown option $arg"}
		    default {
			lappend maxima_priv(plotfile) $arg
			set state file
		    }
		}
	    }
	    file {
		switch -glob -- $arg {
		    -* {vMaxUsage $argv0 "Misplaced option $arg"}
		    default {lappend plotfile $arg}
		}
	    }
	    url     {set maxima_priv(firstUrl) $arg; set state key}
	    version {lappend maxima_priv(opts) -u $arg; set state key}
	    lisp    {lappend maxima_priv(opts) -l $arg; set state key}
	    noopts  {lappend file $arg}
	}
    }
}

object_class MAXTkmaxima {

    method create {} {
	global tcl_platform maxima_priv

	if {$tcl_platform(platform) == "windows" } {

	    set dir [file dir [info name]]
	    # These should be in the same directory as the xmaxima.exe
	    set maxima_priv(kill) [file join $dir winkill.exe]

	    set file [file join $dir tclwinkill.dll]
	    if {[file isfile $file]} {
		catch {load  $file}
	    }
	    unset file
	} else {
	    # unix
	    set maxima_priv(kill) kill
	}

    }

    method install {} {
	global maxima_priv argv argv0 env fontSize maxima_default

	wm withdraw .
	wm title . [mc {Xmaxima: console}]

	set fr .maxima
	MAXGui gui
	set w [gui install $fr]

	#mike Defer looking for maxima until the interface has been built
	vMAXSetMaximaCommand

	#mike Defer the starting of maxima until the interface has been built
	if {[catch {runOneMaxima $w} err]} {
	    tide_failure [concat [mc "Error starting Maxima:"] "\n$err"]
	    return
	}
	after idle focus $maxima_priv(cConsoleText)
    }

    method exit {{text ""} {val "0"}} {
	global maxima_priv
	# save user settings for future sessions
	catch {savePreferences}
	update
	if {$text == ""} {
	    if {[info exists maxima_priv(cConsoleText)]} {
		set text $maxima_priv(cConsoleText)
	    }
	}
	
	if {$text != ""} {
	    if {[catch {closeMaxima $text} err]} {
		tide_failure $err
	    }
	}
	tkexit $val
    }


}

