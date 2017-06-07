# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Tryembed.tcl,v 1.7 2004-10-13 12:08:58 vvzhy Exp $
#
###### Tryembed.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

## the following worked to have an entry box that spoke...
# %     safe::interpCreate jack
# jack
# % set slave jack
# jack
# %     safe::interpInit $slave
# jack
# %     interp eval $slave set env(DISPLAY) $env(DISPLAY)
# :0.0
# %     load {} Tk $slave
# % interp eval jack {entry .ja ; pack .ja}
# %     interp eval $slave { proc policy {args } {} }
# %     Safesock_PolicyInit $slave
# %     setupUnknown $slave
# %     setupPrintVariables $slave
# % interp eval jack plot2d -xfun {sin(x)}




proc makeEmbedWin { parent width height } {
    global maxima_priv env auto_index
    set win $parent.embed[incr maxima_priv(counter)]
    set fr [frame $win -width $width -height $height -container 1]
    set slave tclet$maxima_priv(counter)
    safe::interpCreate $slave
    # make it exist somehow the autoload stuff doesnt make it a command
    if { [info exists auto_index(::safe::allowTk) ]  } {
	::safe::allowTk $slave [list -use [winfo id $fr]]
	::safe::TkInit $slave
	::safe::tkInterpInit $slave [list -use [winfo id $fr]]
	interp eval $slave [list set argv [list -use [winfo id $fr]]]	
    } else {
	safe::interpInit $slave
	interp eval $slave [list set argv [list -use [winfo id $fr]]]	
    }

    if { [info exists env(DISPLAY)] } {
	interp eval $slave set env(DISPLAY) $env(DISPLAY)
    }
    interp eval $slave { proc policy {args } {} }
    #    $slave alias bgerror bgerror
    load {} Tk $slave
    Safesock_PolicyInit $slave
    setupUnknown $slave
    setupPrintVariables $slave
    oset $fr slave $slave
    return $fr
}


proc setupUnknown { slave } {
    interp eval $slave {rename auto_load auto_load-orig}
    interp alias $slave auto_load1 {} auto_load1 $slave
    interp eval $slave { proc auto_load {args} {
	if { [eval auto_load1 $args] } { return 1 }
	uplevel 1 auto_load-orig $args
    }
    }
}



proc auto_load1 { slave name {namespace ""} } {
    if { "[info proc $name ]" != "" } {
	set arglist [info args $name]
	set theargs {}
	foreach v $arglist {
	    if { [info default $name $v theDefault] } {
		lappend theargs [list $v $theDefault]
	    } else {
		lappend theargs $v
	    }
	}
	interp eval $slave [list proc $name $theargs [info body $name]]
	return 1
    }
    return 0
}

proc setupPrintVariables { slave } {
    global printOption fontSize show_balloons getOp parse_table Parser     axisGray plot2dOptions plot3dOptions paperSizes  printOptions writefile     doExit  fontCourier8   plotdfOptions ftpInfo  maxima_priv
    foreach v {printOption fontSize show_balloons getOp parse_table Parser
	axisGray plot2dOptions plot3dOptions paperSizes  printOptions writefile
	doExit  fontCourier8   plotdfOptions ftpInfo maxima_priv} {
	if { [array exists  $v] } {
	    interp eval $slave [list array set $v [array get $v *] ]
	} else {
	    interp eval $slave [list set $v [set $v ]]
	}
    }

}
# proc tryit {{win .}} {
#  global  maxima_priv
#     if { ![info exists maxima_priv(counter)] } {
# 	set maxima_priv(counter) 0
#     }
#     set width [winfo width $win]
#     set height [winfo height $win]
#     if { $width <=1 } {set width 200}
#     if { $height <=1 } {set height 200}
#     set ff [makeEmbedWin $win $width $height]

#     return [list $ff [oget $ff slave]]
# }

if { "[info command policy]" != "policy" } {
    proc policy { args } { }
}

proc browser_log { args } {
    # puts "$args"
}

## source nsafesock.tcl

###### nsafesock.tcl ######
# The Safesock Security Policy.
# -----------------------------
#
# Author: Jacob Levy & Brent Welch, 3/10/97
#
# This policy allows a safe slave to connect to remote sockets under the
# control of a master. The URL from which the applet is classified as
# either "inside" or "outside" and the host is added to the set of "inside"
# and "outside" hosts that this Tclet is allowed to connect to. Then, on
# the first request to connect to a host, if the host is classified as
# "inside" then subsequently the Tclet is allowed to connect only to hosts
# that are classified as "inside" (the same for if the first attempt is to
# connect to a host classified as "outside").
#
# The arrays used to drive this policy are defined in safesock.data.

# Remember the location of the data file for the Safesock policy, so that
# it can be reloaded each time the policy is used, to reflect changes.

global safesockDataFile
set safesockDataFile [file join [file dirname [info script]] safesock.data]

proc Safesock_PolicyInit {slave {version 1.0}} {
    global browser_state		;# Browser state
    global safesock_inside safesock_outside

    interp alias $slave socket {} SafesockSocketAlias $slave
    interp alias $slave fconfigure {} SafesockFconfigureAlias $slave

    uplevel "#0" {source $safesockDataFile}


    # Attempt to get the URL and extract the server and port portions:

    set server "" ; set port "" ; set url ""
    catch {set url $browser_state($slave,url)}
    if {[regexp -nocase {http://([^:/]+)(:([0-9]+))?/} $url \
	     x server y port]} {
	if {[string length $port] == 0} {
	    set port 80
	}
	set server [string tolower $server]
    } elseif {[string match "file:*" $url]} {
	set server localhost
	set port 80
    }

    # At this time it is unknown whether the slave will use inside
    # or outside connections:

    set browser_state($slave,safesock,permissions) unknown

    # Save the homebase for this Tclet:

    set browser_state($slave,safesock,homebase) $server
    set browser_state($slave,safesock,homeport) [list $port 1025- ftp ping]

    # Tell the slave about itself:

    interp eval $slave [list set env(SERVER) $server]
    interp eval $slave [list set env(PORT) $port]
    interp eval $slave [list set env(URL) $url]

    browser_log $slave security installed policy Safesock
}

proc SafesockDecideInsideOrOutside {slave server} {
    global safesock_insideExclude safesock_outsideExclude
    global safesock_inside safesock_outside

    set status unknown

    # If the server matches anything outside and nothing in the outside
    # exclusion list, then it's outside:

    foreach i [array names safesock_outside] {
	if {[string match $i $server]} {
	    set status outside
	    break
	}
    }

    if {"$status" == "outside"} {
	foreach i [array names safesock_outsideExclude] {
	    if {[string match $i $server]} {
		set status unknown
		break
	    }
	}
    }

    # If the status is unknown, check whether it might be inside. It is
    # inside if the server matches anything inside and nothing in the
    # inside exclusion list:

    if {"$status" == "unknown"} {
	foreach i [array names safesock_inside] {
	    if {[string match $i $server]} {
		set status inside
		break
	    }
	}

	if {"$status" == "inside"} {
	    foreach i [array names safesock_insideExclude] {
		if {[string match $i $server]} {
		    set status unknown
		    break
		}
	    }
	}
    }

    # If the status is unknown at this point, raise an error

    if {"$status" == "unknown"} {
	error [concat [mc "unknown host:"] "$server"]
    }

    return $status
}

# This procedure is invoked when the slave is destroyed to clean up
# any associated state. It frees up the array of hosts and ports that
# the slave is allowed to connect to:

proc Safesock_PolicyCleanup {slave} {
    global browser_state

    foreach i [array names browser_state $slave,safesock,*] {
	unset browser_state($i)
    }
}


#
#-----------------------------------------------------------------
#
# SafesockServerAnswer --  will replace COMMAND in a `socket -server command'
#  request.   Checks if the incoming connection is allowed and if so
#  invokes the original command.   Allowed is based on the same criteria
#  as the outgoing connection.
#
#  Results: none
#
#  Side Effects: if connect is allowed, transfer the socket to the slave
#  and eval the original command there.
#
#----------------------------------------------------------------
#
proc SafesockServerAnswer { slave command sock host port } {
    set peer [fconfigure $sock -peername]
    set host [lindex $peer 1]
    set host [string tolower $host]
    if { [SafesockAllow $slave $host [lindex $peer 2]] > 0 } {
	interp transfer {} $sock $slave
	interp eval $slave $command $sock $host $port
    } else {
	interp eval $slave [list error [M [mc "connection from %s and %s disallowed"] "$host" "$port" ] ]
    }
}



#
#-----------------------------------------------------------------
#
# SafesockAllow --  check if connection by SLAVE to HOST at PORT is allowed,
#  based on the inside/outside history of slave and data in safesock.data
#
#  Results: 1 if succeeds and 0 if it fails to allow
#
#  Side Effects:  set GOOD to ok port in the caller
#
#----------------------------------------------------------------
#
proc SafesockAllow { slave host port} {
    global browser_state
    global safesock_insideExclude safesock_outsideExclude
    global safesock_inside safesock_outside
    upvar 1 good good
    set host [string tolower $host]
    if {"$browser_state($slave,safesock,permissions)" == "unknown"} {
	if {[catch {set this [SafesockDecideInsideOrOutside $slave $host]}]} {
	    if {"$host" == "$browser_state($slave,safesock,homebase)"} {
	        set this homebase
	    } else {
	        error [concat [mc "unknown host:"] "$host"]
	    }
	}
	set browser_state($slave,safesock,permissions) $this
	browser_log $slave security $slave classified as $this
    }

    set portset -
    if {"$browser_state($slave,safesock,permissions)" == "homebase"} {
	if {"$host" == "$browser_state($slave,safesock,homebase)"} {
	    set portset $browser_state($slave,safesock,homeport)
	}
    } elseif {"$browser_state($slave,safesock,permissions)" == "inside"} {
	foreach hostpat [array names safesock_inside] {
	    if {[string match $hostpat $host]} {
		set portset $safesock_inside($hostpat)
		break
	    }
	}
	if {"$portset" != "-"} {
	    foreach hostpat [array names safesock_insideExclude] {
		if {[string match $hostpat $host]} {
		    set portset -
		    break
		}
	    }
	}
    } else {
	foreach hostpat [array names safesock_outside] {
	    if {[string match $hostpat $host]} {
		set portset $safesock_outside($hostpat)
		break
	    }
	}
	if {"$portset" != "-"} {
	    foreach hostpat [array names safesock_outsideExclude] {
		if {[string match $hostpat $host]} {
		    set portset -
		    break
		}
	    }
	}
    }

    if {"$portset" == "-"} {
	error [concat [mc "unknown host:"] "$host"]
    }

    if { [safesockPortMatches $port $portset] } {
	set good $port
	return 1
    }
    return 0
}

proc safesockPortMatches { port portset } {
    foreach portspec $portset {
	set low [set high ""]
	if {[regexp {^([0-9]+)-([0-9]*)$} $portspec x low high]} {
	    if {($low <= $port && $high == "") ||
		($low <= $port && $high >= $port)} {
                return 1
		break
	    }
	} elseif {$port == $portspec} {
	    return 1
	}
    }
    return 0
}

# the following should be set in safesock.data
global safesockAllowedServerPorts
if { ![info exists safesockAllowedServerPorts ] } {
    set safesockAllowedServerPorts { 1025-3000 }
}

proc SafesockSocketAlias {slave host port args} {
    global safesockAllowedServerPorts
    set option {}
    if { "$host" == "-server" } {
	set command $port
	set port [lindex $args 0]
	if { ![safesockPortMatches $port $safesockAllowedServerPorts] } {
	    error [concat [mc "bad port:"] "$port"]
	}
	set sock [socket -server \
		      "SafesockServerAnswer $slave [list $command]" $port]
	interp transfer {} $sock $slave
	browser_log $slave normal socket -server $port
	return $sock
    } elseif { "$host" == "-async" } {
	set option $host
	set host $port
	set port [lindex $args 0]
    } else {
	if { [llength $args ] != 0 } {
	    error [mc "wrong args: socket host port OR socket -server command port"]
	}
	set serverCommand ""
    }
    SafesockAllow $slave $host $port
    if [info exists good] {
	if { "$option" != "" } {
	    set sock [interp invokehidden $slave socket $option $host $good]
	} else {
	    set sock [interp invokehidden $slave socket $host $good]
	}
	browser_log $slave normal socket $host $port
	return $sock
    }
    error [concat [mc "bad port:"] "$port"]
}

# This procedure handles the "fconfigure" alias from the slave:

proc SafesockFconfigureAlias {slave sock args} {
    global jack
    if {[llength $args] == 0} {
	return [interp invokehidden $slave fconfigure $sock]
    } elseif {[llength $args] == 1} {
	set flag [lindex $args 0]
	return [interp invokehidden $slave fconfigure $sock $flag]
    } else {
	browser_log $slave normal fconfigure $sock $args

	array set config [interp invokehidden $slave fconfigure $sock]
	foreach {flag value} $args {
	    switch -- $flag {
		-peername -
		-peerport {
		    error [concat [mc "Cannot change"] "$flag configuration"]]
		}
		-blocking -
		-buffering -
		-buffersize -
		-eofchar -
		-translation {
		    set config($flag) $value
		}
		default {
		    error [concat [mc "unknown option"] "$flag"]
		}
	    }
	}
	lappend jack [list interp invokehidden $slave fconfigure $sock \
			  -blocking $config(-blocking) \
			  -buffering $config(-buffering) \
			  -buffersize $config(-buffersize) \
			  -eofchar $config(-eofchar) \
			  -translation $config(-translation)]
	return [interp invokehidden $slave fconfigure $sock \
		    -blocking $config(-blocking) \
		    -buffering $config(-buffering) \
		    -buffersize $config(-buffersize) \
		    -eofchar $config(-eofchar) \
		    -translation $config(-translation)]
    }
}

## endsource nsafesock.tcl


## endsource tryembed.tcl
