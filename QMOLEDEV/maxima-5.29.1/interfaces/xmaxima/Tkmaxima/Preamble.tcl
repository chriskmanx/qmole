# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Preamble.tcl,v 1.4 2004-10-13 12:08:58 vvzhy Exp $
#
###### preamble.tcl ######

# get the number of clicks per second on this machine..
after idle {after 1000 "set maxima_priv(clicks_per_second) \[expr 1.0 *( \[clock clicks\] - [clock clicks])\]" }

catch {
    # the following will be defined only in the plugin
    global embed_args
    array set embed_args [getattr browserArgs]
    proc wm { args } {}
}

# from Send-some.tcl
#mike - I hope these can be eliminated or encapsulated
global port magic interrupt_signal _waiting _debugSend
if { $argc == 0 } {
    set port 4444
    set magic "billyboy"
}
set interrupt_signal "<<interrupt fayve>>"

set _waiting 0

set _debugSend 0

package require msgcat
namespace import msgcat::*

