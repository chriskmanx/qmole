# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Proxy.tcl,v 1.4 2004-10-13 12:08:58 vvzhy Exp $
#
###### Proxy.tcl ######



#
#-----------------------------------------------------------------
#
# openSocketAndSend --  open a Socket to HOST on PORT and then
# send the message MSG to it.   If verify is non 0, then read
# up through the end of the http header and verify this is not
# an error.
#
#  Results:  returns a socket which you can read from using ordinary
#  read and write, but to which you should write only using s
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc openSocketAndSend { host port msg { verify 0}} {
    global maxima_priv  pdata
    dtrace
    if { [info exists maxima_priv(proxy,http)] } {
	global pdata
	set magic "billy-[clock clicks]"
	debugsend "sendViaProxy $msg $host $port $magic"
	
	set sock [sendViaProxy $msg $host $port $magic]

	if { $verify } {
	    fconfigure $sock -blocking  1 -translation {crlf binary}
	    gets $sock tem
	    if { [regexp "503" $tem] } {
		error [concat [mc "Could not connect"] "$host $port"]
	    }
	    while { 1 } {
		gets $sock tem
		if  { [string length $tem] == 0 } { break }
	    }
	}
	set pdata($sock,proxyto) [list $host $port $magic]
	fconfigure $sock -blocking  0
	return $sock
	
	
    } else {
	set sock [socket $host $port]
	if {[info exists pdata($sock,proxyto)]} {
	    unset pdata($sock,proxyto)
	}
	fconfigure $sock -blocking  0
	puts -nonewline $sock $msg
	flush $sock
	return $sock
    }
}





#
#-----------------------------------------------------------------
#
# proxyPuts --  send the MESSAGE to SOCK, not appending a newline.
#
#  Results: none
#
#  Side Effects: message sent
#
#----------------------------------------------------------------
#
proc proxyPuts { sock  message } {
    global pdata
    debugsend "proxyPuts $sock  $message useproxy=[info exists pdata($sock,proxyto)]"
    if { [info exists pdata($sock,proxyto)] } {
	desetq "host port magic" $pdata($sock,proxyto)
	close [sendViaProxy $message $host $port $magic]
    } else {
	puts -nonewline $sock $message
	flush $sock
    }
}


#
#-----------------------------------------------------------------
#
# sendViaProxy --  send a message.
#  this is a private function.
#
#  Results: a socket one can read the answer from.
#   Caller is responsible for closing the socket.
#
#  Side Effects: socket opened and message sent as the body
#  of a post.  The magic is put in the http header request as the
#  filename	
#
#----------------------------------------------------------------
#
proc sendViaProxy { message host port magic  } {
    global maxima_priv
    dtrace
    set ss [eval socket $maxima_priv(proxy,http)]
    fconfigure $ss -blocking 0
    fconfigure $ss -translation {crlf binary}
    set request [getURLrequest http://$host:$port/$magic $host $port "" $message]
    debugsend "<$ss  request=$request>"
    puts $ss $request
    flush $ss
    return $ss
}

## endsource proxy.tcl

