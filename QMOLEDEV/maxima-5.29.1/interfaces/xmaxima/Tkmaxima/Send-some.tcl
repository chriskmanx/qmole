# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Send-some.tcl,v 1.9 2011-03-14 20:01:13 villate Exp $
#
###### send-some.tcl ######

# Usage:
# catch {close $socket}
# source send-some.tcl ; openConnection $tohost $port $magic $program
# one linux14 do
# run-one.tcl octave  4448 billy1
# then from any machine do:
# can also open maxima at same time
# source send-some.tcl ; openConnection linux14 4448 billy1 octave
# then
# sendOneWait octave 2+3
# 5
# If you specified -debug when starting the server then you can
#    evaluate tcl commands in the process controlling 'program'
#   eg:  sendCommand octave "list 1 1"



#
#-----------------------------------------------------------------
#
# myVwait --  this is a replacement for vwait which is missing from
# the plugin tcl.   It is 'supposed' to be the same but in fact if it
# is a fileevent handler that is supposed to do the setting, then the
# fileevent handler might indeed get called continuously because the
# file becomes readable, and myVwait which was checking a variable that
# the handler set,  never gets a chance to return, since the handler
# is called again and again.   So Remove the handler when it is invoked.
# Note this uses tracing of the variable or array, and may interfere
# with other tracing.
#  Results:
#
#  Side Effects: waits till the variable is set if it was unset, or
# until its value is different.
#
#----------------------------------------------------------------
#
proc myVwait { var  } {
    global _waiting maxima_priv
    set tem [split $var "(" ]
    set variable [lindex $tem 0]
    global $variable
    lappend maxima_priv(myVwait) $variable


    set index ""
    if { [llength $tem ] > 1 } {
	set index [lindex [split [lindex $tem 1] ")" ] 0]
    }

    set action  "_myaction [list $index]"
    trace variable $variable w $action
    set _waiting 1

    while { [set _waiting] } {
        #puts "still waiting _waiting=$_waiting"
	update
    }
    set maxima_priv(myVwait) [ ldelete $variable $maxima_priv(myVwait)]
    trace vdelete $variable w $action
}

proc _myaction { ind name1 name2 op } {
    global _waiting
    # puts "action $ind $name1 $name2 $op"
    if { "$ind" == "$name2" } {
	
	global $name1
	set _waiting 0

    }

}

# proc myVwait { x args } {uplevel "#0"  vwait $x }
if { "[info commands vwait]" == "vwait"  } {
    proc myVwait { x  } {
        global maxima_priv
# Fix for Tcl 8.5: linking unreachable global variables used to be ignored
# in Tcl 8.4 but in 8.5 it raises an errror. The catch command should
# restore the Tcl 8.4 behavior. (villate, 20080513)
	catch {global $x}
	lappend maxima_priv(myVwait) $x
	vwait $x
	set maxima_priv(myVwait) [ ldelete $x $maxima_priv(myVwait)]
    }
}

proc omDoInterrupt { win } {
    foreach v [ $win tag names] {
	if { [regexp "com:pdata\\((\[a-z_A-Z]*)," $v junk program] } {
	    set var [string range $v 4 end]
	    # puts "interrupt program=$program,$var"
	    after 10 uplevel "#0" set $var <interrupted>
	    catch { sendInterrupt $program }
	}
    }
}


proc omDoAbort { win } {
    foreach v [ $win tag names] {
	set var [string range $v 4 end]
	if { [regexp "com:pdata\\((\[a-z_A-Z]*)," $v junk program] } {
	    set prog [programName $program]
	    if { "[info command abort_$prog]" != "" } {
		abort_$prog $program
		after 200 uplevel "#0" set $var <aborted>
	    }
	    cleanPdata $program
	    set var [string range $v 4 end]
	    # rputs "interrupt program=$program,$var"
	    after 200 uplevel "#0" set $var <aborted>
	}
    }
}



proc  msleep { n } {
    global Msleeping
    set Msleeping 1
    after $n "set Msleeping 0"
    debugsend "waiting Msleeping.."
    myVwait Msleeping
    debugsend "..donewaiting Msleeping"
}
proc message { msg } {
    global maxima_priv _debugSend
    if { $_debugSend } { puts "setting message=<$msg>" }
    catch { set maxima_priv(load_rate) $msg }
}
proc sendOne { program  com }  {
    global  pdata maxima_priv
    incr pdata($program,currentExpr)
    set socket $pdata($program,socket)

    if { [eof $socket] } {
        error [mc "connection closed"]
    }
    # puts "sending $program ([lindex [fconfigure $socket -peername] 1])"

    message [concat [mc "sending"] "$program" [mc "on"] "[lindex [fconfigure $socket -peername] 1]"]
    debugsend "sending.. {$com<$pdata($program,currentExpr)\|fayve>}"
    set msg "$com<$pdata($program,currentExpr)\|fayve>\n"
    proxyPuts $socket $msg
}


#
#-----------------------------------------------------------------
#
# sendOneDoCommand --  sends to PROGRAM the COMMAND and then
# when the result comes back it invokes the script CALLBACK with
# one argument appended: the global LOCATION where the result
# will be.   [uplevel "#0" set $LOCATION] would retrieve it.
#
#  Results: returns immediately the location that will be
#  watched.
#
#  Side Effects: CALLBACK is invoked later by tracing the
#  result field
#
#----------------------------------------------------------------
#
proc sendOneDoCommand {program command callback } {
    global pdata

    if { ![assureProgram $program 5000 2] } { return "cant connect"}

    set ii [expr {$pdata($program,currentExpr) + 1}]
    catch { unset pdata($program,results,$ii)}
    trace variable pdata($program,results,$ii) w \
	    [list invokeAndUntrace $callback]
    sendOne $program $command
    return pdata($program,results,$ii)
}

proc testit { program com } {
    sendOneDoCommand $program $com "jimmy"
    proc jimmy {s} { puts "<result is:[uplevel #0 set $s]>" ; flush stdout}
}

proc invokeAndUntrace { callback name1 name2 op args} {
    #puts "callback:$callback $name1 $name2 $op, args=$args"
    #puts "trace vdelete [set name1]($name2) w [list invokeAndUntrace $callback]"
    trace vdelete [set name1]($name2) w [list invokeAndUntrace $callback]
    lappend callback  [set name1]($name2)
    # puts "callback=$callback" ; flush stdout

    if { [catch { eval $callback } errmsg ] } {
	global errorInfo
	# report the error in the background
	set com [list  error [concat [mc "had error in"] "$callback:[string range $errmsg 0 300].."]	$errorInfo]
	after 1 $com
    }
}

proc sendOneWait { program com } {
    global pdata
    if { ![assureProgram $program 5000 2] } { return "cant connect"}
    set ii [expr {$pdata($program,currentExpr) + 1}]
    catch { unset pdata($program,results,$ii)}


    sendOne $program $com
    set i $pdata($program,currentExpr)
    set socket $pdata($program,socket)
    if { $ii != $i } { error "expected $ii got $i as expression number " }
    debugsend "waiting for pdata($program,results,$i)"

    myVwait pdata($program,results,$i)
    debugsend "..done waiting for pdata($program,results,$i)"
    return $pdata($program,results,$i)
}

proc closeConnection { program } {
    global pdata
    catch {
	set sock $pdata($program,socket)
	set pdata(input,$sock) ""
	cleanPdata $program
	close $sock

    }
}

proc dtrace { } {
    global _debugSend
    if { $_debugSend } {
	puts "at: [info level -1]"
	if { [info level]>2 } {puts "   from:[info level -2 ]"}
    }
}

proc openConnection { tohost port magic program } {
    global  pdata
    dtrace
    set msg "magic: $magic\n"
    set retries 2
    message [concat [mc "connecting to"] "nmtp($port)://$tohost/$program"]
    debugsend "openConnection { $tohost $port $magic $program }"

    while { [incr retries -1] > 0 \
	    && [catch { set socket [openSocketAndSend $tohost $port $msg 1] }] }   {
	debugsend retries=$retries
	msleep 400
    }

    if { $retries == 0 } { return 0}	

    message [concat [mc "connected to"] "nmtp//$tohost:$port/$program"]
    set pdata($program,socket) $socket
    set pdata($program,currentExpr) 0
    set pdata(input,$socket) ""
    catch { fconfigure $socket -blocking 0 }
    fileevent $socket readable "getResults $program $socket"
    return 1

}

proc sendInterrupt { program } {
    global pdata interrupt_signal
    set socket $pdata($program,socket)
    gui status [mc "Sending scoket interrupt"]
    puts $socket $interrupt_signal
    flush $socket
}

proc sendCommand { program c }   {
    global  pdata
    set socket $pdata($program,socket)
    puts $socket "<command:$c>"
    flush $socket
}

proc dumpInfo {program } {
    sendCommand $program dumpInfo
}

proc getResults {  program socket } {
    # debugsend "enter:getResults"
    global pdata  next_command_available next_command results ii
    if { [eof $socket] } {
	close $socket ;
	debugsend "closed $socket"
	cleanPdata $program
	return "<$program exitted>"
    }
    set s [read $socket]
    if { "[string index $s 0]" != "" } {
	set s [append pdata(input,$socket) $s]
	while { [set inds [testForFayve $s]] != "" } {
	    set input $pdata(input,$socket)
	    # set next_command_available 1
	    debugsend "input=$input"
	    set gotback [string range $input 0 [expr {[lindex $inds 0] -1}]]
	    set index [lindex $inds 2]
	    set pdata($program,results,$index) $gotback
    	    if { [string first "exitted>" $gotback] > 0 } {
		close $socket
		cleanPdata $program
	    }

	    debugsend "gotback{$index:$gotback}"
	    set s \
		    [string range $input [expr {1 + [lindex $inds 1]}] end ]
	    set pdata(input,$socket) $s
	}
    }
    return ""
}

proc cleanPdata { program } {
    global pdata
    catch { close $pdata($program,socket) }
    catch { unset pdata($program,socket) }
    catch { unset pdata($program,preeval) }
    catch {
	foreach v [array names $program,results,*] {
	    unset pdata($v)
	}
    }
}



# number from run-main.tcl
# set MathServer { linux1.ma.utexas.edu 4443 }

proc currentTextWinWidth { } {
    set width 79
    catch {
	set t [oget [omPanel .] textwin]
	set width [expr {round([winfo width $t]*1.0 / [font measure [$t cget -font] 0]) - 12 }]
    }
    return $width
}




#
#-----------------------------------------------------------------
#
# assureProgram --
#
#  Results: return 2 if the program was already open, and 1 if it is just
# now opened.   0 if cant open it.
#
#  Side Effects: program is started.
#
#----------------------------------------------------------------
#
proc assureProgram { program timeout tries } {
    # puts "assure: program=$program"
    global pdata MathServer


    if { $tries <=  0   } { return 0}

    if  { [catch { set socket $pdata($program,socket) } ] \
	    || [catch { eof $socket}] \
	    || [eof $socket] \
	    || [catch { set s [read $socket]; append pdata(input,$socket) $s }] } {
	cleanPdata $program
	message [concat [mc "connecting"] "[lindex $MathServer 0]"]
	set msg "OPEN [programName $program] MMTP/1.0\nLineLength: [currentTextWinWidth]\n\n\n"
	if {[catch {openSocketAndSend [lindex $MathServer 0] \
		[lindex $MathServer 1] "$msg\n"} sock] } {
	    error [concat [mc "Can't connect to"] "$MathServer." [mc "You can try another host by altering Base Program under the \"File\" menu."]]
	}
	
	set pdata($program,currentExpr) 0
	fconfigure $sock -blocking 0
	if { [eof $sock] } {return 0}
	message [concat [mc"connected to"] "[lindex $MathServer 0]"]
	debugsend $msg
	set result ""
	set pdata(waiting,$sock) 1
	set script "close $sock ; debugsend {after closing} ; set pdata(waiting,$sock) -1"
	debugsend "script=$script,timeout=$timeout"
	set af [after $timeout $script ]
	debugsend "after=$af"
	while {1 } {
	    debugsend "waiting pdata(waiting,$sock)=$pdata(waiting,$sock)"
	    #	    puts "pdata=[array get pdata *$sock* ]"
	    fileevent $sock readable "if { [eof $sock] }  {set pdata(waiting,$sock) -2} else { set pdata(waiting,$sock) 0 ;} ;fileevent $sock readable {} "
	    set pdata(waiting,$sock) 1
	    debugsend "waiting on  pdata(waiting,$sock)"
	    myVwait pdata(waiting,$sock)
	
	    debugsend "..done now pdata(waiting,$sock)=$pdata(waiting,$sock)"
	    if { $pdata(waiting,$sock) < 0 } {
		debugsend "timed out,$pdata(waiting,$sock)"
		return 0
	    }
	    set me [read $sock]
	    if { "[string index $me 0]" == ""  && [eof $sock] } {
		debugsend "nothing there"
		return 0
	    }
	    append result $me
	    debugsend "result=<$result>"
	    if { [regexp "RUNNING (\[^ \]+) MMTP\[^\n\]*\nHost: (\[^\n ]+)\nPort: (\[0-9\]+)\nMagic: (\[^\n \]+)\n" \
		    $result junk prog tohost port magic] } {
		after cancel $af
		debugsend "doing openConnection  $tohost $port $magic $program"
		close $sock
		return [openConnection  $tohost $port $magic $program]
	    }
	}
    } elseif { [eof $socket] } {
	close $socket
	unset pdata($program,socket)
	return [assureProgram $program $timeout [expr {$tries -1}]]
    } else {
	# already open
	return 2
    }
}

# name may look like "maxima#1.2"
proc programName { name } {
    set name [file tail $name]
    return [lindex [split $name #] 0]
}

global EOFexpr
set EOFexpr "|fayve>"

proc getMatch { s inds } {
    return [string range $s [lindex $inds 0] [lindex $inds 1]]
}

proc testForFayve { input } {
    global EOFexpr
    set ind [string first $EOFexpr $input]
    if { $ind < 0 } { return "" } else {
	regexp -indices {<([0-9]+)\|fayve>} $input all first
	
	set n [getMatch $input $first]
	return "$all $n"
    }
}

#### the following is correct but just a fair bit slower.. ####
##### because of all the arguments to be parsed for the other..
proc statServer1  {server {timeout 1000}} {
    global statServer
    set ans ""
    if { ![catch { set s [eval socket $server]} ] } {
	puts $s "STAT MMTP/1.0\n" ; flush $s
	if { [readAllData $s -tovar statServer(data) \
		-mimeheader statServer(header) -timeout $timeout ] > 0 } {
	    set head $statServer(header)
	    #	   puts "data=<$statServer(data)>"
	    set res $statServer(header)\n\n$statServer(data)
	    unset statServer
	    return $res
	}
    }
    return ""
}


#
#-----------------------------------------------------------------
#
# needToDo --  Check if we have already done OPERATION for  NAME into data
#
#  Results: returns 0 if the data for name is not preloaded, and 1 otherwise
#
#  Side Effects: adds NAME to those preloaded for PROGRAM if not there
#
#----------------------------------------------------------------
#
proc preeval { program name } {
    global pdata
    assureProgram $program 5000 2
    if { ![info exists pdata($program,preeval)] || \
    [lsearch  $pdata($program,preeval) $name] < 0 } {
	lappend pdata($program,preeval) $name
	return 0
    } else {
	return 1
    }
}



proc statServer  {server {timeout 1000}} {
    global statServer1_
    set ans ""
    if { ![catch { set s [eval socket $server]} ] } {
	puts $s "STAT MMTP/1.0\n" ; flush $s
	if { [readDataTilEof $s data $timeout ] } {
	    foreach v { jobs currentjobs } {
		if { [regexp "\n$v: (\[^\n]*)\n" $data junk val] } {
		    lappend ans $v $val
		}
	    }
	}
    }
    return $ans
}

proc isAlive1 { s } {
    global maxima_priv
    if { [catch { read $s } ] } {
	set maxima_priv(isalive) -1
    } else {
	set maxima_priv(isalive) 1
    }
    close $s
}

proc isAlive { server {timeout 1000} } {
    global maxima_priv

    if { [ catch { set s [eval socket -async $server] } ] } { return -1 }
    set maxima_priv(isalive) 0
    fconfigure $s -blocking 0
    fileevent    $s writable     "isAlive1 $s"
    set c1 "set maxima_priv(isalive) -2"
    set after_id [after $timeout $c1]
    myVwait maxima_priv(isalive)
    catch { close $s}
    after cancel $after_id
    return $maxima_priv(isalive)
}


proc debugsend { s } {
    global _debugSend
    if { $_debugSend } {
	
	puts $s
	flush stdout
    }
}


## endsource send-some.tcl
