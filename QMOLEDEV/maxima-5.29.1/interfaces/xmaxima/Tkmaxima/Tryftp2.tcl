# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Tryftp2.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
###### Tryftp2.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################
if { "[info commands vwait]" == "vwait" && "[info commands myVwait]" == "" } {
    proc myVwait { x  } {uplevel 1  vwait $x }
}

proc submitFtp { viahost host name password directory filename} {
    global ftpInfo

    if  { [catch { set sock [socket $viahost 80] } ] } {
	set sock [socket $viahost 4080]
    }
    set ftpInfo($sock,done) 0
    set len [string length $ftpInfo(data)]
    set ftpInfo($sock,data) $ftpInfo(data)

    # set sock [open /tmp/jim w+]
    fconfigure $sock -blocking 0 -translation {lf lf}
    # global billy ;lappend billy [list [fconfigure $sock]]
    puts $sock "POST /cgi-pub/wfs/submitftp HTTP/1.0"
    puts $sock "MIME-Version: 1.0"
    puts $sock "Accept: text/html"
    puts $sock "Accept: text/plain"
    puts $sock "Content-type: text/plain"
    puts $sock "Content-length: $len"
    puts $sock "Username: $name"
    puts $sock "Password: $password"
    puts $sock "Remote-host: $host"
    puts $sock "Remote-directory: $directory"
    puts $sock "Remote-filename: $filename"
    puts $sock ""
    flush $sock
    # puts $sock $ftpInfo(data) ; flush $sock
    # puts sock=$sock
    set ftpInfo(message) ""

    set after_id [after 10000 "set ftpInfo($sock,done) -1"]

    set ftpInfo($sock,datalength) $len
    set ftpInfo($sock,datanext) 0
    set ftpInfo($sock,log) "none.."
    # puts $sock $ftpInfo(data) ; flush $sock
    fileevent $sock writable "ftp2SendData $sock"
    fileevent $sock readable "ftp2WatchReturn $sock"
    myVwait ftpInfo($sock,done)
    set res $ftpInfo($sock,done)
    set ftpInfo(message) $ftpInfo($sock,log)

    after cancel $after_id

    # puts $ftpInfo($sock,return)
    ftp2Close $sock
    return $res
}

proc ftp2Close { sock } {
    global ftpInfo
    close $sock
    foreach v  [array names ftpInfo $sock,*]  {
	unset ftpInfo($v)
    }
}

proc ftp2WatchReturn { sock } {
    global ftpInfo

    append ftpInfo($sock,return) " watching ..."
    set new [read $sock ]
    #global billy ; lappend billy [list return $new]
    if { [eof $sock] } {fileevent $sock readable {}}
    # puts "watching..new=$new" ; flush stdout
    append ftpInfo($sock,return) $new
    if { [regexp "Succeeded: (\[^\n]*)\n" $ftpInfo($sock,return) junk msg]} {
	set ftpInfo($sock,done) 1
	set ftpInfo($sock,log) $msg
    } elseif { [regexp "Failed: (\[^\n]*)\n" $ftpInfo($sock,return) junk msg] } {
	set ftpInfo($sock,done) -1
	set ftpInfo($sock,log) $msg
    }
    #mike FIXME: this is a wrong use of after cancel
    after cancel "set ftpInfo($sock,done) -1"
    after 3000 "set ftpInfo($sock,done) -1"
}
# set billy {}
proc ftp2SendData { sock } {
    global ftpInfo

    set dn $ftpInfo($sock,datanext)
    set dl $ftpInfo($sock,datalength)
    #global billy ; lappend billy [list $dn $dl]
    set ftpInfo(percent) [expr {($dn >= $dl ? 100.0 : 100.0 * $dn/$dl)}]
    # puts "storing data to $sock $percent %"
    if { $ftpInfo($sock,datanext) >= $ftpInfo($sock,datalength) } {
	#mike FIXME: this is a wrong use of after cancel
	after cancel "set ftpInfo($sock,done) -1"
	after 10000 "set ftpInfo($sock,done) -1"
	fileevent $sock writable ""
	# puts $sock "abcdefghijklmno"
	# flush $sock
	return
    }
    set amtToSend 4000
    puts -nonewline $sock [string range $ftpInfo($sock,data) $ftpInfo($sock,datanext) [expr {$ftpInfo($sock,datanext) + $amtToSend -1}]]
    # puts  $sock $tosend
    flush $sock

    set ftpInfo($sock,datanext) [expr {$ftpInfo($sock,datanext) + $amtToSend}]
    #mike FIXME: this is a wrong use of after cancel
    after cancel "set ftpInfo($sock,done) -1"
    after 10000 "set ftpInfo($sock,done) -1"
}



## endsource tryftp2.tcl
