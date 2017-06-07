# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Getdata1.tcl,v 1.7 2004-10-13 12:08:57 vvzhy Exp $
#
###### getdata1.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################





#
#-----------------------------------------------------------------
#
# readAllData --  read data from CHANNEL.
#  Options: -tovar variable    (store in this global variable)
#           -mimeheader store in alist the mime values
#                   and oset $sock contentlength if
#           -tochannel  (store in channel)
#           -timeout   (for non action)
#           -translation  (for the sock)
#           -chunksize   size to do for each read between updating %
#           -command    a call back run on each chunk
# If -command is not specified, wait and return the result code.
# Value of -1 means a timeout, and value of >1 means success.
# If  command is specified, call command each time data is read,
# with 1 argument appended, the result code.
# allowing no more than TIMEOUT millisconds between reads.
#  We set up local variables for the $CHANNEL
#        result
#        bytesread  (after the header if one specified)
#        mimeheader (extracted)
#        length     (0 if not provied by mime header)
# COMMAND can access
# to examine the data read so far.
#
#  Results:  1 on success, and -1 if it fails or times out.
#
#  Side Effects: CHANNEL will be closed and the global variable VAR will
#  be set..
#
#----------------------------------------------------------------
#


proc readAllData { sock args } {
    global readAllData [oarray $sock] maxima_priv

    array set [oarray $sock] {
	timeout 5000
	command ""
	tochannel ""
	translation binary
	chunksize 2024
	mimeheader ""
	tovar ""
	result ""
	done 0
	usecache 0
	percent 0
	bytesread 0
	headervalue ""
	contentlength -1
    }
    oset $sock begin [clock clicks]
    foreach { key val } $args {
	#puts "	oset $sock [string range $key 1 end] $val"
	oset $sock [string range $key 1 end] $val
	
    }
    #puts "locals:[array get [oarray $sock]]"
    # puts "args=$args"
    if { "[oget $sock translation]" != "" } {
	fconfigure $sock -translation [oget $sock translation]
    }
    fconfigure $sock -blocking 0

    catch { 
	$maxima_priv(cStatusWindow).scale \
	    config -variable [oloc $sock percent] 
    }
    lappend [oloc $sock after] [after [oget $sock timeout] "oset $sock done -1"]
    if { "[oget $sock mimeheader]" != "" } {
	fileevent  $sock readable "readMimeHeader $sock"
    } else {
	fileevent  $sock readable "readAllData1 $sock"
    }

    if { "[oget $sock command]" == "" } {
	oset $sock docommand 0
	return [wrWaitRead $sock]
    } else {
	oset $sock docommand 1
	# the command will do things and maybe caller will vwait..
	return ""
    }
}



#
#-----------------------------------------------------------------
#
# readMimeHeader --  read from SOCK until end of mime header.
#  this is done as a fileevent.  Store result in $sock local HEADERVALUE.
#
#  Results: none
#
#  Side Effects:  data read, and the mime header decoded and stored.
#
#----------------------------------------------------------------
#
proc readMimeHeader { sock } {
    global [oarray $sock]
    set result ""
    set ans ""
    while { 1 } {
	set n [gets $sock line]
	if { $n < 0 } {
	    if { [eof $sock] } {
		oset $sock done -1
		close $sock
		return
	    }
	    append [oloc $sock result] $result\n
	    break
	}
	if { $n <=1 && ($n==0 || "$line" == "\r") }  {
	    # we are done the header
	
	    append [oloc $sock result] $result\n
	    regsub -all "\r"  [oget $sock result] "" result
	    set lis [split $result \n]
	    foreach v $lis {
		if { [regexp "^(\[^:]*):\[ \t]*(.*)\$" $v junk key val] } {
		    lappend ans  [string tolower $key] $val
		}
	    }
	    oset $sock headervalue $ans
	    oset $sock contentlength [assoc content-length $ans -1]
	    if { [oget $sock usecache] } {
		set result [tryCache [oget $sock cachename] $ans]
		if { "$result" != "" } {
		    oset $sock bytesread [string length $result]
		    wrFinishRead $sock
		    return
		}
	    }
	    oset $sock percent 0
	    oset $sock bytesread 0
	    oset $sock result ""
	    #puts "mimeheader = <$ans>"
	    #puts "switching to readAllData1 $sock, [eof $sock]"
	    fileevent $sock readable "readAllData1 $sock"
	    #puts "doing     readAllData1 $sock"
	    return
	}
	append result "$line\n"
    }
}


proc readAllData1 { sock } {
    #puts "readAllData1 $sock" ; flush stdout
    global maxima_priv [oarray $sock]

    makeLocal $sock timeout tovar tochannel docommand chunksize after contentlength begin

    upvar #0 [oloc $sock bytesread] bytesread
    #puts "readAllData1 $sock, bytes=$bytesread" ; flush stdout
    if { [catch {
	foreach v $after {after cancel $v}
	while { 1 } {
	    if { "$tochannel" != "" } {
		if { [eof $sock] } {
		    wrFinishRead $sock
		    return finished
		} else {
		    set amt [expr { $contentlength >= 0 ? ($chunksize < $contentlength - $bytesread ? $chunksize : ($contentlength -$bytesread)) : $chunksize } ]
		    set chunksize $amt
		    set n [fcopy $sock $tochannel -size $chunksize]
		}
	    } else {
		set res [read $sock $chunksize]
		set n [string length $res]
		append [oloc $sock result] $res
	    }
	    incr bytesread $n
	    if { $n == 0 } {
		if { [eof $sock] } {
		    wrFinishRead $sock
		    return finished
		}
	    }
	    set maxima_priv(load_rate) "[expr {round ($bytesread * ($maxima_priv(clicks_per_second)*1.0 / ([clock clicks] - $begin)))}] bytes/sec"

	    if { $contentlength > 0 } {
		oset $sock percent \
		    [expr {$bytesread * 100.0 / $contentlength }]
		
	    }

	    if { $docommand } {
		catch { uplevel "#0"  [oget $sock command] }
	    }
	    # puts "percent=[oget $sock percent],bytes=[oget $sock bytesread]"

	    if { $contentlength >= 0 &&  $bytesread >= $contentlength } {
		wrFinishRead $sock
		return finished
	    }
	    if { $n <= $chunksize } { break    }
	
	}
    } errmsg   ] } {
	if { "$errmsg" == "finished" } {
	    return
	} else {
	    global errorInfo ; error [concat [mc "error:"] "$errmsg , $errorInfo"]
	}
    }
    lappend [oloc $sock after] \
	[after $timeout "oset $sock done -1"]
}




#
#-----------------------------------------------------------------
#
# wrFinishRead --  run at the EOF.  It will run the COMMAND one last
# time and look after setting the global variables with the result,
# closing the channel(s).
#
#  Results:  the $sock variable 'done', 1 for success, -1 for failure.
#
#  Side Effects: many!
#
#----------------------------------------------------------------
#
proc wrFinishRead { sock } {
    makeLocal $sock mimeheader contentlength tovar tochannel headervalue \
	bytesread docommand
    #puts "entering wrFinishRead" ; flush stdout

    if { "$mimeheader" != "" } {
	uplevel "#0" set $mimeheader \[oget $sock headervalue\]
    }
    if { "$tovar" != "" } {
	uplevel "#0" set $tovar \[oget $sock result\]
    } else {
	catch { close $tochannel }
    }
    if { $contentlength < 0 || $bytesread >= $contentlength } {
	oset $sock done 1
    } else {
	oset $sock done -1
    }
    catch { close $sock }	
    if { $docommand } {
	catch { uplevel "#0"  [oget $sock command]  }
    }
    set res [oget $sock done]
    #puts "wrFinishRead, tovar=$tovar,tochannel=$tochannel,res=$res,bytesread=$bytesread"	
    clearLocal $sock
    oset $sock done $res

    return $res
}

proc wrWaitRead { sock } {
    #puts "entering wrWaitRead"
    global [oarray $sock]
    if { [oget $sock done] == 0 } {
	myVwait [oloc $sock done]
    }
    #vwait [oloc $sock done]
    set res [oget $sock done]
    return $res
}

proc testit { addr usecommand args } {
    if { [regexp {//([^/]+)(/.*)$} $addr junk server path] } {
	set sock [socket $server 80]
	#puts "server=$server"
	# fconfigure $sock -translation binary
	#puts "GET $path HTTP/1.0\n"
	puts $sock "GET $path HTTP/1.0\nMIME-Version: 1.0\nAccept: text/html\n\nhi there" ;
	flush $sock
	proc _joe { sock } {
	    makeLocal $sock percent contentlength bytesread
	    puts "percent=$percent,contentlength=$contentlength,bytesread=$bytesread"
	}
	if { $usecommand } {	
	    eval readAllData $sock -command  [list "_joe $sock"] $args
	    wrWaitRead $sock
	} else {
	    eval readAllData $sock $args
	}
	catch { close $sock }
    }
}



#
#-----------------------------------------------------------------
#
# tryGetCache --  look up PATH (eg http://www.ma.utexas.edu:80/...)
# in the cache, and if you find success and a matching ETAG,
# then return the data in the file
#
#  Results:  The cached data in FILE or ""
#
#  Side Effects: Will remove the file if the current etag differs.
#
#----------------------------------------------------------------
#
proc tryGetCache { path alist } {
    global ws_Cache maxima_priv
    set tem [ws_Cache($path)]
    if { "$tem" != "" } {
	set filename [file join $maxima_priv(cachedir) [lindex $tem 1]]
	set etag [assoc etag $alist]
	if { "$etag" != "" } {
	    if {  "[lindex $tem 0]" == "$etag" }  {
		if { ! [catch {
		    set fi [open $filename  r]
		}] } {
		    fconfigure $fi -translation binary
		    set result [read $fi]
		    close $fi
		    return $result
		}
	    } else {
		# cache out of date.
		if { [file exists $filename] } {
		    file delete $filename
		    return ""
		}
	    }

	}
    }
}


proc saveInCache { path etag  result} {
    global ws_Cache maxima_priv
    set cachedir $maxima_priv(cachedir)
    # todo add a catch
    set type [lindex [split [file tail $path] .] 1]
    set count 0
    while [ file exists [set tem [file join $cachedir $count$etag.$type]]] {
	incr count
    }
    set fi [open $tem w]
    #puts "writing $tem"
    fconfigure $fi -translation binary
    puts -nonewline $fi $result
    close $fi
    set ws_Cache($path) [list $etag [file tail $tem]]
    set fi [open [cacheName index.dat] a]
    puts $fi "[list [list $path]] {$ws_Cache($path)}"
    close $fi
}

proc cleanCache { } {
    global ws_Cache
    catch {
	foreach v [glob [cacheName *]] {
	    catch { file delete $v }
	}
    }
    catch { unset ws_Cache }
}
proc cacheName { name } {
    global maxima_priv
    return [ file join $maxima_priv(cachedir) $name]
}



#
#-----------------------------------------------------------------
#
# readAndSyncCache --  read the cache index.dat
# and remove duplicates removing files, and if necessary save
# the file out.   Normally this would be done at start up.
#
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc readAndSyncCache { } {
    global maxima_priv ws_Cache
    if { [catch {  set fi [open [cacheName index.dat] r] } ] } {
	return
    }
    set all [read $fi]
    #puts "all=$all"
    set lis [split $all \n]
    #puts "lis=$lis"
    set doWrite 0
    foreach v $lis {
	set key [lindex $v 0]
	set val [lindex $v 1]
	if { "$v" == ""} { continue}
	if { [info exists  ws_Cache($key)] } {
	    set doWrite 1
	    catch {file delete [cacheName [lindex $ws_Cache($key) 1] ] }
	}
	if { "$val" != "badvalue" } {
	    set ws_Cache($key) $val
	}
    }
    close $fi
    if {  $doWrite}  {
	set fi [open [cacheName index.dat] w]
	puts [concat [mc "writing"] "[cacheName index.dat]"]
	foreach { key val } [array get ws_Cache *]  {
	    puts  $fi "[list [list $key]] {$val}"
	}
	close $fi
    }
}


## endsource getdata1.tcl
