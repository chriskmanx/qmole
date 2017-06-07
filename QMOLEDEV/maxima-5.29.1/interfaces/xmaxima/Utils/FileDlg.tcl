
proc lIDEFileTypes {pat} {
    switch -exact -- $pat *.py {
	set types [list {"Python Files" {.py .pyw}} {"All Files" *}]
    } *.txt {
	set types [list {"Text Files" .txt} {"All Files" *}]
    } *.txt {
	set types [list {"Log Files" {.log}} {"All Files" *}]
    } *.out {
	set types [list {"Output Files" .out} {"All Files" *}]
    } *.bat {
	set types [list {"Batch Files" {.bat .clp .tst}} {"All Files" *}]
    } *.bin {
	set types [list {"Binary Files" {.bin .sav}} {"All Files" *}]
    } *.mac {
	set types [list {"Maxima Files" {.mc .mac .dem}} {"All Files" *}]
    } *.lisp {
	set types [list {"Lisp Files" {.lisp}} {"All Files" *}]
    } *.tcl {
	set types [list {"Tcl Files" .tcl} {"All Files" *}]
    } *.clp {
	set types [list {"CLIPS Files" {.clp}} {"All Files" *}]
    } *.pprj {
	set types [list {"Protege Projects" .pprj} {"All Files" *}]
    } * - default {
	set types [list {"All Files" *}]
    }
    return $types
}

proc tide_openfile {title {self "."} {pat "*"} {file ""}} {
    global maxima_default

    if {$self == ""} {set self .}
    set z [winfo toplevel $self]

    set types [lIDEFileTypes $pat]

    # required for a MSFC/Tk bug workaround
    update

    if {$file == ""} {set file $maxima_default(OpenFile)}
    if {$file != ""} {set dir [file dir $file]} {set dir ""}

    set proc tk_getOpenFile
    global tk_strictMotif
    set old $tk_strictMotif
    set tk_strictMotif 0

    # -defaultextension $pattern
    set list [list $proc -title $title \
	    -filetypes $types \
	    -parent $z]
    if {$dir != ""} {
	lappend list  -initialdir [file native $dir]
    }

    if {[catch {eval $list} retval]} {
	global errorInfo
	tide_failure \
		[M "Error opening file:\n%s" $errorInfo]
	return ""
    }
    set tk_strictMotif $old

    if {$retval != ""} { set maxima_default(OpenFile) $retval }

    return $retval
}

proc tide_savefile {title {self "."} {pat "*"} {file ""}} {
    global maxima_default

    if {$self == ""} {set self .}
    set z [winfo toplevel $self]

    set types [lIDEFileTypes $pat]

    # required for a MSFC/Tk bug workaround
    update

    if {$file == ""} {set file $maxima_default(SaveFile)}
    if {$file != ""} {set dir [file dir $file]} {set dir ""}

    set proc tk_getSaveFile
    global tk_strictMotif
    set old $tk_strictMotif
    set tk_strictMotif 0

    # -defaultextension $pattern
    set list [list $proc  \
	    -filetypes $types \
	    -parent $z \
	    -title $title]
    if {$dir != ""} {
	lappend list -initialdir [file native $dir]
    }
    if {[catch {eval $list} retval]} {
	global errorInfo
	tide_failure \
		"Error Saving file:\n$errorInfo"
	return ""
    }
    set tk_strictMotif $old

    if {$retval != ""} { set maxima_default(SaveFile) $retval }

    return $retval
}

proc tide_opendir {title {self "."} {dir ""}} {
    global maxima_default

    set list [list tk_chooseDirectory \
	    -parent $self -title $title -mustexist 1]
    if {$dir == ""} {set dir $maxima_default(OpenDir)}
    if {$dir != ""} {
	lappend list -initialdir [file native $dir]
    }
    if {[catch {eval $list} retval]} {
	global errorInfo
	tide_failure \
		"Error Saving file:\n$errorInfo"
	return ""
    }

    if {$retval != ""} {set maxima_default(OpenDir) $retval}

    return $retval
}

proc tide_savedir {title {self "."} {dir ""}} {
    global maxima_default

    set list [list tk_chooseDirectory \
	    -parent $self -title $title -mustexist 0]
    if {$dir == ""} {set dir $maxima_default(OpenDir)}
    if {$dir != ""} {
	lappend list -initialdir [file native $dir]
    }
    if {[catch {eval $list} retval]} {
	global errorInfo
	tide_failure \
		"Error Saving file:\n$errorInfo"
	return ""
    }

    if {$retval != ""} {set maxima_default(OpenDir) $retval}

    return $retval
}


proc tide_notify {reason {self "."}} {
    update
    # puts stdout $reason
    tk_messageBox -icon info \
	    -title "Info" \
	    -parent $self \
	    -message $reason -type ok
}

proc tide_failure {reason {self "."}} {
    global errorInfo
    update
    # puts stderr $reason
    # puts stderr $errorInfo
    tk_messageBox -icon error \
	    -title "Error" \
	    -parent $self \
	    -message $reason -type ok
}

proc tide_yesno {reason {self "."}} {
    update
    set retval [tk_messageBox -icon question \
	    -title "Question" \
	    -parent $self \
	    -message $reason -type yesno]
    if {$retval == "yes"} {return 1} {return 0}
}

proc tide_yesnocancel {reason {self "."}} {
    update
    set retval [tk_messageBox -icon question \
	    -title "Question" \
	    -message $reason -type yesnocancel]
    switch $retval "yes" {
	return 1
    } no {
	return 0
    } cancel {
	return -1
    }
}

