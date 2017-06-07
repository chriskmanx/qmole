# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-

# Voodo for CYGWIN
# Is there a canonical way of telling we are under CYGWIN?
global env tcl_platform maxima_priv
if {$tcl_platform(platform) == "windows" && \
	[info exists env(PATH)] && $env(PATH) != "" && \
	[string match {*/usr/bin*} $env(PATH)] && \
	[string match {*:*} $env(PATH)] && \
	![string match {*;*} $env(PATH)]} {
    # CYGWIN uses Unix PATH but Tcl considers it Windows
    # What's even worse auto_execok uses ; but exec uses :
    if {0} {
	set env(PATH) [join [split $env(PATH) ":"] ";"]
    } else {
	set maxima_priv(platform) cygwin
# Windows version.
#
# Note that info executable doesn't work under Windows, so we have to
# look for files with .exe, .com, or .bat extensions.  Also, the path
# may be in the Path or PATH environment variables, and path
# components are separated with semicolons, not colons as under Unix.
#
proc auto_execok name {
    global auto_execs env tcl_platform

    if {[info exists auto_execs($name)]} {
	return $auto_execs($name)
    }
    set auto_execs($name) ""

    set shellBuiltins [list cls copy date del erase dir echo mkdir \
	    md rename ren rmdir rd time type ver vol]
    if {[string equal $tcl_platform(os) "Windows NT"]} {
	# NT includes the 'start' built-in
	lappend shellBuiltins "start"
    }
    if {[info exists env(PATHEXT)]} {
	# Add an initial : to have the {} extension check first.
	set execExtensions [split ":$env(PATHEXT)" ":"]
    } else {
	set execExtensions [list {} .bat .com .exe]
    }

    if {[lsearch -exact $shellBuiltins $name] != -1} {
	return [set auto_execs($name) [list $env(COMSPEC) /c $name]]
    }

    if {[llength [file split $name]] != 1} {
	foreach ext $execExtensions {
	    set file ${name}${ext}
	    if {[file exists $file] && ![file isdirectory $file]} {
		return [set auto_execs($name) [list $file]]
	    }
	}
	return ""
    }

    set path "[file dirname [info nameof]]:.:"
    if {[info exists env(WINDIR)]} {
	set windir $env(WINDIR) 
    }
    if {[info exists windir]} {
	if {[string equal $tcl_platform(os) "Windows NT"]} {
	    append path "$windir/system32:"
	}
	append path "$windir/system:$windir:"
    }

    foreach var {PATH Path path} {
	if {[info exists env($var)]} {
	    append path ":$env($var)"
	    break
	}
    }

    foreach dir [split $path {:}] {
	# Skip already checked directories
	if {[info exists checked($dir)] || [string equal {} $dir]} { continue }
	set checked($dir) {}
	foreach ext $execExtensions {
	    set file [file join $dir ${name}${ext}]
	    if {[file exists $file] && ![file isdirectory $file]} {
		return [set auto_execs($name) [list $file]]
	    }
	}
    }
    return ""
}

    }

} else {
    set maxima_priv(platform) $tcl_platform(platform)
}
