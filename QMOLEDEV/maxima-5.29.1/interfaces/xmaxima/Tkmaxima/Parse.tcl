# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Parse.tcl,v 1.8 2009-03-27 00:14:45 villate Exp $
#
###### Parse.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

global Parser parse_table
if {[info exists Parser]} {catch { unset Parser }}

foreach v  { { ( 120 } { \[ 120 } { ) 120 } { \] 120 }  { ^ 110}
{ ^- 110} {* 100} { / 100} {% 100}  {- 90 } { + 90 }
{ << 80} { >> 80 } { < 70 } { > 70 } { <= 70 } {>= 70}
{ == 60 } { & 50} { | 40 } { , 40 } {= 40}
{ && 30 } { || 20 } { ? 10 } { : 10 }  { ; 5 }}  {
    set parse_table([lindex $v 0]) [lindex $v 1]
    set getOp([lindex $v 0]) doBinary

}

proc binding_power {s} {
    global parse_table billy
    set billy $s
    if { [catch { set tem $parse_table($s) }] } {
	return 0
    } else {
	return $tem
    }
}

proc getOneMatch { s inds } {
    return [string range $s [lindex $inds 0] [lindex $inds 1]]
}

proc parseTokenize { str } {
    regsub  -all {[*][*]} $str "^" str
    set ans ""
    while { [string length $str ] > 0 } {
	#    puts "ans=$ans,str=$str"	
	set str [string trimleft $str " \t\n" ]
	set s [string range $str 0 1]
	set bp [binding_power $s]
	if { $bp > 0 } {
	    append ans " $s"
	    set str [string range $str 2 end]
	    continue
	} else {
	    set s [string range $s 0 0]
	    set bp [binding_power $s]
	    if { $bp > 0 } {
		append ans " $s"
		set str [string range $str 1 end]
		continue
	    }
	}
	if { "$s" == "" } {
	    return $ans
	}
	if { [regexp -indices {^[0-9.]+([eE][+---]?[0-9]+)?} $str all] } {
	    append ans " { number [getOneMatch $str $all] }"
	    # append ans " [getOneMatch $str $all]"
	    set str [string range $str [expr {1+ [lindex $all 1]}] end]
	}  elseif { [regexp -indices {^[$a-zA-Z][a-zA-Z0-9]*} $str all] } {
	    append ans " { id [getOneMatch $str $all] } "
	    # append ans " [getOneMatch $str $all]"
	    set str [string range $str [expr {1+ [lindex $all 1]}] end]
	}  else {
	    error [concat [mc "parser unrecognized:"] "$str"]
	}
    }
    return $ans
}

set Parser(reserved) " acos cos hypo sinh asin cosh log sqrt atan exp log10 tan atan2 floor pow tanh ceil fmod sin abs double int round"

set Parser(help) [join [list [mc \
{
The syntax for the definition of functions is like C, except that it is \
permitted to write x^n instead of pow(x,n).
} ] [mc {
Functions:}] $Parser(reserved) [mc {

Operators:}] " == % & || ( << <= ) : * >=  + && , | < >> - > ^ ? /" ] ""]



proc nexttok { } {
    global Parser
    set x [lindex $Parser(tokenlist) [incr Parser(tokenind) ]]
    # puts "nexttok=$x"
    if {[llength $x ] > 1 } {
	set Parser(tokenval) [lindex $x 1]
	return [lindex $x 0]
    } else {
	return $x
    }
}


#
#-----------------------------------------------------------------
#
# parseToSuffixLists -- Convert EXPR1; EXPR2; ..
# to a list of suffix lists.  Each suffix list is suitable for
# evaluating on a stack machine (like postscript) or for converting
# further into another form.  see parseFromSuffixList.
#  "1+2-3^4;" ==>
#   {number 1} {number 2} + {number 3} {number 4} ^ -
#  Results: suffix list form of the original EXPR
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc parseToSuffixLists { a }  {
    global    Parser
    set Parser(result) ""
    set Parser(tokenlist) [parseTokenize $a]
    set Parser(tokenind) -1
    set Parser(lookahead)  [nexttok]
    #puts tokenlist=$Parser(tokenlist)
    set ans ""
    while { "$Parser(lookahead)" != ""  } {
	getExpr  ; parseMatch ";"
	#puts "here: $Parser(result) "	
	append ans "[list	$Parser(result)] "
	set Parser(result) "" 	
    }
    regsub \\^- $ans {PRE_MINUS ^} ans2
    return $ans2
}

proc parseMatch { t } {
    global Parser
    if { "$t" == "$Parser(lookahead)" } {
	set Parser(lookahead)  [nexttok]
    } else {
	error "syntax error: wanted $t"
    }
}

proc emit { s args } {
    global Parser
    if { "$args" == "" }   {
	append Parser(result) " $s"
	# puts " $s "
    } else {
	append Parser(result) " {[lindex $args 0 ] $s}"
	#puts " {[lindex $args 0 ] $s} "
    }
}

proc getExpr { } {
    getExprn 0
}

proc getExprn { n } {
    global Parser
    #puts "getExpr $n, $Parser(tokenind),$Parser(tokenlist)"
    if { $n == 110 } {
	getExpr120
	return
    }

    incr n 10
    if  { $n == 110 } {
	if { "$Parser(lookahead)" == "-" || "$Parser(lookahead)" == "+"  } {
            if { "$Parser(lookahead)" == "-" } {
		set this PRE_MINUS
	    } else {
		set this PRE_PLUS
	    }
	    parseMatch $Parser(lookahead)
	    getExprn $n
	    #puts "l=$Parser(lookahead),pl=$Parser(result)"
	    emit $this
	    return
	}
	
    }

    getExprn $n
    while { 1 } {
	if { [binding_power $Parser(lookahead)] == $n } {
	    set this $Parser(lookahead)
	    parseMatch $Parser(lookahead)
	    getExprn $n
	    if { $n == 110 } {
		set toemit ""
		while { "$this" == "^" &&  "$Parser(lookahead)" == "^" } {
		    # puts "p=$Parser(result),$
		    set this $Parser(lookahead)
		    append toemit " $this"
		    parseMatch $Parser(lookahead)
		    getExprn $n
		}
		foreach v $toemit { emit $v }
	    }
	    emit $this

	} else {
	    return
	}
    }
}

proc getExpr120 { } {
    global Parser
    #puts "getExpr120, $Parser(tokenind),[lrange $Parser(tokenlist) $Parser(tokenind) end]"
    while { 1 } {
	if { "$Parser(lookahead)" == "(" } {
	    parseMatch $Parser(lookahead)
	    getExpr
	    parseMatch ")"
	    break;
	} elseif { $Parser(lookahead) == "id" } {
	    emit $Parser(tokenval) id

	    parseMatch $Parser(lookahead)
	    if { "$Parser(lookahead)" == "(" } {
		getExpr120
		emit funcall
	    }
	    break;
	} elseif { $Parser(lookahead) == "number" } {
	    emit $Parser(tokenval) number
	    parseMatch $Parser(lookahead)
	    break;
	} else {
	    bgerror [mc "syntax error"]
	    break;
	}
    }
}

global getOp
set getOp(PRE_PLUS) doPrefix
set getOp(PRE_MINUS) doPrefix
set getOp(funcall) doFuncall
set getOp(^) doPower
set getOp(:) doConditional
set getOp(?) doConditional

proc doBinary { } {
    uplevel 1 {set s $nargs; incr nargs -1 ;
    if { "$x" == "," } {
	set a($nargs) "$a($nargs) $x $a($s)"
    } else {
	set a($nargs) "($a($nargs) $x $a($s))"}
    }
}

proc doPower { } {
    uplevel 1 {set s $nargs; incr nargs -1 ; set a($nargs) "pow($a($nargs),$a($s))" }
}

proc doFuncall {} {
    uplevel 1 {
	#puts nargs=$nargs
	set s $nargs; incr nargs -1 ; set a($nargs) "$a($nargs)($a($s))"
    }
}

proc doPrefix {} {
    uplevel 1  { if { "$x" == "PRE_MINUS" } { set a($nargs) "-$a($nargs)" } }
}

proc doConditional { } {
    set x [uplevel 1 set x]
    if { "$x" == "?" } { return }
    # must be :
    uplevel 1 {
	set s $nargs ; incr nargs -2 ;
	set a($nargs) "($a($nargs) ? $a([expr {$nargs + 1}]) : $a($s))"
    }
}


#
#-----------------------------------------------------------------
#
# parseFromSuffixList --  takes a token list, and turns
# it into a suffix form.  eg: 1 + 2 - 3 ^ 4 --> 1 2 + 3 4 ^ -
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#
proc parseFromSuffixList { list } {
    global getOp
    set stack ""
    set lim [llength $list]
    set i 0
    set nargs 0
    while { $i < $lim } {
	set x [lindex $list $i ]
	set bp [binding_power $x]
	incr i
	# all binary
	if { [llength $x] > 1 } {
	
	    set a([incr nargs]) [lindex $x 1]

	} else {
	    $getOp($x)
	}
    }

    return $a(1)
}


#
#-----------------------------------------------------------------
#
# parseConvert --  given an EXPRESSION, parse it and find out
# what are the variables, and convert a^b to pow(a,b).  If
# -variables "x y" is given, then x and y will be replaced by $x $y
#  doall 1 is giv
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#
global Parser
set Parser(convertOptions) {
    { doall 0 "convert all variables x to \$x" }
    { variables "" "list of variables to change from x to \$x" }
}
proc parseConvert { expr args } {
    global   Parser
    getOptions $Parser(convertOptions) $args
    if { "$expr" == "" } { return [list {} {}] }
    set parselist [parseToSuffixLists "$expr;"]
    #puts "parselist=$parselist"
    catch { unset allvars }
    set new ""
    set answers ""
    foreach lis $parselist {

	foreach v $lis {

	    if { ("[lindex $v 0]" == "id")
	    && ([llength $v] == 2)
	    && ([lsearch  $Parser(reserved) [set w [lindex $v 1]]] < 0)
	} {
	    if { ($doall != 0)  || ([lsearch  $variables $w] >= 0) } {
		append new " {id \$$w}"
		set allvars(\$$w) 1
	    } else {
		set allvars($w) 1
		append new " {$v}"
	    }
	}  else {
	    if { [llength $v] > 1 } {
		append new " {$v}"
	    } else {
		append new " $v" }
	    }
	}
	#puts "new=$new"
	append answers "[list [parseFromSuffixList $new]] "
	set new ""
    }
    return [list $answers [array names allvars]]
}

proc test { s } {
    set me [parseFromSuffixList [lindex [parseToSuffixLists "$s;"] 0]]
    puts $me
    return "[eval expr $s] [eval expr $me]"
}




## endsource parse.tcl
