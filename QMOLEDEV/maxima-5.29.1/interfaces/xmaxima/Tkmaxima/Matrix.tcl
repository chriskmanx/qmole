# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Matrix.tcl,v 1.4 2006-10-01 23:58:29 villate Exp $
#
###### Matrix.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

# In this file a matrix is represented by a list of M*N entries together
# with an integer N giving the number of columns: {1 0 0 1} 2  would give
# the two by two identity

proc comment {args } {
}
global mee
set mee " } \] \[ expr { "

proc mkMultLeftExpr { mat n prefix { constant "" } } {
    #create a function body that does MAT (prefix1,prefix2,..) + constant
    global mee
    set all ""

    set vars ""
    for { set i 0} { $i < $n} {incr i} { append vars " $prefix$i" }
    set j 0
    set k 0

    foreach v $mat {
	if { $j == 0 } {
	    set ro ""
	    # append ans ""
	    set op ""
	}
        append ro " $op $v*\$$prefix$j"
	set op "+"
	if { $j == [expr {$n -1}] } {
	    append ans " "
	    if { "[lindex $constant $k]" != "" } {
		append ro " + [lindex $constant $k] "
	    }
	    incr k
	    append ans [concat \[ expr [list $ro] \]]
	    set j -1
	}
	incr j
    }
    # puts [list $vars $ans]
    return [list $vars $ans]
}

proc mkMultLeftFun { mat n name { constant ""} } {
    set expr [mkMultLeftExpr $mat $n _a $constant]
    set bod1 [string trim [lindex $expr 1] " "]
    #    set bod "return \"$bod1\""
    set bod [concat list [lindex $expr 1]]
    proc $name [lindex $expr 0] $bod
}

proc rotationMatrix { th ph } {
    return [list \
		[expr {cos($ph)*cos($th)}] [expr {- cos($ph)*sin($th)}] [expr {sin($ph)}] \
		[expr {sin($th)}] [expr {cos($th)}] 0.0 \
		[expr {- sin($ph)*cos($th)}] [expr {sin($ph)*sin($th)}] [expr {cos($ph)}]]
}

proc rotationMatrix { thx thy thz } {
    return \
	[list  \
	     [expr { cos($thy)*cos($thz) } ] \
	     [expr { cos($thy)*sin($thz) } ] \
	     [expr { sin($thy) } ] \
	     [expr { sin($thx)*sin($thy)*cos($thz)-cos($thx)*sin($thz) } ] \
	     [expr { sin($thx)*sin($thy)*sin($thz)+cos($thx)*cos($thz) } ] \
	     [expr { -sin($thx)*cos($thy) } ] \
	     [expr { -sin($thx)*sin($thz)-cos($thx)*sin($thy)*cos($thz) } ] \
	     [expr { sin($thx)*cos($thz)-cos($thx)*sin($thy)*sin($thz) } ] \
	     [expr { cos($thx)*cos($thy) } ] ]
}

# cross [a,b,c] [d,e,f] == [B*F-C*E,C*D-A*F,A*E-B*D]
# cross_product([a,b,c],[d,e,f]):=[B*F-C*E,C*D-A*F,A*E-B*D]
# cross_product(u,v):=sublis([a=u[1],b=u[2],c=u[3],d=v[1],e=v[2],f=v[3]],[B*F-C*E,C*D-A*F,A*E-B*D]);
# the rotation by azimuth th, and elevation ph
# MATRIX([COS(TH),SIN(TH),0],[-COS(PH)*SIN(TH),COS(PH)*COS(TH),SIN(PH)],
#	    [SIN(PH)*SIN(TH),-SIN(PH)*COS(TH),COS(PH)]);

proc rotationMatrix { th ph {ignore {} } } {
    return \
	[list \
	     [	    expr {cos($th)   } ]\
	     [expr {sin($th)   } ]\
	     0 \
	     [expr {-cos($ph)*sin($th)   } ]\
	     [expr {cos($ph)*cos($th)   } ]\
	     [expr {sin($ph)   } ]\
	     [expr {sin($ph)*sin($th)   } ]\
	     [expr {-sin($ph)*cos($th)   } ]\
	     [expr {cos($ph)   } ]]
}

proc setMatFromList {name lis n} {
    set i 1
    set j 1
    foreach v $lis {
	uplevel 1 set [set name]($i,$j) $v
	if { $j == $n } {set j 1; incr i} else { incr j}
    }
}

proc matRef { mat cols i j } {
    [lindex $mat [expr {$i*$cols + $j}]]
}

proc matTranspose { mat cols } {
    set j 0
    set m [expr {[llength $mat ] / $cols}]
    while { $j < $cols} {
	set i 0
	while { $i < $m } {
	    append ans " [lindex $mat [expr {$i*$cols + $j}]]"
	    incr i
	}
	incr j
    }
    return $ans
}


proc matMul { mat1 cols1 mat2 cols2 } {
    mkMultLeftFun $mat1 $cols1 __tem
    set tr [matTranspose $mat2 $cols2]
    set rows1 [expr {[llength $mat1] / $cols1}]
    #puts "tr=$tr"
    set upto [llength $tr]
    set j 0
    set ans ""
    set i 0
    while { $j < $cols2  } {
	append ans " [eval __tem [lrange $tr $i [expr {$i+$cols1 -1}]]]"
	incr i $cols1
	incr j
    }
    #   return $ans
    # puts "matTranspose $ans $rows1"
    return [matTranspose $ans $rows1]
}



proc invMat3 { mat } {
    setMatFromList xx $mat 3
    set det [expr { double($xx(1,1))*($xx(2,2)*$xx(3,3)-$xx(2,3)*$xx(3,2))-$xx(1,2)* \
			($xx(2,1)*$xx(3,3)-$xx(2,3)*$xx(3,1))+$xx(1,3)*($xx(2,1)*$xx(3,2)\
									    -$xx(2,2)*$xx(3,1)) }]

    return [list   [expr { ($xx(2,2)*$xx(3,3)-$xx(2,3)*$xx(3,2))/$det}] \
		[expr { ($xx(1,3)*$xx(3,2)-$xx(1,2)*$xx(3,3))/$det}] \
		[expr { ($xx(1,2)*$xx(2,3)-$xx(1,3)*$xx(2,2))/$det}] \
		\
		[expr { ($xx(2,3)*$xx(3,1)-$xx(2,1)*$xx(3,3))/$det}] \
		[expr { ($xx(1,1)*$xx(3,3)-$xx(1,3)*$xx(3,1))/$det}] \
		[expr { ($xx(1,3)*$xx(2,1)-$xx(1,1)*$xx(2,3))/$det}] \
		\
		[expr { ($xx(2,1)*$xx(3,2)-$xx(2,2)*$xx(3,1))/$det}] \
		[expr { ($xx(1,2)*$xx(3,1)-$xx(1,1)*$xx(3,2))/$det}] \
		[expr { ($xx(1,1)*$xx(2,2)-$xx(1,2)*$xx(2,1))/$det}]]
}


proc vectorOp { a op b} {
    set i [llength $a]
    set k 0
    set ans [expr [list [lindex $a 0]  $op [lindex $b 0]]]
    while { [incr k] < $i } {
	lappend ans  [expr  [list [lindex $a $k] $op [lindex $b $k]]]
    }
    return $ans
}
## endsource matrix.tcl
