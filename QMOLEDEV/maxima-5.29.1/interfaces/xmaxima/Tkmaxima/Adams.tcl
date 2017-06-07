# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Adams.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
###### Adams.tcl ######


proc adamsMoulton { f g t0 x0 y0  h nsteps } {
    set ans [rungeKutta $f $g $t0 $x0 $y0 $h 3]
    catch {
	set i 0
	set h24 [expr {$h /24.0}]
	foreach { x y } $ans {
	    lappend listXff [xff  [expr {$t0 + $i * $h} ] $x $y]
	    lappend listYff [yff  [expr {$t0 + $i * $h} ] $x $y]
	    incr i
	    set xn $x
	    set yn $y
	}

	set n [expr $nsteps -3]

	while { [incr n -1] >= 0 } {

	    #puts "listXff = $listXff"
	    #puts "listYff = $listYff"		
	    # adams - bashford formula:
	    set xp [expr {$xn + ($h24)*(55 *[lindex $listXff 3]-59*[lindex $listXff 2]+37*[lindex $listXff 1]-9*[lindex $listXff 0]) }]
	    set yp [expr {$yn + ($h24)*(55 *[lindex $listYff 3]-59*[lindex $listYff 2]+37*[lindex $listYff 1]-9*[lindex $listYff 0]) }]
	    #puts "i=$i,xp=$xp,yp=$yp"
	    # adams-moulton corrector-predictor:
	    # compute the yp = yn+1 value..
	    set t [expr {$t0 + $i * $h}]
	    incr i
	    if { 1 } {
		set xap [expr { $xn+($h24)*(9*[xff $t $xp $yp]+19*[lindex $listXff 3]-5*[lindex $listXff 2]+[lindex $listXff 1]) }]
		set yap [expr { $yn+($h24)*(9*[yff $t $xp $yp]+19*[lindex $listYff 3]-5*[lindex $listYff 2]+[lindex $listYff 1]) }]

		set xn $xap
		set yn $yap
		# puts "after correct:i=[expr $i -1],xn=$xn,yn=$yn"	
		# could repeat it, or check against previous to see if changes too much.
	    }
	    set listXff [lrange $listXff 1 end]
	    set listYff [lrange $listYff 1 end]

	    lappend listXff [xff $t $xn $yn]
	    lappend listYff [yff $t $xn $yn]

	    lappend ans $xn $yn
	    # puts "ans=$ans"	
	}
	#puts "adams:t=$t"
    }
    return $ans
}

## endsource adams.tcl
