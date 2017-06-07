# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Rk.tcl,v 1.2 2002-09-07 05:21:42 mikeclarkson Exp $
#
###### Rk.tcl ######
#######################################################################
#######  Copyright William F. Schelter.  All rights reserved.  ########
#######################################################################

proc rungeKutta_try { } {
    proc ff { a b c } { return [expr {$b + $c}] }
    proc gg { a b c } { return [expr {$b - $c}] }
    rungeKutta ff gg 0.2 0.2 0 .01 10
}

proc rungeKutta { f g t0 x0 y0  h nsteps } {
    set n $nsteps
    set ans "$x0 $y0"
    set xn $x0
    set yn $y0
    set tn $t0
    set h2 [expr {$h / 2.0 }]
    set h6 [expr {$h / 6.0 }]
    catch {
	while { [incr nsteps -1] >= 0 } {


	    set kn1 [$f $tn $xn $yn]
	    set ln1 [$g $tn $xn $yn]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn1}] [expr {$yn + $h2*$ln1}]]
	    set kn2 [eval $f $arg]
	    set ln2 [eval $g $arg]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn2}] [expr {$yn +$h2*$ln2}]]
	    set kn3 [eval $f $arg]
	    set ln3 [eval $g $arg]

	    set arg [list [expr {$tn + $h}] [expr {$xn + $h * $kn3}] [expr {$yn + $h*$ln3}]]
	    set kn4 [eval $f $arg]
	    set ln4 [eval $g $arg]

	    set xn [expr {$xn + $h6 * ($kn1+2*$kn2+2*$kn3+$kn4)}]
	    set yn [expr {$yn + $h6 * ($ln1+2*$ln2+2*$ln3+$ln4)}]
	    set tn [expr {$tn+ $h}]

	    lappend ans  $xn $yn
	}
    }

    return $ans
}

proc pathLength { list } {
    set sum 0
    foreach { x y } $list {
	set sum [expr {$sum + sqrt($x*$x+$y*$y)}]
    }
    return $sum
}
proc rungeKuttaA { f g t0 x0 y0  h nsteps } {
    set ans [rungeKutta $f $g $t0 $x0 $y0 $h $nsteps]
    set count 0
    # puts "retrying([llength $ans]) .."
    while { [llength $ans] < $nsteps * .5  && $count < 7 } {
	incr count
	#set leng [pathLength $ans]
	#if { $leng == 0 } {set leng .001}
	set th [expr {$h / 3.0}]
	if { $th  < $h }  { set h $th }
	set ans  [rungeKutta $f $g $t0 $x0 $y0 $h $nsteps]
	# puts -nonewline "..(h=[format "%.5f" $h],pts=[llength $ans])"
	# flush stdout
    }
    return $ans
}



## endsource rk.tcl
