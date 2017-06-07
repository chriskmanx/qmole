#  $Id: rk4.tcl,v 1.5 2009-11-15 23:00:37 villate Exp $
#
# Copyright (C) 2008 Jaime E. Villate <villate@fe.up.pt>
#
# Fourth order Runge-Kutta method, with fixed-lenght steps in phase space.
# Based on Rk.tcl by William F. Schelter
# (the license information can be found in COPYING.tcl)

proc fieldlines { f g t0 x0 y0 sx sy nsteps dir} {
    set n $nsteps
    set ans "$t0 $x0 $y0"
    set xn $x0
    set yn $y0
    set tn $t0
    catch {
	while { [incr nsteps -1] >= 0 } {
	    set kn1 [$f $tn $xn $yn]
	    set ln1 [$g $tn $xn $yn]

	    set h [expr {$dir/[vectorlength [expr {$kn1/$sx}] [expr {$ln1/$sy}]]}]
	    set h2 [expr {$h / 2.0 }]
	    set h6 [expr {$h / 6.0 }]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn1}] [expr {$yn + $h2*$ln1}]]
	    set kn2 [eval $f $arg]
	    set ln2 [eval $g $arg]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn2}] [expr {$yn +$h2*$ln2}]]
	    set kn3 [eval $f $arg]
	    set ln3 [eval $g $arg]

	    set arg [list [expr {$tn + $h}] [expr {$xn + $h * $kn3}] [expr {$yn + $h*$ln3}]]
	    set kn4 [eval $f $arg]
	    set ln4 [eval $g $arg]

	    set dx [expr {$h6 * ($kn1+2*$kn2+2*$kn3+$kn4)}]
	    set dy [expr {$h6 * ($ln1+2*$ln2+2*$ln3+$ln4)}]

	    if { [vectorlength $dx $dy] > 5*[vectorlength $sx $sy] } { return $ans }
	    set xn [expr {$xn + $dx}]
	    set yn [expr {$yn + $dy}]
	    set tn [expr {$tn + $h}]

	    lappend ans $tn $xn $yn
	}
    }
    return $ans
}

proc curves { f g t0 x0 y0 sx sy nsteps dir} {
    set n $nsteps
    set ans "$t0 $x0 $y0"
    set xn $x0
    set yn $y0
    set tn $t0
    catch {
	while { [incr nsteps -1] >= 0 } {
	    set kn1 [expr -1*[$g $tn $xn $yn] ]
	    set ln1 [$f $tn $xn $yn]

	    set h [expr {$dir/[vectorlength [expr {$kn1/$sx}] [expr {$ln1/$sy}]]}]
	    set h2 [expr {$h / 2.0 }]
	    set h6 [expr {$h / 6.0 }]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn1}] [expr {$yn + $h2*$ln1}]]
	    set kn2 [expr -1*[eval $g $arg] ]
	    set ln2 [eval $f $arg]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn2}] [expr {$yn +$h2*$ln2}]]
	    set kn3 [expr -1*[eval $g $arg] ]
	    set ln3 [eval $f $arg]

	    set arg [list [expr {$tn + $h}] [expr {$xn + $h * $kn3}] [expr {$yn + $h*$ln3}]]
	    set kn4 [expr -1*[eval $g $arg] ]
	    set ln4 [eval $f $arg]

	    set dx [expr {$h6 * ($kn1+2*$kn2+2*$kn3+$kn4)}]
	    set dy [expr {$h6 * ($ln1+2*$ln2+2*$ln3+$ln4)}]

	    if { [vectorlength $dx $dy] > 5*[vectorlength $sx $sy] } { return $ans }
	    set xn [expr {$xn + $dx}]
	    set yn [expr {$yn + $dy}]
	    set tn [expr {$tn + $h}]

	    lappend ans $tn $xn $yn
	}
    }
    return $ans
}
