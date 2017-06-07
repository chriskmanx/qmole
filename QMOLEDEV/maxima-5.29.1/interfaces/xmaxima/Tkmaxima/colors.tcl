#  $Id: colors.tcl,v 1.1 2009-11-15 22:55:35 villate Exp $
#
# Copyright (c) 2009, Jaime E. Villate <villate@fe.up.pt>
#
# Procedures to work wih colors
# (the license information can be found in COPYING.tcl)

# transform a (hue, saturation, value) set into a (red, green, blue) set

proc hsv2rgb {hue sat val} {
    if { $sat < 0 } { set sat [expr 1 -  $sat] }
    if { $val < 0 } { set val [expr 1 -  $val] }
    if { $val > 1 } { set val [expr $val - int($val)] }
    if { $sat > 1 } { set sat [expr $sat - int($sat)] }
    set v  [expr round($val*255)]
    if {$sat == 0} {
        return [format "\#%02x%02x%02x" $v $v $v]
    } else {
        set h [expr {round($hue)%360/60.0}]
        set i [expr {round($hue)%360/60}]
        set f [expr {$h-$i}]
        set u [expr {round($v)}]
        set p [expr {round($v*(1-$sat))}]
        set q [expr {round($v*(1-$sat*$f))}]
        set t [expr {round($v*(1-$sat*(1-$f)))}]
        switch $i {
            0 {set r $u; set g $t; set b $p}
            1 {set r $q; set g $u; set b $p}
            2 {set r $p; set g $u; set b $t}
            3 {set r $p; set g $q; set b $u}
            4 {set r $t; set g $p; set b $u}
            5 {set r $u; set g $p; set b $q}
        }
        return [format "\#%02x%02x%02x" $r $g $b]
    }
}

