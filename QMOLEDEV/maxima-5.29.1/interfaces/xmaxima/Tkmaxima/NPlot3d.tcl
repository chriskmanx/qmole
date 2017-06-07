# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: NPlot3d.tcl,v 1.9 2009-11-16 22:41:47 villate Exp $
#
###### NPlot3d.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

# source plotting.tcl ; source nplot3d.tcl ; catch { destroy .plot3d} ;  plot3d -zfun "" -data $sample -xradius 10 -yradius 10
# newidea:
# { plot3d
#  { gridequal {minx maxx} {miny maxy}
#   {{z00 z01 z02 .. z0n } { z10 z11 z12 .. z1n} {..  } ...}
#  { grid {x0 x1  xm} {y0 y1 yn } miny maxy}
#   {{z00 z01 z02 .. z0n } { z10 z11 z12 .. z1n} {..  } ...}
#  { xyzgrid {{x00 y00 z00 x01 y01 z01 .. x0  }{x0 x1  xm} {y0 y1 yn } miny maxy}
#   {{z00 z01 z02 .. z0n } { z10 z11 z12 .. z1n} {..  } ...}
# tclMesh(2*[0,0,0,0,0;1,1,1,1,1]-1,2*[0,1,1,0,0;0,1,1,0,0]-1,2*[0,0,1,1,0;0,0,1,1,0]-1)

#     { gridequal {

# z00 z01 .. all belong to x=minx and y = miny,.... up y=maxy in n+1 steps
#{ grid {minx maxx} {miny maxy}
#  {{z00 z01 z02 .. z0n } { z10 z11 z12 .. z1n} {..  } ...}
# }
# where a mesh(1) {z00 z01 z11 z10} above



# { mesh {{{x00 y00 z00 } { x01 y01 z01} { x02 y02 z02}  ..}{{x10 y10 z10} {x11 y11 z11} ......} ..}}
# mesh(1) = P00 P01 P11 P10

set sample { variable_grid { 0 1 2 } { 3 4 5} { {21 111 2} {3 4 5 } {6 7 8 }}}
set sample { variable_grid { 0 1 2 } { 3 4 5} { {0 1 2} {3 4 5 } {6 7 8 }}}
set sample { matrix_mesh {{0 1} { 2 3 } {4 5 }}  {{0 1} { 2 3 } {4 5 }}  {{0 1} { 2 3 } {4 5 }} }
set sample { matrix_mesh {{0 1 2} {0 1 2 } {0 1 2 }} {{3 4 5} {3 4 5} {3 4 5}} { {0 1 2} {3 4 5 } {6 7 8 }}}
set sample1 { variable_grid  { 1 2 3 4 5 6 7 8 9 10 }
    { 1 2 3 }
    {  { 0 0 0 0 0 0 0 0 0 0 }
	{ 0 0.68404 1.28558 1.73205 1.96962 1.96962 1.73205 1.28558 0.68404 2.44921e-16 }
	{ 0 1.36808 2.57115 3.4641 3.93923 3.93923 3.4641 2.57115 1.36808 4.89843e-16 }
    }
}

set sample { matrix_mesh  {  { 0 0 0 0 0 }
    { 1 1 1 1 1 }
}  {  { 0 1 1 0 0 }
    { 0 1 1 0 0 }
}  {  { 0 0 1 1 0 }
    { 0 0 1 1 0 }
}
}

proc  fixupZ { } {
    uplevel 1 {
	if { [catch { expr $z + 0 } ] } {
	    set z nam
	}
    }
}

proc vectorLength { v } {
    expr { sqrt(1.0 * [lindex $v 0]*[lindex $v 0] + [lindex $v 1]*[lindex $v 1] + [lindex $v 2]*[lindex $v 2]) }
}

proc normalizeToLengthOne { v } {
    set norm [expr { sqrt(1.0 * [lindex $v 0]*[lindex $v 0] + [lindex $v 1]*[lindex $v 1] + [lindex $v 2]*[lindex $v 2]) }]
    if { $norm != 0.0 } {
	return [list [expr { [lindex $v 0] / $norm  } ] \
		    [expr { [lindex $v 1] / $norm  } ] \
		    [expr { [lindex $v 2] / $norm  } ] ]
	
    } else {
	return "1.0 0.0 0.0 "
    }
}



proc vectorCross { x1 x2 }  {
    list \
	[expr { [lindex $x1 1]*[lindex $x2 2]- [lindex $x2 1]*[lindex $x1 2]}] \
	[expr { [lindex $x1 2]*[lindex $x2 0]- [lindex $x2 2]*[lindex $x1 0] } ] \
	[expr { [lindex $x1 0]*[lindex $x2 1]- [lindex $x2 0]*[lindex $x1 1] }]
}

proc linspace { a b n } {
    if { $n < 2 } { error [M [mc "from %s to %s requires at least 2 points"] "$a" "$b" ] }
    set del [expr {($b - $a)*1.0/($n -1)  }]
    for { set i 0 } { $i < $n } { incr i } {
	lappend ans [expr {$a + $del * $i}]
    }
    return $ans
}


proc addOnePlot3d { win data } {
    upvar #0 plot3dMeshes$win meshes
    #puts " adding meshes = plot3dMeshes$win"
    #puts "data=$data"
    linkLocal $win points zmax zmin zcenter zradius rotationcenter xradius yradius xmin xmax ymin ymax lmesh
    makeLocal $win flatten
    oset $win colorfun plot3dcolorFun
    oset $win cmap c1
    global plot3dOptions
    catch { unset  meshes }
    set points ""

    # check whether zradius is a number or "auto"
    set dotruncate [expr ![catch {expr {$zradius + 1} }]]
    if { $dotruncate } {
		set zzmax [expr {$zcenter + $zradius}]
		set zzmin [expr {$zcenter - $zradius}]
    }

    set k [llength $points]
    set type [lindex $data 0]
    # in general the data should be a list of plots..
    if { [lsearch {grid mesh variable_grid matrix_mesh }  $type ]>=0 } {
	set alldata [list $data]
    } else {set alldata $data}
    set lmesh {}
    set first_mesh 0
    foreach data $alldata {	
	set type [lindex $data 0]
	if { "$type" == "grid" } {
	    desetq "xmin xmax" [lindex $data 1]
	    desetq "ymin ymax" [lindex $data 2]
	    set pts [lindex $data 3]
	
	    set ncols [llength $pts]
	    set nrows  [llength [lindex $pts 0]]
	    set data [list variable_grid [linspace $xmin $xmax $ncols] \
			  [linspace $ymin $ymax $nrows] \
			  $pts ]
	    set type "variable_grid"
	}
	if { "$type" == "variable_grid" } {
	    set first_mesh [llength $lmesh]
	    desetq "xrow yrow zmat" [lrange $data 1 end]
	    # puts "xrow=$xrow,yrow=$yrow,zmat=$zmat"
	    set nx [expr {[llength $xrow] -1}]
	    set ny [expr {[llength $yrow] -1}]
	    #puts "nx=$nx,ny=$ny"
	    #	set xmin [lindex $xrow 0]
	    #	set xmax [lindex $xrow $nx]
	    #	set ymin [lindex $yrow 0]
	    #	set ymax [lindex $yrow $ny]
	    desetq "xmin xmax" [minMax $xrow ""]
	    desetq "ymin ymax" [minMax $yrow ""]
	    desetq "zmin zmax" [matrixMinMax [list $zmat]]
	    if { $dotruncate } {
		set zmax  $zzmax
		set zmin  $zzmin
	    }
	    #	puts "and now"
	    #	dshow nx xmin xmax ymin ymax zmin zmax

	    for {set j 0} { $j <= $ny } { incr j} {
		set y [lindex $yrow $j]
		set row [lindex $zmat $j]
		for {set i 0} { $i <= $nx } { incr i} {
		    set x [lindex $xrow $i]
		    set z [lindex $row $i]
		    #puts "x=$x,y=$y,z=$z, at ($i,$j)"
		    fixupZ
		    if { $j != $ny && $i != $nx } {
			lappend lmesh [list $k [expr { $k+3 }] \
					   [expr { $k+3+($nx+1)*3 }] \
					   [expr { $k+($nx+1)*3 }]]
		    }
		    incr k 3
		    lappend points $x $y $z
		}
	    }
	    setupPlot3dColors $win $first_mesh
	} elseif { "$type" == "matrix_mesh" } {
	    set first_mesh [llength $lmesh]
	    desetq "xmat ymat zmat" [lrange $data 1 end]
	    foreach v {x y z} {
		desetq "${v}min ${v}max" [matrixMinMax [list [set ${v}mat]]]
	    }
	    if { $dotruncate } {
		set zmax  $zzmax
		set zmin  $zzmin
	    }
	    set nj [expr {[llength [lindex $xmat 0]] -1 }]
	    set ni [expr {[llength $xmat ] -1 }]
	    set i -1
	    set k [llength $points]
	    foreach rowx $xmat rowy $ymat rowz $zmat {
		set j -1
		incr i
		if { [llength $rowx] != [llength $rowy] } {
		    error [concat [mc "mismatch"] "rowx:$rowx,rowy:$rowy"]
		}
		if { [llength $rowx] != [llength $rowz] } {
		    error [concat [mc "mismatch"] "rowx:$rowx,rowz:$rowz"]
		}
		foreach x $rowx y $rowy z $rowz {
		    incr j
		    if { $j != $nj && $i != $ni } {
			#puts "tes=($i,$j) $x, $y, $z"
			lappend lmesh [ list \
					    $k [expr { $k+3 } ] [expr { $k + 3  + ($nj+1)*3}] \
					    [expr { $k+($nj+1)*3 }] ]
		    }
		    incr k 3
		    lappend points $x $y $z
		}
	    }
	    setupPlot3dColors $win $first_mesh
	} elseif { 0 && "$type" == "mesh" } {
	    set first_mesh [llength $lmesh]
	    # walk thru compute the xmin, xmax, ymin , ymax...
	    # and then go thru setting up the mesh array..
	    # and maybe setting up the color map for these meshes..
	    #
	    # { mesh {{{x00 y00 z00 } { x01 y01 z01} { x02 y02 z02}  ..}{{x10 y10 z10} {x11 y11 z11} ......} ..}}
	    # mesh(1) = P00 P01 P11 P10
	    set mdata [lindex $data 1]
	    set nx [llength $mdata]
	    set ny [llength [lindex $mdata 0]]

	    for {set i 0} { $i <= $nx } { incr i} {
		set pts [lindex $mdata $i]
		set j 0
		foreach { x y z} $pts {
		    fixupZ $z
		    if { $j != $ny && $i != $nx } {
			lappend lmesh [list
				       $k [expr { $k+3 }] [expr { $k+3+($ny+1)*3 }] \
					   [expr { $k+($ny+1)*3 }] ]
		    }
		}
		incr k 3
		lappend points $x $y $z
		incr j
	    }
	    setupPlot3dColors $win $first_mesh
	} elseif { "[assq $type $plot3dOptions notthere]" != "notthere" } {
	    oset $win $type [lindex $data 1]
	    if { $type == "zradius" } {
		# check whether zradius is a number or "auto"
		set dotruncate [expr ![catch {expr {$zradius + 1} }]]
		if { $dotruncate } {
		    set zzmax [expr {$zcenter + $zradius}]
		    set zzmin [expr {$zcenter - $zradius}]
		}
	    }
	}
	if { $first_mesh != [llength $lmesh] } {
	    # set up the min/max values for the complete plot
	    foreach v { x y z } {
		if { [info exists ${v}gmin] } {
		    if { [set ${v}min] < [set ${v}gmin] } {
			set ${v}gmin [set ${v}min]
		    }
		} else {
		    set ${v}gmin [set ${v}min]
		}
		if { [info exists ${v}gmax] } {
		    if { [set ${v}max] > [set ${v}gmax] } {
			set ${v}gmax [set ${v}max]
		    }
		} else {
		    set ${v}gmax [set ${v}max]
		}
	    }
	}
    }
    foreach v { x y z } {
	set ${v}min [set ${v}gmin]
	set ${v}max [set ${v}gmax]
	set a [set ${v}min]
	set b  [set ${v}max]
	if { $a == $b } {
	    set ${v}min [expr {$a -1}]
	    set ${v}max [expr {$a +1}]
	}
	set ${v}radius [expr {($b - $a)/2.0}]
	set ${v}center [expr {($b + $a)/2.0}]
    }
    set rotationcenter "[expr {.5*($xmax + $xmin)}] [expr {.5*($ymax + $ymin)}]   [expr {.5*($zmax + $zmin)}] "

    #puts "meshes data=[array get meshes]"
    #global plot3dMeshes.plot3d
    #puts "array names plot3dMeshes.plot3d = [array names plot3dMeshes.plot3d]"
}

proc vectorDiff { x1 x2 } {
    list [expr { [lindex $x1 0] - [lindex $x2 0] }] \
	[expr { [lindex $x1 1] - [lindex $x2 1] }] \
	[expr { [lindex $x1 2] - [lindex $x2 2] }]
}


proc oneCircle { old2 old1 pt radius nsides { angle 0 } } {
    set dt  [expr {  3.14159265358979323*2.0/($nsides-1.0) + $angle }]
    for  { set i 0 } { $i < $nsides } { incr i } {
	set t [expr {$dt*$i }]
	lappend ans [expr { $radius*([lindex $old2 0]*cos($t) + [lindex $old1 0] * sin($t)) + [lindex $pt 0] } ] \
	    [expr { $radius*([lindex $old2 1]*cos($t) + [lindex $old1 1] * sin($t)) + [lindex $pt 1] } ] \
	    [expr { $radius*([lindex $old2 2]*cos($t) + [lindex $old1 2] * sin($t)) + [lindex $pt 2] } ]
    }
    return $ans
}

proc curve3d { xfun yfun zfun trange } {
    foreach u { x y z} {
	set res [parseConvert [set ${u}fun] -variables t]
	proc _${u}fun { t } [list expr [lindex [lindex $res 0] 0]]
    }
}

proc tubeFromCurveData { pts nsides radius } {
    set n [llength $pts] ;
    set closed [ expr { [vectorLength [vectorDiff [lindex $pts 0] [lindex $pts end]]] < .02} ]
    if { $closed } {
	set f1 [expr {$n -2}]
	set f2 1
    } else {
	set f1 0
	set f2 1
    }
    set delta [vectorDiff [lindex $pts $f2] [lindex $pts $f1]]
    if { [lindex $delta 0] == 0 && [lindex $delta 1] == 0 && [lindex $delta 2] == 0 } { set delta "0 0 1.0" }
    set old ".6543654 0.0765456443 0.2965433"
    set old1 [normalizeToLengthOne [vectorCross $delta $old]]
    set n1 $old1
    set n2 [normalizeToLengthOne [vectorCross $delta $old1]]
    set first1 $n1 ; set first2 $n2

    lappend ans [oneCircle $n2   old1 [lindex $pts 0]]
    for { set j 1 } { $j < $n -1 } { incr j } {
	set delta [vectorDiff [lindex $pts $j] [lindex $pts [expr {$j+1}]]]
	if { [lindex $delta 0] == 0 && [lindex $delta 1] == 0 && [lindex $delta 2] == 0 } { set delta $old
	}
	set old $delta
	set old1 [normalizeToLengthOne [vectorCross $delta $n1]]
	set old2 [normalizeToLengthOne [vectorCross $delta $n2]]
	set n2 $old1
	set n1 $old2
	lappend ans [oneCircle $n2 $n1 [lindex $pts $j] $radius $nsides]
    }
    if { $closed } {
	set f2 1 ; set f1 [expr {$n -2}] ; set f3 0
    } else {
	set f1 [expr {$n -2}] ; set f2 [expr {$n-1}] ; set f3 $f2
    }

    set delta [vectorDiff [lindex $pts $f2] [lindex $pts $f1]]
    if { [lindex $delta 0] == 0 && [lindex $delta 1] == 0 && \
	     [lindex $delta 2] == 0 } { set delta $old }
    set old1 [normalizeToLengthOne [vectorCross delta $n1]]
    set old2 [normalizeToLengthOne [vectorCross $n2 $delta]]
    set n2 $old1 ; set n1 $old2
    if { $closed } {
	set angle [vangle $first1 $n1]
	set n1 $first1 ; st n2 $first2;
    }
    lappend ans [oneCircle $n2 $n1 [lindex $pts $f3] $radius $nsides $angle]
    return $ans
}


#
#-----------------------------------------------------------------
#
# vangle --  angle between two unit vectors
#
#  Results: an angle
#
#  Side Effects: none.
#
#----------------------------------------------------------------
#
proc vangle { x1 x2 } {
    set dot [expr { [lindex $x1 0]*[lindex $x2 0] +\
			[lindex $x1 1]*[lindex $x2 1] +\
			[lindex $x1 2]*[lindex $x2 2]} ]
    if { $dot >= 1 } { return 0.0 }
    if { $dot <= -1.0 } { return 3.141592653589 }
    return [expr { acos($dot) } ]
}

## endsource nplot3d.tcl
