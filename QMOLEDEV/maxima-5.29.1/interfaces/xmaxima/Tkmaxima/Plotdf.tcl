# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Plotdf.tcl,v 1.23 2011-03-20 16:33:16 villate Exp $
#
###### Plotdf.tcl ######
#######################################################################
#######  Copyright William F. Schelter.  All rights reserved.  ########
#######################################################################

global plotdfOptions
set plotdfOptions {
    {dxdt "x-y^2+sin(x)*.3" {specifies dx/dt = dxdt.  eg -dxdt "x+y+sin(x)^2"} }
    {dydt "x+y" {specifies dy/dt = dydt.  eg -dydt "x-y^2+exp(x)"} }
    {dydx "" { may specify dy/dx = x^2+y,instead of dy/dt = x^2+y and dx/dt=1 }}
    {vectors blue "Color for the vectors"}
    {fieldlines red "Color for the fieldlines"}
    {curves "" "Color for the orthogonal curves"}
    {xradius 10 "Width in x direction of the x values" }
    {yradius 10 "Height in y direction of the y values"}
    {width 560 "Width of canvas in pixels"}
    {height 560 "Height of canvas in pixels" }
    {scrollregion {} "Area to show if canvas is larger" }
    {xcenter 0.0 {(xcenter,ycenter) is the origin of the window}}
    {ycenter 0.0 "see xcenter"}
    {bbox "" "xmin ymin xmax ymax .. overrides the -xcenter etc"}
    {tinitial 0.0 "The initial value of variable t"}
    {nsteps 300 "Number of steps to do in one pass"}
    {xfun "" "A semi colon separated list of functions to plot as well"}
    {tstep "" "t step size"}
    {direction "both" "May be both, forward or backward" }
    {versus_t 0 "Plot in a separate window x and y versus t, after each trajectory" }
    {windowname ".dfplot" "window name"}
    {windowtitle "Plotdf" "window title"}
    {parameters "" "List of parameters and values eg k=3,l=7+k"}
    {linecolors { green black  brown gray black} "colors for functions plots"}
    {sliders "" "List of parameters ranges k=3:5,u"}
    {trajectory_at "" "Place to calculate trajectory"}
    {linewidth "2.0" "Width of integral lines" }
    {nolines 0 "If not 0, plot points and nolines"}
    {bargraph 0 "If not 0 this is the width of the bars on a bar graph" }
    {plotpoints 0 "if not 0 plot the points at pointsize" }
    {pointsize 2 "radius in pixels of points" }
    {autoscale "x y" "Set {x,y}center and {x,y}range depending on data and function. "}
    {zoomfactor "1.6 1.6" "Factor to zoom the x and y axis when zooming.  Zoom out will be reciprocal" }
    {errorbar 0 "If not 0 width in pixels of errorbar.  Two y values supplied for each x: {y1low y1high y2low y2high  .. }"}
    {data "" "List of data sets to be plotted.  Has form { {xversusy {x1 x2 ... xn} {y1 .. yn ... ym}} .. {againstIndex {y1 y2 .. yn}}  .. }"}
    {labelposition "10 15" "Position for the curve labels nw corner"}
    {xaxislabel "" "Label for the x axis"}
    {yaxislabel "" "Label for the y axis"}
    {psfile "" "A filename where the graph will be saved in PostScript."}
    {nobox 0 "if not zero, do not draw the box around the plot."}
    {axes "xy" "if zero, no axes are drawn. x, y or xy to draw the axes."}
    {nolegend 0 "if not zero, do not write down the legend."}
}

proc makeFrameDf { win } {
    set w [makeFrame $win df]
    makeLocal $win c dydx

    set top $win
    # puts "w=$w,win=$win"
    catch { set top [winfo parent $win]}
    catch {

	wm title $top [oget $win windowtitle]
	wm iconname $top "plotdf"
	#    wm geometry $top 750x700-0+20
    }
    set wb $w.menubar
    makeLocal $win buttonFont
    button $wb.versust -image ::img::plot -text [mc "Time Plots"] \
        -command "plotVersusT $w" -relief flat -width 30 -height 30
    pack $wb.versust -side left
    setForIntegrate $w
    return $win
}

proc swapChoose {win msg winchoose } {
    # global dydx dxdt dydt

    if { "$msg" == "dydt" } {
	pack $winchoose.dxdt -before $winchoose.dydt -side bottom
	oset $win dydx ""
	$winchoose.dydt.lab config -text "dy/dt"
    } else {
	pack forget $winchoose.dxdt
	oset $win dxdt 1
	oset $win dydx " "
	$winchoose.dydt.lab config -text "dy/dx"
    }
}


proc doHelpdf { win } {
    global Parser
    doHelp $win [join [list \
			 [mc  {
SOLVER/PLOTTER FOR SYSTEMS OF DIFFERENTIAL EQUATIONS

To quit this help click anywhere on this text.

Clicking at a point computes the trajectory (x(t),y(t)) starting at that \
point, and satisfying the differential equations		
      dx/dt = dxdt       dy/dt = dydt

By clicking on Zoom, the mouse will allow you to zoom in on a region \
of the plot. Each click near a point magnifies the plot, keeping the center \
at the point you clicked. Depressing the SHIFT key while clicking \
zooms in the opposite direction. To resume computing trajectories click \
on Integrate.

Clicking on Config will open a menu where several settings can be changed, \
such as the differential equations being solved, the intial point for the \
trajectory to be computed, the direction of integration for that trajectory, \
the time step for each integration interval and the number of integration \
steps (nsteps). Replot is used to update the plot with the \
changes made in the Config menu.

Holding the right mouse button down while moving the mouse will drag \
(translate) the plot sideways or up and down.

The plot can be saved as a postscript file, by clicking on Save.
} ] $Parser(help)]]
}

proc setForIntegrate { win} {
    makeLocal $win c
#    $c delete printrectangle
    bind $c  <1> "doIntegrateScreen $win %x %y "
}

# sample procedures
# proc xff { t x y } { return [expr {$x + $y }] }
# proc yff { t x y } { return [expr {$x - $y }] }

proc doIntegrateScreen { win sx sy  } {
    makeLocal $win c
    doIntegrate $win [storx$win [$c canvasx $sx]] [story$win [$c canvasy $sy]]
}

proc doIntegrate { win x0 y0 } {
    # global xradius yradius c tstep  nsteps
    #    puts "dointegrate $win $x0 $y0"
    makeLocal $win xradius yradius c tstep  nsteps direction linewidth tinitial versus_t xmin xmax ymin ymax
    linkLocal $win didLast trajectoryStarts
    set rtosx rtosx$win ; set rtosy rtosy$win
    set x1 [$rtosx $xmin]
    set y1 [$rtosy $ymax]
    set x2 [$rtosx $xmax]
    set y2 [$rtosy $ymin]
    oset $win trajectory_at [format "%.10g  %.10g" $x0 $y0]
    lappend trajectoryStarts [list $x0 $y0]

    set didLast {}
    # puts "doing at $trajectory_at"
    set steps $nsteps
    set hx [expr {$xradius / 100.0}]
    set hy [expr {$yradius / 100.0}]
    if { "$tstep" == "" } {
	set h [expr {[vectorlength $xradius $yradius] / 200.0}]
	set tstep $h
    } else {set h $tstep }

    # puts h=$h
    set todo "1"
    switch -- $direction {
	forward { set todo "1" }
	backward { set todo "-1" }
	both { set todo "-1 1" }
    }
    set methods ""
    foreach method { fieldlines curves } {
				    set color [oget $win $method]
				    if {"$color" != "" && "$color" != "blank"} {
					lappend methods $method
					lappend useColors $method $color
				    }
				}
    set methodNo -1
    foreach method $methods {
			     incr methodNo
			     set linecolor [assoc $method $useColors ]
			     #    puts method=$method
			     set signs $todo
			     if {"$method" == "curves"} {
				 set signs "-1 1"
				 set coords {}
			     }
			     foreach sgn $signs {
				 set arrow "none"
				 if { "$method"=="fieldlines" } {
				     if { $sgn < 0 } {
					 set arrow "first"
					 set coords {}
				     } else {
					 if { "$direction" == "forward"} {
					     set arrow "last"
					     set coords {}
					 }
				     }
				 }
				 set h1 [expr {- $hx}]
				 set h2 [expr {- $hy}]
				 set form [list $method xff yff $tinitial $x0 $y0 $hx $hy $steps $sgn]
				 set ans [eval $form]
				 lappend didLast $form

				 #puts "doing: $form"
				 set i 0
				 set lim [expr {$steps * 3}]
				 catch {
				     while { $i <= $lim } {
					 set xn [$rtosx [lindex $ans [incr i]]]
					 set yn [$rtosy [lindex $ans [incr i]]]
					 incr i
					 # puts "$xn $yn"
					 # Tests if point is inside the domain.
					 # In version 1.14 there was an if:
					 # abs($xn1)+abs($yn1)+abs($xn2)
					 # +abs($yn2) < [expr {pow(10.0,9)}]
					 # to fix a bug in win95 version.
					 # That's now out of context, but the
					 # bug might reappear. (J. Villate)
					 if { ($xn-$x1)*($xn-$x2) <= 0 &&
					      ($yn-$y1)*($yn-$y2) <= 0 } {
					     lappend coords $xn $yn
					 }
				     }
				 }
				 if { [llength $coords] > 3 } {
				     $c create line $coords -tags path \
					 -width $linewidth -fill $linecolor \
					 -arrow $arrow
				     if { "$direction" == "both" } {
					 set coords [lrange $coords 2 3]
				     }
				 }
			     }
			 }
    if { $versus_t } { plotVersusT $win}
}


proc plotVersusT {win } {
    linkLocal $win didLast dydt dxdt parameters xcenter xradius ycenter yradius
    set nwin .versust.plot2d
    if { "$parameters" != ""  } {
	set pars ", $parameters"
    } else {
	set pars ""
    }
    oset $nwin themaintitle "dy/dt=$dydt, dx/dt=$dxdt $pars"
    lappend plotdata [list maintitle [list oget $nwin themaintitle]]

    set max [expr {$xcenter + $xradius}]
    set min [expr {$xcenter - $xradius}]
    if { ($ycenter + $yradius) > $max } {
	set max [expr {$ycenter + $yradius}]
    }
    if { ($ycenter - $yradius) < $min } {
	set min [expr {$ycenter - $yradius}]
    }

    foreach v $didLast {
	set ans [eval $v]
	desetq "tinitial x0 y0 hx hy steps sgn" [lrange $v 3 end]
	set this [lrange $v 0 5]
	if { [info exists doing($this) ] } { set tem $doing($this) } else {
	    set tem ""
	}
	set doing($this) ""
	set allx "" ; set ally "" ; set allt ""
	set ii 0
	foreach {t x y } $ans {
	    lappend allx $x
	    lappend ally $y
	    lappend allt $t
	    incr ii
	}
	
	foreach u $tem v [list $allx $ally $allt] {
	    if { $sgn > 0 } { lappend doing($this) [concat $u $v]} else {
		lappend doing($this) [concat [lreverse $v] $u]
	    }
	}
    }

    foreach {na val } [array get doing] {
	lappend plotdata [list xaxislabel "t"]
	lappend plotdata [list label [oget $win xaxislabel]] [list plotpoints 0]
	lappend plotdata [list xversusy [lindex $val 2] [lindex $val 0] ]
	lappend plotdata [list label [oget $win yaxislabel]]	
	lappend plotdata [list xversusy [lindex $val 2] [lindex $val 1] ]
    }
    if { ![winfo exists .versust] } {
	toplevel .versust
    }

    plot2d -data $plotdata -windowname $nwin -ycenter [expr {($max+$min)/2.0}] -yradius [expr {($max-$min)/2.0}]
    wm title .versust [concat [oget $win xaxislabel] [mc " and "] [oget $win yaxislabel] [mc " versus t"]]
}

proc lreverse { lis } {
    set ans ""
    set i [llength $lis]
    while { [incr i -1]>=0 } {
	lappend ans [lindex $lis $i]
    }
    return $ans
}


#
#-----------------------------------------------------------------
#
# $rtosx,$rtosy --  convert Real coordinate to screen coordinate
#
#  Results: a window coordinate
#
#  Side Effects:
#
#----------------------------------------------------------------


#
#-----------------------------------------------------------------
#
# $storx,$story --  Convert a screen coordinate to a Real coordinate.
#
#  Results:
#
#  Side Effects:
#
#----------------------------------------------------------------
#

proc drawArrowScreen { c atx aty dfx dfy color } {

    set x1 [expr {$atx + $dfx}]
    set y1 [expr {$aty + $dfy}]
    #   set x2 [expr {$atx + .8*$dfx +.1* $dfy}]
    #   set y2 [expr {$aty + .8*$dfy - .1* $dfx}]
    #   set x3 [expr {$atx + .8*$dfx -.1* $dfy}]
    #   set y3 [expr {$aty + .8*$dfy + .1* $dfx}]
    $c create line $atx $aty $x1 $y1 -tags arrow -fill $color -arrow last -arrowshape {3 5 2}
    #  $c create line $x2 $y2  $x1 $y1 -tags arrow -fill red
    #  $c create line $x3 $y3 $x1 $y1 -tags arrow -fill red
}

proc drawDF { win tinitial } {
    global axisGray
    makeLocal  $win xmin xmax xcenter ycenter c ymin ymax transform vectors xaxislabel yaxislabel
    linkLocal $win nobox axes

    # flush stdout
    set rtosx rtosx$win ; set rtosy rtosy$win
    set storx   storx$win  ;   set story   story$win
    set stepsize 30
    set min 100000000000.0
    set max 0.0
    set t0 $tinitial
    set xfactor [lindex $transform 0]
    set yfactor [lindex $transform 3]
    set extra $stepsize
    set uptox [$rtosx $xmax]
    set uptoy [$rtosy $ymin]
#    set uptox [expr {[$rtosx $xmax] + $extra}]
#    set uptoy [expr {[$rtosy $ymin] + $extra}]
#    set uptox [expr {[$rtosx $xmax] + $extra}]
#    set uptoy [expr {[$rtosy $ymin] + $extra}]
    # draw the axes:
    #puts "draw [$rtosx $xmin] to $uptox"
    if { "$vectors" != "" && "$vectors" != "blank" } {
	for { set x [expr {[$rtosx $xmin] + $extra}] } { $x < $uptox } { set x [expr {$x +$stepsize}] } {
	    for { set y [expr {[$rtosy $ymax] + $extra}] } { $y < $uptoy } { set y [expr {$y + $stepsize}] } {
		set args "$t0 [$storx $x] [$story $y]"
		set dfx [expr {$xfactor * [eval xff $args]}]
		# screen y is negative of other y
		set dfy [expr  {$yfactor * [eval yff $args]}]
		# puts "$dfx $dfy"
		set len  [vectorlength $dfx $dfy]
		append all " $len $dfx $dfy "
		if { $min > $len } { set min $len }
		if { $max < $len } {set  max $len}
	    }
	}
	set fac [expr {($stepsize -5 -8)/($max - $min)}]
	set arrowmin 8
	set arrowrange [expr {$stepsize -4 - $arrowmin}]
	set s1 [expr {($arrowrange*$min+$arrowmin*$min-$arrowmin*$max)/($min-$max)}]
	set s2 [expr {$arrowrange/($max-$min) }]
	# we calculate fac for each length, so that
	# when we multiply the vector times fac, its length
	# will fall somewhere in [arrowmin,arrowmin+arrowrange].
	# vectors of length min and max resp. should get mapped
	# to the two end points.
	# To do this we set fac [expr {$s1/$len + $s2}]
	# puts "now to draw,s1=$s1 s2=$s2,max=$max,min=$min"
	# puts "xfactor=$xfactor,yfactor=$yfactor"
	
	set i -1
	for { set x [expr {[$rtosx $xmin] + $stepsize}] } { $x < $uptox } { set x [expr {$x +$stepsize}] } {
	    for { set y [expr {[$rtosy $ymax] + $stepsize}] } { $y < $uptoy } { set y [expr {$y + $stepsize}] } {
		
		
		set len [lindex $all [incr i]]
	
		set fac [expr {$s1/$len + $s2}]
		set dfx [lindex $all [incr i]]
		set dfy [lindex $all [incr i]]
		#puts "[$storx $x] [$story $y] x=$x y=$y dfx=$dfx dfy=$dfy fac=$fac"
		# puts "$len $dfx $dfy"
		drawArrowScreen $c $x $y [expr {$fac * $dfx}] [expr {$fac * $dfy} ] $vectors
	    }
	}
    }

    set x1 [rtosx$win $xmin]
    set y1 [rtosy$win $ymax]
    set x2 [rtosx$win $xmax]
    set y2 [rtosy$win $ymin]
    # Draw the two axes
    $c del axes
    if { $xmin*$xmax < 0 && ($axes == {y} || $axes == {xy}) } {
	if { $nobox == 0 } {
	    $c create line [$rtosx 0] $y1 [$rtosx 0] $y2 -fill $axisGray \
		-tags axes
	} else {
	    $c create line [$rtosx 0] $y1 [$rtosx 0] $y2 -width 2 \
		-arrow "first" -tags axes
	}
    }
    if { $ymin*$ymax < 0  && ($axes == {x} || $axes == {xy}) } {
	if { $nobox == 0 } {
	    $c create line $x1 [$rtosy 0] $x2 [$rtosy 0] -fill $axisGray \
		-tags axes
	} else {
	    $c create line $x1 [$rtosy 0] $x2 [$rtosy 0] -width 2 \
		-arrow "last" -tags axes
	}
    }
    # Draw the plot box
    if { "[$c find withtag printrectangle]" == "" && $nobox == 0 } {
	$c create rectangle $x1 $y1 $x2 $y2 -tags printrectangle -width 2
	marginTicks $c [storx$win $x1] [story$win $y2] [storx$win $x2] \
	    [story$win $y1] "printrectangle marginticks"

    }
    # Write down the axes labels
    $c del axislabel
    if {$nobox != 0  && $xmin*$xmax < 0  && ($axes == {y} || $axes == {xy})} {
	set xbound [expr { [$rtosx 0] - 30}]
    } else {
	set xbound [expr {$x1 - 30}]
    }
    $c create text $xbound [expr {$y1 - 6}] -anchor sw \
       -text [oget $win yaxislabel] -font {helvetica 16 normal} -tags axislabel
    if {$nobox != 0  && $ymin*$ymax < 0  && ($axes == {x} || $axes == {xy})} {
	$c create text [expr {$x2 - 5}] [expr { [$rtosy 0] + 15}] \
	    -anchor ne -text [oget $win xaxislabel] \
	    -font {helvetica 16 normal} \
	    -tags axislabel
    } else {
	$c create text [expr {($x1 + $x2)/2}] [expr {$y2 + 35}] \
	    -anchor center -text [oget $win xaxislabel] \
	    -font {helvetica 16 normal} -tags axislabel
    }

}

proc parseOdeArg {  s } {
    set orig $s
    set w "\[ ]*"
    set exp "\[dD]$w\\($w\(\[xyz])$w,$w\(\[xyt])$w\\)$w=(\[^;]+)"
    while { [regexp -- $exp $s junk x t expr ] } {
	lappend ans  -d${x}d$t
	lappend ans $expr
	regexp -indices $exp $s junk x t expr
	set s [string range $s [lindex $junk 1] end]
    }
    if { ![info exists ans] || ([llength $ans] == 2 && "[lindex $ans 0]" != "-dydx") } {
	error [mc "bad -ode argument:\n$orig\nShould be d(y,x)=f(x,y)
       OR d(x,t)=f(x,y) d(y,t)=g(x,y)"]
    }
    return $ans
}



proc plotdf { args } {
    global plotdfOptions   printOption printOptions plot2dOptions
    # puts "args=$args"
    # to see options add: -debug 1
    set win [assoc -windowname $args]
    if { "$win" == "" } {set win [getOptionDefault windowname $plotdfOptions] }
    if { "[set ode [assoc "-ode" $args]]" != "" }  {
	set args [delassoc -ode $args]
	set args [concat [parseOdeArg $ode] $args]
    }
    global [oarray $win]
    getOptions $plotdfOptions $args -usearray [oarray $win]

    makeLocal $win dydx

    if { "$dydx" !="" } { oset $win dxdt 1 ; oset $win dydt $dydx }
    setPrintOptions $args
    foreach v {trajectoryStarts recompute} {
	catch { unset [oloc $win $v]  }
    }

    makeFrameDf $win
    oset $win sliderCommand sliderCommandDf
    oset $win trajectoryStarts ""

    oset $win maintitle [concat "makeLocal $win  dxdt dydt dydx ;"  \
			     {if { "$dydx" == "" } { concat "dx/dt = $dxdt , dy/dt = $dydt"}  else {
				 concat "dy/dx = $dydt" } } ]
    replotdf $win
}

proc replotdf { win } {
    global printOption plotdfOptions
    linkLocal $win xfundata data psfile
    if { ![info exists data] } {
	set data ""
	
    }
    makeLocal $win c dxdt dydt tinitial nsteps xfun trajectory_at parameters
    setUpTransforms $win 0.8
    setXffYff $dxdt $dydt $parameters
    $c delete all
    setForIntegrate $win
    oset $win curveNumber -1
    drawDF $win $tinitial
    if { "$trajectory_at" != "" } {
	eval doIntegrate $win  $trajectory_at
    }
    set xfundata ""
    foreach v [sparseListWithParams $xfun {x y t} $parameters ] {
	proc _xf {  x  } "return \[expr { $v } \]"
	regsub "\\$" $v "" label
	lappend xfundata [list label $label] \
	    [linsert [calculatePlot $win _xf $nsteps]  \
		 0 xversusy]
    }
    redraw2dData $win -tags path

    # Create a PostScript file, if requested
    if { $psfile != "" } {
	set printOption(psfilename) $psfile
	writePostscript $win
	$c delete printoptions
	eval [$win.menubar.close cget -command]
    }
}

proc setXffYff { dxdt dydt parameters } {

    proc xff { t x y } "expr { [sparseWithParams $dxdt { x y} $parameters] }"
    proc yff { t x y } "expr { [sparseWithParams $dydt { x y} $parameters] } "
}

proc doConfigdf { win } {
    desetq "wb1 wb2" [doConfig $win]
    makeLocal $win buttonFont
    frame $wb1.choose1
    set frdydx $wb1.choose1
    button $frdydx.dydxbut -command "swapChoose $win dydx $frdydx " \
	-text "dy/dx" -font $buttonFont
    button $frdydx.dydtbut -command "swapChoose $win dydt $frdydx" \
	-text "dy/dt,dx/dt" -font $buttonFont
    mkentry $frdydx.dxdt [oloc $win dxdt] "dx/dt" $buttonFont
    mkentry $frdydx.dydt [oloc $win dydt] "dy/dt" $buttonFont
    pack $frdydx.dxdt  $frdydx.dydt -side bottom  -fill x -expand 1
    pack $frdydx.dydxbut $frdydx.dydtbut -side left -fill x -expand 1

    foreach w {parameters xfun linewidth xradius yradius xcenter ycenter tinitial versus_t nsteps direction curves vectors fieldlines } {
	mkentry $wb1.$w [oloc $win $w] $w $buttonFont
	pack $wb1.$w -side bottom -expand 1 -fill x
    }
    mkentry $wb1.trajectory_at [oloc $win trajectory_at] \
	"Trajectory at" $buttonFont
    bind $wb1.trajectory_at.e <KeyPress-Return> \
	"eval doIntegrate $win \[oget $win trajectory_at\] "
    pack  $wb1.trajectory_at   $frdydx    -side bottom -expand 1 -fill x
    if { "[oget $win dydx]" != "" } { swapChoose $win dydx $frdydx }
    setForIntegrate $win
}

proc sliderCommandDf { win var val } {
    linkLocal $win recompute
    updateParameters $win $var $val
    set com "recomputeDF $win"
    # allow for fast move of slider...
    #mike FIXME: this is a wrong use of after cancel
    after cancel $com
    after 50 $com
}

proc recomputeDF { win } {
    linkLocal $win  recompute
    if { [info exists recompute]  } {
	incr recompute
	return
    } else {
	#	puts "set recompute 1"
	set recompute 1
    }
    linkLocal $win trajectoryStarts  c tinitial dxdt dydt parameters
    set redo 0
    set trajs ""

    catch {     set trajs $trajectoryStarts}


    while { $redo != $recompute } {
	#	puts "	setXffYff $dxdt $dydt $parameters"
	setXffYff $dxdt $dydt $parameters
	#	$c delete path point arrow
	$c delete all
	catch { unset  trajectoryStarts }
	set redo $recompute
	foreach pt $trajs {
	    desetq "x0 y0" $pt
	    catch { doIntegrate $win $x0 $y0 }
	    update
	    if { $redo != $recompute } { break }
	}
	if  { $redo == $recompute } {
	    catch { drawDF $win $tinitial }
	}
    }
    #    puts "    unset recompute"
    unset recompute
}


## endsource plotdf.tcl
