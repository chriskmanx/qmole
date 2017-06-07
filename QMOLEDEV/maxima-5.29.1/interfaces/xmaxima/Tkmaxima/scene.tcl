#  $Id: scene.tcl,v 1.2 2011-03-19 23:13:04 villate Exp $
#
###### scene.tcl ######
# Copyright (c) 2011, Jaime E. Villate <villate@fe.up.pt>
#
# Interface to the Visualization Toolkit (VTK) for Xmaxima
# (the license information can be found in COPYING.tcl)

global sceneOptions
set sceneOptions {
    {background {0 0 0} {red green and blue values for the background}}
    {width 500 {Rendering screen width in pixels}}
    {height 500 {Rendering screen height in pixels}}
    {windowname {.scene} {Window name}}
    {windowtitle {Xmaxima: scene} {Window title}}
    {azimuth 135 {Azimuth angle}}
    {elevation 60 {Elevation angle}}
    {tstep 10 {Time interval between iterations, in miliseconds}}
    {restart 0 {If different from zero, the animation will loop forever.}}
    {objects  {} {A list of Objects of the form {{Class {options}} ...}}} 
}


proc PlayStep {frame} {
    linkLocal $frame playing iteration prev_iteration maxiter trackers \
        restart animate tstep renwin
    if { $iteration<0 } {
        foreach obj $trackers {
            set name [lindex $obj 0]
            "$name:points" Initialize
            "$name:points" Modified
            "$name:lines" Initialize
            "$name:lines" Modified
        }
      }
    if {$iteration < $maxiter } {
        incr iteration
        uplevel $frame.mb.play configure -image ::img::pause
        foreach anim_obj $animate {
            set name [lindex $anim_obj 0]
            set prop [lindex $anim_obj 1]
            set values [lindex $anim_obj 3]
            if { $iteration < [llength $values] } {
                eval "$name:actor" "Set$prop" [lindex $values $iteration]
            } elseif { $prev_iteration < [llength $values] } {
                eval "$name:actor" "Set$prop" [lindex $values end]
            }
        }
        foreach obj $trackers {
            set name [lindex $obj 0]
            if { $iteration <= [lindex $obj 1] } {
                eval "$name:points" InsertPoint $iteration ["$name:actor" GetPosition]
                "$name:points" Modified
                if { $iteration<1 } {
                    "$name:lines" InsertNextCell 1
                    "$name:lines" InsertCellPoint $iteration
                } else {
                    "$name:lines" InsertNextCell 2
                    "$name:lines" InsertCellPoint $prev_iteration
                    "$name:lines" InsertCellPoint $iteration
                    "$name:lines" Modified
                }
            } elseif { $prev_iteration < [lindex $obj 1] } {
                eval "$name:points" InsertPoint [lindex $obj 1] ["$name:actor" GetPosition]
                "$name:points" Modified
                "$name:lines" InsertNextCell 2
                "$name:lines" InsertCellPoint $prev_iteration
                "$name:lines" InsertCellPoint [lindex $obj 1]
                "$name:lines" Modified
            }
        }
        if { $iteration<1 } {
            set prev_iteration 0
        } else {
            set prev_iteration [expr $iteration-1]
        }
        $renwin Render
    }
    if {$iteration < $maxiter } {
        if {$playing == 1} {
            after $tstep PlayStep $frame
        }
    } else {
        if {$restart} {
            set iteration -1
            set trackers {}
            after $tstep PlayStep $frame
        } else {
            uplevel stopAnimation $frame
        }
    }
}

proc playAnimation {frame} {
    linkLocal $frame playing
    if {$playing == 0} {
        set playing 1
        uplevel PlayStep $frame
    } else {
        uplevel stopAnimation $frame
    }
}
proc stopAnimation {frame} {
    linkLocal $frame iteration playing
    after cancel PlayStep $frame
    set playing 0
    uplevel $frame.mb.play configure -image ::img::play
}
proc startAnimation {frame} {
    linkLocal $frame iteration prev_iteration playing
    after cancel PlayStep $frame
    set playing 0
    set iteration -1
    set prev_iteration 0
    uplevel PlayStep $frame
    uplevel $frame.mb.play configure -image ::img::play
}
proc endAnimation {frame} {
    linkLocal $frame maxiter iteration prev_iteration
    after cancel PlayStep $frame
    set prev_iteration $iteration
    set iteration [expr $maxiter-1]
    uplevel PlayStep $frame
}

proc makeVTKFrame { w type } {
    global writefile doExit fontSize buttonfont maxima_priv 
    linkLocal $w width height restart renwin windowtitle
    set win $w
    if { "$w" == "." } {
        set w ""
    } else {
	catch { destroy $w}
	frame $w
    }
    set dismiss "destroy [winfo toplevel $win]"
    if { "$doExit" != "" } {set dismiss $doExit } 	
    oset $w type $type

    set top $w
    catch { set top [winfo parent $w]}
    catch {
	wm title $top $windowtitle
	wm iconname $top "scene"
    }

    set buttonFont $buttonfont
    oset $win buttonFont $buttonfont
    set mb [frame $w.menubar]
    pack $mb -fill x

    set dismiss [concat "vtkCommand DeleteAllObjects;" \
                     "after cancel PlayStep $mb.play;" $dismiss \
                     "; clearLocal $win"]
    # define exit command fot the scene window
    wm protocol $top WM_DELETE_WINDOW \
        "after cancel PlayStep $mb.play; vtkCommand DeleteAllObjects; destroy $top; clearLocal $win"
 
    button $mb.play -image ::img::play -text [mc "Play"] \
        -command "playAnimation $w"
    button $mb.start -image ::img::start -text [mc "Start"] \
        -command "startAnimation $w"
    button $mb.end -image ::img::end -text [mc "End"] \
        -command "endAnimation $w"
    button $mb.config -image ::img::config -text [mc "Config"]
    button $mb.close -image ::img::close -text [mc "Close"] -command $dismiss
    if {$restart} {
        $mb.end configure -state disabled
    }
    set c $w.c
    oset $win c $c
    canvas $c  -borderwidth 2 -scrollregion {-1200 -1200 1200 1200}

    # Create a Tk widget that we can render into.
    set vtkw [vtkTkRenderWidget $c.ren -width $width -height $height -rw $renwin]
    # Setup Tk bindings and VTK observers for that widget.
    ::vtk::bind_tk_render_widget $vtkw

    pack $vtkw -side left -fill both -expand 1
    pack $mb.play $mb.start $mb.end -side left
    pack $mb.close $mb.config -side right
# FIX ME: these bindings do not work inside the VTK frame, because it
# has its own bindings.
    bind $c <Control-w> $dismiss
    bind $c <p> "playAnimation $w"
    pack $c -side right -expand 1 -fill both
    pack $w
    focus $w
    return $w
}

proc scene { args } {
    if {[catch {package require vtk; package require vtkinteraction}]} {
	 bgerror [mc "VTK is not installed, which is required for Scene"]
	 return
    }
    global sceneOptions
    set win [assoc -windowname $args]
    if { "$win" == "" } {set win [getOptionDefault windowname $sceneOptions] }
    global [oarray $win]
    getOptions $sceneOptions $args -usearray [oarray $win]
    set renderer [set win]:renderer
    set renwin [set win]:renwin
    set iren [set win]:iren
    oset $win maxiter 0
    oset $win trackers "" 
    oset $win playing 0
    oset $win iteration 0
    oset $win prev_iteration 0
    oset $win animate ""
    oset $win renderer $renderer
    oset $win renwin $renwin

    vtkRenderer $renderer

    # Create the render window and put the renderer in it
    vtkRenderWindow $renwin
    $renwin AddRenderer $renderer
    # Change from the default interaction style to Trackball
    vtkRenderWindowInteractor $iren
    $iren SetRenderWindow $renwin
    vtkInteractorStyleTrackballCamera style
    $iren SetInteractorStyle style
    # create the frame with the buttons and the rendering widget
    makeVTKFrame $win scene
    # put the objects into the widget
    updateScene $win
}

proc updateScene { win } {
    linkLocal $win background azimuth elevation objects maxiter trackers \
        animate renwin renderer
    set objcount 0
    foreach obj $objects {
        incr objcount
        set name "object$objcount"
        set class [lindex $obj 0]
        set anim_obj [lindex $obj 4]
        "vtk$class\Source" $name
        foreach prop [lindex $obj 1] {
            eval $name "Set$prop"
        }
        vtkPolyDataMapper "$name:mapper"
        "$name:mapper" SetInputConnection [$name GetOutputPort]
        vtkLODActor "$name:actor"
        "$name:actor" SetMapper "$name:mapper"
        foreach prop [lindex $obj 2] {
            eval ["$name:actor" GetProperty] "Set$prop"
        }
        foreach prop [lindex $obj 3] {
            eval "$name:actor" "Set$prop"
        }
        $renderer AddActor "$name:actor"
        if { [llength $anim_obj] > 2 } {
            lappend animate [linsert $anim_obj 0 $name]
            set n [expr [llength [lindex $anim_obj 2]] - 1]
            if { $n > $maxiter } {
                set maxiter $n
            }
            if { [lindex $anim_obj 1] } {
                lappend trackers "$name [llength [lindex $anim_obj 2]]"
                vtkPoints "$name:points"
                eval "$name:points" InsertPoint 0 [lindex [lindex $anim_obj 2] 0]
                vtkCellArray "$name:lines"
                "$name:lines" InsertNextCell 1
                "$name:lines" InsertCellPoint 0
                vtkPolyData "$name:track"
                "$name:track" SetPoints "$name:points"
                "$name:track" SetLines "$name:lines"
                vtkPolyDataMapper "$name:track:mapper"
                "$name:track:mapper" SetInput "$name:track"
                vtkActor "$name:track:actor"
                "$name:track:actor" SetMapper "$name:track:mapper"
                eval ["$name:track:actor" GetProperty] SetColor [["$name:actor" GetProperty] GetColor]
                $renderer AddActor "$name:track:actor"
            }
        }
    }
    eval "$renderer SetBackground $background"

    # Rotate the camera so the axis are in the position familiar to Physicists
    [$renderer GetActiveCamera] Elevation -90
    [$renderer GetActiveCamera] SetViewUp 0.0 0.0 1.0

    # Set azimuth and elevation
    [$renderer GetActiveCamera] Azimuth $azimuth
    [$renderer GetActiveCamera] Elevation [expr "90 - $elevation"]
    $renderer ResetCamera
    $renwin Render

}

## endsource scene.tcl
