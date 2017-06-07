# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#	$Id: Feedback.tcl,v 1.1 2002-09-19 16:13:50 mikeclarkson Exp $
#
# Id: feedback.tcl,v 1.3 1995/02/23 00:23:04 sls Exp
#
#
#    Description:
#          A little feedback widget, used to indicate progress.
#

object_toplevel Feedback {

    param steps 10
    param title
    param barwidth 200
    param barheight 20
    param barcolor DodgerBlue

    member iCount 0

    doc {
	A little feedback widget, used to indicate progress
    }

    method create {} {

	$self config -bd 4 -relief ridge
	label $self.title
	pack $self.title -side top -fill x -padx 2 -pady 2

        frame $self.spacer
        frame $self.bar -relief raised -bd 2 -highlightthickness 0
	pack $self.spacer $self.bar -side top -padx 10 -anchor w
        label $self.percentage -text 0%
	pack $self.percentage -side top -fill x -padx 2 -pady 2
	# should not be . - should be .main0
        wm transient $self ""
        wm title $self [M "Please wait..."]

	$self.title config -text $slot(title)
	$self.spacer config -width $slot(barwidth)
	$self.bar config -height $slot(barheight) -bg $slot(barcolor)

    }

    method reconfig {} {
        center_window $self
	update
    }

    method destroy {} {
	# catch required
	catch {
	    if {[grab current $self] == $self} {
		grab release $self
	    }
	}
	update
	catch {destroy $self}
    }

    method grab {} {
	while {[catch {grab set $self}]} {
	}
    }

    method reset {} {
	set slot(iCount) -1
	$self step
    }

    method step {{inc 1}} {
	if {$slot(iCount) >= $slot(steps)} {
	    return $slot(steps)
	}
        incr slot(iCount) $inc
        set fraction [expr 1.0*$slot(iCount)/$slot(steps)]
        $self.percentage config -text [format %.0f%% [expr 100.0*$fraction]]
        $self.bar config -width [expr int($slot(barwidth)*$fraction)]
        update
	return $slot(iCount)
    }

    method set_title {title} {
	set slot(title) $title
	$self.title config -text $slot(title)
    }
}

