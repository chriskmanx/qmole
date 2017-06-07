
proc cIDEMenuCreatePopup {menu fun box} {
    global tcl_platform

    menu $menu -tearoff 0
    if {$fun != ""} {eval $fun $menu}

    bind $box <Button-3> [list tk_popup $menu %X %Y]
    switch -exact -- $tcl_platform(platform) windows {
	bind $box <Key-App> [list tk_popup $menu %X %Y]
    }
    return $menu
}

proc cIDECreateEvent {text label code} {
    set z [winfo toplevel $text]
    set event "<<[join $label -]>>"
    bind $text $event $code
    return [list event generate $text $event]
}

