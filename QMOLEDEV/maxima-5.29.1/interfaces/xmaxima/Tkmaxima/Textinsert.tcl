# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Textinsert.tcl,v 1.4 2004-03-21 07:30:58 vvzhy Exp $
#
###### Textinsert.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. #
############################################################

proc mkTextItem { c x y args  } {

    global _fixed_default _prop_default fontSize
    set helvetica $_prop_default
    set courier $_fixed_default

    set font [assoc -font $args [list $helvetica $fontSize]]
    set tags [assoc -tags $args {}]
    set item [$c create text $x $y -text " " -width 440 -anchor n -font $font -justify left]
    append tags text
    foreach v $tags { $c addtag $v withtag $item}
    $c bind text <1> "textB1Press $c %x %y"
    $c bind text <B1-Motion> "textB1Move $c %x %y"
    $c bind text <Shift-1> "$c select adjust current @%x,%y"
    $c bind text <Shift-B1-Motion> "textB1Move $c %x %y"
    $c bind text <KeyPress> "textInsert $c %A"
    $c bind text <Return> "textInsert $c \\n"
    $c bind text <Control-h> "textBs $c"
    $c bind text <BackSpace> "textBs $c"
    $c bind text <Delete> "textDel $c"
    $c bind text <2> "textPaste $c @%x,%y"
}


## endsource textinsert.tcl
