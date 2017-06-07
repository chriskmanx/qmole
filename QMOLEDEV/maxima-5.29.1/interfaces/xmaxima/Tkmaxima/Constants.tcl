# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Constants.tcl,v 1.29 2011-03-20 23:14:57 villate Exp $
#

proc cMAXINITBeforeIni {} {
    global maxima_default
    set maxima_default(plotwindow) multiple

    # from Send-some.tcl
    set maxima_default(sMathServerHost) genie1.ma.utexas.edu
    set maxima_default(iMathServerPort) 4443

    # from Browser.tcl
    set maxima_default(sMathServerHost) localhost
    set maxima_default(iMathServerPort) 4443

    #mike turn these off by default
    set maxima_default(iShowBalloons) 0

    set maxima_default(fontAdjust) 0

    set maxima_default(iConsoleWidth) 80
    set maxima_default(iConsoleHeight) 32

    # Set up Maxima console font
    set cfont [font actual TkFixedFont -family]
    set cfontsize [font actual TkFixedFont -size]
    catch {font delete ConsoleFont}
    font create ConsoleFont -family $cfont -size $cfontsize 
    set maxima_default(ConsoleFont) [list $cfont $cfontsize]

    set maxima_default(iLocalPort) 4008

    set maxima_default(bDebugParse) 0

    # from FileDlg.tcl
    set maxima_default(OpenDir) "~/"
    set maxima_default(OpenFile) "~/.xmaximrc"
    set maxima_default(SaveFile) "~/.xmaximrc"

    # From Browser.tcl
    set maxima_default(defaultservers) {
	nmtp://genie1.ma.utexas.edu/
	nmtp://linux51.ma.utexas.edu/
	nmtp://linux52.ma.utexas.edu/
    }

    global embed_args
    if { "[info var embed_args]" != "" } {
	# the following will be defined only in the plugin
	set maxima_default(defaultservers) nmtp://genie1.ma.utexas.edu/
    }


    # maxima_default(lProxyHttp)
}

proc cMAXINITReadIni {} {
    if {[file isfile ~/.xmaximarc]} {
	if {[catch {uplevel "#0" [list source ~/.xmaximarc] } err]} {
	    tide_failure [M [mc "Error sourcing %s\n%s"] \
			      [file native ~/.xmaximarc] \
			      $err]
	}
    }
}

proc cMAXINITAfterIni {} {
    global maxima_default maxima_priv MathServer
    lMaxInitSetOpts
    set MathServer [list $maxima_default(sMathServerHost) \
			$maxima_default(iMathServerPort) ]

    # from plot3d.tcl
    set maxima_priv(speed) [expr {(9700.0 / (1 + [lindex [time {set i 0 ; while { [incr i] < 1000} {}} 1] 0]))}]

    # from Wmenu.tcl
    global show_balloons
    set show_balloons $maxima_default(iShowBalloons)

    # From Browser.tcl
    global debugParse
    set debugParse $maxima_default(bDebugParse)

    if {[info exists maxima_default(lProxyHttp)] && \
	    [llength $maxima_default(lProxyHttp)] == "2"} {
	#mike FIXME: make this a _default
	set maxima_priv(proxy,http) $maxima_default(lProxyHttp)
    }

}

# Constants
global maxima_priv
set maxima_priv(date) 14/03/2011

# from
if { ![info exists maxima_priv(date)] } {
    set maxima_priv(date) [clock  format [clock seconds] -format {%m/%d/%Y} ]
}

# from Preamble.tcl
set maxima_priv(clicks_per_second) 1000000

# from Getdata1.tcl
set maxima_priv(cachedir) ~/.netmath/cache

# from Plotconf.tcl
global ftpInfo
set ftpInfo(host) genie1.ma.utexas.edu
set ftpInfo(viahost) genie1.ma.utexas.edu

# from Plot2d.tcl
array set maxima_priv { bitmap,disc4 {#define disc4_width 4
#define disc4_height 4
static unsigned char disc4_bits[] = {
    0x06, 0x0f, 0x0f, 0x06};}
    bitmap,disc6 {#define disc_width 6
#define disc_height 6
static unsigned char disc_bits[] = {
    0xde, 0xff, 0xff, 0xff, 0xff, 0xde};}
}

# from xmaxima.tcl
set maxima_priv(options,maxima) {{doinsert 0 "Do an insertion" boolean}}

# from EOctave.tcl
set maxima_priv(options,octave) {{doinsert 1 "Do an insertion" boolean}}

# from EOpenplot.tcl
set maxima_priv(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

# from EHref.tcl
set maxima_priv(options,href) {
    {src "" [mc "A URL (universal resource locator) such as http://www.ma.utexas.edu/foo.om"]}
    {search "" [mc "A string to search for, to get an initial position"]}
    {searchregexp "" [mc "A regexp to search for, to get an initial position"]}
}

# from Preamle.tcl
set maxima_priv(counter) 0
	
# the linelength initially will have Maxima's default value.
set maxima_priv(linelength) 79

# From Browser.tcl
set maxima_priv(sticky) "^Teval$|^program:"
set maxima_priv(richTextCommands) {Tins TinsSlashEnd}
set maxima_priv(urlHandlers) {
    text/html  netmath
    text/plain netmath
    image/gif  netmath
    image/png  netmath
    image/jpeg netmath
    application/postscript "ghostview -safer %s"
    application/pdf "acroread %s"
    application/x-dvi "xdvi %s"
}
set maxima_priv(imagecounter) 0

global evalPrograms
set evalPrograms {  gp gap gb }
#set maxima_priv(options,maxima) {{doinsert 1 "Do an insertion" boolean}}
#set maxima_priv(options,gp) {{doinsert 1 "Do an insertion" boolean}}
#set maxima_priv(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

# Icons from the Tango Desktop Project (http://tango.freedesktop.org)
# "The Tango base icon theme is released to the Public Domain.
# "The palette is in public domain. Developers, feel free to ship it
# "along with your application."

image create photo ::img::brokenimage -format GIF -data {
    R0lGODlhHQAgAOMEAAAAAP9jMcbGxoSEhP///zExY/9jzgCEAP/////////////////////
    //////////yH5BAEAAAIALAAAAAAdACAAAATTcMhJqxU440G6/+AnaRkXnuAgqKRJvi+3th
    1sbzWLE/bh+xoTT+d6/Y6lz2xVIx2fO8+sqHkiBYCsNsvkOa1ATGFMBkiaVfAhSDFTM+r1q
    +NGp6Ew+plnMF7zBHV8fTc3ei4GiYU2hzWJioskjYOPLwGXmJdiBYICj5UamZqbI4ifkBii
    mwVEjqckowJkI0mUqDBjtEGut6ucrC+doBq5EzADwoQkwDccwsRkzMeTL6y6MSrU0DotkoF
    7HsU22SGCHbmFKN9bWdwk7PBmK9fYFhQZEQA7
}

image create photo ::img::play -format GIF -data {
    R0lGODlhFgAWAPZZAEpMSVJUUVRWU1dZVVdaVVhaVVhaVlhbVllbVllaV1lbV1lbWFpbWFl
    cWFteWVxeW11eW11fW19hXF5gXV9iXWBiXmFiXmNkYGRlYmRmYmdoZW9wbnN2cnV3c3x9e5
    KTkJGUkJKUkaChn7K0sLO2srq7ur/AvcDCv8/Rz9jZ2OPk4+Tl4+Xm4+fp5Ofp5ejq5unq5
    +nr5+ns5+rs6Ovs6evt6uzt6uzu6uzu6+zt7O7v7O7w7e/w7e/w7u/x7vDx7vDx7/Hy7/Hy
    8PLz8vP08fP08vT08/T18/X19fX29PX29fb39ff39/f49vf49/j5+Pr6+vr7+vv8+/z8+/z
    8/Pz9/P39/P39/f7+/v///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAFoALAAAAAAWABYAAAejgFqCg4SFhoeI
    iYqLhAQRAoyHBE4jCwCRjVlSQiALmIIEWS9CWSscBJihL6tEWSgWAYyqqzMzTlIjDosIWTM
    2v78/UUggD4mhPsnKNi8+USocxoa8tbWrLy0tSFknCIfUODvKPq0qGxSIvD4/REZI20YfEo
    q87EhPWVkl0vRZOz9OsqTAoCFSvSk5PFz4tCALEhHoPmlBcCKiRC0WMFzcyNFQIAA7
}

image create photo ::img::pause -format GIF -data {
    R0lGODlhFgAWAPUxAFJUUFRWUlVXU1ZYVFdZVVhaVltdWV5hXGVmYmVmY2VnZGZoZW1va9j
    b1Nve2d7g29/h3OHj3+Lk3+Lk4OPl4OPl4eTl4uTm4uXm4uXm4+Xn4+bn4+fp5ujp5ujq5+
    rr6Ovs6e7u7e/w7vj5+Pn5+Pr6+fr7+vv7+/v8+/z8+/z8/Pz9/P39/P39/f3+/f7+/v///
    v///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAADIA
    LAAAAAAWABYAAAaNQJlwSCwaj8ikcslsOpGAQWAwKAyn0sHhGFB5VYyh4atacFWRSyksLKg
    ol1PiHJGQ2DL3hVJSnCV2eG4UFClzRgMqFxd3YooXhkeJHh5rQ24ODSqHRV0XEZZtaBCbki
    oecYKPKn5GXSAeKXhjlGWmIiBglyohuWaIZKpfv0VUVQUGQ1VUBcRPz9DR0kRBADs=
}

image create photo ::img::start -format GIF -data {
    R0lGODlhFgAWAPZZAElLR0tNSk1OS01QTE5QTVFTT1JTT1NVUVVXU1ZYVFdZVVhZVVhaVVh
    bV1lcV1lbWFpdWFtdWVtdWlxdWV1eW19hXGJkYGRlYmtsaW1wbHBxbYKGgISHhIiKho6Pi5
    SWk6Cin6Kjn6utqrGyr7/AvsLEwMjJx8rMydLT0NfY1dnd1trc2d7h2+Lj4ePm4ebo5Ofp5
    efo5ujq5ejq5unq5ujq5+nr5+rs6Ovt6ezt6uzu6uzu6+3u6+3v7O7v7O/w7e/w7/Dy7/Lz
    8fP08vT19PX29Pb39fb29vb39vf39vf49vf49/j59/n5+Pn6+fr7+vv7+/v8+/z8+/z8/Pz
    9/P39/P39/f7+/f7+/v///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAFoALAAAAAAWABYAAAe7gFqCg4SFhoeI
    iYqLjI2OhgqRiAGHAoIINgiGBBuahAcengoqCoUUKVaeWgIaMKmXLKoFIUpDVqUAFChSQFY
    JggoupQIYNk83N6kHI09CNja+wDaRJc0511YdRUrXyK9aCj0KECtWQj3oVi1WQTAwz98KQq
    UOHEpWRUWpH/pFQkLRwBUpBcyElYOaGIg4+CQggycEBVXA0EsVAxIIBTEYCCmEKkEQTniCk
    ADCoQsmC1l48Kily5cwYxoKBAA7
}

image create photo ::img::end -format GIF -data {
    R0lGODlhFgAWAPZyAEZIRUtNSUxOSkxOS01OS01PTFBRT1BSTlJUUFRWUlVXU1ZXU1ZYVFd
    ZVFdZVVdaVVdbVlhZVlhaVllbVllaV1lbV1pbWFlcWF5gXF9gXV5hXWBiXmFiX2JjYGJkYG
    NkYGNkYWRmYmZnY2ZoZGdoZmxtaW5xbG9wbW9wbnBxbnBybnByb3FzcHJzcHN0cXN1cnR1c
    XR1cnZ5dXl6d4GDf4iJhpGSj5CSkJOUkJ6enKOkoa+xrrW3s7q8ur6/vMbIxczNytHS0N/h
    3uDh3+Lj4OTm4+fp5Ofp5ejp5ejq5unr5ujp6Onq6Onr6Ovt6ezt6uzt6+zu6u3u6+3t7e3
    v7O7v7e/v7u/w7vDx7vDy7/Dx8PHy8PHz8PLz8PLz8fPz8/P08fT18/X19PX19fT29PX29f
    b29ff39vj4+Pn6+fr6+vv7+vv7+/v8+/z8+/z8/P39/f7+/v///wAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAHMALAAAAAAWABYAAAfLgHOCg4SFhoeI
    iYqLjI2OhgWHAogQFxCCGDQHhRgyk4UvYQ6CJ3FDJJ9zKXFAEYUnUK6qbllrPwoAc6VkZDi
    bgilXsiVrR0lhaDoIJ2tJUGtEIgFzJUuyLG1V2rVaN21QTklbazwKG0LXbWBb62SmbUlHR1
    tvQQsbRReCMHFaWe5farCIs8UdmRn6OizRN4efuzU5LMx5EWdNnB6y5nSowpDfmh0Z+S358
    IHQRoYjfIwi9MGGxEIbZVkQYShjoQgMMDzaybOnz5+GAgEAOw==
}

image create photo ::img::config -format GIF -data {
    R0lGODlhFgAWAPZaAF9fXnl4dHx8fH19fX9/fyBKhzRlpFhthFtxiWB2j2B3kWN7lU50pEV
    xqVR8rll9q2yFoWWErHWQroaGhoeHh4GKlZGRkZmZmZ6enp+fno2YpYGTqYKXsoSctpeltp
    CmvpGmvqenpq2tra+vr6SttqKuvKexvbW1tbu7u5mtw52wyp22zp240qG0yKS60LvByLDG2
    8HBwc/NyszMzNjY19zc3N/f38DR4tPf6tfi7d/n8OPi4ePj4uTj4efm5Ojn5Ojn5uvp5+rp
    6Orq6uzq6O3r6e3r6vLx7+3y9u7z9vDw8PPy8fPz8vD09/T09Pb29fj39vj49/n49/n59/j
    4+Pn5+Pr6+fr6+vv7+/7+/v///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAFsALAAAAAAWABYAAAfxgFuCggQEg4eI
    iVsERVY0hoqRhFJLj5EcBpEERFaQiAZZDJlbAgIQgiicnoOgMyMRggJBQgQjPZwBiQY2I6N
    bF0JLRURBxVKrBlYGvoIUMlLQU8aQoMyHGAITAidWxU4ZydaSBFY/Lw1O4pKLKrsPIeuHBf
    MGTg4kWQLxWwUSLiAFDJiI0SPLqkT9cJRYsaCAgCxCiBiMVKBDDg0LWCgosCgLEU4TEKbQs
    eEACwkIOHb8aIWCvBZINiQ4mfIQgSw9AGQ5xQ9EEw8zUaq0mWXJklz8YCCpQHMoIgsE9Akq
    AIHFDRA19w2iCqGhVkTznO4LBAA7
}

image create photo ::img::close -format GIF -data {
    R0lGODlhFgAWAPYAAEhKR09STVBSTVBTT1NUUFRVUVRWUlVWUlZXU1VXVFVYVFZYVFZZVFZ
    ZVVdZVVdaVVdZVlhZVFhaVFhaVVhZVlhaVllaVlhaV36BfYCCfYGDfoKDgIKEgIOEgIWGg4
    aHhIaIhYeJhYeJhoiJhYiKhomKhomLhomKh4mLh4mLiIqLiIuNiIuMiY6Qi46QjY+RjZCRj
    ZCSjZCSjpGSjpGSj5GTj5GTkJKUkJSWkZWWkpWXkpaYk5eZlJialZqbl5qcl52fmqWnoqan
    pKaopKeppaeppqippqiqpqmqp6mrp6qrp6qsqKusqKutqaytqqyuqq2uq62vq66vq66wrK+
    wra+xrbCxrrCyrrGyrrGzr7K0sLO0sbS1sQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAF0ALAAAAAAWABYAAAfNgF2Cg4SFhoeI
    iYqLhBYODoiPFocWHEQckIUOGkQbk4UWQTlLJJ+CDiBLPESZjRpLMUspnxYgVDdLmIcOJE4
    5VCwOFixZO1QrrYaoWTxaMTZcPlkgyZQkWT9TVEBcs4xdFi9LPT9SMaaLDi9KPDxO59/DVD
    tKSj1U3oq8SzdSLy9ZcjjJtwuEkhakLFjohQsEukGbhqxQUkpQBBBOWijZUK2LAyExiFAjZ
    OCDkhtKFhgioEEIBwOGDGAI4gGmIQEOACASkIDAt59AgzIKBAA7
}

image create photo ::img::replot -format GIF -data {
    R0lGODlhFgAWAPcAAC9ZjzFbkTRflzZlojZnozNlpDRlpDVlpDRlpTVlpTRmpDVmpDVmpTZ
    mpDZnpDdnpDZmpTdmpTZnpTdnpTZnpjdnpjdopTdopjdopzxpozhopjlopjhppjlppjhopz
    lppztppjpppzpqpztqpztrpzpqqDtqqDprqDtrqDpqqT1rqDxsqD1sqD1sqT5tqT9vqT5tq
    z5uqz9uq0FuqEBuqUBuqkBvq0RxqkFwrENxrEVyrEZ0rkd0rkx4r0d2sEx4sE16sE56sU56
    tFB7slB8slN+s1V+s1eBtVqDt2qOvGqQv2KNw2WOxG+VwWyWynKXw3OYxXeaxXKfz3Sfz3+
    gx36hyn+iy3+kzIKjy4elyoSkzIWmzZSx05Oy1pG02ZS22pa325q42p252Z+52Zu63KC726
    C82qG+3aK+3aO/3aC+3qbA3afA3ajC3qbC4KrE4arG4q3G4a/H4azG4q7I4q7I46/J47DI4
    rHJ47LJ4rTJ4rDJ5LHK5LPK5LTM5bXM5bbN5bfN5bbN5rfN5rjN5brP5rvQ57vR57vR6L3S
    6MDS5sDU6cHV6sPW6sXV6MjY6cja7Mna7Mrb7M3d7c3d7tHf7tTi8NXi8Njj8dnk8Nrl8dv
    m8tzm8tzn8+Dp8+Dp9OHq9OLr9ePs9eTs9eXt9gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAKUALAAAAAAWABYAA
    Aj+AEsJHEiwoMGDCBMWRIFiwYIRKEYYXGCQYZQyc+ywGVPlB0WBFwoUHIEE0B0zcwAVOoQI
    0ZwmIxZA4VFQxxiPIyZc+LEFECA3d85UYaSD4IgiI2DY+FhqxIkqfMB4ARQJhtEFQpZIHNm
    jjZ0plKwORGHDSSkUB2Ho+SLFEtqBTA+OYBOJkqVOFRQSLPAQosO4Ff4K1gt306fDmYwojL
    vg05kzmrBcqCgQx1uBFT6RSaQJqcELTHgUECtwASlJcxh1kjy5NAwYOYoSNJ0IU6U3iwphS
    TJEhQ0kWLhshetoxIgsj3z+4cN8zpswpOHKLpUhCZc7iUymeTICwESDAQgMELAxYYIAwujT
    6w0IADs=
}

image create photo ::img::zoom-in -format GIF -data {
    R0lGODlhFgAWAPcAAAAAAFhaV15gXV9gXmRmYmZoZGdpZWhpZmhqZmlrZmhqZ2lqaGpraGt
    ra2psaGtsaWttaWxuaWxtamxuam1vamxua21ua21vbG5wa29wbG9xbHBxbXZ4dXp8eH+AfV
    mArVyFs2GKuWaQv2iTwmmUxW2XxW6ayW6aym+czXKdz3GeznKfznOfznKez3Kfz3Ofz3Sfz
    nagzXWhz3ihzXihzoOFgIKEgYSGgoiKhYmKiY+Qj5GSkZaWlpmZmZubm6GhoaOkoaeopqio
    qKipqK2trb6+voqmxY6oxYKkyoWmyompy5apwZutwp6vxIWr1o+x2Zaz0pe005e42626yrn
    K3L7O3qnE4q7I47bN5sLCwsTExMnJyMrKycvMyszMzNHR0dLS0tXV1dHV2dTW2dbY2tfY2t
    jY2NnZ2Nra2dja29ra2trb29vb29rb3Nzc3Nzd3t7e3t/f38fY68fZ7NHZ4dLZ4NXc5NLg7
    9vi6dHg8Nbi8dbj8drl8tvm8tvm8+Hh4eLi4ufn5+zs7OHq9efu9wAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAIUALAAAAAAWABYAA
    Aj3AAsJHEiwoEGDHDRs0KCBQoeDBzV0aTNGTJkyXShAJKgBzZISMlq4GMHkDYaNhSigMdJC
    DgAAeq64OIJGI8QgTVzoIfSSzx0pLqYAgZgAjQgrfAb1vOPEBRI0CQ5SeHPipVWrLk4AkiA
    VjYurV0UCOmlwKownWOa8HPjiRM2DNbQkIeGi0FqBIZR0qQHRRh0XKOwCGOiijoGNEoRQkQ
    Fi4AcYVbwQiQqRwg48UWi4oBHFTgJAQngchiihg44sb4rkIJByDI8elCHioEAbh8AGFMD06
    DEaZVkwP4Qw8H2QARgtYQKgFBTIoIA3gIYTNxjA5vTr2AUGBAA7
}

image create photo ::img::zoom-out -format GIF -data {
    R0lGODlhFgAWAPcAAAAAAFhaV15gXV9gXmRmYmZoZGdpZWhpZmhqZmlrZmhqZ2lqaGpraGt
    ra2psaGtsaWttaWxuaWxtamxuam1vamxua21ua21vbG5wa29wbG9xbHBxbXZ4dXp8eH+AfV
    mArVyFs2GKuWaQv2iTwmmUxW2XxW6ayW6aym+czXKdz3GeznKfznOfznKez3Kfz3Ofz3Sfz
    nagzXWhz3ihzXihzoOFgIKEgYSGgoiKhYmKiY+Qj5GSkZaWlpmZmZubm6GhoaOkoaeopqio
    qKipqK2trb6+voqmxY6oxYKkyoWmyompy5apwZutwp6vxIWr1o+x2Zaz0pe005e42626yrn
    K3L7O3qnE4q7I47bN5sLCwsTExMnJyMrKycvMyszMzNHR0dLS0tXV1dHV2dTW2dbY2tfY2t
    jY2NnZ2Nra2dja29ra2trb29vb29rb3Nzc3Nzd3t7e3t/f38DT6cfY68fZ7NHZ4dLZ4NXc5
    NLg79vi6dHg8Nbi8dbj8drl8tvm8tvm8+Hh4eLi4ufn5+zs7ODq9OHq9eLr9eTs9eTs9ufu
    9+zy+AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAIsALAAAAAAWABYAA
    Aj4ABcJHEiwoEGDHDRswKCBQoeDBy90aTNGDBkyXChAJKihzZISMlzAGMHkDYaNiyigMdKC
    zqFCfK64WNKmwcYgTVzsOaQIUR88UlRMAQIxARoRVvoUOuQTjxMZSNAkOEjhzQkAWLNidXE
    ikE2DGNC40KrVhQtAGg1WhfEECx05BLmySVvwRpYkJFwYDKFkyw2INuq4UGHQhZipEBsMoe
    ICxMAPMLB4IYL4YIQdeaDMgEEjyp0IgITwMLCRgoccWeIUyUEg5RkePSofzEGhNg6BDSh86
    dGDNEqqYH4IYfD7IAMwWsIEQDlIkEEBbwIRL24wAF3q2LMvCggAOw==
}

image create photo ::img::save -format GIF -data {
    R0lGODlhFgAWAPYAAFt6clRye1d4fV58eWZoZGdpZG5wa2B9bnJ1a3V5bHd5dXyBbWyFd3+
    Bfj9shjhniz9qiTtskTxtkT9ylklugkJuiEFsik1yglR3gFF5ilN5ilt+iEJwkk15kUR4nE
    d9okd9o22NjnCKgmiJk0qApkqAqE6HsFCIsVCJslGLtVOMtViSvWKWu1uWwVyXw16Zxl6ax
    2CcymCdy2KfzmOgzmeizmOh0Gej0mul03So03So1HWp1YOFgI6PjZGRkZSWkZSUlJeXl5iY
    mJudmaOkoqWlpampqaysrLCwsLGxsLKysri4uLq6uru7u7y8vIGt0Y+228DAwMHBwcTExMX
    FxMXFxcfHx8bMwMnOw8jIyMnJycvLy8zMy8zMzM3Nzc/Pzc7OzsrQxMzRxdLWzNPYzNDQ0N
    HR0dbW1tfX19ja2Nvb2+Dg4OHh4eHi4eLi4eLj4ePj4+Xl5ebm5ufn5+np6Orr6uvr6+zs7
    O3t7fHx8fPz8/b29gAAAAAAAAAAAAAAACH5BAEAAHwALAAAAAAWABYAAAf+gHyCDB0Phg8V
    GIKLjI0cLE85UDszGhSNmHwZNR4TESAzOzggFpmLAyEbixUgMDAtIBWmfBcijREqKy0zAge
    mAAsGwgYJDyvHKyMBwwaNCHvQ0MbIMQ/Re82LPHd7dt7GKMc2D95pdjyMQ3d2e4cr4SszDx
    EPbHU/iwZccN8xMTcoUuhyAYPcHS4IBBmIw8/OmAgxTKCQaMJFBDF27tzJpoCbNztiHkSU6
    OJBGG977jQQ1ONOHGgasYhEUfIktDh3evAxsCSOnTkvoZERGYHMzThs2CgxQGCMnjhQo8a5
    8gCL1KR5wBAwcK2r167C2IAZS5bsFjBn0W45yyZsFjVBPoQUQaLESRMrZqKc0cu3rQE2WdA
    AkWvkyBIpU/LuXRzFL2AwU6JEjhLFShbFfPf63ZK0s+fPnrM0Y0a6NGk+gQAAOw==
}

image create photo ::img::redo -format GIF -data {
    R0lGODlhFgAWAPUAAEmRBUqTBUuUBUyXBU2ZBU6aBlKSDWesD2i/E2nBE2rCFGvEFG7KFG/
    LFXLKHHTUFnXVFnbYFnjbF3XQHHbTHHfTHXfUHXnWHnnXHnrZHnzdHnrUInzWI33aIn3aJJ
    SjA5egIKe0MozPLYDVK4LYKpHTK4XdMJTWPJrcP4fgLInpK4rjMYvjMoriNIvkNIzkNYzkN
    4zqMI7kOo/lO4/lPJDlPZDlPpHlPpHlP5LlQJPlQpnrRqHsVwAAAAAAAAAAACH5BAEAAD0A
    LAAAAAAWABYAAAaMwJ5wSBQWisjksHBUOo2t5jNZaEWnRCazpblOC6CTtWXCOLzKAoo22mw
    8FQwigS4WcK+KXh9pzOlSSyI4FhUOExMQEgsJgFQuGRsvLzg4NDQJDXVZOB0qJVoFMpqBdj
    gqEFJ3m3Y6PIFVpUgFLjuwsrMhdbizHyu8XykGWGkkwF8Hx08AAsRPzc7RQkEAOw==
}

image create photo ::img::home -format GIF -data {
    R0lGODlhFgAWAPYAAFVXU3tMSmVnY3V1dXV2dnJ1enV4e3d4enV4fHZ5fHZ5fZwICKQAAIQ
    iIdwlJdwmJt4mJuAmJuInJ+U5OYhaWJldW511c+xGRvFMTPBRUfJVVfNbW/JeXvNjY/NkZP
    RmZvRsbPR1dX6Ae3+BfIqIZYmIcICCfYiJfjRlpG52gXF4gUp2rl+Gt3eYwnmZw4SGgYSFg
    oWHgoaIg4eJhIiKhY+QjZeZlJiZlpmalpmal5ubmqONi6efnZ+gnqGgnqKjoaKioqOjoqOj
    o6SkpLGztri5uLq6ubq7vL+/vsCKisShocm1tbi8wbm9wcLCwdfX19zQ0NjY2Nra2tvc293
    d3d7e3d7e3uLe3uDg4OLi4uPj4+Tk5OXl5ebm5ufn5+np6erq6uvr6+zs7O3t7e7u7u/v7/
    Dw8PHx8fLy8vPz8/j4+Pn5+QAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAGwALAAAAAAWABYAAAfygGyCg4QMDISI
    iYUhIYeKj2wMjIyOkIsTjBKNloOSFxiZmpWPkhkajZISopCSGxyNARSSD6uJkh4ejRUwOkm
    ztZ0hIB+NFjVFTkVKv5vBuQw7PUhV1E9LCyEPzYKSjT5BUmHiXmFeUNjbkSENPj9RXfDwWl
    1aWNijkVMA+/thXQcJBiQ4YAYfGwBlAIgQMWIEvAQuBrRIYAZAIgBiAMDYCEOLFgQsBqwoU
    PFiRhkySJSopwLFABQpSiLCCIAGjRMmsKwhwuQIkyEJL4apaZPGlTVBgghRWsXizDICbNyY
    mqYMmatX1TglxK+r166cworlFAgAOw==
}

image create photo ::img::next -format GIF -data {
    R0lGODlhFgAWAPYAAC9dAzNmAjduAzt0Azt1Azt3Azx2AzhwBDlzBDpzBDt0BDp1BDt1BDt
    2BDx3BT13BT12CT53CUR9D0V8EE6aBk+aB0+cBkqDFkuBGE6DHlKhCFSjCFWlCVeoClipCl
    iqDFyvDFywDF6zDV+0DV2qE16uEl+lHGC2DmG3DmCvFWGtFmW9EFWLIlaKIliJKVqNKWaVO
    WufOmGlIWanJ2SpImirJ2aoKGqtK2qqLWurLWutLm2sMm+tNHCtNnCuNnOvOne2OnWwPXax
    PnaxP2fBEWnDEWnCE3HGH3PMGnShSniyQXiyQnqzRHu0RXy0R321R361SX+0Sn+2SoOtW4e
    7V4m8WI23Z5e/cYTER47DWpXTWZjJZ5jFb53HdZzKcJ3FeJ/GeKPfaaTPeqPLfqTLfqbPf6
    bWdqfecqbMgqjNhanUgazYgq7bg6/cg7DSkLXVl7fVm7nXnLzZoLzZob7ao77ZpL/apL/ap
    cDbpsHcqQAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAHoALAAAAAAWABYAAAfGgHqCg4SFhoeI
    iYgLio0JLo2JCHcwBpGGCHlwSQqNCwmgoAp3VG5TlokJeKurc3NNTmhWEKlxUbdOTkpBQTx
    cXBOICXFKxbxBPjs4NlRgwYYLbz42MjIzOTk3MzQmSmMtjIQLaRYUFBYbHB8lKiokQF0s4Y
    MJY+boHiAjKCgjWF4XHBiqZ05DhxAoVhAxomXLs4FkIpZRw6aNESRhskRINQCBRwQK2Bw5E
    2XepQRszPQQcIlQAjEuArR0mQHATEIsb+rcSSgQADs=
}

image create photo ::img::previous -format GIF -data {
    R0lGODlhFgAWAPcAAC1ZAzNlAjduAzpyAzp0Azt1Azt3AzpzBDtzBDp0BDt1BDx0BDx0Bjx
    1Bzx2Bzx1CD11CDx2CD13CD52CT92Cj93C0F7C0F9CUJ5DUR7D06aBlOdDVSeD0mCFUmAFk
    yBG1WeEFKgB1enCVenC1elDFekD1mrClqqC1qsC1uuC1ywDF+zDV+0DVqhF16jHGC2DmK4D
    mO6D2O7D2S7D2GxFGm5HFSIIFSKIFSIJVeLJWKSNGaYNmGlIWOmI2SnJWevI2aoKGmrK2uv
    KWqqLWuvLGqwKG2sMW2sMm+tNHCtNnKvOXOvOnKyNnWwPXWxPnaxPnaxP2fBEWjBEWrFEmv
    GEnGeRXWmRXiyQXiyQnqzRH21R361SX+2Sn6qVYGwU4S5Uoa7U4e6VYu2Y4y5YZS8bJTGY5
    bDa5XAbJ3Ob5rEcZrDdJvEdKPOeaTLf6HQdajUfqrXf6rThKzYga3bgK3Zgq7dgK/dg6/Rj
    rTVlbXUmLjWmrnXnbrWn7vYn7LhhLTlhbXmhrzZoLzZob7ao7/apL/apcDbpgAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAIcALAAAAAAWABYAA
    AjfAA8JHEiwoMGDCBMqPJRgYUIcCxwaPKBjTwWJBBNUyVPoIsZDC7rc+SLIwoEEB1I2RChB
    TBssV/YIKlTIkKFCGBBmIGPGSJIlUK5k0bJlSyAJBz2sCcMDyBAjSIAGxaKHQsEEN9ZAaeG
    iaRAhRX78EMIED1KCBm6kQbKBA4cSIkygUKECxYk4ZzN2OKNFQwgRKVjAmCEDxoo3eQtiGA
    OmROAZUahQiRJDzoOECbyUocGijh9Af/zYoZPY4AAraGrMuXASZcqFAna4mVNaYoAcbGpLB
    PDhwEeDAn4LHy4wIAA7}

image create photo ::img::help -format GIF -data {
    R0lGODlhFgAWAPfmAA8jQRMsURUyWhk4Zh5BcyBFeiFJgiJJgiBJhSFKhiBKhyFLhyFLiCJ
    LiCJMiCNNiSlRiylRjClSjCxTjSxVji5Wjy1WkC9XkTNblDVclDRclTRdlTdelTZeljhglz
    hgmDlhmTpimTtjmjxkmkBnnUFpnkJpn0Rrn0Zrn0psnkxunkxun0RroEVsoUhvo0lvo01uo
    E5xoU5xoktxpUtypUxxpExypU10p050p1BxoFByo1JzolB0pFV1pFV2pVl4pVh4plh5plB2
    qFF3qVJ4qlR6rFV7rFd8rVd9rlh8q1x8qFh9rlp/r1p/sFqAsF2Csl6Dsl6Ds1+Ds2KBrWK
    Fs2GFtGGGtGKGtWKHtWOHtmaIs2OItmSItmSJt2yIsGeLuWiMuWuNuGqOu2uOu2uPu2uPvH
    aRtniTt3CTv3eWvnyWu32Wu3uXvXKVwHOWwXOXwXeZw3yawnibxXmcxXydxX6dxHyfyH2fy
    H2gyH6hyX+hyYKcv4edvoeevoCbwIOix4yiwYGiyYCiyoCjyoOkyoOky4Wnzoanzoaozoao
    z4mqz5CnxZKpx5SoxZesyZaty5mxz5ywzJ2xzIip0Iiq0Imq0I2v1I6v1JGy1pKz15+205i
    11p652J+72pu73Z+83KCyzKGzzaO0zaS1zqa40Ka40aO616a61KS716i50Km50ai60qm60q
    m81K+/1a/A1rPB1rTC17LC2LLF27XE2LTE2bXE2bXF2rXF27bG27fH3LjH27rH2rrI273L3
    b7L3cHO38HQ4sLR48HR5MjT48nU5MjW58nX58vX587Z6NPe69ri7N/l7uDm7uDm7+Ho8eHp
    8ePo8ODo8uXq8ebr8ebr8ufs8ufs8+ft9ejt8+nt8+nu9Onu9env9erv9env9uzw9e3x9u7
    y9/Dz9/P1+Pj5+////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAOcALAAAAAAWABYAA
    Aj+AM8JHEiwoMGD5xQoVMCAgUKEBBXC4AOrmTNafVQogJgQAqhpo87EyLEm1TZQEjYaVJCC
    WSoOFjBw6CBCBAtazWCoHKhAQjMvFqbYmlaO3LJFJ0zsaSahoAJRqWKmMke1aqsZM3qdesA
    zxzQOHESc2sVGyymq3orwSOItB09AoDCIMDFDSJEmYaiOg9KkiC1GKhX0MuNhRN0jULgkoy
    rsSxYnj35xTWgtxogWN4pA6YKMqjM4aMJ8YTMt8DYehok4yTKLqrRCduS4GSNn2mQFv9SYs
    KH5izSqnCYZKiQHjSlgtxudOnHjSJYwcApNuoTpkiE7b4A5Cvxj2ozmWcpyKMO27ZMnTJT0
    zAnnI+KrV+DLbANeHZEdZLImD6ywLY4RKmFgQ9UmkyCiByrbVLDSD9bYQsUWZbgxhx56EII
    MOO0dxEAFtmyjCyR1/GFKMOHYQgFHCSkRCTHhhINMJD8YgKJAAQxQgAIGECAAADP2SFBAAD
    s=
    }

image create photo ::img::stop -format GIF -data {
    R0lGODlhFgAWAPf6AFYAAF4AAG4AAHQAAIIAAIMAAIQAAIYAAIgAAIkAAIkEBIgFBYkHB4w
    HB4kICIoJCZkNDZsPD5wQEJ0SEp8UFKIPD6gPD7UAALMGBrUGBrkAALoAALsAAL0AAL4AAL
    8AALsEBL0EBL8EBKwREb8WFrocHLodHakqKqkrK6srK6wqKqwuLrcoKLspKb4uLq8yMq81N
    bY1Nb8wMLA5ObA7O7E+PsAAAMEAAMIAAMMAAMUAAMYAAMAFBcIFBcMFBcgAAMsAAMsLC88Q
    EMsWFsIdHcMfH9EdHdMeHtUfH8UgIMYgIMgkJMgnJ8cpKcosLNggINkqKsszM80xMcw0NM4
    1Nc02Ns83N8s7O884OMg8PNA4ONE5OdE6OtE7O9I7O9I8PNI9PdE/P9Q8PNc/P9g9Pdg+Pt
    g/P7tERL1ERL1HR8FDQ8VCQsVDQ8BFRcFHR8JGRsRERMxFRc1GRsVJScRLS8xISM1JSc5MT
    M1OTsxPT9lBQdlCQtlDQ9lERNlFRdpGRtpHR9RPT9pISNpJSdpKSttLS9pMTNtMTNtNTdtP
    T91PT8BRUcFTU8BUVMFVVcJVVcBXV8JXV8VWVsxRUc5SUsNcXMVdXcRfX8VfX8lZWctbW85
    fX9BVVdpQUNtTU9xQUNxRUdxSUt9SUuRFRelLS+pNTeRTU+FVVeFXV+RUVOpSUulWVutWVu
    ZYWOdbW+laWuhdXehfX+1dXcViYsVjY8ZiYsZjY8dkZMZnZ8hmZsx2dsx7e8t8fONhYeViY
    uNkZOpiYu1mZu9mZuRoaOVqau9paexqau1sbO9sbOxvb+pycu92dvF7e8+IiNWVlfKBgfKI
    iPSJifWMjPWNjfWOjvSPj/WPj/aQkPSTk/aSkvSUlPSVlfaWlveYmPeZmfeamvebm9miouS
    8vN3T097U1N/V1eXV1eXW1ubW1uLY2ObY2ObZ2ejf3+bi4uri4uvj4+7j4+nk5Ovr6+3s7O
    7u7vHj4/Dk5PXm5vbq6vDu7gAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAPoALAAAAAAWABYAA
    Aj+APUJHEiwoMGDCBIqTJAAQYKDBBHU2AbOm7dt3LJFU4EAor4DNLYRU6SIEKE/fVA944hw
    hkhPnhCZRKnnFDMUHSPC2DbMUyZOMQkJkjPHjKllOAcieIGtV6dMzWZRkmmn0a43YhQpS6p
    PQbVeiDqlKTfuEp5AjdS5S5PFC6FiDwQ2yCYIEaJJs8aZqwRJ3btLa75gsQJXbrU/JgnhmV
    WuXbx4s9aIGRylsL4G1fT06WOSTjN69Jyp+eLFipUpwRQY3rzZTpp1oN/N2iTFSZMmxxYYN
    qNHT5xF6NzpulWvHhoXHzhwKHVAbjQyer40QscO15oMufDdi3GBA4lXzbt+CoMiJAidcI1M
    6LBRIpc4NMqJKAuvr4IsJD+GvJGhY70NDJJkwQERprSQk0AUsHJEf/7Z8EFyHChxCgsHDkS
    BKkj0Z8OGECqRyggVEgQBKUfwwIMIKHKwxCkWhEiQABCMwgowwMDiiiu9VDCARwIBcMABBQ
    QZ5AEB8GjkQQEBADs=}

image create photo ::img::track -format GIF -data {
    R0lGODlhFgAWAPYAACBKhyBLiCFLiCFLiSNNiSJMiiRNiSVPiydRjShSjilUkSxXkS9XkTJ
    clzFdmThfmDZhmzhjnDRlpDVlpDVmpDZmpTZnpT1noT5noDdopjhopjpppzpqpztqpzxqpj
    xrpztrqD5sqD5tqT9uqUNso0BqpEFspUdvpEhuo0FuqEBuqUJvqUFvqklxp0NwqENxq0Nyq
    0Rxq0Vxq0ZxqkVyq0RyrUdzrElzqUtzqEp1rUl2rk53rE15r1J6rkh3sEt4sU15sU96sFF7
    sVZ9sFJ+tFN/tVuCtFuCtVuDtVmDtmOLvGGMvWmQwGuSwWiRwmuTwm+XxXGWw3OaxnKfz3O
    fz3ieynOgz3Sgz3igzYCo04So0IWs1Yas1Yet1Yet1oiu1o+y2JCz2ZW325e425y42Z+72p
    q63Jy73aG/36LA36PA36XB4LDI4rHJ47HK5LjO5gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAHAALAAAAAAWABYAAAfcgHCCg4SFExKF
    iYpwEzxNGYuREzBsb0oTkYoZZGNfZB2ZiTpuVlRsIpEBAYQTUWNTU2hJiwELKKuCE01isGp
    AiwZGRie4cERnsGugiQAQQhNGDIMgaFRfWpDMOCoSNje4NmZUZj+YzD0dEho9BIxFYVtV2c
    w7IRMdO6swUltgqLQXbExQUYLAhCVivEAxtygBjwk2IgTQhcXLk2W0SqgQcSGAhB9bsMyLh
    EBHBxMBCmQAkYFhKggqbjQoFkoQAAQ2VKigWRMOARUeVADoSQiAAg8OePYMAEAp0Z6BAAA7
}

image create photo ::img::plot -format GIF -data {
R0lGODlhFgAWAMIHAAAAAAAAMgAAfwAPAAAZAAAnAAE8AP///yH5BAEKAAcALAAAAAAWABYAAANb
eLrcriDKSSl8eAEVct4H6DkCVI6McQLCORpGeLVvLN80k8OaMiy5ltDWOxB0woWqYRMpXIdYTlG4
PHhPF8ipgBGzVpAXSwqPRysr6pC+rYXbtS4uL1bu9zomAQA7
}

