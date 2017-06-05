#!/bin/bash
exec java -Dawt.toolkit=gnu.java.awt.peer.gtk.GtkToolkit -Xbootclasspath/p:/usr/share/jamvm/classes.zip:/usr/share/classpath/glibj.zip -cp ./ HelloWorld

