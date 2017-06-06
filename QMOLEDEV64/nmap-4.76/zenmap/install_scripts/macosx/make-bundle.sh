#!/bin/sh -e

# make-bundle.sh
# David Fifield
#
# This script works the magic needed to build Zenmap into a .app bundle for Mac
# OS X. It's complicated because py2app doesn't really support Pango or PyGTK.
#
# It is based on the osx-app.sh script used by Wireshark, which contains the
# following notice:
#
# AUTHORS
#		 Kees Cook <kees@outflux.net>
#		 Michael Wybrow <mjwybrow@users.sourceforge.net>
#		 Jean-Olivier Irisson <jo.irisson@gmail.com>
#
# Copyright (C) 2005 Kees Cook
# Copyright (C) 2005-2007 Michael Wybrow
# Copyright (C) 2007 Jean-Olivier Irisson
#
# Released under GNU GPL, read the file 'COPYING' for more information

# This script relies on having an installation of MacPorts in $(LIBPREFIX),
# configured as you wish. You need to have installed the packages py25-gtk,
# py25-macholib-devel, py25-py2app-devel, py25-sqlite3, and py25-zlib.

LIBPREFIX=/opt/local-universal-10.4
PYTHON=$LIBPREFIX/bin/python2.5
PKG_CONFIG=$LIBPREFIX/bin/pkg-config
APP_NAME=Zenmap
BASE=dist/$APP_NAME.app/Contents
SCRIPT_DIR=`dirname "$0"`

CC=${CC:-gcc}
CFLAGS=${CFLAGS:--Wall}

echo "Running $0."

echo "Removing old build."
rm -rf build dist

echo "Compiling using py2app."
$PYTHON setup.py py2app --no-strip

mkdir -p $BASE/Resources/etc
mkdir -p $BASE/Resources/lib

gtk_version=`$PKG_CONFIG --variable=gtk_binary_version gtk+-2.0`
echo "Copying GTK+ $gtk_version files."
mkdir -p $BASE/Resources/lib/gtk-2.0/$gtk_version
cp -R $LIBPREFIX/lib/gtk-2.0/$gtk_version/* $BASE/Resources/lib/gtk-2.0/$gtk_version/

mkdir -p $BASE/Resources/etc/gtk-2.0
sed -e "s|$LIBPREFIX|\${RESOURCES}|g" $LIBPREFIX/etc/gtk-2.0/gdk-pixbuf.loaders >> $BASE/Resources/etc/gtk-2.0/gdk-pixbuf.loaders.in
sed -e "s|$LIBPREFIX|\${RESOURCES}|g" $LIBPREFIX/etc/gtk-2.0/gtk.immodules >> $BASE/Resources/etc/gtk-2.0/gtk.immodules.in

pango_version=`$PKG_CONFIG --variable=pango_module_version pango`
echo "Copying Pango $pango_version files."
mkdir -p $BASE/Resources/lib/pango/$pango_version/modules
cp $LIBPREFIX/lib/pango/$pango_version/modules/*.so $BASE/Resources/lib/pango/$pango_version/modules

mkdir -p $BASE/Resources/etc/pango
cat > $BASE/Resources/etc/pango/pangorc.in <<EOF
# This template is filled in at run time by the application.

[Pango]
ModuleFiles = \${ETC}/pango/pango.modules
[PangoX]
AliasFiles = \${RESOURCES}/etc/pango/pangox.aliases
EOF
cat > $BASE/Resources/etc/pango/pango.modules.in <<EOF
# This template is filled in at run time by the application.

EOF
sed -e "s|$LIBPREFIX|\${RESOURCES}|g" $LIBPREFIX/etc/pango/pango.modules >> $BASE/Resources/etc/pango/pango.modules.in
cp $LIBPREFIX/etc/pango/pangox.aliases $BASE/Resources/etc/pango/

echo "Copying Fontconfig files."
cp -R $LIBPREFIX/etc/fonts $BASE/Resources/etc/

echo "Renaming main Zenmap executable."
mv $BASE/MacOS/$APP_NAME $BASE/MacOS/zenmap.bin

echo "Installing wrapper script."
cp $SCRIPT_DIR/zenmap_wrapper.py $BASE/MacOS/

echo "Compiling and installing authorization wrapper."
echo $CC $CPPFLAGS $CFLAGS $LDFLAGS -framework Security -o $BASE/MacOS/$APP_NAME $SCRIPT_DIR/zenmap_auth.c
$CC $CPPFLAGS $CFLAGS $LDFLAGS -framework Security -o $BASE/MacOS/$APP_NAME $SCRIPT_DIR/zenmap_auth.c
