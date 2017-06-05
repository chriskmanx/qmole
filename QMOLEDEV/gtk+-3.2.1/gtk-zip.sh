#!/bin/sh

# Build zipfiles for GTK on Win32

ZIP=/tmp/gtk+-3.2.1.zip
DEVZIP=/tmp/gtk+-dev-3.2.1.zip

SHORTAPIVERSION=`echo 3.0 | tr -d '.'`
APIVERSIONMAJOR=`echo 3.0 | sed -e 's/\..*//'`

cd /usr/local

mkdir -p share/doc/gtk+-3.2.1
cp -p /home/chris/gtk+-3.2.1/COPYING share/doc/gtk+-3.2.1

mkdir -p share/doc/gtk+-dev-3.2.1
cp -p /home/chris/gtk+-3.2.1/COPYING share/doc/gtk+-dev-3.2.1

rm $ZIP
zip $ZIP -@ <<EOF
etc/gtk-3.0/gtkrc
etc/gtk-3.0/im-multipress.conf
bin/libgdk-win32-3.0-0.dll
bin/libgtk-win32-3.0-0.dll
bin/libgailutil-3.0-0.dll
bin/gtk-query-immodules-3.0.exe
lib/gtk-3.0/3.0.0/loaders.cache
lib/gtk-3.0/3.0.0/immodules.cache
EOF

zip $ZIP lib/gtk-3.0/modules/libgail.dll lib/gtk-3.0/3.0.0/loaders/*.dll lib/gtk-3.0/3.0.0/immodules/*.dll lib/gtk-3.0/3.0.0/engines/*.dll

zip $ZIP share/themes/Raleigh/gtk-3.0/gtkrc
zip $ZIP share/themes/Default/gtk-3.0-key/gtkrc
zip $ZIP share/themes/Emacs/gtk-3.0-key/gtkrc
zip $ZIP share/themes/MS-Windows/gtk-3.0/gtkrc

zip $ZIP share/locale/*/LC_MESSAGES/gtk${SHORTAPIVERSION}.mo
zip $ZIP share/locale/*/LC_MESSAGES/gtk${SHORTAPIVERSION}-properties.mo

zip -r -D $ZIP share/doc/gtk+-3.2.1

rm $DEVZIP
zip -r -D $DEVZIP -@ <<EOF
include/gtk-3.0
include/gail-1.0
bin/gtk-builder-convert
bin/gtk${APIVERSIONMAJOR}-demo.exe
bin/gtk-update-icon-cache.exe
share/man
lib/libgdk-win32-3.0.dll.a
lib/gdk-win32-3.0.lib
lib/libgtk-win32-3.0.dll.a
lib/gtk-win32-3.0.lib
lib/libgailutil.dll.a
lib/gailutil.lib
lib/gtk-3.0/include
lib/pkgconfig
share/aclocal
share/gtk-3.0
share/gtk-doc
EOF

zip -r $DEVZIP share/doc/gtk+-dev-3.2.1
