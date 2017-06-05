#
# RPM spec file for mrxvt-0.4.0
#
# Copyright  (c)  2004-2005  Jingyu Zhou <jzhou@cs.ucsb.edu>
# Copyright  (c)  2005       Jingmin Zhou <jimmyzhou@users.sourceforge.net>
#

Name:      mrxvt
Summary:   A lightweight multi-tabbed X terminal
#Requires:  qt >= 2.3.0
Version:   0.4.1
Release:   2
License:   GPL
Vendor:    The mrxvt team <http://materm.sourceforge.net/>
Packager:  Jingyu Zhou <jzhou@cs.ucsb.edu>
Group:     User Interface/X
Source0:   /home/jzhou/tmp/t/mrxvt-%version.tar.gz
BuildRoot: %_tmppath/%name-%version-%release-root

%description
Mrxvt (previously named as materm) is a lightweight and powerful
multi-tabbed X terminal emulator based on the popular rxvt and aterm.
It implements many useful features seen in some modern X terminal
emulators, like gnome-terminal and konsole, but keep to be lightweight
and independent from the GNOME and KDE desktop environment.

%prep
%setup -q

%build
./configure --prefix=/usr --enable-everything --with-save-lines=600 --with-encoding=noenc --disable-ourstrings --disable-debug
make

%install
#make install
mkdir -p %buildroot/share/doc
mkdir -p %buildroot/bin
make DESTDIR=%buildroot install

%clean
rm -rf %buildroot

%post

%postun

%files

%dir /usr/share/doc/mrxvt
#/usr/share/doc/mrxvt

/usr/bin/mrxvt
/usr/man/man1/mrxvt.1.gz
/usr/share/doc/mrxvt/README.greek
/usr/share/doc/mrxvt/README.menu
/usr/share/doc/mrxvt/README.xvt
/usr/share/doc/mrxvt/TIPS
/usr/share/doc/mrxvt/mrxvt.vbs
/usr/share/doc/mrxvt/xdefaults-sample.txt
/usr/share/doc/mrxvt/xterm.seq
/usr/share/doc/mrxvt/mrxvtset.pl
/usr/share/pixmaps/mrxvt-csh.png
/usr/share/pixmaps/mrxvt-csh.xpm
/usr/share/pixmaps/mrxvt-root.png
/usr/share/pixmaps/mrxvt-root.xpm
/usr/share/pixmaps/mrxvt.png
/usr/share/pixmaps/mrxvt.xpm


%changelog
* Sun Feb 27 2005 Jingmin Zhou <jimmyzhou@users.sourceforge.net>
- Update configure script

* Sat Feb 26 2005 Jingyu Zhou <jzhou@cs.ucsb.edu>
- Version 0.4.0 update

* Sun Dec 12 2004 Jingyu Zhou <jzhou@cs.ucsb.edu>
- Version 0.3.11 update

* Mon Dec 06 2004 Jingyu Zhou <jzhou@cs.ucsb.edu>
- Version 0.3.10 update

* Tue Nov 23 2004 Jingyu Zhou <jzhou@cs.ucsb.edu>
- created this spec file
