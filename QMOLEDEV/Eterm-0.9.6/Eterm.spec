%if %{!?compression:1}0
%define compression gz
%endif

%define __os_install_post /usr/lib/rpm/brp-compress
%if %{?optflags:1}0
%define optflags ${RPM_OPT_FLAGS:--O0 -g3}
%endif

Summary: Enlightened terminal emulator
Name: Eterm
Version: 0.9.6
Release: 1
#Release: 0.%(date '+%Y%m%d')
License: BSD
Group: User Interface/X
URL: http://www.eterm.org/
Source0: http://www.eterm.org/download/%{name}-%{version}.tar.%{compression}
Source1: http://www.eterm.org/download/%{name}-bg-%{version}.tar.%{compression}
#BuildSuggests: xorg-x11-devel, XFree86-devel, xorg-x11-proto-devel, libXext-devel, libXt-devel, freetype-devel
BuildRequires: libast, imlib2-devel
Requires: imlib2, imlib2-loader_jpeg, imlib2-loader_png
BuildRoot: %{?_tmppath}%{!?_tmppath:/var/tmp}/%{name}-%{version}-%{release}-root

%description
Eterm is a color vt102 terminal emulator with enhanced graphical
capabilities.  Eterm is intended to be a replacement for xterm for
Enlightenment window manager users, but it can also be used as a
replacement for xterm by users without Enlightenment.  Eterm supports
various themes and is very configurable, in keeping with the
philosophy of Enlightenment. If you install Eterm, you'll also need to
have the Imlib2 library installed.

%prep
%setup -a 1

%build
CFLAGS="%{optflags}"
export CFLAGS

# When using the configure macro, I also specify all the directory
# macros I use for compatibility with older versions of the macro
%configure --bindir=%{_bindir} --libdir=%{_libdir} --mandir=%{_mandir} \
           --datadir=%{_datadir} --sysconfdir=%{_sysconfdir} \
           --enable-multi-charset --enable-escreen --enable-auto-encoding \
           --with-debugging=9 %{?acflags}
%{__make} %{?mflags}

%install
%{__rm} -rf $RPM_BUILD_ROOT

# If the configure macro is used above (which it is), there
# is NO reason to use the makeinstall macro here, so don't.
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

( cd $RPM_BUILD_ROOT
  %{__mv} .%{_bindir}/%{name} .%{_bindir}/%{name}-%{version}
  cd $RPM_BUILD_ROOT%{_bindir}
  %{__ln_s} -f %{name}-%{version} %{name}
  cd $RPM_BUILD_ROOT
  chmod +x .%{_libdir}/lib*so* ||:
)

%{__mkdir_p} $RPM_BUILD_ROOT%{_sysconfdir}/X11/applnk/Utilities
cat > $RPM_BUILD_ROOT%{_sysconfdir}/X11/applnk/Utilities/Eterm.desktop <<EOF
[Desktop Entry]
Name=Eterm
Comment=Eterm
TryExec=Eterm
Exec=Eterm
Icon=gnome-eterm.png
Terminal=0
Type=Application
EOF
chmod 0644 $RPM_BUILD_ROOT%{_sysconfdir}/X11/applnk/Utilities/Eterm.desktop

%post
/sbin/ldconfig || :

if [ -d /usr/share/terminfo -a ! -f /usr/share/terminfo/E/Eterm ]; then
    tic -o/usr/share/terminfo %{_docdir}/%{name}-%{version}/%{name}.ti || :
fi

%postun
/sbin/ldconfig || :

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
%doc doc/Eterm_reference.html doc/Eterm.1.html doc/Eterm.tcap doc/Eterm.ti doc/README.Escreen
%doc LICENSE README ReleaseNotes ReleaseNotes.1 ChangeLog
%config %{_sysconfdir}/X11/applnk/Utilities/Eterm.desktop
%{_bindir}/*
%{_libdir}/*
%{_mandir}/man1/*
%{_datadir}/%{name}/*

%changelog
* Tue Feb 20 2001 Tim Powers <timp@redhat.com>
- builds on Alpha now. No need to excludearch (bug 28472)

* Wed Feb 14 2001 Tim Powers <timp@redhat.com>
- removed images which are from Digital Blasphemy from the extra
  images tarball, had to delete those images because they are not
  allowed to be redistributed on CD, or in compressed format

* Tue Jan  9 2001 Tim Powers <timp@redhat.com>
- fixed a bunch of pre and post in/uninstall brokenness by dropping
  split backgrounds package, backgrounds now included in Eterm
  proper. This also makes the spec file a lot cleaner :)

* Sat Aug 19 2000 Tim Powers <timp@redhat.com>
- fix bug #15687

* Tue Aug 8 2000 Tim Powers <timp@redhat.com>
- fixed bug #15687 using Hans' patch

* Wed Aug 2 2000 Tim Powers <timp@redhat.com>
- rebuilt against libpng-1.0.8

* Mon Jul 24 2000 Prospector <prospector@redhat.com>
- rebuilt

* Sat Jul 22 2000 Tim Powers <timp@redhat.com>
- fix spec file problem with configure picking up egcs and running with it,
  fixes linking bugs

* Mon Jul 10 2000 Tim Powers <timp@redhat.com>
- rebuilt

* Fri Jun 2 2000 Tim Powers <timp@redhat.com>
- no more wmconfig :) converted to applnk
- fix man page location. Now FHS compliant
- use macros wherever possible
- removed redundant defines at the top of the spec

* Mon Apr 10 2000 Tim Powers <timp@redhat.com>
- rebuilt for 7.0

* Thu Feb 03 2000 Tim Powers <timp@redhat.com>
- strip debug from libraries

* Wed Feb 02 2000 Tim Powers <timp@redhat.com>
- fixed problems when upgrading and error messages due to a faulty script in
        the post section for the Eterm package
        
* Tue Feb 01 2000 Tim Powers <timp@redhat.com>
- applied patch from Hans de Goede <hans@highrise.nl> to fix some del, home
        and end issues
        
* Fri Jan 29 2000 Tim Powers <timp@redhat.com>
- rebuilt for 6.2 powertools
- bzipped source to conserve space
- using percent configure so that libtoolize is run, needed for some of the
	newer alphas instead of ./configure --prefix=
- stripping binaries again

* Fri Dec 10 1999 Michael Jennings <mej@eterm.org>
- Added Tim's spec file to CVS as Eterm.spec.in for 0.9

* Wed Dec 8 1999 Tim Powers <timp@redhat.com>
- using unified patch for utempter and themes from Michael Jennings

* Tue Dec 7 1999 Tim Powers <timp@redhat.com>
- added wmconfig entry
- split up into 2 packages, Eterm proper, and Eterm-backgrounds
- thanks to ewt, we no longer have to make Eterm suid root, uses utempter
        instead

* Mon Dec 6 1999 Tim Powers <timp@redhat.com>
- updated to 0.8.10
- patched so that Eterm finds pix/themes in the right place
- new version fixes problems with utmp, conforms to Eterm docs.
- added RedHat.Eterm_suid which includes instructions on how to run Eterm in
        order to have it seen by "w" and "who" as a regular user

* Fri Aug 20 1999 Tim Powers <timp@redhat.com>
- fixed roblem with removing all files when uninstalling Eterm.

* Tue Jul 27 1999 Tim Powers <timp@redhat.com>
- updated version to 0.8.9
- cleaned up spec
- updated patch
- includes new backgrounds
- built for 6.1

* Mon Apr 05 1999 Michael Maher <mike@redhat.com>
- update to 0.8.8

* Fri Oct 23 1998 Jeff Johnson <jbj@redhat.com>
- update to 0.8.7.

* Fri Oct 08 1998 Michael Maher <mike@redhat.com>
- built eterm
