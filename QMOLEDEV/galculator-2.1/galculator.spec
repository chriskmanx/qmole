Summary: GTK 2 based scientific calculator
Name: galculator
Version: 2.1
Release: 1
License: GPL
Group: Application/Math
Source: http://prdownloads.sourceforge.net/galculator/galculator-2.1.tar.gz
Packager: Victor Soroka <gbs@tnss.kharkov.ua>
BuildRoot: /var/tmp/%{name}-%{version}-root
BuildRequires: gtk2-devel >= 3.0.0 

%description
galculator is a GTK 2 / GTK 3 based scientific calculator with "ordinary" and reverse polish notation.

%prep
%setup -q

%build
CFLAGS="$RPM_OPT_FLAGS" 
%configure

%install
rm -rf $RPM_BUILD_ROOT
make -k DESTDIR=$RPM_BUILD_ROOT prefix=%{_prefix} install
strip -s $RPM_BUILD_ROOT%{_bindir}/galculator

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-, root, root)
%doc %attr(0644, root, root) AUTHORS ChangeLog NEWS README COPYING TODO
%{_bindir}/galculator
%{_datadir}/applications/galculator.desktop
%{_datadir}/galculator/ui/*
%{_datadir}/icons/*
%{_datadir}/locale/*
%{_datadir}/pixmaps/*
%{_mandir}/man1/%{name}.1*

%changelog
* Sat Nov 15 2003 Simon Flöry <simon.floery@rechenraum.com>
- applied patch by Filippo

* Thu Oct 3 2002 Simon Flöry <simon.floery@rechenraum.com>
- description update
