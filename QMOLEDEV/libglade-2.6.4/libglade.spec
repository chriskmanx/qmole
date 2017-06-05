Summary: libglade library
Name: libglade
Version: 2.6.4
Release: 1
Copyright: LGPL
Group: System Environment/Libraries
Source: http://ftp.gnome.org/pub/GNOME/sources/libglade/2.4/libglade-%{version}.tar.gz
BuildRoot: /var/tmp/%{name}-%{version}-root
URL: http://www.gnome.org

Requires: gtk+ >= 2.4.0
Requires: libxml2 >= 2.4.10

%description
This library allows you to load user interfaces in your program, which are
stored externally.  This allows alteration of the interface without
recompilation of the program.

The interfaces can also be edited with GLADE.

%package devel
Summary: Libraries, includes, etc to develop libglade applications
Group: Development/Libraries
Requires: libglade gtk+-devel libxml2-devel

%description devel
Libraries, include files, etc you can use to develop libglade applications.


%changelog

* Sun Nov  1 1998 James Henstridge <james@daa.com.au>

- Updated the dependencies of the devel package, so users must have gtk+-devel.

* Sun Oct 25 1998 James Henstridge <james@daa.com.au>

- Initial release 0.0.1

%prep
%setup

%build
%configure
make

%install
%makeinstall

%post
/sbin/ldconfig

CATALOG=/etc/xml/catalog
/usr/bin/xmlcatalog --noout --add "system" \
    "http://glade.gnome.org/glade-2.0.dtd" \
    %{_prefix}/share/xml/libglade/glade-2.0.dtd $CATALOG || true

%postun
/sbin/ldconfig

CATALOG=/etc/xml/catalog
/usr/bin/xmlcatalog --noout --del \
    %{_prefix}/share/xml/libglade/glade-2.0.dtd $CATALOG || true

%files
%defattr(-, root, root)

%doc AUTHORS ChangeLog NEWS README COPYING
%{_prefix}/lib/lib*.so.*
%{_prefix}/share/xml/libglade/glade-2.0.dtd

%files devel
%defattr(-, root, root)

%{_prefix}/bin/*
%{_prefix}/lib/lib*.so
%{_prefix}/lib/*a
%{_prefix}/include/libglade-2.0/glade/*
%{_prefix}/lib/pkgconfig/libglade-2.0.pc

%doc test-libglade.c
%doc examples/*.glade
%doc %{_prefix}/share/gtk-doc/html/libglade/*
