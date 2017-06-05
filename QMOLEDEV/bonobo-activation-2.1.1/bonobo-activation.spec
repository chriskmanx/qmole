# Note that this is NOT a relocatable package
%define name		bonobo-activation
%define ver		2.1.1
%define RELEASE		1
%define rel		%{?CUSTOM_RELEASE} %{!?CUSTOM_RELEASE:%RELEASE}
%define prefix		/usr
%define sysconfdir	/etc

Name:		%name
Summary:	Bonobo object activation framework for GNOME
Version: 	%ver
Release: 	%rel
License: 	LGPL and GPL
Group:		System Environment/Libraries
Source: 	%{name}-%{ver}.tar.gz
URL: 		http://www.gnome.org/
BuildRoot:	/var/tmp/%{name}-%{ver}-root
Docdir: 	%{prefix}/share/doc

%description
Bonobo Activation is an object activation framework for GNOME. It uses ORBit.

%package devel
Summary:	Libraries and include files for Bonobo Activation
Group:		Development/Libraries
Requires:	%name = %{PACKAGE_VERSION}
Obsoletes:	%{name}-devel

%description devel

%changelog
* Sat Dec 15 2001 Ross Golder <ross@golder.org>
- updated docdir to %prefix/share/doc
* Sun Nov 18 2001 Ross Golder <ross@golder.org>
- updated spec file (changed oaf stuff to bonobo-activation)
* Tue Aug 29 2000 Maciej Stachowiak <mjs@eazel.com>
- corrected Copyright field and renamed it to License
* Sun May 21 2000 Ross Golder <rossigee@bigfoot.com>
- created spec file (based on bonobo.spec.in)

%prep
%setup

%build
%ifarch alpha
	MYARCH_FLAGS="--host=alpha-redhat-linux"
%endif

LC_ALL=""
LINGUAS=""
LANG=""
export LC_ALL LINGUAS LANG

CFLAGS="$RPM_OPT_FLAGS" ./configure $MYARCH_FLAGS \
	--enable-more-warnings \
	--prefix=%{prefix} \
	--sysconfdir=%{sysconfdir}

make -k

%install
[ -n "$RPM_BUILD_ROOT" -a "$RPM_BUILD_ROOT" != / ] && rm -rf $RPM_BUILD_ROOT
make -k prefix=$RPM_BUILD_ROOT%{prefix} sysconfdir=$RPM_BUILD_ROOT%{sysconfdir} install

for FILE in "$RPM_BUILD_ROOT/bin/*"; do
	file "$FILE" | grep -q not\ stripped && strip $FILE
done

%clean
[ -n "$RPM_BUILD_ROOT" -a "$RPM_BUILD_ROOT" != / ] && rm -rf $RPM_BUILD_ROOT

%post
if ! grep %{prefix}/lib /etc/ld.so.conf > /dev/null ; then
	echo "%{prefix}/lib" >> /etc/ld.so.conf
fi
  
/sbin/ldconfig
  
%postun -p /sbin/ldconfig

%files
%defattr(0555, bin, bin)

%doc AUTHORS COPYING ChangeLog NEWS README
%config %{sysconfdir}/%{name}/*.xml
%{prefix}/bin/*
%{prefix}/sbin/*
%{prefix}/lib/*.so.*
%{prefix}/lib/bonobo/servers/*.server

%defattr (0444, bin, bin)
%{prefix}/share/idl/bonobo-activation-2.0/*.idl
%{prefix}/share/locale/*/LC_MESSAGES/*.mo

%files devel

%defattr(0555, bin, bin)
%dir %{prefix}/include/bonobo-activation-2.0
%{prefix}/lib/*.so
%{prefix}/lib/*.la

%defattr(0444, bin, bin)
%{prefix}/lib/pkgconfig/bonobo-activation-2.0.pc
%{prefix}/include/bonobo-activation-2.0/bonobo-activation/*.h
