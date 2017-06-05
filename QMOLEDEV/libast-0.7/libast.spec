%define __os_install_post /usr/lib/rpm/brp-compress
%if %{?optflags:1}0
%define optflags ${RPM_OPT_FLAGS:--O0 -g3}
%endif

Summary: Library of Assorted Spiffy Things
Name: libast
Version: 0.7
Release: 1
#Release: 0.%(date '+%Y%m%d')
Group: System Environment/Libraries
License: BSD
URL: http://www.eterm.org/
Source: %{name}-%{version}.tar.gz
#BuildSuggests: pcre-devel xorg-x11-devel imlib2-devel
BuildRoot: %{_tmppath}/%{name}-%{version}-root

%description
LibAST is the Library of Assorted Spiffy Things.  It contains various
handy routines and drop-in substitutes for some good-but-non-portable
functions.  It currently has a built-in memory tracking subsystem as
well as some debugging aids and other similar tools.

It's not documented yet, mostly because it's not finished.  Hence the
version number that begins with 0.

%prep
%setup -q

%build
CFLAGS="%{optflags}"
export CFLAGS

%{configure} --prefix=%{_prefix} --bindir=%{_bindir} --libdir=%{_libdir} \
             --includedir=%{_includedir} --datadir=%{_datadir} %{?acflags}
%{__make} %{?_smp_mflags} %{?mflags}

%install
%{__make} install DESTDIR=$RPM_BUILD_ROOT %{?mflags_install}

%post
test -x /sbin/ldconfig && /sbin/ldconfig || :

%postun
test -x /sbin/ldconfig && /sbin/ldconfig || :

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-, root, root)
%doc ChangeLog DESIGN README
%{_bindir}/*
%{_libdir}/*
%{_includedir}/*
%{_datadir}/*

%changelog
