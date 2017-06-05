# Atol RPM spec file

Name:           atol
Version:        0.7.3
Release:        1%{?dist}
Summary:        Dual-panel file manager.

Group:          Office/Productivity
License:        BSD
URL:            http://atol.sf.net/
Source:         atol-0.7.3_src.tar.gz
Icon:           atol.xpm
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
BuildRequires:  gtk2-devel openssl-devel unix2dos sed zip
Requires:       gtk2 openssl

%description
Atol is a dual-panel file manager.

%prep
%setup -n %{name}-%{version}

%build
%{__make}

%install
%{__rm} -rf "%{buildroot}"
%{makeinstall}
%find_lang %{name}

%clean
%{__rm} -rf "%{buildroot}"

%files -f %{name}.lang
%defattr(-, root, root)
%{_bindir}/atol
%{_datadir}/icons/atol.xpm
%{_datadir}/applications/atol.desktop
%{_prefix}/local/lib/atol/ArjLib.atp
%{_prefix}/local/lib/atol/Bz2Lib.atp
%{_prefix}/local/lib/atol/GzLib.atp
%{_prefix}/local/lib/atol/LstLib.atp
%{_prefix}/local/lib/atol/TarLib.atp
%{_prefix}/local/lib/atol/ZLib.atp
%{_prefix}/local/lib/atol/ZipLib.atp

%changelog
* Tue Aug 02 2005 Miroslav Rajcic <miroslav.rajcic@inet.hr>
- using %{?dist} macro instead of hardcoded distro code, moved Vendor field to .rpmmacros
* Mon Jun 06 2005 Miroslav Rajcic <miroslav.rajcic@inet.hr>
- using %find_lang macro instead of hardcoded list of .po files
* Sat Nov 13 2004 Miroslav Rajcic <miro@users.sourceforge.net>
- initial release
