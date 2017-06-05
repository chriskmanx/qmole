AC_PREREQ(2.53)
AC_INIT([gsettings-desktop-schemas], [3.0.0],
        [http://bugzilla.gnome.org/enter_bug.cgi?product=gsettings-desktop-schemas])
AC_CONFIG_SRCDIR(schemas/org.gnome.desktop.default-applications.gschema.xml.in.in)
AM_INIT_AUTOMAKE([1.9 foreign no-dist-gzip dist-bzip2])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_CONFIG_MACRO_DIR([m4])

AM_MAINTAINER_MODE

AC_PROG_SED

GLIB_GSETTINGS

GETTEXT_PACKAGE=gsettings-desktop-schemas
AC_SUBST(GETTEXT_PACKAGE)
AM_GLIB_GNU_GETTEXT
IT_PROG_INTLTOOL([0.40.0])

AC_OUTPUT([
Makefile
gsettings-desktop-schemas.pc
headers/Makefile
schemas/Makefile
po/Makefile.in
])

dnl ---------------------------------------------------------------------------
dnl - Show summary
dnl ---------------------------------------------------------------------------

echo "
              gsettings-desktop-schemas $VERSION
              `echo gsettings-desktop-schemas $VERSION | sed "s/./=/g"`

        prefix:                             ${prefix}
        exec_prefix:                        ${exec_prefix}
        libdir:                             ${libdir}
        bindir:                             ${bindir}
        sbindir:                            ${sbindir}
        sysconfdir:                         ${sysconfdir}
        localstatedir:                      ${localstatedir}
        datadir:                            ${datadir}
        source code location:               ${srcdir}
        Maintainer mode:                    ${USE_MAINTAINER_MODE}
"
