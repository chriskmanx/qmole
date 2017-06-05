# Set VERSION so we ongit ly need to edit it one place (ie. here)
AC_INIT(TypeRex, 1.0.1, contact@ocamlpro.com)

AC_COPYRIGHT(Copyright 2011 OcamlPro SAS)

# Checking for OCaml executables, version and stdlib path
AC_PROG_OCAML
AC_PROG_CAMLP4

# Make sure that OCaml is installed
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_ARG_ENABLE(version-check,
 [  --disable-version-check            do not check OCaml version],
 [VERSION_CHECK="$enableval"],
 [VERSION_CHECK="yes"])

# Check that OCaml version is 3.11.2 or 3.12.1
if test "$VERSION_CHECK" = "yes" &&
   test "$OCAMLVERSION" != "3.11.2" &&
   test "$OCAMLVERSION" != "3.12.0" &&
   test "$OCAMLVERSION" != "3.12.1" ; then
  AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported])
fi

# Checking for existing ocp-type and ocp-wrapper
AC_CHECK_TOOLS(OCP_TYPE, ocp-type, `pwd`/_obuild/ocp-type/ocp-type.asm)
AC_CHECK_TOOLS(OCP_WRAPPER, ocp-wrapper, `pwd`/_obuild/ocp-wrapper/ocp-wrapper.asm)
# We should check that they actually exist, or provide them for bootstrap.

# Check whether to compile with ocp-type
AC_MSG_CHECKING([whether to apply ocp-type])
AC_ARG_ENABLE(cmt,
 [  --enable-cmt            generate typedtrees for TypeRex with ocp-type],
 [AC_MSG_RESULT([yes])
# if test "$OCP_TYPE" = "no" || test  "$OCP_WRAPPER" = "no" ; then
#   AC_MSG_ERROR([Cannot generate cmt: ocp-type or ocp-wrapper is missing])
# fi
 OCAMLCW="$OCP_WRAPPER -with-ocp-type $OCP_TYPE -save-types $OCAMLC -annot"
 ],
 [OCAMLCW="$OCAMLC"
 AC_MSG_RESULT([no])])
AC_SUBST(OCAMLCW)

# Not needed
## Checking for ocamlrun (for ocp-build bootstrap)
#AC_CHECK_TOOL([OCAMLRUN],[ocamlrun],[no])
#if test "$OCAMLRUN" = "no"; then
#  AC_MSG_ERROR([You must install the OCaml bytecode interpreter])
#fi

# Checking for ocp-build
AC_ARG_WITH([ocp-build],
 [  --with-ocp-build        override the default ocp-build command],
 [ OCP_BUILD="$withval"
   AC_MSG_CHECKING([ocp-build command])
   AC_MSG_RESULT([$OCP_BUILD])],
 [
 AC_CHECK_TOOL([OCP_BUILD],[ocp-build],[`pwd`/_obuild/unixrun `pwd`/boot/ocp-build.boot])
 ])
# Default uses the absolute path to ocp-build.boot, to be usable from
# any directory.

# Checking for ocamllex and ocamlyacc
AC_PROG_OCAMLLEX
AC_PROG_OCAMLYACC

# Checking for emacs and site-lisp
AM_PATH_LISPDIR
# Make sure that Emacs is installed
if test "$EMACS" = "no"; then
  AC_MSG_ERROR([You must install Emacs to run TypeRex])
fi

# Stolen from:
# http://linuxha-net.googlecode.com/svn-history/r12/trunk/drbd-8.3.9/configure.ac
if test "$prefix" = "NONE"; then
  prefix=$ac_default_prefix
fi
exec_prefix=$prefix

prefix="`eval echo ${prefix}`"
exec_prefix="`eval echo ${exec_prefix}`"
bindir="`eval echo ${bindir}`"
sbindir="`eval echo ${sbindir}`"
libexecdir="`eval echo ${libexecdir}`"
datarootdir="`eval echo ${datarootdir}`"
datadir="`eval echo ${datadir}`"
sysconfdir="`eval echo ${sysconfdir}`"
sharedstatedir="`eval echo ${sharedstatedir}`"
localstatedir="`eval echo ${localstatedir}`"
libdir="`eval echo ${libdir}`"
includedir="`eval echo ${includedir}`"
oldincludedir="`eval echo ${oldincludedir}`"
infodir="`eval echo ${infodir}`"
mandir="`eval echo ${mandir}`"
docdir="`eval echo ${docdir}`"
lispdir="`eval echo ${lispdir}`"

## Checking for zlib
#AX_CHECK_ZLIB

# Install auto-complete unless already present, or disabled by user
#AC_MSG_NOTICE([checking whether to install auto-complete...])
AC_ARG_ENABLE(auto-complete,
 [  --disable-auto-complete            do not install Emacs auto-complete],
 [AC_MSG_RESULT([auto-complete will NOT be installed])],
 [AC_CHECK_FILE(${lispdir}/auto-complete-mode/auto-complete.el,
    [AC_MSG_RESULT([auto-complete will NOT be installed])],
    [INSTALL_AUTO_COMPLETE=1]
     AC_MSG_RESULT([auto-complete will be installed]))])
AC_SUBST(INSTALL_AUTO_COMPLETE)

# Files to substitutes
AC_CONFIG_FILES(
  Makefile.config
  emacs.append
  libs/ocamlpro/typerex-config/typerex_config.ml
  tools/ocp-typerex-ide/emacs/typerex-config.el
  libs/ocamlpro/system/ocplib-system.ocp
)
#  ocaml.conf

# Perform the substitution
AC_OUTPUT

echo
echo Executables will be installed in ${bindir}
echo Emacs files will be installed in ${lispdir}
if test "$INSTALL_AUTO_COMPLETE" = 1; then
  echo auto-complete will be installed
else
  echo auto-complete will NOT be installed
fi
