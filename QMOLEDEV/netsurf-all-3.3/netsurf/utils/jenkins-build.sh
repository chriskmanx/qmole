#!/bin/bash
#
# Copyright © 2013 Vincent Sanders <vince@netsurf-browser.org>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   * The above copyright notice and this permission notice shall be included in
#     all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# NetSurf continuous integration build script for jenkins
#
# This script is executed by jenkins to build netsurf itself
#
# Usage: jenkins-build.sh
#

# TARGET is set to the frontend target to build
# HOST is set to the identifier of the toolchain doing the building
# CC is the compiler (gcc or clang)
# BUILD_JS is the javascript type (json or jsoff)
# BUILD_NUMBER is the CI build number

#####

# set defaults - this is not retrivable from the jenkins environment
OLD_ARTIFACT_COUNT=25

################# Parameter and environment setup #####################

#identifier for this specific build
IDENTIFIER="$CC-${BUILD_JS}-${BUILD_NUMBER}"

# Identifier for build which will be cleaned
OLD_IDENTIFIER="$CC-${BUILD_JS}-$((BUILD_NUMBER - ${OLD_ARTIFACT_COUNT}))"

# default atari architecture - bletch
ATARIARCH=68020-60

# make tool
MAKE=make

# Ensure the combination of target and toolchain works and set build
#   specific parameters too
case ${TARGET} in
    "riscos")
	case ${HOST} in
	    "arm-unknown-riscos")
		ARTIFACT_TARGET=riscos
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST})\""
		exit 1
		;;

	esac

	PKG_SRC=netsurf
	PKG_SFX=.zip
	;;

    "haiku")
	case ${HOST} in
	    "i586-pc-haiku")
		ARTIFACT_TARGET=Haiku
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST})\""
		exit 1
		;;

	esac

	PKG_SRC=NetSurf
	PKG_SFX=
	;;


    "windows")
	case ${HOST} in
	    "i686-w64-mingw32")
		ARTIFACT_TARGET=windows
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST})\""
		exit 1
		;;

	esac

	PKG_SRC=netsurf-installer
	PKG_SFX=.exe
	;;


    "cocoa")
	case ${HOST} in
	    "i686-apple-darwin10")
		ARTIFACT_TARGET=Darwin
		IDENTIFIER="${HOST}-${IDENTIFIER}"
		OLD_IDENTIFIER="${HOST}-${OLD_IDENTIFIER}"
		;;

	    "powerpc-apple-darwin9")
		ARTIFACT_TARGET=powerpc-apple-darwin9
		IDENTIFIER="${ARTIFACT_TARGET}-${IDENTIFIER}"
		OLD_IDENTIFIER="${ARTIFACT_TARGET}-${OLD_IDENTIFIER}"
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST})\""
		exit 1
		;;

	esac

	PKG_SRC=NetSurf
	PKG_SFX=.dmg
	;;


    "amiga")
	case ${HOST} in
	    "ppc-amigaos")
		ARTIFACT_TARGET=amiga
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST})\""
		exit 1
		;;

	esac

	PKG_SRC=NetSurf_Amiga/netsurf
	PKG_SFX=.lha
	;;


    "atari")
	case ${HOST} in
	    "m68k-atari-mint")
		ARTIFACT_TARGET=m68k-atari-mint
		PKG_SRC=ns020
		PKG_SFX=.zip
		;;

	    "m5475-atari-mint")
		ARTIFACT_TARGET=m5475-atari-mint
		export GCCSDK_INSTALL_ENV=/opt/netsurf/m5475-atari-mint/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/m5475-atari-mint/cross/bin
		ATARIARCH=v4e
		PKG_SRC=nsv4e
		PKG_SFX=.zip
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST})\""
		exit 1
		;;

	esac

	IDENTIFIER="${ARTIFACT_TARGET}-${IDENTIFIER}"
	OLD_IDENTIFIER="${ARTIFACT_TARGET}-${OLD_IDENTIFIER}"
	;;


    "gtk")
	case ${HOST} in
	    "x86_64-linux-gnu")
		ARTIFACT_TARGET=Linux
		;;

	    amd64-unknown-openbsd*)
		ARTIFACT_TARGET=OpenBSD
		MAKE=gmake
		;;

	    x86_64-unknown-freebsd*)
		ARTIFACT_TARGET=FreeBSD
		MAKE=gmake
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST}\""
		exit 1
		;;

	esac

	PKG_SRC=nsgtk
	PKG_SFX=
	;;


    "framebuffer")
	case ${HOST} in
	    "x86_64-linux-gnu")
		ARTIFACT_TARGET=Linux
		;;

	    "i686-apple-darwin10")
		ARTIFACT_TARGET=Darwin
		;;

	    "powerpc-apple-darwin9")
		ARTIFACT_TARGET=powerpc-apple-darwin9
		;;

	    amd64-unknown-openbsd*)
		ARTIFACT_TARGET=OpenBSD
		MAKE=gmake
		;;

	    x86_64-unknown-freebsd*)
		ARTIFACT_TARGET=FreeBSD
		MAKE=gmake
		;;

	    "arm-unknown-riscos")
		ARTIFACT_TARGET=riscos
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    "m68k-atari-mint")
		ARTIFACT_TARGET=m68k-atari-mint
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    "m5475-atari-mint")
		ATARIARCH=v4e
		ARTIFACT_TARGET=m5475-atari-mint
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    "i686-w64-mingw32")
		ARTIFACT_TARGET=windows
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    "ppc-amigaos")
		ARTIFACT_TARGET=amiga
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST})\""
		exit 1
		;;

	esac

	PKG_SRC=nsfb
	PKG_SFX=
	;;


    "monkey")
	# monkey target can be built on most of the supported architectures
	case ${HOST} in
	    "x86_64-linux-gnu")
		ARTIFACT_TARGET=Linux
		;;

	    "i686-apple-darwin10")
		ARTIFACT_TARGET=Darwin
		;;

	    "powerpc-apple-darwin9")
		ARTIFACT_TARGET=powerpc-apple-darwin9
		;;

	    amd64-unknown-openbsd*)
		ARTIFACT_TARGET=OpenBSD
		MAKE=gmake
		;;

	    x86_64-unknown-freebsd*)
		ARTIFACT_TARGET=FreeBSD
		MAKE=gmake
		;;

	    "arm-unknown-riscos")
		ARTIFACT_TARGET=riscos
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    "m68k-atari-mint")
		ARTIFACT_TARGET=m68k-atari-mint
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    "m5475-atari-mint")
		ATARIARCH=v4e
		ARTIFACT_TARGET=m5475-atari-mint
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    "i686-w64-mingw32")
		ARTIFACT_TARGET=windows
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    "ppc-amigaos")
		ARTIFACT_TARGET=amiga
		export GCCSDK_INSTALL_ENV=/opt/netsurf/${HOST}/env
		export GCCSDK_INSTALL_CROSSBIN=/opt/netsurf/${HOST}/cross/bin
		;;

	    *)
		echo "Target \"${TARGET}\" cannot be built on \"${HOST})\""
		exit 1
		;;

	esac

	IDENTIFIER="${HOST}-${IDENTIFIER}"
	OLD_IDENTIFIER="${HOST}-${OLD_IDENTIFIER}"
	PKG_SRC=nsmonkey
	PKG_SFX=
	;;

    *)
	# TARGET must be in the environment and set correctly
	echo "Unkown TARGET \"${TARGET}\""
	exit 1
	;;

esac

# setup environment
export PREFIX=${JENKINS_HOME}/artifacts-${HOST}
export PKG_CONFIG_PATH=${PREFIX}/lib/pkgconfig
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${PREFIX}/lib
export PATH=${PATH}:${PREFIX}/bin

# configure ccache for clang
if [ "${CC}" = "clang" ];then
    export CCACHE_CPP2=yes
    export CC="clang -Qunused-arguments"
fi

# convert javascript parameters
if [ "${BUILD_JS}" = "json" ];then
    case ${HOST} in
        "arm-unknown-riscos")
	    BUILD_MOZJS=NO
	    BUILD_JS=YES
	    ;;

        "amd64-unknown-openbsd5.4")
	    BUILD_MOZJS=NO
	    BUILD_JS=YES
            ;;

	*)
	    BUILD_MOZJS=YES
	    BUILD_JS=NO
	;;

    esac

else
    BUILD_JS=NO
    BUILD_MOZJS=NO
fi




########### Build from source ##################

# Clean first
${MAKE} NETSURF_USE_JS=${BUILD_JS} NETSURF_USE_MOZJS=${BUILD_MOZJS} clean

# Do the Build
${MAKE} -k NETSURF_USE_JS=${BUILD_JS} NETSURF_USE_MOZJS=${BUILD_MOZJS} CI_BUILD=${BUILD_NUMBER} ATARIARCH=${ATARIARCH} Q=




############ Package artifact construction ################

# build the package file
${MAKE} -k NETSURF_USE_JS=${BUILD_JS} NETSURF_USE_MOZJS=${BUILD_MOZJS} CI_BUILD=${BUILD_NUMBER} ATARIARCH=${ATARIARCH} package Q=

if [ ! -f "${PKG_SRC}${PKG_SFX}" ]; then
    # unable to find package file
    exit 1
fi



############ Package artifact deployment ################

#destination for package artifacts
DESTDIR=/srv/ci.netsurf-browser.org/html/builds/${TARGET}/

NEW_ARTIFACT_TARGET="NetSurf-${IDENTIFIER}${PKG_SFX}"

# copy the file into the output - always use scp as it works local or remote
scp "${PKG_SRC}${PKG_SFX}" netsurf@ci.netsurf-browser.org:${DESTDIR}/${NEW_ARTIFACT_TARGET}

# remove the local package file artifact
rm -f "${PKG_SRC}${PKG_SFX}"

# setup latest link
ssh netsurf@ci.netsurf-browser.org "rm -f ${DESTDIR}/LATEST && echo "${NEW_ARTIFACT_TARGET}" > ${DESTDIR}/LATEST"



############ Package artifact cleanup ################

OLD_ARTIFACT_TARGET="NetSurf-${OLD_IDENTIFIER}${PKG_SFX}"

ssh netsurf@ci.netsurf-browser.org "rm -f ${DESTDIR}/${OLD_ARTIFACT_TARGET}"
