#
# $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/uil/Makefile.am,v 1.14 2004/08/28 19:23:37 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

# Only in 1.2
uil_12private_h =

# In other versions above as well
uil_12base_h = \
	Uil.h UilDBDef.h UilDef.h UilSymDef.h UilSymGl.h \
        XmAppl.uil

# Only in 2.0
uil_20private_h =

# In other versions above as well
uil_20base_h =

# Only in 2.1
uil_21private_h =

# There's no 2.2 yet :-)
uil_21base_h =

#
# Install headers
#
# Lets put the headers in a sub directory to avoid
# poluting /usr/local with more directories for default installs.
#
uildir = $(includedir)/uil

uil_HEADERS= $(uil_12base_h) $(uil_20base_h) $(uil_21base_h) $(uil_21private_h)
