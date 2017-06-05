#
# $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Mrm/Makefile.am,v 1.16 2004/08/28 19:23:24 dannybackx Exp $
#
MAINTAINERCLEANFILES=Makefile.in

# Only in 1.2
mrm_12private_h = 

# In other versions above as well
mrm_12base_h = \
        MrmAppl.h MrmDecls.h MrmPrivate.h MrmPublic.h

# Only in 2.0
mrm_20private_h =

# In other versions above as well
mrm_20base_h =

# Only in 2.1
mrm_21private_h =

# There's no 2.2 yet :-)
mrm_21base_h =

#
# Install headers
#
# Lets put the headers in a sub directory to avoid
# poluting /usr/local with more directories for default installs.
#
mrmdir = $(includedir)/Mrm

EXTRA_DIST = $(mrm_21base_h) $(mrm_21private_h)

mrm_HEADERS =	$(mrm_12base_h) $(mrm_20base_h) \
		$(mrm_21base_h) $(mrm_21private_h)
