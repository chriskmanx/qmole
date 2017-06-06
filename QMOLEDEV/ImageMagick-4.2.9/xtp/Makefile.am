#  Generic makefile for display, animate, montage, and import for
#  computers that do not have xmkmf.
#
#  Copyright 1998 E. I. du Pont de Nemours and Company
#
#  Permission is hereby granted, free of charge, to any person obtaining a
#  copy of this software and associated documentation files ("ImageMagick"),
#  to deal in ImageMagick without restriction, including without limitation 
#  the rights to use, copy, modify, merge, publish, distribute, sublicense, 
#  and/or sell copies of ImageMagick, and to permit persons to whom the 
#  ImageMagick is furnished to do so, subject to the following conditions:
#   
#  The above copyright notice and this permission notice shall be included in 
#  all copies or substantial portions of ImageMagick. 
#   
#  The software is provided "as is", without warranty of any kind, express or 
#  implied, including but not limited to the warranties of merchantability, 
#  fitness for a particular purpose and noninfringement.In no event shall 
#  E. I. du Pont de Nemours and Company be liable for any claim, damages or 
#  other liability, whether in an action of contract, tort or otherwise,
#  arising from, out of or in connection with ImageMagick or the use or other 
#  dealings in ImageMagick. 
#   
#  Except as contained in this notice, the name of the E. I. du Pont de 
#  Nemours and Company shall not be used in advertising or otherwise to 
#  promote the sale, use or other dealings in ImageMagick without prior 
#  written authorization from the E. I. du Pont de Nemours and Company. 
#
#

# Don't require all the GNU mandated files
# Remove comment from no-dependencies for distributions compatable with
# traditional 'make'
AUTOMAKE_OPTIONS = 1.2 foreign # no-dependencies

LIBS = @XTP_LIBS@
DEFS = @XTP_DEFS@

# Executables to build
bin_PROGRAMS = xtp

xtp_SOURCES = xtp.c network.c regular.c regular.h xtp.h

man_MANS = xtp.1

# Additional files to distribute
EXTRA_DIST = Imakefile xtp.1
