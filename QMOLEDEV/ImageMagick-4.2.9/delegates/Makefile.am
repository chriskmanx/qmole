#  Makefile for installing delegate support files
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
AUTOMAKE_OPTIONS = 1.2 foreign

# Architecture-independent data files
pkgdata_DATA = delegates.mgk

# Additional files to distribute
EXTRA_DIST = delegates.mgk.in delegates.mgk mac.mgk nt.mgk unix.mgk vms.mgk win95.mgk

# Ensure that processed delegates file is not distributed
#dist-hook:
#	$(RM) $(distdir)/delegates.mgk
