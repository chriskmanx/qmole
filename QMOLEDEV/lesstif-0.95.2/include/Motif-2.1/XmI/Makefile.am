#
# $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/XmI/Makefile.am,v 1.14 2004/11/18 21:00:59 dannybackx Exp $
#

MAINTAINERCLEANFILES=Makefile.in

# Only in 1.2
xmi_12private_h = \
        MessagesI.h

# In other versions above as well
xmi_12base_h = \
	AtomMgrI.h CutPasteI.h DebugUtil.h DragDropI.h \
	HashI.h ImageCacheI.h \
	LTV5EditresP.h LTV6EditresP.h LTglob.h LTmisc.h \
	MacrosI.h TraversalI.h UilI.h XmI.h \
	xdnd.h

# Only in 2.0
xmi_20private_h = \
        MessagesI.h

# In other versions above as well
xmi_20base_h = \
        DirectionI.h ManagerI.h PixConvI.h PopupUtil.h \
        TransltnsI.h XpmI.h 


# Only in 2.1
xmi_21private_h = \
        MessagesI.h

# There's no 2.2 yet :-)
xmi_21base_h =

# Not installed, build time only

noinst_HEADERS= $(xmi_12base_h) $(xmi_20base_h) $(xmi_21base_h) $(xmi_21private_h)
