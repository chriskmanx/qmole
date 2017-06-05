
noinst_LTLIBRARIES = libxp.la

AM_CPPFLAGS = \
	$(MATHVIEW_CFLAGS) \
	-I$(srcdir)/../itex2mml \
	-DABI_BUILD_VERSION=\"$(VERSION)\"

libxp_la_SOURCES = \
	AbiMathView.cpp \
	AbiMathView.h \
	gr_Abi_AreaFactory.h \
	gr_Abi_CharArea.cpp \
	gr_Abi_CharArea.h \
	gr_Abi_ColorArea.cpp \
	gr_Abi_ColorArea.h \
	gr_Abi_ComputerModernShaper.cpp \
	gr_Abi_ComputerModernShaper.h \
	gr_Abi_DefaultShaper.cpp \
	gr_Abi_DefaultShaper.h \
	gr_Abi_InkArea.cpp \
	gr_Abi_InkArea.h \
	gr_Abi_MathGraphicDevice.cpp \
	gr_Abi_MathGraphicDevice.h \
	gr_Abi_RenderingContext.cpp \
	gr_Abi_RenderingContext.h \
	gr_Abi_StandardSymbolsShaper.cpp \
	gr_Abi_StandardSymbolsShaper.h \
	ie_imp_MathML.cpp \
	ie_imp_MathML.h

EXTRA_DIST = \
	entitiesMap.inc

