# Generated automatically from Makefile.in:boilerplate.mk by configure.

CXXSRCS = \
  Blacklist.cxx \
  CConnection.cxx \
  CMsgHandler.cxx \
  CMsgReader.cxx \
  CMsgReaderV3.cxx \
  CMsgWriter.cxx \
  CMsgWriterV3.cxx \
  CSecurityVncAuth.cxx \
  ComparingUpdateTracker.cxx \
  Configuration.cxx \
  ConnParams.cxx \
  Cursor.cxx \
  Decoder.cxx \
  Encoder.cxx \
  HTTPServer.cxx \
  HextileDecoder.cxx \
  HextileEncoder.cxx \
  KeyRemapper.cxx \
  LogWriter.cxx \
  Logger.cxx \
  Logger_file.cxx \
  Logger_stdio.cxx \
  Password.cxx \
  PixelBuffer.cxx \
  PixelFormat.cxx \
  RREEncoder.cxx \
  RREDecoder.cxx \
  RawDecoder.cxx \
  RawEncoder.cxx \
  Region.cxx \
  SConnection.cxx \
  SMsgHandler.cxx \
  SMsgReader.cxx \
  SMsgReaderV3.cxx \
  SMsgWriter.cxx \
  SMsgWriterV3.cxx \
  ServerCore.cxx \
  SSecurityFactoryStandard.cxx \
  SSecurityVncAuth.cxx \
  Timer.cxx \
  TransImageGetter.cxx \
  UpdateTracker.cxx \
  VNCSConnectionST.cxx \
  VNCServerST.cxx \
  ZRLEEncoder.cxx \
  ZRLEDecoder.cxx \
  encodings.cxx \
  secTypes.cxx \
  util.cxx

SRCS = d3des.c $(CXXSRCS)

OBJS = d3des.o $(CXXSRCS:.cxx=.o)

DIR_CPPFLAGS = -I$(top_srcdir) -DHAVE_VSNPRINTF

library = librfb.a

all:: $(library)

$(library): $(OBJS)
	rm -f $(library)
	$(AR) $(library) $(OBJS)
	$(RANLIB) $(library)

# followed by boilerplate.mk

all::
	@subdirs="$(SUBDIRS)"; for d in $$subdirs; do (cd $$d; $(MAKE) $@) || exit 1; done

clean::
	@subdirs="$(SUBDIRS)"; for d in $$subdirs; do (cd $$d; $(MAKE) $@) || exit 1; done

clean::
	rm -f $(program) $(library) *.o

SHELL = /bin/sh
top_srcdir = ..

CC = /usr/bin/gcc
CFLAGS = -O2 -Wall $(DIR_CFLAGS)
CCLD = $(CC)
CXX = c++
CXXFLAGS = -O2 -Wall
CXXLD = $(CXX)
CPPFLAGS = 
DEFS =  -DX_DISPLAY_MISSING=1 
ALL_CPPFLAGS = $(CPPFLAGS) $(DEFS) $(DIR_CPPFLAGS)
LIBS = 
LDFLAGS = 
RANLIB = ranlib
AR = ar cq

.SUFFIXES:
.SUFFIXES: .cxx .c .o

.c.o:
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -c $<

.cxx.o:
	$(CXX) $(ALL_CPPFLAGS) $(CXXFLAGS) -c $<
