GENCCODE_ASSEMBLY_TYPE=-a gcc-darwin
SO=dylib
SOBJ=dylib
A=a
LIBPREFIX=lib
LIB_EXT_ORDER=.48.1.1.dylib
COMPILE=/usr/bin/gcc -DU_ATTRIBUTE_DEPRECATED=  -O2 -Wall -ansi -pedantic -Wshadow -Wpointer-arith -Wmissing-prototypes -Wwrite-strings -Wno-long-long  -fno-common -c
LIBFLAGS=-I../../common -I../../common  -dynamic
GENLIB=/usr/bin/gcc -dynamiclib -dynamic -O2 -Wall -ansi -pedantic -Wshadow -Wpointer-arith -Wmissing-prototypes -Wwrite-strings -Wno-long-long   
LDICUDTFLAGS=
LD_SONAME=-Wl,-compatibility_version -Wl,48 -Wl,-current_version -Wl,48.1.1 -install_name
RPATH_FLAGS=
BIR_LDFLAGS=
AR=ar
ARFLAGS=r
RANLIB=ranlib
INSTALL_CMD=/usr/bin/install -c -m 644
