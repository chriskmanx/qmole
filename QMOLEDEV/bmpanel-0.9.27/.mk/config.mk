PREFIX:=/usr
UGLY:=0
DEBUG:=0
CFLAGS+=-Wall -DWITH_EV -DPREFIX=\"/usr\"
LIBS+=-L/usr/local/lib -lImlib2 -lXrender -lX11 -lXcomposite -lXfixes -lfontconfig -lev
