PREFIX ?= /usr/local
ETCDIR ?= $(PREFIX)/etc
MANDIR ?= $(PREFIX)/share/man

CFLAGS += -O2 -Wall -Werror
SRCS = $(shell find -type f -name '*.c')
OBJS = $(SRCS:.c=.o)

all: man

$(OBJS): %.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

man: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

install:
	install -d $(DESTDIR)/$(ETCDIR)
	install -m644 man.conf $(DESTDIR)/$(ETCDIR)
	install -d $(DESTDIR)/$(PREFIX)/bin
	install -m755 man $(DESTDIR)/$(PREFIX)/bin
	install -d $(DESTDIR)/$(MANDIR)/man1
	install -m644 man.1 $(DESTDIR)/$(MANDIR)/man1
	install -d $(DESTDIR)/$(MANDIR)/man5
	install -m644 man.conf.5 $(DESTDIR)/$(MANDIR)/man5

clean:
	-rm -f $(OBJS) man

.PHONY: all install clean
