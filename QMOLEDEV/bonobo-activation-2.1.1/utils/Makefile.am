NULL = 

bin_SCRIPTS   = $(BONOBO_SLAY)
EXTRA_SCRIPTS = bonobo-slay.in 

sbin_PROGRAMS  = bonobo-activation-sysconf

bonobo_activation_sysconf_SOURCES =	\
	bonobo-activation-sysconf.c	\
	$(NULL)

INCLUDES = 					\
	$(UTILS_CFLAGS)				\
	-I$(top_srcdir)				\
	-DSERVER_LOCALEDIR=\""$(datadir)/locale"\"	\
	-DSERVER_CONFDIR=\"$(sysconfdir)\"		\
	$(NULL)

bonobo_activation_sysconf_LDADD =	\
	$(UTILS_LIBS)			\
	$(INTLLIBS)			\
	$(NULL)

