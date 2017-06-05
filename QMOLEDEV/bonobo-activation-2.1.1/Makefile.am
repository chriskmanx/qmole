NULL = 

SUBDIRS=					\
	po					\
        api-docs                                \
	docs					\
	idl					\
	bonobo-activation			\
	server					\
	utils					\
	test					\
	$(NULL)

EXTRA_DIST=					\
	bonobo-activation.spec.in		\
	bonobo-activation-config.xml		\
	bonobo-activation-config.xml.sample	\
	bonobo-activation.spec			\
	intltool-extract.in			\
	intltool-merge.in			\
	intltool-update.in			\
	AUTHORS					\
	COPYING					\
	ChangeLog				\
	NEWS					\
	README					\
	bonobo-activation-2.0.pc.in		\
	$(NULL)

sysconfigdir=$(sysconfdir)/bonobo-activation
sysconfig_DATA =				\
	bonobo-activation-config.xml		\
	$(NULL)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = bonobo-activation-2.0.pc
