INCLUDES =					\
	$(AWN_CFLAGS)			\
	$(WARN_CFLAGS)

bin_PROGRAMS = avant-window-navigator

AM_CPPFLAGS =								\
	-DDATADIR=\""$(datadir)"\"					\
	-DGNOMELOCALEDIR=\""$(prefix)/$(DATADIRNAME)/locale"\"

avant_window_navigator_SOURCES =			\
	main.c				\
	awn-gconf.h			\
	awn-gconf.c			\
	awn-bar.h			\
	awn-bar.c   			\
	awn-app.h			\
	awn-app.c      			\
	awn-win-manager.h		\
	awn-win-manager.c    		\
	awn-title.h			\
	awn-title.c                     \
	awn-window.h			\
	awn-window.c	 		\
	awn-utils.h
                                        						

avant_window_navigator_LDADD =			\
	$(AWN_LIBS)						

