SUBDIRS = svg
DIST_SUBDIRS = svg

## GQview inline icons

ICONS_INLINE = \
	folder_closed.png	\
	folder_locked.png	\
	folder_open.png		\
	folder_up.png		\
	gqview_icon.png		\
	geeqie_logo.png		\
	scroller.png		\
	sheet_broken.png	\
	icon_float.png		\
	icon_thumb.png		\
	icon_book.png		\
	icon_config.png		\
	icon_tools.png		\
	icon_view.png

ICONS_INLINE_PAIRS = \
	folder_closed		$(srcdir)/folder_closed.png	\
	folder_locked		$(srcdir)/folder_locked.png	\
	folder_open		$(srcdir)/folder_open.png	\
	folder_up		$(srcdir)/folder_up.png		\
	gqview_icon		$(srcdir)/gqview_icon.png	\
	geeqie_logo		$(srcdir)/geeqie_logo.png	\
	icon_float		$(srcdir)/icon_float.png	\
	icon_thumb		$(srcdir)/icon_thumb.png	\
	icon_scroller		$(srcdir)/scroller.png		\
	icon_broken		$(srcdir)/sheet_broken.png	\
	icon_book		$(srcdir)/icon_book.png		\
	icon_config		$(srcdir)/icon_config.png	\
	icon_tools		$(srcdir)/icon_tools.png	\
	icon_view		$(srcdir)/icon_view.png

icons_inline.h: $(ICONS_INLINE) Makefile.in
	@(echo '/* Auto generated file, do not edit */'; echo; \
	 echo '#ifndef ICONS_INLINE_H'; \
	 echo '#define ICONS_INLINE_H'; echo; \
	 $(GDK_PIXBUF_CSOURCE) --raw --extern --build-list $(ICONS_INLINE_PAIRS); \
	 echo '#endif /* ICONS_INLINE_H */') > $@ || echo "!!! Failed to generate $@ !!!"

noinst_DATA = icons_inline.h
CLEANFILES = $(noinst_DATA)

## end GQview specific icons


EXTRA_DIST = \
	$(ICONS_INLINE)		\
	marker.xpm

