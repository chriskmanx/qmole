noinst_LIBRARIES = libFbTk.a

if XFT
xft_SOURCE= XftFontImp.hh XftFontImp.cc
endif
if MULTIBYTE
xmb_SOURCE= XmbFontImp.hh XmbFontImp.cc
endif
if PNG
png_SOURCE=ImagePNG.cc ImagePNG.hh
endif

libFbTk_a_SOURCES = App.hh App.cc Color.cc Color.hh EventHandler.hh EventManager.hh EventManager.cc \
	FbWindow.hh FbWindow.cc Font.cc Font.hh FontImp.hh NotCopyable.hh \
	Directory.hh Directory.cc \
	XFontImp.cc XFontImp.hh \
	FbDrawable.hh FbDrawable.cc \
	${xft_SOURCE} \
	${xmb_SOURCE} \
	ImageControl.hh ImageControl.cc \
	Image.hh Image.cc \
	MenuTheme.hh MenuTheme.cc Menu.hh Menu.cc MenuItem.hh MenuItem.cc \
	MenuIcon.hh MenuIcon.cc MenuSeparator.hh MenuSeparator.cc \
	TextureRender.hh TextureRender.cc Theme.hh Theme.cc RefCount.hh Command.hh \
	SimpleCommand.hh \
	Color.hh Color.cc Texture.hh Texture.cc XrmDatabaseHelper.hh Timer.hh Timer.cc \
	StringUtil.hh StringUtil.cc Text.hh Text.cc Transparent.hh Transparent.cc \
	GContext.hh GContext.cc FbPixmap.hh FbPixmap.cc ThemeItems.hh \
	Subject.hh Subject.cc \
	Observer.hh Observer.cc \
	PixmapWithMask.hh Resource.hh Resource.cc XrmDatabaseHelper.hh \
	${png_SOURCE} \
	ImageXPM.hh ImageXPM.cc \
	Surface.hh Surface.cc TextBox.hh TextBox.cc KeyUtil.hh KeyUtil.cc \
	FileUtil.hh FileUtil.cc ThemeItems.cc ImageImlib2.hh ImageImlib2.cc
