## Process this file with automake to produce Makefile.in

htmldir = $(datadir)/doc/@PACKAGE@-@FOX_MAJOR_VERSION@.@FOX_MINOR_VERSION@/html

SUBDIRS = art screenshots


# Human-written HTML files
HTML_FILES = \
adie.html \
app.html \
calc.html \
consulting.html \
clipboard.html \
datatarget.html \
doc.html \
download.html \
doxygen.cfg \
draganddrop.html \
faq.html \
filefuncs.html \
filter.pl \
focus.html \
fonts.html \
footer.html \
foreword.html \
fox.html \
goals.html \
gpgkey.html \
guiupdate.html \
header.html \
home.html \
icons.html \
install.html \
introduction.html \
layout.html \
license.html \
menu.css \
menu.html \
messages.html \
news.html \
news1.html \
news2.html \
news3.html \
page.css \
pathfinder.html \
projects.html \
references.html \
registry.html \
rex.html \
screenshots.html \
serialization.html \
styles.css \
timers.html \
top.html \
widgets.html \
win32.html \
window.html \
xml.html


docs:
	doxygen doxygen.cfg


# Complete documentation package
html_DATA = $(HTML_FILES)

EXTRA_DIST = $(html_DATA)


