// Common Xfe constants

#ifndef COPYRIGHT
#define COPYRIGHT "Copyright (C) 2002-2013 Roland Baudin (roland65@free.fr)"
#endif

// Default file and directory list time format
#ifndef DEFAULT_TIME_FORMAT
#define DEFAULT_TIME_FORMAT "%x %X"
#endif

// Default initial main window width
#ifndef DEFAULT_WINDOW_WIDTH
#define DEFAULT_WINDOW_WIDTH 800
#endif

// Default initial main window heigth
#ifndef DEFAULT_WINDOW_HEIGHT
#define DEFAULT_WINDOW_HEIGHT 600
#endif

// Default initial main window X position
#ifndef DEFAULT_WINDOW_XPOS
#define DEFAULT_WINDOW_XPOS 50
#endif

// Default initial main window Y position
#ifndef DEFAULT_WINDOW_YPOS
#define DEFAULT_WINDOW_YPOS 50
#endif

// Maximum sizes for thumbnail image preview
#ifndef MAX_BIGTHUMB_SIZE
#define MAX_BIGTHUMB_SIZE 64
#endif
#ifndef MAX_MINITHUMB_SIZE
#define MAX_MINITHUMB_SIZE 20
#endif

// Minimum width of a file panel or directory panel
#ifndef MIN_PANEL_WIDTH
#define MIN_PANEL_WIDTH 100
#endif

// Maximum length of a file name
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

// Maximum length of a command line
#ifndef MAX_COMMAND_SIZE
#define MAX_COMMAND_SIZE 256
#endif

// Maximum length of a filter pattern
#ifndef MAX_PATTERN_SIZE
#define MAX_PATTERN_SIZE 64
#endif

// Root directory string
#ifndef ROOTDIR
#define ROOTDIR "/"
#endif

// Path separator
#ifndef PATHSEPSTRING
#define PATHSEPSTRING "/"
#endif

// Path separator
#ifndef PATHSEPCHAR
#define PATHSEPCHAR '/'
#endif

// Maximum number of path links 
#ifndef MAXLINKS
#define MAX_LINKS 40
#endif

// Run history size
#ifndef RUN_HIST_SIZE
#define RUN_HIST_SIZE 30
#endif

// Open with history size
#ifndef OPEN_HIST_SIZE
#define OPEN_HIST_SIZE 30
#endif

// Filter history size
#ifndef FILTER_HIST_SIZE
#define FILTER_HIST_SIZE 30
#endif

#ifdef STARTUP_NOTIFICATION
// If startup notification is used, this is the timeout value (seconds)
#define STARTUP_TIMEOUT 15
#endif

// If startup notification is not used, we use an ugly simulation of a startup time (seconds)
#define SIMULATED_STARTUP_TIME 3

// Local data path
#ifndef DATAPATH
#define DATAPATH ".local/share"
#endif

// Local config path
#ifndef CONFIGPATH
#define CONFIGPATH ".config"
#endif

// Xfe config path
#ifndef XFECONFIGPATH
#define XFECONFIGPATH "xfe"
#endif

// Local trashcan directory path
#ifndef TRASHPATH
#define TRASHPATH "Trash"
#endif

// Local trashcan directory path for files
#ifndef TRASHFILESPATH
#define TRASHFILESPATH "Trash/files"
#endif

// Local trashcan directory path for infos
#ifndef TRASHINFOPATH
#define TRASHINFOPATH "Trash/info"
#endif

// Xfe application name
#ifndef XFEAPPNAME
#define XFEAPPNAME "xfe"
#endif

// Xfe vendor name
#ifndef XFEVDRNAME
#define XFEVDRNAME "Xfe"
#endif 

// Xfe config file name
#ifndef XFECONFIGNAME
#define XFECONFIGNAME "xferc"
#endif

// Default icon path
#ifndef DEFAULTICONPATH
#define DEFAULTICONPATH   "~/.config/xfe/icons/xfe-theme:/usr/local/share/xfe/icons/xfe-theme:/usr/share/xfe/icons/xfe-theme"
#endif

// Command to launch Xfe as root with sudo or su, using Xvt as a terminal
#ifndef SUDOCMD
#define SUDOCMD "-fn 7x14 -geometry 60x4 -e sudo su -c 'nohup xfe >& /dev/null & sleep 1'"
#endif

#ifndef SUCMD
#define SUCMD "-fn 7x14 -geometry 60x4 -e su -c 'nohup xfe >& /dev/null & sleep 1'"
#endif

// Tooltips setup time and duration
#ifndef TOOLTIP_PAUSE 
#define TOOLTIP_PAUSE 500
#endif

#ifndef TOOLTIP_TIME
#define TOOLTIP_TIME 10000
#endif

// Coefficient used to darken the sorted column in detailed mode
#ifndef DARKEN_SORT
#define DARKEN_SORT 0.96
#endif


// Default terminal program
#ifndef DEFAULT_TERMINAL
#define DEFAULT_TERMINAL "xterm -sb"
#endif


// These have to be the same as in xferc.in

// Default text editor program
#ifndef DEFAULT_TXTEDITOR
#define DEFAULT_TXTEDITOR "xfw"
#endif

// Default text viewer program
#ifndef DEFAULT_TXTVIEWER
#define DEFAULT_TXTVIEWER "xfv"
#endif

// Default image editor program
#ifndef DEFAULT_IMGEDITOR
#define DEFAULT_IMGEDITOR "gimp"
#endif

// Default image viewer program
#ifndef DEFAULT_IMGVIEWER
#define DEFAULT_IMGVIEWER "xfi"
#endif

// Default archiver program
#ifndef DEFAULT_ARCHIVER
#define DEFAULT_ARCHIVER "xarchiver"
#endif

// Default PDF viewer program
#ifndef DEFAULT_PDFVIEWER
#define DEFAULT_PDFVIEWER "xpdf"
#endif

// Default audio viewer program
#ifndef DEFAULT_AUDIOVIEWER
#define DEFAULT_AUDIOVIEWER "audacious"
#endif

// Default video viewer program
#ifndef DEFAULT_VIDEOVIEWER
#define DEFAULT_VIDEOVIEWER "mplayer"
#endif


// Common macros

// Tab character
#define TAB (FXString)"\t"
#define TAB2 (FXString)"\t\t"

// Macro to add tab characters before and after a given string
#define TABS(s) ((FXString)"\t"+(s)+(FXString)"\t")

// Macro to add parentheses before and after a given string
#define PARS(s) ((FXString)" ("+(s)+(FXString)")")


// Linux specials

#if defined(linux)

// fstab path
#ifndef FSTAB_PATH
#define FSTAB_PATH "/etc/fstab"
#endif

// mtab path
#ifndef MTAB_PATH
#define MTAB_PATH "/proc/mounts"
#endif

#endif
