// This is adapted from 'textedit', a demo text viewer found
// in the early versions of the FOX library and written
// by Jeroen van der Zijp.

#include "config.h"
#include "i18n.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "startupnotification.h"
#include "MessageBox.h"
#include "DirList.h"
#include "ViewWindow.h"
#include "XFileView.h"

// Add FOX hacks
#include "foxhacks.cpp"

FXColor highlightcolor;
FXbool allowPopupScroll=FALSE;
unsigned int single_click;
FXbool file_tooltips;
FXbool relative_resize;
FXString homedir;
FXString xdgconfighome;
FXString xdgdatahome;


// Hand cursor replacement
#define hand_width 32
#define hand_height 32
#define hand_x_hot 6
#define hand_y_hot 1
 static const unsigned char hand_bits[] = {
	0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x90, 0x00, 0x00, 0x00,
	0x90, 0x00, 0x00, 0x00, 0x90, 0x00, 0x00, 0x00, 0x90, 0x07, 0x00, 0x00,
	0x97, 0x1a, 0x00, 0x00, 0x99, 0x2a, 0x00, 0x00, 0x11, 0x28, 0x00, 0x00,
	0x12, 0x20, 0x00, 0x00, 0x02, 0x20, 0x00, 0x00, 0x02, 0x20, 0x00, 0x00,
	0x04, 0x20, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00,
	0xf0, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
 
 static const unsigned char hand_mask_bits[] = {
	0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0xf0, 0x00, 0x00, 0x00,
	0xf0, 0x00, 0x00, 0x00, 0xf0, 0x00, 0x00, 0x00, 0xf0, 0x07, 0x00, 0x00,
	0xf7, 0x1f, 0x00, 0x00, 0xff, 0x3f, 0x00, 0x00, 0xff, 0x3f, 0x00, 0x00,
	0xfe, 0x3f, 0x00, 0x00, 0xfe, 0x3f, 0x00, 0x00, 0xfe, 0x3f, 0x00, 0x00,
	0xfc, 0x3f, 0x00, 0x00, 0xfc, 0x1f, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00,
	0xf0, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
   

// Map
FXDEFMAP(XFileView) XFileViewMap[]={
									 FXMAPFUNC(SEL_SIGNAL,XFileView::ID_CLOSEALL,XFileView::onCmdCloseAll),
									 FXMAPFUNC(SEL_COMMAND,XFileView::ID_CLOSEALL,XFileView::onCmdCloseAll),
                                   };


// Object implementation
FXIMPLEMENT(XFileView,FXApp,XFileViewMap,ARRAYNUMBER(XFileViewMap))


// Make some windows
XFileView::XFileView(const FXString& appname, const FXString& vdrname):FXApp(appname,vdrname)
{
    // If interrupt happens, quit gracefully; we may want to
    // save edit buffer contents w/o asking if display gets
    // disconnected or if hangup signal is received.
    addSignal(SIGINT,this,ID_CLOSEALL);
    addSignal(SIGQUIT,this,ID_CLOSEALL);
    addSignal(SIGHUP,this,ID_CLOSEALL);
    addSignal(SIGPIPE,this,ID_CLOSEALL);
}


// Initialize application
void XFileView::init(int& argc,char** argv,bool connect)
{
    // After init, the registry has been loaded
    FXApp::init(argc,argv,connect);
}


// Exit application
void XFileView::exit(int code)
{
    // Write registry, and quit
    FXApp::exit(code);
}


// Close all windows
long XFileView::onCmdCloseAll(FXObject*,FXSelector,void*)
{
    while(0<windowlist.no() && windowlist[0]->close(TRUE));
    return 1;
}


// Clean up
XFileView::~XFileView()
{
    FXASSERT(windowlist.no()==0);
}




// Usage message
#define USAGE_MSG	_("\
\nUsage: xfv [options] [file1] [file2] [file3]...\n\
\n\
    [options] can be any of the following:\n\
\n\
        -h, --help         Print (this) help screen and exit.\n\
        -v, --version      Print version information and exit.\n\
\n\
    [file1] [file2] [file3]... are the path(s) to the file(s) you want to open on start up.\n\
\n")


// Start the whole thing
int main(int argc,char *argv[])
{
    ViewWindow *window=NULL;
    FXString    file;
    int       i;
	const char *appname = "xfv";
	const char *xfename = XFEAPPNAME;
	const char *vdrname = XFEVDRNAME;
	FXbool loadicons;

	// Get environment variables $HOME, $XDG_DATA_HOME and $XDG_CONFIG_HOME
	homedir=FXSystem::getHomeDirectory();
    if(homedir=="")
        homedir=ROOTDIR;
	xdgdatahome=getenv("XDG_DATA_HOME");
    if(xdgdatahome=="")
        xdgdatahome=homedir + PATHSEPSTRING DATAPATH;     
	xdgconfighome=getenv("XDG_CONFIG_HOME");
    if(xdgconfighome=="")
        xdgconfighome=homedir + PATHSEPSTRING CONFIGPATH;

#ifdef HAVE_SETLOCALE
	// Set locale via LC_ALL.
	setlocale (LC_ALL, "");
#endif

#if ENABLE_NLS
	// Set the text message domain.
	bindtextdomain (PACKAGE, LOCALEDIR);
	bind_textdomain_codeset(PACKAGE,"utf-8");
	textdomain (PACKAGE);
#endif
	
    // Make application
    XFileView* application=new XFileView(appname,vdrname);

    // Open display
    application->init(argc,argv);

    // Redefine the default hand cursor
    FXCursor* hand=new FXCursor(application,hand_bits,hand_mask_bits,hand_width,hand_height,hand_x_hot,hand_y_hot);
    application->setDefaultCursor(DEF_HAND_CURSOR,hand);

	// Load all application icons
	loadicons=loadAppIcons(application);
		
    // Read the Xfe registry
	FXRegistry* reg_xfe=new FXRegistry(xfename,vdrname);
	reg_xfe->read();

	// Set base color (to change the default base color at first run)
	FXColor basecolor=reg_xfe->readColorEntry("SETTINGS","basecolor",FXRGB(237,233,227));
	application->setBaseColor(basecolor);

	// Set Xfv normal font according to the Xfe registry
	FXString fontspec;
	fontspec=reg_xfe->readStringEntry("SETTINGS","font","Helvetica,100,normal,regular");
	if (!fontspec.empty())
	{
    	FXFont* normalFont=new FXFont(application,fontspec);
        application->setNormalFont(normalFont);
    }

	// Set single click navigation according to the Xfe registry
	single_click=reg_xfe->readUnsignedEntry("SETTINGS","single_click",SINGLE_CLICK_NONE);

	// Set smooth scrolling according to the Xfe registry
	FXbool smoothscroll=reg_xfe->readUnsignedEntry("SETTINGS","smooth_scroll",TRUE);
	
	// Set file list tooltip flag according to the Xfe registry
	file_tooltips=reg_xfe->readUnsignedEntry("SETTINGS","file_tooltips",TRUE);

	// Set relative resizing flag according to the Xfe registry
	relative_resize=reg_xfe->readUnsignedEntry("SETTINGS","relative_resize",TRUE);

	// Delete the Xfe registry
	delete reg_xfe;

	// Make a tool tip
    new FXToolTip(application,0);

    // Create application
    application->create();
	if (!loadicons)
		 MessageBox::error(application,BOX_OK,_("Error loading icons"),_("Unable to load some icons. Please check your icons path!"));

	// Tooltips setup time and duration
	application->setTooltipPause(TOOLTIP_PAUSE);
	application->setTooltipTime(TOOLTIP_TIME);

    // Loop over args
    for(i=1; i<argc; ++i)
    {

        // Parse a few options
    	if(compare(argv[i],"-v")==0 || compare(argv[i],"--version")==0)
		{
			fprintf(stdout,"%s version %s\n",PACKAGE,VERSION);
			exit(0);
		}
        if(compare(argv[i],"-h")==0 || compare(argv[i],"--help")==0)
        {
			fprintf(stdout,USAGE_MSG);
            exit(0);
        }

        // Load the file
        else
        {
            file=FXPath::absolute(argv[i]);
            window=new ViewWindow(application,_("untitled"));

			// Catch SIGCHLD to harvest zombie child processes
			application->addSignal(SIGCHLD,window,ViewWindow::ID_HARVEST,TRUE);

			window->setSmoothScroll(smoothscroll);
            window->create();
            window->loadFile(file);
        }
    }

    // Make window, if none opened yet
    if(!window)
    {
        window=new ViewWindow(application,_("untitled"));

		// Catch SIGCHLD to harvest zombie child processes
		application->addSignal(SIGCHLD,window,ViewWindow::ID_HARVEST,TRUE);

        window->setSmoothScroll(smoothscroll);
		window->create();
    }

    // Run
    return application->run();
}


