#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "startupnotification.h"
#include "FileDialog.h"
#include "MessageBox.h"
#include "CommandWindow.h"
#include "XFilePackage.h"


// Add FOX hacks
#include "foxhacks.cpp"

// Maximum length of a file name
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

char **args;
FXbool dpkg=FALSE;
FXbool rpm=FALSE;
FXColor highlightcolor;
FXbool allowPopupScroll=FALSE;
unsigned int single_click;
FXbool file_tooltips;
FXbool relative_resize;
FXbool save_win_pos;
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
FXDEFMAP(XFilePackage) XFilePackageMap[]=
{
	FXMAPFUNC(SEL_SIGNAL,XFilePackage::ID_HARVEST,XFilePackage::onSigHarvest),
	FXMAPFUNC(SEL_CLOSE,0,XFilePackage::onCmdQuit),
	FXMAPFUNC(SEL_COMMAND,XFilePackage::ID_QUIT,XFilePackage::onCmdQuit),
	FXMAPFUNC(SEL_COMMAND,XFilePackage::ID_UNINSTALL,XFilePackage::onCmdUninstall),
	FXMAPFUNC(SEL_COMMAND,XFilePackage::ID_ABOUT,XFilePackage::onCmdAbout),
	FXMAPFUNC(SEL_COMMAND,XFilePackage::ID_OPEN,XFilePackage::onCmdOpen),
	FXMAPFUNC(SEL_COMMAND,XFilePackage::ID_INSTALL,XFilePackage::onCmdInstall),
	FXMAPFUNC(SEL_UPDATE,XFilePackage::ID_DESCRIPTION,XFilePackage::onUpdDescription),
	FXMAPFUNC(SEL_UPDATE,XFilePackage::ID_FILELIST,XFilePackage::onUpdFileList),
};


// Object implementation
FXIMPLEMENT(XFilePackage,FXMainWindow,XFilePackageMap,ARRAYNUMBER(XFilePackageMap))

// Constructor
XFilePackage::XFilePackage(FXApp *a):FXMainWindow(a,"Xfp",NULL,NULL,DECOR_ALL)
{
	FXString key;
	FXHotKey hotkey;

    setIcon(xfpicon);
		
    // Make menu bar
    menubar=new FXMenuBar(this,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);

	// Sites where to dock
	FXDockSite* topdock=new FXDockSite(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
	new FXDockSite(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
	new FXDockSite(this,LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y);
	new FXDockSite(this,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y);

    // Tool bar
  	FXToolBarShell* dragshell1=new FXToolBarShell(this,FRAME_RAISED);
  	toolbar=new FXToolBar(topdock,dragshell1,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED);
  	new FXToolBarGrip(toolbar,toolbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);
    int style=BUTTON_TOOLBAR;

    // File menu
    filemenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&File"),NULL,filemenu);

    // Preferences Menu
    prefsmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Preferences"),NULL,prefsmenu);

    // Help menu
    helpmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Help"),NULL,helpmenu);

    // Toolbar button: File open
	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
    new FXButton(toolbar,TAB+_("Open package file")+PARS(key),fileopenicon,this,ID_OPEN,style|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // File Menu entries
	FXMenuCommand* mc = NULL;

    mc=new FXMenuCommand(filemenu,_("&Open..."),fileopenicon,this,ID_OPEN);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("&Quit"),quiticon,this,ID_QUIT);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","quit","Ctrl-Q");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
	getAccelTable()->addAccel(KEY_Escape,this,FXSEL(SEL_COMMAND,ID_QUIT));
	
    // Preferences menu
    new FXMenuCheck(prefsmenu,_("&Toolbar"),toolbar,FXWindow::ID_TOGGLESHOWN);

    // Help Menu entries
    mc=new FXMenuCommand(helpmenu,_("&About X File Package"),NULL,this,ID_ABOUT,0);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","help","F1");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	// Close accelerator
	key=getApp()->reg().readStringEntry("KEYBINDINGS","close","Ctrl-W");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,this,FXSEL(SEL_COMMAND,XFilePackage::ID_QUIT));

    // Make a tool tip
    new FXToolTip(getApp(),0);

    // Buttons
    FXHorizontalFrame* buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
	new FXDragCorner(buttons);

    // Close
    new FXButton(buttons,_("&Close"),NULL,this,XFilePackage::ID_QUIT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    // Uninstall
    new FXButton(buttons,_("&Uninstall"),NULL,this,XFilePackage::ID_UNINSTALL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    // Install/Upgrade
    new FXButton(buttons,_("&Install/Upgrade"),NULL,this,XFilePackage::ID_INSTALL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    // Switcher
    FXTabBook* tabbook = new FXTabBook(this,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_RIGHT|FRAME_RAISED,0,0,0,0, 0,0,0,0);

    // First item is Description
    new FXTabItem(tabbook,_("&Description"),NULL);
    FXVerticalFrame *descr=new FXVerticalFrame(tabbook);
	FXPacker *p=new FXPacker(descr,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN,0,0,0,0, 0,0,0,0);
    description=new FXText(p,this,XFilePackage::ID_DESCRIPTION,LAYOUT_FILL_X|LAYOUT_FILL_Y);

    // Second item is File List
    new FXTabItem(tabbook,_("File &List"),NULL);
    FXVerticalFrame *flistfr=new FXVerticalFrame(tabbook);
    p=new FXPacker(flistfr,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN,0,0,0,0, 0,0,0,0);
	list=new FXTreeList(p,this,XFilePackage::ID_FILELIST,LAYOUT_FILL_X|LAYOUT_FILL_Y|TREELIST_SHOWS_LINES);

	// Initialize file name
	filename="";
}

// Destructor
XFilePackage::~XFilePackage()
{
    delete menubar;
    delete toolbar;
	delete filemenu;
	delete prefsmenu;
	delete helpmenu;
    delete list;
	delete description;
}

// About box
long XFilePackage::onCmdAbout(FXObject*,FXSelector,void*)
{
	FXString msg;
	msg.format(_("X File Package Version %s is a simple rpm or deb package manager.\n\n"),VERSION);
	msg += COPYRIGHT;
	MessageBox about(this,_("About X File Package"),msg.text(),xfpicon,BOX_OK|DECOR_TITLE|DECOR_BORDER,
	                 JUSTIFY_CENTER_X|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    about.execute(PLACEMENT_OWNER);
    return 1;
}

// Open package
long XFilePackage::onCmdOpen(FXObject*,FXSelector,void*)
{
    const char *patterns[]=
        {
            _("All Files"),			"*",
            _("RPM source packages"),	"*.src.rpm",
            _("RPM packages"),			"*.rpm",
            _("DEB packages"),			"*.deb",
			NULL
        };

	FileDialog opendialog(this,_("Open Document"));
	opendialog.setSelectMode(SELECTFILE_EXISTING);
    opendialog.setPatternList(patterns);
  	opendialog.setDirectory(FXPath::directory(filename));
    if(opendialog.execute())
    {
        filename=opendialog.getFilename();
		list_clean=TRUE;
		desc_clean=TRUE;
    }

    return 1;
}

// Install/upgrade package
long XFilePackage::onCmdInstall(FXObject*,FXSelector,void*)
{
	FXString cmd;
	
	getApp()->flush();

	if (strlen(filename.text())==0)
	{
        MessageBox::error(this,BOX_OK,_("Error"),_("No package loaded"));
		return 0;
	}
		
	// Package name
    FXString package=FXPath::name(filename);

    // Command to perform
	FXString ext=FXPath::extension(filename);
    if(comparecase(ext,"rpm")==0)
		cmd="rpm -Uvh "+filename;
	else if(comparecase(ext,"deb")==0)
		cmd="dpkg -i "+filename;
	else
	{
		MessageBox::error(this,BOX_OK,_("Error"),_("Unknown package format"));
		return 0;
	}

    // Make and show command window
	CommandWindow *cmdwin=new CommandWindow(this,_("Install/Upgrade Package"),cmd,10,80);			
	cmdwin->create();

	FXString msg;
	msg.format(_("Installing package: %s \n"),package.text());
	cmdwin->appendText(msg.text());

	// The CommandWindow object will delete itself when closed!

    return 1;
}


// Uninstall package based on the package name (version is ignored)
long XFilePackage::onCmdUninstall(FXObject*,FXSelector,void*)
{
	FXString cmd;

	getApp()->flush();
	
	if (strlen(filename.text())==0)
	{
        MessageBox::error(this,BOX_OK,_("Error"),_("No package loaded"));
		return 0;
	}

    // Command to perform
	FXString package;
	FXString ext=FXPath::extension(filename);
    if(comparecase(ext,"rpm")==0)
	{
		// Get package name
		package=FXPath::name(filename);
		package=package.section('-',0);
		cmd="rpm -e "+ package;
	}
	else if(comparecase(ext,"deb")==0)
	{
		// Get package name
		package=FXPath::name(filename);
		package=package.section('_',0);
		cmd="dpkg -r "+ package;
	}
	else
	{
		MessageBox::error(this,BOX_OK,_("Error"),_("Unknown package format"));
		return 0;
	}

    // Make and show command window
	CommandWindow *cmdwin=new CommandWindow(this,_("Uninstall Package"),cmd,10,80);			
	cmdwin->create();

	FXString msg;
	msg.format(_("Uninstalling package: %s \n"),package.text());	
	cmdwin->appendText(msg.text());

	// The CommandWindow object will delete itself when closed!

    return 1;
}


// Save configuration when quitting
void XFilePackage::saveConfig()
{
    // Write new window size and position back to registry
    getApp()->reg().writeUnsignedEntry("OPTIONS","width",(unsigned int)getWidth());
    getApp()->reg().writeUnsignedEntry("OPTIONS","height",(unsigned int)getHeight());

	if (save_win_pos)
	{
		// Account for the Window Manager border size
		XWindowAttributes xwattr;
		if (XGetWindowAttributes((Display*)getApp()->getDisplay(),this->id(),&xwattr))
		{
			getApp()->reg().writeIntEntry("OPTIONS","xpos",getX()-xwattr.x);
			getApp()->reg().writeIntEntry("OPTIONS","ypos",getY()-xwattr.y);
		}
		else
		{
			getApp()->reg().writeIntEntry("OPTIONS","xpos",getX());
			getApp()->reg().writeIntEntry("OPTIONS","ypos",getY());
		}
	}

    // Last opened filename
    getApp()->reg().writeStringEntry("OPTIONS","filename",filename.text());

    // Toolbar status
	if(toolbar->shown())
        getApp()->reg().writeUnsignedEntry("OPTIONS","showtoolbar",TRUE);
    else
        getApp()->reg().writeUnsignedEntry("OPTIONS","showtoolbar",FALSE);

	// Write registry options
	getApp()->reg().write();
}


// Harvest the zombies
long XFilePackage::onSigHarvest(FXObject*,FXSelector,void*)
{
	while(waitpid(-1,NULL,WNOHANG)>0);
	return 1;
}


// Quit application
long XFilePackage::onCmdQuit(FXObject*,FXSelector,void*)
{
	// Save options
    saveConfig();

    // Exit
	getApp()->exit(0);
    return 1;
}


// Update file list
long XFilePackage::onUpdFileList(FXObject*,FXSelector,void*)
{
	FXString cmd;

	if(!list_clean)
        return 0;
	
    list_clean=FALSE;
 
	FXString ext=FXPath::extension(filename);
    if(comparecase(ext,"rpm")==0)
    {
		errorflag=FALSE;
		cmd="rpm -qlp "+::quote(filename);
	}
	else if(comparecase(ext,"deb")==0)
	{
		errorflag=FALSE;
		cmd="dpkg -c "+::quote(filename);
	}
	else if (errorflag==FALSE)
	{
		errorflag=TRUE;
    	list->clearItems();
		MessageBox::error(this,BOX_OK,_("Error"),_("Unknown package format"));
		return 0;
	}

    FILE *pcmd=popen(cmd.text(),"r");
    if(!pcmd)
    {
        perror("popen");
        exit(1);
    }
    char text[10000]={0};
    FXString str;
    str=FXPath::name(filename);
    strlcpy(text,str.text(),str.length()+1);

	// Clear list
    list->clearItems();

    // First item
    getApp()->beginWaitCursor();
	FXTreeItem *topmost;
	topmost=list->prependItem(NULL,text,minifoldericon,minifolderopenicon);
   
	// Next items
	while(fgets(text,sizeof(text),pcmd))
    {
        text[strlen(text)-1]='\0';// kill trailing lf
        list->appendItem(topmost,text,minidocicon,minidocicon);
	}
	list->expandTree(topmost);
    getApp()->endWaitCursor();
	
	pclose(pcmd);
	return 1;
}


// Update description
long XFilePackage::onUpdDescription(FXObject*,FXSelector,void*)
{
	FXString cmd;

    if(!desc_clean)
        return 0;
    desc_clean=FALSE;

    FXString buf;
	FXString ext=FXPath::extension(filename);
    if(comparecase(ext,"rpm")==0)
	{
		errorflag=FALSE;
		cmd="rpm -qip "+::quote(filename);
		buf += _("[RPM package]\n"); 
	}
	else if(comparecase(ext,"deb")==0)
	{
		errorflag=FALSE;
		buf += _("[DEB package]\n"); 
		cmd="dpkg -I "+::quote(filename);
	}
	else if (errorflag==FALSE)
	{
		errorflag=TRUE;
		list->clearItems();
		MessageBox::error(this,BOX_OK,_("Error"),_("Unknown package format"));
		return 0;
	}

    FILE *pcmd=popen(cmd.text(),"r");
    if(!pcmd)
    {
        perror("popen");
        exit(1);
    }
	
    // Don't display the header for Debian packages
	int suppress_header=0;
	if(comparecase(ext,"deb")==0)
		suppress_header=1;

    char text[10000]={0};
    while(fgets(text,sizeof(text),pcmd))
    {
		if (suppress_header)
			suppress_header = (strstr(text,"Package:")==NULL); 
        if (!suppress_header) 
			buf+=text;
    }
    if (pclose(pcmd)==-1 && errno!=ECHILD)  // ECHILD can be set if the child was caught by sigHarvest
    {
        MessageBox::error(this,BOX_OK,_("Error"),_("Query of %s failed!"),filename.text());
		list->clearItems();
        description->setText("");
		filename="";
		desc_clean=FALSE;
    	list_clean=FALSE;
    }
    else
        description->setText(buf.text());

    return 1;
}

// Start the ball rolling
void XFilePackage::start(FXString startpkg)
{
    filename=startpkg;

    if(filename != "")
    {
    	struct stat info;
    	FILE *fp;
    	if(stat(filename.text(),&info)!=0)
    	{
        	MessageBox::error(this,BOX_OK,_("Error Loading File"),_("Non-existing file: %s"),filename.text());
			filename="";
			desc_clean=FALSE;
    		list_clean=FALSE;
    	}
    	else
		{
			fp=fopen(filename.text(),"r");
    		if(!fp)
    		{
        		MessageBox::error(this,BOX_OK,_("Error Loading File"),_("Unable to open file: %s"),filename.text());
				filename="";
				desc_clean=FALSE;
    			list_clean=FALSE;
    		}
			else
			{
				fclose(fp);
    			desc_clean=TRUE;
    			list_clean=TRUE;
			}
		}
	}
	else
	{
		filename="";
		desc_clean=FALSE;
    	list_clean=FALSE;
	}
}

void XFilePackage::create()
{
    // Read the Xfe registry
	FXRegistry* reg_xfe=new FXRegistry(XFEAPPNAME,"");
	reg_xfe->read();
	
	// Set Xfp text font according to the Xfe registry
	FXString fontspec;
	fontspec=reg_xfe->readStringEntry("SETTINGS","textfont","Helvetica,100,normal,regular");
	if(!fontspec.empty())
	{
    	FXFont* font=new FXFont(getApp(),fontspec);
		font->create();
        description->setFont(font);
        list->setFont(font);
	}
	delete reg_xfe;
				
	// Get toolbar status
    if(getApp()->reg().readUnsignedEntry("OPTIONS","showtoolbar",TRUE)==FALSE)
        toolbar->hide();

    // Get size
    unsigned int width=getApp()->reg().readUnsignedEntry("OPTIONS","width",DEFAULT_WINDOW_WIDTH);
    unsigned int height=getApp()->reg().readUnsignedEntry("OPTIONS","height",DEFAULT_WINDOW_HEIGHT);

	// Get position and position window
    if (save_win_pos)
	{
		int xpos=getApp()->reg().readIntEntry("OPTIONS","xpos",DEFAULT_WINDOW_XPOS);
		int ypos=getApp()->reg().readIntEntry("OPTIONS","ypos",DEFAULT_WINDOW_YPOS);
		position(xpos,ypos,width,height);
	}
	else
    	position(getX(),getY(),width,height);

    FXMainWindow::create();

	// This is necessary otherwise Xfp crashes when opening a package, but why?
	minifoldericon->create();
	minifolderopenicon->create();
	minidocicon->create();

    show();
    description->handle(this,FXSEL(SEL_COMMAND,FXText::ID_TOGGLE_EDITABLE),NULL);

#ifdef STARTUP_NOTIFICATION
	startup_completed();
#endif
}



// Usage message
#define USAGE_MSG	_("\
\nUsage: xfp [options] [package] \n\
\n\
    [options] can be any of the following:\n\
\n\
        -h, --help         Print (this) help screen and exit.\n\
        -v, --version      Print version information and exit.\n\
\n\
    [package] is the path to the rpm or deb package you want to open on start up.\n\
\n")



// Main function to start the program
int main(int argc,char* argv[])
{
	int i;
	FXString startpkg="";
	const char *appname = "xfp";
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
	
	// Parse basic arguments
	for(i = 1; i < argc; ++i)
	{
    	if(compare(argv[i],"-v")==0 || compare(argv[i],"--version")==0)
		{
			fprintf(stdout,"%s version %s\n",PACKAGE,VERSION);
			exit(0);
		}
    	else if(compare(argv[i],"-h")==0 || compare(argv[i],"--help")==0)
		{
			fprintf(stdout,USAGE_MSG);
			exit(0);
		}
		else
	    {
			// Start file, if any
			startpkg=argv[i];
	    }
	}

	// Global variable
	args=argv;

    // Make application
    FXApp* application=new FXApp(appname,vdrname);

	// Open display
    application->init(argc,argv);

    // Redefine the default hand cursor
    FXCursor* hand=new FXCursor(application,hand_bits,hand_mask_bits,hand_width,hand_height,hand_x_hot,hand_y_hot);
    application->setDefaultCursor(DEF_HAND_CURSOR,hand);

	// Load all application icons
	loadicons=loadAppIcons(application);
	
    // Read the Xfe registry
	FXRegistry *reg_xfe;
  	reg_xfe=new FXRegistry(xfename,vdrname);
	reg_xfe->read();

	// Set base color (to change the default base color at first run)
	FXColor basecolor=reg_xfe->readColorEntry("SETTINGS","basecolor",FXRGB(237,233,227));
	application->setBaseColor(basecolor);

	// Set Xfp normal font according to the Xfe registry
	FXString fontspec;
	fontspec=reg_xfe->readStringEntry("SETTINGS","font","Helvetica,100,normal,regular");
	if(!fontspec.empty())
	{
    	FXFont* normalFont=new FXFont(application,fontspec);
		normalFont->create();
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
	
    // Get value of the window position flag
	save_win_pos=reg_xfe->readUnsignedEntry("SETTINGS","save_win_pos",FALSE);

	// Delete the Xfe registry
	delete reg_xfe;

	// Make window
	XFilePackage* window=new XFilePackage(application);

 	// Catch SIGCHLD to harvest zombie child processes
	application->addSignal(SIGCHLD,window,XFilePackage::ID_HARVEST,TRUE);

	// Smooth scrolling
	window->setSmoothScroll(smoothscroll);

	// Create it
    application->create();
	if (!loadicons)
		 MessageBox::error(application,BOX_OK,_("Error loading icons"),_("Unable to load some icons. Please check your icons path!"));
	
	// Tooltips setup time and duration
	application->setTooltipPause(TOOLTIP_PAUSE);
	application->setTooltipTime(TOOLTIP_TIME);
	
    // Test the existence of the Debian package manager (dpkg)
	// This is done by checking the existence of the string 'Debian'
	// in the response to the command 'dpkg' (with --version argument)
	FXString cmd="dpkg --version 2>&1";
    FILE *debcmd=popen(cmd.text(),"r");
    if(!debcmd)
    {
        perror("popen");
        exit(1);
    }
    char textdeb[10000]={0};
    FXString bufdeb;
    while(fgets(textdeb,sizeof(textdeb),debcmd))
        bufdeb+=textdeb;
    snprintf(textdeb,sizeof(textdeb)-1,"%s",bufdeb.text());
	if (strstr(textdeb,"Debian")!=NULL)
		dpkg=TRUE;
	pclose(debcmd);
	
    // Test the existence of the RedHat package manager (rpm)
	// This is done by checking the existence of the string 'RPM'
	// in the response to the command 'rpm --version'
	cmd="rpm --version 2>&1";
    FILE *rpmcmd=popen(cmd.text(),"r");
    if(!rpmcmd)
    {
        perror("popen");
        exit(1);
    }
    char textrpm[10000]={0};
    FXString bufrpm;
    while(fgets(textrpm,sizeof(textrpm),rpmcmd))
        bufrpm+=textrpm;
    snprintf(textrpm,sizeof(textrpm)-1,"%s",bufrpm.text());
	if (strstr(textrpm,"RPM")!=NULL)
		rpm=TRUE;
	pclose(rpmcmd);
	
	// No package manager was found
	if ((dpkg==FALSE) && (rpm==FALSE))
	{
        MessageBox::error(window,BOX_OK,_("Error"),_("No compatible package manager (rpm or dpkg) found!"));
		exit(1);
	}

    // Start
    window->start(startpkg);

    // Run the application
    application->run();

    return 0;
}
