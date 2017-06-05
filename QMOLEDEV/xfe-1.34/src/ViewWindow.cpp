// This is adapted from 'textedit', a demo text viewer found
// in the early versions of the FOX library and written
// by Jeroen van der Zijp.

#include "config.h"
#include "i18n.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <unistd.h>

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "startupnotification.h"
#include "FontDialog.h"
#include "InputDialog.h"
#include "FileDialog.h"
#include "MessageBox.h"
#include "ViewWindow.h"
#include "XFileView.h"

FXbool save_win_pos;

// Map
FXDEFMAP(ViewWindow) ViewWindowMap[]=
{
	FXMAPFUNC(SEL_COMMAND,ViewWindow::ID_ABOUT,ViewWindow::onCmdAbout),
	FXMAPFUNC(SEL_COMMAND,ViewWindow::ID_OPEN,ViewWindow::onCmdOpen),
	FXMAPFUNC(SEL_COMMAND,ViewWindow::ID_OPEN_RECENT,ViewWindow::onCmdOpenRecent),
	FXMAPFUNC(SEL_UPDATE,0,ViewWindow::onUpdTitle),
	FXMAPFUNC(SEL_COMMAND,ViewWindow::ID_FIND,ViewWindow::onCmdFind),
	FXMAPFUNC(SEL_COMMAND,ViewWindow::ID_FIND_AGAIN,ViewWindow::onCmdFind),
	FXMAPFUNC(SEL_SIGNAL,ViewWindow::ID_HARVEST,ViewWindow::onSigHarvest),
	FXMAPFUNC(SEL_COMMAND,ViewWindow::ID_QUIT,ViewWindow::onCmdQuit),
	FXMAPFUNC(SEL_COMMAND,ViewWindow::ID_PRINT,ViewWindow::onCmdPrint),
	FXMAPFUNC(SEL_COMMAND,ViewWindow::ID_FONT,ViewWindow::onCmdFont),
	FXMAPFUNC(SEL_UPDATE,ViewWindow::ID_FIND_AGAIN,ViewWindow::onCmdFind),
	FXMAPFUNC(SEL_UPDATE,ViewWindow::ID_FIND,ViewWindow::onUpdMenu),
	FXMAPFUNC(SEL_UPDATE,ViewWindow::ID_PRINT,ViewWindow::onUpdMenu),
	FXMAPFUNCS(SEL_UPDATE,ViewWindow::ID_WINDOW_1,ViewWindow::ID_WINDOW_50,ViewWindow::onUpdWindow),
	FXMAPFUNCS(SEL_COMMAND,ViewWindow::ID_WINDOW_1,ViewWindow::ID_WINDOW_50,ViewWindow::onCmdWindow),
};


// Object implementation
FXIMPLEMENT(ViewWindow,FXMainWindow,ViewWindowMap,ARRAYNUMBER(ViewWindowMap))


// Make some windows
ViewWindow::ViewWindow(XFileView* a,const FXString& file):FXMainWindow(a,"XFileView",NULL,NULL,DECOR_ALL),mrufiles(a)
{
    FXHotKey hotkey;
	FXString key;

    // Add to list of windows
    getApp()->windowlist.append(this);
	
	// Icon
	setIcon(xfvicon);
	
    // Default font
    font=NULL;

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

    // Status bar
    statusbar=new FXStatusBar(this,FRAME_RAISED|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
	new FXDragCorner(statusbar);

    // File menu
    filemenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&File"),NULL,filemenu);

    // Edit menu
    editmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Edit"),NULL,editmenu);

	// Search Menu
    searchmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Search"),NULL,searchmenu);

    // Preferences Menu
    prefsmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Preferences"),NULL,prefsmenu);

    // Window menu
    windowmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Window"),NULL,windowmenu);

	// Help menu
    helpmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Help"),NULL,helpmenu);

	// Make contents
    contents=new FXHorizontalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED,0,0,0,0, 0,0,0,0, 0,0);

    // Make viewer window
    viewer=new FXText(contents,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);

    // Show column number in status bar
    FXTextField* colno=new FXTextField(statusbar,4,viewer,FXText::ID_CURSOR_COLUMN,TEXTFIELD_READONLY|FRAME_SUNKEN|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,0,0,0,0);
	colno->setBackColor(statusbar->getBackColor());
    
    // Caption before number
    new FXLabel(statusbar,_("Col:"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);

    // Show line number in status bar
    FXTextField* rowno=new FXTextField(statusbar,4,viewer,FXText::ID_CURSOR_ROW,TEXTFIELD_READONLY|FRAME_SUNKEN|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,0,0,0,0);
	rowno->setBackColor(statusbar->getBackColor());

    // Caption before number
    new FXLabel(statusbar,_("Line:"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);

    // Toolbar button: File open
	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
    new FXButton(toolbar,TAB+_("Open")+PARS(key)+TAB+_("Open document.")+PARS(key),fileopenicon,this,ID_OPEN,style|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // Toolbar button: File close
	key=getApp()->reg().readStringEntry("KEYBINDINGS","close","Ctrl-W");
    new FXButton(toolbar,TAB+_("Close")+PARS(key)+TAB+_("Close document.")+PARS(key),closefileicon,this,ID_CLOSE,style|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // Toolbar button: Print
    new FXSeparator(toolbar,SEPARATOR_GROOVE);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","print","Ctrl-P");
	new FXButton(toolbar,TAB+_("Print")+PARS(key)+TAB+_("Print document.")+PARS(key),printicon,this,ID_PRINT,style|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	// Toolbar button: Quit
	key=getApp()->reg().readStringEntry("KEYBINDINGS","quit","Ctrl-Q");
    new FXButton(toolbar,TAB+_("Quit")+PARS(key)+TAB+_("Quit Xfv.")+PARS(key),quiticon,this,ID_QUIT,style|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
	getAccelTable()->addAccel(KEY_Escape,this,FXSEL(SEL_COMMAND,ID_QUIT));

	// Toolbar button: Copy
    new FXSeparator(toolbar,SEPARATOR_GROOVE);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy","Ctrl-C");
    new FXButton(toolbar,TAB+_("Copy")+PARS(key)+TAB+_("Copy selection to clipboard.")+PARS(key),copy_clpicon,viewer,FXText::ID_COPY_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	// Toolbar buttons: Searching
    new FXSeparator(toolbar,SEPARATOR_GROOVE);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","search","Ctrl-F");
	new FXButton(toolbar,TAB+_("Find")+PARS(key)+TAB+_("Find string in document.")+PARS(key),searchicon,this,ID_FIND,style|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
	
	key=getApp()->reg().readStringEntry("KEYBINDINGS","search_next","Ctrl-G");
	new FXButton(toolbar,TAB+_("Find again")+PARS(key)+TAB+_("Find string again.")+PARS(key),find_againicon,this,ID_FIND_AGAIN,style|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // File Menu entries
	FXMenuCommand* mc = NULL;
	FXString text;

	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
	text=_("&Open...")+TABS(key)+_("Open document.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,fileopenicon,this,ID_OPEN);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
 
	key=getApp()->reg().readStringEntry("KEYBINDINGS","close","Ctrl-W");
	text=_("&Close")+TABS(key)+_("Close document.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,closefileicon,this,ID_CLOSE);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","print","Ctrl-P");
	text=_("&Print...")+TABS(key)+_("Print document.")+PARS(key);
	mc=new FXMenuCommand(filemenu,text,printicon,this,ID_PRINT);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
 
    // Recent file menu; this automatically hides if there are no files
    FXMenuSeparator* sep1=new FXMenuSeparator(filemenu);
    sep1->setTarget(&mrufiles);
    sep1->setSelector(FXRecentFiles::ID_ANYFILES);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_1);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_2);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_3);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_4);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_5);
    new FXMenuCommand(filemenu,_("&Clear Recent Files"),NULL,&mrufiles,FXRecentFiles::ID_CLEAR);
    FXMenuSeparator* sep2=new FXMenuSeparator(filemenu);
    sep2->setTarget(&mrufiles);
    sep2->setSelector(FXRecentFiles::ID_ANYFILES);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","quit","Ctrl-Q");
	text=_("&Quit")+TABS(key)+_("Quit Xfv.")+PARS(key);
	mc=new FXMenuCommand(filemenu,text,quiticon,this,ID_QUIT);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	// Edit menu
	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy","Ctrl-C");
	text=_("&Copy")+TABS(key)+_("Copy selection to clipboard.")+PARS(key);
	mc=new FXMenuCommand(editmenu,text,copy_clpicon,viewer,FXText::ID_COPY_SEL);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	// Search Menu entries
	key=getApp()->reg().readStringEntry("KEYBINDINGS","search","Ctrl-F");
	text=_("&Find...")+TABS(key)+_("Find a string in a document.")+PARS(key);
    mc=new FXMenuCommand(searchmenu,text,searchicon,this,ID_FIND);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","search_again","Ctrl-G");
	text=_("Find &again")+TABS(key)+_("Find string again.")+PARS(key);
    mc=new FXMenuCommand(searchmenu,text,find_againicon,this,ID_FIND_AGAIN);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    // Preferences menu
    new FXMenuCheck(prefsmenu,_("&Toolbar")+TAB2+_("Display toolbar."),toolbar,FXWindow::ID_TOGGLESHOWN);
    new FXMenuCheck(prefsmenu,_("&Status bar")+TAB2+_("Display status bar."),statusbar,FXWindow::ID_TOGGLESHOWN);
    new FXMenuCommand(prefsmenu,_("&Font...")+TAB2+_("Change text font."),fontsicon,this,ID_FONT);

    // Window menu
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_1);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_2);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_3);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_4);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_5);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_6);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_7);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_8);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_9);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_10);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_11);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_12);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_13);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_14);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_15);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_16);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_17);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_18);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_19);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_20);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_21);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_22);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_23);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_24);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_25);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_26);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_27);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_28);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_29);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_30);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_31);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_32);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_33);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_34);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_35);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_36);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_37);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_38);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_39);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_40);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_41);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_42);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_43);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_44);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_45);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_46);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_47);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_48);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_49);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_50);

    // Help Menu entries
	key=getApp()->reg().readStringEntry("KEYBINDINGS","help","F1");
	text=_("&About X File View")+TABS(key)+_("About X File View.")+PARS(key);
    mc=new FXMenuCommand(helpmenu,text,NULL,this,ID_ABOUT,0);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    // Make a tool tip
    new FXToolTip(getApp(),0);

	// Dialogs
	printdialog=NULL;
	finddialog=NULL;
	findtext=NULL;

	// Recent files
    mrufiles.setTarget(this);
    mrufiles.setSelector(ID_OPEN_RECENT);

	// Initialize file name
    filename=file;
    filenameset=FALSE;

	// Initialize window position and size
	fromreg=TRUE;
	ww=0;
	hh=0;
	xx=0;
	yy=0;
	
	// Initialize search string
	searchstr="";
}


// Create and show window
void ViewWindow::create()
{
    // Read the Xfe registry
	FXRegistry* reg_xfe=new FXRegistry(XFEAPPNAME,"");
	reg_xfe->read();
	
	// Set Xfv text font according to the Xfe registry
	FXString fontspec;
	fontspec=reg_xfe->readStringEntry("SETTINGS","textfont","Helvetica,100,normal,regular");
	if(!fontspec.empty())
	{
    	FXFont* font=new FXFont(getApp(),fontspec);
		font->create();
        viewer->setFont(font);
	}

    // Get value of the window position flag
	save_win_pos=reg_xfe->readUnsignedEntry("SETTINGS","save_win_pos",FALSE);

	delete reg_xfe;

	
    // Get size and position from registry
	if (fromreg)
	{
		ww=getApp()->reg().readUnsignedEntry("OPTIONS","width",DEFAULT_WINDOW_WIDTH);
		hh=getApp()->reg().readUnsignedEntry("OPTIONS","height",DEFAULT_WINDOW_HEIGHT);
	}

	// Get toolbar status
    if(getApp()->reg().readUnsignedEntry("OPTIONS","showtoolbar",TRUE)==FALSE)
        toolbar->hide();
   
	// Get status bar status
    if(getApp()->reg().readUnsignedEntry("OPTIONS","showstatusbar",TRUE)==FALSE)
        statusbar->hide();
   
	// Get position and position window    
    if (save_win_pos && fromreg)
	{
		int xpos=getApp()->reg().readIntEntry("OPTIONS","xpos",DEFAULT_WINDOW_XPOS);
		int ypos=getApp()->reg().readIntEntry("OPTIONS","ypos",DEFAULT_WINDOW_YPOS);
		position(xpos,ypos,ww,hh);
	}
	else
	{
    	position(getX(),getY(),ww,hh);
	}

    FXMainWindow::create();
    filemenu->create();
	editmenu->create();
    searchmenu->create();
    prefsmenu->create();
    windowmenu->create();
    helpmenu->create();
    show(PLACEMENT_DEFAULT);
    viewer->setFocus();
    viewer->handle(this,FXSEL(SEL_COMMAND,FXText::ID_TOGGLE_EDITABLE),NULL);

#ifdef STARTUP_NOTIFICATION
	startup_completed();
#endif
}


// Destructor
ViewWindow::~ViewWindow()
{
    getApp()->windowlist.remove(this);
    delete menubar;
    delete toolbar;
	delete statusbar;
	delete font;
	delete filemenu;
	delete editmenu;
	delete searchmenu;
	delete prefsmenu;
	delete helpmenu;
 	delete windowmenu;
    delete contents;
	delete printdialog;
	delete finddialog;
	delete findtext;
}


// About box
long ViewWindow::onCmdAbout(FXObject*,FXSelector,void*)
{
	FXString msg;
	msg.format(_("X File View Version %s is a simple text viewer.\n\n"),VERSION);
	msg += COPYRIGHT;
    MessageBox about(this,_("About X File View"),msg.text(),xfvicon,BOX_OK|DECOR_TITLE|DECOR_BORDER,
	                 JUSTIFY_CENTER_X|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    about.execute(PLACEMENT_OWNER);

    return 1;
}


// Load file
FXbool ViewWindow::loadFile(const FXString& file)
{
    struct stat info;
    char *text;
    FILE *fp;
    int n;
    if(stat(file.text(),&info)!=0)
    {
        MessageBox::error(this,BOX_OK,_("Error Loading File"),_("Non-existing file: %s"),file.text());
        return FALSE;
    }
    fp=fopen(file.text(),"r");
    if(!fp)
    {
        MessageBox::error(this,BOX_OK,_("Error Loading File"),_("Unable to open file: %s"),file.text());
        return FALSE;
    }

    // Set wait cursor
    getApp()->beginWaitCursor();

    FXMALLOC(&text,char,info.st_size+1);
    n=fread(text,1,info.st_size,fp);
    if(n!=info.st_size)
    {
        MessageBox::error(this,BOX_OK,_("Error Reading File"),_("Unable to load entire file: %s"),file.text());
		getApp()->endWaitCursor();
       return FALSE;
    }
    else
    {
        text[info.st_size]='\0';
        viewer->setText(text);
        viewer->setModified(FALSE);
    }
    FXFREE(&text);
    fclose(fp);

    // Kill wait cursor
    getApp()->endWaitCursor();

    mrufiles.appendFile(file);
    filenameset=TRUE;
	filename=file;
    return TRUE;
}


// Open
long ViewWindow::onCmdOpen(FXObject*,FXSelector,void*)
{
    const char *patterns[]=
        {
            _("All Files"),          "*",
            _("Text Files"),         "*.txt",
            _("C Source Files"),     "*.c",
            _("C++ Source Files"),   "*.cpp",
			_("C++ Source Files"),   "*.cc",
 			_("C++ Source Files"),   "*.cxx",
            _("C/C++ Header Files"), "*.h",
            _("HTML Files"),         "*.html",
            _("HTML Files"),         "*.htm",
            _("PHP Files"),          "*.php",NULL
        };
        		
    FileDialog opendialog(this,_("Open Document"));
	
	opendialog.setSelectMode(SELECTFILE_MULTIPLE);
    opendialog.setPatternList(patterns);
  	opendialog.setDirectory(FXPath::directory(filename));
    if(opendialog.execute())
    {
        FXString* files=opendialog.getFilenames();
		unsigned int i=0;
		while (files[i]!=FXString::null)
		{
			ViewWindow *window=findWindow(files[i]);
			if(!window)
			{
				window=findUnused();
				if(!window)
				{
					// New window
					window=new ViewWindow(getApp(),unique());
					
					// Smooth scrolling
					window->setSmoothScroll(smoothscroll);
					
					// Set the size and position of the new window
					window->fromreg=FALSE;
					window->ww=getWidth();
					window->hh=getHeight();
					window->xx=getX();
					window->yy=getY();
					
					// Create window
					window->create();
				}
				window->loadFile(files[i]);
			}
			window->raise();
			window->setFocus();
			i++;
		}
		delete[] files;
    }

    return 1;
}


// Open recent file
long ViewWindow::onCmdOpenRecent(FXObject*,FXSelector,void* ptr)
{
    FXString file=(const char*)ptr;
    ViewWindow *window=findWindow(file);
    if(!window)
    {
        window=findUnused();
        if(!window)
        {
            window=new ViewWindow(getApp(),unique());
            window->create();
        }
        window->loadFile(file);
    }
    window->raise();
    window->setFocus();
    return 1;
}


// Print the text
long ViewWindow::onCmdPrint(FXObject*,FXSelector,void*)
{
    // Read the current print command from the registry
    FXString printcommand, command;	
	printcommand=getApp()->reg().readStringEntry("OPTIONS","print_command","lpr -P printer");
	
	// Open print dialog filled with the current print command
	int rc=1;
	if (printdialog==NULL)
		printdialog=new InputDialog(this,printcommand,_("Print command: \n(ex: lpr -P <printer>)"),_("Print"),"",printbigicon);    
	printdialog->setText(printcommand);
	printdialog->CursorEnd();
    rc=printdialog->execute(PLACEMENT_CURSOR);
	printcommand=printdialog->getText();
	
	// If cancel was pressed, exit
	if (!rc)
		return 0;
	
	// Write the new print command to the registry
	getApp()->reg().writeStringEntry("OPTIONS","print_command",printcommand.text());
	
	// Perform the print command
	command =  "cat " + filename + " |" + printcommand + " &";
	system(command.text());
	
    return 1;
}


// Change text font
long ViewWindow::onCmdFont(FXObject*,FXSelector,void*)
{
    FontDialog fontdlg(this,_("Change Font"),DECOR_BORDER|DECOR_TITLE);
    FXFontDesc fontdesc;
    viewer->getFont()->getFontDesc(fontdesc);
    fontdlg.setFontSelection(fontdesc);
    if(fontdlg.execute())
    {
        FXFont *oldfont=font;
        fontdlg.getFontSelection(fontdesc);
        font=new FXFont(getApp(),fontdesc);
        font->create();
        viewer->setFont(font);
        delete oldfont;
    }
	saveConfig();
    return 1;
}


// Save configuration when quitting
void ViewWindow::saveConfig()
{
    FXString fontspec;

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
	
    // Toolbar status
    if(toolbar->shown())
        getApp()->reg().writeUnsignedEntry("OPTIONS","showtoolbar",TRUE);
    else
        getApp()->reg().writeUnsignedEntry("OPTIONS","showtoolbar",FALSE);

    // Status bar status
    if(statusbar->shown())
        getApp()->reg().writeUnsignedEntry("OPTIONS","showstatusbar",TRUE);
    else
        getApp()->reg().writeUnsignedEntry("OPTIONS","showstatusbar",FALSE);

    // Font
    fontspec=viewer->getFont()->getFont();
    getApp()->reg().writeStringEntry("OPTIONS","textfont",fontspec.text());

	// Write registry options
	getApp()->reg().write();
}


// Harvest the zombies
long ViewWindow::onSigHarvest(FXObject*,FXSelector,void*)
{
	while(waitpid(-1,NULL,WNOHANG)>0);
	return 1;
}


// Quit
long ViewWindow::onCmdQuit(FXObject* sender,FXSelector sel,void* ptr)
{
	// Save options
    saveConfig();

    // Quit
    getApp()->exit(0);
    return 1;
}


// Update title
long ViewWindow::onUpdTitle(FXObject* sender,FXSelector sel,void* ptr)
{
    FXMainWindow::onUpdate(sender,sel,ptr);
    FXString title=FXPath::name(getFilename());
    FXString directory=FXPath::directory(getFilename());
    if(!directory.empty())
		title+=" - " + directory;
    setTitle(title);
    return 1;
}


// Update open, print and find menus
long ViewWindow::onUpdMenu(FXObject* sender,FXSelector,void*)
{
    if (viewer->getLength()==0)
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
	else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
    return 1;
}


// Open find dialog
long ViewWindow::onCmdFind(FXObject* obj,FXSelector sel,void* ptr)
{
    int loc;

    if(FXSELTYPE(sel)==SEL_UPDATE)
    {
        if(searchstr=="")
            ((FXWindow*)obj)->disable();
        else
            ((FXWindow*)obj)->enable();
        return 1;
    }

    if(FXSELID(sel)==ID_FIND)
    {
        if (finddialog==NULL)
		{
			finddialog=new DialogBox(this,_("Find"),DECOR_TITLE|DECOR_BORDER);
			FXVerticalFrame* content=new FXVerticalFrame(finddialog,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,10,10,10,10,10,10);
			findtext=new FXTextField(content,30,NULL,0,FRAME_SUNKEN|LAYOUT_FILL_X);
			new FXHorizontalSeparator(content,SEPARATOR_GROOVE|LAYOUT_FILL_X);
			FXHorizontalFrame* buttons=new FXHorizontalFrame(content,LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0,0,0,0,0);
			new FXButton(buttons,_("&Cancel"),NULL,finddialog,DialogBox::ID_CANCEL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,20,20);
			new FXButton(buttons,_("&Find"),NULL,finddialog,DialogBox::ID_ACCEPT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT);
		}
		findtext->setText(searchstr);
		findtext->setSelection(0,searchstr.length());
		findtext->setFocus();

        if(!finddialog->execute())
            return 0;
        searchstr=findtext->getText();
    }
    loc=find(searchstr,viewer->getCursorPos());
    if(loc==-1)
        MessageBox::information(this,BOX_OK,_("Not Found"),_("String '%s' not found"),searchstr.text());
    else
    {
        viewer->setCursorPos(loc+searchstr.length());
        viewer->setTopLine(loc);
        viewer->setSelection(loc,searchstr.length());
        viewer->setFocus();
    }
    return 1;
}


// Find string
int ViewWindow::find(const FXString str,int pos)
{
    FXString buf=viewer->getText();
    const char* text=buf.text();
    const char *p = strstr(&text[pos],str.text());
    if(!p)
        return -1;
    return (p-text);
}


// Close window
FXbool ViewWindow::close(FXbool notify)
{
	// Save options 
    saveConfig();

    return FXMainWindow::close(notify);
}


// User clicks on one of the window menus
long ViewWindow::onCmdWindow(FXObject*,FXSelector sel,void*)
{
    int which=FXSELID(sel)-ID_WINDOW_1;
    if(which<getApp()->windowlist.no())
    {
		getApp()->windowlist[which]->raise();
        getApp()->windowlist[which]->setFocus();
    }
    return 1;
}


// Update handler for window menus
long ViewWindow::onUpdWindow(FXObject *sender,FXSelector sel,void*)
{
    int which=FXSELID(sel)-ID_WINDOW_1;

    if(which<getApp()->windowlist.no())
    {
        ViewWindow *window=getApp()->windowlist[which];
        FXString string;
        if(which<49)
            string.format("&%d %s",which+1,window->getTitle().text());
        else
            string.format("5&0 %s",window->getTitle().text());

        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);

		if(window==getApp()->getActiveWindow())
            sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_CHECK),NULL);
        else
           sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK),NULL);
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SHOW),NULL);
    }
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
    return 1;
}


// Generate unique name for a new window
FXString ViewWindow::unique() const
{
    FXString name=_("untitled");
    for(int i=1; i<2147483647; i++)
    {
        if(!findWindow(name))
        	break;
        name.format(_("untitled%d"),i);
    }
    return name;
}


// Find an as yet untitled, unedited window
ViewWindow *ViewWindow::findUnused() const
{
    for(int w=0; w<getApp()->windowlist.no(); w++)
    {
		if(!getApp()->windowlist[w]->isFilenameSet())
            return getApp()->windowlist[w];
    }
    return NULL;
}


// Find window, if any, currently editing the given file
ViewWindow *ViewWindow::findWindow(const FXString& file) const
{
    for(int w=0; w<getApp()->windowlist.no(); w++)
    {
        if(getApp()->windowlist[w]->getFilename()==file)
			return getApp()->windowlist[w];
    }
    return NULL;
}
