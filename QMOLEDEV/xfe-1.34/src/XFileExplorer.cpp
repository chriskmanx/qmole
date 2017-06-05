#include "config.h"
#include "i18n.h"

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>

#include <xvt/xvt.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "startupnotification.h"
#include "File.h"
#include "FileList.h"
#include "FileDict.h"
#include "Preferences.h"
#include "FilePanel.h"
#include "InputDialog.h"
#include "HistInputDialog.h"
#include "DirPanel.h"
#include "MessageBox.h"
#include "TextWindow.h"
#include "CommandWindow.h"
#include "Bookmarks.h"
#include "FileDialog.h"
#include "help.h"
#include "DirHistBox.h"
#include "XFileExplorer.h"


// Size of the location bar
#define LOCATION_BAR_LENGTH 60
#define LOCATION_BAR_HEIGHT 6


// Global variables
FXString clipboard="";
char OpenHistory[OPEN_HIST_SIZE][MAX_COMMAND_SIZE];
int OpenNum;
char FilterHistory[FILTER_HIST_SIZE][MAX_PATTERN_SIZE];
int FilterNum;

FXbool allowPopupScroll=FALSE;
unsigned int single_click;
FXbool file_tooltips;
FXbool relative_resize;
//FXbool save_win_pos;


// External global variables
extern char** args;
extern int panel_mode;
extern FXString homedir;
extern FXString xdgdatahome;
extern FXString xdgconfighome;

// Global options
#if defined(linux)
extern FXStringDict* fsdevices;
extern FXStringDict* updevices;
#endif



// Helper function to draw a toolbar separator
void toolbarSeparator(FXToolBar* tb)
{
#define SEP_SPACE	1
#define SEP_HEIGHT	15
    new FXFrame(tb,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,SEP_SPACE);
    new FXVerticalSeparator(tb,LAYOUT_SIDE_TOP|LAYOUT_CENTER_Y|SEPARATOR_GROOVE|LAYOUT_FIX_HEIGHT,0,0,0,SEP_HEIGHT);
    new FXFrame(tb,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,SEP_SPACE);
}



// Map
FXDEFMAP(XFileExplorer) XFileExplorerMap[]=
{
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_TOGGLE_STATUS,XFileExplorer::onCmdToggleStatus),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_RUN,XFileExplorer::onCmdRun),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_SU,XFileExplorer::onCmdSu),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_COPY,XFileExplorer::onCmdFileCopyClp),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_CUT,XFileExplorer::onCmdFileCutClp),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_PASTE,XFileExplorer::onCmdFilePasteClp),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_RENAME,XFileExplorer::onCmdFileRename),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_COPYTO,XFileExplorer::onCmdFileCopyto),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_MOVETO,XFileExplorer::onCmdFileMoveto),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_SYMLINK,XFileExplorer::onCmdFileSymlink),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_PROPERTIES,XFileExplorer::onCmdFileProperties),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_DELETE,XFileExplorer::onCmdFileDelete),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_TRASH,XFileExplorer::onCmdFileTrash),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_RESTORE,XFileExplorer::onCmdFileRestore),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_QUIT,XFileExplorer::onQuit),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_HELP,XFileExplorer::onCmdHelp),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_ABOUT,XFileExplorer::onCmdAbout),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_FILE_ASSOC,XFileExplorer::onCmdFileAssoc),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_REFRESH,XFileExplorer::onCmdRefresh),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_EMPTY_TRASH,XFileExplorer::onCmdEmptyTrash),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_TRASH_SIZE,XFileExplorer::onCmdTrashSize),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_XTERM,XFileExplorer::onCmdXTerm),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_CLEAR_LOCATION,XFileExplorer::onCmdClearLocation),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_GOTO_LOCATION,XFileExplorer::onCmdGotoLocation),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_PREFS,XFileExplorer::onCmdPrefs),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_SHOW_ONE_PANEL,XFileExplorer::onCmdShowPanels),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_SHOW_TWO_PANELS,XFileExplorer::onCmdShowPanels),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_SHOW_TREE_PANEL,XFileExplorer::onCmdShowPanels),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_SHOW_TREE_TWO_PANELS,XFileExplorer::onCmdShowPanels),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_SYNCHRONIZE_PANELS,XFileExplorer::onCmdSynchronizePanels),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_SWITCH_PANELS,XFileExplorer::onCmdSwitchPanels),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_RESTART,XFileExplorer::onCmdRestart),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_NEW_WIN,XFileExplorer::onCmdNewWindow),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_BOOKMARK,XFileExplorer::onCmdBookmark),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_DIR_UP,XFileExplorer::onCmdDirUp),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_ADD_BOOKMARK,XFileExplorer::onCmdBookmark),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_DIR_BACK,XFileExplorer::onCmdDirBack),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_DIR_FORWARD,XFileExplorer::onCmdDirForward),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_DIR_BACK_HIST,XFileExplorer::onCmdDirBackHist),
	FXMAPFUNC(SEL_COMMAND,XFileExplorer::ID_DIR_FORWARD_HIST,XFileExplorer::onCmdDirForwardHist),
	FXMAPFUNC(SEL_KEYPRESS,0,XFileExplorer::onKeyPress),
	FXMAPFUNC(SEL_SIGNAL,XFileExplorer::ID_HARVEST,XFileExplorer::onSigHarvest),
	FXMAPFUNC(SEL_CLOSE,0,XFileExplorer::onQuit),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_DIR_FORWARD_HIST,XFileExplorer::onUpdDirForwardHist),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_DIR_BACK_HIST,XFileExplorer::onUpdDirBackHist),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_DIR_BACK,XFileExplorer::onUpdDirBack),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_DIR_FORWARD,XFileExplorer::onUpdDirForward),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_TOGGLE_STATUS,XFileExplorer::onUpdToggleStatus),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_SHOW_ONE_PANEL,XFileExplorer::onUpdShowPanels),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_SHOW_TWO_PANELS,XFileExplorer::onUpdShowPanels),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_SHOW_TREE_PANEL,XFileExplorer::onUpdShowPanels),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_SHOW_TREE_TWO_PANELS,XFileExplorer::onUpdShowPanels),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_GOTO_LOCATION,XFileExplorer::onUpdFileLocation),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_EMPTY_TRASH,XFileExplorer::onUpdEmptyTrash),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_EMPTY_TRASH,XFileExplorer::onUpdTrashSize),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_DELETE,XFileExplorer::onUpdFileDelete),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_TRASH,XFileExplorer::onUpdFileTrash),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_RESTORE,XFileExplorer::onUpdFileRestore),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_RENAME,XFileExplorer::onUpdFileRename),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_PROPERTIES,XFileExplorer::onUpdFileMan),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_COPYTO,XFileExplorer::onUpdFileMan),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_MOVETO,XFileExplorer::onUpdFileMan),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_SYMLINK,XFileExplorer::onUpdFileMan),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_COPY,XFileExplorer::onUpdFileMan),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_CUT,XFileExplorer::onUpdFileMan),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_FILE_PASTE,XFileExplorer::onUpdFilePaste),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_SYNCHRONIZE_PANELS,XFileExplorer::onUpdSynchronizePanels),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_SWITCH_PANELS,XFileExplorer::onUpdSwitchPanels),
	FXMAPFUNC(SEL_UPDATE,XFileExplorer::ID_SU,XFileExplorer::onUpdSu),
};


// Object implementation
FXIMPLEMENT(XFileExplorer,FXMainWindow,XFileExplorerMap,ARRAYNUMBER(XFileExplorerMap))


// Make some windows
XFileExplorer::XFileExplorer(FXApp *app, const FXbool iconic, const FXbool maximized, const FXString startdir1, const FXString startdir2, const char *title,FXIcon *bigicon,FXIcon *miniicon):
        FXMainWindow(app,title,bigicon,miniicon,DECOR_ALL)
{
    bookmarks=new Bookmarks("bookmarks",this,ID_BOOKMARK);

    // Menu bar
    menubar=new FXMenuBar(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);

	// Site where to dock (for toolbars)
	FXDockSite* topdock=new FXDockSite(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
	
    // General toolbar
  	FXToolBarShell* dragshell1=new FXToolBarShell(this,FRAME_RAISED);
  	generaltoolbar=new FXToolBar(topdock,dragshell1,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_Y);
  	new FXToolBarGrip(generaltoolbar,generaltoolbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

    // Tools toolbar
  	FXToolBarShell* dragshell2=new FXToolBarShell(this,FRAME_RAISED);
  	toolstoolbar=new FXToolBar(topdock,dragshell2,LAYOUT_DOCK_SAME|LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_Y);
  	new FXToolBarGrip(toolstoolbar,toolstoolbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

	// Panel toolbar
  	FXToolBarShell* dragshell3=new FXToolBarShell(this,FRAME_RAISED);
  	paneltoolbar=new FXToolBar(topdock,dragshell3,LAYOUT_DOCK_SAME|LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  	new FXToolBarGrip(paneltoolbar,paneltoolbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

	// Location bar
  	FXToolBarShell* dragshell4=new FXToolBarShell(this,FRAME_RAISED);
  	locationbar=new FXToolBar(topdock,dragshell4,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  	new FXToolBarGrip(locationbar,locationbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

    // Splitter
	FXHorizontalFrame* splitterbox=new FXHorizontalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED,0,0,0,0, 0,0,0,0, 0,0);
	FXSplitter* mainsplit=new FXSplitter(splitterbox,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|SPLITTER_TRACKING|FRAME_NONE);
	
	// File list background, foreground, highlight, progress bar and attention colors
	listbackcolor=getApp()->reg().readColorEntry("SETTINGS","listbackcolor",FXRGB(255,255,255));
	listforecolor=getApp()->reg().readColorEntry("SETTINGS","listforecolor",FXRGB(0,0,0));
	highlightcolor=getApp()->reg().readColorEntry("SETTINGS","highlightcolor",FXRGB(238,238,238));
	pbarcolor=getApp()->reg().readColorEntry("SETTINGS","pbarcolor",FXRGB(0,0,255));
	attentioncolor=getApp()->reg().readColorEntry("SETTINGS","attentioncolor",FXRGB(255,0,0));
	scrollbarcolor=getApp()->reg().readColorEntry("SETTINGS","scrollbarcolor",FXRGB(237,233,227));
	
	// Smooth scrolling
	smoothscroll=getApp()->reg().readUnsignedEntry("SETTINGS","smooth_scroll",TRUE);

	// Directory panel on the left (with minimum size)
    dirpanel=new DirPanel(this,mainsplit,listbackcolor,listforecolor,smoothscroll,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_NONE,0,0,0,0);

	// File panels on the right : remembers size of each field
	lpanel=new FilePanel(this,"LEFT PANEL",mainsplit,dirpanel,
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","name_size",200),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","size_size",60),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","type_size",100),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","ext_size",100),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","modd_size",150),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","user_size",50),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","grou_size",50),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","attr_size",100),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","deldate_size",150),
                     getApp()->reg().readUnsignedEntry("LEFT PANEL","origpath_size",200),
					 getApp()->reg().readUnsignedEntry("LEFT PANEL","showthumbnails",0),
					 listbackcolor,listforecolor,attentioncolor,smoothscroll,
					 LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_NONE,0,0,0,0);
    rpanel=new FilePanel(this,"RIGHT PANEL",mainsplit,dirpanel,
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","name_size",200),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","size_size",60),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","type_size",100),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","ext_size",100),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","modd_size",150),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","user_size",50),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","grou_size",50),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","attr_size",100),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","deldate_size",150),
                     getApp()->reg().readUnsignedEntry("RIGHT PANEL","origpath_size",200),
					 getApp()->reg().readUnsignedEntry("RIGHT PANEL","showthumbnails",0),
					 listbackcolor,listforecolor,attentioncolor,smoothscroll,
					 LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_NONE,0,0,0,0);

    lpanel->Next(rpanel);
    rpanel->Next(lpanel);
    lpanel->setActive();

	FXString sort_func;
	
	// Dir panel options
	sort_func=getApp()->reg().readStringEntry("DIR PANEL","sort_func","ascendingCase");
    if (sort_func=="ascendingCase")
		dirpanel->setSortFunc(DirList::ascendingCase);        
    else if (sort_func=="descendingCase")
		dirpanel->setSortFunc(DirList::descendingCase);        
    else if (sort_func=="ascending")
		dirpanel->setSortFunc(DirList::ascending);        
    else if (sort_func=="descending")
		dirpanel->setSortFunc(DirList::descending);        

	// Left panel options
	sort_func=getApp()->reg().readStringEntry("LEFT PANEL","sort_func","ascendingCase");
    if (sort_func=="ascendingCase")
		lpanel->setSortFunc(FileList::ascendingCase);        
    else if (sort_func=="ascendingCaseMix")
		lpanel->setSortFunc(FileList::ascendingCaseMix);        
    else if (sort_func=="descendingCase")
		lpanel->setSortFunc(FileList::descendingCase);        
    else if (sort_func=="descendingCaseMix")
		lpanel->setSortFunc(FileList::descendingCaseMix);        
    else if (sort_func=="ascending")
		lpanel->setSortFunc(FileList::ascending);        
	else if (sort_func=="ascendingMix")
		lpanel->setSortFunc(FileList::ascendingMix);        
    else if (sort_func=="descending")
		lpanel->setSortFunc(FileList::descending);        
    else if (sort_func=="descendingMix")
		lpanel->setSortFunc(FileList::descendingMix);        
	else if (sort_func=="ascendingSize")
		lpanel->setSortFunc(FileList::ascendingSize);        
	else if (sort_func=="ascendingSizeMix")
		lpanel->setSortFunc(FileList::ascendingSizeMix);        
	else if (sort_func=="descendingSize")
		lpanel->setSortFunc(FileList::descendingSize);        
	else if (sort_func=="descendingSizeMix")
		lpanel->setSortFunc(FileList::descendingSizeMix);        
	else if (sort_func=="ascendingType")
		lpanel->setSortFunc(FileList::ascendingType);        
	else if (sort_func=="ascendingTypeMix")
		lpanel->setSortFunc(FileList::ascendingTypeMix);        
	else if (sort_func=="descendingType")
		lpanel->setSortFunc(FileList::descendingType);        
	else if (sort_func=="descendingTypeMix")
		lpanel->setSortFunc(FileList::descendingTypeMix);        
	else if (sort_func=="ascendingExt")
		lpanel->setSortFunc(FileList::ascendingExt);        
	else if (sort_func=="ascendingExtMix")
		lpanel->setSortFunc(FileList::ascendingExtMix);        
	else if (sort_func=="descendingExt")
		lpanel->setSortFunc(FileList::descendingExt);        
	else if (sort_func=="descendingExtMix")
		lpanel->setSortFunc(FileList::descendingExtMix);        
	else if (sort_func=="ascendingTime")
		lpanel->setSortFunc(FileList::ascendingTime);        
	else if (sort_func=="ascendingTimeMix")
		lpanel->setSortFunc(FileList::ascendingTimeMix);        
	else if (sort_func=="descendingTime")
		lpanel->setSortFunc(FileList::descendingTime);        
	else if (sort_func=="descendingTimeMix")
		lpanel->setSortFunc(FileList::descendingTimeMix);        
	else if (sort_func=="ascendingUser")
		lpanel->setSortFunc(FileList::ascendingUser);        
	else if (sort_func=="ascendingUserMix")
		lpanel->setSortFunc(FileList::ascendingUserMix);        
	else if (sort_func=="descendingUser")
		lpanel->setSortFunc(FileList::descendingUser);         
	else if (sort_func=="descendingUserMix")
		lpanel->setSortFunc(FileList::descendingUserMix);         
	else if (sort_func=="ascendingGroup")
		lpanel->setSortFunc(FileList::ascendingGroup);        
	else if (sort_func=="ascendingGroupMix")
		lpanel->setSortFunc(FileList::ascendingGroupMix);        
	else if (sort_func=="descendingGroup")
		lpanel->setSortFunc(FileList::descendingGroup);        
	else if (sort_func=="descendingGroupMix")
		lpanel->setSortFunc(FileList::descendingGroupMix);        
	else if (sort_func=="ascendingPerm")
		lpanel->setSortFunc(FileList::ascendingPerm);        
	else if (sort_func=="ascendingPermMix")
		lpanel->setSortFunc(FileList::ascendingPermMix);        
	else if (sort_func=="descendingPerm")
		lpanel->setSortFunc(FileList::descendingPerm);        
	else if (sort_func=="descendingPermMix")
		lpanel->setSortFunc(FileList::descendingPermMix);        
	unsigned int ignore_case=getApp()->reg().readUnsignedEntry("LEFT PANEL","ignore_case",1);
	lpanel->setIgnoreCase(ignore_case);        
	unsigned int dirs_first=getApp()->reg().readUnsignedEntry("LEFT PANEL","dirs_first",1);
	lpanel->setDirsFirst(dirs_first);        

	// Right panel options
	sort_func=getApp()->reg().readStringEntry("RIGHT PANEL","sort_func","ascendingCase");
    if (sort_func=="ascendingCase")
		rpanel->setSortFunc(FileList::ascendingCase);        
    else if (sort_func=="ascendingCaseMix")
		rpanel->setSortFunc(FileList::ascendingCaseMix);        
    else if (sort_func=="descendingCase")
		rpanel->setSortFunc(FileList::descendingCase);        
    else if (sort_func=="descendingCaseMix")
		rpanel->setSortFunc(FileList::descendingCaseMix);        
    else if (sort_func=="ascending")
		rpanel->setSortFunc(FileList::ascending);        
	else if (sort_func=="ascendingMix")
		rpanel->setSortFunc(FileList::ascendingMix);        
    else if (sort_func=="descending")
		rpanel->setSortFunc(FileList::descending);        
    else if (sort_func=="descendingMix")
		rpanel->setSortFunc(FileList::descendingMix);        
	else if (sort_func=="ascendingSize")
		rpanel->setSortFunc(FileList::ascendingSize);        
	else if (sort_func=="ascendingSizeMix")
		rpanel->setSortFunc(FileList::ascendingSizeMix);        
	else if (sort_func=="descendingSize")
		rpanel->setSortFunc(FileList::descendingSize);        
	else if (sort_func=="descendingSizeMix")
		rpanel->setSortFunc(FileList::descendingSizeMix);        
	else if (sort_func=="ascendingType")
		rpanel->setSortFunc(FileList::ascendingType);        
	else if (sort_func=="ascendingTypeMix")
		rpanel->setSortFunc(FileList::ascendingTypeMix);        
	else if (sort_func=="descendingType")
		rpanel->setSortFunc(FileList::descendingType);        
	else if (sort_func=="descendingTypeMix")
		rpanel->setSortFunc(FileList::descendingTypeMix);        
	else if (sort_func=="ascendingExt")
		rpanel->setSortFunc(FileList::ascendingExt);        
	else if (sort_func=="ascendingExtMix")
		rpanel->setSortFunc(FileList::ascendingExtMix);        
	else if (sort_func=="descendingExt")
		rpanel->setSortFunc(FileList::descendingExt);        
	else if (sort_func=="descendingExtMix")
		rpanel->setSortFunc(FileList::descendingExtMix);        
	else if (sort_func=="ascendingTime")
		rpanel->setSortFunc(FileList::ascendingTime);        
	else if (sort_func=="ascendingTimeMix")
		rpanel->setSortFunc(FileList::ascendingTimeMix);        
	else if (sort_func=="descendingTime")
		rpanel->setSortFunc(FileList::descendingTime);        
	else if (sort_func=="descendingTimeMix")
		rpanel->setSortFunc(FileList::descendingTimeMix);        
	else if (sort_func=="ascendingUser")
		rpanel->setSortFunc(FileList::ascendingUser);        
	else if (sort_func=="ascendingUserMix")
		rpanel->setSortFunc(FileList::ascendingUserMix);        
	else if (sort_func=="descendingUser")
		rpanel->setSortFunc(FileList::descendingUser);         
	else if (sort_func=="descendingUserMix")
		rpanel->setSortFunc(FileList::descendingUserMix);         
	else if (sort_func=="ascendingGroup")
		rpanel->setSortFunc(FileList::ascendingGroup);        
	else if (sort_func=="ascendingGroupMix")
		rpanel->setSortFunc(FileList::ascendingGroupMix);        
	else if (sort_func=="descendingGroup")
		rpanel->setSortFunc(FileList::descendingGroup);        
	else if (sort_func=="descendingGroupMix")
		rpanel->setSortFunc(FileList::descendingGroupMix);        
	else if (sort_func=="ascendingPerm")
		rpanel->setSortFunc(FileList::ascendingPerm);        
	else if (sort_func=="ascendingPermMix")
		rpanel->setSortFunc(FileList::ascendingPermMix);        
	else if (sort_func=="descendingPerm")
		rpanel->setSortFunc(FileList::descendingPerm);        
	else if (sort_func=="descendingPermMix")
		rpanel->setSortFunc(FileList::descendingPermMix);        
	ignore_case=getApp()->reg().readUnsignedEntry("RIGHT PANEL","ignore_case",1);
	rpanel->setIgnoreCase(ignore_case);        
	dirs_first=getApp()->reg().readUnsignedEntry("RIGHT PANEL","dirs_first",1);
	rpanel->setDirsFirst(dirs_first);        

    FXButton *btn=NULL;
	FXHotKey hotkey;
	FXString key;

    // General toolbar
	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_back","Ctrl-Backspace");
    btn=new FXButton(generaltoolbar,TAB+_("Go to previous directory")+PARS(key),dirbackicon,this,XFileExplorer::ID_DIR_BACK,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	btnbackhist=new FXArrowButton(generaltoolbar,this,XFileExplorer::ID_DIR_BACK_HIST,LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_TOOLBAR);
	
	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_forward","Shift-Backspace");
    btn=new FXButton(generaltoolbar,TAB+_("Go to next directory")+PARS(key),dirforwardicon,this,XFileExplorer::ID_DIR_FORWARD,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	btnforwardhist=new FXArrowButton(generaltoolbar,this,XFileExplorer::ID_DIR_FORWARD_HIST,LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_TOOLBAR);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_up","Backspace");
	btn=new FXButton(generaltoolbar,TAB+_("Go to parent directory")+PARS(key),dirupicon,this,XFileExplorer::ID_DIR_UP,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	toolbarSeparator(generaltoolbar);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_home","Ctrl-H");
	new FXButton(generaltoolbar,TAB+_("Go to home directory")+PARS(key),homeicon,lpanel,FilePanel::ID_GO_HOME,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
	
	key=getApp()->reg().readStringEntry("KEYBINDINGS","refresh","Ctrl-R");
	new FXButton(generaltoolbar,TAB+_("Refresh panels")+PARS(key),reloadicon,this,XFileExplorer::ID_REFRESH,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	toolbarSeparator(generaltoolbar);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_file","F2");
	new FXButton(generaltoolbar,TAB+_("Create new file")+PARS(key),newfileicon,lpanel,FilePanel::ID_NEW_FILE,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_folder","F7");
    new FXButton(generaltoolbar,TAB+_("Create new folder")+PARS(key),newfoldericon,lpanel,FilePanel::ID_NEW_DIR,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_symlink","Ctrl-J");
    new FXButton(generaltoolbar,TAB+_("Create new symlink")+PARS(key),newlinkicon,lpanel,FilePanel::ID_NEW_SYMLINK,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	toolbarSeparator(generaltoolbar);
	
	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy","Ctrl-C");
	new FXButton(generaltoolbar,TAB+_("Copy selected files to clipboard")+PARS(key),copy_clpicon,this,XFileExplorer::ID_FILE_COPY,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","cut","Ctrl-X");
	new FXButton(generaltoolbar,TAB+_("Cut selected files to clipboard")+PARS(key),cut_clpicon,this,XFileExplorer::ID_FILE_CUT,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","paste","Ctrl-V");
    new FXButton(generaltoolbar,TAB+_("Paste from clipboard")+PARS(key),paste_clpicon,this,XFileExplorer::ID_FILE_PASTE,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","properties","F9");
    new FXButton(generaltoolbar,TAB+_("Show properties of selected files")+PARS(key),attribicon,this,XFileExplorer::ID_FILE_PROPERTIES,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);

	toolbarSeparator(generaltoolbar);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","move_to_trash","Del");
	new FXButton(generaltoolbar,TAB+_("Move selected files to trash can")+PARS(key),filedeleteicon,this,XFileExplorer::ID_FILE_TRASH,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","restore_from_trash","Alt-Del");
	new FXButton(generaltoolbar,TAB+_("Restore selected files from trash can")+PARS(key),filerestoreicon,this,XFileExplorer::ID_FILE_RESTORE,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","delete","Shift-Del");
	new FXButton(generaltoolbar,TAB+_("Delete selected files")+PARS(key),filedelete_permicon,this,XFileExplorer::ID_FILE_DELETE,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);
		
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_window","F3");
   	new FXButton(toolstoolbar,TAB+_("Launch Xfe")+PARS(key),minixfeicon,this,XFileExplorer::ID_NEW_WIN,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_root_window","Shift-F3");
    new FXButton(toolstoolbar,TAB+_("Launch Xfe as root")+PARS(key),minixferooticon,this,XFileExplorer::ID_SU,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","execute_command","Ctrl-E");
    new FXButton(toolstoolbar,TAB+_("Execute command")+PARS(key),runicon,this,XFileExplorer::ID_RUN,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
 
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","terminal","Ctrl-T");
    new FXButton(toolstoolbar,TAB+_("Launch terminal")+PARS(key),shellicon,this,XFileExplorer::ID_XTERM,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

#if defined(linux)
	toolbarSeparator(toolstoolbar);

    // Mount and unmount buttons
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","mount","Ctrl-M");
	btn=new FXButton(toolstoolbar,TAB+_("Mount (Linux only)")+PARS(key),maphosticon,lpanel,FilePanel::ID_MOUNT,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","unmount","Ctrl-U");
    btn=new FXButton(toolstoolbar,TAB+_("Unmount (Linux only)")+PARS(key),unmaphosticon,lpanel,FilePanel::ID_UMOUNT,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);
#endif

	// Panel toolbar
	
	// Show one panel
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","one_panel","Ctrl-F1");
   	btn=new FXButton(paneltoolbar,TAB+_("Show one panel")+PARS(key),onepanelicon,this,XFileExplorer::ID_SHOW_ONE_PANEL,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	// Show tree and panel
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","tree_panel","Ctrl-F2");
   	btn=new FXButton(paneltoolbar,TAB+_("Show tree and panel")+PARS(key),treeonepanelicon,this,XFileExplorer::ID_SHOW_TREE_PANEL,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	// Show two panels
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","two_panels","Ctrl-F3");
   	btn=new FXButton(paneltoolbar,TAB+_("Show two panels")+PARS(key),twopanelsicon,this,XFileExplorer::ID_SHOW_TWO_PANELS,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	// Show tree and two panels
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","tree_two_panels","Ctrl-F4");
   	btn = new FXButton(paneltoolbar,TAB+_("Show tree and two panels")+PARS(key),treetwopanelsicon,this,XFileExplorer::ID_SHOW_TREE_TWO_PANELS,BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	toolbarSeparator(paneltoolbar);

    // Switch display modes
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","big_icons","F10");
    btn=new FXButton(paneltoolbar,TAB+_("Big icon list")+PARS(key),bigiconsicon,lpanel,FilePanel::ID_SHOW_BIG_ICONS,BUTTON_TOOLBAR|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT|FRAME_RAISED);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","small_icons","F11");
    btn=new FXButton(paneltoolbar,TAB+_("Small icon list")+PARS(key),smalliconsicon,lpanel,FilePanel::ID_SHOW_MINI_ICONS,BUTTON_TOOLBAR|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT|FRAME_RAISED);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","detailed_file_list","F12");
    btn=new FXButton(paneltoolbar,TAB+_("Detailed file list")+PARS(key),detailsicon,lpanel,FilePanel::ID_SHOW_DETAILS,BUTTON_TOOLBAR|LAYOUT_TOP|LAYOUT_LEFT|ICON_BEFORE_TEXT|FRAME_RAISED);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

  	// Location bar
  	new FXLabel(locationbar,_("Location:"));
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","clear_location","Ctrl-L");
  	btn = new FXButton(locationbar,TAB+_("Clear location")+PARS(key),locationicon,this,ID_CLEAR_LOCATION,BUTTON_TOOLBAR|LAYOUT_CENTER_Y|LAYOUT_LEFT|ICON_BEFORE_TEXT|FRAME_RAISED);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);
	address=new ComboBox(locationbar,LOCATION_BAR_LENGTH,this,ID_GOTO_LOCATION,COMBOBOX_INSERT_LAST|JUSTIFY_LEFT|LAYOUT_CENTER_Y);
	address->setNumVisible(5);
 	new FXButton(locationbar,TAB+_("Go to location"),entericon,this,ID_GOTO_LOCATION,BUTTON_TOOLBAR|LAYOUT_CENTER_Y|LAYOUT_LEFT|ICON_BEFORE_TEXT|FRAME_RAISED);


    // Menus

    // File menu	
	filemenu=new FXMenuPane(this);
	FXMenuCommand* mc = NULL;

    mc=new FXMenuCommand(filemenu,_("New &file..."),newfileicon,lpanel,FilePanel::ID_NEW_FILE);	
	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_file","F2");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("New fo&lder..."),newfoldericon,lpanel,FilePanel::ID_NEW_DIR);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_folder","F7");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("New s&ymlink..."),newlinkicon,lpanel,FilePanel::ID_NEW_SYMLINK);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_symlink","Ctrl-J");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("Go &home"),homeicon,lpanel,FilePanel::ID_GO_HOME);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_home","Ctrl-H");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("&Refresh"),reloadicon,this,XFileExplorer::ID_REFRESH);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","refresh","Ctrl-R");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(filemenu);

    mc=new FXMenuCommand(filemenu,_("&Open..."),fileopenicon,lpanel,FilePanel::ID_OPEN);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("Re&name..."),renameiticon,this,XFileExplorer::ID_FILE_RENAME);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","rename","Ctrl-N");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("&Copy to..."),copy_clpicon,this,XFileExplorer::ID_FILE_COPYTO);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy_to","F5");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("&Move to..."),moveiticon,this,XFileExplorer::ID_FILE_MOVETO);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","move_to","F6");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("&Symlink to..."),minilinkicon,this,XFileExplorer::ID_FILE_SYMLINK);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","symlink_to","Ctrl-S");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("Mo&ve to trash"),filedeleteicon,this,XFileExplorer::ID_FILE_TRASH);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","move_to_trash","Del");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("R&estore from trash"),filerestoreicon,this,XFileExplorer::ID_FILE_RESTORE);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","restore_from_trash","Alt-Del");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("&Delete"),filedelete_permicon,this,XFileExplorer::ID_FILE_DELETE);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","delete","Shift-Del");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(filemenu,_("&Properties..."),attribicon,this,XFileExplorer::ID_FILE_PROPERTIES);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","properties","F9");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(filemenu);

    mc=new FXMenuCommand(filemenu,_("&Quit"),quiticon,this,XFileExplorer::ID_QUIT);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","quit","Ctrl-Q");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	filemenutitle=new FXMenuTitle(menubar,_("&File"),NULL,filemenu);

	// Edit menu
    editmenu=new FXMenuPane(this);
    
	mc=new FXMenuCommand(editmenu,_("&Copy"),copy_clpicon,this,XFileExplorer::ID_FILE_COPY);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy","Ctrl-C");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(editmenu,_("C&ut"),cut_clpicon,this,XFileExplorer::ID_FILE_CUT);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","cut","Ctrl-X");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(editmenu,_("&Paste"),paste_clpicon,this,XFileExplorer::ID_FILE_PASTE);
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","paste","Ctrl-V");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
   
	new FXMenuSeparator(editmenu);
    
	mc=new FXMenuCommand(editmenu,_("&Select all"),selallicon,lpanel,FilePanel::ID_SELECT_ALL);
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","select_all","Ctrl-A");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
   
	mc=new FXMenuCommand(editmenu,_("&Deselect all"),deselicon,lpanel,FilePanel::ID_DESELECT_ALL);
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","deselect_all","Ctrl-Z");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
    
	mc=new FXMenuCommand(editmenu,_("&Invert selection"),invselicon,lpanel,FilePanel::ID_SELECT_INVERSE);
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","invert_selection","Ctrl-I");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(editmenu);    
	new FXMenuCommand(editmenu,_("P&references"),prefsicon,this,XFileExplorer::ID_PREFS);
   
	editmenutitle=new FXMenuTitle(menubar,_("&Edit"),NULL,editmenu);

    // Bookmarks menu
    bookmarksmenu=new FXMenuPane(this);
    mc=new FXMenuCommand(bookmarksmenu,_("&Add bookmark"),setbookicon,this,ID_ADD_BOOKMARK);
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","add_bookmark","Ctrl-B");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(bookmarksmenu);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_1);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_2);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_3);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_4);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_5);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_6);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_7);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_8);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_9);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_10);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_11);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_12);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_13);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_14);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_15);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_16);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_17);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_18);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_19);
    new FXMenuCommand(bookmarksmenu,FXString::null,NULL,bookmarks,Bookmarks::ID_BOOKMARK_20);
    new FXMenuSeparator(bookmarksmenu);
	new FXMenuCommand(bookmarksmenu,_("&Clear bookmarks"),clrbookicon,bookmarks,Bookmarks::ID_CLEAR);
    
	bookmarksmenutitle=new FXMenuTitle(menubar,_("&Bookmarks"),NULL,bookmarksmenu);

    // View menu
    viewmenu=new FXMenuPane(this);
    new FXMenuCheck(viewmenu,_("&General toolbar"),generaltoolbar,FXWindow::ID_TOGGLESHOWN);
    new FXMenuCheck(viewmenu,_("&Tools toolbar"),toolstoolbar,FXWindow::ID_TOGGLESHOWN);
    new FXMenuCheck(viewmenu,_("&Panel toolbar"),paneltoolbar,FXWindow::ID_TOGGLESHOWN);
	new FXMenuCheck(viewmenu,_("&Location bar"),locationbar,FXWindow::ID_TOGGLESHOWN);
    new FXMenuCheck(viewmenu,_("&Status bar"),this,XFileExplorer::ID_TOGGLE_STATUS);

    new FXMenuSeparator(viewmenu);

    mc=new FXMenuRadio(viewmenu,_("&One panel"),this,XFileExplorer::ID_SHOW_ONE_PANEL);
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","one_panel","Ctrl-F1");
	mc->setAccelText(key);

    mc=new FXMenuRadio(viewmenu,_("T&ree and panel"),this,XFileExplorer::ID_SHOW_TREE_PANEL);
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","tree_panel","Ctrl-F2");
	mc->setAccelText(key);

    mc=new FXMenuRadio(viewmenu,_("Two &panels"),this,XFileExplorer::ID_SHOW_TWO_PANELS);
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","two_panels","Ctrl-F3");
	mc->setAccelText(key);

    mc=new FXMenuRadio(viewmenu,_("Tr&ee and two panels"),this,XFileExplorer::ID_SHOW_TREE_TWO_PANELS);
   	key=getApp()->reg().readStringEntry("KEYBINDINGS","tree_two_panels","Ctrl-F4");
	mc->setAccelText(key);
   
	viewmenutitle=new FXMenuTitle(menubar,_("&View"),NULL,viewmenu);

    // Left Panel Menu
    lpanelmenu=new FXMenuPane(this);
	new FXMenuCommand(lpanelmenu,_("&Filter..."),filtericon,lpanel,FilePanel::ID_FILTER);
    new FXMenuCheck(lpanelmenu,_("&Hidden files"),lpanel->getList(),FileList::ID_TOGGLE_HIDDEN);
    new FXMenuCheck(lpanelmenu,_("&Thumbnails"),lpanel->getList(),FileList::ID_TOGGLE_THUMBNAILS);
    new FXMenuSeparator(lpanelmenu);
    new FXMenuRadio(lpanelmenu,_("&Big icons"),lpanel->getList(),IconList::ID_SHOW_BIG_ICONS);
    new FXMenuRadio(lpanelmenu,_("&Small icons"),lpanel->getList(),IconList::ID_SHOW_MINI_ICONS);
    new FXMenuRadio(lpanelmenu,_("F&ull file list"),lpanel->getList(),IconList::ID_SHOW_DETAILS);
    new FXMenuSeparator(lpanelmenu);
    new FXMenuRadio(lpanelmenu,_("&Rows"),lpanel->getList(),FileList::ID_ARRANGE_BY_ROWS);
    new FXMenuRadio(lpanelmenu,_("&Columns"),lpanel->getList(),FileList::ID_ARRANGE_BY_COLUMNS);
    new FXMenuCheck(lpanelmenu,_("Autosize"),lpanel->getList(),FileList::ID_AUTOSIZE);
    new FXMenuSeparator(lpanelmenu);
    new FXMenuRadio(lpanelmenu,_("&Name"),lpanel->getList(),FileList::ID_SORT_BY_NAME);
    new FXMenuRadio(lpanelmenu,_("Si&ze"),lpanel->getList(),FileList::ID_SORT_BY_SIZE);
    new FXMenuRadio(lpanelmenu,_("T&ype"),lpanel->getList(),FileList::ID_SORT_BY_TYPE);
    new FXMenuRadio(lpanelmenu,_("E&xtension"),lpanel->getList(),FileList::ID_SORT_BY_EXT);
    new FXMenuRadio(lpanelmenu,_("D&ate"),lpanel->getList(),FileList::ID_SORT_BY_TIME);
    new FXMenuRadio(lpanelmenu,_("Us&er"),lpanel->getList(),FileList::ID_SORT_BY_USER);
    new FXMenuRadio(lpanelmenu,_("Gr&oup"),lpanel->getList(),FileList::ID_SORT_BY_GROUP);
    new FXMenuRadio(lpanelmenu,_("&Permissions"),lpanel->getList(),FileList::ID_SORT_BY_PERM);
    new FXMenuRadio(lpanelmenu,_("Deletion date"),lpanel->getList(),FileList::ID_SORT_BY_DELTIME);
    new FXMenuRadio(lpanelmenu,_("Original path"),lpanel->getList(),FileList::ID_SORT_BY_ORIGPATH);
    new FXMenuSeparator(lpanelmenu);
  	new FXMenuCheck(lpanelmenu,_("I&gnore case"),lpanel->getList(),FileList::ID_SORT_CASE);
  	new FXMenuCheck(lpanelmenu,_("&Directories first"),lpanel->getList(),FileList::ID_DIRS_FIRST);
  	new FXMenuCheck(lpanelmenu,_("Re&verse order"),lpanel->getList(),FileList::ID_SORT_REVERSE);
    lpanelmenutitle=new FXMenuTitle(menubar,_("&Left panel"),NULL,lpanelmenu);

    // Right Panel Menu
    rpanelmenu=new FXMenuPane(this);
    new FXMenuCommand(rpanelmenu,_("&Filter"),filtericon,rpanel,FilePanel::ID_FILTER);
    new FXMenuCheck(rpanelmenu,_("&Hidden files"),rpanel->getList(),FileList::ID_TOGGLE_HIDDEN);
    new FXMenuCheck(rpanelmenu,_("&Thumbnails"),rpanel->getList(),FileList::ID_TOGGLE_THUMBNAILS);
    new FXMenuSeparator(rpanelmenu);
    new FXMenuRadio(rpanelmenu,_("&Big icons"),rpanel->getList(),IconList::ID_SHOW_BIG_ICONS);
    new FXMenuRadio(rpanelmenu,_("&Small icons"),rpanel->getList(),IconList::ID_SHOW_MINI_ICONS);
    new FXMenuRadio(rpanelmenu,_("F&ull file list"),rpanel->getList(),IconList::ID_SHOW_DETAILS);
    new FXMenuSeparator(rpanelmenu);
    new FXMenuRadio(rpanelmenu,_("&Rows"),rpanel->getList(),FileList::ID_ARRANGE_BY_ROWS);
    new FXMenuRadio(rpanelmenu,_("&Columns"),rpanel->getList(),FileList::ID_ARRANGE_BY_COLUMNS);
    new FXMenuCheck(rpanelmenu,_("Autosize"),rpanel->getList(),FileList::ID_AUTOSIZE);
    new FXMenuSeparator(rpanelmenu);
    new FXMenuRadio(rpanelmenu,_("&Name"),rpanel->getList(),FileList::ID_SORT_BY_NAME);
    new FXMenuRadio(rpanelmenu,_("Si&ze"),rpanel->getList(),FileList::ID_SORT_BY_SIZE);
    new FXMenuRadio(rpanelmenu,_("T&ype"),rpanel->getList(),FileList::ID_SORT_BY_TYPE);
    new FXMenuRadio(rpanelmenu,_("E&xtension"),rpanel->getList(),FileList::ID_SORT_BY_EXT);
    new FXMenuRadio(rpanelmenu,_("D&ate"),rpanel->getList(),FileList::ID_SORT_BY_TIME);
    new FXMenuRadio(rpanelmenu,_("Us&er"),rpanel->getList(),FileList::ID_SORT_BY_USER);
    new FXMenuRadio(rpanelmenu,_("Gr&oup"),rpanel->getList(),FileList::ID_SORT_BY_GROUP);
    new FXMenuRadio(rpanelmenu,_("&Permissions"),rpanel->getList(),FileList::ID_SORT_BY_PERM);
    new FXMenuRadio(rpanelmenu,_("Deletion date"),rpanel->getList(),FileList::ID_SORT_BY_DELTIME);
    new FXMenuRadio(rpanelmenu,_("Original path"),rpanel->getList(),FileList::ID_SORT_BY_ORIGPATH);
    new FXMenuSeparator(rpanelmenu);
  	new FXMenuCheck(rpanelmenu,_("I&gnore case"),rpanel->getList(),FileList::ID_SORT_CASE);
  	new FXMenuCheck(rpanelmenu,_("&Directories first"),rpanel->getList(),FileList::ID_DIRS_FIRST);
  	new FXMenuCheck(rpanelmenu,_("Re&verse order"),rpanel->getList(),FileList::ID_SORT_REVERSE);
    rpanelmenutitle=new FXMenuTitle(menubar,_("&Right panel"),NULL,rpanelmenu);

	// Tools menu
	toolsmenu=new FXMenuPane(this);
    
	mc=new FXMenuCommand(toolsmenu,_("New &window"),minixfeicon,this,XFileExplorer::ID_NEW_WIN);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_window","F3");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(toolsmenu,_("New &root window"),minixferooticon,this,XFileExplorer::ID_SU);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_root_window","Shift-F3");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(toolsmenu);

    mc=new FXMenuCommand(toolsmenu,_("E&xecute command..."),runicon,this,ID_RUN);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","execute_command","Ctrl-E");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    mc=new FXMenuCommand(toolsmenu,_("&Terminal"),minishellicon,this,XFileExplorer::ID_XTERM);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","terminal","Ctrl-T");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
    
	mc=new FXMenuCommand(toolsmenu,_("&Synchronize panels"),syncpanelsicon,this,XFileExplorer::ID_SYNCHRONIZE_PANELS);
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","synchronize_panels","Ctrl-Y");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
   
	mc=new FXMenuCommand(toolsmenu,_("Sw&itch panels"),switchpanelsicon,this,XFileExplorer::ID_SWITCH_PANELS);
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","switch_panels","Ctrl-K");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

#if defined(linux)
	new FXMenuSeparator(toolsmenu);
    
	mc=new FXMenuCommand(toolsmenu,_("&Mount"),maphosticon,lpanel,FilePanel::ID_MOUNT);
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","mount","Ctrl-M");
	mc->setAccelText(key);
    
	mc=new FXMenuCommand(toolsmenu,_("&Unmount"),unmaphosticon,lpanel,FilePanel::ID_UMOUNT);
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","unmount","Ctrl-U");
	mc->setAccelText(key);

#endif
	toolsmenutitle=new FXMenuTitle(menubar,_("&Tools"),NULL,toolsmenu);

	// Trash menu
	trashmenu=new FXMenuPane(this);
    
	mc=new FXMenuCommand(trashmenu,_("&Go to trash"),totrashicon,lpanel,FilePanel::ID_GO_TRASH);
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_to_trash","Ctrl-F8");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
    
	mc=new FXMenuCommand(trashmenu,_("&Trash size"),filedeleteicon,this,XFileExplorer::ID_TRASH_SIZE);

	mc=new FXMenuCommand(trashmenu,_("&Empty trash can"),trash_fullicon,this,XFileExplorer::ID_EMPTY_TRASH);
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","empty_trash_can","Ctrl-Del");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));


	trashmenutitle=new FXMenuTitle(menubar,_("T&rash"),NULL,trashmenu);

	// Help menu
    helpmenu=new FXMenuPane(this);
    
	mc=new FXMenuCommand(helpmenu,_("&Help"),helpicon,this,ID_HELP);
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","help","F1");
	mc->setAccelText(key);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
    
	new FXMenuCommand(helpmenu,_("&About X File Explorer"),NULL,this,ID_ABOUT);
    helpmenutitle=new FXMenuTitle(menubar,_("&Help"),NULL,helpmenu);

  	// Other accelerators
	
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","edit","F4");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,lpanel,FXSEL(SEL_COMMAND,FilePanel::ID_EDIT));

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","view","Shift-F4");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,lpanel,FXSEL(SEL_COMMAND,FilePanel::ID_VIEW));

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","hidden_dirs","Ctrl-F5");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,dirpanel,FXSEL(SEL_COMMAND,DirPanel::ID_TOGGLE_HIDDEN));
	
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","filter","Ctrl-D");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,lpanel,FXSEL(SEL_COMMAND,FilePanel::ID_FILTER_CURRENT));

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","hidden_files","Ctrl-F6");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,lpanel,FXSEL(SEL_COMMAND,FilePanel::ID_TOGGLE_HIDDEN));
	
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","thumbnails","Ctrl-F7");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,lpanel,FXSEL(SEL_COMMAND,FilePanel::ID_TOGGLE_THUMBNAILS));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","close","Ctrl-W");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,this,FXSEL(SEL_COMMAND,XFileExplorer::ID_QUIT));
	
	// Escape key
	getAccelTable()->addAccel(KEY_Escape,lpanel,FXSEL(SEL_COMMAND,FilePanel::ID_DESELECT_ALL));
	
    // Make a tool tip
    new FXToolTip(app,0);

	// File operations dialog
    rundialog=NULL;
	prefsdialog=NULL;
	helpwindow=NULL;
	
	// Initial focus is on (left) file panel
	panelfocus=FILEPANEL_FOCUS;
	
	// Trahscan locations
	trashfileslocation=xdgdatahome + PATHSEPSTRING TRASHFILESPATH;
	trashinfolocation=xdgdatahome + PATHSEPSTRING TRASHINFOPATH;

	// Start location (we return to the start location after each chdir)
	startlocation=FXSystem::getCurrentDirectory();

	// Other initializations
	startdirectory1=startdir1;
	startdirectory2=startdir2;
	starticonic=iconic;
	startmaximized=maximized;
	prevdir=FXString::null;
	prev_width=getWidth();
}


// Save configuration when quitting
void XFileExplorer::saveConfig()
{	
	// Get autosave flag
    FXbool auto_save_layout=getApp()->reg().readUnsignedEntry("OPTIONS","auto_save_layout",TRUE);

	if(auto_save_layout==TRUE)
    {
		FXString sort_func;

		// Dir panel options
		if (dirpanel->getSortFunc()==DirList::ascendingCase)
			sort_func="ascendingCase";
		else if (dirpanel->getSortFunc()==DirList::descendingCase)
			sort_func="descendingCase";
		else if (dirpanel->getSortFunc()==DirList::ascending)
			sort_func="ascending";
		else if (dirpanel->getSortFunc()==DirList::descending)
			sort_func="descending";
		else
			sort_func="ascendingCase";
		getApp()->reg().writeStringEntry("DIR PANEL","sort_func",sort_func.text());
        getApp()->reg().writeUnsignedEntry("DIR PANEL","hidden_dir",dirpanel->shownHiddenFiles());

		// Left panel options
		getApp()->reg().writeUnsignedEntry("LEFT PANEL","name_size",lpanel->getHeaderSize(0));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","size_size",lpanel->getHeaderSize(1));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","type_size",lpanel->getHeaderSize(2));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","ext_size",lpanel->getHeaderSize(3));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","modd_size",lpanel->getHeaderSize(4));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","user_size",lpanel->getHeaderSize(5));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","grou_size",lpanel->getHeaderSize(6));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","attr_size",lpanel->getHeaderSize(7));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","deldate_size",lpanel->getHeaderSize(8));
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","origpath_size",lpanel->getHeaderSize(9));
		getApp()->reg().writeUnsignedEntry("LEFT PANEL","liststyle",lpanel->getListStyle());
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","hiddenfiles",lpanel->shownHiddenFiles());
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","showthumbnails",lpanel->shownThumbnails());

		// Get and write sort function for left panel
		if (lpanel->getSortFunc()==FileList::ascendingCase)
			sort_func="ascendingCase";
		else if (lpanel->getSortFunc()==FileList::ascendingCaseMix)
			sort_func="ascendingCaseMix";
		else if (lpanel->getSortFunc()==FileList::descendingCase)
			sort_func="descendingCase";
		else if (lpanel->getSortFunc()==FileList::descendingCaseMix)
			sort_func="descendingCaseMix";
		else if (lpanel->getSortFunc()==FileList::ascending)
			sort_func="ascending";
		else if (lpanel->getSortFunc()==FileList::ascendingMix)
			sort_func="ascendingMix";
		else if (lpanel->getSortFunc()==FileList::descending)
			sort_func="descending";
		else if (lpanel->getSortFunc()==FileList::descendingMix)
			sort_func="descendingMix";
		else if (lpanel->getSortFunc()==FileList::ascendingSize)
			sort_func="ascendingSize";
		else if (lpanel->getSortFunc()==FileList::ascendingSizeMix)
			sort_func="ascendingSizeMix";
		else if (lpanel->getSortFunc()==FileList::descendingSize)
			sort_func="descendingSize";
		else if (lpanel->getSortFunc()==FileList::descendingSizeMix)
			sort_func="descendingSizeMix";
		else if (lpanel->getSortFunc()==FileList::ascendingType)
			sort_func="ascendingType";
		else if (lpanel->getSortFunc()==FileList::ascendingTypeMix)
			sort_func="ascendingTypeMix";
		else if (lpanel->getSortFunc()==FileList::descendingType)
			sort_func="descendingType";
		else if (lpanel->getSortFunc()==FileList::descendingTypeMix)
			sort_func="descendingTypeMix";
		else if (lpanel->getSortFunc()==FileList::ascendingExt)
			sort_func="ascendingExt";
		else if (lpanel->getSortFunc()==FileList::ascendingExtMix)
			sort_func="ascendingExtMix";
		else if (lpanel->getSortFunc()==FileList::descendingExt)
			sort_func="descendingExt";
		else if (lpanel->getSortFunc()==FileList::descendingExtMix)
			sort_func="descendingExtMix";
		else if (lpanel->getSortFunc()==FileList::ascendingTime)
			sort_func="ascendingTime";
		else if (lpanel->getSortFunc()==FileList::ascendingTimeMix)
			sort_func="ascendingTimeMix";
		else if (lpanel->getSortFunc()==FileList::descendingTime)
			sort_func="descendingTime";
		else if (lpanel->getSortFunc()==FileList::descendingTimeMix)
			sort_func="descendingTimeMix";
		else if (lpanel->getSortFunc()==FileList::ascendingUser)
			sort_func="ascendingUser";
		else if (lpanel->getSortFunc()==FileList::ascendingUserMix)
			sort_func="ascendingUserMix";
		else if (lpanel->getSortFunc()==FileList::descendingUser)
			sort_func="descendingUser";
		else if (lpanel->getSortFunc()==FileList::descendingUserMix)
			sort_func="descendingUserMix";
		else if (lpanel->getSortFunc()==FileList::ascendingGroup)
			sort_func="ascendingGroup";
		else if (lpanel->getSortFunc()==FileList::ascendingGroupMix)
			sort_func="ascendingGroupMix";
		else if (lpanel->getSortFunc()==FileList::descendingGroup)
			sort_func="descendingGroup";
		else if (lpanel->getSortFunc()==FileList::descendingGroupMix)
			sort_func="descendingGroupMix";
		else if (lpanel->getSortFunc()==FileList::ascendingPerm)
			sort_func="ascendingPerm";
		else if (lpanel->getSortFunc()==FileList::ascendingPermMix)
			sort_func="ascendingPermMix";
		else if (lpanel->getSortFunc()==FileList::descendingPerm)
			sort_func="descendingPerm";
		else if (lpanel->getSortFunc()==FileList::descendingPermMix)
			sort_func="descendingPermMix";
		else
			sort_func="ascendingCase";
		getApp()->reg().writeStringEntry("LEFT PANEL","sort_func",sort_func.text());
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","ignore_case",lpanel->getIgnoreCase());
        getApp()->reg().writeUnsignedEntry("LEFT PANEL","dirs_first",lpanel->getDirsFirst());

		// Right panel options
		getApp()->reg().writeUnsignedEntry("RIGHT PANEL","name_size",rpanel->getHeaderSize(0));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","size_size",rpanel->getHeaderSize(1));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","type_size",rpanel->getHeaderSize(2));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","ext_size",rpanel->getHeaderSize(3));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","modd_size",rpanel->getHeaderSize(4));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","user_size",rpanel->getHeaderSize(5));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","grou_size",rpanel->getHeaderSize(6));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","attr_size",rpanel->getHeaderSize(7));
		getApp()->reg().writeUnsignedEntry("RIGHT PANEL","deldate_size",rpanel->getHeaderSize(8));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","origpath_size",rpanel->getHeaderSize(9));
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","liststyle",rpanel->getListStyle());
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","hiddenfiles",rpanel->shownHiddenFiles());
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","showthumbnails",rpanel->shownThumbnails());

		// Get and write sort function for right panel
		if (rpanel->getSortFunc()==FileList::ascendingCase)
			sort_func="ascendingCase";
		else if (rpanel->getSortFunc()==FileList::ascendingCaseMix)
			sort_func="ascendingCaseMix";
		else if (rpanel->getSortFunc()==FileList::descendingCase)
			sort_func="descendingCase";
		else if (rpanel->getSortFunc()==FileList::descendingCaseMix)
			sort_func="descendingCaseMix";
		else if (rpanel->getSortFunc()==FileList::ascending)
			sort_func="ascending";
		else if (rpanel->getSortFunc()==FileList::ascendingMix)
			sort_func="ascendingMix";
		else if (rpanel->getSortFunc()==FileList::descending)
			sort_func="descending";
		else if (rpanel->getSortFunc()==FileList::descendingMix)
			sort_func="descendingMix";
		else if (rpanel->getSortFunc()==FileList::ascendingSize)
			sort_func="ascendingSize";
		else if (rpanel->getSortFunc()==FileList::ascendingSizeMix)
			sort_func="ascendingSizeMix";
		else if (rpanel->getSortFunc()==FileList::descendingSize)
			sort_func="descendingSize";
		else if (rpanel->getSortFunc()==FileList::descendingSizeMix)
			sort_func="descendingSizeMix";
		else if (rpanel->getSortFunc()==FileList::ascendingType)
			sort_func="ascendingType";
		else if (rpanel->getSortFunc()==FileList::ascendingTypeMix)
			sort_func="ascendingTypeMix";
		else if (rpanel->getSortFunc()==FileList::descendingType)
			sort_func="descendingType";
		else if (rpanel->getSortFunc()==FileList::descendingTypeMix)
			sort_func="descendingTypeMix";
		else if (rpanel->getSortFunc()==FileList::ascendingExt)
			sort_func="ascendingExt";
		else if (rpanel->getSortFunc()==FileList::ascendingExtMix)
			sort_func="ascendingExtMix";
		else if (rpanel->getSortFunc()==FileList::descendingExt)
			sort_func="descendingExt";
		else if (rpanel->getSortFunc()==FileList::descendingExtMix)
			sort_func="descendingExtMix";
		else if (rpanel->getSortFunc()==FileList::ascendingTime)
			sort_func="ascendingTime";
		else if (rpanel->getSortFunc()==FileList::ascendingTimeMix)
			sort_func="ascendingTimeMix";
		else if (rpanel->getSortFunc()==FileList::descendingTime)
			sort_func="descendingTime";
		else if (rpanel->getSortFunc()==FileList::descendingTimeMix)
			sort_func="descendingTimeMix";
		else if (rpanel->getSortFunc()==FileList::ascendingUser)
			sort_func="ascendingUser";
		else if (rpanel->getSortFunc()==FileList::ascendingUserMix)
			sort_func="ascendingUserMix";
		else if (rpanel->getSortFunc()==FileList::descendingUser)
			sort_func="descendingUser";
		else if (rpanel->getSortFunc()==FileList::descendingUserMix)
			sort_func="descendingUserMix";
		else if (rpanel->getSortFunc()==FileList::ascendingGroup)
			sort_func="ascendingGroup";
		else if (rpanel->getSortFunc()==FileList::ascendingGroupMix)
			sort_func="ascendingGroupMix";
		else if (rpanel->getSortFunc()==FileList::descendingGroup)
			sort_func="descendingGroup";
		else if (rpanel->getSortFunc()==FileList::descendingGroupMix)
			sort_func="descendingGroupMix";
		else if (rpanel->getSortFunc()==FileList::ascendingPerm)
			sort_func="ascendingPerm";
		else if (rpanel->getSortFunc()==FileList::ascendingPermMix)
			sort_func="ascendingPermMix";
		else if (rpanel->getSortFunc()==FileList::descendingPerm)
			sort_func="descendingPerm";
		else if (rpanel->getSortFunc()==FileList::descendingPermMix)
			sort_func="descendingPermMix";
		else
			sort_func="ascendingCase";
		getApp()->reg().writeStringEntry("RIGHT PANEL","sort_func",sort_func.text());
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","ignore_case",rpanel->getIgnoreCase());
        getApp()->reg().writeUnsignedEntry("RIGHT PANEL","dirs_first",rpanel->getDirsFirst());

        // Global options
		getApp()->reg().writeUnsignedEntry("OPTIONS","width",(unsigned int)getWidth());
        getApp()->reg().writeUnsignedEntry("OPTIONS","height",(unsigned int)getHeight());
        
        // Get value of window position flag and position the window
        FXbool save_win_pos=getApp()->reg().readUnsignedEntry("SETTINGS","save_win_pos",FALSE);
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
        getApp()->reg().writeUnsignedEntry("OPTIONS","generaltoolbar",(unsigned int)generaltoolbar->shown());
        getApp()->reg().writeUnsignedEntry("OPTIONS","toolstoolbar",(unsigned int)toolstoolbar->shown());
        getApp()->reg().writeUnsignedEntry("OPTIONS","paneltoolbar",(unsigned int)paneltoolbar->shown());
		getApp()->reg().writeUnsignedEntry("OPTIONS","locationbar",(unsigned int)locationbar->shown());
        getApp()->reg().writeUnsignedEntry("OPTIONS","status",(unsigned int)lpanel->statusbarShown());
        getApp()->reg().writeUnsignedEntry("SETTINGS","file_tooltips",(unsigned int)file_tooltips);
        getApp()->reg().writeUnsignedEntry("SETTINGS","relative_resize",(unsigned int)relative_resize);
        getApp()->reg().writeRealEntry("OPTIONS","treepanel_tree_pct",treepanel_tree_pct);
        getApp()->reg().writeRealEntry("OPTIONS","twopanels_lpanel_pct",twopanels_lpanel_pct);
        getApp()->reg().writeRealEntry("OPTIONS","treetwopanels_tree_pct",treetwopanels_tree_pct);
        getApp()->reg().writeRealEntry("OPTIONS","treetwopanels_lpanel_pct",treetwopanels_lpanel_pct);
		
		// Save panel view only if not given from command line
		if (panel_mode == -1)
			getApp()->reg().writeUnsignedEntry("OPTIONS","panel_view",(unsigned int)panel_view);

        getApp()->reg().writeUnsignedEntry("SETTINGS","single_click",single_click);

        FXString history="";
        for(int i=0;i<RunHistSize;i++)
        {
            history+=RunHistory[i];
            history+=":";
        }
        if(RunHistory)
            getApp()->reg().writeStringEntry("HISTORY","run",history.text());

        history="";
        for(int i=0;i<OpenNum;i++)
        {
            history+=OpenHistory[i];
            history+=":";
        }
        if(OpenNum)
            getApp()->reg().writeStringEntry("HISTORY","open",history.text());

        history="";
        for(int i=0;i<FilterNum;i++)
        {
            history+=FilterHistory[i];
            history+=":";
        }
        if(FilterNum)
            getApp()->reg().writeStringEntry("HISTORY","filter",history.text());
    }
	getApp()->reg().write();
}

// Make application
void XFileExplorer::create()
{
	// Switch to two panels mode if startdir2 was specified
	// and no particular panel mode was selected
	if (startdirectory2 != "" && panel_mode==-1)
		panel_mode=2;

	// Eventually select panel mode from the command line option
	// or revert to last saved panel view
	switch (panel_mode)
	{
	case 0:
		panel_view=TREE_PANEL;
		break;
	case 1:
		panel_view=ONE_PANEL;
		break;
	case 2:
		panel_view=TWO_PANELS;
		break;
	case 3:
		panel_view=TREE_TWO_PANELS;
		break;
	default:
		panel_view=getApp()->reg().readUnsignedEntry("OPTIONS","panel_view",TREE_PANEL);
	}

    int width=getApp()->reg().readUnsignedEntry("OPTIONS","width",DEFAULT_WINDOW_WIDTH);
    int height=getApp()->reg().readUnsignedEntry("OPTIONS","height",DEFAULT_WINDOW_HEIGHT);
    int save_win_pos=getApp()->reg().readUnsignedEntry("SETTINGS","save_win_pos",FALSE);
    if (save_win_pos)
	{
		int xpos=getApp()->reg().readIntEntry("OPTIONS","xpos",DEFAULT_WINDOW_XPOS);
		int ypos=getApp()->reg().readIntEntry("OPTIONS","ypos",DEFAULT_WINDOW_YPOS);
		position(xpos,ypos,width,height);
	}
	else
    	position(getX(),getY(),width,height);

    FXMainWindow::create();
	int window_width=getWidth();
	twopanels_lpanel_pct=getApp()->reg().readRealEntry("OPTIONS","twopanels_lpanel_pct",0.50);
	treepanel_tree_pct=getApp()->reg().readRealEntry("OPTIONS","treepanel_tree_pct",0.20);
	treetwopanels_tree_pct=getApp()->reg().readRealEntry("OPTIONS","treetwopanels_tree_pct",0.20);
	treetwopanels_lpanel_pct=getApp()->reg().readRealEntry("OPTIONS","treetwopanels_lpanel_pct",0.40);
	
	switch(panel_view)
	{
	case ONE_PANEL:
		rpanel->hide();
        dirpanel->hide();
		// Handle drag corner 
		rpanel->showCorner(FALSE);
		lpanel->showCorner(TRUE);
 		// Handle active icon
        lpanel->showActiveIcon(FALSE);
		lpanel->setWidth((int)round(1.0*window_width));
		break;
	case TWO_PANELS:
		dirpanel->hide();
		lpanel->setWidth((int)round(twopanels_lpanel_pct*window_width));
		// Handle drag corner 
		rpanel->showCorner(TRUE);
		lpanel->showCorner(FALSE);
 		// Handle active icon
       lpanel->showActiveIcon(TRUE);
		break;
	case TREE_PANEL:
		rpanel->hide();
		dirpanel->setWidth((int)round(treepanel_tree_pct*window_width));
		lpanel->setWidth((int)round((1.0-treepanel_tree_pct)*window_width));
		// Handle drag corner 
		rpanel->showCorner(FALSE);
		lpanel->showCorner(TRUE);
 		// Handle active icon
        lpanel->showActiveIcon(TRUE);
		break;
	case TREE_TWO_PANELS:
		dirpanel->setWidth((int)round(treetwopanels_tree_pct*window_width));
		lpanel->setWidth((int)round(treetwopanels_lpanel_pct*window_width));
		// Handle drag corner 
		rpanel->showCorner(TRUE);
		lpanel->showCorner(FALSE);
 		// Handle active icon
        lpanel->showActiveIcon(TRUE);
		break;
	}

    if(!getApp()->reg().readUnsignedEntry("OPTIONS","generaltoolbar",TRUE))
        generaltoolbar->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),NULL);
    if(!getApp()->reg().readUnsignedEntry("OPTIONS","toolstoolbar",TRUE))
        toolstoolbar->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),NULL);
    if(!getApp()->reg().readUnsignedEntry("OPTIONS","paneltoolbar",TRUE))
        paneltoolbar->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),NULL);
    if(!getApp()->reg().readUnsignedEntry("OPTIONS","locationbar",TRUE))
        locationbar->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),NULL);
    if(!getApp()->reg().readUnsignedEntry("OPTIONS","status",TRUE))
        handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_TOGGLE_STATUS),NULL);
	file_tooltips=getApp()->reg().readUnsignedEntry("SETTINGS","file_tooltips",1);
	relative_resize=getApp()->reg().readUnsignedEntry("SETTINGS","relative_resize",1);

	// Wheel scrolling
    int wheellines=getApp()->reg().readUnsignedEntry("SETTINGS","wheellines",5);
	getApp()->setWheelLines(wheellines);
	
    // Open left and right panels in starting directories (if specified) or in current directory
    if (startdirectory1=="")
        startdirectory1=FXSystem::getCurrentDirectory();
    if (startdirectory2=="")
        startdirectory2=FXSystem::getCurrentDirectory();
    lpanel->setDirectory(startdirectory1,TRUE);
	lpanel->setPathLink(startdirectory1);
	lpanel->setPathText(startdirectory1);
    rpanel->setDirectory(startdirectory2,TRUE);
	rpanel->setPathLink(startdirectory2);
	rpanel->setPathText(startdirectory2);
	dirpanel->setDirectory(startdirectory1,TRUE);

	// Set file panels list style
	liststyle=getApp()->reg().readUnsignedEntry("LEFT PANEL","liststyle",(unsigned int)IconList::ID_SHOW_DETAILS);
	lpanel->setListStyle(liststyle);
    liststyle=getApp()->reg().readUnsignedEntry("RIGHT PANEL","liststyle",(unsigned int)IconList::ID_SHOW_DETAILS);
    rpanel->setListStyle(liststyle);

	// Show or hide hidden files listed in panels
    FXbool hiddenfiles=getApp()->reg().readUnsignedEntry("LEFT PANEL","hiddenfiles",0);
    lpanel->showHiddenFiles(hiddenfiles);
    hiddenfiles=getApp()->reg().readUnsignedEntry("RIGHT PANEL","hiddenfiles",0);
    rpanel->showHiddenFiles(hiddenfiles);
	
	// Show or hide hidden directories listed in dirpanel
    FXbool hidden_dir=getApp()->reg().readUnsignedEntry("DIR PANEL","hidden_dir",0);
    dirpanel->showHiddenFiles(hidden_dir);

	// History
    FXString history=getApp()->reg().readStringEntry("HISTORY","run","");
   	int i;
    FXString histent;
    if(history != "")
    {
        for(i=0;;i++)
        {
            histent=history.section(':',i);
            if(streq(histent.text(),""))
                break;
            strlcpy(RunHistory[i],histent.text(),histent.length()+1);
        }
        RunHistSize=i;
    }
    else
        RunHistSize=0;
		
		
    history=getApp()->reg().readStringEntry("HISTORY","open","");
    histent="";
    if(history != "")
    {
        for(i=0;;i++)
        {
            histent=history.section(':',i);
            if(streq(histent.text(),""))
                break;
			strlcpy(OpenHistory[i],histent.text(),histent.length()+1);
        }
        OpenNum=i;
    }
    else
        OpenNum=0;

    history=getApp()->reg().readStringEntry("HISTORY","filter","");
    histent="";
    if(history != "")
    {
        for(i=0;;i++)
        {
            histent=history.section(':',i);
            if(streq(histent.text(),""))
                break;
			strlcpy(FilterHistory[i],histent.text(),histent.length()+1);
        }
        FilterNum=i;
    }
    else
        FilterNum=0;

    getApp()->forceRefresh();

	// Running as root?
    FXbool root_warn=getApp()->reg().readUnsignedEntry("OPTIONS","root_warn",TRUE);
	if(getuid()==0 && root_warn)
        MessageBox::information(this,BOX_OK,_("Warning"),_("Running Xfe as root!"));

	// Initial focus is always on the left panel
	lpanel->setFocusOnList();
	
	// Show window
    show();

#if defined(linux)
	// Warning message if a mount point is down
	FXbool mount_warn=getApp()->reg().readUnsignedEntry("OPTIONS","mount_warn",TRUE);
	if (mount_warn)
	{
		int d;
		for (d=updevices->first(); d<updevices->size(); d=updevices->next(d))
		{
			if(streq(updevices->data(d),"down"))
				MessageBox::warning(this,BOX_OK,_("Warning"),_("Mount point %s is not responding..."),updevices->key(d));
		}
	}
#endif

	// If no Xfe local configuration exists (i.e. at first call or after a purge of the configuration files),
	// copy the global xferc file to the local configuration directory, and read / write the registry
	int mask;
	FXString configlocation=xdgconfighome + PATHSEPSTRING XFECONFIGPATH;
	FXString configpath=configlocation + PATHSEPSTRING XFECONFIGNAME;
	if (!::exists(configpath))
	{
		// If old configuration path (i.e. ~/.xfe) exists then warn the user about the new configuration scheme
		FXString oldconfigpath=homedir + PATHSEPSTRING ".xfe";
		if (::exists(oldconfigpath))
		{
			// Display warning message		
			FXString message;
			message.format(_("Starting from Xfe 1.32, the location of the configuration files has changed to '%s'.\nNote you can manually edit the new configuration files to import your old customizations..."),configlocation.text());
			MessageBox::warning(this,BOX_OK,_("Warning"),message.text());
		}
		
		// Create ~/.config/xfe directory if it doesn't exist	
		if (!::exists(configlocation))
		{
			// Create the ~/.config/xfe directory according to the umask
			mask=umask(0);
			umask(mask);
			errno=0;
			int ret=mkpath(configlocation.text(),511 & ~mask);
			int errcode=errno;
			if (ret==-1)
			{
				if (errcode)
					MessageBox::error(this,BOX_OK,_("Error"),_("Can't create Xfe config folder %s : %s"),configlocation.text(),strerror(errcode));
				else
					MessageBox::error(this,BOX_OK,_("Error"),_("Can't create Xfe config folder %s"),configlocation.text());					
			}
		}
		
		// Copy the global xfrec file (three possible locations) to the local configuration file
		if (::exists("/usr/share/xfe/xferc"))
			FXFile::copy("/usr/share/xfe/xferc",configpath,FALSE);		
		else if (::exists("/usr/local/share/xfe/xferc"))
			FXFile::copy("/usr/local/share/xfe/xferc",configpath,FALSE);
		else if (::exists("/opt/local/share/xfe/xferc"))
			FXFile::copy("/opt/local/share/xfe/xferc",configpath,FALSE);

		// If nothing is found, display a file dialog to let the user choose the right place
		else
		{
			FileDialog browse(this,_("No global xferc file found! Please select a configuration file..."));

			const char *patterns[]=
			{
				_("XFE configuration file"),     "*xferc*",NULL
			};
			browse.setFilename(ROOTDIR);
			browse.setPatternList(patterns);
			if(browse.execute())
			{
				FXString path=browse.getFilename();
				FXFile::copy(path,configpath,FALSE);
			}
		}

		// Read and write the registry
		getApp()->reg().read();
		getApp()->reg().write();
	}

    // Create trash can files directory if it doesn't exist
	if (!::exists(trashfileslocation))
	{
		// Create the trash can files directory according to the umask
		mask=umask(0);
		umask(mask);
		errno=0;
		int ret=mkpath(trashfileslocation.text(),511 & ~mask);
		int errcode=errno;
		if (ret==-1)
		{
			if (errcode)
				MessageBox::error(this,BOX_OK,_("Error"),_("Can't create trash can 'files' folder %s : %s"),trashfileslocation.text(),strerror(errcode));
			else
				MessageBox::error(this,BOX_OK,_("Error"),_("Can't create trash can 'files' folder %s"),trashfileslocation.text());				
		}
	}
    
	// Create trash can info directory if it doesn't exist
	if (!::exists(trashinfolocation))
	{
		// Create the trash can info directory according to the umask
		mask=umask(0);
		umask(mask);
		errno=0;
		int ret=mkpath(trashinfolocation.text(),511 & ~mask);
		int errcode=errno;
		if (ret==-1)
		{
			if (errcode)
				MessageBox::error(this,BOX_OK,_("Error"),_("Can't create trash can 'info' folder %s: %s"),trashinfolocation.text(),strerror(errcode));
			else
				MessageBox::error(this,BOX_OK,_("Error"),_("Can't create trash can 'info' folder %s"),trashinfolocation.text());				
		}
	}

	// Eventually start iconic or maximized
	if (starticonic)
		minimize();
	if (startmaximized)
		maximize();			

#ifdef STARTUP_NOTIFICATION
	startup_completed();
#endif

}

// Destructor
XFileExplorer::~XFileExplorer()
{
    delete menubar;
	delete locationbar;
	delete address;
	delete filemenu;
	delete toolsmenu;
	delete trashmenu;
	delete editmenu;
	delete bookmarksmenu;
	delete viewmenu;
	delete lpanelmenu;
	delete rpanelmenu;
	delete helpmenu;
	delete filemenutitle;
	delete trashmenutitle;
	delete editmenutitle;
	delete bookmarksmenutitle;
	delete viewmenutitle;
	delete lpanelmenutitle;
	delete rpanelmenutitle;
	delete helpmenutitle;
    delete generaltoolbar;
	delete paneltoolbar;
	delete toolstoolbar;
    delete dirpanel;
    delete lpanel;
    delete rpanel;
    delete bookmarks;	
	delete btnbackhist;
	delete btnforwardhist;
	delete rundialog;
	delete prefsdialog;
	delete helpwindow;
}


// If Tab pressed, cycle through the panels 
long XFileExplorer::onKeyPress(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
	int current;
    
	// Tab was pressed : cycle through the panels from left to right
	if(event->code==KEY_Tab)
    {
		if (dirpanel->shown())
		{
			if (dirpanel->isActive())
			{
				lpanel->setFocusOnList();
				current=lpanel->getCurrentItem();
				if (current<0)
					current=0;
				lpanel->setCurrentItem(current);
				lpanel->selectItem(current);
			}
			else if ( (rpanel->shown()) && (lpanel->isActive()) )
			{
				rpanel->setFocusOnList();
				current=rpanel->getCurrentItem();
				if (current<0)
					current=0;
				rpanel->setCurrentItem(current);
				rpanel->selectItem(current);			
			}			
			else
			{
				dirpanel->setFocusOnList();
				current=lpanel->getCurrentItem();
				if (current<0)
					current=0;				
				lpanel->deselectItem(current);
			}
		}
		else if (rpanel->shown())
		{
			if (lpanel->getCurrent()==rpanel)
			{
				lpanel->setActive();
				current=lpanel->getCurrentItem();
				if (current<0)
					current=0;
				lpanel->setCurrentItem(current);
				lpanel->selectItem(current);
			}
			else
			{
				rpanel->setActive();
				current=rpanel->getCurrentItem();
				if (current<0)
					current=0;
				rpanel->setCurrentItem(current);
				rpanel->selectItem(current);
			}
		}
		return 1;
	}

	// Shift-Tab was pressed : cycle through the panels from right to left
	else if( ((event->state&SHIFTMASK) && (event->code==KEY_Tab))
	      || ((event->state&SHIFTMASK) && (event->code==KEY_ISO_Left_Tab)) )
    {		
		if (rpanel->shown())
		{
			if (rpanel->isActive())
			{
				lpanel->setFocusOnList();
				current=lpanel->getCurrentItem();
				if (current<0)
					current=0;
				lpanel->setCurrentItem(current);
				lpanel->selectItem(current);
			}
			else if (dirpanel->shown() && dirpanel->isActive())
			{
				rpanel->setFocusOnList();
				current=rpanel->getCurrentItem();
				if (current<0)
					current=0;
				rpanel->setCurrentItem(current);
				rpanel->selectItem(current);			
			}
			else if (lpanel->isActive())
			{
				if (dirpanel->shown())
				{
					dirpanel->setFocusOnList();
					current=lpanel->getCurrentItem();
					if (current<0)
						current=0;				
					lpanel->deselectItem(current);
				}
				else
				{
					rpanel->setFocusOnList();
					current=rpanel->getCurrentItem();
					if (current<0)
						current=0;
					rpanel->setCurrentItem(current);
					rpanel->selectItem(current);			
				}
			}
		}
		else
		{
			if (dirpanel->isActive() && dirpanel->shown())
			{
				lpanel->setFocusOnList();
				current=lpanel->getCurrentItem();
				if (current<0)
					current=0;
				lpanel->setCurrentItem(current);
				lpanel->selectItem(current);
			}
			else if (dirpanel->shown())
			{
				dirpanel->setFocusOnList();
				current=lpanel->getCurrentItem();
				if (current<0)
					current=0;				
				lpanel->deselectItem(current);
			}			
		}
		
		return 1;
	}

	// Shift-F10 was pressed : open popup menu
	else if(event->state&SHIFTMASK && event->code==KEY_F10)
    {
		lpanel->getCurrent()->handle(sender,FXSEL(SEL_COMMAND,FilePanel::ID_POPUP_MENU),ptr);
        return 1;
    }

	// Any other key was pressed : handle the pressed key in the usual way
    else
	{
		if(FXTopWindow::onKeyPress(sender,sel,ptr))
        	return 1;
	}

	return 0;

}


// Harvest the zombies
long XFileExplorer::onSigHarvest(FXObject*,FXSelector,void*)
{
	while(waitpid(-1,NULL,WNOHANG)>0);
	return 1;
}


// Handle quitting
long XFileExplorer::onQuit(FXObject*,FXSelector,void*)
{
    saveConfig();

	getApp()->exit(0);
    return 1;
}


// Directory up
long  XFileExplorer::onCmdDirUp(FXObject* sender,FXSelector,void*)
{
	lpanel->getCurrent()->handle(sender,FXSEL(SEL_COMMAND,FilePanel::ID_DIRECTORY_UP),NULL);

	// Set focus on dirpanel or filepanel
	if (panelfocus==DIRPANEL_FOCUS)
		dirpanel->setFocusOnList();
	else
		lpanel->getCurrent()->setFocusOnList();

    return 1;
}


// Directory back
long  XFileExplorer::onCmdDirBack(FXObject*,FXSelector,void*)
{
	StringItem *item;
	FXString pathname;
	FilePanel* filepanel=lpanel->getCurrent();
	
	// Get the previous directory
	item=filepanel->backhistGetFirst();
	if (item)
		pathname=filepanel->backhistGetString(item);
	
	// Update the history
	filepanel->backhistRemoveFirstItem();
	filepanel->forwardhistInsertFirstItem(filepanel->getDirectory());
	
	// Go to the previous directory
	filepanel->setDirectory(pathname,FALSE);
	filepanel->updatePath();
	dirpanel->setDirectory(pathname,TRUE);
	
	// Set focus on dirpanel or filepanel
	if (panelfocus==DIRPANEL_FOCUS)
		dirpanel->setFocusOnList();
	else
		filepanel->setFocusOnList();

    return 1;
}


// Update directory back
long  XFileExplorer::onUpdDirBack(FXObject* sender, FXSelector, void* ptr)
{
	FXString pathname;
	FilePanel* filepanel=lpanel->getCurrent();
	
	// Gray out the button if no item in history 
	if (filepanel->backhistGetNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);	
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory forward
long  XFileExplorer::onCmdDirForward(FXObject*,FXSelector,void*)
{
	StringItem *item;
	FXString pathname;
	FilePanel* filepanel=lpanel->getCurrent();

	// Get the next directory
	item=filepanel->forwardhistGetFirst();
	if (item)
		pathname=filepanel->forwardhistGetString(item);
	
	// Update the history
	filepanel->forwardhistRemoveFirstItem();
	filepanel->backhistInsertFirstItem(lpanel->getCurrent()->getDirectory());
	
	// Go to the next directory
	filepanel->setDirectory(pathname,FALSE);
	filepanel->updatePath();
	dirpanel->setDirectory(pathname,TRUE);

	// Set focus on dirpanel or filepanel
	if (panelfocus==DIRPANEL_FOCUS)
		dirpanel->setFocusOnList();
	else
		filepanel->setFocusOnList();

    return 1;
}


// Update directory forward
long  XFileExplorer::onUpdDirForward(FXObject* sender, FXSelector sel, void* ptr)
{
	FXString pathname;
	FilePanel* filepanel=lpanel->getCurrent();

	// Gray out the button if no item in history 
	if (filepanel->forwardhistGetNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory back history
long  XFileExplorer::onCmdDirBackHist(FXObject *sender,FXSelector sel,void* ptr)
{
	StringItem *item;
	FXString pathname;
	FilePanel* filepanel=lpanel->getCurrent();

	// Get all string items and display them in a list box
	int num=filepanel->backhistGetNumItems();
	if (num>0)
	{
		FXString *dirs= new FXString[num];
		FXString strlist="";
		
		// Get string items
		item=filepanel->backhistGetFirst();
		int nb=0;
		for(int i=0; i<=num-1; i++)
		{
			if (item)
			{
				FXString str=filepanel->backhistGetString(item);
				FXbool flag=TRUE;
				for (int j=0; j<=nb-1; j++)
				{
					if (str==dirs[j])
					{
						flag=FALSE;
						break;
					}
				}
				if (flag)
				{
					dirs[nb]=str;
					strlist=strlist+str+"\n";
					nb++;
				}
				item=filepanel->backhistGetNext(item);
			}
		}
		
		// Display list box
		int pos=DirHistBox::box(btnbackhist,DECOR_NONE,strlist,getX()+40,getY()+60);
		
		// If an item was selected
		if (pos!=-1)
		{
			// Update back history
			if (pos==num-1)
				filepanel->backhistRemoveAllItems();
			else
			{
				item=filepanel->backhistGetItemAtPos(pos+1);
				filepanel->backhistRemoveAllItemsBefore(item);
			}
			
			// Update forward history
			filepanel->forwardhistInsertFirstItem(filepanel->getDirectory());
			if (pos>0)
			{
				for (int i=0; i<=pos-1; i++)
					filepanel->forwardhistInsertFirstItem(dirs[i]);
			}
	
			// Go to to the selected directory
			pathname=dirs[pos];
			filepanel->setDirectory(pathname,FALSE);
			filepanel->updatePath();
			dirpanel->setDirectory(pathname,TRUE);
		}
		delete[]dirs;
	}

    return 1;
}


// Update directory back
long  XFileExplorer::onUpdDirBackHist(FXObject* sender, FXSelector sel, void* ptr)
{
	FXString pathname;
	FilePanel* filepanel=lpanel->getCurrent();

	// Gray out the button if no item in history 
	if (filepanel->backhistGetNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);	
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory forward history
long  XFileExplorer::onCmdDirForwardHist(FXObject *sender,FXSelector sel,void* ptr)
{
	StringItem *item;
	FXString pathname;
	FilePanel* filepanel=lpanel->getCurrent();

	// Get all string items and display them in a list box
	int num=filepanel->forwardhistGetNumItems();
	if (num>0)
	{
		FXString *dirs= new FXString[num];
		FXString strlist="";
		
		// Get string items
		item=filepanel->forwardhistGetFirst();
		int nb=0;
		for(int i=0; i<=num-1; i++)
		{
			if (item)
			{
				FXString str=filepanel->forwardhistGetString(item);
				FXbool flag=TRUE;
				for (int j=0; j<=nb-1; j++)
				{
					if (str==dirs[j])
					{
						flag=FALSE;
						break;
					}
				}
				if (flag)
				{
					dirs[nb]=str;
					strlist=strlist+str+"\n";
					nb++;
				}
				item=filepanel->forwardhistGetNext(item);
			}
		}
		
		// Display list box
		int pos=DirHistBox::box(btnforwardhist,DECOR_NONE,strlist,getX()+85,getY()+60);
		
		// If an item was selected
		if (pos!=-1)
		{
			// Update forward history
			if (pos==num-1)
				filepanel->forwardhistRemoveAllItems();
			else
			{
				item=filepanel->forwardhistGetItemAtPos(pos+1);
				filepanel->forwardhistRemoveAllItemsBefore(item);
			}
			
			// Update back history
			filepanel->backhistInsertFirstItem(filepanel->getDirectory());
			if (pos>0)
			{
				for (int i=0; i<=pos-1; i++)
					filepanel->backhistInsertFirstItem(dirs[i]);
			}
	
			// Go to to the selected directory
			pathname=dirs[pos];
			filepanel->setDirectory(pathname,FALSE);
			filepanel->updatePath();
			dirpanel->setDirectory(pathname,TRUE);
		}
		delete[]dirs;
	}

    return 1;
}


// Update directory forward
long  XFileExplorer::onUpdDirForwardHist(FXObject* sender, FXSelector sel, void* ptr)
{
	FXString pathname;
	FilePanel* filepanel=lpanel->getCurrent();

	// Gray out the button if no item in history 
	if (filepanel->forwardhistGetNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Handle bookmarks
long  XFileExplorer::onCmdBookmark(FXObject*, FXSelector s, void* p)
{
    if(FXSELID(s) == ID_ADD_BOOKMARK)
	{
        bookmarks->appendBookmark(lpanel->getCurrent()->getDirectory());
		saveConfig();
	}
	
	// Handle location address fields
	else if(FXSELID(s) == ID_BOOKMARK)
	{
        lpanel->getCurrent()->setDirectory((char*)p);
		lpanel->getCurrent()->updatePath();
        dirpanel->setDirectory((char*)p,TRUE);
        FXString item;
		int i=0;
		int count=address->getNumItems();
		if(!count)
		{
			count++;
			address->insertItem(0,address->getText());
		}
        while(i < count)
		{
        	item=address->getItem(i++);
        	if(streq((char*)p,(const char*)&item[0]))
			{
				i--;
				break;
			}
        }
        if(i==count) 
			address->insertItem(0,(char*)p);
			
		// Set focus to the active panel
		lpanel->getCurrent()->setFocusOnList();	
    }
    return 1;
}


// Goto location entered into the text field;
long XFileExplorer::onCmdGotoLocation(FXObject*,FXSelector,void*)
{
	// Location where we want to go
	FXString location=address->getText();
	
	// In case location is given in URI form, convert it
	location=::fileFromURI(location);
	
	// Get complete path
	FXString path=FXPath::absolute(lpanel->getCurrent()->getDirectory(),location);
	FXString dir=path;

	// Go up to the lowest directory which still exists
	while(!FXPath::isTopDirectory(dir) && !::isDirectory(dir))
    	dir=FXPath::upLevel(dir);

	// Move to this existing directory
	lpanel->getCurrent()->setDirectory(dir);
	lpanel->getCurrent()->updatePath();
	dirpanel->setDirectory(dir,TRUE);
	address->setText(dir);
	
	return 1;
}


// Clear location bar
long XFileExplorer::onCmdClearLocation(FXObject*,FXSelector,void*)
{
  address->setText(FXString::null);
  address->CursorEnd();
  return 1;
}

// Restart the application when required
long  XFileExplorer::onCmdRestart(FXObject*,FXSelector,void*)
{
    saveConfig();

    if(fork() == 0)
        execvp("xfe",args);
    else
        _exit(0);
    return 1;
}

// Start a new Xfe session
long  XFileExplorer::onCmdNewWindow(FXObject*,FXSelector,void*)
{
	FXString cmd="xfe " + homedir + " &";
	system(cmd.text());
    return 1;
}

// Run Terminal
long  XFileExplorer::onCmdXTerm(FXObject*,FXSelector,void*)
{
	getApp()->beginWaitCursor();
    FXString xterm=getApp()->reg().readStringEntry("PROGS","xterm","xterm -sb");
	chdir(lpanel->getCurrent()->getDirectory().text());
    FXString cmd=xterm;
    cmd += " &";
    system(cmd.text());
    lpanel->getCurrent()->setFocusOnList();
	chdir(startlocation.text());
	getApp()->endWaitCursor();
    return 1;
}


// Help menu
long XFileExplorer::onCmdHelp(FXObject*,FXSelector,void*)
{
    // Display help window
	if (helpwindow==NULL)
		helpwindow=new TextWindow(getApp(),_("Help"),40,120);			
	helpwindow->setIcon(helpicon);

    // Set text font
	FXString fontspec;
	fontspec=getApp()->reg().readStringEntry("SETTINGS","textfont","Helvetica,100,normal,regular");
	if(!fontspec.empty())
	{
    	FXFont* font=new FXFont(getApp(),fontspec);
        font->create();
        helpwindow->setFont(font);
	}

	// NB: The HELP_TEXT macro is defined in help.h
	FXString str=(FXString)"                         "+COPYRIGHT+HELP_TEXT;
	helpwindow->setText(str.text());
	// Non modal window
	helpwindow->create();
 	helpwindow->show(PLACEMENT_OWNER);
	lpanel->getCurrent()->setFocusOnList();
    return 1;
}


// About menu
long XFileExplorer::onCmdAbout(FXObject*,FXSelector,void*)
{
    FXString msg;
	msg.format(_("X File Explorer Version %s"),VERSION);
	FXString copyright=(FXString)"\n\n" + COPYRIGHT + "\n\n" + _("Based on X WinCommander by Maxim Baranov\n");
	FXString translators=
_("\nTranslators\n\
-------------\n\
Argentinian Spanish: Bruno Gilberto Luciani\n\
Brazilian Portuguese: Eduardo R.B.S., Jose Carlos Medeiros,\n\
Phantom X, Tomas Acauan Schertel\n\
Bosnian: Samir Ribi, Bajrami Emran, Balagija Jasmina,\n\
Bilalovi, Omar Cogo Emir\n\
Catalan: muzzol\n\
Chinese: Xin Li\n\
Chinese (Taïwan): Wei-Lun Chao\n\
Czech: David Vachulka\n\
Danish: Jonas Bardino, Vidar Jon Bauge\n\
Dutch: Hans Strijards\n\
French: Claude Leo Mercier, Roland Baudin\n\
German: Bastian Kleineidam, Joachim Wiedorn, Tim Benke, Jens Körner\n\
Greek: Nikos Papadopoulos\n\
Hungarian: Attila Szervac, Sandor Sipos\n\
Italian: Claudio Fontana, Giorgio Moscardi\n\
Japanese: Karl Skewes\n\
Norwegian: Vidar Jon Bauge\n\
Polish: Jacek Dziura, Franciszek Janowski\n\
Portuguese: Miguel Santinho\n\
Russian: Dimitri Sertolov, Vad Vad\n\
Spanish: Felix Medrano Sanz, Lucas 'Basurero' Vieites,\n\
Martin Carr\n\
Swedish: Anders F. Bjorklund\n\
Turkish: erkaN\n\
");

	msg = msg + copyright + translators;
    MessageBox about(this,_("About X File Explorer"),msg.text(),xfeicon,BOX_OK|DECOR_TITLE|DECOR_BORDER,
	                 JUSTIFY_CENTER_X|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);	
	about.execute(PLACEMENT_OWNER);
    lpanel->getCurrent()->setFocusOnList();
    return 1;
}


// Handle file association (called by Properties.cc and FilePanel.cc)
long XFileExplorer::onCmdFileAssoc(FXObject*,FXSelector s,void *p)
{
	char **str=(char**)p;
    char* ext=str[0];
    char* cmd=str[1];

	// ext=extension, cmd=associated command
	// replace : to allow immediate association in Xfe
    FileDict *associations=lpanel->getAssociations();
	associations->replace(ext,cmd);
    associations=rpanel->getAssociations();
    associations->replace(ext,cmd);

    saveConfig();
	
    return 1;
}


// FilePanel and DirPanel refresh
long XFileExplorer::onCmdRefresh(FXObject*,FXSelector,void*)
{
	getApp()->beginWaitCursor();
	
#if defined(linux)
	dirpanel->forceDevicesRefresh();
#endif
	lpanel->getCurrent()->onCmdRefresh(0,0,0);
	dirpanel->forceRefresh();
	getApp()->endWaitCursor();
	return 1;
}


// Update file location
long XFileExplorer::onUpdFileLocation(FXObject* sender,FXSelector,void*)
{
	FXString currentdir=lpanel->getCurrent()->getDirectory();
	if (currentdir != prevdir)
	{
		address->setText(::cleanPath(currentdir));
		prevdir=currentdir;
	}
	return 1;
}


// Switch between the four possible panel views
long XFileExplorer::onCmdShowPanels(FXObject* sender,FXSelector sel,void* ptr)
{
	// Get window width
	int window_width=getWidth();

    switch(FXSELID(sel))
    {
    case ID_SHOW_ONE_PANEL:
		panel_view=ONE_PANEL;
		if (dirpanel->shown())
    		dirpanel->handle(sender,FXSEL(SEL_COMMAND,DirPanel::ID_TOGGLE_TREE),ptr);
		if (rpanel->shown())
			rpanel->handle(sender,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),ptr);	
		// Handle drag corner 
		rpanel->showCorner(FALSE);
		lpanel->showCorner(TRUE);
        // Handle active icon
        lpanel->showActiveIcon(FALSE);
        
        break;

	case ID_SHOW_TWO_PANELS:
		panel_view=TWO_PANELS;
		lpanel->setWidth((int)round(twopanels_lpanel_pct*window_width));
		if (dirpanel->shown())
    		dirpanel->handle(sender,FXSEL(SEL_COMMAND,DirPanel::ID_TOGGLE_TREE),ptr);
		if (!rpanel->shown())
			rpanel->handle(sender,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),ptr);	
		// Handle drag corner 
		rpanel->showCorner(TRUE);
		lpanel->showCorner(FALSE);
        // Handle active icon
        lpanel->showActiveIcon(TRUE);
        break;
    
	case ID_SHOW_TREE_PANEL:
		panel_view=TREE_PANEL;
		dirpanel->setWidth((int)round(treepanel_tree_pct*window_width) );
		if (!dirpanel->shown())
    		dirpanel->handle(sender,FXSEL(SEL_COMMAND,DirPanel::ID_TOGGLE_TREE),ptr);
		if (rpanel->shown())
			rpanel->handle(sender,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),ptr);	
		// Handle drag corner 
		rpanel->showCorner(FALSE);
		lpanel->showCorner(TRUE);
        // Handle active icon
        lpanel->showActiveIcon(TRUE);
		break;
    
	case ID_SHOW_TREE_TWO_PANELS:
		panel_view=TREE_TWO_PANELS;
		dirpanel->setWidth((int)round(treetwopanels_tree_pct*window_width) );
		lpanel->setWidth((int)round(treetwopanels_lpanel_pct*window_width) );
		if (!dirpanel->shown())
    		dirpanel->handle(sender,FXSEL(SEL_COMMAND,DirPanel::ID_TOGGLE_TREE),ptr);
		if (!rpanel->shown())
			rpanel->handle(sender,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),ptr);	
		// Handle drag corner 
		lpanel->showCorner(FALSE);
		rpanel->showCorner(TRUE);
        // Handle active icon
        lpanel->showActiveIcon(TRUE);
        break;
    }
	
	// Set focus on current panel
	lpanel->getCurrent()->setFocusOnList();
	
	return 1;
}


// Update the panels
long XFileExplorer::onUpdShowPanels(FXObject* sender,FXSelector sel,void* ptr)
{		
	// Keep the panel sizes relative to the window width (if option enabled)
	
	register int width;
	
	// Get the current window width
	width=getWidth();
	
	// If width has changed and relative resizing option is enabled
	if (relative_resize && prev_width!=width)
	{
		// One panel mode not relevant
		
		// Two panels mode
		if (!dirpanel->shown() && rpanel->shown())
		{
			// Set left panel width to the new value
			lpanel->setWidth((int)round(twopanels_lpanel_pct*width));
		}
		
		// Tree panel mode
		else if (dirpanel->shown() && !rpanel->shown())
		{
			// Set dirpanel width to the new value
			dirpanel->setWidth((int)round(treepanel_tree_pct * width) );
		}
		
		// Tree and two panels mode
		else if (dirpanel->shown() && rpanel->shown())
		{
			// Set dirpanel width to the new value
			dirpanel->setWidth((int)round(treetwopanels_tree_pct * width) );
			
			// Set left panel width to the new value
			lpanel->setWidth((int)round(treetwopanels_lpanel_pct * width) );
		}
	}
			
	// Update previous window width
	prev_width=width;			
		

	// Update the panel menus and the panel display
	unsigned int msg=FXWindow::ID_UNCHECK;
    switch(FXSELID(sel))
    {
    case ID_SHOW_ONE_PANEL:
		if (!dirpanel->shown() && !rpanel->shown())
		{
            msg = FXWindow::ID_CHECK;		
        	if(rpanelmenutitle->shown())
        	{
            	rpanelmenutitle->hide();
             	rpanelmenutitle->disable();
           		lpanelmenutitle->setText(_("&Panel"));
				lpanel->show();
				lpanel->repaint();
				lpanel->setActive();
        	}
		}
        break;
    
	case ID_SHOW_TWO_PANELS:
		if (!dirpanel->shown() && rpanel->shown())
		{
			// Update the left panel relative size (only if the window size is sufficient)
			if (getWidth()>10)
				twopanels_lpanel_pct=(double)(lpanel->getWidth())/(double)(getWidth());
            
			msg=FXWindow::ID_CHECK;
        	if(!rpanelmenutitle->shown())
        	{
             	rpanelmenutitle->enable();
            	rpanelmenutitle->show();
            	rpanelmenutitle->setText(_("&Right panel"));
            	lpanelmenutitle->setText(_("&Left panel"));
				lpanel->repaint();
				lpanel->setActive();
        	}
		}
        break;
    
	case ID_SHOW_TREE_PANEL:
 		if (dirpanel->shown() && !rpanel->shown())
		{
			// Update the tree panel relative size (only if the window size is sufficient)
			if (getWidth()>10)
				treepanel_tree_pct=(double)(dirpanel->getWidth())/(double)(getWidth());
			
           	msg=FXWindow::ID_CHECK;		
        	if(rpanelmenutitle->shown())
        	{
            	rpanelmenutitle->hide();
             	rpanelmenutitle->disable();
            	lpanelmenutitle->setText(_("&Panel"));
				lpanel->repaint();
				lpanel->setActive();
       	 	}
		}
        break;
    
	case ID_SHOW_TREE_TWO_PANELS:
 		if (dirpanel->shown() && rpanel->shown())
		{
			// Update the tree panel relative size (only if the window size is sufficient)
			if (getWidth()>10)
				treetwopanels_tree_pct=(double)(dirpanel->getWidth())/(double)(getWidth());

			// Update the left panel relative size (only if the window size is sufficient)
			if (getWidth()>10)
				treetwopanels_lpanel_pct=(double)(lpanel->getWidth())/(double)(getWidth());

            msg = FXWindow::ID_CHECK;
        	if(!rpanelmenutitle->shown())
        	{
             	rpanelmenutitle->enable();
           	 	rpanelmenutitle->show();
            	rpanelmenutitle->setText(_("&Right panel"));
            	lpanelmenutitle->setText(_("&Left panel"));
				lpanel->repaint();
				lpanel->setActive();
        	}
		}
        break;
    }	
    sender->handle(this,FXSEL(SEL_COMMAND,msg),ptr);

    return 1;
}


// Synchronize the panels to the same directory
long XFileExplorer::onCmdSynchronizePanels(FXObject* sender,FXSelector sel,void*)
{
	FXString dir;
	
	// Left panel is active
	if (lpanel->getCurrent() == lpanel)
	{
		dir=lpanel->getDirectory();
		rpanel->setDirectory(dir);
		rpanel->updatePath();
	}
	
	// Right panel is active 
	else
	{
		dir=rpanel->getDirectory();
		lpanel->setDirectory(dir);
		lpanel->updatePath();
		
	}
	return 1;
}


// Update the synchronize panels menu item
long XFileExplorer::onUpdSynchronizePanels(FXObject* o,FXSelector,void*)
{
	if (rpanel->shown())
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

	return 1;
}


// Switch the panels
long XFileExplorer::onCmdSwitchPanels(FXObject* sender,FXSelector sel,void*)
{
	FXString leftdir, rightdir;
	
	leftdir=lpanel->getDirectory();
	rightdir=rpanel->getDirectory();
	lpanel->setDirectory(rightdir);
	lpanel->updatePath();
	rpanel->setDirectory(leftdir);
	rpanel->updatePath();

	return 1;
}


// Update the switch panels menu item
long XFileExplorer::onUpdSwitchPanels(FXObject* o,FXSelector,void*)
{
	if (rpanel->shown())
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

	return 1;
}


// Preferences
long XFileExplorer::onCmdPrefs(FXObject*,FXSelector s,void *p)
{
    if (prefsdialog==NULL)
		prefsdialog=new PreferencesBox(this,listbackcolor,listforecolor,highlightcolor,pbarcolor,attentioncolor,scrollbarcolor);
    prefsdialog->execute(PLACEMENT_OWNER);
    lpanel->getCurrent()->setFocusOnList();
    return 1;
}


// Toggle status bar
long XFileExplorer::onCmdToggleStatus(FXObject*,FXSelector s,void *p)
{
    dirpanel->toggleStatusbar();
    lpanel->toggleStatusbar();
    rpanel->toggleStatusbar();
    return 1;
}


long XFileExplorer::onUpdToggleStatus(FXObject *o,FXSelector s,void *p)
{
    FXMenuCheck *cmd =(FXMenuCheck*)o;
    if(lpanel->statusbarShown())
        cmd->setCheck(TRUE);
    else
        cmd->setCheck(FALSE);
    return 1;
}


// Run shell command or X program
long XFileExplorer::onCmdRun(FXObject*,FXSelector,void*)
{
    chdir(lpanel->getCurrent()->getDirectory().text());
    FXString command="";
    if (rundialog==NULL)
		rundialog=new HistInputDialog(this,"",_("Execute the command:"),_("Execute command"),"", NULL,HIST_INPUT_EXECUTABLE_FILE,TRUE, _("Console mode"));
	rundialog->create();
	rundialog->setText(command);
    rundialog->CursorEnd();
    rundialog->SelectAll();
    rundialog->clearItems();

	for(int i=0;i<RunHistSize;i++)
        rundialog->appendItem(RunHistory[i]);

	rundialog->setDirectory(ROOTDIR);
    if(rundialog->execute())
    {
        command=rundialog->getText();
        if(command != "")
        {
			// Execute command in command window
			if (rundialog->getOption())
			{
    			// Make and show command window
				CommandWindow *cmdwin=new CommandWindow(getApp(),_("Command log"),command,30,80);			
				cmdwin->create();
				cmdwin->setIcon(runicon);

				// The CommandWindow object will delete itself when closed!
			}
			
			// Execute silently in background
			else
			{
				command+=" &";
				system(command.text());
			}
        }
		// Update history list
		RunHistSize=rundialog->getHistorySize();
		command=rundialog->getText();
		
		// Check if cmd is a new string, i.e. is not already in history
		FXbool newstr=TRUE;
		for (int i=0;i<RunHistSize-1;i++)
		{
			if (streq(RunHistory[i],command.text()))
			{
				newstr=FALSE;
				break;
			}
		}
		
		// No new string or history limit reached
		if (!newstr || RunHistSize>RUN_HIST_SIZE)
			RunHistSize--;
		
		// New string
		if (newstr)
		{
			// FIFO
			strlcpy(RunHistory[0],command.text(),command.length()+1);
			for(int i=1;i<RunHistSize;i++)
				strlcpy(RunHistory[i],rundialog->getHistoryItem(i-1).text(),rundialog->getHistoryItem(i-1).length()+1);
		}
    }
	chdir(startlocation.text());
    lpanel->getCurrent()->setFocusOnList();
    return 1;
}


// Run an Xfe as root
long XFileExplorer::onCmdSu(FXObject*,FXSelector,void*)
{
	// Wait cursor
	getApp()->beginWaitCursor();

	// Obtain preferred root mode
	FXbool use_sudo=getApp()->reg().readUnsignedEntry("OPTIONS","use_sudo",FALSE);

	// Select sudo or su to launch xfe as root
	chdir(lpanel->getCurrent()->getDirectory().text());
	FXString title, sucmd;
	if (use_sudo)
	{
		title = _("Enter the user password:");
		sucmd = SUDOCMD;
	}
	else
	{
		title = _("Enter the root password:");
		sucmd = SUCMD;
	}
	
	// Use appropriate background and foreground colors for Xvt
	char color[64];
	fxnamefromcolor(color,getApp()->getBackColor());
	FXString bg=" -bg ";
	bg = bg + color;
	fxnamefromcolor(color,getApp()->getForeColor());
	FXString fg=" -fg ";
	fg = fg + color + " ";
	
	// Command string
	FXString command = "xvt -title " + ::quote(title) + bg + fg + sucmd;
	
	// Execute su or sudo command in an internal Xvt terminal
	int status=runinxvt(command);
	
	// If error
	chdir(startlocation.text());
	if (status<0)
	{
		MessageBox::error(getApp(),BOX_OK,_("Error"),_("An error has occurred!"));
		getApp()->endWaitCursor();
		return 0;
	}	
	
 	// Wait cursor
	getApp()->endWaitCursor();
    return 1;
}


// Empty trash can
long XFileExplorer::onCmdEmptyTrash(FXObject*,FXSelector sel,void* ptr)
{
    // Confirmation message
	FXString message=_("Do you really want to empty the trash can?\n\nAll items will be definitively lost!");
    MessageBox box(this,_("Empty trash can"),message,trash_full_bigicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);
 	if(box.execute(PLACEMENT_CURSOR) != BOX_CLICKED_OK)
    	return 0;

	// Wait cursor
	getApp()->beginWaitCursor();

	// Delete trash can files folder	
   	File* f;
	f=new File(this,_("File delete"),DELETE);
   	f->create();
	f->remove(trashfileslocation);
	delete f;

	// Delete trash can info folder	
   	f=new File(this,_("File delete"),DELETE);
   	f->create();
	f->remove(trashinfolocation);
	delete f;
	
	// Re-create the trash can files directory
	if (!::exists(trashfileslocation))
	{
		errno=0;
		int ret=mkpath(trashfileslocation.text(),0755);
		int errcode=errno;
		if (ret==-1)
		{
			if (errcode)
				MessageBox::error(this,BOX_OK,_("Error"),_("Can't create trash can 'files' folder %s: %s"),trashfileslocation.text(),strerror(errcode));
			else
				MessageBox::error(this,BOX_OK,_("Error"),_("Can't create trash can 'files' folder %s"),trashfileslocation.text());
		}
	}
 
	// Re-create the trash can info directory
	if (!::exists(trashinfolocation))
	{
		errno=0;
		int ret=mkpath(trashinfolocation.text(),0755);
		int errcode=errno;
		if (ret==-1)
		{
			if (errcode)
				MessageBox::error(this,BOX_OK,_("Error"),_("Can't create trash can 'info' folder %s: %s"),trashinfolocation.text(),strerror(errcode));
			else
				MessageBox::error(this,BOX_OK,_("Error"),_("Can't create trash can 'info' folder %s"),trashinfolocation.text());
		}
	}

 	// Wait cursor
	getApp()->endWaitCursor();
	
	onCmdRefresh(0,0,0);
	
	return 1;
}


// Display trash size
long XFileExplorer::onCmdTrashSize(FXObject*,FXSelector sel,void*)
{
	struct stat linfo;
	FXString trashsize, trashmtime, trashnbfiles, trashnbfolders;
	if (lstatrep(trashfileslocation.text(),&linfo)==0)
	{
		// Read time format
		FXString timeformat=getApp()->reg().readStringEntry("SETTINGS","time_format",DEFAULT_TIME_FORMAT);

		// Trash files size
		trashmtime=FXSystem::time(timeformat.text(),linfo.st_mtime);
		char buf[MAXPATHLEN];
		FXulong dirsize=0;
		unsigned int nbfiles=0, nbsubfolders=0;
		strlcpy(buf,trashfileslocation.text(),trashfileslocation.length()+1);
		dirsize=pathsize(buf,&nbfiles,&nbsubfolders);
		snprintf(buf,sizeof(buf),"%llu",dirsize);
		trashsize=::hSize(buf);
		trashnbfiles=FXStringVal(nbfiles-nbsubfolders);
		trashnbfolders=FXStringVal(nbsubfolders-1);
		
		// Dialog box
		FXString msg;
		msg.format(_("Trash size: %s (%s files, %s subfolders)\n\nModified date: %s"),
		            trashsize.text(),trashnbfiles.text(),trashnbfolders.text(),trashmtime.text());
	    MessageBox dialog(this,_("Trash size"),msg.text(),delete_bigicon,BOX_OK|DECOR_TITLE|DECOR_BORDER,
	                 JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);	
		dialog.execute(PLACEMENT_CURSOR);
	}
	else
	{
		// Error
		MessageBox::error(this,BOX_OK,_("Error"),_("Trash can 'files' folder %s is not readable!"),trashfileslocation.text());
		return 0;
	}
		
	return 1;
}


// File copy to clipboard
long XFileExplorer::onCmdFileCopyClp(FXObject* o, FXSelector sel, void*)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_COPY_CLIPBOARD),NULL);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_COPY_CLIPBOARD),NULL);	
	
	return 1;
}


// File cut to clipboard
long XFileExplorer::onCmdFileCutClp(FXObject* o, FXSelector sel, void*)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_CUT_CLIPBOARD),NULL);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_CUT_CLIPBOARD),NULL);	
	
	return 1;
}


// File paste from clipboard
long XFileExplorer::onCmdFilePasteClp(FXObject* o, FXSelector sel, void*)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_PASTE_CLIPBOARD),NULL);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_PASTE_CLIPBOARD),NULL);	
	return 1;
}


// File rename
long XFileExplorer::onCmdFileRename(FXObject* o, FXSelector sel, void*)
{
	lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_FILE_RENAME),NULL);	
	return 1;
}


// File move
long XFileExplorer::onCmdFileMoveto(FXObject* o, FXSelector sel, void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_DIR_MOVETO),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_FILE_MOVETO),NULL);	
	return 1;
}


// File copy to
long XFileExplorer::onCmdFileCopyto(FXObject* o, FXSelector sel, void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_DIR_COPYTO),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_FILE_COPYTO),NULL);	

	return 1;
}


// File symlink
long XFileExplorer::onCmdFileSymlink(FXObject* o, FXSelector sel, void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_DIR_SYMLINK),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_FILE_SYMLINK),NULL);	

	return 1;
}


// File trash
long XFileExplorer::onCmdFileTrash(FXObject* o, FXSelector sel, void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_DIR_TRASH),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_FILE_TRASH),ptr);	
	
	return 1;
}


// File restore
long XFileExplorer::onCmdFileRestore(FXObject* o,FXSelector sel,void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_DIR_RESTORE),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_FILE_RESTORE),ptr);	
	
	return 1;
}


// File delete
long XFileExplorer::onCmdFileDelete(FXObject* o,FXSelector sel,void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_DIR_DELETE),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_FILE_DELETE),ptr);
	
	return 1;
}


// File properties
long XFileExplorer::onCmdFileProperties(FXObject* o,FXSelector sel,void*)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_COMMAND,DirPanel::ID_PROPERTIES),NULL);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_COMMAND,FilePanel::ID_PROPERTIES),NULL);	

	return 1;
}


// Update the empty trash can and trash menus
long XFileExplorer::onUpdEmptyTrash(FXObject* o,FXSelector,void* ptr)
{

	FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
	if (use_trash_can)
	{
		// Update the empty trash can menu
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);

		// Update the trash menu title	
		helpmenutitle->setText("");
		trashmenutitle->setText(_("T&rash"));
		trashmenutitle->enable();
		trashmenutitle->show();
		helpmenutitle->setText(_("&Help"));
	}
	else
	{
		// Update the empty trash can menu
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

		// Update the trash menu title
		trashmenutitle->hide();
		trashmenutitle->disable();
		helpmenutitle->setText("");
		helpmenutitle->setText(_("&Help"));
	}

    return 1;
}


// Update the trash size menu
long XFileExplorer::onUpdTrashSize(FXObject* o,FXSelector,void*)
{
	FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
	if (use_trash_can)
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

    return 1;
}


// Update the file delete menu item
long XFileExplorer::onUpdFileDelete(FXObject* o,FXSelector,void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_UPDATE,DirPanel::ID_DIR_DELETE),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_UPDATE,FilePanel::ID_FILE_DELETE),ptr);

	return 1;
}


// Update the move to trash menu item
long XFileExplorer::onUpdFileTrash(FXObject* o,FXSelector,void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_UPDATE,DirPanel::ID_DIR_TRASH),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_UPDATE,FilePanel::ID_FILE_TRASH),ptr);

   return 1;
}


// Update the restore from trash menu item
long XFileExplorer::onUpdFileRestore(FXObject* o,FXSelector,void* ptr)
{
	if (dirpanel->isActive())
		dirpanel->handle(o,FXSEL(SEL_UPDATE,DirPanel::ID_DIR_RESTORE),ptr);
	
	else
		lpanel->getCurrent()->handle(o,FXSEL(SEL_UPDATE,FilePanel::ID_FILE_RESTORE),ptr);

   return 1;
}


// Update the file operation menu items
long XFileExplorer::onUpdFileMan(FXObject* o,FXSelector,void*)
{
	// Update the panelfocus variable
	if (lpanel->getCurrent()->isActive())
		panelfocus=FILEPANEL_FOCUS;
	if (dirpanel->isActive())
		panelfocus=DIRPANEL_FOCUS;

	// Update the file operation menu items
	if (dirpanel->isActive())
	{
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	}
	else
	{
		int num=lpanel->getCurrent()->getNumSelectedItems();
		if (num==0)
			o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
		else if (num==1 && lpanel->getCurrent()->isItemSelected(0))
			o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
		else
			o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);		
	}

	return 1;
}


// Update the file rename menu items
long XFileExplorer::onUpdFileRename(FXObject* o,FXSelector,void*)
{
	int num=lpanel->getCurrent()->getNumSelectedItems();
	if (num==1)
	{
		if (lpanel->getCurrent()->isItemSelected(0))
			o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
		else
			o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	}
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
	return 1;
}


// Update the paste menu and button
long XFileExplorer::onUpdFilePaste(FXObject* o,FXSelector sel,void*)
{
	lpanel->getCurrent()->handle(o,FXSEL(SEL_UPDATE,FilePanel::ID_PASTE_CLIPBOARD),NULL);	
	return 1;
}


// Update the root menu items
long XFileExplorer::onUpdSu(FXObject* o,FXSelector,void*)
{
	FXbool root_mode=getApp()->reg().readUnsignedEntry("OPTIONS","root_mode",TRUE);

	if (!root_mode || getuid()==0)
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	
	return 1;
}

