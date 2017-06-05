#ifndef XFILEEXPLORER_H
#define XFILEEXPLORER_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "xfedefs.h"
#include "xfeutils.h"
#include "FileDict.h"
#include "FilePanel.h"
#include "InputDialog.h"
#include "HistInputDialog.h"
#include "BrowseInputDialog.h"
#include "Properties.h"
#include "DirPanel.h"
#include "Bookmarks.h"
#include "Preferences.h"
#include "TextWindow.h"


// Helper function
void toolbarSeparator(FXToolBar*);

// Application object
class XFileExplorer : public FXMainWindow
{
    FXDECLARE(XFileExplorer)
protected:
	enum
	{
		TREE_PANEL,
		ONE_PANEL,
		TWO_PANELS,
		TREE_TWO_PANELS,
		FILEPANEL_FOCUS,
		DIRPANEL_FOCUS,
	};	
	int panel_view;
    int RunHistSize;
    char RunHistory[RUN_HIST_SIZE][MAX_COMMAND_SIZE];
	FXMenuBar		*menubar;
    FXMenuPane		*toolsmenu;
    FXMenuPane		*filemenu;
    FXMenuPane		*trashmenu;
    FXMenuPane		*editmenu;
    FXMenuPane		*bookmarksmenu;
    FXMenuPane		*viewmenu;
	FXMenuPane		*lpanelmenu;
    FXMenuPane		*rpanelmenu;
    FXMenuPane		*helpmenu;
    FXMenuTitle		*toolsmenutitle;
    FXMenuTitle		*filemenutitle;
    FXMenuTitle		*trashmenutitle;
    FXMenuTitle		*editmenutitle;
    FXMenuTitle		*bookmarksmenutitle;
    FXMenuTitle		*viewmenutitle;
	FXMenuTitle		*lpanelmenutitle;
    FXMenuTitle		*rpanelmenutitle;
    FXMenuTitle		*helpmenutitle;
    Bookmarks		*bookmarks;
    FXToolBar		*generaltoolbar;
    FXToolBar		*toolstoolbar;
    FXToolBar		*paneltoolbar;
    FXToolBar		*locationbar;
	ComboBox		*address;
    FXStatusBar		*status;
    DirPanel		*dirpanel;
    FilePanel		*lpanel;
    FilePanel		*rpanel;
	FXString		trashfileslocation;
	FXString		trashinfolocation;
	FXString		startlocation;
	unsigned int			liststyle;
	FXColor			listbackcolor;
	FXColor			listforecolor;
	FXColor			highlightcolor;
	FXColor			pbarcolor;
	FXColor			attentioncolor;
	FXColor			scrollbarcolor;
	FXArrowButton   *btnbackhist;
	FXArrowButton   *btnforwardhist;
	HistInputDialog*   rundialog;
	PreferencesBox*    prefsdialog;
	TextWindow*		   helpwindow;
	FXString 		message;
	unsigned int 			panelfocus;
	FXString		startdirectory1;
	FXString		startdirectory2;
	FXbool			starticonic;
	FXbool			startmaximized;
	FXbool			smoothscroll;
	double        twopanels_lpanel_pct;      // Panel sizes, relatively to the window width (in percent)
	double        treepanel_tree_pct;
	double        treetwopanels_tree_pct;
	double        treetwopanels_lpanel_pct;
	FXString        prevdir;
	int			prev_width;
public:
	enum{
        ID_ABOUT=FXMainWindow::ID_LAST,
		ID_HELP,
		ID_REFRESH,
		ID_EMPTY_TRASH,
		ID_TRASH_SIZE,
        ID_XTERM,
		ID_DIR_UP,
		ID_DIR_BACK,
		ID_DIR_FORWARD,
		ID_DIR_BACK_HIST,
		ID_DIR_FORWARD_HIST,
		ID_FILE_PROPERTIES,
        ID_FILE_COPY,
        ID_FILE_RENAME,
        ID_FILE_MOVETO,
        ID_FILE_COPYTO,
        ID_FILE_CUT,
		ID_FILE_PASTE,
        ID_FILE_SYMLINK,
        ID_FILE_DELETE,
        ID_FILE_TRASH,
        ID_FILE_RESTORE,
        ID_FILE_ASSOC,
		ID_CLEAR_LOCATION,
		ID_GOTO_LOCATION,
        ID_RUN,
        ID_SU,
        ID_PREFS,
        ID_DIR_BOX,
        ID_TOGGLE_STATUS,
		ID_SHOW_ONE_PANEL,
		ID_SHOW_TWO_PANELS,
		ID_SHOW_TREE_PANEL,
		ID_SHOW_TREE_TWO_PANELS,
		ID_SYNCHRONIZE_PANELS,
		ID_SWITCH_PANELS,
        ID_RESTART,
        ID_NEW_WIN,
        ID_BOOKMARK,
        ID_ADD_BOOKMARK,
        ID_HARVEST,
        ID_QUIT,
		ID_LAST
    };
public:
    XFileExplorer(FXApp *app=NULL,const FXbool iconic=FALSE,const FXbool maximized=FALSE,const FXString startdir1 = "",const FXString startdir2 = "",const char *title = "X File Explorer",FXIcon *bigicon=NULL,FXIcon *miniicon=NULL);
    virtual void create();
    ~XFileExplorer();
    void saveConfig();
	long onSigHarvest(FXObject*,FXSelector,void*);
    long onQuit(FXObject*,FXSelector,void*);
	long onKeyPress(FXObject*,FXSelector,void*);
	long onCmdHelp(FXObject*,FXSelector,void*);
    long onCmdAbout(FXObject*,FXSelector,void*);
    long onCmdFileAssoc(FXObject*,FXSelector,void*);
	long onCmdRefresh(FXObject*,FXSelector,void*);
    long onCmdToggleStatus(FXObject*,FXSelector,void*);
    long onCmdPopupMenu(FXObject*,FXSelector,void*);
    long onCmdPrefs(FXObject*,FXSelector,void*);
    long onCmdRun(FXObject*,FXSelector,void*);
	long onCmdSu(FXObject*,FXSelector,void*);
    long onCmdXTerm(FXObject*,FXSelector,void*);
	long onCmdEmptyTrash(FXObject*,FXSelector,void*);
	long onCmdTrashSize(FXObject*,FXSelector,void*);
	long onCmdShowPanels(FXObject*,FXSelector,void*);
    long onCmdRestart(FXObject*,FXSelector,void*);
	long onCmdNewWindow(FXObject*,FXSelector,void*);
    long onCmdBookmark(FXObject*,FXSelector,void*);
	long onCmdGotoLocation(FXObject*,FXSelector,void*);
	long onCmdClearLocation(FXObject*,FXSelector,void*);
    long onUpdToggleStatus(FXObject*,FXSelector,void*);
	long onUpdShowPanels(FXObject*,FXSelector,void*);
	long onUpdFileLocation(FXObject*,FXSelector,void*);
	long onUpdEmptyTrash(FXObject*,FXSelector,void*);
	long onUpdTrashSize(FXObject*,FXSelector,void*);
	long onCmdFileDelete(FXObject*,FXSelector,void*);
	long onCmdFileTrash(FXObject*,FXSelector,void*);
	long onCmdFileRestore(FXObject*,FXSelector,void*);
	long onUpdFileDelete(FXObject*,FXSelector,void*);
	long onUpdFileTrash(FXObject*,FXSelector,void*);
	long onUpdFileRestore(FXObject*,FXSelector,void*);
	long onCmdDirUp(FXObject*,FXSelector,void*);
	long onCmdDirBack(FXObject*,FXSelector,void*);
	long onUpdDirBack(FXObject*,FXSelector,void*);
	long onCmdDirForward(FXObject*,FXSelector,void*);
	long onUpdDirForward(FXObject*,FXSelector,void*);
	long onCmdDirBackHist(FXObject*,FXSelector,void*);
	long onUpdDirBackHist(FXObject*,FXSelector,void*);
	long onCmdDirForwardHist(FXObject*,FXSelector,void*);
	long onUpdDirForwardHist(FXObject*,FXSelector,void*);
	long onCmdFileCopyClp(FXObject*,FXSelector,void*);
	long onCmdFileCutClp(FXObject*,FXSelector,void*);
	long onCmdFilePasteClp(FXObject*,FXSelector,void*);
	long onCmdFileRename(FXObject*,FXSelector,void*);
	long onCmdFileMoveto(FXObject*,FXSelector,void*);
	long onCmdFileCopyto(FXObject*,FXSelector,void*);
	long onCmdFileSymlink(FXObject*,FXSelector,void*);
	long onUpdFileMan(FXObject*,FXSelector,void*);
	long onUpdFilePaste(FXObject*,FXSelector,void*);
	long onCmdFileProperties(FXObject*,FXSelector,void*);
	long onUpdFileRename(FXObject*,FXSelector,void*);
	long onCmdSynchronizePanels(FXObject*,FXSelector,void*);
	long onUpdSynchronizePanels(FXObject*,FXSelector,void*);
	long onCmdSwitchPanels(FXObject*,FXSelector,void*);
	long onUpdSwitchPanels(FXObject*,FXSelector,void*);
	long onUpdSu(FXObject*,FXSelector,void*);
public:
    // Get associations
    FileDict *getAssociations()
    {
        return lpanel->getCurrent()->getAssociations();
    }

	// Change to selected directory
	void setDirectory(FXString pathname)
	{
		lpanel->getCurrent()->setDirectory(pathname,FALSE);
		lpanel->getCurrent()->updatePath();
	}

	// Change default cursor for file and dir panels
	void setDefaultCursor(FXCursor *cur)
	{
		lpanel->setDefaultCursor(cur);
		rpanel->setDefaultCursor(cur);
		dirpanel->setDefaultCursor(cur);
	}

	// Redraw file lists
	void redrawPanels(void)
	{
		lpanel->redraw();
		rpanel->redraw();
	}

	// Return a pointer on the current file panel
	FilePanel* getCurrentPanel(void)
	{
		return lpanel->getCurrent();	
	}

	// Return a pointer on the next file panel
	FilePanel* getNextPanel(void)
	{
		return lpanel->getNext();	
	}

	// Return the address box (location bar)
	FXComboBox* getAddressBox(void)
	{
		return address;
	}

	// Return a pointer on the directory panel
	DirPanel* getDirPanel(void)
	{
		return dirpanel;	
	}
};
#endif
