#ifndef DIRPANEL_H
#define DIRPANEL_H
#include "DirList.h"
#include "Properties.h"
#include "InputDialog.h"
#include "ArchInputDialog.h"
#include "BrowseInputDialog.h"


#include <sys/types.h>




class DirPanel : public FXVerticalFrame
{
    FXDECLARE(DirPanel)
protected:
	DirList*	list;
	FXPacker*	statusbar;
    FXLabel*	status;
	FXLabel*	activeicon;
	FXString 	trashlocation;
	FXString 	trashfileslocation;
	FXString 	trashinfolocation;
	FXString 	startlocation;
	FXDragType 	urilistType;        	// Standard uri-list type
	FXDragType 	xfelistType;        	// Xfe, Gnome and XFCE list type
	FXDragType 	kdelistType;        	// KDE list type
	FXDragType 	utf8Type;           	// UTF-8 text type
	FXbool 		clipboard_locked;       // Clipboard locked to prevent changes when viewing it
	InputDialog* 		newdirdialog;
	ArchInputDialog* 	archdialog;
	BrowseInputDialog* 	operationdialog;
	FXbool 		paste;
	FXWindow* 	focuswindow;
	FXbool 		ctrlflag;				// Flag to select the right click control menu
	TextLabel*	paneltitle;				// Panel title
	FXbool		isactive;				// Flag to indicate is panel has keyboard focus
	FXbool		stopListRefresh;		// To stop refreshing in some cases
	time_t  	curr_mtime;				// Current directory mtime
	FXString	curr_dirpath;			// Current directory path
	FXbool		allowDirsizeRefresh;	// Allow or avoid directory size refresh 


public:
DirPanel(FXWindow *owner,FXComposite *p,FXColor listbackcolor=FXRGB(255,255,255),FXColor listforecolor=FXRGB(0,0,0), FXbool smoothscroll=TRUE,
         unsigned int opts=0,int x=0,int y=0,int w=0,int h=0);
    DirPanel()
    {}
    virtual void create();
    ~DirPanel();
    enum {
        ID_FILELIST=FXVerticalFrame::ID_LAST,
		ID_STOP_LIST_REFRESH_TIMER,
		ID_EXPANDTREE,
		ID_TOGGLE_HIDDEN,
		ID_COLLAPSEDIR,
		ID_COLLAPSETREE,
		ID_PROPERTIES,
		ID_ARCHIVE,
		ID_DIR_COPY,
		ID_DIR_CUT,
		ID_DIR_COPYTO,
		ID_DIR_MOVETO,
		ID_DIR_RENAME,
		ID_DIR_SYMLINK,
		ID_DIR_DELETE,
		ID_DIR_TRASH,
		ID_DIR_RESTORE,
		ID_NEW_DIR,
		ID_XTERM,
		ID_COPY_CLIPBOARD,
		ID_CUT_CLIPBOARD,
		ID_PASTE_CLIPBOARD,
        ID_TOGGLE_TREE,
		ID_TITLE,
		ID_DIRSIZE_REFRESH,
		ID_POPUP_MENU,
#if defined(linux)
        ID_MOUNT,
        ID_UMOUNT,
#endif
        ID_LAST,
    	};
	long exploreUp(DirItem *item, const DirItem *rootitem, const int task);
	long exploreDown(DirItem *item, const DirItem *rootitem, const int task);
public:
	long onClipboardGained(FXObject*,FXSelector,void*);
	long onClipboardLost(FXObject*,FXSelector,void*);
	long onClipboardRequest(FXObject*,FXSelector,void*);
	long onCmdToggleHidden(FXObject*,FXSelector,void*);
	long onUpdToggleHidden(FXObject* sender,FXSelector,void*);
	long onCmdPopupMenu(FXObject*,FXSelector,void*);
	long onExpandTree(FXObject*,FXSelector,void*);
	long onCollapseTree(FXObject*,FXSelector,void*);
	long onCmdProperties(FXObject*,FXSelector,void*);
	long onCmdAddToArch(FXObject*,FXSelector,void*);
	long onCmdDirMan(FXObject*,FXSelector,void*);
	long onCmdDirDelete(FXObject*,FXSelector,void*);
	long onCmdDirTrash(FXObject*,FXSelector,void*);
	long onCmdDirRestore(FXObject*,FXSelector,void*);
	long onCmdNewDir(FXObject*,FXSelector,void*);
	long onCmdXTerm(FXObject*,FXSelector,void*);
	long onCmdCopyCut(FXObject*,FXSelector,void*);
	long onCmdPaste(FXObject*,FXSelector,void*);
	long onUpdPaste(FXObject*,FXSelector,void*);
	long onCmdDirectory(FXObject*,FXSelector,void*);
	long onCmdToggleTree(FXObject*,FXSelector sel,void*);
	long onCmdDirsizeRefresh(FXObject*,FXSelector,void*);
	long onUpdToggleTree(FXObject*,FXSelector,void*);
	long onUpdMount(FXObject*,FXSelector,void*);
	long onUpdUnmount(FXObject*,FXSelector,void*);
	long onUpdMenu(FXObject*,FXSelector,void*);
	long onUpdDirTrash(FXObject*,FXSelector,void*);
	long onUpdDirRestore(FXObject*,FXSelector,void*);
	long onUpdDirDelete(FXObject*,FXSelector,void*);
	long onUpdTitle(FXObject*,FXSelector,void*);
	long onUpdStatus(FXObject*,FXSelector,void*);
	long onExpand(FXObject*,FXSelector,void*);
	long onKeyPress(FXObject*,FXSelector,void*);
	long onCmdFocus(FXObject*,FXSelector,void*);
	long onCmdStopListRefreshTimer(FXObject*,FXSelector,void*);
	long onUpdDirsizeRefresh(FXObject*,FXSelector,void*);
#if defined(linux)
	long onCmdMount(FXObject*,FXSelector,void*);
#endif
public:

	void setActive();
	void setInactive();
	
	// Toggle dirsize refresh and force refresh if flag is true
	void setAllowDirsizeRefresh(FXbool flag);

	// Change sort function
	void setSortFunc(FXTreeListSortFunc func)
	{
		list->setSortFunc(func);
	}

	// Return sort function
	FXTreeListSortFunc getSortFunc() const
	{
		return list->getSortFunc();
	}

	// Change default cursor
	void setDefaultCursor(FXCursor* cur)
	{
		list->setDefaultCursor(cur);
	}

	// Set current directory
	void setDirectory(const FXString& pathname,FXbool notify=FALSE)
	{
		list->setDirectory(pathname,notify);
	}

	// Get current directory
	FXString getDirectory(void) const
	{
		return list->getDirectory();
	}

	// Get current item
	DirItem* getCurrentItem(void) const
	{
		return (DirItem*)list->getCurrentItem();
	}

	// Get current path name
	FXString getItemPathname(const DirItem* item) const
	{
		return list->getItemPathname((TreeItem*)item);
	}

	// Hidden files shown?
	FXbool shownHiddenFiles() const
	{
		return list->shownHiddenFiles();
	}

	// Show hidden files
	void showHiddenFiles(FXbool shown)
	{
		list->showHiddenFiles(shown);
	}

	// Set focus on list
	void setFocusOnList(void)
	{
		list->setFocus();
	}

	// Is panel active?
	FXbool isActive(void)
	{
		return isactive;
	}

	// Force dir panel refresh
	void forceRefresh(void)
	{
		list->onCmdRefresh(0,0,0);
	}
	
	DirList* getList(void)
	{
		return list;
	}

#if defined(linux)
	// Force devices refresh
	void forceDevicesRefresh(void)
	{
		list->onMtdevicesRefresh(0,0,0);
		list->onUpdevicesRefresh(0,0,0);
	}
#endif

	// Toggle status bar
	void toggleStatusbar(void)
	{
		statusbar->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),NULL);
	}

};
#endif
