#ifndef FILEPANEL_H
#define FILEPANEL_H
#include "Properties.h"
#include "FileList.h"
#include "PathLinker.h"
#include "InputDialog.h"
#include "BrowseInputDialog.h"
#include "HistInputDialog.h"
#include "ArchInputDialog.h"

// Clipboard operations
enum
{	
	COPY_CLIPBOARD,
	CUT_CLIPBOARD,
};

class FilePanel : public FXVerticalFrame
{
    FXDECLARE(FilePanel)
protected:
    FilePanel*		current;
    FileList*	  	list;
    FilePanel*		next;
	DirPanel*		dirpanel;
    PathLinker*		pathlink;
	FXPacker* 		statusbar;
    FXLabel*		statuslabel;
    FXLabel*		filterlabel;
	FXLabel* 		activeicon;
    FXString 		name;
	FXbool			ctrlkey;
	FXbool			selmult;
	FXString		trashlocation;
	FXString		trashfileslocation;
	FXString		trashinfolocation;
	FXString		startlocation;
	FXDragCorner    *corner;
	FXDragType      urilistType;        // Standard uri-list type
	FXDragType      xfelistType;        // Xfe, Gnome and XFCE list type
	FXDragType      kdelistType;        // KDE list type
	FXDragType      utf8Type;           // UTF-8 text type
	FXbool 			clipboard_locked;	// Clipboard locked to prevent changes when viewing it
	InputDialog*    newfiledialog;
	InputDialog*    newdirdialog;
	InputDialog*    newlinkdialog;
	HistInputDialog* opendialog;
	ArchInputDialog* archdialog;
	HistInputDialog* filterdialog;
	BrowseInputDialog* operationdialogsingle;
	BrowseInputDialog* operationdialogmultiple;
	FXbool			paste;
	FXbool 			ctrl;			    // Flag to select the right click control menu
	FXbool			shiftf10;           // Flag indicating that Shift-F10 was pressed 
	TextLabel*		pathtext;
	FXbool			isactive;
	FXbool			stopListRefresh;
	FXColor			attenclr;
	
public:

FilePanel(FXWindow *owner, const char*,FXComposite*, DirPanel*, unsigned int name_size=200, unsigned int size_size=60, unsigned int type_size=100, unsigned int ext_size=100, 
	      unsigned int modd_size=150, unsigned int user_size=50, unsigned int grou_size=50, unsigned int attr_size=100, unsigned int deldate_size=150, unsigned int origpath_size=200, FXbool showthumbs=FALSE,
          FXColor listbackcolor=FXRGB(255,255,255), FXColor listforecolor=FXRGB(0,0,0), FXColor attentioncolor=FXRGB(255,0,0), FXbool smoothscroll=TRUE,
 		  unsigned int opts=0,int x=0,int y=0,int w=0,int h=0);
    FilePanel()
    {}
    virtual void create();
    ~FilePanel();

    void setActive();
    void setInactive(FXbool=TRUE);

    void Next(FilePanel*);
	void updateLocation();
	void showCorner(FXbool show);
	void showActiveIcon(FXbool show);
	void execFile(FXString pathname);

	enum
	{
        ID_FILELIST=FXVerticalFrame::ID_LAST,
		ID_STOP_LIST_REFRESH_TIMER,
        ID_DIRECTORY_UP,
        ID_VIEW,
        ID_EDIT,
        ID_PROPERTIES,
        ID_FILE_COPY,
		ID_FILE_CUT,
        ID_FILE_COPYTO,
        ID_FILE_MOVETO,
        ID_FILE_RENAME,
        ID_FILE_SYMLINK,
        ID_FILE_DELETE,
        ID_FILE_TRASH,
        ID_FILE_RESTORE,
        ID_FILE_ASSOC,
		ID_POPUP_MENU,
		ID_XTERM,
        ID_EXTRACT,
		ID_EXTRACT_TO_FOLDER,
        ID_EXTRACT_HERE,
        ID_NEW_DIR,
        ID_NEW_FILE,
		ID_NEW_SYMLINK,
		ID_ADD_TO_ARCH,
        ID_GO_HOME,
		ID_GO_TRASH,
        ID_COPY_CLIPBOARD,
        ID_CUT_CLIPBOARD,
        ID_PASTE_CLIPBOARD,
        ID_OPEN,
        ID_OPEN_WITH,
        ID_FILTER,
		ID_FILTER_CURRENT,
        ID_STATUS,
        ID_LABEL,
        ID_REFRESH,
        ID_SELECT_ALL,
        ID_DESELECT_ALL,
        ID_SELECT_INVERSE,
        ID_SHOW_BIG_ICONS,
        ID_SHOW_MINI_ICONS,
        ID_SHOW_DETAILS,
        ID_TOGGLE_HIDDEN,
        ID_TOGGLE_THUMBNAILS,
#if defined(linux)
        ID_MOUNT,
        ID_UMOUNT,
		ID_PKG_QUERY,
		ID_PKG_INSTALL,
		ID_PKG_UNINSTALL,
#endif
        ID_LAST,
    };
public:
	long onClipboardGained(FXObject*,FXSelector,void*);
	long onClipboardLost(FXObject*,FXSelector,void*);
	long onClipboardRequest(FXObject*,FXSelector,void*);
    long onUpdStatus(FXObject*,FXSelector,void*);
    long onCmdItemDoubleClicked(FXObject*,FXSelector,void*);
    long onCmdItemClicked(FXObject*,FXSelector,void*);
	long onCmdFocus(FXObject*,FXSelector, void*);
    long onCmdItemFilter(FXObject*,FXSelector,void*);
    long onCmdCopyCut(FXObject*,FXSelector,void*);
    long onCmdPaste(FXObject*,FXSelector,void*);
	long onCmdDirectoryUp(FXObject*,FXSelector,void*);
    long onCmdGoHome(FXObject*,FXSelector,void*);
	long onCmdGoTrash(FXObject*,FXSelector,void*);
    long onCmdEdit(FXObject*,FXSelector,void*);
    long onCmdProperties(FXObject*,FXSelector,void*);
    long onCmdFileMan(FXObject*,FXSelector,void*);
    long onCmdFileTrash(FXObject*,FXSelector,void*);
    long onCmdFileRestore(FXObject*,FXSelector,void*);
	long onCmdFileDelete(FXObject*,FXSelector,void*);
    long onCmdFileAssoc(FXObject*,FXSelector,void*);
    long onCmdNewDir(FXObject*,FXSelector,void*);
    long onCmdNewFile(FXObject*,FXSelector,void*);
	long onCmdNewSymlink(FXObject*,FXSelector,void*);
	long onCmdOpen(FXObject*,FXSelector,void*);
    long onCmdOpenWith(FXObject*,FXSelector,void*);
	long onCmdXTerm(FXObject*,FXSelector,void*);
    long onCmdExtract(FXObject*,FXSelector,void*);
    long onCmdExtractToFolder(FXObject*,FXSelector,void*);
    long onCmdExtractHere(FXObject*,FXSelector,void*);
    long onCmdRefresh(FXObject*,FXSelector,void*);
    long onCmdSelect(FXObject*,FXSelector,void*);
    long onCmdPopupMenu(FXObject*,FXSelector,void*);
    long onCmdShow(FXObject*,FXSelector,void*);
    long onUpdShow(FXObject*,FXSelector,void*);
    long onUpdUp(FXObject*,FXSelector,void*);
    long onUpdPaste(FXObject*,FXSelector,void*);
    long onCmdToggleHidden(FXObject*,FXSelector,void*);
    long onUpdToggleHidden(FXObject*,FXSelector,void*);
	long onCmdToggleThumbnails(FXObject*,FXSelector,void*);
	long onUpdToggleThumbnails(FXObject*,FXSelector,void*);
    long onCmdAddToArch(FXObject*,FXSelector,void*);
	long onUpdMenu(FXObject*,FXSelector,void*);
	long onUpdOpen(FXObject*,FXSelector,void*);
	long onUpdAddToArch(FXObject*,FXSelector,void*);
	long onUpdSelMult(FXObject*,FXSelector,void*);
	long onUpdFileDelete(FXObject*,FXSelector,void*);
	long onUpdFileTrash(FXObject*,FXSelector,void*);
	long onUpdFileRestore(FXObject*,FXSelector,void*);
	long onUpdGoTrash(FXObject*,FXSelector,void*);
	void updatePath();
	long onCmdStopListRefreshTimer(FXObject*,FXSelector,void*);

#if defined(linux)
    long onCmdMount(FXObject*,FXSelector,void*);
    long onUpdMount(FXObject*,FXSelector,void*);
    long onUpdUnmount(FXObject*,FXSelector,void*);
    long onCmdPkgQuery(FXObject*,FXSelector,void*);
    long onUpdPkgQuery(FXObject*,FXSelector,void*);
    long onCmdPkgInstall(FXObject*,FXSelector,void*);
    long onCmdPkgUninstall(FXObject*,FXSelector,void*);
#endif
public:

	// Change path text
	void setPathText(FXString title)
	{
		pathtext->setText(title);
	}
	
	// Toggle FileList refresh
	void setAllowRefresh(FXbool flag)
	{
		list->setAllowRefresh(flag);
	}

	// Change sort function
	void setSortFunc(IconListSortFunc func)
	{
		list->setSortFunc(func);
	}

	// Return sort function
	IconListSortFunc getSortFunc() const
	{
		return list->getSortFunc();
	}

	// Change default cursor
	void setDefaultCursor(FXCursor* cur)
	{
		list->setDefaultCursor(cur);
	}

	// Redraw file list
	void redraw(void)
	{
		list->recalc();
	}

	// Return a pointer on the current panel
	FilePanel* getCurrent(void) const
	{
		return current;
	}

	// Return a pointer on the next panel
	FilePanel* getNext(void) const
	{
		return next;
	}
	
	// Set current directory
	void setDirectory(FXString pathname, FXbool notify=FALSE)
	{
		list->setDirectory(pathname,notify);
	}

	// Get current directory
	FXString getDirectory(void) const
	{
		return list->getDirectory();
	}

	// Get associations
	FileDict* getAssociations(void)
	{
		return list->getAssociations();
	}

	// Get header size given its index
	int getHeaderSize(int index) const
	{
		return list->getHeaderSize(index);
	}

	// Hidden files shown?
	FXbool shownHiddenFiles(void) const
	{
		return list->shownHiddenFiles();
	}

	// Show hidden files
	void showHiddenFiles(FXbool shown)
	{
		list->showHiddenFiles(shown);
	}

	// Thumbnails shown? 
	FXbool shownThumbnails(void) const
	{
		return list->shownThumbnails();
	}

	// Show thumbnails
	void showThumbnails(FXbool shown)
	{
		list->showThumbnails(shown);
	}

	// Get the current icon list style
	unsigned int getListStyle(void) const
	{
		return list->getListStyle();
	}

	// Get the current icon list style
	void setListStyle(unsigned int style)
	{
		list->setListStyle(style);
	}

	// Return pointer on the file list
	FileList* getList(void) const
	{
		return list;
	}
		
	// Set ignore case
	void setIgnoreCase(FXbool ignorecase)
	{
		list->setIgnoreCase(ignorecase);
	}

	// Get ignore case
	FXbool getIgnoreCase(void)
	{
		return list->getIgnoreCase();
	}

	// Set directory first
	void setDirsFirst(FXbool dirsfirst)
	{
		list->setDirsFirst(dirsfirst);  
	}

	// Set directory first
	FXbool getDirsFirst(void)
	{
		return list->getDirsFirst();  
	}

	// Set focus on file list
	void setFocusOnList(void)
	{
		list->setFocus();
	}

	// Is panel active?
	FXbool isActive(void)
	{
		return isactive;
	}

	// Get current item
	int getCurrentItem(void) const
	{
		return list->getCurrentItem();
	}

	// Set current item
	void setCurrentItem(int item)
	{
		list->setCurrentItem(item);
		list->makeItemVisible(item);		
	}

	// Select item
	void selectItem(int item)
	{
		list->selectItem(item);
	}

	// Deselect item
	void deselectItem(int item)
	{
		list->deselectItem(item);
	}

	// Is item selected?
	FXbool isItemSelected(int item)
	{
		return list->isItemSelected(item);
		
	}

	// Get number od selected items
	int getNumSelectedItems(void)
	{
		return list->getNumSelectedItems();
	}

	// Status bar is shown?
	FXbool statusbarShown(void)
	{
		return statusbar->shown();
	}

	// Toggle status bar
	void toggleStatusbar(void)
	{
		statusbar->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),NULL);
	}

	// Set path of the path linker
	void setPathLink(FXString pathname)
	{
		pathlink->setPath(pathname);
	}

	// Get back history first item
	StringItem* backhistGetFirst(void)
	{
		return list->backhist->getFirst();
	}

	// Get forward history first item
	StringItem* forwardhistGetFirst(void)
	{
		return list->forwardhist->getFirst();
	}

	// Get back history next item
	StringItem* backhistGetNext(StringItem* item)
	{
		return list->backhist->getNext(item);
	}

	// Get forward history next item
	StringItem* forwardhistGetNext(StringItem* item)
	{
		return list->forwardhist->getNext(item);
	}

	// Get back history string from item
	FXString backhistGetString(StringItem* item)
	{
		return list->backhist->getString(item);
	}

	// Get forward history string from item
	FXString forwardhistGetString(StringItem* item)
	{
		return list->forwardhist->getString(item);
	}

	// Remove back history first item
	void backhistRemoveFirstItem(void)
	{
		list->backhist->removeFirstItem();
	}

	// Remove forward history first item
	void forwardhistRemoveFirstItem(void)
	{
		list->forwardhist->removeFirstItem();
	}

	// Insert back history first item
	void backhistInsertFirstItem(FXString item)
	{
		list->backhist->insertFirstItem(item);
	}

	// Insert forward history first item
	void forwardhistInsertFirstItem(FXString item)
	{
		list->forwardhist->insertFirstItem(item);
	}

	// Get back history number of items
	int backhistGetNumItems(void)
	{
		if (list->backhist)
			return list->backhist->getNumItems();
		else
			return 0;
	}

	// Get forward history number of items
	int forwardhistGetNumItems(void)
	{
		if (list->forwardhist)
			return list->forwardhist->getNumItems();
		else
			return 0;
	}

	// Remove all back history items
	void backhistRemoveAllItems(void)
	{
		list->backhist->removeAllItems();
	}

	// Remove all forward history items
	void forwardhistRemoveAllItems(void)
	{
		list->forwardhist->removeAllItems();
	}


	// Remove all back history items before item
	void backhistRemoveAllItemsBefore(StringItem* item)
	{
		list->backhist->removeAllItemsBefore(item);
	}

	// Remove all forward history items before item
	void forwardhistRemoveAllItemsBefore(StringItem* item)
	{
		list->forwardhist->removeAllItemsBefore(item);
	}

	// Get back history item at position pos
	StringItem* backhistGetItemAtPos(int pos)
	{
		return list->backhist->getItemAtPos(pos);
	}

	// Get forward history item at position pos
	StringItem* forwardhistGetItemAtPos(int pos)
	{
		return list->forwardhist->getItemAtPos(pos);
	}

};
#endif
