#include "config.h"
#include "i18n.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/time.h>

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "File.h"
#include "DirList.h"
#include "Properties.h"
#include "BrowseInputDialog.h"
#include "ArchInputDialog.h"
#include "XFileExplorer.h"
#include "MessageBox.h"
#include "DirPanel.h"



// Refresh interval for the directory size (ms)
#define DIRSIZE_REFRESH_INTERVAL 1000

// Duration (in ms) before we can stop refreshing the file list
// Used for file operations on a large list of files
#define STOP_LIST_REFRESH_INTERVAL 5000

// Number of files before stopping the file list refresh
#define STOP_LIST_REFRESH_NBMAX 100

// Clipboard
extern FXString clipboard;
extern unsigned int clipboard_type;

// Global variables
extern FXMainWindow *mainWindow;
extern FXbool allowPopupScroll;
#if defined(linux)
extern FXStringDict* fsdevices;
extern FXStringDict* mtdevices;
#endif
extern unsigned int single_click;
extern FXString xdgdatahome;



// Dirty hack to change the KEY_up and KEY_down behaviour
// These keys are no more associated with the mouse click action
#define SELECT_MASK (TREELIST_SINGLESELECT|TREELIST_BROWSESELECT)
FXbool fromKeyPress=FALSE;
long FXTreeList::onKeyPress(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    FXTreeItem *item=currentitem;
    FXTreeItem *succ;
    int page;
    flags&=~FLAG_TIP;
    if (!isEnabled())
		return 0;
    if (target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr))
		return 1;
    if (item==NULL) item=firstitem;
    switch (event->code)
    {
    case KEY_Control_L:
    case KEY_Control_R:
    case KEY_Shift_L:
    case KEY_Shift_R:
    case KEY_Alt_L:
    case KEY_Alt_R:
        if (flags&FLAG_DODRAG)
            handle(this,FXSEL(SEL_DRAGGED,0),ptr);
        return 1;
    case KEY_Page_Up:
    case KEY_KP_Page_Up:
        for (succ=item,page=verticalScrollBar()->getPage(); succ && 0<page; )
        {
            item=succ;
            page-=succ->getHeight(this);
            if (succ->prev)
            {
                succ=succ->prev;
                while (succ->last && ((options&TREELIST_AUTOSELECT) || succ->isExpanded())) succ=succ->last;
            }
            else if (succ->parent)
                succ=succ->parent;
        }
        goto hop;
    case KEY_Page_Down:
    case KEY_KP_Page_Down:
        for (succ=item,page=verticalScrollBar()->getPage(); succ && 0<page; )
        {
            item=succ;
            page-=succ->getHeight(this);
            if (succ->first && ((options&TREELIST_AUTOSELECT) || succ->isExpanded()))
                succ=succ->first;
            else
            {
                while (!succ->next && succ->parent) succ=succ->parent;
                succ=succ->next;
            }
        }
        goto hop;
    case KEY_Up:                          // Move up
    case KEY_KP_Up:
        if (item)
        {
            if (item->prev)
            {
                item=item->prev;
                while (item->last && ((options&TREELIST_AUTOSELECT) || item->isExpanded())) item=item->last;
            }
            else if (item->parent)
                item=item->parent;
        }
        goto hop;
    case KEY_Down:                        // Move down
    case KEY_KP_Down:
        if (item)
        {
            if (item->first && ((options&TREELIST_AUTOSELECT) || item->isExpanded()))
                item=item->first;
            else
            {
                while (!item->next && item->parent) item=item->parent;
                item=item->next;
            }
        }
        goto hop;
    case KEY_Right:                       // Move right/down and open subtree
    case KEY_KP_Right:
        if (item)
        {
            if (!(options&TREELIST_AUTOSELECT) && !item->isExpanded() && (item->hasItems() || item->getFirst()))
                expandTree(item,TRUE);
            else if (item->first)
                item=item->first;
            else
            {
                while (!item->next && item->parent) item=item->parent;
                item=item->next;
            }
        }
        goto hop;
    case KEY_Left:                        // Move left/up and close subtree
    case KEY_KP_Left:
        if (item)
        {
            if (!(options&TREELIST_AUTOSELECT) && item->isExpanded() && (item->hasItems() || item->getFirst()))
                collapseTree(item,TRUE);
            else if (item->parent)
                item=item->parent;
            else if (item->prev)
                item=item->prev;
        }
        goto hop;
    case KEY_Home:                        // Move to first
    case KEY_KP_Home:
        item=firstitem;
        goto hop;
    case KEY_End:                         // Move to last
    case KEY_KP_End:
        item=lastitem;
        while (item)
        {
            if (item->last && ((options&TREELIST_AUTOSELECT) || item->isExpanded()))
                item=item->last;
            else if (item->next)
                item=item->next;
            else
                break;
        }
hop:
        lookup=FXString::null;
        if (item)
        {
            setCurrentItem(item,TRUE);
            makeItemVisible(item);
            if ((options&SELECT_MASK)==TREELIST_EXTENDEDSELECT)
            {
                if (item->isEnabled())
                {
                    if (event->state&SHIFTMASK)
                    {
                        if (anchoritem)
                        {
                            selectItem(anchoritem,TRUE);
                            extendSelection(item,TRUE);
                        }
                        else
                        {
                            selectItem(item,TRUE);
                            setAnchorItem(item);
                        }
                    }
                    else if (!(event->state&CONTROLMASK))
                    {
                        killSelection(TRUE);
                        selectItem(item,TRUE);
                        setAnchorItem(item);
                    }
                }
            }
        }

        // !!!! Hack to change the KEY_up and KEY_down behaviour !!!
		fromKeyPress=TRUE;
		// !!!! End of hack !!!
		handle(this,FXSEL(SEL_CLICKED,0),(void*)currentitem);
		
        if (currentitem && currentitem->isEnabled())
            handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
        return 1;
    case KEY_space:
    case KEY_KP_Space:
        lookup=FXString::null;
        if (item && item->isEnabled())
        {
            switch (options&SELECT_MASK)
            {
            case TREELIST_EXTENDEDSELECT:
                if (event->state&SHIFTMASK)
                {
                    if (anchoritem)
                    {
                        selectItem(anchoritem,TRUE);
                        extendSelection(item,TRUE);
                    }
                    else
                        selectItem(item,TRUE);
                }
                else if (event->state&CONTROLMASK)
                    toggleItem(item,TRUE);
                else
                {
                    killSelection(TRUE);
                    selectItem(item,TRUE);
                }
                break;
            case TREELIST_MULTIPLESELECT:
            case TREELIST_SINGLESELECT:
                toggleItem(item,TRUE);
                break;
            }
            setAnchorItem(item);
        }
        handle(this,FXSEL(SEL_CLICKED,0),(void*)currentitem);
        if (currentitem && currentitem->isEnabled())
            handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
        return 1;
    case KEY_Return:
    case KEY_KP_Enter:
        lookup=FXString::null;
        handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)currentitem);
        if (currentitem && currentitem->isEnabled())
            handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
        return 1;
    default:
        if ((unsigned char)event->text[0]<' ')
			return 0;
        if (event->state&(CONTROLMASK|ALTMASK))
			return 0;
        if (!Ascii::isPrint(event->text[0]))
			return 0;
        lookup.append(event->text);
        getApp()->addTimeout(this,ID_LOOKUPTIMER,getApp()->getTypingSpeed());
        item=findItem(lookup,currentitem,SEARCH_FORWARD|SEARCH_WRAP|SEARCH_PREFIX);
        if (item)
        {
            setCurrentItem(item,TRUE);
            makeItemVisible(item);
            if ((options&SELECT_MASK)==TREELIST_EXTENDEDSELECT)
            {
                if (item->isEnabled())
                {
                    killSelection(TRUE);
                    selectItem(item,TRUE);
                }
            }
            setAnchorItem(item);
        }
        handle(this,FXSEL(SEL_CLICKED,0),(void*)currentitem);
        if (currentitem && currentitem->isEnabled())
            handle(this,FXSEL(SEL_COMMAND,0),(void*)currentitem);
        return 1;
    }
    return 0;
}


// Map
FXDEFMAP(DirPanel) DirPanelMap[]={
	FXMAPFUNC(SEL_CLIPBOARD_LOST,0,DirPanel::onClipboardLost),
	FXMAPFUNC(SEL_CLIPBOARD_GAINED,0,DirPanel::onClipboardGained),
	FXMAPFUNC(SEL_CLIPBOARD_REQUEST,0,DirPanel::onClipboardRequest),
	FXMAPFUNC(SEL_TIMEOUT,DirPanel::ID_STOP_LIST_REFRESH_TIMER,DirPanel::onCmdStopListRefreshTimer),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_EXPANDTREE,DirPanel::onExpandTree),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_COLLAPSETREE,DirPanel::onCollapseTree),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_PROPERTIES,DirPanel::onCmdProperties),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_ARCHIVE,DirPanel::onCmdAddToArch),
	FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,DirPanel::ID_FILELIST,DirPanel::onCmdPopupMenu),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_POPUP_MENU,DirPanel::onCmdPopupMenu),
	FXMAPFUNC(SEL_CLICKED,DirPanel::ID_FILELIST,DirPanel::onCmdDirectory),
	FXMAPFUNC(SEL_EXPANDED,DirPanel::ID_FILELIST,DirPanel::onExpand),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_COPY_CLIPBOARD,DirPanel::onCmdCopyCut),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_CUT_CLIPBOARD,DirPanel::onCmdCopyCut),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_PASTE_CLIPBOARD,DirPanel::onCmdPaste),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_COPY,DirPanel::onCmdDirMan),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_CUT,DirPanel::onCmdDirMan),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_COPYTO,DirPanel::onCmdDirMan),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_MOVETO,DirPanel::onCmdDirMan),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_RENAME,DirPanel::onCmdDirMan),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_SYMLINK,DirPanel::onCmdDirMan),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_DELETE,DirPanel::onCmdDirDelete),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_TRASH,DirPanel::onCmdDirTrash),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_DIR_RESTORE,DirPanel::onCmdDirRestore),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_NEW_DIR,DirPanel::onCmdNewDir),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_XTERM,DirPanel::onCmdXTerm),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_TOGGLE_HIDDEN,DirPanel::onCmdToggleHidden),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_TOGGLE_TREE,DirPanel::onCmdToggleTree),
	FXMAPFUNC(SEL_TIMEOUT,DirPanel::ID_DIRSIZE_REFRESH,DirPanel::onCmdDirsizeRefresh),
	FXMAPFUNC(SEL_FOCUSIN,DirPanel::ID_FILELIST,DirPanel::onCmdFocus),
#if defined(linux)
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_MOUNT,DirPanel::onCmdMount),
	FXMAPFUNC(SEL_COMMAND,DirPanel::ID_UMOUNT,DirPanel::onCmdMount),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_MOUNT,DirPanel::onUpdMount),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_UMOUNT,DirPanel::onUpdUnmount),
#endif
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_PASTE_CLIPBOARD,DirPanel::onUpdPaste),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_TOGGLE_HIDDEN,DirPanel::onUpdToggleHidden),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_TOGGLE_TREE,DirPanel::onUpdToggleTree),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_CUT_CLIPBOARD,DirPanel::onUpdMenu),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_ARCHIVE,DirPanel::onUpdMenu),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_DIR_MOVETO,DirPanel::onUpdMenu),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_DIR_RENAME,DirPanel::onUpdMenu),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_DIR_TRASH,DirPanel::onUpdDirTrash),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_DIR_RESTORE,DirPanel::onUpdDirRestore),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_DIR_DELETE,DirPanel::onUpdDirDelete),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_NEW_DIR,DirPanel::onUpdMenu),
	FXMAPFUNC(SEL_UPDATE,DirPanel::ID_TITLE,DirPanel::onUpdTitle),
	FXMAPFUNC(SEL_UPDATE,0,DirPanel::onUpdDirsizeRefresh),
};

// Object implementation 
FXIMPLEMENT(DirPanel,FXVerticalFrame,DirPanelMap,ARRAYNUMBER(DirPanelMap))

// Construct Directory Panel
DirPanel::DirPanel(FXWindow *owner, FXComposite *p, FXColor listbackcolor, FXColor listforecolor, FXbool smoothscroll, unsigned int opts, int x, int y, int w, int h):
        FXVerticalFrame(p,opts,x,y,w,h,0,0,0,0)
{
	// Construct directory panel
    FXVerticalFrame* cont=new FXVerticalFrame(this,LAYOUT_FILL_Y|LAYOUT_FILL_X|FRAME_NONE,0,0,0,0, 0,0,0,0, 0,0);
	FXPacker* packer=new FXHorizontalFrame(cont,LAYOUT_LEFT|JUSTIFY_LEFT|LAYOUT_FILL_X|FRAME_NONE,0,0,0,0, 0,0,0,0);
	
	// Visually indicate if the panel is active
	activeicon=new FXLabel(packer,"",greenbuttonicon,JUSTIFY_LEFT|LAYOUT_LEFT);
	
	// Panel title
	paneltitle=new TextLabel(packer,0,this,ID_FILELIST,LAYOUT_FILL_X|LAYOUT_FILL_Y);
	paneltitle->setText(_("Folders"));
	paneltitle->setBackColor(getApp()->getBaseColor());

	unsigned int options;
	if (smoothscroll)
		options=LAYOUT_FILL_X|LAYOUT_FILL_Y|TREELIST_BROWSESELECT|TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES|FRAME_SUNKEN;
	else
		options=LAYOUT_FILL_X|LAYOUT_FILL_Y|TREELIST_BROWSESELECT|TREELIST_SHOWS_LINES|TREELIST_SHOWS_BOXES|SCROLLERS_DONT_TRACK;
	
	FXVerticalFrame* cont2=new FXVerticalFrame(cont,LAYOUT_FILL_Y|LAYOUT_FILL_X|FRAME_SUNKEN,0,0,0,0, 0,0,0,0, 0,0);
	list=new DirList(owner,cont2,this,ID_FILELIST,options);
	list->setTextColor(listforecolor);
	list->setBackColor(listbackcolor);

	statusbar=new FXHorizontalFrame(cont,JUSTIFY_LEFT|LAYOUT_FILL_X|FRAME_NONE,0,0,0,0, 3,3,3,3);

	FXString key=getApp()->reg().readStringEntry("KEYBINDINGS","hidden_dirs","Ctrl-F5");
    new FXToggleButton(statusbar,TAB+_("Show hidden folders")+PARS(key),TAB+_("Hide hidden folders")+PARS(key),showhiddenicon,hidehiddenicon,this,
	                   ID_TOGGLE_HIDDEN,BUTTON_TOOLBAR|LAYOUT_LEFT|ICON_BEFORE_TEXT|FRAME_NONE);

    status=new FXLabel(statusbar,_("0 bytes in root"),NULL,JUSTIFY_LEFT|LAYOUT_LEFT|LAYOUT_FILL_X|FRAME_NONE);
    status->setTarget(this);
    status->setSelector(FXSEL(SEL_UPDATE,DirPanel::ID_TITLE));

	// Home and trahscan locations
	trashlocation=xdgdatahome+PATHSEPSTRING TRASHPATH;
	trashfileslocation=xdgdatahome + PATHSEPSTRING TRASHFILESPATH;
	trashinfolocation=xdgdatahome + PATHSEPSTRING TRASHINFOPATH;
	
	// Start location (we return to the start location after each chdir)
	startlocation=FXSystem::getCurrentDirectory();

	// Single click navigation
    single_click=getApp()->reg().readUnsignedEntry("SETTINGS","single_click",SINGLE_CLICK_NONE);
	if (single_click==SINGLE_CLICK_DIR_FILE)
		list->setDefaultCursor(getApp()->getDefaultCursor(DEF_HAND_CURSOR));

	// Dialogs
	newdirdialog=NULL;
	archdialog=NULL;
	operationdialog=NULL;

	// Initialize clipboard flag	
	paste=FALSE;
	
	// Initialize control flag for right click popup menu
	ctrlflag=FALSE;

	// Other initializations
	focuswindow=owner;	
	isactive=FALSE;
	curr_mtime=0;
	curr_dirpath="";
	allowDirsizeRefresh=TRUE;
}


// Destructor
DirPanel::~DirPanel()
{
    getApp()->removeTimeout(this,ID_DIRSIZE_REFRESH);
	delete list;
	delete statusbar;
	delete status;
	delete newdirdialog;
	delete archdialog;
	delete operationdialog;
	delete paneltitle;
}

// Create X window
void DirPanel::create()
{	
	// Register standard uri-list type
	urilistType=getApp()->registerDragType("text/uri-list");
    
	// Register special uri-list type used for Gnome, XFCE and Xfe 
	xfelistType=getApp()->registerDragType("x-special/gnome-copied-files");
	
	// Register special uri-list type used for KDE 
	kdelistType=getApp()->registerDragType("application/x-kde-cutselection");

	// Register standard UTF-8 text type used for file dialogs 
	utf8Type=getApp()->registerDragType("UTF8_STRING");

    getApp()->addTimeout(this,ID_DIRSIZE_REFRESH,DIRSIZE_REFRESH_INTERVAL);
	FXVerticalFrame::create();
}



// Make DirPanel active
void DirPanel::setActive()
{
	// Set active icon
	activeicon->setIcon(greenbuttonicon);
	activeicon->setTipText(_("Panel has focus"));
	list->setFocus();
	isactive=TRUE;
	
	// Current panel must get an inactive icon (but not get the inactive status!)
	FilePanel* currentpanel=((XFileExplorer*) mainWindow)->getCurrentPanel();
	currentpanel->setInactive(FALSE);	
}


// Make DirPanel inactive
void DirPanel::setInactive()
{	
	// Set active icon
	activeicon->setIcon(graybuttonicon);
	activeicon->setTipText(_("Panel does not have focus"));
	isactive=FALSE;
}


// Focus on DirPanel when clicked (i.e. make panel active)
long DirPanel::onCmdFocus(FXObject* sender,FXSelector sel, void* ptr)
{
	setActive();
    return 1;
}



// To pass the expand message to DirList
long DirPanel::onExpand(FXObject*,FXSelector,void* ptr)
{
	list->handle(this,FXSEL(SEL_EXPANDED,0),(void*)ptr);
	return 1;
}


// Change the directory when clicking on the tree list
long DirPanel::onCmdDirectory(FXObject*,FXSelector,void* ptr)
{
    TreeItem *item=(TreeItem*)ptr;
    if(item)
    {
        FXString directory=list->getItemPathname(item);		
        if(!::isReadExecutable(directory))
        {
            MessageBox::error(this,BOX_OK_SU,_("Error"),_(" Permission to: %s denied."), directory.text());
            return 0;
        }
		FilePanel* currentpanel=((XFileExplorer*) mainWindow)->getCurrentPanel();
		FXComboBox* address=((XFileExplorer*) mainWindow)->getAddressBox();
        currentpanel->setDirectory(directory,TRUE);
        currentpanel->updatePath();
		
		// Remember latest directory in the location address
       	FXString item;
		int i=0;
		int count=address->getNumItems();
       	FXString p=currentpanel->getDirectory();

		if(!count)
		{
			count++;
			address->insertItem(0,address->getText());
		}
       	while(i < count)
		{
       		item=address->getItem(i++);
       		if(streq((const char*)&p[0],(const char*)&item[0]))
			{
				i--;
				break;
			}
       	}
       	if(i==count)
			address->insertItem(0,currentpanel->getDirectory());

    }

	// Manage single click navigation
	if (item && (single_click!=SINGLE_CLICK_NONE) && !fromKeyPress)
		list->handle(this,FXSEL(SEL_EXPANDED,0),(void*)ptr);
	fromKeyPress=FALSE;
		
    return 1;
}


// Toggle hidden files
long DirPanel::onCmdToggleHidden(FXObject*,FXSelector,void*)
{
    list->showHiddenFiles(!list->shownHiddenFiles());
    return 1;
}


// Update toggle hidden files widget
long DirPanel::onUpdToggleHidden(FXObject* sender,FXSelector,void*)
{
    if(list->shownHiddenFiles())
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


long DirPanel::onCmdToggleTree(FXObject* sender,FXSelector sel,void* ptr)
{
	return this->handle(sender,FXSEL(SEL_COMMAND,FXWindow::ID_TOGGLESHOWN),ptr);
}

long DirPanel::onUpdToggleTree(FXObject* sender,FXSelector sel,void* ptr)
{
    if(this->shown())
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_CHECK),ptr);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK),ptr);
    return 1;
}


// Directory list context menu
long DirPanel::onCmdPopupMenu(FXObject* o,FXSelector s,void* p)
{
	// Check if control key was pressed
	if (p!=NULL)
	{
		FXEvent* event=(FXEvent*)p;
		if (event->state&CONTROLMASK)
			ctrlflag=TRUE;
	}

	// Current item becomes item under cursor
	int x, y, xitem, yitem;
    unsigned int state;
	getRoot()->getCursorPosition(x,y,state);
	list->getCursorPosition(xitem,yitem,state);   	    
	DirItem* item=(DirItem*)list->getItemAt(xitem,yitem);

	// If item, then set it current and set directory in DirList and FileList
	FXString dir;
	if (item)
	{
		list->setCurrentItem(item,TRUE);
		dir=list->getItemPathname((TreeItem*)item);
		list->setDirectory(dir,TRUE);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->setDirectory(dir,TRUE);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->updatePath();
	}
	else
		ctrlflag=TRUE;
	
    // Popup menu pane
    FXMenuPane *menu = new FXMenuPane(this);
    
	// Menu

    // Control flag set
	if(ctrlflag)
    {
		// Reset the control flag
		ctrlflag=FALSE;
		
		// Panel menu items
		new FXMenuCommand(menu,_("New &folder..."),newfoldericon,this,DirPanel::ID_NEW_DIR);

		new FXMenuSeparator(menu);
		new FXMenuCheck(menu,_("&Hidden folders"),this,DirPanel::ID_TOGGLE_HIDDEN);
  		new FXMenuCheck(menu,_("Ignore c&ase"),list,DirList::ID_SORT_CASE);
		new FXMenuCheck(menu,_("&Reverse order"),list,DirList::ID_SORT_REVERSE);
		new FXMenuCommand(menu,_("E&xpand tree"),exptreeicon,this,DirPanel::ID_EXPANDTREE);
		new FXMenuCommand(menu,_("Collap&se tree"),colltreeicon,this,DirPanel::ID_COLLAPSETREE);
   }
   else
   {
		// Panel submenu items
		FXMenuPane* submenu=new FXMenuPane(this);

		new FXMenuCommand(submenu,_("New &folder..."),newfoldericon,this,DirPanel::ID_NEW_DIR);

		new FXMenuSeparator(submenu);
		new FXMenuCheck(submenu,_("&Hidden folders"),this,DirPanel::ID_TOGGLE_HIDDEN);
  		new FXMenuCheck(submenu,_("Ignore c&ase"),list,DirList::ID_SORT_CASE);
		new FXMenuCheck(submenu,_("&Reverse order"),list,DirList::ID_SORT_REVERSE);
		new FXMenuCommand(submenu,_("E&xpand tree"),exptreeicon,this,DirPanel::ID_EXPANDTREE);
		new FXMenuCommand(submenu,_("Collap&se tree"),colltreeicon,this,DirPanel::ID_COLLAPSETREE);

		new FXMenuCascade(menu,_("Pane&l"),NULL,submenu);
		new FXMenuSeparator(menu);

#if defined(linux)
		if (::isLink(dir))
			dir=::readLink(dir);
		if(fsdevices->find(dir.text()) || mtdevices->find(dir.text()))
		{
			new FXMenuCommand(menu,_("M&ount"),maphosticon,this,ID_MOUNT);
			new FXMenuCommand(menu,_("Unmoun&t"),unmaphosticon,this,ID_UMOUNT);
			new FXMenuSeparator(menu);
		}
#endif
		new FXMenuCommand(menu,_("&Add to archive..."),archaddicon,this,DirPanel::ID_ARCHIVE);
		new FXMenuSeparator(menu);
		new FXMenuCommand(menu,_("&Copy"),copy_clpicon,this,DirPanel::ID_COPY_CLIPBOARD);
		new FXMenuCommand(menu,_("C&ut"),cut_clpicon,this,DirPanel::ID_CUT_CLIPBOARD);
		new FXMenuCommand(menu,_("&Paste"),paste_clpicon,this,DirPanel::ID_PASTE_CLIPBOARD);
		new FXMenuSeparator(menu);
		new FXMenuCommand(menu,_("Re&name..."),renameiticon,this,DirPanel::ID_DIR_RENAME);
		new FXMenuCommand(menu,_("Cop&y to..."),copy_clpicon,this,DirPanel::ID_DIR_COPYTO);
		new FXMenuCommand(menu,_("&Move to..."),moveiticon,this,DirPanel::ID_DIR_MOVETO);
		new FXMenuCommand(menu,_("Symlin&k to..."),minilinkicon,this,DirPanel::ID_DIR_SYMLINK);
		new FXMenuCommand(menu,_("Mo&ve to trash"),filedeleteicon,this,DirPanel::ID_DIR_TRASH);
		new FXMenuCommand(menu,_("R&estore from trash"),filerestoreicon,this,DirPanel::ID_DIR_RESTORE);
		new FXMenuCommand(menu,_("&Delete"),filedelete_permicon,this,DirPanel::ID_DIR_DELETE);
		new FXMenuSeparator(menu);
		new FXMenuCommand(menu,_("Prop&erties"),attribicon,this,DirPanel::ID_PROPERTIES);
	}
	
    menu->create();
    allowPopupScroll=TRUE;  // Allow keyboard scrolling
	menu->popup(NULL,x,y);
	getApp()->runModalWhileShown(menu);
    allowPopupScroll=FALSE;
    return 1;
}


// Helper function used to explore the directory tree and expand or collapse it
long DirPanel::exploreUp(DirItem *item, const DirItem *rootitem, const int task)
{
	DirItem* parentitem=item;
	
	if (task==ID_EXPANDTREE)
		list->expandTree((TreeItem*)item,TRUE);
	else
		list->collapseTree((TreeItem*)item,TRUE);
	
	item=(DirItem*)item->getFirst();
	if (!item)
		exploreDown(parentitem,rootitem,task);
	else
		exploreUp(item,rootitem,task);
	return 1;	
}


// Helper function used to explore the directory tree and expand or collapse it
long DirPanel::exploreDown(DirItem *item, const DirItem *rootitem, const int task)
{
	if (item==rootitem)
		return 1;
	
	DirItem* parentitem=(DirItem*)item->getParent();
	
	if (task==ID_EXPANDTREE)
		list->expandTree((TreeItem*)item,TRUE);
	else
		list->collapseTree((TreeItem*)item,TRUE);
	item=(DirItem*)item->getNext();
	
	if (!item)
		exploreDown(parentitem,rootitem,task);
	else
	{
		if (task==ID_EXPANDTREE)
			list->expandTree((TreeItem*)item,TRUE);
		else
			list->collapseTree((TreeItem*)item,TRUE);
		if (!list->isItemLeaf(item))
			exploreUp(item,rootitem,task);
		else
			exploreDown(item,rootitem,task);
	}
	return 1;		
}
				

// Expand the directory tree under cursor
long DirPanel::onExpandTree(FXObject* sender, FXSelector sel, void*)
{
    DirItem* rootitem=(DirItem*)list->getCurrentItem();
	getApp()->beginWaitCursor();
	exploreUp(rootitem,rootitem,ID_EXPANDTREE);
	getApp()->endWaitCursor();
	
	return 1;
}


// Collapse the directory tree under cursor
long DirPanel::onCollapseTree(FXObject* sender, FXSelector sel, void*)
{
    DirItem* rootitem=(DirItem*)list->getCurrentItem();
	getApp()->beginWaitCursor();
	exploreUp(rootitem,rootitem,ID_COLLAPSETREE);
	getApp()->endWaitCursor();
	
	return 1;
}


// Directory properties
long DirPanel::onCmdProperties(FXObject* sender, FXSelector, void*)
{
    // Current item
	DirItem* item=(DirItem*)list->getCurrentItem();
	FXString pathname=list->getItemPathname((TreeItem*)item);
	
	PropertiesBox* attrdlg=new PropertiesBox(this,FXPath::name(pathname),FXPath::directory(pathname));	
   	if(attrdlg->execute(PLACEMENT_OWNER))
        list->setDirectory(pathname,TRUE);
    delete attrdlg;
   	return 1;
}


// Add files or directory to an archive
long DirPanel::onCmdAddToArch(FXObject* o,FXSelector,void*)
{
	FXString ext1, ext2, cmd, archive;
	File *f;

	// Name and path of the current item
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString name=list->getItemText(item);
	FXString pathname=list->getItemPathname((TreeItem*)item);	
	FXString parentdir=FXPath::directory(pathname);
 
    // Initial archive name with full path and default extension
    archive=parentdir+PATHSEPSTRING+name+".tar.gz";

	chdir(parentdir.text());

	// Archive dialog
	if (archdialog==NULL)
		archdialog=new ArchInputDialog(this,"");
	archdialog->setText(archive);
	archdialog->CursorEnd();

    if(archdialog->execute())
    {
        if (archdialog->getText()=="")
		{
			MessageBox::warning(this,BOX_OK,_("Warning"),_("File name is empty, operation cancelled"));
			return 0;
		}

        // Get string and preserve escape characters
		archive=::quote(archdialog->getText());

        // Get extensions of the archive name
        ext1=archdialog->getText().rafter('.',1);
		ext1.lower();
        ext2=archdialog->getText().rafter('.',2);
		ext2.lower();
		
		// Handle different archive formats
		if (ext2=="tar.gz")
            cmd="tar -zcvf "+archive+" ";
		else if (ext2=="tar.bz2")
            cmd="tar -jcvf "+archive+" ";
		else if (ext2=="tar.xz")
            cmd="tar -Jcvf "+archive+" ";
		else if (ext2=="tar.z")
           	cmd="tar -Zcvf "+archive+" ";
		else if (ext1=="tar")
           	cmd="tar -cvf "+archive+" ";
		else if (ext1=="gz")
			cmd="gzip -v ";				
		else if (ext1=="tgz")
            cmd="tar -zcvf "+archive+" ";
		else if (ext1=="taz")
            cmd="tar -Zcvf "+archive+" ";
		else if (ext1=="bz2")
			cmd="bzip2 -v ";				
		else if (ext1=="xz")
			cmd="xz -v ";				
		else if (ext1=="tbz2" || ext1=="tbz")
            cmd="tar -jcvf "+archive+" ";
		else if (ext1=="txz")
            cmd="tar -Jcvf "+archive+" ";
		else if (ext1=="z")
            cmd="compress -v ";
		else if (ext1=="zip")
            cmd="zip -r "+archive+" ";
		else if (ext1=="7z")
            cmd="7z a "+archive+" ";
		
		// Default archive format
		else
		{
			archive += ".tar.gz";
			cmd="tar -zcvf "+archive+" ";
		}
		
		// Archive command name
		cmd=cmd+::quote(name);
		
		// Wait cursor
		getApp()->beginWaitCursor();

		// File object
		f=new File(this,_("Create archive"),ARCHIVE);
		f->create();

		// Create archive
		f->archive(archive,cmd);
		chdir(startlocation.text());
 
		getApp()->endWaitCursor();
		delete f; 

		// Display parent directory in DirList and FileList
		list->setDirectory(parentdir,TRUE);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->setDirectory(parentdir);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->updatePath();
    }
    return 1;
}


// We now really do have the clipboard, keep clipboard content
long DirPanel::onClipboardGained(FXObject* sender,FXSelector sel,void* ptr)
{
    FXVerticalFrame::onClipboardGained(sender,sel,ptr);
    return 1;
}


// We lost the clipboard, free clipboard content
long DirPanel::onClipboardLost(FXObject* sender,FXSelector sel,void* ptr)
{
    FXVerticalFrame::onClipboardLost(sender,sel,ptr);
	return 1;
}


// Somebody wants our clipboard content
long DirPanel::onClipboardRequest(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent *event=(FXEvent*)ptr;
    unsigned char *data;
    unsigned int len;

	// Perhaps the target wants to supply its own data for the clipboard
    if(FXVerticalFrame::onClipboardRequest(sender,sel,ptr))
		return 1;

    // Clipboard target is xfelistType (Xfe, Gnome or XFCE)
	if(event->target==xfelistType)
    {	
		// Don't modify the clipboard if we are called from updPaste()
		if (!clipboard_locked)
		{
			// Prepend "copy" or "cut" as in the Gnome way and avoid duplicating these strings
			if (clipboard.find("copy\n")<0 && clipboard.find("cut\n")<0)
			{
				if (clipboard_type==CUT_CLIPBOARD)
					clipboard = "cut\n" + clipboard;
				else
					clipboard = "copy\n" + clipboard;
			}
		}
		
        // Return clipboard content
        if(event->target==xfelistType)
        {
			if(!clipboard.empty())
			{
				len=clipboard.length();
				FXMEMDUP(&data,clipboard.text(),unsigned char,len);
				setDNDData(FROM_CLIPBOARD,event->target,data,len);
				
				// Return because xfelistType is not compatible with other types
				return 1;
			}
		}
    }

    // Clipboard target is kdelisType (KDE)
    if(event->target==kdelistType)
    {
		// The only data to be passed in this case is "0" for copy and "1" for cut
		// The uri data are passed using the standard uri-list type
		FXString flag;
		if (clipboard_type==CUT_CLIPBOARD)
			flag = "1";
		else
			flag = "0";

        // Return clipboard content
        if(event->target==kdelistType)
        {
			FXMEMDUP(&data,flag.text(),unsigned char,1);
			setDNDData(FROM_CLIPBOARD,event->target,data,1);
		}
    }

    // Clipboard target is urilistType (KDE apps ; non Gnome, non XFCE and non Xfe apps)  
    if(event->target==urilistType)
    {
		if(!clipboard.empty())
		{
			len=clipboard.length();
			FXMEMDUP(&data,clipboard.text(),unsigned char,len);
			setDNDData(FROM_CLIPBOARD,event->target,data,len);
		
			return 1;
		}
    }

    // Clipboard target is utf8Type (to paste file pathes as text to other applications)  
    if(event->target==utf8Type)
    {
		if(!clipboard.empty())
		{			
			int beg=0, end=0;
			FXString str="";
			FXString filepath, url;

			// Clipboard don't contain 'copy\n' or 'cut\n' as first line
			if (clipboard.find("copy\n")<0 && clipboard.find("cut\n")<0)
			{
				// Remove the 'file:' prefix for each file path
				while (1)
				{
					end=clipboard.find('\n',end);
					if (end<0) // Last line
					{
						end=clipboard.length();
						url=clipboard.mid(beg,end-beg+1);
						filepath=FXURL::decode(FXURL::fileFromURL(url));
						str += filepath;
						break;
					}
					url=clipboard.mid(beg,end-beg+1);
					filepath=FXURL::decode(FXURL::fileFromURL(url));
					str += filepath;
					end++;
					beg=end;
				}
				end=str.length();
				str=str.mid(0,end-2);  // Why is it end-2 here????
			}
			
			// Clipboard contains 'copy\n' or 'cut\n' as first line, thus skip it
			else
			{				
				// Start after the 'copy\n' or 'cut\n' prefix
				end=clipboard.find('\n',0);
				end++;
				beg=end;
				
				// Remove the 'file:' prefix for each file path
				while (1)
				{
					end=clipboard.find('\n',end);
					if (end<0) // Last line
					{
						end=clipboard.length();
						url=clipboard.mid(beg,end-beg+1);
						filepath=FXURL::decode(FXURL::fileFromURL(url));
						str += filepath;
						break;
					}
					url=clipboard.mid(beg,end-beg+1);
					filepath=FXURL::decode(FXURL::fileFromURL(url));
					str += filepath;
					end++;
					beg=end;
				}
				end=str.length();
				//str=str.mid(0,end-1);
			}
			
			if(!str.empty())
			{
				len=str.length();
				FXMEMDUP(&data,str.text(),unsigned char,len);
				setDNDData(FROM_CLIPBOARD,event->target,data,len);
			
				return 1;
			}
		}
    }
    return 0;
}


// Copy or cut to clipboard
long DirPanel::onCmdCopyCut(FXObject*,FXSelector sel,void*)
{			
	// Clear clipboard
	clipboard.clear();

    // Clipboard type
	if (FXSELID(sel)==ID_CUT_CLIPBOARD)
		clipboard_type=CUT_CLIPBOARD;
	else
		clipboard_type=COPY_CLIPBOARD;

	// Current item
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString pathname=list->getItemPathname((TreeItem*)item);
	clipboard=FXURL::encode(::fileToURI(pathname));
		
	// Acquire the clipboard
	FXDragType types[4];
	types[0]=xfelistType;
	types[1]=kdelistType;
	types[2]=urilistType;
	types[3]=utf8Type;
	if (acquireClipboard(types,4))
		return 0;
    
	return 1;
}


// Paste file(s) from clipboard
long DirPanel::onCmdPaste(FXObject*,FXSelector sel,void*)
{
    unsigned char *data;
    unsigned int len;
	int beg, end, pos;
	FXString chaine, url, param;		  
	int num=0;
	FXbool from_kde=FALSE;
	
    // Target directory
	FXString targetdir=((XFileExplorer*) mainWindow)->getCurrentPanel()->getDirectory();
	
	// If source is xfelistType (Gnome, XFCE, or Xfe app)
    if(getDNDData(FROM_CLIPBOARD,xfelistType,data,len))
    {		
        FXRESIZE(&data,unsigned char,len+1);
        data[len]='\0';
		
		clipboard=(char*)data;
		
		// Loop over clipboard items
		for(beg=0; beg<clipboard.length(); beg=end+1)
		{
			if((end=clipboard.find("\n",beg))<0)
				end=clipboard.length();
			
			// Obtain item url
			url=clipboard.mid(beg,end-beg);
				
			// Eventually remove the trailing '\r' if any
			if ((pos=url.rfind('\r'))>0)
				url.erase(pos);

			// Process first item
			if (num==0)
			{
				// First item should be "copy" or "cut"
				if (url=="copy")
				{
					clipboard_type=COPY_CLIPBOARD;
					num++;
				}
				else if (url=="cut")
				{
					clipboard_type=CUT_CLIPBOARD;
					num++;
				}
				
				// If first item is not "copy" nor "cut", process it as a normal url
				// and use default clipboard type
				else
				{											
					// Update the param string
					param += FXURL::decode(FXURL::fileFromURL(url)) + "\n";
						
					// Add one more because the first line "copy" or "cut" was not present
					num += 2;
				}
			}
			
			// Process other items
			else
			{
				// Update the param string
				param += FXURL::decode(FXURL::fileFromURL(url)) + "\n";
				num++;
			}
		}

		// Construct the final param string passed to the file management routine
		param = targetdir+"\n" + FXStringVal(num-1) + "\n" + param;
		
		// Copy or cut operation depending on the clipboard type
		switch(clipboard_type)
		{
		case COPY_CLIPBOARD:
			sel=FXSEL(SEL_COMMAND,DirPanel::ID_DIR_COPY);
			break;
		case CUT_CLIPBOARD:
			clipboard.clear();
			sel=FXSEL(SEL_COMMAND,DirPanel::ID_DIR_CUT);
			break;
		}
		paste=TRUE;
		this->handle(this,sel,(void*)param.text());
    		
        // Free data pointer
		FXFREE(&data);
		
		// Return here because xfelistType is not compatible with other types
		return 1;
	}

	// If source type is kdelistType (KDE)
    if(getDNDData(FROM_CLIPBOARD,kdelistType,data,len))
    {		
        from_kde=TRUE;

        FXRESIZE(&data,unsigned char,len+1);
        data[len]='\0';
		clipboard=(char*)data;
				
		// Obtain clipboard type (copy or cut)
		if (clipboard=="1")
			clipboard_type=CUT_CLIPBOARD;
		else
			clipboard_type=COPY_CLIPBOARD;
		
		FXFREE(&data);
	}
	
	
	// If source type is urilistType (KDE apps ; non Gnome, non XFCE and non Xfe apps)
    if(getDNDData(FROM_CLIPBOARD,urilistType,data,len))
    {		
		// For non KDE apps, set action to copy
		if (!from_kde)
			clipboard_type=COPY_CLIPBOARD;

        FXRESIZE(&data,unsigned char,len+1);
        data[len]='\0';
		clipboard=(char*)data;
				
		// Loop over clipboard items
		for(beg=0; beg<clipboard.length(); beg=end+1)
		{
			if((end=clipboard.find("\n",beg))<0)
				end=clipboard.length();

			// Obtain item url
			url=clipboard.mid(beg,end-beg);

			// Eventually remove the trailing '\r' if any
			if ((pos=url.rfind('\r'))>0)
				url.erase(pos);

			// Update the param string
			param += FXURL::decode(FXURL::fileFromURL(url)) + "\n";
			num++;
		}

		// Construct the final param string passed to the file management routine
		param = targetdir+"\n" + FXStringVal(num) + "\n" + param;
				
		// Copy or cut operation depending on the clipboard type
		switch(clipboard_type)
		{
		case COPY_CLIPBOARD:
			sel=FXSEL(SEL_COMMAND,DirPanel::ID_DIR_COPY);
			break;
		case CUT_CLIPBOARD:
			clipboard.clear();
			sel=FXSEL(SEL_COMMAND,DirPanel::ID_DIR_CUT);
			break;
		}
		paste=TRUE;
		handle(this,sel,(void*)param.text());
					
		FXFREE(&data);
		return 1;
	}
	return 0;
}


// Set the flag that allows to stop the file list refresh
long DirPanel::onCmdStopListRefreshTimer(FXObject*,FXSelector,void*)
{
	stopListRefresh=TRUE;
    return 0;
}



// Copy/Move/Rename/Symlink directory
long DirPanel::onCmdDirMan(FXObject* sender,FXSelector sel,void* ptr)
{
	int num;
	FXString src, targetdir, target, name, source;

	// Confirmation dialog?
	FXbool ask_before_copy=getApp()->reg().readUnsignedEntry("OPTIONS","ask_before_copy",TRUE);

	// If we are called from the paste command, get the parameters from the pointer
	// Multiple sources are allowed
	if (paste)
	{
		// Reset the flag
		paste=FALSE;

		// Get the parameters
		FXString str=(char*)ptr;
		targetdir=str.section('\n',0);
		num=FXUIntVal(str.section('\n',1));
		src=str.after('\n',2);
		source=src.section('\n',0);

		// If no item, return
		if(num<=0)
			return 0;
	}
	
	// Obtain the parameters from the dir panel (only one source)
	else
	{
		// Current item
		DirItem* item=(DirItem*)list->getCurrentItem();
		
		// Number of items
		if (item)
			num=1;
		else
			return 0;
    
		// Source directory
		source=list->getItemPathname((TreeItem*)item);	

		// Target directory
		targetdir=FXPath::directory(source);	
	}

	// Go to target directory
	//chdir(targetdir.text());
	
	// Name and directory of the first source file
	name=FXPath::name(source);
	FXString dir=FXPath::directory(source);

	// Initialise target name	
	if (targetdir!=ROOTDIR)
		target=targetdir+PATHSEPSTRING;
	else
		target=targetdir;

    // Configure the command, title, message, etc.
    FXIcon *icon=NULL;
	FXString command, title, message;
	if (FXSELID(sel)==ID_DIR_COPY)
    {
        command="copy";
        title=_("Copy");
        icon=copy_bigicon;
        if(num==1)
        {
            message=_("Copy ");
            message+=source;
            target+=name;
		}
        else
			message.format(_("Copy %s items from: %s"),FXStringVal(num).text(),dir.text());
    }
    if (FXSELID(sel)==ID_DIR_RENAME)
    {
        command="rename";
        title=_("Rename");
        icon=move_bigicon;
        if(num==1)
        {
            message=_("Rename ");
			message+=name;
            target=name;
			title=_("Rename");
        }
        else
			return 0;
    }
	if (FXSELID(sel)==ID_DIR_COPYTO)
    {
        command="copy";
        title=_("Copy to");
        icon=copy_bigicon;
        if(num==1)
        {
            message=_("Copy ");
            message+=source;
		}
        else
			message.format(_("Copy %s items from: %s"),FXStringVal(num).text(),dir.text());
    }
    if (FXSELID(sel)==ID_DIR_MOVETO)
    {
        command="move";
        title=_("Move");
        icon=move_bigicon;
        if(num==1)
        {
            message=_("Move ");
            message+=source;
            title=_("Move");
        }
        else
			message.format(_("Move %s items from: %s"),FXStringVal(num).text(),dir.text());
    }
    if (FXSELID(sel)==ID_DIR_CUT)
    {
        command="move";
        title=_("Move");
        icon=move_bigicon;
        if(num==1)
        {
            message=_("Move ");
            message+=source;
            target+=name;
            title=_("Move");
        }
        else
			message.format(_("Move %s items from: %s"),FXStringVal(num).text(),dir.text());
    }
    if (FXSELID(sel)==ID_DIR_SYMLINK)
    {
        command="symlink";
        title=_("Symlink");
        icon=link_bigicon;
        if(num==1)
        {
            message=_("Symlink ");
            message+=source;
            target+=name;
        }
        else
			message.format(_("Symlink %s items from: %s"),FXStringVal(num).text(),dir.text());
    }
	
    // File operation dialog, if needed
	if (ask_before_copy || source==target ||  FXSELID(sel)==ID_DIR_COPYTO || FXSELID(sel)==ID_DIR_MOVETO ||  FXSELID(sel)==ID_DIR_RENAME || FXSELID(sel)==ID_DIR_SYMLINK)
	{   
		if (operationdialog==NULL)
			operationdialog=new BrowseInputDialog(this,"","",title,_("To:"),icon,BROWSE_INPUT_FOLDER);
		operationdialog->setTitle(title);
		operationdialog->setIcon(icon);
		operationdialog->setMessage(message);
		operationdialog->setText(target);
		if (FXSELID(sel)==ID_DIR_RENAME)
			operationdialog->SelectAll();
		operationdialog->CursorEnd();
		operationdialog->setDirectory(targetdir);
		int rc=1;
		rc=operationdialog->execute(PLACEMENT_CURSOR);
		target=operationdialog->getText();
		if (!rc)
			return 0;
	}
	
	// Update target and target parent directory
	target=::filePath(target,targetdir);
	
    // Target directory not writable
    if(!::isWritable(targetdir))
    {
        MessageBox::error(this,BOX_OK_SU,_("Error"), _("Can't write to %s: Permission denied"), targetdir.text());
        return 0;
    }

	// Multiple sources and non existent destination
	if (num>1 && !::exists(target))
	{
        MessageBox::error(this,BOX_OK,_("Error"),_("Folder %s doesn't exist"),target.text());
        return 0;
	}
		
	// Multiple sources and target is a file
	if (num>1 && ::isFile(target))
	{ 			
        MessageBox::error(this,BOX_OK,_("Error"),_("%s is not a folder"),target.text());
        return 0;
	}

	// Target is a directory and is not writable
    if (::isDirectory(target) & !::isWritable(target))
    {
        MessageBox::error(this,BOX_OK_SU,_("Error"),_("Can't write to %s: Permission denied"), target.text());
        return 0;
    }
	
	// Target is a file and its parent directory is not writable
	if (::isFile(target) && !::isWritable(targetdir))
	{
        MessageBox::error(this,BOX_OK_SU,_("Error"),_("Can't write to %s: Permission denied"),targetdir.text());
        return 0;
	}

	// Target parent directory doesn't exist
	if (!::exists(targetdir))
	{
		MessageBox::error(this,BOX_OK,_("Error"),_("Folder %s doesn't exist"),targetdir.text());
        return 0;
	}
		
	// Target parent directory is not a directory
	if (!::isDirectory(targetdir))
	{
		MessageBox::error(this,BOX_OK,_("Error"),_("%s is not a folder"),targetdir.text());
        return 0;
	}
		
	// Current and next panel
	FilePanel* currentpanel=((XFileExplorer*) mainWindow)->getCurrentPanel();
	FilePanel* nextpanel=((XFileExplorer*) mainWindow)->getNextPanel();

	// One source
	File *f=NULL;
	int ret;
    if(num==1)
	{
		// An empty source file name corresponds to the ".." file
		// Don't perform any file operation on it!
		if (source=="")
			return 0;
		
		// Wait cursor
		getApp()->beginWaitCursor();

		// File object
        if(command=="copy")
        {
			f=new File(this,_("File copy"),COPY);
        	f->create();

			// If target file is located at trash location, also create the corresponding trashinfo file
			// Do it silently and don't report any error if it fails
			FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
			if (use_trash_can && target==trashfileslocation )
			{						
				// Trash files path name
				FXString trashpathname=createTrashpathname(source,trashfileslocation);
				
				// Adjust target name to get the _N suffix if any
				FXString trashtarget=target+PATHSEPSTRING+FXPath::name(trashpathname);
		
				// Create trashinfo file
				createTrashinfo(source,trashpathname,trashfileslocation,trashinfolocation);

				// Copy source to trash target
				ret=f->copy(source,trashtarget);
			}

			// Copy source to target
			else
				ret=f->copy(source,target);

			// An error has occurred
			if (ret==0 && !f->isCancelled())
			{
				f->hideProgressDialog();
				MessageBox::error(this,BOX_OK,_("Error"),_("An error has occurred during the copy file operation!"));
			}
					
			// If action is cancelled in progress dialog
			if (f->isCancelled())
			{
				f->hideProgressDialog();
				MessageBox::error(this,BOX_OK,_("Error"),_("Copy file operation cancelled!"));
			}
        }
        else if(command=="rename")
        {
			f=new File(this,_("File rename"),RENAME);
        	f->create();
			ret=f->rename(source,target);

			// If file is located at trash location, try to also remove the corresponding trashinfo if it exists
			// Do it silently and don't report any error if it fails
			FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
			if (use_trash_can && ret && (source.left(trashfileslocation.length())==trashfileslocation) )
			{
				FXString trashinfopathname=trashinfolocation+PATHSEPSTRING+FXPath::name(source)+".trashinfo";
				::unlink(trashinfopathname.text());
			}
        }
        else if(command=="move")
        {
			f=new File(this,_("File move"),MOVE);
        	f->create();
			
			// If target file is located at trash location, also create the corresponding trashinfo file
			// Do it silently and don't report any error if it fails
			FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);			
			if (use_trash_can && target==trashfileslocation )
			{						
				// Trash files path name
				FXString trashpathname=createTrashpathname(source,trashfileslocation);
				
				// Adjust target name to get the _N suffix if any
				FXString trashtarget=target+PATHSEPSTRING+FXPath::name(trashpathname);
		
				// Create trashinfo file
				createTrashinfo(source,trashpathname,trashfileslocation,trashinfolocation);

				// Move source to trash target
				ret=f->move(source,trashtarget);
			}

			// Move source to target
			else
				ret=f->move(source,target);
			
			// If source file is located at trash location, try to also remove the corresponding trashinfo file if it exists
			// Do it silently and don't report any error if it fails
			if (use_trash_can && ret && (source.left(trashfileslocation.length())==trashfileslocation) )
			{
				FXString trashinfopathname=trashinfolocation+PATHSEPSTRING+FXPath::name(source)+".trashinfo";
				::unlink(trashinfopathname.text());
			}
			
			// An error has occurred
			if (ret==0 && !f->isCancelled())
			{
				f->hideProgressDialog();
				MessageBox::error(this,BOX_OK,_("Error"),_("An error has occurred during the move file operation!"));
			}
					
			// If action is cancelled in progress dialog
			if (f->isCancelled())
			{
				f->hideProgressDialog();
				MessageBox::error(this,BOX_OK,_("Error"),_("Move file operation cancelled!"));
			}
        }
        else if(command=="symlink")
        {
			f=new File(this,_("Symlink"),SYMLINK);
        	f->create();
			f->symlink(source,target);
		}
       	else
           	exit(-1);
									
		getApp()->endWaitCursor();
		delete f;
	}
	
	// Multiple sources
	// Note : rename cannot be used in this case!
    else if(num>1)
    {		
		// Wait cursor
		getApp()->beginWaitCursor();

		// File object
        if (command=="copy")
			f=new File(this,_("File copy"),COPY);
		else if (command=="move")
			f=new File(this,_("File move"),MOVE);
		else if  (command=="symlink")
			f=new File(this,_("Symlink"),SYMLINK);
			
		f->create();

		// Initialize file list stop refresh timer and flag
		stopListRefresh=FALSE;
		getApp()->addTimeout(this,ID_STOP_LIST_REFRESH_TIMER,STOP_LIST_REFRESH_INTERVAL);

		// Loop on the multiple files
		for(int i=0;i<num;i++)
        {
			// Stop refreshing the file list and directory size if file operation is long and has many files
			// This avoids flickering and speeds up things a bit
			if (stopListRefresh && i>STOP_LIST_REFRESH_NBMAX)
			{
				// Force a last refresh if current panel is destination
				if ( currentpanel->getDirectory()==targetdir)
					currentpanel->getList()->onCmdRefresh(0,0,0);
				
				// Force a last refresh if next panel is destination
				if ( nextpanel->getDirectory()==targetdir)
					nextpanel->getList()->onCmdRefresh(0,0,0);

				// Tell the file list to not refresh anymore
				currentpanel->setAllowRefresh(FALSE);
				nextpanel->setAllowRefresh(FALSE);
				
				// Avoid to refresh the dirsize
				setAllowDirsizeRefresh(FALSE); 

				// Don't need to stop again
				stopListRefresh=FALSE;
			}

			// Individual source file
			source=src.section('\n',i);

			// An empty file name corresponds to the ".." file (why?)
			// Don't perform any file operation on it! 
			if (source!="")
			{
            	if(command=="copy")
				{
					// If target file is located at trash location, also create the corresponding trashinfo file
					// Do it silently and don't report any error if it fails
					FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
					if (use_trash_can && target==trashfileslocation )
					{						
						// Trash files path name
						FXString trashpathname=createTrashpathname(source,trashfileslocation);
						
						// Adjust target name to get the _N suffix if any
						FXString trashtarget=target+PATHSEPSTRING+FXPath::name(trashpathname);
				
						// Create trashinfo file
						createTrashinfo(source,trashpathname,trashfileslocation,trashinfolocation);

						// Copy source to trash target
						ret=f->copy(source,trashtarget);
					}

					// Copy source to target
					else
						ret=f->copy(source,target);

					// An error has occurred
					if (ret==0 && !f->isCancelled())
					{
						f->hideProgressDialog();
						MessageBox::error(this,BOX_OK,_("Error"),_("An error has occurred during the copy file operation!"));
						break;
					}
					
					// If action is cancelled in progress dialog
					if (f->isCancelled())
					{
						f->hideProgressDialog();
						MessageBox::error(this,BOX_OK,_("Error"),_("Copy file operation cancelled!"));
						break;
					}
				}
            	else if(command=="move")
            	{
					// If target file is located at trash location, also create the corresponding trashinfo file
					// Do it silently and don't report any error if it fails
					FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
					if (use_trash_can && target==trashfileslocation )
					{						
						// Trash files path name
						FXString trashpathname=createTrashpathname(source,trashfileslocation);
						
						// Adjust target name to get the _N suffix if any
						FXString trashtarget=target+PATHSEPSTRING+FXPath::name(trashpathname);
				
						// Create trashinfo file
						createTrashinfo(source,trashpathname,trashfileslocation,trashinfolocation);

						// Move source to trash target
						ret=f->move(source,trashtarget);
					}

					// Move source to target
					else
						ret=f->move(source,target);
					
					// If source file is located at trash location, try to also remove the corresponding trashinfo file if it exists
					// Do it silently and don't report any error if it fails
					if (use_trash_can && ret && (source.left(trashfileslocation.length())==trashfileslocation) )
					{
						FXString trashinfopathname=trashinfolocation+PATHSEPSTRING+FXPath::name(source)+".trashinfo";
						::unlink(trashinfopathname.text());
					}
					
					// An error has occurred
					if (ret==0 && !f->isCancelled())
					{
						f->hideProgressDialog();
						MessageBox::error(this,BOX_OK,_("Error"),_("An error has occurred during the move file operation!"));
						break;
					}
					
					// If action is cancelled in progress dialog
					if (f->isCancelled())
					{
						f->hideProgressDialog();
						MessageBox::error(this,BOX_OK,_("Error"),_("Move file operation cancelled!"));
						break;
					}
           	 	}
				else if (command=="symlink")
				{
					ret=f->symlink(source,target);
					
					// An error has occurred
					if (ret==0 && !f->isCancelled())
					{
						f->hideProgressDialog();
						MessageBox::error(this,BOX_OK,_("Error"),_("An error has occurred during the symlink operation!"));
						break;
					}
					
					// If action is cancelled in progress dialog
					if (f->isCancelled())
					{
						f->hideProgressDialog();
						MessageBox::error(this,BOX_OK,_("Error"),_("Symlink operation cancelled!"));
						break;
					}
				}
            	else
                	exit(-1);
			}
		}
		
		// Reinit timer and refresh flags
		getApp()->removeTimeout(this,ID_STOP_LIST_REFRESH_TIMER);
		currentpanel->setAllowRefresh(TRUE);
		nextpanel->setAllowRefresh(TRUE);
		setAllowDirsizeRefresh(TRUE);

		getApp()->endWaitCursor();		
		delete f;
	}	
	
	// Go to target directory and refresh the DirList
	list->setDirectory(target,TRUE);
	currentpanel->setDirectory(target);
	currentpanel->updatePath();
	list->handle(this,FXSEL(SEL_COMMAND,DirList::ID_REFRESH),NULL);
    return 1;
}


// Delete directory
long DirPanel::onCmdDirDelete(FXObject*,FXSelector,void*)
{
	// Current item
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString pathname=list->getItemPathname((TreeItem*)item);	
	FXString parentdir=FXPath::directory(pathname);	

    // If we don't have permission to write to the parent directory
    if(!::isWritable(parentdir))
    {
        MessageBox::error(this,BOX_OK_SU,_("Error"),_("Can't write to %s: Permission denied"), parentdir.text());
        return 0;
    }

    FXbool confirm_del=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_delete",TRUE);
    FXbool confirm_del_emptydir=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_delete_emptydir",TRUE);

	if(confirm_del)
    {
		FXString message;
		message.format(_("Definitively delete folder %s ?"),pathname.text());		
		MessageBox box(this,_("Confirm Delete"),message,delete_big_permicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);
        if(box.execute(PLACEMENT_CURSOR) != BOX_CLICKED_OK)
            return 0;
    }

	// File object
   	File* f=new File(this,_("File delete"),DELETE);
   	f->create();

	// Confirm empty directory deletion
	if (confirm_del & confirm_del_emptydir)
	{
		if (::isEmptyDir(pathname)==0 && !::isLink(pathname))
		{
			// Dialog to confirm file deletion
			f->hideProgressDialog();
			FXString msg;
			msg.format(_("Folder %s is not empty, delete it anyway?"),pathname.text());
			MessageBox box(this,_("Confirm Delete"),msg,delete_big_permicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);
			if(box.execute(PLACEMENT_CURSOR) == BOX_CLICKED_CANCEL)
				goto end;
			f->showProgressDialog();
			
		}
	}

    // If we don't have permission to write to the directory
 	if (!::isWritable(pathname))
    {
		// Dialog to confirm directory deletion
		f->hideProgressDialog();
		FXString msg;
		msg.format(_("Folder %s is write-protected, definitively delete it anyway?"),pathname.text());
		MessageBox box(this,_("Confirm Delete"),msg,errorbigicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);        	
		unsigned int answer=box.execute(PLACEMENT_OWNER);
		if(answer == BOX_CLICKED_OK)
		{
			f->showProgressDialog();
			f->remove(pathname); 	

			// If action is cancelled in progress dialog
			if (f->isCancelled())
			{
				f->hideProgressDialog();
				MessageBox::error(this,BOX_OK,_("Error"),_("Delete folder operation cancelled!"));
			}
			
			// Return to parent directory in DirList and FileList
			list->setDirectory(parentdir,TRUE);
			((XFileExplorer*) mainWindow)->getCurrentPanel()->setDirectory(parentdir);
			((XFileExplorer*) mainWindow)->getCurrentPanel()->updatePath();
		}
	}

	// If we have permission to write
	else
	{
		f->remove(pathname); 	
		
		// If directory is located at trash location, try to also remove the trashinfo if it exists
		// Do it silently and don't report any error if it fails
		FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
		if (use_trash_can && (pathname.left(trashfileslocation.length())==trashfileslocation) )
		{
			FXString trashinfopathname=trashinfolocation+PATHSEPSTRING+FXPath::name(pathname)+".trashinfo";
			::unlink(trashinfopathname.text());
		}

		// If action is cancelled in progress dialog
		if (f->isCancelled())
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("Delete folder operation cancelled!"));
		}
		// Return to parent directory in DirList and FileList
		list->setDirectory(parentdir,TRUE);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->setDirectory(parentdir);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->updatePath();
	}

end:
	delete f;

	// Force DirPanel and FilePanel refresh
	setAllowDirsizeRefresh(TRUE);
	list->handle(this,FXSEL(SEL_COMMAND,DirList::ID_REFRESH),NULL);
    return 1;
}


// Move directory to trash can
long DirPanel::onCmdDirTrash(FXObject*,FXSelector,void*)
{	
	// Current item
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString pathname=list->getItemPathname((TreeItem*)item);	
	FXString parentdir=FXPath::directory(pathname);	

    // If we don't have permission to write to the parent directory
    if(!::isWritable(parentdir))
    {
        MessageBox::error(this,BOX_OK_SU,_("Error"),_("Can't write to %s: Permission denied"), parentdir.text());
        return 0;
    }

    FXbool confirm_trash=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_trash",TRUE);

	if(confirm_trash)
    {
		FXString message;
		message.format(_("Move folder %s to trash can?"),pathname.text());
		MessageBox box(this,_("Confirm Trash"),message,delete_bigicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);
        if(box.execute(PLACEMENT_CURSOR) != BOX_CLICKED_OK)
            return 0;
    }

	// File object
   	File* f=new File(this,_("Move to trash"),DELETE);
   	f->create();

    // If we don't have permission to write to the directory
 	if (!::isWritable(pathname))
    {
		// Dialog to confirm directory deletion
		f->hideProgressDialog();
		FXString str;
		str.format(_("Folder %s is write-protected, move it to trash can anyway?"),pathname.text());
		MessageBox box(this,_("Confirm Trash"),str,errorbigicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);        	
		unsigned int answer=box.execute(PLACEMENT_OWNER);
		if(answer == BOX_CLICKED_OK)
		{
			// Allow progress dialog
			f->showProgressDialog();

			// Trash files path name
			FXString trashpathname=createTrashpathname(pathname,trashfileslocation);
	
			// Create trashinfo file
			createTrashinfo(pathname,trashpathname,trashfileslocation,trashinfolocation);

			// Move file to trash files location
			int ret=f->move(pathname,trashpathname);

			// An error has occurred
			if (ret==0 && !f->isCancelled())
			{
				f->hideProgressDialog();
				MessageBox::error(this,BOX_OK,_("Error"),_("An error has occurred during the move to trash operation!"));
			}

			// If action is cancelled in progress dialog
			if (f->isCancelled())
			{
				f->hideProgressDialog();
				MessageBox::error(this,BOX_OK,_("Error"),_("Move to trash folder operation cancelled!"));
			}
			
			// Return to parent directory in DirList and FileList
			list->setDirectory(parentdir,TRUE);
			((XFileExplorer*) mainWindow)->getCurrentPanel()->setDirectory(parentdir);
			((XFileExplorer*) mainWindow)->getCurrentPanel()->updatePath();
		}
	}

	// If we have permission to write
	else
	{
		// Trash files path name
		FXString trashpathname=createTrashpathname(pathname,trashfileslocation);

		// Create trashinfo file
		createTrashinfo(pathname,trashpathname,trashfileslocation,trashinfolocation);

		// Move file to trash files location
		int ret=f->move(pathname,trashpathname);

		// An error has occurred
		if (ret==0 && !f->isCancelled())
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("An error has occurred during the move to trash operation!"));
		}

		// If action is cancelled in progress dialog
		if (f->isCancelled())
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("Move to trash folder operation cancelled!"));
		}
		// Return to parent directory in DirList and FileList
		list->setDirectory(parentdir,TRUE);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->setDirectory(parentdir);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->updatePath();
	}
	delete f;

	// Force DirPanel and FilePanel refresh
	setAllowDirsizeRefresh(TRUE);
	list->handle(this,FXSEL(SEL_COMMAND,DirList::ID_REFRESH),NULL);
    return 1;
}


// Restore directory from trash can
long DirPanel::onCmdDirRestore(FXObject*,FXSelector,void*)
{	
	// Current item
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString pathname=list->getItemPathname((TreeItem*)item);	
	FXString parentdir=FXPath::directory(pathname);	
    FXbool confirm_trash=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_trash",TRUE);

	// File object
   	File* f=new File(this,_("Restore from trash"),DELETE);
   	f->create();

	// Obtain trash base name and sub path
	FXString subpath=pathname;
	subpath.erase(0,trashfileslocation.length()+1);
	FXString trashbasename=subpath.before('/');
	if (trashbasename=="")
		trashbasename=FXPath::name(pathname);
	subpath.erase(0,trashbasename.length());
	
	// Read the .trashinfo file
	FILE *fp;
	char line[1024];
	FXbool success=TRUE;
	FXString trashinfopathname=trashinfolocation+PATHSEPSTRING+trashbasename+".trashinfo";
	FXString origpathname="";
	
	if ((fp=fopen(trashinfopathname.text(),"r"))!=NULL)
	{
		// Read the first two lines and get the strings
		if (fgets(line,sizeof(line),fp)==NULL)
			success=FALSE;
		if (fgets(line,sizeof(line),fp)==NULL)
			success=FALSE;
		if (success)
		{
			origpathname=line;
			origpathname=origpathname.after('=');
			origpathname=origpathname.before('\n');
		}						
		fclose(fp);
		origpathname=origpathname+subpath;
	}
	
	// Confirm restore dialog
	if(confirm_trash)
    {
		FXString message;
		message.format(_("Restore folder %s to its original location %s ?"),FXPath::name(pathname).text(),origpathname.text());
		f->hideProgressDialog();
		MessageBox box(this,_("Confirm Restore"),message,restore_bigicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);
        if(box.execute(PLACEMENT_CURSOR) != BOX_CLICKED_OK)
		{
			getApp()->endWaitCursor();
			delete f;
			return 0;
		}
		f->showProgressDialog();
    }

	// Bracket used because of a compilation problem with gotos
	{
		if (origpathname=="")
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("Restore information not available for %s"),pathname.text());
			goto end;
		}

		// If parent dir of the original location does not exist
		FXString origparentdir=FXPath::directory(origpathname);
		if (!::exists(origparentdir))
		{
			// Ask the user if he wants to create it
			f->hideProgressDialog();
			FXString message;
			message.format(_("Parent directory %s does not exist, do you want to create it?"),origparentdir.text());
			MessageBox box(this,_("Confirm Restore"),message,restore_bigicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);
			if(box.execute(PLACEMENT_CURSOR) != BOX_CLICKED_OK)
				goto end;
			else
			{
				f->showProgressDialog();
				errno=0;
				int ret=mkpath(origparentdir.text(),0755);
				int errcode=errno;
				if (ret==-1)
				{
					f->hideProgressDialog();
					if (errcode)
						MessageBox::error(this,BOX_OK,_("Error"),_("Can't create folder %s : %s"),origparentdir.text(),strerror(errcode));
					else
						MessageBox::error(this,BOX_OK,_("Error"),_("Can't create folder %s"),origparentdir.text());
					goto end;					
				}
			}
		}

		// Move file to original location (with restore option)
		int ret=f->move(pathname,origpathname,TRUE);
		
		// An error has occurred
		if (ret==0 && !f->isCancelled())
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("An error has occurred during the restore from trash operation!"));
			goto end;					
		}
		
		// Silently remove trashinfo file
		FXString trashfilespathname=trashfileslocation+PATHSEPSTRING+trashbasename;
		if (pathname==trashfilespathname && !::exists(trashfilespathname))
			::unlink(trashinfopathname.text());

		// If action is cancelled in progress dialog
		if (f->isCancelled())
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("Restore from trash file operation cancelled!"));
			goto end;					
		}

		// Return to parent directory in DirList and FileList
		list->setDirectory(parentdir,TRUE);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->setDirectory(parentdir);
		((XFileExplorer*) mainWindow)->getCurrentPanel()->updatePath();
	}

end:
	delete f;

	// Force DirPanel and FilePanel refresh
	setAllowDirsizeRefresh(TRUE);
	list->handle(this,FXSEL(SEL_COMMAND,DirList::ID_REFRESH),NULL);
    return 1;
}


// Create new directory
long DirPanel::onCmdNewDir(FXObject*,FXSelector,void*)
{
	// Current item
    DirItem* item=(DirItem*)list->getCurrentItem();

	FXString dirpath=list->getItemPathname((TreeItem*)item);
    if(dirpath!=ROOTDIR)
        dirpath+=PATHSEPSTRING;
    
    if (newdirdialog==NULL)
		newdirdialog=new InputDialog(this,"",_("Create new folder:"),_("New Folder"));
	newdirdialog->setText("");
    if(newdirdialog->execute(PLACEMENT_CURSOR))
    {
		FXString dirname=dirpath+newdirdialog->getText();		
        if(dirname!=dirpath)
        {
			// Create the new dir according to the current umask
			int mask;
			mask=umask(0);
			umask(mask);
			
			// Note that the umask value is in decimal (511 means octal 0777)
			errno=0;
			int ret=::mkdir(dirname.text(),511 & ~mask);
			int errcode=errno;
			if (ret==-1)
			{
         		if (errcode)
					MessageBox::error(this,BOX_OK_SU,_("Error"),_("Can't create folder %s : %s"),dirname.text(),strerror(errcode));
				else
					MessageBox::error(this,BOX_OK_SU,_("Error"),_("Can't create folder %s"),dirname.text());
    			return 0;
			}
		}							
    }
	
	// Force dirpanel refresh
	list->handle(this,FXSEL(SEL_COMMAND,DirList::ID_REFRESH),NULL);
    return 1;
}


// Run Terminal in the selected directory
long DirPanel::onCmdXTerm(FXObject*,FXSelector,void*)
{
	getApp()->beginWaitCursor();
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString buf=list->getItemPathname((TreeItem*)item);
	chdir(buf.text());
    FXString cmd=getApp()->reg().readStringEntry("PROGS","xterm","xterm -sb");
    cmd += " &";
    system(cmd.text());
	chdir(startlocation.text());
	getApp()->endWaitCursor();
    return 1;
}


#if defined(linux)
// Mount/Unmount directory
long DirPanel::onCmdMount(FXObject*,FXSelector sel,void*)
{
    FXString cmd, msg, text;
	unsigned int op;
	File *f;
	
	// Current item
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString dir=list->getItemPathname((TreeItem*)item);

	// If symbolic link
	if (::isLink(dir))
		dir=FXFile::symlink(dir);

    // Select the command and set the appropriate message
    if(FXSELID(sel)==ID_MOUNT)
    {
        op=MOUNT;
		msg=_("Mount");
        cmd="mount ";
    }
    else
    {
        op=UNMOUNT;
		msg=_("Unmount");
        cmd="umount ";
    }
    cmd+=::quote(dir);
    cmd+=" 2>&1";
    chdir(ROOTDIR);

	// Wait cursor
	getApp()->beginWaitCursor();

	// File object
	text=msg + _(" file system...");
	f=new File(this,text.text(),op);
	f->create();
				   
	// Mount/unmount file system
	text=msg + _(" the folder:");
	f->mount(dir,text,cmd,op);
	chdir(startlocation.text());
 
	// If action is cancelled in progress dialog
	if (f->isCancelled())
	{
		getApp()->endWaitCursor();
		f->hide();
		text=msg + _(" operation cancelled!");
		MessageBox::error(this,BOX_OK,_("Error"),text.text());
		delete f;
		return 0;
	}					
	
	getApp()->endWaitCursor();
	delete f; 

	// Force dirpanel refresh
	list->handle(this,FXSEL(SEL_COMMAND,DirList::ID_REFRESH),NULL);

    return 1;
}

// Update the Mount menu item
long DirPanel::onUpdMount(FXObject* o,FXSelector sel,void*)
{
	// Current item
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString dir=list->getItemPathname((TreeItem*)item);

    if(fsdevices->find(dir.text()) && !mtdevices->find(dir.text()))
        o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
    else
        o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

    return 1;
}


// Update the Unmount menu item
long DirPanel::onUpdUnmount(FXObject* o,FXSelector sel,void*)
{
	// Current item
    DirItem* item=(DirItem*)list->getCurrentItem();
	FXString dir=list->getItemPathname((TreeItem*)item);

    if(fsdevices->find(dir.text()) || mtdevices->find(dir.text()))
        o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
    else
        o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

    return 1;
}
#endif


// Update the paste button
long DirPanel::onUpdPaste(FXObject* o,FXSelector,void*)
{
    unsigned char *data;
    unsigned int len;	
	FXString buf;
	FXbool clipboard_empty=TRUE;
	
	// Lock clipboard to prevent changes in method onCmdRequestClipboard()
	clipboard_locked=TRUE;
	
	// If source is xfelistType (Gnome, XFCE, or Xfe app)
    if (getDNDData(FROM_CLIPBOARD,xfelistType,data,len))
    {		
        FXRESIZE(&data,unsigned char,len+1);
        data[len]='\0';
		buf=(char*)data;
		
		// Check if valid clipboard
		if (buf.find("file:/")>=0)
			clipboard_empty=FALSE;
			
        // Free data pointer
		FXFREE(&data);
	}
		
	// If source type is urilistType (KDE apps ; non Gnome, non XFCE and non Xfe apps)
	else if (getDNDData(FROM_CLIPBOARD,urilistType,data,len))
	{		
		FXRESIZE(&data,unsigned char,len+1);
		data[len]='\0';
		buf=(char*)data;
		
		// Test if valid clipboard
		if (buf.find("file:/")>=0)
			clipboard_empty=FALSE;
		
		// Free data pointer
		FXFREE(&data);
	}
		
	// Gray out the paste button, if necessary
	if (clipboard_empty)
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);

	// Unlock clipboard
	clipboard_locked=FALSE;

    return 1;
}


// Update menu items and toolbar buttons that are related to file operations
long DirPanel::onUpdMenu(FXObject* o,FXSelector,void*)
{
	// Name of the current selected item
	TreeItem* item=(TreeItem*)list->getCurrentItem();
	
	// There is no selected item
	if (item==NULL)
		return 0;
	
	// Path name of the selected item
	FXString dir=list->getItemPathname(item);	
    return 1;
}


// Update menu items and toolbar buttons that are related to file operations
long DirPanel::onUpdDirDelete(FXObject* o,FXSelector,void*)
{
	// Name of the current selected item
	TreeItem* item=(TreeItem*)list->getCurrentItem();
	
	// There is no selected item
	if (item==NULL)
		return 0;
	
	// Path name of the selected item
	FXString dir=list->getItemPathname(item);	

 	FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
	FXbool use_trash_bypass=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_bypass",FALSE);
	if (!use_trash_can | use_trash_bypass)
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

	return 1;
}


// Update menu items and toolbar buttons that are related to file operations
long DirPanel::onUpdDirTrash(FXObject* o,FXSelector,void*)
{
	// Name of the current selected item
	TreeItem* item=(TreeItem*)list->getCurrentItem();
	
	// There is no selected item
	if (item==NULL)
		return 0;
	
	// Path name of the selected item
	FXString dir=list->getItemPathname(item);	

	// Disable move to trash menu if we are in trash can
	// or if the trash can directory is selected
	FXbool trashenable=TRUE;
	FXString trashparentdir=trashlocation.rbefore('/');

	if (dir.left(trashlocation.length())==trashlocation)
		trashenable=FALSE;
		
	if (dir==trashparentdir)
		trashenable=FALSE;

	FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
	if (use_trash_can && trashenable)
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

	return 1;
}


long DirPanel::onUpdDirRestore(FXObject* o,FXSelector,void*)
{
	// Name of the current selected item
	TreeItem* item=(TreeItem*)list->getCurrentItem();
	
	// There is no selected item
	if (item==NULL)
		return 0;
	
	// Path name of the selected item
	FXString dir=list->getItemPathname(item);	

	// Enable restore from trash menu if we are in trash can
	FXbool restoreenable=FALSE;
	if (dir.left(trashfileslocation.length()+1)==trashfileslocation+PATHSEPSTRING)
		restoreenable=TRUE;

	FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
	if (use_trash_can && restoreenable)
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

	return 1;
}


// Toggle dirsize refresh and force refresh if flag is true
void DirPanel::setAllowDirsizeRefresh(FXbool flag)
{
	allowDirsizeRefresh=flag;
	
	// Force refresh
	if (allowDirsizeRefresh)
	{
		curr_dirpath="";
		onCmdDirsizeRefresh(0,0,0);
	}
}


// Refresh the directory size in the status bar
long DirPanel::onCmdDirsizeRefresh(FXObject* sender,FXSelector,void*)
{
	// Don't refresh if not allowed or window is minimized
	if (!allowDirsizeRefresh || ((FXTopWindow*)focuswindow)->isMinimized())
		return 0;

	FXulong dnsize;
	char dsize[64];
	FXString hsize;
		
	// Name of the current selected item
	TreeItem* item=(TreeItem*)list->getCurrentItem();
	
	// There is no selected item
	if (item==NULL)
	{
		status->setText(_("0 bytes in root"));
		return 0;
	}
	
	// Path name of the selected item (without trailing '/' except for the root path)
	FXString path=::filePath(list->getItemPathname(item),"");

	// Compute directory size only if something has changed in directory
	struct stat statbuf;
	if (lstatrep(path.text(),&statbuf)==0)
	{
		if (!(path==curr_dirpath && statbuf.st_mtime == curr_mtime))
		{			
			// Update curr directory mtime
			curr_mtime=statbuf.st_mtime;

			// Update curr directory path
			curr_dirpath=path;

			// Size of the files present in the directory
			//strlcpy(buf,path.text(),path.length()+1);
			dnsize=::dirsize(path.text());

			// Size in human readable form
			snprintf(dsize,sizeof(dsize)-1,"%llu",dnsize);
			hsize=::hSize(dsize);

			// Refresh the status label
			FXString string = hsize +  _(" in root");
			status->setText(string);
		}
	}

    // Reset timer again
    getApp()->addTimeout(this,ID_DIRSIZE_REFRESH,DIRSIZE_REFRESH_INTERVAL);

    // Important : returning 0 here avoids to continuously update the GUI!
    return 0;
}


// Update the path name in the Window title
long DirPanel::onUpdTitle(FXObject* sender,FXSelector,void*)
{
	
	// Name of the current selected item
	TreeItem* item=(TreeItem*)list->getCurrentItem();
	
	// There is no selected item
	if (item==NULL)
	{
		mainWindow->setTitle("Xfe - ");
		return 0;
	}
	
	// Path name of the selected item
	FXString path=list->getItemPathname(item);	

	// Update the path in the window title
	if(getuid()==0)
		mainWindow->setTitle("Xfe (root) - " + path);
	else
		mainWindow->setTitle("Xfe - " + path);

    return 1;
}


// Update dirsize refresh timer if the window gains focus
long DirPanel::onUpdDirsizeRefresh(FXObject*,FXSelector,void*)
{
	static FXbool prevMinimized=TRUE;
	static FXbool minimized=TRUE;

	prevMinimized=minimized;
	if (((FXTopWindow*)focuswindow)->isMinimized())
		minimized=FALSE;
	else
		minimized=TRUE;

	// Update timer if window is unminimized
	if (prevMinimized == FALSE && minimized == TRUE)
		onCmdDirsizeRefresh(0,0,0);

    return 1;
}
