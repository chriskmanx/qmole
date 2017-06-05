// Directory list. Taken from the FOX library and slightly modified.

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <FXPNGIcon.h>
#if defined(linux)
#include <mntent.h>
#endif

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "File.h"
#include "FileDict.h"
#include "InputDialog.h"
#include "MessageBox.h"
#include "XFileExplorer.h"
#include "DirList.h"


// Interval between updevices and mtdevices read (s)
#define UPDEVICES_INTERVAL		300
#define MTDEVICES_INTERVAL		5

// Interval between refreshes (ms)
#define REFRESH_INTERVAL     1000

// File systems not supporting mod-time, refresh every nth time
#define REFRESH_FREQUENCY    30

// Time interval before expanding a folder (ms)
#define EXPAND_INTERVAL		500

// Global variables
#if defined(linux)
extern FXStringDict* fsdevices;
extern FXStringDict* mtdevices;
extern FXStringDict* updevices;
#endif

extern FXbool allowPopupScroll;
extern FXString xdgdatahome;


// Object implementation
FXIMPLEMENT(DirItem,FXTreeItem,NULL,0)



// Map
FXDEFMAP(DirList) DirListMap[]=
{
	FXMAPFUNC(SEL_DRAGGED,0,DirList::onDragged),
	FXMAPFUNC(SEL_TIMEOUT,DirList::ID_REFRESH_TIMER,DirList::onCmdRefreshTimer),
#if defined(linux)
	FXMAPFUNC(SEL_TIMEOUT,DirList::ID_MTDEVICES_REFRESH,DirList::onMtdevicesRefresh),
	FXMAPFUNC(SEL_TIMEOUT,DirList::ID_UPDEVICES_REFRESH,DirList::onUpdevicesRefresh),
#endif
	FXMAPFUNC(SEL_TIMEOUT,DirList::ID_EXPAND_TIMER,DirList::onExpandTimer),
	FXMAPFUNC(SEL_DND_ENTER,0,DirList::onDNDEnter),
	FXMAPFUNC(SEL_DND_LEAVE,0,DirList::onDNDLeave),
	FXMAPFUNC(SEL_DND_DROP,0,DirList::onDNDDrop),
	FXMAPFUNC(SEL_DND_MOTION,0,DirList::onDNDMotion),
	FXMAPFUNC(SEL_DND_REQUEST,0,DirList::onDNDRequest),
	FXMAPFUNC(SEL_BEGINDRAG,0,DirList::onBeginDrag),
	FXMAPFUNC(SEL_ENDDRAG,0,DirList::onEndDrag),
	FXMAPFUNC(SEL_OPENED,0,DirList::onOpened),
	FXMAPFUNC(SEL_CLOSED,0,DirList::onClosed),
	FXMAPFUNC(SEL_EXPANDED,0,DirList::onExpanded),
	FXMAPFUNC(SEL_COLLAPSED,0,DirList::onCollapsed),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_SHOW_HIDDEN,DirList::onUpdShowHidden),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_HIDE_HIDDEN,DirList::onUpdHideHidden),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_TOGGLE_HIDDEN,DirList::onUpdToggleHidden),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_SHOW_FILES,DirList::onUpdShowFiles),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_HIDE_FILES,DirList::onUpdHideFiles),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_TOGGLE_FILES,DirList::onUpdToggleFiles),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_SET_PATTERN,DirList::onUpdSetPattern),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_SORT_REVERSE,DirList::onUpdSortReverse),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_SHOW_HIDDEN,DirList::onCmdShowHidden),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_DRAG_COPY,DirList::onCmdDragCopy),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_DRAG_MOVE,DirList::onCmdDragMove),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_DRAG_LINK,DirList::onCmdDragLink),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_DRAG_REJECT,DirList::onCmdDragReject),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_HIDE_HIDDEN,DirList::onCmdHideHidden),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_TOGGLE_HIDDEN,DirList::onCmdToggleHidden),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_SHOW_FILES,DirList::onCmdShowFiles),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_HIDE_FILES,DirList::onCmdHideFiles),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_TOGGLE_FILES,DirList::onCmdToggleFiles),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_SET_PATTERN,DirList::onCmdSetPattern),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_SORT_REVERSE,DirList::onCmdSortReverse),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_REFRESH,DirList::onCmdRefresh),
	FXMAPFUNC(SEL_COMMAND,DirList::ID_SORT_CASE,DirList::onCmdSortCase),
	FXMAPFUNC(SEL_UPDATE,DirList::ID_SORT_CASE,DirList::onUpdSortCase),
	FXMAPFUNC(SEL_UPDATE,0,DirList::onUpdRefreshTimers),
};


// Object implementation
FXIMPLEMENT(DirList,FXTreeList,DirListMap,ARRAYNUMBER(DirListMap))



// Directory List Widget
DirList::DirList(FXWindow *focuswin,FXComposite *p,FXObject* tgt,FXSelector sel,unsigned int opts,int x,int y,int w,int h):
        FXTreeList(p,tgt,sel,opts,x,y,w,h),pattern("*")
{
    flags|=FLAG_ENABLED|FLAG_DROPTARGET;
    matchmode=FILEMATCH_FILE_NAME|FILEMATCH_NOESCAPE;
    associations=NULL;
    if(!(options&DIRLIST_NO_OWN_ASSOC))
        associations=new FileDict(getApp());
    list=NULL;
    sortfunc=(FXTreeListSortFunc)ascendingCase;
    dropaction=DRAG_MOVE;
    counter=0;
	prevSelItem=NULL;
	focuswindow=focuswin;

#if defined(linux)

    // Initialize the fsdevices, mtdevices and updevices lists
    struct mntent *mnt;
    if (fsdevices==NULL)
    {
        // To list file system devices
        fsdevices=new FXStringDict();
        FILE *fstab=setmntent(FSTAB_PATH,"r");
        if(fstab)
        {
            while((mnt=getmntent(fstab)))
            {
                if(!streq(mnt->mnt_type,MNTTYPE_IGNORE) && !streq(mnt->mnt_type,MNTTYPE_SWAP) 
					&& !streq(mnt->mnt_dir,"/"))
                {
                    if(!strncmp(mnt->mnt_fsname,"/dev/fd",7))
                        fsdevices->insert(mnt->mnt_dir,"floppy");
                    else if (!strncmp(mnt->mnt_type,"iso",3))
                        fsdevices->insert(mnt->mnt_dir,"cdrom");
                    else if (!strncmp(mnt->mnt_fsname,"/dev/zip",8))
                        fsdevices->insert(mnt->mnt_dir,"zip");
                    else if (streq(mnt->mnt_type,"nfs"))
                        fsdevices->insert(mnt->mnt_dir,"nfsdisk");
                    else if (streq(mnt->mnt_type,"smbfs"))
                        fsdevices->insert(mnt->mnt_dir,"smbdisk");
                    else
                        fsdevices->insert(mnt->mnt_dir,"harddisk");
                }
            }
            endmntent(fstab);
        }
    }
    if (mtdevices==NULL)
    {
        // To list mounted devices
        mtdevices=new FXStringDict();
        FILE *mtab=setmntent(MTAB_PATH,"r");
        if(mtab)
        {
            while((mnt=getmntent(mtab)))
			{
				// To fix an issue with some Linux distributions
				FXString mntdir=mnt->mnt_dir;
				if (mntdir!="/dev/.static/dev" &&   mntdir.rfind(".gvfs",5,mntdir.length())==-1)
					mtdevices->insert(mnt->mnt_dir,mnt->mnt_type);
			}
            endmntent(mtab);
        }
    }
	if (updevices==NULL)
    {
        // To mark mount points that are up or down
        updevices=new FXStringDict();
        struct stat statbuf;
        FXString mtstate;
        FILE *mtab=setmntent(MTAB_PATH,"r");
        if(mtab)
        {
            while((mnt=getmntent(mtab)))
            {
				// To fix an issue with some Linux distributions
				FXString mntdir=mnt->mnt_dir;
				if (mntdir!="/dev/.static/dev" &&   mntdir.rfind(".gvfs",5,mntdir.length())==-1)
				{
					if (lstatmt(mnt->mnt_dir,&statbuf)==-1)
						mtstate="down";
					else
						mtstate="up";
					updevices->insert(mnt->mnt_dir,mtstate.text());
				}
            }
            endmntent(mtab);
        }
    }
#endif

	// Trahscan location
	trashfileslocation=xdgdatahome + PATHSEPSTRING TRASHFILESPATH;
	trashinfolocation=xdgdatahome + PATHSEPSTRING TRASHINFOPATH;
}


// Create the directory list
void DirList::create()
{
    FXTreeList::create();
    if(!deleteType)
        deleteType=getApp()->registerDragType(deleteTypeName);
    if(!urilistType)
        urilistType=getApp()->registerDragType(urilistTypeName);
    getApp()->addTimeout(this,ID_REFRESH_TIMER,REFRESH_INTERVAL);
#if defined(linux)
    getApp()->addTimeout(this,ID_MTDEVICES_REFRESH,MTDEVICES_INTERVAL*1000);
    getApp()->addTimeout(this,ID_UPDEVICES_REFRESH,UPDEVICES_INTERVAL*1000);
#endif
    dropEnable();

    // Scan root directory
    scan(FALSE);
}


// Expand folder tree when hovering long over a folder
long DirList::onExpandTimer(FXObject* sender,FXSelector sel,void* ptr)
{
    int xx,yy;
    unsigned int state;
    DirItem *item;

    getCursorPosition(xx,yy,state);
    item=(DirItem*)getItemAt(xx,yy);

    if(!(item->state&DirItem::FOLDER))
        return 0;

    // Expand tree item
    expandTree((TreeItem*)item,TRUE);
    scan(TRUE);

    // Set open timer
    getApp()->addTimeout(this,ID_EXPAND_TIMER,EXPAND_INTERVAL);

    return 1;

}

// Create item
TreeItem* DirList::createItem(const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr)
{
    return (TreeItem*) new DirItem(text,oi,ci,ptr);
}


// Sort ascending order, keeping directories first
int DirList::ascending(const FXTreeItem* pa,const FXTreeItem* pb)
{
    register const DirItem *a=(DirItem*)pa;
    register const DirItem *b=(DirItem*)pb;
    register int diff=(int)b->isDirectory() - (int)a->isDirectory();
    return diff ? diff : compare(a->label,b->label);
}


// Sort descending order, keeping directories first
int DirList::descending(const FXTreeItem* pa,const FXTreeItem* pb)
{
    register const DirItem *a=(DirItem*)pa;
    register const DirItem *b=(DirItem*)pb;
    register int diff=(int)b->isDirectory() - (int)a->isDirectory();
    return diff ? diff : compare(b->label,a->label);
}


// Sort ascending order, case insensitive, keeping directories first
int DirList::ascendingCase(const FXTreeItem* pa,const FXTreeItem* pb)
{
    register const DirItem *a=(DirItem*)pa;
    register const DirItem *b=(DirItem*)pb;
    register int diff=(int)b->isDirectory() - (int)a->isDirectory();
    return diff ? diff : comparecase(a->label,b->label);
}


// Sort descending order, case insensitive, keeping directories first
int DirList::descendingCase(const FXTreeItem* pa,const FXTreeItem* pb)
{
    register const DirItem *a=(DirItem*)pa;
    register const DirItem *b=(DirItem*)pb;
    register int diff=(int)b->isDirectory() - (int)a->isDirectory();
    return diff ? diff : comparecase(b->label,a->label);
}



// Handle drag-and-drop enter
long DirList::onDNDEnter(FXObject* sender,FXSelector sel,void* ptr)
{
    FXTreeList::onDNDEnter(sender,sel,ptr);
    return 1;
}


// Handle drag-and-drop leave
long DirList::onDNDLeave(FXObject* sender,FXSelector sel,void* ptr)
{
    // Cancel open up timer
    getApp()->removeTimeout(this,ID_EXPAND_TIMER);

    stopAutoScroll();
    FXTreeList::onDNDLeave(sender,sel,ptr);
    if(prevSelItem)
    {
        if(!isItemCurrent(prevSelItem))
            closeItem(prevSelItem);
        prevSelItem = NULL;
    }
    return 1;
}


// Handle drag-and-drop motion
long DirList::onDNDMotion(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent *event=(FXEvent*)ptr;
    TreeItem *item;

    // Cancel open up timer
    getApp()->removeTimeout(this,ID_EXPAND_TIMER);

    // Start autoscrolling
    if(startAutoScroll(event,FALSE))
        return 1;

    // Give base class a shot
    if(FXTreeList::onDNDMotion(sender,sel,ptr))
        return 1;

    // Dropping list of filenames
    if(offeredDNDType(FROM_DRAGNDROP,urilistType))
    {
        // Locate drop place
        item=(TreeItem*)getItemAt(event->win_x,event->win_y);

        // We can drop in a directory
        if(item && isItemDirectory(item))
        {
            // Get drop directory
            dropdirectory=getItemPathname(item);

            // What is being done (move,copy,link)
            dropaction=inquireDNDAction();

            // Set open up timer
            getApp()->addTimeout(this,ID_EXPAND_TIMER,EXPAND_INTERVAL);

            // Set icon to open folder icon
			setItemOpenIcon(item,minifolderopenicon);

			// See if this is writable
            if(::isWritable(dropdirectory))
            {
                acceptDrop(DRAG_ACCEPT);
                int x,y;
                unsigned int state;
                getCursorPosition(x,y,state);
                TreeItem* item=(TreeItem*)getItemAt(x,y);

                if(prevSelItem && prevSelItem != item)
                {
                    if(!isItemCurrent(prevSelItem))
                        closeItem(prevSelItem);
                    prevSelItem = NULL;
                }
                if(item && prevSelItem != item)
                {
                    openItem(item);
                    prevSelItem = item;
                }
            }
        }
        return 1;
    }
    return 0;
}


// Set drag type to copy
long DirList::onCmdDragCopy(FXObject* sender,FXSelector sel,void* ptr)
{
	dropaction=DRAG_COPY;
	return 1;
}


// Set drag type to move
long DirList::onCmdDragMove(FXObject* sender,FXSelector sel,void* ptr)
{
	dropaction=DRAG_MOVE;
	return 1;
}


// Set drag type to symlink
long DirList::onCmdDragLink(FXObject* sender,FXSelector sel,void* ptr)
{
	dropaction=DRAG_LINK;
	return 1;
}


// Cancel drag action
long DirList::onCmdDragReject(FXObject* sender,FXSelector sel,void* ptr)
{
	dropaction=DRAG_REJECT;
	return 1;
}


// Handle drag-and-drop drop
long DirList::onDNDDrop(FXObject* sender,FXSelector sel,void* ptr)
{
	unsigned char *data;
    unsigned int len;
    FXbool showdialog=TRUE;
	int ret;
    File *f=NULL;

    FXbool ask_before_copy=getApp()->reg().readUnsignedEntry("OPTIONS","ask_before_copy",TRUE);
    FXbool confirm_dnd=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_drag_and_drop",TRUE);
	
    // Cancel open up timer
    getApp()->removeTimeout(this,ID_EXPAND_TIMER);

    // Stop scrolling
    stopAutoScroll();

    // Perhaps target wants to deal with it
    if(FXTreeList::onDNDDrop(sender,sel,ptr))
        return 1;
	
	// Check if control key or shift key were pressed
	FXbool ctrlshiftkey=FALSE;
	if (ptr!=NULL)
	{
		FXEvent* event=(FXEvent*)ptr;
		if (event->state&CONTROLMASK)
			ctrlshiftkey=TRUE;
		if (event->state&SHIFTMASK)
			ctrlshiftkey=TRUE;
	}

    // Get DND data
    // This is done before displaying the popup menu to fix a drag and drop problem with konqueror and dolphin file managers
    FXbool dnd=getDNDData(FROM_DRAGNDROP,urilistType,data,len);

	// Display the dnd dialog if the control or shift key were not pressed
	if (confirm_dnd & !ctrlshiftkey)
	{
		// Display a popup to select the drag type
		dropaction=DRAG_REJECT;
		FXMenuPane menu(this);
		int x,y;
		unsigned int state;
		getRoot()->getCursorPosition(x,y,state);
		new FXMenuCommand(&menu,_("Copy here"),copy_clpicon,this,DirList::ID_DRAG_COPY);
		new FXMenuCommand(&menu,_("Move here"),moveiticon,this,DirList::ID_DRAG_MOVE);
		new FXMenuCommand(&menu,_("Link here"),minilinkicon,this,DirList::ID_DRAG_LINK);
		new FXMenuSeparator(&menu);
		new FXMenuCommand(&menu,_("Cancel"),NULL,this,DirList::ID_DRAG_REJECT);
		menu.create();
		allowPopupScroll=TRUE;  // Allow keyboard scrolling
		menu.popup(NULL,x,y);
		getApp()->runModalWhileShown(&menu);
		allowPopupScroll=FALSE;
	}

	// Close item
	if(prevSelItem)
    {
        if(!isItemCurrent(prevSelItem))
            closeItem(prevSelItem);
        prevSelItem = NULL;
    }

    // Get uri-list of files being dropped
    //if(getDNDData(FROM_DRAGNDROP,urilistType,data,len))
    if(dnd)  // See comment upper
    {
        FXRESIZE(&data,unsigned char,len+1);
        data[len]='\0';
        char *p,*q;
        p=q=(char*)data;
        
		// Number of selected items
		FXString buf=p;
        int num=buf.contains('\n')+1;
		
		// Eventually correct the number of selected items
		// because sometimes there is another '\n' at the end of the string
		int pos=buf.rfind('\n');
		if (pos==buf.length()-1)
			num=num-1;

        // File object
        if (dropaction==DRAG_COPY)
            f=new File(this,_("File copy"),COPY);
        else if (dropaction==DRAG_MOVE)
            f=new File(this,_("File move"),MOVE);
        else if (dropaction==DRAG_LINK)
			f=new File(this,_("File symlink"),SYMLINK);
		else
		{
	        FXFREE(&data);
            return 0;
		}

        // Target directory
		FXString targetdir=dropdirectory;

        while(*p)
        {
            while(*q && *q!='\r')
                q++;
            FXString url(p,q-p);
            FXString source(FXURL::fileFromURL(url));
            FXString target(targetdir);
            FXString sourcedir=FXPath::directory(source);
 
            // File operation dialog, if needed
            if ((!confirm_dnd | ctrlshiftkey) & ask_before_copy & showdialog)
            {
                FXIcon *icon=NULL;
                FXString title,message;
                if (dropaction==DRAG_COPY)
                {
                    title=_("Copy ");
                    icon = copy_bigicon;
					if (num==1)
                        message=title+source;
                    else				
						title.format(_("Copy %s files/folders.\nFrom: %s"),FXStringVal(num).text(),sourcedir.text());
                }
                else if (dropaction==DRAG_MOVE)
                {
                    title=_("Move ");
                    icon = move_bigicon;
                    if (num==1)
                        message=title+source;
                    else
						title.format(_("Move %s files/folders.\nFrom: %s"),FXStringVal(num).text(),sourcedir.text());
                }
                else if ((dropaction==DRAG_LINK) && (num==1))
                {
                    title=_("Symlink ");
                    icon=link_bigicon;
					message=title+source;
                }

                InputDialog* dialog = new InputDialog(this,targetdir,message,title,_("To:"),icon);
                dialog->CursorEnd();
                int rc=1;
                rc=dialog->execute();
                target=dialog->getText();
                target=::filePath(target);
                if (num>1)
                    showdialog=FALSE;
                delete dialog;
                if (!rc)
                    return 0;
            }

            // Move the source file
            if(dropaction==DRAG_MOVE)
            {
                // Move file
                f->create();

				// If target file is located at trash location, also create the corresponding trashinfo file
				// Do it silently and don't report any error if it fails
				FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
				
				if (use_trash_can && FXPath::directory(target)==trashfileslocation )
				{						
					// Trash files path name
					FXString trashpathname=createTrashpathname(source,trashfileslocation);
					
					// Adjust target name to get the _N suffix if any
					FXString trashtarget=FXPath::directory(target)+PATHSEPSTRING+FXPath::name(trashpathname);

					// Create trashinfo file
					createTrashinfo(source,trashpathname,trashfileslocation,trashinfolocation);

					// Move source to trash target
					ret=f->move(source,trashtarget);
				}

				// Move source to target
				else
				{
					//target=FXPath::directory(target);
					ret=f->move(source,target);
				}

				// If source file is located at trash location, try to also remove the corresponding trashinfo if it exists
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

				// Set directory to the source parent
				setDirectory(sourcedir,FALSE);
            }
            // Copy the source file
            else if(dropaction==DRAG_COPY)
            {
                // Copy file
                f->create();

				// If target file is located at trash location, also create the corresponding trashinfo file
				// Do it silently and don't report any error if it fails
				FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
				
				if (use_trash_can && FXPath::directory(target)==trashfileslocation )
				{						
					// Trash files path name
					FXString trashpathname=createTrashpathname(source,trashfileslocation);
					
					// Adjust target name to get the _N suffix if any
					FXString trashtarget=FXPath::directory(target)+PATHSEPSTRING+FXPath::name(trashpathname);

					// Create trashinfo file
					createTrashinfo(source,trashpathname,trashfileslocation,trashinfolocation);

					// Copy source to trash target
					ret=f->copy(source,trashtarget);
				}

				// Copy source to target
				else
				{
					//target=FXPath::directory(target);
					ret=f->copy(source,target);
				}

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
            // Link the source file (no progress dialog in this case)
            else if(dropaction==DRAG_LINK)
            {
                // Link file
                f->create();
                f->symlink(source,target);
            }
            if(*q=='\r')
                q+=2;
            p=q;
        }
        delete f;
        FXFREE(&data);
        
		// Force a refresh of the DirList
		onCmdRefresh(0,0,0);

		return 1;
    }
    return 0;
}


// Somebody wants our dragged data
long DirList::onDNDRequest(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent *event=(FXEvent*)ptr;
    unsigned char *data;
    unsigned int len;

    // Perhaps the target wants to supply its own data
    if(FXTreeList::onDNDRequest(sender,sel,ptr))
        return 1;

    // Return list of filenames as a uri-list
    if(event->target==urilistType)
    {
        if(!dragfiles.empty())
        {
            len=dragfiles.length();
            FXMEMDUP(&data,dragfiles.text(),unsigned char,len);
            setDNDData(FROM_DRAGNDROP,event->target,data,len);
        }
        return 1;
    }

    // Delete selected files
    if(event->target==deleteType)
        return 1;

    return 0;
}


// Start a drag operation
long DirList::onBeginDrag(FXObject* sender,FXSelector sel,void* ptr)
{
    register TreeItem *item;
    if(FXTreeList::onBeginDrag(sender,sel,ptr))
        return 1;
    if(beginDrag(&urilistType,1))
    {
        dragfiles=FXString::null;
        item=(TreeItem*)firstitem;
        while(item)
        {
            if(item->isSelected())
            {
                if(!dragfiles.empty())
                    dragfiles+="\r\n";
                dragfiles+=::fileToURI(getItemPathname(item));
            }
            if(item->first)
                item=(TreeItem*)item->first;
            else
            {
                while(!item->next && item->parent)
                    item=(TreeItem*)item->parent;
                item=(TreeItem*)item->next;
            }
        }
        return 1;
    }
    return 0;
}


// End drag operation
long DirList::onEndDrag(FXObject* sender,FXSelector sel,void* ptr)
{
    if(FXTreeList::onEndDrag(sender,sel,ptr))
        return 1;
    endDrag((didAccept()!=DRAG_REJECT));
    setDragCursor(getDefaultCursor());

    return 1;
}


// Dragged stuff around
long DirList::onDragged(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    FXDragAction action;
    if(FXTreeList::onDragged(sender,sel,ptr))
        return 1;
    action=DRAG_MOVE;
    if(event->state&CONTROLMASK)
        action=DRAG_COPY;
    if(event->state&SHIFTMASK)
        action=DRAG_MOVE;
	if((event->state&CONTROLMASK) && (event->state&SHIFTMASK))
		action=DRAG_LINK;
    handleDrag(event->root_x,event->root_y,action);
    if(didAccept()!=DRAG_REJECT)
    {
        if(action==DRAG_MOVE)
            setDragCursor(getApp()->getDefaultCursor(DEF_DNDMOVE_CURSOR));
    	else if(action==DRAG_LINK)
			setDragCursor(getApp()->getDefaultCursor(DEF_DNDLINK_CURSOR));
        else
            setDragCursor(getApp()->getDefaultCursor(DEF_DNDCOPY_CURSOR));
    }
    else
        setDragCursor(getApp()->getDefaultCursor(DEF_DNDSTOP_CURSOR));
    return 1;
}


// Toggle hidden files
long DirList::onCmdToggleHidden(FXObject*,FXSelector,void*)
{
    showHiddenFiles(!shownHiddenFiles());
    return 1;
}


// Update toggle hidden files widget
long DirList::onUpdToggleHidden(FXObject* sender,FXSelector,void*)
{
    if(shownHiddenFiles())
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Show hidden files
long DirList::onCmdShowHidden(FXObject*,FXSelector,void*)
{
    showHiddenFiles(TRUE);
    return 1;
}


// Update show hidden files widget
long DirList::onUpdShowHidden(FXObject* sender,FXSelector,void*)
{
    if(shownHiddenFiles())
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Hide hidden files
long DirList::onCmdHideHidden(FXObject*,FXSelector,void*)
{
    showHiddenFiles(FALSE);
    return 1;
}


// Update hide hidden files widget
long DirList::onUpdHideHidden(FXObject* sender,FXSelector,void*)
{
    if(!shownHiddenFiles())
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Toggle files display
long DirList::onCmdToggleFiles(FXObject*,FXSelector,void*)
{
    showFiles(!showFiles());
    return 1;
}


// Update toggle files widget
long DirList::onUpdToggleFiles(FXObject* sender,FXSelector,void*)
{
    if(showFiles())
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Show files
long DirList::onCmdShowFiles(FXObject*,FXSelector,void*)
{
    showFiles(TRUE);
    return 1;
}


// Update show files widget
long DirList::onUpdShowFiles(FXObject* sender,FXSelector,void*)
{
    if(showFiles())
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Hide files
long DirList::onCmdHideFiles(FXObject*,FXSelector,void*)
{
    showFiles(FALSE);
    return 1;
}


// Update hide files widget
long DirList::onUpdHideFiles(FXObject* sender,FXSelector,void*)
{
    if(!showFiles())
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Change pattern
long DirList::onCmdSetPattern(FXObject*,FXSelector,void* ptr)
{
    if(!ptr)
        return 0;
    setPattern((const char*)ptr);
    return 1;
}


// Update pattern
long DirList::onUpdSetPattern(FXObject* sender,FXSelector,void*)
{
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETVALUE),(void*)pattern.text());
    return 1;
}


// Reverse sort order
long DirList::onCmdSortReverse(FXObject*,FXSelector,void*)
{
    if(sortfunc==(FXTreeListSortFunc)ascending)
        sortfunc=(FXTreeListSortFunc)descending;
    else if(sortfunc==(FXTreeListSortFunc)descending)
        sortfunc=(FXTreeListSortFunc)ascending;
    else if(sortfunc==(FXTreeListSortFunc)ascendingCase)
        sortfunc=(FXTreeListSortFunc)descendingCase;
    else if(sortfunc==(FXTreeListSortFunc)descendingCase)
        sortfunc=(FXTreeListSortFunc)ascendingCase;
    scan(TRUE);
    return 1;
}


// Update sender
long DirList::onUpdSortReverse(FXObject* sender,FXSelector,void* ptr)
{
    sender->handle(this,(sortfunc==(FXTreeListSortFunc)descending || sortfunc==(FXTreeListSortFunc)descendingCase) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),ptr);
    return 1;
}

// Toggle case sensitivity
long DirList::onCmdSortCase(FXObject*,FXSelector,void*)
{
    if(sortfunc==(FXTreeListSortFunc)ascending)
        sortfunc=(FXTreeListSortFunc)ascendingCase;
    else if(sortfunc==(FXTreeListSortFunc)descending)
        sortfunc=(FXTreeListSortFunc)descendingCase;
    else if(sortfunc==(FXTreeListSortFunc)ascendingCase)
        sortfunc=(FXTreeListSortFunc)ascending;
    else if(sortfunc==(FXTreeListSortFunc)descendingCase)
        sortfunc=(FXTreeListSortFunc)descending;
    scan(TRUE);
    return 1;
}


// Check if case sensitive
long DirList::onUpdSortCase(FXObject* sender,FXSelector,void* ptr)
{
    sender->handle(this,(sortfunc==(FXTreeListSortFunc)ascendingCase || sortfunc==(FXTreeListSortFunc)descendingCase) ? FXSEL(SEL_COMMAND,ID_CHECK) : FXSEL(SEL_COMMAND,ID_UNCHECK),ptr);
    return 1;
}


// Close directory
long DirList::onClosed(FXObject*,FXSelector,void* ptr)
{
    DirItem *item=(DirItem*)ptr;
    if(item->state&DirItem::FOLDER)
        return target && target->handle(this,FXSEL(SEL_CLOSED,message),ptr);

    return 1;
}


// Open directory
long DirList::onOpened(FXObject*,FXSelector,void* ptr)
{
    DirItem *item=(DirItem*)ptr;
    if(item->state&DirItem::FOLDER)
        return target && target->handle(this,FXSEL(SEL_OPENED,message),ptr);
    return 1;
}


// Item opened
long DirList::onExpanded(FXObject* sender,FXSelector sel,void* ptr)
{
    DirItem *item=(DirItem*)ptr;

    if(!(item->state&DirItem::FOLDER))
        return 0;

    // Expand tree item
    expandTree((TreeItem*)item,TRUE);
    listChildItems(item);

    // Now we know for sure whether we really have subitems or not
    if(!item->first)
        item->state&=~DirItem::HASITEMS;
    else
        item->state|=DirItem::HASITEMS;

    sortChildItems(item);
    return 1;
}


// Item closed
long DirList::onCollapsed(FXObject* sender,FXSelector sel,void* ptr)
{
    DirItem *item=(DirItem*)ptr;
    if(!(item->state&DirItem::FOLDER))
        return 0;

    // Collapse tree item
    collapseTree((TreeItem*)item,TRUE);

    return 1;
}



// Expand tree
FXbool DirList::expandTree(TreeItem* tree,FXbool notify)
{
    if(FXTreeList::expandTree(tree,notify))
    {
        if(isItemDirectory(tree))
        {
            listChildItems((DirItem*)tree);
            sortChildItems(tree);
        }
        return TRUE;
    }
    return FALSE;
}


// Collapse tree
FXbool DirList::collapseTree(TreeItem* tree,FXbool notify)
{
    if(FXTreeList::collapseTree(tree,notify))
    {
        if(isItemDirectory(tree))
        {
            // As a memory saving feature, all knowledge below this item
            // is deleted; we'll just recreate it when its reexpanded!
           removeItems(tree->first,tree->last);
           recalc();
        }
        return TRUE;
    }
    return FALSE;
}


#if defined(linux)
// To periodically scan /proc/mounts and refresh the mtdevices list
long DirList::onMtdevicesRefresh(FXObject*,FXSelector,void*)
{
	// Don't refresh if window is minimized
	if (((FXTopWindow*)focuswindow)->isMinimized())
		return 0;
 
	struct mntent *mnt;
	FXStringDict* tmpdict = new FXStringDict();
	FILE *mtab=setmntent(MTAB_PATH,"r");
	if(mtab)
	{
		while((mnt=getmntent(mtab)))
		{
			// To fix an issue with some Linux distributions
			FXString mntdir=mnt->mnt_dir;
			if (mntdir!="/dev/.static/dev" &&   mntdir.rfind(".gvfs",5,mntdir.length())==-1)
			{
				tmpdict->insert(mnt->mnt_dir,"");
				if (mtdevices->find(mnt->mnt_dir))
					mtdevices->remove(mnt->mnt_dir);
				mtdevices->insert(mnt->mnt_dir,mnt->mnt_type);
			}
		}
		endmntent(mtab);
	}

	// Remove mount points that don't exist anymore
	int s;
	const char *key, *data;
	for (s = mtdevices->first(); s < mtdevices->size(); s = mtdevices->next(s))
	{
		key = mtdevices->key(s);
		data = mtdevices->data(s);
		if (!tmpdict->find(mtdevices->key(s)))
			mtdevices->remove(mtdevices->key(s));
	}
	delete tmpdict;
    
	// Reset timer again
    getApp()->addTimeout(this,ID_MTDEVICES_REFRESH,MTDEVICES_INTERVAL*1000);
    return 0;
}


// To periodically scan /proc/mounts and detect up and down mounted devices
// NB : the refresh period is much longer than for onMtdevicesRefresh
long DirList::onUpdevicesRefresh(FXObject*,FXSelector,void*)
{
	// Don't refresh if window is minimized
	if (((FXTopWindow*)focuswindow)->isMinimized())
		return 0;

    struct mntent *mnt;
    struct stat statbuf;
    FXString mtstate;

    FXbool mount_warn=getApp()->reg().readUnsignedEntry("OPTIONS","mount_warn",TRUE);
	
	FXStringDict* tmpdict = new FXStringDict();
    FILE *mtab=setmntent(MTAB_PATH,"r");
    if(mtab)
    {
        while((mnt=getmntent(mtab)))
        {			
			// To fix an issue with some Linux distributions
			FXString mntdir=mnt->mnt_dir;
			if (mntdir!="/dev/.static/dev" &&   mntdir.rfind(".gvfs",5,mntdir.length())==-1)
			{
				tmpdict->insert(mnt->mnt_dir,"");

				if (lstatmt(mnt->mnt_dir,&statbuf)==-1)
				{
					mtstate="down";
					if (mount_warn)
						MessageBox::warning(this,BOX_OK,_("Warning"),_("Mount point %s is not responding..."),mnt->mnt_dir);
				}
				else
					mtstate="up";
				
				if (updevices->find(mnt->mnt_dir))
					updevices->remove(mnt->mnt_dir);
				updevices->insert(mnt->mnt_dir,mtstate.text());
			}

        }
        endmntent(mtab);
    }

    // Remove mount points that don't exist anymore
    int s;
    const char *key, *data;
    for (s = updevices->first(); s < updevices->size(); s = updevices->next(s))
    {
        key = updevices->key(s);
        data = updevices->data(s);
        if (!tmpdict->find(updevices->key(s)))
            updevices->remove(updevices->key(s));

    }
    delete tmpdict;

    // Reset timer again
    getApp()->addTimeout(this,ID_UPDEVICES_REFRESH,UPDEVICES_INTERVAL*1000);
    return 0;
}
#endif


// Refresh with timer
long DirList::onCmdRefreshTimer(FXObject*,FXSelector,void*)
{
	// Don't refresh if window is minimized
	if (((FXTopWindow*)focuswindow)->isMinimized())
		return 0;

    if(flags&FLAG_UPDATE)
    {
        scan(FALSE);
        counter=(counter+1)%REFRESH_FREQUENCY;
    }

    // Reset timer again
    getApp()->addTimeout(this,ID_REFRESH_TIMER,REFRESH_INTERVAL);
    return 0;
}


// Force refresh
long DirList::onCmdRefresh(FXObject*,FXSelector,void*)
{
    scan(TRUE);
    return 0;
}


// Scan items to see if listing is necessary
void DirList::scan(FXbool force)
{
    FXString pathname;
    struct stat info;
    DirItem *item;

    // Do root first time
    if(!firstitem || force)
    {
        listRootItems();
        sortRootItems();
    }

    // Check all items
    item=(DirItem*)firstitem;
    while(item)
    {
        // Is expanded directory?
        if(item->isDirectory() && item->isExpanded())
        {
            // Get the full path of the item
            pathname=getItemPathname((TreeItem*)item);

            // Stat this directory
			if (statrep(pathname.text(),&info)==0)
			{
				// Get the mod date of the item
				FXTime newdate=(FXTime)FXMAX(info.st_mtime,info.st_ctime);

				// Forced, date was changed, or failed to get proper date and counter expired
				if(force || (item->date!=newdate) || (counter==0))
				{
					// And do the refresh
					listChildItems(item);
					sortChildItems(item);

					// Remember when we did this
					item->date=newdate;
				}

				// Go deeper
				if(item->first)
				{
					item=(DirItem*)item->first;
					continue;
				}
			}
			
			// Directory does not exist
			else
			{
				// Go to parent and rescan
				setDirectory(FXPath::directory(pathname),FALSE);
				scan(TRUE);
				break;
			}	
        }

        // Go up
        while(!item->next && item->parent)
        {
            item=(DirItem*)item->parent;
		}

        // Go to next
        item=(DirItem*)item->next;
        
    }
}



// List root directories
void DirList::listRootItems()
{
    DirItem *item=(DirItem*)firstitem;
    FXIcon *openicon, *closedicon;
    FileAssoc *fileassoc;

    // First time, make root node
    if(!item)
        item=list=(DirItem*)appendItem(NULL,PATHSEPSTRING,harddiskicon,harddiskicon,NULL,TRUE);

    // Root is a directory, has items under it, and is searchable
    item->state|=DirItem::FOLDER|DirItem::HASITEMS;
    item->state&=~(DirItem::CHARDEV|DirItem::BLOCKDEV|DirItem::FIFO|DirItem::SOCK|DirItem::SYMLINK|DirItem::EXECUTABLE);

    // Determine associations, icons and type
    fileassoc=NULL;
    openicon=harddiskicon;
    closedicon=harddiskicon;
    if(associations)
        fileassoc=associations->findDirBinding(PATHSEPSTRING);

    // If association is found, use it
    if(fileassoc)
    {
        if(fileassoc->miniicon)
            closedicon=fileassoc->miniicon;
        if(fileassoc->miniiconopen)
            openicon=fileassoc->miniiconopen;
    }

    // Update item information
    item->openIcon=openicon;
    item->closedIcon=closedicon;
    item->size=0L;
    item->assoc=fileassoc;
    item->date=0;

    // Create item
    if(id())
        item->create();

    // Need to layout
    recalc();
}


// List child items
void DirList::listChildItems(DirItem *par)
{
    DirItem *oldlist, *newlist, **po, **pp, **pn, *item, *link;
    FXIcon *openicon, *closedicon;
    FileAssoc *fileassoc;
    DIR *dirp;
    struct dirent *dp;
    struct stat    info;
    FXString pathname, directory, name;
	FXString type, mod, usrid, grpid, atts, del;
	FXString timeformat;
    int islink;
	long deldate;

    // Path to parent node
    directory=getItemPathname((TreeItem*)par);

	// Read time format
	timeformat=getApp()->reg().readStringEntry("SETTINGS","time_format",DEFAULT_TIME_FORMAT);

    // Build new insert-order list
    oldlist=par->list;
    newlist=NULL;

    // Assemble lists
    po=&oldlist;
    pn=&newlist;

    // Get directory stream pointer
    dirp=opendir(directory.text());

    // Managed to open directory
    if(dirp)
    {
        // Process directory entries
#ifdef FOX_THREAD_SAFE
        struct fxdirent dirresult;
        while(!readdir_r(dirp,&dirresult,&dp) && dp)
        {
#else
        while((dp=readdir(dirp))!=NULL)
        {
#endif
            // Get name of entry
            name=dp->d_name;
			
            // A dot special file?
            if(name[0]=='.' && (name[1]==0 || (name[1]=='.' && name[2]==0)))
                continue;

            // Hidden file or directory normally not shown
            if(name[0]=='.' && !(options&DIRLIST_SHOWHIDDEN))
                continue;

            // Build full pathname of entry
            pathname=directory;
            if(!ISPATHSEP(pathname[pathname.length()-1]))
                pathname+=PATHSEPSTRING;
            pathname+=name;

            // Get file/link info
			if(lstatrep(pathname.text(),&info)!=0)
                continue;

            // If its a link, get the info on file itself
            islink=S_ISLNK(info.st_mode);
			if(islink && statrep(pathname.text(),&info)!=0)
                continue;

            // If it is not a directory, and not showing files and matching pattern skip it
            if(!S_ISDIR(info.st_mode) && !((options&DIRLIST_SHOWFILES) && FXPath::match(pattern,name,matchmode)))
                continue;
		   
		    // Find it, and take it out from the old list if found
            for(pp=po; (item=*pp)!=NULL; pp=&item->link)
            {
                if(compare(item->label,name)==0)
                {
                    *pp=item->link;
                    item->link=NULL;
                    po=pp;
                    goto fnd;
                }
            }

            // Not found; prepend before list
            item=(DirItem*)appendItem(par,name,minifolderopenicon,minifolderclosedicon,NULL,TRUE);

            // Next gets hung after this one
fnd:
            *pn=item;
            pn=&item->link;

            // Item flags
            if(info.st_mode&(S_IXUSR|S_IXGRP|S_IXOTH))
                item->state|=DirItem::EXECUTABLE;
            else
                item->state&=~DirItem::EXECUTABLE;

            if(S_ISDIR(info.st_mode))
            {
                item->state|=DirItem::FOLDER;
                item->state&=~DirItem::EXECUTABLE;
            }
            else
                item->state&=~(DirItem::FOLDER|DirItem::HASITEMS);

            if(S_ISCHR(info.st_mode))
            {
                item->state|=DirItem::CHARDEV;
                item->state&=~DirItem::EXECUTABLE;
            }
            else
                item->state&=~DirItem::CHARDEV;

            if(S_ISBLK(info.st_mode))
            {
                item->state|=DirItem::BLOCKDEV;
                item->state&=~DirItem::EXECUTABLE;
            }
            else
                item->state&=~DirItem::BLOCKDEV;

            if(S_ISFIFO(info.st_mode))
            {
                item->state|=DirItem::FIFO;
                item->state&=~DirItem::EXECUTABLE;
            }
            else
                item->state&=~DirItem::FIFO;

            if(S_ISSOCK(info.st_mode))
            {
                item->state|=DirItem::SOCK;
                item->state&=~DirItem::EXECUTABLE;
            }
            else
                item->state&=~DirItem::SOCK;

            if(islink)
                item->state|=DirItem::SYMLINK;
            else
            {
                item->state&=~DirItem::SYMLINK;
            }

            // We can drag items
            item->state|=DirItem::DRAGGABLE;

            // Assume no associations
            fileassoc=NULL;

            // Determine icons and type
            if(item->state&DirItem::FOLDER)
            {
                if(!::isReadExecutable(pathname))
                {
                    openicon=minifolderlockedicon;
                    closedicon=minifolderlockedicon;
                }
                else
                {
                    openicon=minifolderopenicon;
                    closedicon=minifolderclosedicon;
                }
                if(associations)
                    fileassoc=associations->findDirBinding(pathname.text());
            }
            else if(item->state&DirItem::EXECUTABLE)
            {
                openicon=miniappicon;
                closedicon=miniappicon;
                if(associations)
                    fileassoc=associations->findExecBinding(pathname.text());
            }
            else
            {
                openicon=minidocicon;
                closedicon=minidocicon;
                if(associations)
                    fileassoc=associations->findFileBinding(pathname.text());
            }

            // If association is found, use it
            if(fileassoc)
            {
                if(fileassoc->miniicon)
                    closedicon=fileassoc->miniicon;
                if(fileassoc->miniiconopen)
                    openicon=fileassoc->miniiconopen;
            }

            // Update item information
            item->openIcon=openicon;
            item->closedIcon=closedicon;
            item->size=(unsigned long)info.st_size;
            item->assoc=fileassoc;
            item->date=info.st_mtime;

			// Set the HASITEMS flag			
			(hasSubDirs(pathname.text())==1 ? item->setHasItems(TRUE) : item->setHasItems(FALSE));

			// Default folder type
			type=_("Folder");

			// Obtain user name
			FXString usrid=FXSystem::userName(info.st_uid);

			// Obtain group name
			FXString grpid=FXSystem::groupName(info.st_gid);

			// Permissions (caution : we don't use the FXSystem::modeString() function because
			// it seems to be incompatible with the info.st_mode format)
			FXString atts=::permissions(info.st_mode);
			
			// Modification time
			mod=FXSystem::time(timeformat.text(),item->date);

			// If we are in trash can, obtain the deletion time
			deldate=0;
			del="";
			if (FXPath::directory(pathname)==trashfileslocation)
			{
				char *endptr;
				FXString str;
				str=pathname.rafter('_');
				str=str.rbefore('-');
				deldate=strtol(str.text(),&endptr,10);
				if (deldate!=0)
					del=FXSystem::time(timeformat.text(),deldate);
			}

#if defined(linux)
			// Mounted devices may have a specific icon
			if(mtdevices->find(pathname.text()))
			{
				type=_("Mount point");
				
				if(streq(mtdevices->find(pathname.text()),"cifs"))
				{
					item->closedIcon=nfsdriveicon;
					item->openIcon=nfsdriveicon;
				}
				else
				{
					item->closedIcon=harddiskicon;
					item->openIcon=harddiskicon;
				}
			}

            // Devices found in fstab may have a specific icon
            if(fsdevices->find(pathname.text()))
            {
				type=_("Mount point");
				
                if(streq(fsdevices->find(pathname.text()),"harddisk"))
                {
                    item->closedIcon=harddiskicon;
                    item->openIcon=harddiskicon;
                }
                else if(streq(fsdevices->find(pathname.text()),"nfsdisk"))
                {
                    item->closedIcon=nfsdriveicon;
                    item->openIcon=nfsdriveicon;
                }
                else if(streq(fsdevices->find(pathname.text()),"smbdisk"))
                {
                    item->closedIcon=nfsdriveicon;
                    item->openIcon=nfsdriveicon;
                }
                else if(streq(fsdevices->find(pathname.text()),"floppy"))
                {
                    item->closedIcon=floppyicon;
                    item->openIcon=floppyicon;
                }
                else if(streq(fsdevices->find(pathname.text()),"cdrom"))
                {
                    item->closedIcon=cdromicon;
                    item->openIcon=cdromicon;
                }
                else if(streq(fsdevices->find(pathname.text()),"zip"))
                {
                    item->closedIcon=zipicon;
                    item->openIcon=zipicon;
                }
            }
#endif

            // Symbolic links have a specific icon
            if(islink)
            {
                type=_("Link to Folder");
				item->closedIcon=minilinkicon;
                item->openIcon=minilinkicon;
            }

			// Data used to update the tooltip
			item->tdata=item->label+"\t"+type+"\t"+mod+"\t"+usrid+"\t"+grpid+"\t"+atts+"\t"+del+"\t"+pathname;
			item->setData(&item->tdata);

            // Create item
            if(id())
                item->create();
        }

        // Close it
        closedir(dirp);
    }

    // Wipe items remaining in list:- they have disappeared!!
    for(item=oldlist; item; item=link)
    {
        link=item->link;
        removeItem(item,TRUE);
    }

    // Now we know for sure whether we really have subitems or not
    if(par->first)
        par->state|=DirItem::HASITEMS;
    else
        par->state&=~DirItem::HASITEMS;

    // Remember new list
    par->list=newlist;

    // Need to layout
    recalc();
}



// Is directory
FXbool DirList::isItemDirectory(const TreeItem* item) const
{
    if(item==NULL)
        fxerror("%s::isItemDirectory: item is NULL.\n",getClassName());
    return (item->state&DirItem::FOLDER)!=0;
}


// Is file
FXbool DirList::isItemFile(const TreeItem* item) const
{
    if(item==NULL)
        fxerror("%s::isItemFile: item is NULL.\n",getClassName());
    return (item->state&(DirItem::FOLDER|DirItem::CHARDEV|DirItem::BLOCKDEV|DirItem::FIFO|DirItem::SOCK))==0;
}


// Is executable
FXbool DirList::isItemExecutable(const TreeItem* item) const
{
    if(item==NULL)
        fxerror("%s::isItemExecutable: item is NULL.\n",getClassName());
    return (item->state&DirItem::EXECUTABLE)!=0;
}


// Return absolute pathname of item
FXString DirList::getItemPathname(const TreeItem* item) const
{
    FXString pathname;
    if(item)
    {
        while(1)
        {
            pathname.prepend(item->getText());
            item=(TreeItem*)item->parent;
            if(!item)
                break;
            if(item->parent)
                pathname.prepend(PATHSEP);
        }
    }
    return pathname;
}


// Return the item from the absolute pathname
TreeItem* DirList::getPathnameItem(const FXString& path)
{
    register TreeItem *item,*it;
    register int beg=0,end=0;
    FXString name;
    if(!path.empty())
    {
        if(ISPATHSEP(path[0]))
            end++;
        if(beg<end)
        {
            name=path.mid(beg,end-beg);
            for(it=(TreeItem*)firstitem; it; it=(TreeItem*)it->next)
            {
                if(compare(name,it->getText())==0)
                    goto x;
            }
            listRootItems();
            sortRootItems();
            for(it=(TreeItem*)firstitem; it; it=(TreeItem*)it->next)
            {
                if(compare(name,it->getText())==0)
                    goto x;
            }
            return NULL;
x:
            item=it;
            FXASSERT(item);
            while(end<path.length())
            {
                beg=end;
                while(end<path.length() && !ISPATHSEP(path[end]))
                    end++;
                name=path.mid(beg,end-beg);
                for(it=(TreeItem*)item->first; it; it=(TreeItem*)it->next)
                {
                    if(compare(name,it->getText())==0)
                        goto y;
                }
                listChildItems((DirItem*)item);
                sortChildItems(item);
                for(it=(TreeItem*)item->first; it; it=(TreeItem*)it->next)
                {
                    if(compare(name,it->getText())==0)
                        goto y;
                }
                return item;
y:
                item=it;
                FXASSERT(item);
                if(end<path.length() && ISPATHSEP(path[end]))
                    end++;
            }
            FXASSERT(item);
            return item;
        }
    }
    return NULL;
}


// Obtain item's file name only
FXString DirList::getItemFilename(const TreeItem* item) const
{
    if(item==NULL)
        fxerror("%s::getItemFilename: item is NULL.\n",getClassName());
    return item->label;
}


// Open all intermediate directories down toward given one
void DirList::setDirectory(const FXString& pathname,FXbool notify)
{
    if(!pathname.empty())
    {
        FXString path=FXPath::absolute(getItemPathname((TreeItem*)currentitem),pathname);
        	
		while(!FXPath::isTopDirectory(path) && !::isDirectory(path))
            path=FXPath::upLevel(path);

        TreeItem *item=getPathnameItem(path);
        if(id())
            layout();
		makeItemVisible(item);
		setCurrentItem(item,notify);
    }
}


// Return directory part of path to current item
FXString DirList::getDirectory() const
{
    const TreeItem* item=(TreeItem*)currentitem;
    while(item)
    {
        if(item->state&DirItem::FOLDER)
            return getItemPathname(item);
        item=(TreeItem*)item->parent;
    }
    return "";
}


// Set current (dir/file) name path
void DirList::setCurrentFile(const FXString& pathname,FXbool notify)
{
    if(!pathname.empty())
    {
        FXString path=FXPath::absolute(getItemPathname((TreeItem*)currentitem),pathname);
        while(!FXPath::isTopDirectory(path) && !::exists(path))
        {
            path=FXPath::upLevel(path);
        }
        TreeItem *item=getPathnameItem(path);
        if(id())
            layout();
		makeItemVisible(item);
        setCurrentItem(item,notify);
    }
}


// Get current (dir/file) name path
FXString DirList::getCurrentFile() const
{
    return getItemPathname((TreeItem*)currentitem);
}



// Get list style
FXbool DirList::showFiles() const
{
    return (options&DIRLIST_SHOWFILES)!=0;
}


// Change list style
void DirList::showFiles(FXbool showing)
{
    unsigned int opts=options;
    if(showing)
        opts|=DIRLIST_SHOWFILES;
    else
        opts&=~DIRLIST_SHOWFILES;
    if(options!=opts)
    {
        options=opts;
        scan(TRUE);
    }
}


// Return TRUE if showing hidden files
FXbool DirList::shownHiddenFiles() const
{
    return (options&DIRLIST_SHOWHIDDEN)!=0;
}


// Change show hidden files mode
void DirList::showHiddenFiles(FXbool showing)
{
    unsigned int opts=options;
    if(showing)
        opts|=DIRLIST_SHOWHIDDEN;
    else
        opts&=~DIRLIST_SHOWHIDDEN;
    if(opts!=options)
    {
        options=opts;
        scan(TRUE);
    }
}


// Set associations
void DirList::setAssociations(FileDict* assoc)
{
    associations=assoc;
    scan(TRUE);
}


// Set the pattern to filter
void DirList::setPattern(const FXString& ptrn)
{
    if(ptrn.empty())
        return;
    if(pattern!=ptrn)
    {
        pattern=ptrn;
        scan(TRUE);
    }
}


// Change file match mode
void DirList::setMatchMode(unsigned int mode)
{
    if(matchmode!=mode)
    {
        matchmode=mode;
        scan(TRUE);
    }
}


// Cleanup
DirList::~DirList()
{
    clearItems();
    getApp()->removeTimeout(this,ID_REFRESH_TIMER);
    getApp()->removeTimeout(this,ID_EXPAND_TIMER);
#if defined(linux)
    getApp()->removeTimeout(this,ID_MTDEVICES_REFRESH);
    getApp()->removeTimeout(this,ID_UPDEVICES_REFRESH);
#endif
    if(!(options&DIRLIST_NO_OWN_ASSOC))
        delete associations;
    associations=(FileDict*)-1;
}

// Update refresh timers if the window gains focus
long DirList::onUpdRefreshTimers(FXObject*,FXSelector,void*)
{
	static FXbool prevMinimized=TRUE;
	static FXbool minimized=TRUE;

	prevMinimized=minimized;
	if (((FXTopWindow*)focuswindow)->isMinimized())
		minimized=FALSE;
	else
		minimized=TRUE;

	// Update refresh timers
	if (prevMinimized == FALSE && minimized == TRUE)
	{
		onCmdRefreshTimer(0,0,0);
		/*		onMtdevicesRefresh(0,0,0);
				onUpdevicesRefresh(0,0,0); */
	}

    return 1;
}
