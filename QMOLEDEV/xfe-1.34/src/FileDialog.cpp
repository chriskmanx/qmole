// File dialog. Taken from the FOX library and slightly modified.

#include <stddef.h>
#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "FileList.h"
#include "InputDialog.h"
#include "DirHistBox.h"
#include "MessageBox.h"
#include "FileDialog.h"

#define FILELISTMASK (_ICONLIST_EXTENDEDSELECT|_ICONLIST_SINGLESELECT|_ICONLIST_BROWSESELECT|_ICONLIST_MULTIPLESELECT)
#define FILESTYLEMASK (_ICONLIST_DETAILED|_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS|_ICONLIST_ROWS|_ICONLIST_COLUMNS|_ICONLIST_AUTOSIZE)

// Single click navigation
extern unsigned int single_click;

// To allow keyboard scrolling in popup dialogs
extern FXbool allowPopupScroll;



// Map
FXDEFMAP(FileSelector) FileSelectorMap[]=
{
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_ACCEPT,FileSelector::onCmdAccept),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_FILEFILTER,FileSelector::onCmdFilter),
	FXMAPFUNC(SEL_DOUBLECLICKED,FileSelector::ID_FILELIST,FileSelector::onCmdItemDoubleClicked),
	FXMAPFUNC(SEL_CLICKED,FileSelector::ID_FILELIST,FileSelector::onCmdItemClicked),
	FXMAPFUNC(SEL_SELECTED,FileSelector::ID_FILELIST,FileSelector::onCmdItemSelected),
	FXMAPFUNC(SEL_DESELECTED,FileSelector::ID_FILELIST,FileSelector::onCmdItemDeselected),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_DIR_UP,FileSelector::onCmdDirUp),
	FXMAPFUNC(SEL_UPDATE,FileSelector::ID_DIR_UP,FileSelector::onUpdDirUp),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_DIR_BACK,FileSelector::onCmdDirBack),
	FXMAPFUNC(SEL_UPDATE,FileSelector::ID_DIR_BACK,FileSelector::onUpdDirBack),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_DIR_FORWARD,FileSelector::onCmdDirForward),
	FXMAPFUNC(SEL_UPDATE,FileSelector::ID_DIR_FORWARD,FileSelector::onUpdDirForward),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_DIR_BACK_HIST,FileSelector::onCmdDirBackHist),
	FXMAPFUNC(SEL_UPDATE,FileSelector::ID_DIR_BACK_HIST,FileSelector::onUpdDirBackHist),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_DIR_FORWARD_HIST,FileSelector::onCmdDirForwardHist),
	FXMAPFUNC(SEL_UPDATE,FileSelector::ID_DIR_FORWARD_HIST,FileSelector::onUpdDirForwardHist),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_HOME,FileSelector::onCmdHome),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_NEWDIR,FileSelector::onCmdNewDir),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_NEWFILE,FileSelector::onCmdNewFile),
	FXMAPFUNC(SEL_COMMAND,FileSelector::ID_WORK,FileSelector::onCmdWork),
	FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,FileSelector::ID_FILELIST,FileSelector::onCmdPopupMenu),
};


// Implementation
FXIMPLEMENT(FileSelector,FXPacker,FileSelectorMap,ARRAYNUMBER(FileSelectorMap))


// Default pattern
static const char allfiles[]="All Files (*)";

// File selector object
FileSelector::FileSelector(FXComposite *p,FXObject* tgt,FXSelector sel,unsigned int opts,int x,int y,int w,int h):
        FXPacker(p,opts,x,y,w,h,0,0,0,0,0,0)
{
	FXAccelTable *table=getShell()->getAccelTable();
    target=tgt;
    message=sel;
    
    // Global container
    FXVerticalFrame* cont=new FXVerticalFrame(this,LAYOUT_FILL_Y|LAYOUT_FILL_X|FRAME_NONE,0,0,0,0, 0,0,0,0, 0,0);

    // Container for the action buttons
    FXHorizontalFrame* buttons=new FXHorizontalFrame(cont,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED,0,0,0,0, 5,5,5,5, 0,0);
    
    // Container for the path linker
    FXHorizontalFrame* pathframe=new FXHorizontalFrame(cont,LAYOUT_FILL_X|FRAME_RAISED,0,0,0,0, 0,0,0,0, 0,0);

 	// File list
 	unsigned int options;
	FXbool smoothscroll=getApp()->reg().readUnsignedEntry("SETTINGS","smooth_scroll",TRUE);	
	if (smoothscroll)
		options=_ICONLIST_MINI_ICONS|_ICONLIST_BROWSESELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y;
	else
		options=_ICONLIST_MINI_ICONS|_ICONLIST_BROWSESELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y|SCROLLERS_DONT_TRACK;
	FXbool thumbnails=getApp()->reg().readUnsignedEntry("FILEDIALOG","thumbnails",FALSE);
    list=new FileList(this,cont,this,ID_FILELIST,thumbnails,options);
    
	// Set list colors and columns size for detailed mode
	list->setTextColor(getApp()->reg().readColorEntry("SETTINGS","listforecolor",FXRGB(0,0,0)));
	list->setBackColor(getApp()->reg().readColorEntry("SETTINGS","listbackcolor",FXRGB(255,255,255)));
	list->setHeaderSize(0,getApp()->reg().readUnsignedEntry("FILEDIALOG","name_size",200));
    list->setHeaderSize(1,getApp()->reg().readUnsignedEntry("FILEDIALOG","size_size",60));
    list->setHeaderSize(2,getApp()->reg().readUnsignedEntry("FILEDIALOG","type_size",100));
    list->setHeaderSize(3,getApp()->reg().readUnsignedEntry("FILEDIALOG","ext_size",100));
    list->setHeaderSize(4,getApp()->reg().readUnsignedEntry("FILEDIALOG","modd_size",150));
    list->setHeaderSize(5,getApp()->reg().readUnsignedEntry("FILEDIALOG","user_size",50));
    list->setHeaderSize(6,getApp()->reg().readUnsignedEntry("FILEDIALOG","grou_size",50));
    list->setHeaderSize(7,getApp()->reg().readUnsignedEntry("FILEDIALOG","attr_size",100));  	
	
	// Set file selector options
	unsigned int listmode=getApp()->reg().readUnsignedEntry("FILEDIALOG","listmode",_ICONLIST_MINI_ICONS);
	FXbool hiddenfiles=getApp()->reg().readUnsignedEntry("FILEDIALOG","hiddenfiles",FALSE);
	showHiddenFiles(hiddenfiles);
	setFileBoxStyle	(listmode);

    // Entry buttons
    FXMatrix* fields=new FXMatrix(cont,3,MATRIX_BY_COLUMNS|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
    new FXLabel(fields,_("&File Name:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y);
    filename=new FXTextField(fields,25,this,ID_ACCEPT,TEXTFIELD_ENTER_ONLY|LAYOUT_FILL_COLUMN|LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK);
    new FXButton(fields,_("&OK"),NULL,this,ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,0,0,0,0,20,20);
    accept=new FXButton(buttons,FXString::null,NULL,NULL,0,LAYOUT_FIX_X|LAYOUT_FIX_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,0,0, 0,0,0,0);
    new FXLabel(fields,_("File F&ilter:"),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y);
    FXHorizontalFrame* filterframe=new FXHorizontalFrame(fields,LAYOUT_FILL_COLUMN|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  	filefilter=new FXComboBox(filterframe,10,this,ID_FILEFILTER,COMBOBOX_STATIC|FRAME_SUNKEN|LAYOUT_FILL_X);
  	filefilter->setNumVisible(4);

    readonly=new FXCheckButton(filterframe,_("Read Only"),NULL,0,ICON_BEFORE_TEXT|JUSTIFY_LEFT|LAYOUT_CENTER_Y);
    cancel=new FXButton(fields,_("&Cancel"),NULL,NULL,0,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_X,0,0,0,0,20,20);

    // Action buttons
    FXString key;
	FXHotKey hotkey;
	FXButton *btn;
	FXToggleButton *tglbtn;
	
	new FXFrame(buttons,LAYOUT_FIX_WIDTH,0,0,4,1);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_back","Ctrl-Backspace");
    btn=new FXButton(buttons,TAB+_("Go to previous directory")+PARS(key),dirbackicon,this,ID_DIR_BACK,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	btnbackhist=new FXArrowButton(buttons,this,ID_DIR_BACK_HIST,LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_TOOLBAR);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_forward","Shift-Backspace");
    btn=new FXButton(buttons,TAB+_("Go to next directory")+PARS(key),dirforwardicon,this,ID_DIR_FORWARD,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	btnforwardhist=new FXArrowButton(buttons,this,ID_DIR_FORWARD_HIST,LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_TOOLBAR);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_up","Backspace");
    btn=new FXButton(buttons,TAB+_("Go to parent directory")+PARS(key),dirupicon,this,ID_DIR_UP,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_home","Ctrl-H");
    btn=new FXButton(buttons,TAB+_("Go to home directory")+PARS(key),homeicon,this,ID_HOME,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_work","Shift-F2");
    btn=new FXButton(buttons,TAB+_("Go to working directory")+PARS(key),workicon,this,ID_WORK,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_folder","F7");
    btn=new FXButton(buttons,TAB+_("New folder")+PARS(key),newfoldericon,this,ID_NEWDIR,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","big_icons","F10");
    btn=new FXButton(buttons,TAB+_("Big icon list")+PARS(key),bigiconsicon,list,FileList::ID_SHOW_BIG_ICONS,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","small_icons","F11");
    btn=new FXButton(buttons,TAB+_("Small icon list")+PARS(key),smalliconsicon,list,FileList::ID_SHOW_MINI_ICONS,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","detailed_file_list","F12");
    btn=new FXButton(buttons,TAB+_("Detailed file list")+PARS(key),detailsicon,list,FileList::ID_SHOW_DETAILS,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	btn->addHotKey(hotkey);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","hidden_files","Ctrl-F6");
    tglbtn=new FXToggleButton(buttons,TAB+_("Show hidden files")+PARS(key),TAB+_("Hide hidden files")+PARS(key),showhiddenicon,hidehiddenicon,list,FileList::ID_TOGGLE_HIDDEN,TOGGLEBUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	tglbtn->addHotKey(hotkey);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","thumbnails","Ctrl-F7");
    tglbtn=new FXToggleButton(buttons,TAB+_("Show thumbnails")+PARS(key),TAB+_("Hide thumbnails")+PARS(key),showthumbicon,hidethumbicon,list,FileList::ID_TOGGLE_THUMBNAILS,TOGGLEBUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	hotkey=_parseAccel(key);
	tglbtn->addHotKey(hotkey);

	// Path text
	pathtext=new TextLabel(pathframe,0,this,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);
	pathtext->setBackColor(getApp()->getBaseColor());

    // Path linker
    pathlink = new PathLinker(pathframe,list,NULL,LAYOUT_FILL_X);

    readonly->hide();
  	if(table)
	{
 		FXString key;
		FXHotKey hotkey;
		
  		key=getApp()->reg().readStringEntry("KEYBINDINGS","select_all","Ctrl-A");
		hotkey=_parseAccel(key);	
		table->addAccel(hotkey,list,FXSEL(SEL_COMMAND,FileList::ID_SELECT_ALL));
  	
		key=getApp()->reg().readStringEntry("KEYBINDINGS","deselect_all","Ctrl-Z");
		hotkey=_parseAccel(key);	
		table->addAccel(hotkey,list,FXSEL(SEL_COMMAND,FileList::ID_DESELECT_ALL));
	
  		key=getApp()->reg().readStringEntry("KEYBINDINGS","invert_selection","Ctrl-I");
		hotkey=_parseAccel(key);	
		table->addAccel(hotkey,list,FXSEL(SEL_COMMAND,FileList::ID_SELECT_INVERSE));
	
		key=getApp()->reg().readStringEntry("KEYBINDINGS","new_file","F2");
		hotkey=_parseAccel(key);	
		table->addAccel(hotkey,this,FXSEL(SEL_COMMAND,ID_NEWFILE));
    }

    setSelectMode(SELECT_FILE_ANY);                          // For backward compatibility, this HAS to be the default!
    setPatternList(allfiles);
    setDirectory(FXSystem::getCurrentDirectory());           // Update file list
   	pathlink->setPath(FXSystem::getCurrentDirectory());      // Update path linker
   	pathtext->setText(FXSystem::getCurrentDirectory());      // Update path text

    list->setFocus();
    accept->hide();

	// Change default cursor if single click navigation
	if (single_click==SINGLE_CLICK_DIR_FILE)
		list->setDefaultCursor(getApp()->getDefaultCursor(DEF_HAND_CURSOR));
}


// Create X window
void FileSelector::create()
{
	// Display or hide path linker
	FXbool show_pathlink=getApp()->reg().readUnsignedEntry("SETTINGS","show_pathlinker",TRUE);
	if (show_pathlink)
	{
		pathtext->hide();
		pathlink->show();
	}
	else
	{
		pathlink->hide();
		pathtext->show();
	}
	FXPacker::create();
	
}



// Double-clicked item in file list
long FileSelector::onCmdItemDoubleClicked(FXObject*,FXSelector,void* ptr)
{
    long index=(long)ptr;
    if(index<0)
        return 1;

    // If directory, open the directory
    if(list->isItemDirectory(index))
    {
		FXString pathname=list->getItemPathname(index);

		// Does not have access
		if(!::isReadExecutable(pathname))
		{
			MessageBox::error(this,BOX_OK,_("Error"),_(" Permission to: %s denied."), pathname.text());
			return 0;
		}
		
        setDirectory(pathname);
		pathlink->setPath(pathname);
		pathtext->setText(pathname);
        return 1;
    }

    // Only return if we wanted a file
    if((selectmode!=SELECT_FILE_DIRECTORY) && (selectmode!=SELECT_FILE_MIXED))
    {
        if(list->isItemFile(index))
        {
            FXObject *tgt=accept->getTarget();
            FXSelector sel=accept->getSelector();
            if(tgt)
                tgt->handle(accept,FXSEL(SEL_COMMAND,sel),(void*)1);
        }
    }
    return 1;
}


// Single clicked item in file list
long FileSelector::onCmdItemClicked(FXObject*,FXSelector,void* ptr)
{
	if (single_click != SINGLE_CLICK_NONE)
	{		
		long index=(long)ptr;
		if(index<0)
			return 1;

		// In detailed mode, avoid single click when cursor is not over the first column
		int x, y;
		unsigned int state;
		getCursorPosition(x,y,state);
		FXbool allow=TRUE;
		if (!(list->getListStyle()&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS)) && (x-list->getXPosition())>list->getHeaderSize(0))
			allow=FALSE;

		// Single click with control or shift
		if (state&(CONTROLMASK|SHIFTMASK))
			return 1;
		
		// Single click without control or shift
		else
		{
			// If directory, open the directory
			if(single_click != SINGLE_CLICK_NONE && list->isItemDirectory(index) && allow)
			{
				FXString pathname=list->getItemPathname(index);

				// Does not have access
				if(!::isReadExecutable(pathname))
				{
					MessageBox::error(this,BOX_OK,_("Error"),_(" Permission to: %s denied."), pathname.text());
					return 0;
				}				
				setDirectory(pathname);
				pathlink->setPath(pathname);
				pathtext->setText(pathname);
				return 1;
			}
			else if((single_click==SINGLE_CLICK_DIR_FILE) && list->isItemFile(index) && allow)
			{
				FXObject *tgt=accept->getTarget();
				FXSelector sel=accept->getSelector();
				if(tgt)
					tgt->handle(accept,FXSEL(SEL_COMMAND,sel),(void*)1);
			}
		}
	}
    return 1;
}

// Change in items which are selected
long FileSelector::onCmdItemSelected(FXObject*,FXSelector,void* ptr)
{
    long index=(long)ptr;
    FXString text,file;

    if(selectmode==SELECT_FILE_MULTIPLE)
    {
        for(int i=0; i<list->getNumItems(); i++)
        {
            if(list->isItemFile(i) && list->isItemSelected(i))
            {
                if(!text.empty())
                    text+=' ';
                text+=::quote(list->getItemFilename(i));
            }
        }
        filename->setText(text);
    }
    else if(selectmode==SELECT_FILE_MULTIPLE_ALL)
    {
        for(int i=0; i<list->getNumItems(); i++)
        {
            if(list->isItemSelected(i) && list->getItemFilename(i)!="..")
            {
                if(!text.empty())
                    text+=' ';
                text+=::quote(list->getItemFilename(i));
            }
        }
        filename->setText(text);
    }
    else if(selectmode==SELECT_FILE_DIRECTORY)
    {
        if(list->isItemDirectory(index))
        {
			if (list->getItemFilename(index)!="..")
			{
				text=list->getItemFilename(index);
				filename->setText(text);
			}
        }
    }
    // Mode added to select both directories and files
	else if(selectmode==SELECT_FILE_MIXED)
    {
		if (list->getItemFilename(index)!="..")
		{
			text=list->getItemFilename(index);
			filename->setText(text);
		}
    }
    else
    {
        if(list->isItemFile(index))
        {
            text=list->getItemFilename(index);
            filename->setText(text);
        }
    }
    return 1;
}


// Change in items which are deselected
long FileSelector::onCmdItemDeselected(FXObject*,FXSelector,void*)
{
    FXString text,file;
    if(selectmode==SELECT_FILE_MULTIPLE)
    {
        for(int i=0; i<list->getNumItems(); i++)
        {
            if(list->isItemFile(i) && list->isItemSelected(i))
            {
                if(!text.empty())
                    text+=' ';
                text+=::quote(list->getItemFilename(i));
            }
        }
        filename->setText(text);
    }
    else if(selectmode==SELECT_FILE_MULTIPLE_ALL)
    {
        for(int i=0; i<list->getNumItems(); i++)
        {
            if(list->isItemSelected(i) && list->getItemFilename(i)!="..")
            {
                if(!text.empty())
                    text+=' ';
                text+=::quote(list->getItemFilename(i));
            }
        }
        filename->setText(text);
    }
    return 1;
}


// Hit the accept button or enter in text field
long FileSelector::onCmdAccept(FXObject*,FXSelector,void*)
{
    FXSelector sel=accept->getSelector();
    FXObject *tgt=accept->getTarget();

    // Get (first) filename
    FXString path=getFilename();
	
	// If filename is empty, we get the current directory
	if(path.empty())
	{
		path=list->getDirectory();
        filename->setText(path);
	}
	
    // Only do something if a selection was made
    if(!path.empty())
    {
        // Is directory?
        if(::isDirectory(path))
        {
            // In directory mode:- we got our answer!
            if(selectmode==SELECT_FILE_DIRECTORY || selectmode==SELECT_FILE_MULTIPLE_ALL || selectmode==SELECT_FILE_MIXED)
            {
                if(tgt)
                   tgt->handle(accept,FXSEL(SEL_COMMAND,sel),(void*)1);
                return 1;
            }

            // Hop over to that directory
            list->setDirectory(path);
            pathlink->setPath(list->getDirectory());
            pathtext->setText(list->getDirectory());
            filename->setText(FXString::null);
            return 1;
        }

        // Get directory part of path
        FXString dir=FXPath::directory(path);

        // In file mode, directory part of path should exist
        if(::isDirectory(dir))
        {

            // In any mode, existing directory part is good enough
            if(selectmode==SELECT_FILE_ANY)
            {
                if(tgt)
                    tgt->handle(accept,FXSEL(SEL_COMMAND,sel),(void*)1);
                return 1;
            }

            // In existing mode, the whole filename must exist and be a file
            else if(selectmode==SELECT_FILE_EXISTING)
            {
                if(::isFile(path))
                {
                    if(tgt)
                        tgt->handle(accept,FXSEL(SEL_COMMAND,sel),(void*)1);
                    return 1;
                }
            }

            // In multiple mode, return if all selected files exist
            else if(selectmode==SELECT_FILE_MULTIPLE)
            {
                for(int i=0; i<list->getNumItems(); i++)
                {
                    if(list->isItemSelected(i) && list->isItemFile(i))
                    {
                        if(tgt)
                            tgt->handle(accept,FXSEL(SEL_COMMAND,sel),(void*)1);
                        return 1;
                    }
                }
            }

            // Multiple files and/or directories
            else
            {
                for(int i=0; i<list->getNumItems(); i++)
                {
                    if(list->isItemSelected(i) && list->getItemFilename(i)!="..")
                    {
                        if(tgt)
                            tgt->handle(accept,FXSEL(SEL_COMMAND,sel),(void*)1);
                        return 1;
                    }
                }
            }
        }

        // Go up to the lowest directory which still exists
        while(!FXPath::isTopDirectory(dir) && !::isDirectory(dir))
            dir=FXPath::upLevel(dir);

        // Switch as far as we could go
        list->setDirectory(dir);
        pathlink->setPath(list->getDirectory());
        pathtext->setText(list->getDirectory());

        // Put the tail end back for further editing
        if(ISPATHSEP(path[dir.length()]))
            path.erase(0,dir.length()+1);
        else
            path.erase(0,dir.length());

        // Replace text box with new stuff
        filename->setText(path);
        filename->selectAll();
    }

    return 1;
}


// User clicked up directory button
long FileSelector::onCmdDirUp(FXObject*,FXSelector,void*)
{
    setDirectory(FXPath::upLevel(list->getDirectory()));
   	pathlink->setPath(list->getDirectory());
   	pathtext->setText(list->getDirectory());
    return 1;
}


// Can we still go up
long FileSelector::onUpdDirUp(FXObject* sender,FXSelector,void*)
{
    if(FXPath::isTopDirectory(list->getDirectory()))
        sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    return 1;
}


// Directory back
long  FileSelector::onCmdDirBack(FXObject*,FXSelector s,void* p)
{
	StringList *backhist, *forwardhist;
	StringItem *item;
	FXString pathname;

	// Get the filelist history
	backhist=list->backhist;
	forwardhist=list->forwardhist;

	// Get the previous directory
	item=backhist->getFirst();
	if (item)
		pathname=backhist->getString(item);
	
	// Update the history
	backhist->removeFirstItem();
	forwardhist->insertFirstItem(list->getDirectory());
	
	// Go to to the previous directory
	list->setDirectory(pathname,FALSE);
   	pathlink->setPath(list->getDirectory());
   	pathtext->setText(list->getDirectory());

    return 1;
}


// Update directory back
long  FileSelector::onUpdDirBack(FXObject* sender, FXSelector sel, void* ptr)
{
	StringList *backhist;
	FXString pathname;

	// Get the filelist history
	backhist=list->backhist;
	
	// Gray out the button if no item in history 
	if (backhist->getNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);	
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory forward
long  FileSelector::onCmdDirForward(FXObject*,FXSelector s,void* p)
{
	StringList *backhist, *forwardhist;
	StringItem *item;
	FXString pathname;

	// Get the filelist history
	backhist=list->backhist;
	forwardhist=list->forwardhist;

	// Get the next directory
	item=forwardhist->getFirst();
	if (item)
		pathname=forwardhist->getString(item);
	
	// Update the history
	forwardhist->removeFirstItem();
	backhist->insertFirstItem(list->getDirectory());
	
	// Go to to the previous directory
	list->setDirectory(pathname,FALSE);
   	pathlink->setPath(list->getDirectory());
   	pathtext->setText(list->getDirectory());

    return 1;
}


// Update directory forward
long  FileSelector::onUpdDirForward(FXObject* sender, FXSelector sel, void* ptr)
{
	StringList *forwardhist;
	FXString pathname;

	// Get the filelist history
	forwardhist=list->forwardhist;
	
	// Gray out the button if no item in history 
	if (forwardhist->getNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);	
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory back history
long  FileSelector::onCmdDirBackHist(FXObject *sender,FXSelector sel,void* ptr)
{
	StringList *backhist, *forwardhist;
	StringItem *item;
	FXString pathname;

	// Get the filelist history
	backhist=list->backhist;
	forwardhist=list->forwardhist;

	// Get all string items and display them in a list box
	int num=backhist->getNumItems();
	if (num>0)
	{
		FXString *dirs= new FXString[num];
		FXString strlist="";
		
		// Get string items
		item=backhist->getFirst();
		for(int i=0; i<=num-1; i++)
		{
			if (item)
			{
				FXString str=backhist->getString(item);
				dirs[i]=str;
				strlist=strlist+str+"\n";
				item=backhist->getNext(item);
			}
		}

		// Display list box
		FXWindow* owner=this->getOwner();
		int pos=DirHistBox::box(btnbackhist,DECOR_NONE,strlist,owner->getX()+245,owner->getY()+37);
		
		// If an item was selected
		if (pos!=-1)
		{
		
			// Update back history
			if (pos==num-1)
				backhist->removeAllItems();
			else
			{
				item=backhist->getItemAtPos(pos+1);
				backhist->removeAllItemsBefore(item);
			}
			
			// Update forward history
			forwardhist->insertFirstItem(list->getDirectory());
			if (pos>0)
			{
				for (int i=0; i<=pos-1; i++)
					forwardhist->insertFirstItem(dirs[i]);
			}
	
			// Go to to the selected directory
			pathname=dirs[pos];
			list->setDirectory(pathname,FALSE);
   			pathlink->setPath(list->getDirectory());
   			pathtext->setText(list->getDirectory());
		}
		delete[]dirs;
	}

    return 1;
}


// Update directory back
long  FileSelector::onUpdDirBackHist(FXObject* sender, FXSelector sel, void* ptr)
{
	StringList *backhist;
	FXString pathname;

	// Get the filelist history
	backhist=list->backhist;
	
	// Gray out the button if no item in history 
	if (backhist->getNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);	
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory forward history
long  FileSelector::onCmdDirForwardHist(FXObject *sender,FXSelector sel,void* ptr)
{
	StringList *backhist, *forwardhist;
	StringItem *item;
	FXString pathname;

	// Get the filelist history
	backhist=list->backhist;
	forwardhist=list->forwardhist;

	// Get all string items and display them in a list box
	int num=forwardhist->getNumItems();
	if (num>0)
	{
		FXString *dirs= new FXString[num];
		FXString strlist="";
		
		// Get string items
		item=forwardhist->getFirst();
		for(int i=0; i<=num-1; i++)
		{
			if (item)
			{
				FXString str=forwardhist->getString(item);
				dirs[i]=str;
				strlist=strlist+str+"\n";
				item=forwardhist->getNext(item);
			}
		}
		
		// Display list box
		FXWindow* owner=this->getOwner();
		int pos=DirHistBox::box(btnforwardhist,DECOR_NONE,strlist,owner->getX()+285,owner->getY()+37);
		
		// If an item was selected
		if (pos!=-1)
		{
			// Update forward history
			if (pos==num-1)
				forwardhist->removeAllItems();
			else
			{
				item=forwardhist->getItemAtPos(pos+1);
				forwardhist->removeAllItemsBefore(item);
			}
			
			// Update back history
			backhist->insertFirstItem(list->getDirectory());
			if (pos>0)
			{
				for (int i=0; i<=pos-1; i++)
					backhist->insertFirstItem(dirs[i]);
			}
	
			// Go to to the selected directory
			pathname=dirs[pos];
			list->setDirectory(pathname,FALSE);
   			pathlink->setPath(list->getDirectory());
   			pathtext->setText(list->getDirectory());
		}
		delete[]dirs;
	}

    return 1;
}


// Update directory forward
long  FileSelector::onUpdDirForwardHist(FXObject* sender, FXSelector sel, void* ptr)
{
	StringList *forwardhist;
	FXString pathname;

	// Get the filelist history
	forwardhist=list->forwardhist;
	
	// Gray out the button if no item in history 
	if (forwardhist->getNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Back to home directory
long FileSelector::onCmdHome(FXObject*,FXSelector,void*)
{
    setDirectory(FXSystem::getHomeDirectory());
   	pathlink->setPath(list->getDirectory());
   	pathtext->setText(list->getDirectory());
    return 1;
}

// Create new directory
long FileSelector::onCmdNewDir(FXObject*,FXSelector,void*)
{
	// Focus on current list
	list->setFocus();

    FXString dirname="";	
    FXString dirpath=list->getDirectory();
    if(dirpath!=ROOTDIR)
        dirpath+=PATHSEPSTRING;

    InputDialog* dialog=new InputDialog(this,dirname,_("Create new folder..."),_("New Folder"));
    dialog->CursorEnd();
    if(dialog->execute(PLACEMENT_CURSOR))
    {
		dirname=dirpath+dialog->getText();
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
					MessageBox::error(this,BOX_OK_SU,_("Error"),"Can't create folder %s: %s",dirname.text(),strerror(errcode));
				else
					MessageBox::error(this,BOX_OK_SU,_("Error"),"Can't create folder %s",dirname.text());
    			return 0;
			}
		}							
    }
    delete dialog;
	
    return 1;
}


// Create new file
long FileSelector::onCmdNewFile(FXObject*,FXSelector,void*)
{
	FXString filename="";
	
	// Focus on current list
    list->setFocus();

    FXString pathname=list->getDirectory();
    if(pathname!=ROOTDIR)
        pathname+=PATHSEPSTRING;

    InputDialog* dialog=new InputDialog(this,filename,_("Create new file..."),_("New File"));
    dialog->CursorEnd();
    
	// Accept was pressed
    if(dialog->execute(PLACEMENT_CURSOR))
    {	
		filename=pathname+dialog->getText();
		FILE *file;
		if(filename!=pathname)
        {
			// Test some error conditions
			if (::exists(filename))
        	{
				MessageBox::error(this,BOX_OK,_("Error"),_("File or folder %s already exists"), filename.text());
        		return 0;
			}
			// Create the new file
			errno=0;
			if (!(file=fopen(filename.text(),"w+"))  ||  fclose(file) )
			{
				if (errno)
					MessageBox::error(this,BOX_OK_SU,_("Error"),"Can't create file %s: %s",filename.text(),strerror(errno));
				else
					MessageBox::error(this,BOX_OK_SU,_("Error"),"Can't create file %s",filename.text());
    			return 0;
			}
			// Change the file permissions according to the current umask
			int mask;
			mask=umask(0);
			umask(mask);
			errno=0;
			if (chmod(filename.text(), 438 & ~mask) != 0)
			{
				if (errno)
					MessageBox::error(this,BOX_OK_SU,_("Error"),"Can't set permissions in %s: %s",filename.text(),strerror(errno));
				else
					MessageBox::error(this,BOX_OK_SU,_("Error"),"Can't set permissions in %s",filename.text());
				return 0;
			}
		}							
    }
    delete dialog;
	
	
	return 1;
}




// Back to current working directory
long FileSelector::onCmdWork(FXObject*,FXSelector,void*)
{
    setDirectory(FXSystem::getCurrentDirectory());
   	pathlink->setPath(list->getDirectory());
	pathtext->setText(list->getDirectory());
    return 1;
}



// Strip pattern from text if present
FXString FileSelector::patternFromText(const FXString& pattern)
{
    int beg,end;
    end=pattern.rfind(')');         // Search from the end so we can allow ( ) in the pattern name itself
    beg=pattern.rfind('(',end-1);
    if(0<=beg && beg<end)
        return pattern.mid(beg+1,end-beg-1);
    return pattern;
}


// Return the first extension "ext1" found in the pattern if the
// pattern is of the form "*.ext1,*.ext2,..." or the empty string
// if the pattern contains other wildcard combinations.
FXString FileSelector::extensionFromPattern(const FXString& pattern)
{
    int beg,end,c;
    beg=0;
    if(pattern[beg]=='*')
    {
        beg++;
        if(pattern[beg]=='.')
        {
            beg++;
            end=beg;
            while((c=pattern[end])!='\0' && c!=',' && c!='|')
            {
                if(c=='*' || c=='?' || c=='[' || c==']' || c=='^' || c=='!')
                    return FXString::null;
                end++;
            }
            return pattern.mid(beg,end-beg);
        }
    }
    return FXString::null;
}


// Change the pattern; change the filename to the suggested extension
long FileSelector::onCmdFilter(FXObject*,FXSelector,void* ptr)
{
    FXString pat=patternFromText((char*)ptr);
    list->setPattern(pat);
    if(selectmode==SELECT_FILE_ANY)
    {
        FXString ext=extensionFromPattern(pat);
        if(!ext.empty())
        {
            FXString name=FXPath::stripExtension(filename->getText());
            if(!name.empty())
                filename->setText(name+"."+ext);
        }
    }
    return 1;
}


// Set directory
void FileSelector::setDirectory(const FXString& path)
{
    FXString abspath=FXPath::absolute(path);
    list->setDirectory(abspath);
   	pathlink->setPath(list->getDirectory());
   	pathtext->setText(list->getDirectory());

    if(selectmode!=SELECT_FILE_ANY)
        filename->setText(FXString::null);
}


// Get directory
FXString FileSelector::getDirectory() const
{
    return list->getDirectory();
}


// Set file name
void FileSelector::setFilename(const FXString& path)
{
    FXString abspath=FXPath::absolute(path);
    list->setCurrentFile(abspath);
   	pathlink->setPath(FXPath::directory(abspath));
   	pathtext->setText(FXPath::directory(abspath));
    filename->setText(FXPath::name(abspath));
}


// Get complete path + filename
FXString FileSelector::getFilename() const
{
    register int i;
    if(selectmode==SELECT_FILE_MULTIPLE_ALL)
    {
        for(i=0; i<list->getNumItems(); i++)
        {
            if(list->isItemSelected(i) && list->getItemFilename(i)!="..")
                return FXPath::absolute(list->getDirectory(),list->getItemFilename(i));
        }
    }
    else if(selectmode==SELECT_FILE_MULTIPLE)
    {
        for(i=0; i<list->getNumItems(); i++)
        {
            if(list->isItemSelected(i) && list->isItemFile(i))
                return FXPath::absolute(list->getDirectory(),list->getItemFilename(i));
        }
    }
    else
    {
        if(!filename->getText().empty())
            return FXPath::absolute(list->getDirectory(),filename->getText());
    }
    return FXString::null;
}


// Return empty-string terminated list of selected file names, or NULL
FXString* FileSelector::getFilenames() const
{
    register FXString *files=NULL;
    register int i,n;
    if(list->getNumItems())
    {
        if(selectmode==SELECT_FILE_MULTIPLE_ALL)
        {
            for(i=n=0; i<list->getNumItems(); i++)
            {
                if(list->isItemSelected(i) && list->getItemFilename(i)!="..")
                    n++;
            }
            if(n)
            {
                files=new FXString [n+1];
                for(i=n=0; i<list->getNumItems(); i++)
                {
                    if(list->isItemSelected(i) && list->getItemFilename(i)!="..")
                        files[n++]=list->getItemPathname(i);
                }
                files[n]=FXString::null;
            }
        }
        else
        {
            for(i=n=0; i<list->getNumItems(); i++)
            {
                if(list->isItemSelected(i) && list->isItemFile(i))
                    n++;
            }
            if(n)
            {
                files=new FXString [n+1];
                for(i=n=0; i<list->getNumItems(); i++)
                {
                    if(list->isItemSelected(i) && list->isItemFile(i))
                        files[n++]=list->getItemPathname(i);
                }
                files[n]=FXString::null;
            }
        }
    }
    return files;
}


// Set bunch of patterns
void FileSelector::setPatternList(const char **ptrns)
{
    filefilter->clearItems();
    if(ptrns)
    {
        while(ptrns[0] && ptrns[1])
        {
            filefilter->appendItem(FXStringFormat("%s (%s)",ptrns[0],ptrns[1]));
            ptrns+=2;
        }
    }
    if(!filefilter->getNumItems())
        filefilter->appendItem(allfiles);
    setCurrentPattern(0);
}


// Change patterns, each pattern separated by newline
void FileSelector::setPatternList(const FXString& patterns)
{
    FXString pat;
    int i;
    filefilter->clearItems();
    for(i=0; !(pat=patterns.section('\n',i)).empty(); i++)
        filefilter->appendItem(pat);
    if(!filefilter->getNumItems())
        filefilter->appendItem(allfiles);
    setCurrentPattern(0);
}


// Return list of patterns
FXString FileSelector::getPatternList() const
{
    FXString pat;
    int i;
    for(i=0; i<filefilter->getNumItems(); i++)
    {
        if(!pat.empty())
            pat+='\n';
        pat+=filefilter->getItemText(i);
    }
    return pat;
}


// Set current filter pattern
void FileSelector::setPattern(const FXString& ptrn)
{
    filefilter->setText(ptrn);
    list->setPattern(ptrn);
}


// Get current filter pattern
FXString FileSelector::getPattern() const
{
    return list->getPattern();
}


// Set current file pattern from the list
void FileSelector::setCurrentPattern(int patno)
{
	if ((unsigned int)patno >= (unsigned int)filefilter->getNumItems())
        fxerror("%s::setCurrentPattern: index out of range.\n",getClassName());
    filefilter->setCurrentItem(patno);
    list->setPattern(patternFromText(filefilter->getItemText(patno)));
}


// Return current pattern
int FileSelector::getCurrentPattern() const
{
    return filefilter->getCurrentItem();
}


// Change pattern for pattern number patno
void FileSelector::setPatternText(int patno,const FXString& text)
{
	if ((unsigned int)patno >= (unsigned int)filefilter->getNumItems())
        fxerror("%s::setPatternText: index out of range.\n",getClassName());
    filefilter->setItemText(patno,text);
    if(patno==filefilter->getCurrentItem())
        setPattern(patternFromText(text));
}


// Return pattern text of pattern patno
FXString FileSelector::getPatternText(int patno) const
{
	if ((unsigned int)patno >= (unsigned int)filefilter->getNumItems())
        fxerror("%s::getPatternText: index out of range.\n",getClassName());
    return filefilter->getItemText(patno);
}


// Change space for item
void FileSelector::setItemSpace(int s)
{
    list->setItemSpace(s);
}


// Get space for item
int FileSelector::getItemSpace() const
{
    return list->getItemSpace();
}


// Change File List style
void FileSelector::setFileBoxStyle(unsigned int style)
{
	list->setListStyle((list->getListStyle()&~FILESTYLEMASK) | (style&FILESTYLEMASK));
}


// Return File List style
unsigned int FileSelector::getFileBoxStyle() const
{
    return list->getListStyle()&FILESTYLEMASK;
}



// Change file selection mode
void FileSelector::setSelectMode(unsigned int mode)
{
    switch(mode)
    {
    case SELECT_FILE_EXISTING:
        list->showOnlyDirectories(FALSE);
        list->setListStyle((list->getListStyle()&~FILELISTMASK)|_ICONLIST_BROWSESELECT);
        break;
    case SELECT_FILE_MULTIPLE:
    case SELECT_FILE_MULTIPLE_ALL:
        list->showOnlyDirectories(FALSE);
        list->setListStyle((list->getListStyle()&~FILELISTMASK)|_ICONLIST_EXTENDEDSELECT);
        break;
    case SELECT_FILE_DIRECTORY:
        list->showOnlyDirectories(TRUE);
        list->setListStyle((list->getListStyle()&~FILELISTMASK)|_ICONLIST_BROWSESELECT);
        break;
    case SELECT_FILE_MIXED:
        list->setListStyle((list->getListStyle()&~FILELISTMASK)|_ICONLIST_BROWSESELECT);
        break;
    default:
        list->showOnlyDirectories(FALSE);
        list->setListStyle((list->getListStyle()&~FILELISTMASK)|_ICONLIST_BROWSESELECT);
        break;
    }
    selectmode=mode;
}


// Show readonly button
void FileSelector::showReadOnly(FXbool show)
{
    show ? readonly->show() : readonly->hide();
}


// Return TRUE if readonly is shown
FXbool FileSelector::shownReadOnly() const
{
    return readonly->shown();
}



// Set initial state of readonly button
void FileSelector::setReadOnly(FXbool state)
{
    readonly->setCheck(state);
}


// Get readonly state
FXbool FileSelector::getReadOnly() const
{
    return readonly->getCheck();
}


// Return TRUE if hidden files are displayed
FXbool FileSelector::shownHiddenFiles() const
{
	return list->shownHiddenFiles();
}


// Return TRUE if thumbnails are displayed
FXbool FileSelector::shownThumbnails() const
{
	return list->shownThumbnails();
}


// Change show hidden files mode
void FileSelector::showHiddenFiles(FXbool shown)
{
	list->showHiddenFiles(shown);
}


// Change show thumbnails files mode
void FileSelector::showThumbnails(FXbool shown)
{
	list->showThumbnails(shown);
}


// Cleanup; icons must be explicitly deleted
FileSelector::~FileSelector()
{	
	// Write options to the registry
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","name_size",list->getHeaderSize(0));
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","size_size",list->getHeaderSize(1));
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","type_size",list->getHeaderSize(2));
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","ext_size",list->getHeaderSize(3));
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","modd_size",list->getHeaderSize(4));
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","user_size",list->getHeaderSize(5));
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","grou_size",list->getHeaderSize(6));
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","attr_size",list->getHeaderSize(7));
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","listmode",getFileBoxStyle());
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","hiddenfiles",shownHiddenFiles());
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","thumbnails",shownThumbnails());
	getApp()->reg().write();
	
	FXAccelTable *table=getShell()->getAccelTable();
	if(table)
	{
    	table->removeAccel(MKUINT(KEY_BackSpace,0));
    	table->removeAccel(MKUINT(KEY_h,CONTROLMASK));
    	table->removeAccel(MKUINT(KEY_w,CONTROLMASK));
    	table->removeAccel(MKUINT(KEY_a,CONTROLMASK));
    	table->removeAccel(MKUINT(KEY_i,CONTROLMASK));
    }
    delete list;
    delete pathlink;
	delete pathtext;
	delete filename;
	delete filefilter;
	delete readonly;
	delete accept;
	delete cancel;
	delete btnbackhist;
	delete btnforwardhist;
}


// File selector context menu
long FileSelector::onCmdPopupMenu(FXObject* o,FXSelector s,void* p)
{
    // Popup menu pane
	FXMenuPane menu(this);
    int x,y;
    unsigned int state;
    getRoot()->getCursorPosition(x,y,state);

	new FXMenuCommand(&menu,_("Go ho&me"),homeicon,this,ID_HOME);
	new FXMenuCommand(&menu,_("Go &work"),workicon,this,ID_WORK);
	new FXMenuCommand(&menu,_("New &file..."),newfileicon,this,ID_NEWFILE);
	new FXMenuCommand(&menu,_("New f&older..."),newfoldericon,this,ID_NEWDIR);
	new FXMenuSeparator(&menu);
	new FXMenuCheck(&menu,_("&Hidden files"),list,FileList::ID_TOGGLE_HIDDEN);
	new FXMenuCheck(&menu,_("Thum&bnails"),list,FileList::ID_TOGGLE_THUMBNAILS);
	new FXMenuSeparator(&menu);
	new FXMenuRadio(&menu,_("B&ig icons"),list,IconList::ID_SHOW_BIG_ICONS);
	new FXMenuRadio(&menu,_("&Small icons"),list,IconList::ID_SHOW_MINI_ICONS);
	new FXMenuRadio(&menu,_("Fu&ll file list"),list,IconList::ID_SHOW_DETAILS);
	new FXMenuSeparator(&menu);
	new FXMenuRadio(&menu,_("&Rows"),list,FileList::ID_ARRANGE_BY_ROWS);
	new FXMenuRadio(&menu,_("&Columns"),list,FileList::ID_ARRANGE_BY_COLUMNS);
	new FXMenuCheck(&menu,_("Autosize"),list,FileList::ID_AUTOSIZE);
	new FXMenuSeparator(&menu);
	new FXMenuRadio(&menu,_("&Name"),list,FileList::ID_SORT_BY_NAME);
	new FXMenuRadio(&menu,_("Si&ze"),list,FileList::ID_SORT_BY_SIZE);
	new FXMenuRadio(&menu,_("&Type"),list,FileList::ID_SORT_BY_TYPE);
	new FXMenuRadio(&menu,_("E&xtension"),list,FileList::ID_SORT_BY_EXT);
	new FXMenuRadio(&menu,_("&Date"),list,FileList::ID_SORT_BY_TIME);
	new FXMenuRadio(&menu,_("&User"),list,FileList::ID_SORT_BY_USER);
	new FXMenuRadio(&menu,_("&Group"),list,FileList::ID_SORT_BY_GROUP);
	new FXMenuRadio(&menu,_("&Permissions"),list,FileList::ID_SORT_BY_PERM);
	new FXMenuSeparator(&menu);
	new FXMenuCheck(&menu,_("Ignore c&ase"),list,FileList::ID_SORT_CASE);
	new FXMenuCheck(&menu,_("Dir&ectories first"),list,FileList::ID_DIRS_FIRST);
	new FXMenuCheck(&menu,_("Re&verse order"),list,FileList::ID_SORT_REVERSE);

	menu.create();
	allowPopupScroll=TRUE;  // Allow keyboard scrolling
	menu.popup(NULL,x,y);
	getApp()->runModalWhileShown(&menu);
	allowPopupScroll=FALSE;
	return 1;
}



// Object implementation
FXIMPLEMENT(FileDialog,DialogBox,NULL,0)


// File Dialog object
FileDialog::FileDialog(FXWindow* owner,const FXString& name,unsigned int opts,int x,int y,int w,int h):
        DialogBox(owner,name,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,x,y,w,h,0,0,0,0,4,4)
{
    list=new FileSelector(this,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);
	list->acceptButton()->setTarget(this);
    list->acceptButton()->setSelector(DialogBox::ID_ACCEPT);
    list->cancelButton()->setTarget(this);
    list->cancelButton()->setSelector(DialogBox::ID_CANCEL);
    
    // Set file dialog options 
	unsigned int width=getApp()->reg().readUnsignedEntry("FILEDIALOG","width",640);
	unsigned int height=getApp()->reg().readUnsignedEntry("FILEDIALOG","height",480);
	setWidth(width);
	setHeight(height);
}


// Set file name
void FileDialog::setFilename(const FXString& path)
{
    list->setFilename(path);
}


// Get filename, if any
FXString FileDialog::getFilename() const
{
    return list->getFilename();
}


// Return empty-string terminated list of selected file names,
FXString* FileDialog::getFilenames() const
{
    return list->getFilenames();
}


// Set pattern
void FileDialog::setPattern(const FXString& ptrn)
{
    list->setPattern(ptrn);
}


// Get pattern
FXString FileDialog::getPattern() const
{
    return list->getPattern();
}


// Change patterns, each pattern separated by newline
void FileDialog::setPatternList(const FXString& patterns)
{
    list->setPatternList(patterns);
}


// Return list of patterns
FXString FileDialog::getPatternList() const
{
    return list->getPatternList();
}


// Set directory
void FileDialog::setDirectory(const FXString& path)
{
    list->setDirectory(path);
}


// Get directory
FXString FileDialog::getDirectory() const
{
    return list->getDirectory();
}


// Set current file pattern from the list
void FileDialog::setCurrentPattern(int n)
{
    list->setCurrentPattern(n);
}


// Return current pattern
int FileDialog::getCurrentPattern() const
{
    return list->getCurrentPattern();
}

FXString FileDialog::getPatternText(int patno) const
{
    return list->getPatternText(patno);
}


void FileDialog::setPatternText(int patno,const FXString& text)
{
    list->setPatternText(patno,text);
}


// Set list of patterns (DEPRECATED)
void FileDialog::setPatternList(const char **ptrns)
{
    list->setPatternList(ptrns);
}


// Change space for item
void FileDialog::setItemSpace(int s)
{
    list->setItemSpace(s);
}


// Get space for item
int FileDialog::getItemSpace() const
{
    return list->getItemSpace();
}


// Change File List style
void FileDialog::setFileBoxStyle(unsigned int style)
{
    list->setFileBoxStyle(style);
}


// Return File List style
unsigned int FileDialog::getFileBoxStyle() const
{
    return list->getFileBoxStyle();
}


// Change file selection mode
void FileDialog::setSelectMode(unsigned int mode)
{
    list->setSelectMode(mode);
}


// Return file selection mode
unsigned int FileDialog::getSelectMode() const
{
    return list->getSelectMode();
}


// Show readonly button
void FileDialog::showReadOnly(FXbool show)
{
    list->showReadOnly(show);
}


// Return TRUE if readonly is shown
FXbool FileDialog::shownReadOnly() const
{
    return list->shownReadOnly();
}



// Set initial state of readonly button
void FileDialog::setReadOnly(FXbool state)
{
    list->setReadOnly(state);
}


// Get readonly state
FXbool FileDialog::getReadOnly() const
{
    return list->getReadOnly();
}


// Return TRUE if thumbnails are displayed
FXbool FileDialog::shownThumbnails() const
{
	return list->shownThumbnails();
}


// Return TRUE if hidden files are displayed
FXbool FileDialog::shownHiddenFiles() const
{
	return list->shownHiddenFiles();
}


// Change show hidden files mode
void FileDialog::showHiddenFiles(FXbool shown)
{
	list->showHiddenFiles(shown);
}


// Change show thumbnails files mode
void FileDialog::showThumbnails(FXbool shown)
{
	list->showThumbnails(shown);
}


// Cleanup
FileDialog::~FileDialog()
{
	// Write options to the registry
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","width",getWidth());
	getApp()->reg().writeUnsignedEntry("FILEDIALOG","height",getHeight());
	getApp()->reg().write();

    delete list;
}


// Open existing filename
FXString FileDialog::getOpenFilename(FXWindow* owner,const FXString& caption,const FXString& path,const FXString& patterns,int initial)
{
    FileDialog opendialog(owner,caption);
    FXString filename;
    opendialog.setSelectMode(SELECT_FILE_EXISTING);
    opendialog.setPatternList(patterns);
    opendialog.setCurrentPattern(initial);
    opendialog.setFilename(path);
    if(opendialog.execute())
    {
        filename=opendialog.getFilename();
        if(::isFile(filename))
            return filename;
    }
    return FXString::null;
}


// Save to filename
FXString FileDialog::getSaveFilename(FXWindow* owner,const FXString& caption,const FXString& path,const FXString& patterns,int initial)
{
    FileDialog savedialog(owner,caption);
    savedialog.setSelectMode(SELECT_FILE_ANY);
    savedialog.setPatternList(patterns);
    savedialog.setCurrentPattern(initial);
    savedialog.setFilename(path);
    if(savedialog.execute())
        return savedialog.getFilename();
    return FXString::null;
}


// Open multiple existing files
FXString* FileDialog::getOpenFilenames(FXWindow* owner,const FXString& caption,const FXString& path,const FXString& patterns,int initial)
{
    FileDialog opendialog(owner,caption);
    opendialog.setSelectMode(SELECT_FILE_MULTIPLE);
    opendialog.setPatternList(patterns);
    opendialog.setCurrentPattern(initial);
    opendialog.setFilename(path);
    if(opendialog.execute())
        return opendialog.getFilenames();
    return NULL;
}


// Open existing directory name
FXString FileDialog::getOpenDirectory(FXWindow* owner,const FXString& caption,const FXString& path)
{
    FileDialog dirdialog(owner,caption);
    FXString dirname;
    dirdialog.setSelectMode(SELECT_FILE_DIRECTORY);
    dirdialog.setFilename(path);
    if(dirdialog.execute())
    {
        dirname=dirdialog.getFilename();
        if(::isDirectory(dirname))
            return dirname;
    }
    return FXString::null;
}


