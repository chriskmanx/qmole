#ifndef DIRLIST_H
#define DIRLIST_H

struct FileAssoc;
class  FileDict;
class  DirList;

// Tree item
class FXAPI TreeItem : public FXTreeItem
{
    FXDECLARE(TreeItem)
    friend class DirList;
protected:
    TreeItem()
    {}
public:
    // Constructor
    TreeItem(const FXString& text,FXIcon* oi=NULL,FXIcon* ci=NULL,void* ptr=NULL):FXTreeItem(text,oi,ci,ptr)
    {}
};

// Directory item
class FXAPI DirItem : public FXTreeItem
{
    FXDECLARE(DirItem)
    friend class DirList;
protected:
    FileAssoc  *assoc;              // File association
    DirItem    *link;               // Link to next item
    DirItem    *list;               // List of child items
    FXulong      size;               // File size (if a file)
    FXTime      date;               // Time of item
    FXString	tdata;				// Tooltip data
protected:
    DirItem():assoc(NULL),link(NULL),list(NULL),size(0L),date(0)
    {}
protected:
    enum {
        FOLDER      = 512,                // Directory item
        EXECUTABLE  = 1024,               // Executable item
        SYMLINK     = 2048,               // Symbolic linked item
        CHARDEV     = 4096,               // Character special item
        BLOCKDEV    = 8192,               // Block special item
        FIFO        = 16384,              // FIFO item
        SOCK        = 32768               // Socket item
    };
public:
    // Constructor
    DirItem(const FXString& text,FXIcon* oi=NULL,FXIcon* ci=NULL,void* ptr=NULL):FXTreeItem(text,oi,ci,ptr),assoc(NULL),link(NULL),list(NULL),size(0),date(0)
    {
        state=HASITEMS;
        tdata="";
    }
    FXbool isDirectory() const
    {
        return (state&FOLDER)!=0;
    }
    FXbool isExecutable() const
    {
        return (state&EXECUTABLE)!=0;
    }
    FXbool isSymlink() const
    {
        return (state&SYMLINK)!=0;
    }
    FXbool isChardev() const
    {
        return (state&CHARDEV)!=0;
    }
    FXbool isBlockdev() const
    {
        return (state&BLOCKDEV)!=0;
    }
    FXbool isFifo() const
    {
        return (state&FIFO)!=0;
    }
    FXbool isSocket() const
    {
        return (state&SOCK)!=0;
    }
    FileAssoc* getAssoc() const
    {
        return assoc;
    }
    FXulong getSize() const
    {
        return size;
    }
    FXTime getDate() const
    {
        return date;
    }
    FXString getTooltipData() const
    {
		if (getData() != NULL)
			return tdata;
		else
			return "";
	}
};


// Directory tree List
class FXAPI DirList : public FXTreeList
{
    FXDECLARE(DirList)
protected:
    TreeItem 		*prevSelItem;
    DirItem         *list;                  // Root item list
    FileDict		*associations;			// Association table
    FXString		dropdirectory;			// Drop directory
    FXDragAction	dropaction;				// Drop action
    FXString		dragfiles;				// Dragged files
    FXString		pattern;				// Pattern of file names
    unsigned int			matchmode;				// File wildcard match mode
    unsigned int			counter;				// Refresh counter
	FXString		trashfileslocation;     // Location of the trash files directory
	FXString		trashinfolocation;      // Location of the trash info directory
	FXWindow		*focuswindow;			// Window used to test focus
protected:
    DirList()
    {}
    virtual TreeItem* createItem(const FXString& text,FXIcon* oi,FXIcon* ci,void* ptr);
    TreeItem* getitem(char* pathname);
    void listRootItems();
    void listChildItems(DirItem *par);
private:
    DirList(const DirList&);
    DirList &operator=(const DirList&);
public:
    long onCmdRefresh(FXObject*,FXSelector,void*);
    long onCmdRefreshTimer(FXObject*,FXSelector,void*);
#if defined(linux)
    long onMtdevicesRefresh(FXObject*,FXSelector,void*);
    long onUpdevicesRefresh(FXObject*,FXSelector,void*);
#endif
    long onExpandTimer(FXObject*,FXSelector,void*);
    long onBeginDrag(FXObject*,FXSelector,void*);
    long onEndDrag(FXObject*,FXSelector,void*);
    long onDragged(FXObject*,FXSelector,void*);
    long onDNDEnter(FXObject*,FXSelector,void*);
    long onDNDLeave(FXObject*,FXSelector,void*);
    long onDNDMotion(FXObject*,FXSelector,void*);
    long onDNDDrop(FXObject*,FXSelector,void*);
    long onDNDRequest(FXObject*,FXSelector,void*);
    long onOpened(FXObject*,FXSelector,void*);
    long onClosed(FXObject*,FXSelector,void*);
    long onExpanded(FXObject*,FXSelector,void*);
    long onCollapsed(FXObject*,FXSelector,void*);
    long onCmdToggleHidden(FXObject*,FXSelector,void*);
    long onUpdToggleHidden(FXObject*,FXSelector,void*);
    long onCmdShowHidden(FXObject*,FXSelector,void*);
    long onUpdShowHidden(FXObject*,FXSelector,void*);
    long onCmdHideHidden(FXObject*,FXSelector,void*);
    long onUpdHideHidden(FXObject*,FXSelector,void*);
    long onCmdToggleFiles(FXObject*,FXSelector,void*);
    long onUpdToggleFiles(FXObject*,FXSelector,void*);
    long onCmdShowFiles(FXObject*,FXSelector,void*);
    long onUpdShowFiles(FXObject*,FXSelector,void*);
    long onCmdHideFiles(FXObject*,FXSelector,void*);
    long onUpdHideFiles(FXObject*,FXSelector,void*);
    long onCmdSetPattern(FXObject*,FXSelector,void*);
    long onUpdSetPattern(FXObject*,FXSelector,void*);
    long onCmdSortReverse(FXObject*,FXSelector,void*);
    long onUpdSortReverse(FXObject*,FXSelector,void*);
    long onCmdSortCase(FXObject*,FXSelector,void*);
    long onUpdSortCase(FXObject*,FXSelector,void*);
	long onCmdDragCopy(FXObject* sender,FXSelector,void*);
	long onCmdDragMove(FXObject* sender,FXSelector,void*);
	long onCmdDragLink(FXObject* sender,FXSelector,void*);
	long onCmdDragReject(FXObject* sender,FXSelector,void*);
	long onUpdRefreshTimers(FXObject*,FXSelector,void*);
public:
    static int ascending(const FXTreeItem* a,const FXTreeItem* b);
    static int descending(const FXTreeItem* a,const FXTreeItem* b);
    static int ascendingCase(const FXTreeItem* a,const FXTreeItem* b);
    static int descendingCase(const FXTreeItem* a,const FXTreeItem* b);
public:
    enum {
        ID_REFRESH_TIMER=FXTreeList::ID_LAST,
        ID_SHOW_FILES,
        ID_HIDE_FILES,
        ID_TOGGLE_FILES,
        ID_SHOW_HIDDEN,
        ID_HIDE_HIDDEN,
        ID_TOGGLE_HIDDEN,
        ID_SET_PATTERN,
        ID_SORT_REVERSE,
        ID_SORT_CASE,
        ID_EXPAND_TIMER,
#if defined(linux)
        ID_UPDEVICES_REFRESH,
        ID_MTDEVICES_REFRESH,
#endif
		ID_DRAG_COPY,
		ID_DRAG_MOVE,
		ID_DRAG_LINK,
		ID_DRAG_REJECT,
		ID_REFRESH,
        ID_LAST
    };
public:

    // Construct a directory list
    DirList(FXWindow *focuswin,FXComposite *p,FXObject* tgt=NULL,FXSelector sel=0,unsigned int opts=0,int x=0,int y=0,int w=0,int h=0);

    // Create server-side resources
    virtual void create();

    // Scan the directories and update the items if needed, or if force is TRUE
    void scan(FXbool force=TRUE);

    // Return TRUE if item is a directory
    FXbool isItemDirectory(const TreeItem* item) const;

    // Return TRUE if item is a file
    FXbool isItemFile(const TreeItem* item) const;

    // Return TRUE if item is executable
    FXbool isItemExecutable(const TreeItem* item) const;

    // Collapse tree
    virtual FXbool collapseTree(TreeItem* tree,FXbool notify=FALSE);

    // Expand tree
    virtual FXbool expandTree(TreeItem* tree,FXbool notify=FALSE);

    // Set current file
    void setCurrentFile(const FXString& file,FXbool notify=FALSE);

    // Return current file
    FXString getCurrentFile() const;

    // Set current directory
    void setDirectory(const FXString& pathname,FXbool notify);

    // Return current directory
    FXString getDirectory() const;

    // Return name of item
    FXString getItemFilename(const TreeItem* item) const;

    // Return absolute pathname of item
    FXString getItemPathname(const TreeItem* item) const;

    // Return the item from the absolute pathname
    TreeItem* getPathnameItem(const FXString& path);

    // Change wildcard matching pattern
    void setPattern(const FXString& ptrn);

    // Return wildcard pattern
    FXString getPattern() const
    {
        return pattern;
    }

    // Return wildcard matching mode
    unsigned int getMatchMode() const
    {
        return matchmode;
    }

    // Change wildcard matching mode
    void setMatchMode(unsigned int mode);

    // Return TRUE if showing files as well as directories
    FXbool showFiles() const;

    // Show or hide normal files
    void showFiles(FXbool showing);

    // Return TRUE if showing hidden files and directories
    FXbool shownHiddenFiles() const;

    // Show or hide hidden files and directories
    void showHiddenFiles(FXbool showing);

    // Change file associations
    void setAssociations(FileDict* assoc);

    // Return file associations
    FileDict* getAssociations() const
    {
        return associations;
    }

    // Destructor
    virtual ~DirList();
};


#endif
