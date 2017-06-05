#ifndef _ICONLIST_H
#define _ICONLIST_H

#ifndef FXSCROLLAREA_H
#include "FXScrollArea.h"
#endif


class IconList;
class FileList;


// Icon List options (prefixed with an underscore to avoid conflict with the FOX library)
enum {
  _ICONLIST_EXTENDEDSELECT = 0,                // Extended selection mode
  _ICONLIST_SINGLESELECT   = 0x00100000,       // At most one selected item
  _ICONLIST_BROWSESELECT   = 0x00200000,       // Always exactly one selected item
  _ICONLIST_MULTIPLESELECT = 0x00300000,       // Multiple selection mode
  _ICONLIST_AUTOSIZE       = 0x00400000,       // Automatically size item spacing
  _ICONLIST_DETAILED       = 0,                // List mode
  _ICONLIST_MINI_ICONS     = 0x00800000,       // Mini Icon mode
  _ICONLIST_BIG_ICONS      = 0x01000000,       // Big Icon mode
  _ICONLIST_ROWS           = 0,                // Row-wise mode
  _ICONLIST_COLUMNS        = 0x02000000,       // Column-wise mode
  _ICONLIST_SEARCH	       = 0x10000000,       // Icon list is a search list (must be the same value as in FileList)
  _ICONLIST_STANDARD       = 0x20000000,       // Icon list is a not a file list and not a search list
  _ICONLIST_NORMAL         = _ICONLIST_EXTENDEDSELECT
  };




// Icon item
class FXAPI IconItem : public FXObject
{
    FXDECLARE(IconItem)
    friend class IconList;
protected:
    FXString  label;      // Text of item
    FXIcon   *bigIcon;    // Icon of item
    FXIcon   *miniIcon;   // Icon of item
    void     *data;       // Item user data pointer
    unsigned int    state;      // Item state flags
private:
    IconItem(const IconItem&);
    IconItem& operator=(const IconItem&);
protected:
    IconItem():bigIcon(NULL),miniIcon(NULL),data(NULL),state(0){}
    virtual void draw(IconList* list,FXDC& dc,int x,int y,int w,int h) const;
    virtual int hitItem(const IconList* list,int rx,int ry,int rw=1,int rh=1) const;
protected:
    virtual void drawBigIcon(const IconList* list,FXDC& dc,int x,int y,int w,int h) const;
    virtual void drawMiniIcon(const IconList* list,FXDC& dc,int x,int y,int w,int h) const;
	FXbool isOdd(int i) const;
    virtual void drawDetails(IconList* list,FXDC& dc,int x,int y,int w,int h) const;
public:
    enum
    {
        SELECTED      = 1,  // Selected
        FOCUS         = 2,  // Focus
        DISABLED      = 4,  // Disabled
        DRAGGABLE     = 8,  // Draggable
        BIGICONOWNED  = 16, // Big icon owned by item
        MINIICONOWNED = 32  // Mini icon owned by item
    };
public:

    // Construct new item with given text, icons, and user-data
    IconItem(const FXString& text,FXIcon* bi=NULL,FXIcon* mi=NULL,void* ptr=NULL):label(text),bigIcon(bi),miniIcon(mi),data(ptr),state(0){}

    // Change item's text label
    virtual void setText(const FXString& txt);

    // Return item's text label
    const FXString& getText() const
    {
        return label;
    }

    // Change item's big icon, deleting the old icon if it was owned
    virtual void setBigIcon(FXIcon* icn,FXbool owned=FALSE);

    // Return item's big icon
    FXIcon* getBigIcon() const
    {
        return bigIcon;
    }

    // Change item's mini icon, deleting the old icon if it was owned
    virtual void setMiniIcon(FXIcon* icn,FXbool owned=FALSE);

    // Return item's mini icon
    FXIcon* getMiniIcon() const
    {
        return miniIcon;
    }

    // Change item's user data
    void setData(void* ptr)
    {
        data=ptr;
    }

    // Get item's user data
    void* getData() const
    {
        return data;
    }

    // Make item draw as focused
    virtual void setFocus(FXbool focus);

    // Return true if item has focus
    FXbool hasFocus() const
    {
        return (state&FOCUS)!=0;
    }

    // Select item
    virtual void setSelected(FXbool selected);

    // Return true if this item is selected
    FXbool isSelected() const
    {
        return (state&SELECTED)!=0;
    }

    // Enable or disable item
    virtual void setEnabled(FXbool enabled);

    // Return true if this item is enabled
    FXbool isEnabled() const
    {
        return (state&DISABLED)==0;
    }

    // Make item draggable
    virtual void setDraggable(FXbool draggable);

    // Return true if this item is draggable
    FXbool isDraggable() const
    {
        return (state&DRAGGABLE)!=0;
    }

    // Return width of item as drawn in list
    virtual int getWidth(const IconList* list) const;

    // Return height of item as drawn in list
    virtual int getHeight(const IconList* list) const;

    // Create server-side resources
    virtual void create();

    // Detach server-side resources
    virtual void detach();

    // Destroy server-side resources
    virtual void destroy();

    // Save to stream
    virtual void save(FXStream& store) const;

    // Load from stream
    virtual void load(FXStream& store);

    // Destroy item and free icons if owned
    virtual ~IconItem();
};


// Icon item collate function
typedef int (*IconListSortFunc)(const IconItem*,const IconItem*);


// List of IconItem's
typedef FXObjectListOf<IconItem> IconItemList;


// A Icon List Widget displays a list of items, each with a text and
// optional icon.  Icon List can display its items in essentially three
// different ways; in big-icon mode, the bigger of the two icons is used
// for each item, and the text is placed underneath the icon. In mini-
// icon mode, the icons are listed in rows and columns, with the smaller
// icon preceding the text.  Finally, in detail mode the icons are listed
// in a single column, and all fields of the text are shown under a
// header control with one button for each subfield.
// When an item's selected state changes, the icon list sends
// a SEL_SELECTED or SEL_DESELECTED message.  A change of the current
// item is signified by the SEL_CHANGED message.
// The icon list sends SEL_COMMAND messages when the user clicks on an item,
// and SEL_CLICKED, SEL_DOUBLECLICKED, and SEL_TRIPLECLICKED when the user
// clicks once, twice, or thrice, respectively.
// When items are added, replaced, or removed, the icon list sends messages
// of the type SEL_INSERTED, SEL_REPLACED, or SEL_DELETED.
// In each of these cases, the index to the item, if any, is passed in the
// 3rd argument of the message.

class FXAPI IconList : public FXScrollArea
{
    FXDECLARE(IconList)
protected:
    FXHeader          *header;            // Header control
    IconItemList      items;		      // Item list
    int              nrows;             // Number of rows
    int              ncols;             // Number of columns
    int              anchor;            // Anchor item
    int              current;           // Current item
    int              extent;            // Extent item
    int              cursor;            // Cursor item
    int              viewable;          // Visible item
    FXFont            *font;              // Font
    IconListSortFunc   sortfunc;          // Item sort function
    FXColor            textColor;         // Text color
    FXColor            selbackColor;      // Selected back color
    FXColor            seltextColor;      // Selected text color
	FXColor			   highlightColor;    // Highlight color
	FXColor			   sortColor;         // Sort color
	FXColor			   highlightSortColor;// Highlight sort color
    int              itemWidth;         // Item width
    int              itemHeight;        // Item height
    int              itemSpace;         // Space for item label
    int              anchorx;           // Rectangular selection
    int              anchory;
    int              currentx;
    int              currenty;
    int              grabx;             // Grab point x
    int              graby;             // Grab point y
    FXString           lookup;            // Lookup string
    FXString           help;              // Help text
    FXbool             state;             // State of item
	FXbool			   allowTooltip;      // Allow tooltip in single click mode
	unsigned int			   numsortheader;     // Index of the sorted column
	double           headerpct[10];     // Header sizes, relatively to the list width (in percent)
	int			   count;             // Counter used to properly initialize the relative header sizes
	FXbool 			   autosize;
	FXbool             ignorecase;        // Case sensitivity for file name sorting
	FXbool			   initheaderpct;	  // Indicates we have to initialize the headerpct for the deletion columns
protected:
    IconList()
	{}
    void recompute();
    void getrowscols(int& nr,int& nc,int w,int h) const;
    void drawLasso(int x0,int y0,int x1,int y1);
    void lassoChanged(int ox,int oy,int ow,int oh,int nx,int ny,int nw,int nh,FXbool notify);
    virtual void moveContents(int x,int y);
    virtual IconItem *createItem(const FXString& text,FXIcon *big,FXIcon* mini,void* ptr);
    static int compareSection(const char *p,const char* q,int s);
    static int compareSectionCase(const char *p,const char* q,int s);
private:
    IconList(const IconList&);
    IconList &operator=(const IconList&);
public:
	long onConfigure(FXObject*,FXSelector,void*);
    long onPaint(FXObject*,FXSelector,void*);
    long onEnter(FXObject*,FXSelector,void*);
    long onLeave(FXObject*,FXSelector,void*);
    long onUngrabbed(FXObject*,FXSelector,void*);
    long onKeyPress(FXObject*,FXSelector,void*);
    long onKeyRelease(FXObject*,FXSelector,void*);
    long onLeftBtnPress(FXObject*,FXSelector,void*);
    long onLeftBtnRelease(FXObject*,FXSelector,void*);
    long onRightBtnPress(FXObject*,FXSelector,void*);
    long onRightBtnRelease(FXObject*,FXSelector,void*);
    long onMotion(FXObject*,FXSelector,void*);
    long onQueryTip(FXObject*,FXSelector,void*);
    long onQueryHelp(FXObject*,FXSelector,void*);
    long onTipTimer(FXObject*,FXSelector,void*);
    long onCmdSelectAll(FXObject*,FXSelector,void*);
    long onCmdDeselectAll(FXObject*,FXSelector,void*);
    long onCmdSelectInverse(FXObject*,FXSelector,void*);
    long onCmdArrangeByRows(FXObject*,FXSelector,void*);
    long onUpdArrangeByRows(FXObject*,FXSelector,void*);
    long onCmdArrangeByColumns(FXObject*,FXSelector,void*);
    long onUpdArrangeByColumns(FXObject*,FXSelector,void*);
    long onCmdShowDetails(FXObject*,FXSelector,void*);
    long onUpdShowDetails(FXObject*,FXSelector,void*);
    long onCmdShowBigIcons(FXObject*,FXSelector,void*);
    long onUpdShowBigIcons(FXObject*,FXSelector,void*);
    long onCmdShowMiniIcons(FXObject*,FXSelector,void*);
    long onUpdShowMiniIcons(FXObject*,FXSelector,void*);
    long onHeaderChanged(FXObject*,FXSelector,void*);
    long onHeaderResize(FXObject*,FXSelector,void*);
    long onFocusIn(FXObject*,FXSelector,void*);
    long onFocusOut(FXObject*,FXSelector,void*);
    long onClicked(FXObject*,FXSelector,void*);
    long onDoubleClicked(FXObject*,FXSelector,void*);
    long onTripleClicked(FXObject*,FXSelector,void*);
    long onCommand(FXObject*,FXSelector,void*);
    long onAutoScroll(FXObject*,FXSelector,void*);
    long onLookupTimer(FXObject*,FXSelector,void*);
    long onCmdSetValue(FXObject*,FXSelector,void*);
    long onCmdGetIntValue(FXObject*,FXSelector,void*);
    long onCmdSetIntValue(FXObject*,FXSelector,void*);
	long onCmdToggleAutosize(FXObject*,FXSelector,void*);
	long onUpdToggleAutosize(FXObject*,FXSelector,void*);
	long onCmdHeaderClicked(FXObject*,FXSelector,void*);
public:
    static int ascending(const IconItem* a,const IconItem* b);
    static int descending(const IconItem* a,const IconItem* b);
    static int ascendingCase(const IconItem* a,const IconItem* b);
    static int descendingCase(const IconItem* a,const IconItem* b);
public:
    enum
    {
        ID_SHOW_DETAILS=FXScrollArea::ID_LAST,
        ID_SHOW_MINI_ICONS,
        ID_SHOW_BIG_ICONS,
        ID_ARRANGE_BY_ROWS,
        ID_ARRANGE_BY_COLUMNS,
        ID_HEADER_CHANGE,
        ID_LOOKUPTIMER,
        ID_SELECT_ALL,
        ID_DESELECT_ALL,
        ID_SELECT_INVERSE,
        ID_AUTOSIZE,
        ID_LAST
    };
public:

    // Construct icon list with no items in it initially
    IconList(FXComposite *p,FXObject* tgt=NULL,FXSelector sel=0,unsigned int opts=_ICONLIST_NORMAL,int x=0,int y=0,int w=0,int h=0);

    // Create server-side resources
    virtual void create();

    // Detach server-side resources
    virtual void detach();

    // Recalculate layout
    virtual void recalc();

    // Perform layout
    virtual void layout();

	// Compute and return content width
    virtual int getContentWidth();

    // Return content height
    virtual int getContentHeight();

    // Icon list can receive focus
    virtual bool canFocus() const;

    // Move the focus to this window
    virtual void setFocus();

    // Remove the focus from this window
    virtual void killFocus();

    // Return viewport size
    virtual int getViewportHeight();

    // Resize this window to the specified width and height
    virtual void resize(int w,int h);

    // Move and resize this window in the parent's coordinates
    virtual void position(int x,int y,int w,int h);

    // Return ignore case flag
	FXbool getIgnoreCase() const
	{
		return ignorecase;
	}
	
	// Set ignore case flag
	void setIgnoreCase(const FXbool);
	
	// Return number of items
    int getNumItems() const
    {
        return items.no();
    }

    // Return number of rows
    int getNumRows() const
    {
        return nrows;
    }

    // Return number of columns
    int getNumCols() const
    {
        return ncols;
    }

    // Return header control
    FXHeader* getHeader() const
    {
        return header;
    }

    // Set headers from array of strings
    void setHeaders(const char** strings,int size=1);

    // Set headers from newline separated strings
    void setHeaders(const FXString& strings,int size=1);

    // Append header with given text and optional icon
    void appendHeader(const FXString& text,FXIcon *icon=NULL,int size=1);

    // Remove header at index
    void removeHeader(int index);

    // Change text of header at index
    void setHeaderText(int index,const FXString& text);

    // Return text of header at index
    FXString getHeaderText(int index) const;

    // Change icon of header at index
    void setHeaderIcon(int index,FXIcon *icon);

    // Return icon of header at index
    FXIcon* getHeaderIcon(int index) const;

    // Change size of header at index
    void setHeaderSize(int index,int size);

    // Return width of header at index
    int getHeaderSize(int index) const;

    // Return number of headers
    int getNumHeaders() const;

    // Return the item at the given index
    IconItem *getItem(int index) const;

    // Replace the item with a [possibly subclassed] item
    int setItem(int index,IconItem* item,FXbool notify=FALSE);

    // Replace items text, icons, and user-data pointer
    int setItem(int index,const FXString& text,FXIcon *big=NULL,FXIcon* mini=NULL,void* ptr=NULL,FXbool notify=FALSE);

    // Fill list by appending items from array of strings
    int fillItems(const char** strings,FXIcon *big=NULL,FXIcon* mini=NULL,void* ptr=NULL,FXbool notify=FALSE);

    // Fill list by appending items from newline separated strings
    int fillItems(const FXString& strings,FXIcon *big=NULL,FXIcon* mini=NULL,void* ptr=NULL,FXbool notify=FALSE);

    // Insert a new [possibly subclassed] item at the give index
    int insertItem(int index,IconItem* item,FXbool notify=FALSE);

    // Insert item at index with given text, icons, and user-data pointer
    int insertItem(int index,const FXString& text,FXIcon *big=NULL,FXIcon* mini=NULL,void* ptr=NULL,FXbool notify=FALSE);

    // Append a [possibly subclassed] item to the end of the list
    int appendItem(IconItem* item,FXbool notify=FALSE);

    // Append new item with given text and optional icons, and user-data pointer
    int appendItem(const FXString& text,FXIcon *big=NULL,FXIcon* mini=NULL,void* ptr=NULL,FXbool notify=FALSE);

    // Prepend a [possibly subclassed] item to the end of the list
    int prependItem(IconItem* item,FXbool notify=FALSE);

    // Prepend new item with given text and optional icons, and user-data pointer
    int prependItem(const FXString& text,FXIcon *big=NULL,FXIcon* mini=NULL,void* ptr=NULL,FXbool notify=FALSE);

    // Move item from oldindex to newindex
    int moveItem(int newindex,int oldindex,FXbool notify=FALSE);

    // Extract item from list
    IconItem* extractItem(int index,FXbool notify=FALSE);

    // Remove item from list
    void removeItem(int index,FXbool notify=FALSE);

    // Remove all items from list
    void clearItems(FXbool notify=FALSE);

    // Return item width
    int getItemWidth() const
    {
        return itemWidth;
    }

    // Return item height
    int getItemHeight() const
    {
        return itemHeight;
    }

	
    // Return index of item at x,y, or -1 if none
    virtual int getItemAt(int x,int y);

	// Search items by name, beginning from item start.  If the start
	// item is -1 the search will start at the first item in the list.
	// Flags may be SEARCH_FORWARD or SEARCH_BACKWARD to control the
	// search direction; this can be combined with SEARCH_NOWRAP or SEARCH_WRAP
	// to control whether the search wraps at the start or end of the list.
	// The option SEARCH_IGNORECASE causes a case-insensitive match.  Finally,
	// passing SEARCH_PREFIX causes searching for a prefix of the item name.
	// Return -1 if no matching item is found.
    int findItem(const FXString& text,int start=-1,unsigned int flags=SEARCH_FORWARD|SEARCH_WRAP) const;

   
    // Search items by associated user data, beginning from item start. If the
    // start item is -1 the search will start at the first item in the list.
    // Flags may be SEARCH_FORWARD or SEARCH_BACKWARD to control the
    // search direction; this can be combined with SEARCH_NOWRAP or SEARCH_WRAP
    // to control whether the search wraps at the start or end of the list.
    int findItemByData(const void *ptr,int start=-1,unsigned int flags=SEARCH_FORWARD|SEARCH_WRAP) const;

    // Scroll to make item at index visible
    virtual void makeItemVisible(int index);

    // Change item text
    void setItemText(int index,const FXString& text);

    // Return item text
    FXString getItemText(int index) const;

    // Change item big icon
    void setItemBigIcon(int index,FXIcon* icon,FXbool owned=FALSE);

    // Return big icon of item at index
    FXIcon* getItemBigIcon(int index) const;

    // Change item mini icon
    void setItemMiniIcon(int index,FXIcon* icon,FXbool owned=FALSE);

    // Return mini icon of item at index
    FXIcon* getItemMiniIcon(int index) const;

    // Change item user-data pointer
    void setItemData(int index,void* ptr);

    // Return item user-data pointer
    void* getItemData(int index) const;

	// Return TRUE if item is selected
	FXbool isItemSelected(int index) const
	{
		if ((unsigned int)index >= (unsigned int)items.no())
			fxerror("%s::isItemSelected: index out of range.\n",getClassName());
		return items[index]->isSelected();
	}

    // Return TRUE if item at index is current
    FXbool isItemCurrent(int index) const;

    // Return TRUE if item at index is visible
    FXbool isItemVisible(int index) const;

    // Return TRUE if item at index is enabled
    FXbool isItemEnabled(int index) const;

    // Return item hit code: 0 outside, 1 icon, 2 text
    int hitItem(int index,int x,int y,int ww=1,int hh=1) const;

    // Repaint item at index
    void updateItem(int index) const;

    // Enable item at index
    virtual FXbool enableItem(int index);

    // Disable item at index
    virtual FXbool disableItem(int index);

    // Select item at index
    virtual FXbool selectItem(int index,FXbool notify=FALSE);

    // Deselect item at index
    virtual FXbool deselectItem(int index,FXbool notify=FALSE);

    // Toggle item at index
    virtual FXbool toggleItem(int index,FXbool notify=FALSE);

    // Select items in rectangle
    virtual FXbool selectInRectangle(int x,int y,int w,int h,FXbool notify=FALSE);

    // Extend selection from anchor index to index
    virtual FXbool extendSelection(int index,FXbool notify=FALSE);

    // Deselect all items
    virtual FXbool killSelection(FXbool notify=FALSE);

    // Change current item index
    virtual void setCurrentItem(int index,FXbool notify=FALSE);

    // Return current item index, or -1 if none
    int getCurrentItem() const
    {
        return current;
    }

    // Change anchor item index
    void setAnchorItem(int index);

    // Return anchor item index, or -1 if none
    int getAnchorItem() const
    {
        return anchor;
    }

    // Return index of item under cursor, or -1 if none
    int getCursorItem() const
    {
        return cursor;
    }

    // Sort items
    void sortItems();

    // Return sort function
    IconListSortFunc getSortFunc() const
    {
        return sortfunc;
    }

    // Change sort function
    void setSortFunc(IconListSortFunc func)
    {
        sortfunc=func;
    }

	// Set sort header
	void setSortHeader(const unsigned int num)
	{
		numsortheader=num;
	}
	
	// Get sort header
	unsigned int getSortHeader()
	{
		return numsortheader;
	}
	
    
	// Change text font
    void setFont(FXFont* fnt);

    // Return text font
    FXFont* getFont() const
    {
        return font;
    }

    // Return normal text color
    FXColor getTextColor() const
    {
        return textColor;
    }

    // Change normal text color
    void setTextColor(FXColor clr);

    // Return selected text background
    FXColor getSelBackColor() const
    {
        return selbackColor;
    }

    // Change selected text background
    void setSelBackColor(FXColor clr);

    // Return selected text color
    FXColor getSelTextColor() const
    {
        return seltextColor;
    }

    // Return highlight color
    FXColor getHighlightColor() const
    {
        return highlightColor;
    }

    // Return sort color
    FXColor getSortColor() const
    {
        return sortColor;
    }

    // Return highlight sort color
    FXColor getHighlightSortColor() const
    {
        return highlightSortColor;
    }

    // Change selected text color
    void setSelTextColor(FXColor clr);

    // Change maximum item space for each item
    void setItemSpace(int s);

    // Return maximum item space
    int getItemSpace() const
    {
        return itemSpace;
    }

    // Get the current icon list style
    unsigned int getListStyle() const;

    // Set the current icon list style.
    void setListStyle(unsigned int style);

    // Set the status line help text for this widget
    void setHelpText(const FXString& text);

    // Get the status line help text for this widget
    const FXString& getHelpText() const
    {
        return help;
    }

    // Save list to a stream
    virtual void save(FXStream& store) const;

    // Load list from a stream
    virtual void load(FXStream& store);

    // Destructor
    virtual ~IconList();
};


#endif
