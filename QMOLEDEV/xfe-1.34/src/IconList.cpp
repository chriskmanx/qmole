// Icon list. Taken from the FOX library and slightly modified.
#include "config.h"
#include "i18n.h"

#include <ctype.h>
#include <time.h>

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>
#include <FXJPGIcon.h>
#include <FXTIFIcon.h>
#if defined(linux)
#include <mntent.h>
#endif

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "IconList.h"


// Number of columns in detailed view, in the general case
// NB : when the deletion date and original path are displayed, two more columns are added
#define NUM_HEADERS 8



#define SIDE_SPACING             4    // Left or right spacing between items
#define DETAIL_TEXT_SPACING      2    // Spacing between text and icon in detail icon mode
#define MINI_TEXT_SPACING        2    // Spacing between text and icon in mini icon mode
#define BIG_LINE_SPACING         6    // Line spacing in big icon mode
#define BIG_TEXT_SPACING         2    // Spacing between text and icon in big icon mode
#define ITEM_SPACE             192    // Default space for item name

#define SELECT_MASK   (_ICONLIST_EXTENDEDSELECT|_ICONLIST_SINGLESELECT|_ICONLIST_BROWSESELECT|_ICONLIST_MULTIPLESELECT)
#define ICONLIST_MASK (SELECT_MASK|_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS|_ICONLIST_COLUMNS|_ICONLIST_SEARCH|_ICONLIST_STANDARD|_ICONLIST_AUTOSIZE)

extern unsigned int single_click;
extern FXbool file_tooltips;
extern FXbool relative_resize;


// Object implementation
FXIMPLEMENT(IconItem,FXObject,NULL,0)


// Check if integer is odd (return TRUE) or even (return FALSE)
inline FXbool IconItem::isOdd(int i) const
{
	return (FXbool)(i & 0x1);
}

// Draw item
void IconItem::draw(IconList* list,FXDC& dc,int x,int y,int w,int h) const
{
    register unsigned int options=list->getListStyle();
    if (options&_ICONLIST_BIG_ICONS)
    	drawBigIcon(list,dc,x,y,w,h);
    else if (options&_ICONLIST_MINI_ICONS)
    	drawMiniIcon(list,dc,x,y,w,h);
    else
    	drawDetails(list,dc,x,y,w,h);
}


// Draw big icon
void IconItem::drawBigIcon(const IconList* list,FXDC& dc,int x,int y,int w,int h) const
{
    register int len,dw,s,space,xt,yt,xi,yi;
    register FXFont *font=list->getFont();
    register int iw=0,ih=0,tw=0,th=0,ss=0;
    space=w-SIDE_SPACING;
      
    if (!label.empty())
    {
        for (len=0; len<label.length() && label[len]!='\t'; len++)
        	;
        tw=4+font->getTextWidth(label.text(),len);        
        th=4+font->getFontHeight();
        yt=y+h-th-BIG_LINE_SPACING/2;
        dw=0;
        if (tw>space)
        {       	
            dw=font->getTextWidth("...",3);
            s=space-dw;
            while ((tw=4+font->getTextWidth(label.text(),len))>s && len>1)
            	len=label.dec(len);
            if (tw>s)
            	dw=0;
        }
        if (tw<=space)         // FIXME as below in drawDetails
        {
            xt=x+(w-tw-dw)/2;
            if (isSelected())
            {
                dc.setForeground(list->getSelBackColor());
                dc.fillRectangle(xt,yt,tw+dw,th);
            }
            if (!isEnabled())
                dc.setForeground(makeShadowColor(list->getBackColor()));
            else if (isSelected())
                dc.setForeground(list->getSelTextColor());
            else
                dc.setForeground(list->getTextColor());
            dc.drawText(xt+2,yt+font->getFontAscent()+2,label.text(),len);
            if (dw)
            	dc.drawText(xt+tw-2,yt+font->getFontAscent()+2,"...",3);
            if (hasFocus())
                dc.drawFocusRectangle(xt+1,yt+1,tw+dw-2,th-2);
        }
        ss=BIG_TEXT_SPACING;    // Space between text and icon only added if we have both icon and text
    }
    if (bigIcon)
    {
        iw=bigIcon->getWidth();
        ih=bigIcon->getHeight();
        xi=x+(w-iw)/2;
        yi=y+BIG_LINE_SPACING/2+(h-th-BIG_LINE_SPACING-ss-ih)/2;
        if (isSelected())
            dc.drawIconShaded(bigIcon,xi,yi);
        else
            dc.drawIcon(bigIcon,xi,yi);
    }
}


// Draw mini icon
void IconItem::drawMiniIcon(const IconList* list,FXDC& dc,int x,int y,int w,int h) const
{
    register FXFont *font=list->getFont();
    register int iw=0,ih=0,tw=0,th=0;
    register int len,dw,s,space;
    x+=SIDE_SPACING/2;
    space=w-SIDE_SPACING;
    if (miniIcon)
    {
        iw=miniIcon->getWidth();
        ih=miniIcon->getHeight();
        if (isSelected())
            dc.drawIconShaded(miniIcon,x,y+(h-ih)/2);
        else
            dc.drawIcon(miniIcon,x,y+(h-ih)/2);
        x+=iw+MINI_TEXT_SPACING;
        space-=iw+MINI_TEXT_SPACING;
    }
    if (!label.empty())
    {
        for (len=0; len<label.length() && label[len]!='\t'; len++)
        	;
        tw=4+font->getTextWidth(label.text(),len);
        th=4+font->getFontHeight();
        dw=font->getTextWidth("...",3);
        y+=(h-th)/2;
        dw=0;
        if (tw>space)                  // FIXME as below in drawDetails
        {
            dw=font->getTextWidth("...",3);
            s=space-dw;
            while ((tw=4+font->getTextWidth(label.text(),len))>s && len>1)
            	len=label.dec(len);
            if (tw>s)
            	dw=0;
        }
        if (tw<=space)
        {
            if (isSelected())
            {
                dc.setForeground(list->getSelBackColor());
                dc.fillRectangle(x,y,tw+dw,th);
            }
            if (!isEnabled())
                dc.setForeground(makeShadowColor(list->getBackColor()));
            else if (isSelected())
                dc.setForeground(list->getSelTextColor());
            else
                dc.setForeground(list->getTextColor());
            dc.drawText(x+2,y+font->getFontAscent()+2,label.text(),len);
            if (dw)
            	dc.drawText(x+tw-2,y+font->getFontAscent()+2,"...",3);
            if (hasFocus())
                dc.drawFocusRectangle(x+1,y+1,tw+dw-2,th-2);
        }
    }
}


void IconItem::drawDetails(IconList* list,FXDC& dc,int x,int y,int,int h) const
{
    register FXHeader *header=list->getHeader();
    register FXFont *font=list->getFont();
    register int iw=0,ih=0,tw=0,th=0,yt,beg,end,hi,drw,space,used,dw,xx,dx=0;
	register FXbool highlight;
	register unsigned int options=list->getListStyle();

    if (header->getNumItems()==0)
        return;

    // Display a colored bar in the Filelist, one line over two
	highlight=IconItem::isOdd(list->getItemAt(x,y));
    if (highlight)
    {
        dc.setForeground(list->getHighlightColor());
        dc.fillRectangle(x,y,header->getTotalSize(),h);
    }
    highlight = !highlight;
	
	// Darken the sorted column
	for (hi=0; hi<(int)list->getSortHeader(); hi++)
		dx+=header->getItemSize(hi);

	if (highlight)
	{
		dc.setForeground(list->getSortColor());
		dc.fillRectangle(x+dx,y,header->getItemSize(list->getSortHeader()),h);
	}
	else
	{
		dc.setForeground(list->getHighlightSortColor());
		dc.fillRectangle(x+dx,y,header->getItemSize(list->getSortHeader()),h);		
	}
	
    // Color the selected items
	if (isSelected())
    {
        dc.setForeground(list->getSelBackColor());
        dc.fillRectangle(x,y,header->getTotalSize(),h);
    }
    if (hasFocus())
        dc.drawFocusRectangle(x+1,y+1,header->getTotalSize()-2,h-2);
    xx=x+SIDE_SPACING/2;
    if (miniIcon)
    {
        iw=miniIcon->getWidth();
        ih=miniIcon->getHeight();
        dc.setClipRectangle(x,y,header->getItemSize(0),h);
        dc.drawIcon(miniIcon,xx,y+(h-ih)/2);
        dc.clearClipRectangle();
        xx+=iw+DETAIL_TEXT_SPACING;
    }
    if (!label.empty())
    {
        th=font->getFontHeight();
        dw=font->getTextWidth("...",3);
        yt=y+(h-th-4)/2;
        if (!isEnabled())
            dc.setForeground(makeShadowColor(list->getBackColor()));
        else if (isSelected())
            dc.setForeground(list->getSelTextColor());
        else
            dc.setForeground(list->getTextColor());
        used=iw+DETAIL_TEXT_SPACING+SIDE_SPACING/2;

        for (hi=beg=0; beg<label.length() && hi<header->getNumItems(); hi++,beg=end+1)
        {
            space=header->getItemSize(hi)-used;
            for (end=beg; end<label.length() && label[end]!='\t'; end++)
                ;
            if (end>beg)
            {
                drw=end-beg;
                tw=font->getTextWidth(&label[beg],drw);
                
				// Right justify the file size column (second one in file list mode or third one in search list mode)
				if (!(options&_ICONLIST_STANDARD) && ((!(options&_ICONLIST_SEARCH) && hi==1) || (options&_ICONLIST_SEARCH && hi==2)))
				{				
					if (tw>space-20)
					{
						while ((tw=font->getTextWidth(&label[beg],drw))+dw>space-4 && drw>1)
							drw--;
						dc.setClipRectangle(xx,y,space,h);
						dc.drawText(xx+2,yt+font->getFontAscent()+2,&label[beg],drw);
						dc.drawText(xx+tw+2,yt+font->getFontAscent()+2,"...",3);
						dc.clearClipRectangle();
					}
					else
						dc.drawText(xx+space-tw-20,yt+font->getFontAscent()+2,&label[beg],drw);
				}
				else
				{
					if (tw>space-4)
					{
						while ((tw=font->getTextWidth(&label[beg],drw))+dw>space-4 && drw>1)
							drw--;
						dc.setClipRectangle(xx,y,space,h);
						dc.drawText(xx+2,yt+font->getFontAscent()+2,&label[beg],drw);
						dc.drawText(xx+tw+2,yt+font->getFontAscent()+2,"...",3);
						dc.clearClipRectangle();
					}
					else
						dc.drawText(xx+2,yt+font->getFontAscent()+2,&label[beg],drw);
				}
            }
            xx+=space;
            used=0;
        }
    }
}


// See if item got hit and where: 0 is outside, 1 is icon, 2 is text
int IconItem::hitItem(const IconList* list,int rx,int ry,int rw,int rh) const
{
    register int iw=0,tw=0,ih=0,th=0,ss=0,ix,iy,tx,ty,w,h,sp,tlen;
    register unsigned int options=list->getListStyle();
    register FXFont *font=list->getFont();
    for (tlen=0; tlen<label.length() && label[tlen]!='\t'; tlen++);
    if (options&_ICONLIST_BIG_ICONS)
    {
        w=list->getItemSpace();
        h=list->getItemHeight();
        sp=w-SIDE_SPACING;
        if (!label.empty())
        {
            tw=4+font->getTextWidth(label.text(),tlen);
            th=4+font->getFontHeight();
            if (tw>sp)
            	tw=sp;
            if (bigIcon)
            	ss=BIG_TEXT_SPACING;
        }
        if (bigIcon)
        {
            iw=bigIcon->getWidth();
            ih=bigIcon->getHeight();
        }
        ty=h-th-BIG_LINE_SPACING/2;
        iy=BIG_LINE_SPACING/2+(h-th-BIG_LINE_SPACING-ss-ih)/2;
        ix=(w-iw)/2;
        tx=(w-tw)/2;
    }
    else if (options&_ICONLIST_MINI_ICONS)
    {
        sp=list->getItemSpace()-SIDE_SPACING;
        ix=SIDE_SPACING/2;
        tx=SIDE_SPACING/2;
        if (miniIcon)
        {
            iw=miniIcon->getWidth();
            ih=miniIcon->getHeight();
            tx+=iw+MINI_TEXT_SPACING;
            sp=sp-iw-MINI_TEXT_SPACING;
        }
        if (!label.empty())
        {
            tw=4+font->getTextWidth(label.text(),tlen);
            th=4+font->getFontHeight();
            if (tw>sp)
            	tw=sp;
        }
        h=list->getItemHeight();
        iy=(h-ih)/2;
        ty=(h-th)/2;
    }
    else
    {
        ix=SIDE_SPACING/2;
        tx=SIDE_SPACING/2;
        if (miniIcon)
        {
            iw=miniIcon->getWidth();
            ih=miniIcon->getHeight();
            tx+=iw+DETAIL_TEXT_SPACING;
        }
        if (!label.empty())
        {
            tw=10000000;
            th=4+font->getFontHeight();
        }
        h=list->getItemHeight();
        iy=(h-ih)/2;
        ty=(h-th)/2;
    }

    // In icon?
    if (ix<=rx+rw && iy<=ry+rh && rx<ix+iw && ry<iy+ih)
    	return 1;

    // In text?
    if (tx<=rx+rw && ty<=ry+rh && rx<tx+tw && ry<ty+th)
    	return 2;

    // Outside
    return 0;
}


// Set or kill focus
void IconItem::setFocus(FXbool focus)
{
    if (focus)
    	state|=FOCUS;
    else
    	state&=~FOCUS;
}

// Select or deselect item
void IconItem::setSelected(FXbool selected)
{
    if (selected)
    	state|=SELECTED;
    else
    	state&=~SELECTED;
}

// Enable or disable the item
void IconItem::setEnabled(FXbool enabled)
{
    if (enabled)
    	state&=~DISABLED;
    else
    	state|=DISABLED;
}

// Icon is draggable
void IconItem::setDraggable(FXbool draggable)
{
    if (draggable)
    	state|=DRAGGABLE;
    else
    	state&=~DRAGGABLE;
}


// Change item's text label
void IconItem::setText(const FXString& txt)
{
    label=txt;
}


// Change item's big icon
void IconItem::setBigIcon(FXIcon* icn,FXbool owned)
{
    if (bigIcon && (state&BIGICONOWNED))
    {
        if (bigIcon!=icn)
        	delete bigIcon;
        state&=~BIGICONOWNED;
    }
    bigIcon=icn;
    if (bigIcon && owned)
        state|=BIGICONOWNED;
}


// Change item's mini icon
void IconItem::setMiniIcon(FXIcon* icn,FXbool owned)
{
    if (miniIcon && (state&MINIICONOWNED))
    {
        if (miniIcon!=icn)
        	delete miniIcon;
        state&=~MINIICONOWNED;
    }
    miniIcon=icn;
    if (miniIcon && owned)
        state|=MINIICONOWNED;
}


// Create icon
void IconItem::create()
{
    if (bigIcon)
    	bigIcon->create();
    if (miniIcon)
    	miniIcon->create();
}


// Destroy icon
void IconItem::destroy()
{
    if ((state&BIGICONOWNED) && bigIcon)
    	bigIcon->destroy();
    if ((state&MINIICONOWNED) && miniIcon)
    	miniIcon->destroy();
}


// Detach from icon resource
void IconItem::detach()
{
    if (bigIcon)
    	bigIcon->detach();
    if (miniIcon)
    	miniIcon->detach();
}


// Get item width
int IconItem::getWidth(const IconList* list) const
{
    register unsigned int options=list->getListStyle();
    register FXFont *font=list->getFont();
    register int iw=0,tw=0,w=0,tlen;
    for (tlen=0; tlen<label.length() && label[tlen]!='\t'; tlen++);
    if (options&_ICONLIST_BIG_ICONS)
    {
        if (bigIcon)
        	iw=bigIcon->getWidth();
        if (!label.empty())
        	tw=4+font->getTextWidth(label.text(),tlen);
        w=SIDE_SPACING+FXMAX(tw,iw);
    }
    else if (options&_ICONLIST_MINI_ICONS)
    {
        if (miniIcon)
        	iw=miniIcon->getWidth();
        if (!label.empty())
        	tw=4+font->getTextWidth(label.text(),tlen);
        if (iw && tw)
        	iw+=MINI_TEXT_SPACING;
        w=SIDE_SPACING+iw+tw;
    }
    else
        w=SIDE_SPACING;
    return w;
}


// Get item height
int IconItem::getHeight(const IconList* list) const
{
    register unsigned int options=list->getListStyle();
    register int ih=0,th=0,h=0;
    if (options&_ICONLIST_BIG_ICONS)
    {
        if (bigIcon)
        	ih=bigIcon->getHeight();
        if (!label.empty())
        	th=4+list->getFont()->getFontHeight();
        if (ih && th)
        	ih+=BIG_TEXT_SPACING;
        h=BIG_LINE_SPACING+ih+th;
    }
    else if (options&_ICONLIST_MINI_ICONS)
    {
        if (miniIcon)
        	ih=miniIcon->getHeight();
        if (!label.empty())
        	th=4+list->getFont()->getFontHeight();
        h=FXMAX(ih,th);
    }
    else
    {
        if (miniIcon)
        	ih=miniIcon->getHeight();
        if (!label.empty())
        	th=4+list->getFont()->getFontHeight();
        h=FXMAX(ih,th);
    }
    return h;
}


// Save data
void IconItem::save(FXStream& store) const
{
    FXObject::save(store);
    store << label;
    store << bigIcon;
    store << miniIcon;
    store << state;
}


// Load data
void IconItem::load(FXStream& store)
{
    FXObject::load(store);
    store >> label;
    store >> bigIcon;
    store >> miniIcon;
    store >> state;
}


// Delete icons if owned
IconItem::~IconItem()
{
    if (state&BIGICONOWNED)
    	delete bigIcon;
    if (state&MINIICONOWNED)
    	delete miniIcon;
    bigIcon=(FXIcon*)-1L;
    miniIcon=(FXIcon*)-1L;
}



// Map
FXDEFMAP(IconList) IconListMap[]=
{
    FXMAPFUNC(SEL_PAINT,0,IconList::onPaint),
    FXMAPFUNC(SEL_MOTION,0,IconList::onMotion),
	FXMAPFUNC(SEL_CONFIGURE,0,IconList::onConfigure),
    FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,IconList::onLeftBtnPress),
    FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,IconList::onLeftBtnRelease),
    FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,IconList::onRightBtnPress),
    FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,IconList::onRightBtnRelease),
    FXMAPFUNC(SEL_TIMEOUT,FXWindow::ID_AUTOSCROLL,IconList::onAutoScroll),
    FXMAPFUNC(SEL_TIMEOUT,IconList::ID_TIPTIMER,IconList::onTipTimer),
    FXMAPFUNC(SEL_TIMEOUT,IconList::ID_LOOKUPTIMER,IconList::onLookupTimer),
    FXMAPFUNC(SEL_UNGRABBED,0,IconList::onUngrabbed),
    FXMAPFUNC(SEL_KEYPRESS,0,IconList::onKeyPress),
    FXMAPFUNC(SEL_KEYRELEASE,0,IconList::onKeyRelease),
    FXMAPFUNC(SEL_ENTER,0,IconList::onEnter),
    FXMAPFUNC(SEL_LEAVE,0,IconList::onLeave),
    FXMAPFUNC(SEL_FOCUSIN,0,IconList::onFocusIn),
    FXMAPFUNC(SEL_FOCUSOUT,0,IconList::onFocusOut),
    FXMAPFUNC(SEL_CLICKED,0,IconList::onClicked),
    FXMAPFUNC(SEL_DOUBLECLICKED,0,IconList::onDoubleClicked),
    FXMAPFUNC(SEL_TRIPLECLICKED,0,IconList::onTripleClicked),
	FXMAPFUNC(SEL_QUERY_TIP,0,IconList::onQueryTip),
    FXMAPFUNC(SEL_QUERY_HELP,0,IconList::onQueryHelp),
    FXMAPFUNC(SEL_CHANGED,IconList::ID_HEADER_CHANGE,IconList::onHeaderChanged),
    FXMAPFUNC(SEL_CLICKED,IconList::ID_HEADER_CHANGE,IconList::onHeaderResize),
    FXMAPFUNC(SEL_UPDATE,IconList::ID_SHOW_DETAILS,IconList::onUpdShowDetails),
    FXMAPFUNC(SEL_UPDATE,IconList::ID_SHOW_MINI_ICONS,IconList::onUpdShowMiniIcons),
    FXMAPFUNC(SEL_UPDATE,IconList::ID_SHOW_BIG_ICONS,IconList::onUpdShowBigIcons),
    FXMAPFUNC(SEL_UPDATE,IconList::ID_ARRANGE_BY_ROWS,IconList::onUpdArrangeByRows),
    FXMAPFUNC(SEL_UPDATE,IconList::ID_ARRANGE_BY_COLUMNS,IconList::onUpdArrangeByColumns),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_SHOW_DETAILS,IconList::onCmdShowDetails),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_SHOW_MINI_ICONS,IconList::onCmdShowMiniIcons),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_SHOW_BIG_ICONS,IconList::onCmdShowBigIcons),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_ARRANGE_BY_ROWS,IconList::onCmdArrangeByRows),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_ARRANGE_BY_COLUMNS,IconList::onCmdArrangeByColumns),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_SELECT_ALL,IconList::onCmdSelectAll),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_DESELECT_ALL,IconList::onCmdDeselectAll),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_SELECT_INVERSE,IconList::onCmdSelectInverse),
    FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETVALUE,IconList::onCmdSetValue),
    FXMAPFUNC(SEL_COMMAND,FXWindow::ID_SETINTVALUE,IconList::onCmdSetIntValue),
    FXMAPFUNC(SEL_COMMAND,FXWindow::ID_GETINTVALUE,IconList::onCmdGetIntValue),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_AUTOSIZE,IconList::onCmdToggleAutosize),
    FXMAPFUNC(SEL_UPDATE,IconList::ID_AUTOSIZE,IconList::onUpdToggleAutosize),
    FXMAPFUNC(SEL_COMMAND,IconList::ID_HEADER_CHANGE,IconList::onCmdHeaderClicked),
};


// Object implementation
FXIMPLEMENT(IconList,FXScrollArea,IconListMap,ARRAYNUMBER(IconListMap))




// Icon List
IconList::IconList(FXComposite *p,FXObject* tgt,FXSelector sel,unsigned int opts,int x,int y,int w,int h):
                   FXScrollArea(p,opts,x,y,w,h)
{
	//autosize=opts&_ICONLIST_AUTOSIZE; // Workaround for a bug (?)
    flags|=FLAG_ENABLED;

	// Headers look slightly different depending on the control theme 
	FXbool use_clearlooks=getApp()->reg().readUnsignedEntry("SETTINGS","use_clearlooks",TRUE);
	if (use_clearlooks)
    	header=new FXHeader(this,this,IconList::ID_HEADER_CHANGE,HEADER_TRACKING|HEADER_BUTTON|HEADER_RESIZE|FRAME_RAISED);
	else
    	header=new FXHeader(this,this,IconList::ID_HEADER_CHANGE,HEADER_TRACKING|HEADER_BUTTON|HEADER_RESIZE|FRAME_RAISED|FRAME_THICK);

    target=tgt;
    message=sel;
    nrows=1;
    ncols=1;
    anchor=-1;
    current=-1;
    extent=-1;
    cursor=-1;
    viewable=-1;
    font=getApp()->getNormalFont();
    sortfunc=NULL;
    textColor=getApp()->getForeColor();
    selbackColor=getApp()->getSelbackColor();
    seltextColor=getApp()->getSelforeColor();
	highlightColor=getApp()->reg().readColorEntry("SETTINGS","highlightcolor",FXRGB(238,238,238));

	// Sort colors for detailed mode
	FXColor listbackColor=getApp()->reg().readColorEntry("SETTINGS","listbackcolor",FXRGB(255,255,255));
	unsigned int R, G, B;
	R=(unsigned int)(DARKEN_SORT*FXREDVAL(listbackColor));
	G=(unsigned int)(DARKEN_SORT*FXGREENVAL(listbackColor));
	B=(unsigned int)(DARKEN_SORT*FXBLUEVAL(listbackColor));
	sortColor=FXRGB(R,G,B);
	R=(unsigned int)(DARKEN_SORT*FXREDVAL(highlightColor));
	G=(unsigned int)(DARKEN_SORT*FXGREENVAL(highlightColor));
	B=(unsigned int)(DARKEN_SORT*FXBLUEVAL(highlightColor));
	highlightSortColor=FXRGB(R,G,B);

	itemSpace=ITEM_SPACE;
    itemWidth=1;
    itemHeight=1;
    anchorx=0;
    anchory=0;
    currentx=0;
    currenty=0;
    grabx=0;
    graby=0;
    state=FALSE;
	numsortheader=0;
	count=0;
	ignorecase=TRUE;
	initheaderpct=TRUE;
	allowTooltip=TRUE;
}


// Used to resize the headers relatively to the list width
long IconList::onConfigure(FXObject*,FXSelector,void*)
{
	// Obtain the main window width
	int width=(this->getShell())->getWidth();
	
	// Initialize the relative sizes, skipping the first call
	if (count==1)
	{
		// Initialize the relative header sizes
		for (int hi=0; hi<getNumHeaders(); hi++)
			headerpct[hi]=(double)getHeaderSize(hi)/(double)width;
	}

	// Update the relative header sizes
	if (relative_resize && count>=1)
	{
		// Initialize the extra header pcts if necessary
		if (initheaderpct && getNumHeaders()==9)  // Search list, is it really necessary???
		{
			headerpct[8]=(double)getHeaderSize(8)/(double)width;
			initheaderpct=FALSE;
		}
		if (initheaderpct && getNumHeaders()==10)
		{
			headerpct[8]=(double)getHeaderSize(8)/(double)width;
			headerpct[9]=(double)getHeaderSize(9)/(double)width;
			initheaderpct=FALSE;
		}
		int newhsize;
		for (int hi=0; hi<getNumHeaders(); hi++)
		{
			newhsize=(int)round(headerpct[hi]*width);
			setHeaderSize(hi,newhsize);		
		}
	}
	
	// Update the counter
	if (count<=2)
		count++;

	return 1;
}



// Create window
void IconList::create()
{
    register int i;
    FXScrollArea::create();
    for (i=0; i<items.no(); i++)
    {
        items[i]->create();
    }
    font->create();
}


// Detach window
void IconList::detach()
{
    register int i;
    FXScrollArea::detach();
    for (i=0; i<items.no(); i++)
    {
        items[i]->detach();
    }
    font->detach();
}


// If window can have focus
bool IconList::canFocus() const
{
    return true;
}


// Into focus chain
void IconList::setFocus()
{
    FXScrollArea::setFocus();
    setDefault(TRUE);
}


// Out of focus chain
void IconList::killFocus()
{
    FXScrollArea::killFocus();
    setDefault(MAYBE);
}


// Move content
void IconList::moveContents(int x,int y)
{
    int dx=x-pos_x;
    int dy=y-pos_y;
    int top=0;
    pos_x=x;
    pos_y=y;
    if (!(options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS)))
    {
        top=header->getDefaultHeight();
        header->setPosition(x);
    }
	scroll(0,top,viewport_w,viewport_h,dx,dy);
}

// Propagate size change
void IconList::recalc()
{
    FXScrollArea::recalc();
    flags|=FLAG_RECALC;
    cursor=-1;
}


// Recompute interior
void IconList::recompute()
{	
    register int w,h,i;

    itemWidth=1;
    itemHeight=1;

    // Measure the items
    for (i=0; i<items.no(); i++)
    {
        w=items[i]->getWidth(this);
        h=items[i]->getHeight(this);
        if (w>itemWidth)
        	itemWidth=w;
        if (h>itemHeight)
        	itemHeight=h;
    }

    // Automatically size item spacing
    //if (autosize)
    if (options&_ICONLIST_AUTOSIZE)
    	itemSpace=FXMAX(itemWidth,1);
    else
    	itemSpace=ITEM_SPACE;
    	
    // Adjust for detail mode
    if (!(options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS)))
    	itemWidth=header->getDefaultWidth();

    // Get number of rows or columns
    getrowscols(nrows,ncols,width,height);

    // Done
    flags&=~FLAG_RECALC;
}


// Determine number of columns and number of rows
void IconList::getrowscols(int& nr,int& nc,int w,int h) const
{
    if (options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
    {
        if (options&_ICONLIST_COLUMNS)
        {
            nc=w/itemSpace;
            if (nc<1)
            	nc=1;
            nr=(items.no()+nc-1)/nc;
            if (nr*itemHeight > h)
            {
                nc=(w-vertical->getDefaultWidth())/itemSpace;
                if (nc<1) nc=1;
                nr=(items.no()+nc-1)/nc;
            }
            if (nr<1)
            	nr=1;
        }
        else
        {
            nr=h/itemHeight;
            if (nr<1)
            	nr=1;
            nc=(items.no()+nr-1)/nr;
            if (nc*itemSpace > w)
            {
                nr=(h-horizontal->getDefaultHeight())/itemHeight;
                if (nr<1)
                	nr=1;
                nc=(items.no()+nr-1)/nr;
            }
            if (nc<1)
            	nc=1;
        }
    }
    else
    {
        nr=items.no();
        nc=1;
    }
}


// Size of a possible column caption
int IconList::getViewportHeight()
{
    return (options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS)) ? height : height-header->getDefaultHeight();
}


// Determine content width of icon list
int IconList::getContentWidth()
{
    if (flags&FLAG_RECALC)
    	recompute();
    if (options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS))
    	return ncols*itemSpace;
    return header->getDefaultWidth();
}


// Determine content height of icon list
int IconList::getContentHeight()
{
    if (flags&FLAG_RECALC)
    	recompute();
    return nrows*itemHeight;
}


// Recalculate layout
void IconList::layout()
{
    // Update scroll bars
    FXScrollArea::layout();

    // In detail mode
    if (!(options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS)))
    {
        header->position(0,0,viewport_w,header->getDefaultHeight());
        header->show();
    }
    else
    {
        header->hide();
    }

    // Set line size
    vertical->setLine(itemHeight);
    horizontal->setLine(itemSpace);

    // We were supposed to make this item viewable
    if (0<=viewable)
    {
        makeItemVisible(viewable);
    }

    // Force repaint
    update();

    flags&=~FLAG_DIRTY;
}


// Changed size:- this is a bit tricky, because
// we don't want to re-measure the items, but the content
// size has changed because the number of rows/columns has...
void IconList::resize(int w,int h)
{
    int nr=nrows;
    int nc=ncols;
    if (w!=width || h!=height)
    {
        getrowscols(nrows,ncols,w,h);
        if (nr!=nrows || nc!=ncols)
        	update();
    }
    FXScrollArea::resize(w,h);
}


// Changed size and/or pos:- this is a bit tricky, because
// we don't want to re-measure the items, but the content
// size has changed because the number of rows/columns has...
void IconList::position(int x,int y,int w,int h)
{
    int nr=nrows;
    int nc=ncols;
    if (w!=width || h!=height)
    {
        getrowscols(nrows,ncols,w,h);
        if (nr!=nrows || nc!=ncols)
        	update();
    }
    FXScrollArea::position(x,y,w,h);
}


// A header button was clicked : pass the message to the target
long IconList::onCmdHeaderClicked(FXObject*,FXSelector,void* ptr)
{
    if (target && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr));
	return 1;
}


// Header subdivision has changed:- this is a bit tricky,
// we want to update the content size w/o re-measuring the items...
long IconList::onHeaderChanged(FXObject*,FXSelector,void* ptr)
{	
	// Obtain the main window width
	int width=(this->getShell())->getWidth();

	// Update the header relative sizes
	for (int hi=0; hi<getNumHeaders(); hi++)
		headerpct[hi]=(double)getHeaderSize(hi)/(double)width;
	
    flags&=~FLAG_RECALC;
    return 1;
}


// Header subdivision resize has been requested;
// we want to set the width of the header column
// to that of the widest item.
long IconList::onHeaderResize(FXObject*,FXSelector,void* ptr)
{
    register int hi=(int)(FXival)ptr;
    register int i,iw,tw,w,nw=0;
    FXString text;

    // For detailed icon list
    if (!(options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS)))
    {
        for (i=0; i<items.no(); i++)
        {
            w=0;

            // The first header item may have an icon
            if (hi==0)
            {
                if (items[i]->miniIcon)
                {
                    iw=items[i]->miniIcon->getWidth();
                    w+=iw+DETAIL_TEXT_SPACING+SIDE_SPACING/2;
                }
            }

            // Measure section of text
            text=items[i]->label.section('\t',hi);
            if (!text.empty())
            {
                tw=font->getTextWidth(text.text(),text.length());
                w+=tw+SIDE_SPACING+2;
            }

            // Keep the max
            if (w>nw)
            	nw=w;
        }

        // Set new header width
        if (nw>0 && nw!=header->getItemSize(hi))
        {
            header->setItemSize(hi,nw);
            flags&=~FLAG_RECALC;
        }
    }
    return 1;
}


// Set headers from array of strings
void IconList::setHeaders(const char** strings,int size)
{
    header->clearItems();
    header->fillItems(strings,NULL,size);
}


// Set headers from newline separated strings
void IconList::setHeaders(const FXString& strings,int size)
{
    header->clearItems();
    header->fillItems(strings,NULL,size);
}


// Append header caption
void IconList::appendHeader(const FXString& text,FXIcon *icon,int size)
{
    header->appendItem(text,icon,size);
}


// Remove header caption
void IconList::removeHeader(int index)
{
    if (index<0 || header->getNumItems()<=index)
    {
        fxerror("%s::removeHeader: index out of range.\n",getClassName());
    }
    header->removeItem(index);
}


// Change header caption
void IconList::setHeaderText(int index,const FXString& text)
{
    if (index<0 || header->getNumItems()<=index)
    {
        fxerror("%s::setHeaderText: index out of range.\n",getClassName());
    }
    header->setItemText(index,text);
}


// Get header caption
FXString IconList::getHeaderText(int index) const
{
    if (index<0 || header->getNumItems()<=index)
    {
        fxerror("%s::getHeaderText: index out of range.\n",getClassName());
    }
    return header->getItemText(index);
}


// Change header icon
void IconList::setHeaderIcon(int index,FXIcon *icon)
{
    if (index<0 || header->getNumItems()<=index)
    {
        fxerror("%s::setHeaderIcon: index out of range.\n",getClassName());
    }
    header->setItemIcon(index,icon);
}


// Get header icon
FXIcon* IconList::getHeaderIcon(int index) const
{
    if (index<0 || header->getNumItems()<=index)
    {
        fxerror("%s::getHeaderIcon: index out of range.\n",getClassName());
    }
    return header->getItemIcon(index);
}


// Change header size
void IconList::setHeaderSize(int index,int size)
{
    if (index<0 || header->getNumItems()<=index)
    {
        fxerror("%s::setHeaderSize: index out of range.\n",getClassName());
    }
    header->setItemSize(index,size);
}


// Get header size
int IconList::getHeaderSize(int index) const
{
    if (index<0 || header->getNumItems()<=index)
    {
        fxerror("%s::getHeaderSize: index out of range.\n",getClassName());
    }
    return header->getItemSize(index);
}


// Return number of headers
int IconList::getNumHeaders() const
{
    return header->getNumItems();
}


// Change item text
void IconList::setItemText(int index,const FXString& text)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::setItemText: index out of range.\n",getClassName());
    }
    if (items[index]->getText()!=text)
    {
        items[index]->setText(text);
        recalc();
    }
}


// Get item text
FXString IconList::getItemText(int index) const
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::getItemText: index out of range.\n",getClassName());
    }
    return items[index]->getText();
}


// Set item icon
void IconList::setItemBigIcon(int index,FXIcon* icon,FXbool owned)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::setItemBigIcon: index out of range.\n",getClassName());
    }
    if (items[index]->getBigIcon()!=icon)
    	recalc();
    items[index]->setBigIcon(icon,owned);
}


// Get item icon
FXIcon* IconList::getItemBigIcon(int index) const
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::getItemBigIcon: index out of range.\n",getClassName());
    }
    return items[index]->getBigIcon();
}


// Set item icon
void IconList::setItemMiniIcon(int index,FXIcon* icon,FXbool owned)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::setItemMiniIcon: index out of range.\n",getClassName());
    }
    if (items[index]->getMiniIcon()!=icon)
    	recalc();
    items[index]->setMiniIcon(icon,owned);
}


// Get item icon
FXIcon* IconList::getItemMiniIcon(int index) const
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::getItemMiniIcon: index out of range.\n",getClassName());
    }
    return items[index]->getMiniIcon();
}


// Set item data
void IconList::setItemData(int index,void* ptr)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::setItemData: index out of range.\n",getClassName());
    }
    items[index]->setData(ptr);
}


// Get item data
void* IconList::getItemData(int index) const
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::getItemData: index out of range.\n",getClassName());
    }
    return items[index]->getData();
}


// True if item is current
FXbool IconList::isItemCurrent(int index) const
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::isItemCurrent: index out of range.\n",getClassName());
    }
    return index==current;
}


// True if item is enabled
FXbool IconList::isItemEnabled(int index) const
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::isItemEnabled: index out of range.\n",getClassName());
    }
    return items[index]->isEnabled();
}


// True if item (partially) visible
FXbool IconList::isItemVisible(int index) const
{
    register FXbool vis=FALSE;
    register int x,y,hh;
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::isItemVisible: index out of range.\n",getClassName());
    }
    if (options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
    {
        if (options&_ICONLIST_COLUMNS)
        {
            FXASSERT(ncols>0);
            x=pos_x+itemSpace*(index%ncols);
            y=pos_y+itemHeight*(index/ncols);
        }
        else
        {
            FXASSERT(nrows>0);
            x=pos_x+itemSpace*(index/nrows);
            y=pos_y+itemHeight*(index%nrows);
        }
        if (0<x+itemSpace && x<viewport_w && 0<y+itemHeight && y<viewport_h)
        	vis=TRUE;
    }
    else
    {
        hh=header->getDefaultHeight();
        y=pos_y+hh+index*itemHeight;
        if (hh<y+itemHeight && y<viewport_h)
        	vis=TRUE;
    }
    return vis;
}


// Make item fully visible
void IconList::makeItemVisible(int index)
{
    register int x,y,hh,px,py;
    if (0<=index && index<items.no())
    {
        // Remember for later
        viewable=index;

        // Was realized
        if (xid)
        {
            // Force layout if dirty
            if (flags&FLAG_RECALC)
            	layout();

            px=pos_x;
            py=pos_y;

            // Showing icon view
            if (options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
            {
                if (options&_ICONLIST_COLUMNS)
                {
                    FXASSERT(ncols>0);
                    x=itemSpace*(index%ncols);
                    y=itemHeight*(index/ncols);
                }
                else
                {
                    FXASSERT(nrows>0);
                    x=itemSpace*(index/nrows);
                    y=itemHeight*(index%nrows);
                }
                if (px+x+itemSpace >= viewport_w)
                	px=viewport_w-x-itemSpace;
                if (px+x <= 0)
                	px=-x;
                if (py+y+itemHeight >= viewport_h)
                	py=viewport_h-y-itemHeight;
                if (py+y <= 0)
                	py=-y;
            }

            // Showing list view
            else
            {
                hh=header->getDefaultHeight();
                y=hh+index*itemHeight;
                if (py+y+itemHeight >= viewport_h+hh)
                	py=hh+viewport_h-y-itemHeight;
                if (py+y <= hh)
                	py=hh-y;
            }

            // Scroll into view
            setPosition(px,py);

            // Done it
            viewable=-1;
        }
    }
}



// Hack to avoid displaying the allowTooltip in detailed mode when the mouse is not on the first column
// Get item at position x,y
//int IconList::getItemAt(int x,int y) const
int IconList::getItemAt(int x,int y)
{
	register int ix,iy;
	register int r,c,index;
	y-=pos_y;
	x-=pos_x;
	
	// Update the allowTooltip variable
	allowTooltip=TRUE;
	if (options&_ICONLIST_STANDARD)
		allowTooltip=FALSE;

	if (!(options&_ICONLIST_STANDARD) && (single_click==SINGLE_CLICK_DIR_FILE) )
	{
		// Don't change cursor while the wait cursor is displayed
		if (::setWaitCursor(getApp(),QUERY_CURSOR)==0)
			setDefaultCursor(getApp()->getDefaultCursor(DEF_HAND_CURSOR));
	}
		
	if(options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
	{		
		c=x/itemSpace;
		r=y/itemHeight;
		
		if(c<0 || c>=ncols || r<0 || r>=nrows)
			return -1;

		index=(options&_ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;

		if(index<0 || index>=items.no())
			return -1;

		ix=itemSpace*c;
		iy=itemHeight*r;

		if(items[index]->hitItem(this,x-ix,y-iy)==0)
			return -1;
	}
	else
	{		
		// Update the allowTooltip variable
		if (x==0 || x>header->getItemSize(0))
		{
			allowTooltip=FALSE;		
			
			// Don't change cursor while the wait cursor is displayed
			if (::setWaitCursor(getApp(),QUERY_CURSOR)==0)
				setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
		}
		
		y-=header->getDefaultHeight();
		c=0;
		index=y/itemHeight;
		
		
		if(index<0 || index>=items.no())
			return -1;
	}
	return index;
}



// Compare strings up to n
static int comp(const FXString& s1,const FXString& s2,int n)
{
    register const unsigned char *p1=(const unsigned char *)s1.text();
    register const unsigned char *p2=(const unsigned char *)s2.text();
    register int c1,c2;
    if (0<n)
    {
        do
        {
            c1=*p1++;
            if (c1=='\t')
            	c1=0;
            c2=*p2++;
            if (c2=='\t')
            	c2=0;
        }
        while (--n && c1 && (c1==c2));
        return c1-c2;
    }
    return 0;
}


// Compare strings case insensitive up to n
static int compcase(const FXString& s1,const FXString& s2,int n)
{
    register const unsigned char *p1=(const unsigned char *)s1.text();
    register const unsigned char *p2=(const unsigned char *)s2.text();
    register int c1,c2;
    if (0<n)
    {
        do
        {
            c1=Ascii::toLower(*p1++);
            if (c1=='\t')
            	c1=0;     // FIXME UTF8 version
            c2=Ascii::toLower(*p2++);
            if (c2=='\t')
            	c2=0;
        }
        while (--n && c1 && (c1==c2));
        return c1-c2;
    }
    return 0;
}


typedef int (*FXCompareFunc)(const FXString&,const FXString&,int);


// Get item by name
int IconList::findItem(const FXString& text,int start,unsigned int flgs) const
{
    register FXCompareFunc comparefunc;
    register int index,len;
    if (0<items.no())
    {
        comparefunc=(flgs&SEARCH_IGNORECASE) ? (FXCompareFunc)compcase : (FXCompareFunc)comp;
        len=(flgs&SEARCH_PREFIX)?text.length():2147483647;
        if (flgs&SEARCH_BACKWARD)
        {
            if (start<0)
            	start=items.no()-1;
            for (index=start; 0<=index; index--)
            {
                if ((*comparefunc)(items[index]->getText(),text,len)==0)
                	return index;
            }
            if (!(flgs&SEARCH_WRAP))
            	return -1;
            for (index=items.no()-1; start<index; index--)
            {
                if ((*comparefunc)(items[index]->getText(),text,len)==0)
                	return index;
            }
        }
        else
        {
            if (start<0)
            	start=0;
            for (index=start; index<items.no(); index++)
            {
                if ((*comparefunc)(items[index]->getText(),text,len)==0)
                	return index;
            }
            if (!(flgs&SEARCH_WRAP))
            	return -1;
            for (index=0; index<start; index++)
            {
                if ((*comparefunc)(items[index]->getText(),text,len)==0)
                	return index;
            }
        }
    }
    return -1;
}


// Get item by data
int IconList::findItemByData(const void *ptr,int start,unsigned int flgs) const
{
    register int index;
    if (0<items.no())
    {
        if (flgs&SEARCH_BACKWARD)
        {
            if (start<0)
            	start=items.no()-1;
            for (index=start; 0<=index; index--)
            {
                if (items[index]->getData()==ptr)
                	return index;
            }
            if (!(flgs&SEARCH_WRAP))
            	return -1;
            for (index=items.no()-1; start<index; index--)
            {
                if (items[index]->getData()==ptr)
                	return index;
            }
        }
        else
        {
            if (start<0) start=0;
            for (index=start; index<items.no(); index++)
            {
                if (items[index]->getData()==ptr)
                	return index;
            }
            if (!(flgs&SEARCH_WRAP))
            	return -1;
            for (index=0; index<start; index++)
            {
                if (items[index]->getData()==ptr)
                	return index;
            }
        }
    }
    return -1;
}


// Did we hit the item, and which part of it did we hit
int IconList::hitItem(int index,int x,int y,int ww,int hh) const
{
    int ix,iy,r,c,hit=0;
    if (0<=index && index<items.no())
    {
        x-=pos_x;
        y-=pos_y;
        if (!(options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS)))
        	y-=header->getDefaultHeight();
        if (options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
        {
            if (options&_ICONLIST_COLUMNS)
            {
                r=index/ncols;
                c=index%ncols;
            }
            else
            {
                c=index/nrows;
                r=index%nrows;
            }
        }
        else
        {
            r=index;
            c=0;
        }
        ix=itemSpace*c;
        iy=itemHeight*r;
        hit=items[index]->hitItem(this,x-ix,y-iy,ww,hh);
    }
    return hit;
}


// Repaint
void IconList::updateItem(int index) const
{
    if (xid && 0<=index && index<items.no())
    {
        if (options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
        {
            if (options&_ICONLIST_COLUMNS)
            {
                FXASSERT(ncols>0);
                update(pos_x+itemSpace*(index%ncols),pos_y+itemHeight*(index/ncols),itemSpace,itemHeight);
            }
            else
            {
                FXASSERT(nrows>0);
                update(pos_x+itemSpace*(index/nrows),pos_y+itemHeight*(index%nrows),itemSpace,itemHeight);
            }
        }
        else
        {
            update(0,pos_y+header->getDefaultHeight()+index*itemHeight,width,itemHeight);
        }
    }
}


// Enable one item
FXbool IconList::enableItem(int index)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::enableItem: index out of range.\n",getClassName());
    }
    if (!items[index]->isEnabled())
    {
        items[index]->setEnabled(TRUE);
        updateItem(index);
        return TRUE;
    }
    return FALSE;
}


// Disable one item
FXbool IconList::disableItem(int index)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::disableItem: index out of range.\n",getClassName());
    }
    if (items[index]->isEnabled())
    {
        items[index]->setEnabled(FALSE);
        updateItem(index);
        return TRUE;
    }
    return FALSE;
}


// Select one item
FXbool IconList::selectItem(int index,FXbool notify)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::selectItem: index out of range.\n",getClassName());
    }
    if (!items[index]->isSelected())
    {
        switch (options&SELECT_MASK)
        {
        case _ICONLIST_SINGLESELECT:
        case _ICONLIST_BROWSESELECT:
            killSelection(notify);
        case _ICONLIST_EXTENDEDSELECT:
        case _ICONLIST_MULTIPLESELECT:
            items[index]->setSelected(TRUE);
            updateItem(index);
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);
            }
            break;
        }
        return TRUE;
    }
    return FALSE;
}


// Deselect one item
FXbool IconList::deselectItem(int index,FXbool notify)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::deselectItem: index out of range.\n",getClassName());
    }
    if (items[index]->isSelected())
    {
        switch (options&SELECT_MASK)
        {
        case _ICONLIST_EXTENDEDSELECT:
        case _ICONLIST_MULTIPLESELECT:
        case _ICONLIST_SINGLESELECT:
            items[index]->setSelected(FALSE);
            updateItem(index);
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)index);
            }
            break;
        }
        return TRUE;
    }
    return FALSE;
}


// Toggle one item
FXbool IconList::toggleItem(int index,FXbool notify)
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::toggleItem: index out of range.\n",getClassName());
    }
    switch (options&SELECT_MASK)
    {
    case _ICONLIST_BROWSESELECT:
        if (!items[index]->isSelected())
        {
            killSelection(notify);
            items[index]->setSelected(TRUE);
            updateItem(index);
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);
            }
        }
        break;
    case _ICONLIST_SINGLESELECT:
        if (!items[index]->isSelected())
        {
            killSelection(notify);
            items[index]->setSelected(TRUE);
            updateItem(index);
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);
            }
        }
        else
        {
            items[index]->setSelected(FALSE);
            updateItem(index);
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)index);
            }
        }
        break;
    case _ICONLIST_EXTENDEDSELECT:
    case _ICONLIST_MULTIPLESELECT:
        if (!items[index]->isSelected())
        {
            items[index]->setSelected(TRUE);
            updateItem(index);
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)index);
            }
        }
        else
        {
            items[index]->setSelected(FALSE);
            updateItem(index);
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)index);
            }
        }
        break;
    }
    return TRUE;
}


// Select items in rectangle
FXbool IconList::selectInRectangle(int x,int y,int w,int h,FXbool notify)
{
    register int r,c,index;
    register FXbool changed=FALSE;
    if (options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
    {
        for (r=0; r<nrows; r++)
        {
            for (c=0; c<ncols; c++)
            {
                index=(options&_ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;
                if (index<items.no())
                {
                    if (hitItem(index,x,y,w,h))
                    {
                        changed|=selectItem(index,notify);
                    }
                }
            }
        }
    }
    else
    {
        for (index=0; index<items.no(); index++)
        {
            if (hitItem(index,x,y,w,h))
            {
                changed|=selectItem(index,notify);
            }
        }
    }
    return changed;
}


// Extend selection
FXbool IconList::extendSelection(int index,FXbool notify)
{
    register FXbool changes=FALSE;
    int i1,i2,i3,i;
    if (0<=index && 0<=anchor && 0<=extent)
    {

        // Find segments
        i1=index;
        if (anchor<i1)
        {
            i2=i1;
            i1=anchor;
        }
        else
        {
            i2=anchor;
        }
        if (extent<i1)
        {
            i3=i2;
            i2=i1;
            i1=extent;
        }
        else if (extent<i2)
        {
            i3=i2;
            i2=extent;
        }
        else
        {
            i3=extent;
        }

        // First segment
        for (i=i1; i<i2; i++)
        {

            // item===extent---anchor
            // item===anchor---extent
            if (i1==index)
            {
                if (!items[i]->isSelected())
                {
                    items[i]->setSelected(TRUE);
                    updateItem(i);
                    changes=TRUE;
                    if (notify && target)
                    {
                        target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)i);
                    }
                }
            }

            // extent===anchor---item
            // extent===item-----anchor
            else if (i1==extent)
            {
                if (items[i]->isSelected())
                {
                    items[i]->setSelected(FALSE);
                    updateItem(i);
                    changes=TRUE;
                    if (notify && target)
                    {
                        target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)i);
                    }
                }
            }
        }

        // Second segment
        for (i=i2+1; i<=i3; i++)
        {

            // extent---anchor===item
            // anchor---extent===item
            if (i3==index)
            {
                if (!items[i]->isSelected())
                {
                    items[i]->setSelected(TRUE);
                    updateItem(i);
                    changes=TRUE;
                    if (notify && target)
                    {
                        target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)(FXival)i);
                    }
                }
            }

            // item-----anchor===extent
            // anchor---item=====extent
            else if (i3==extent)
            {
                if (items[i]->isSelected())
                {
                    items[i]->setSelected(FALSE);
                    updateItem(i);
                    changes=TRUE;
                    if (notify && target)
                    {
                        target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)i);
                    }
                }
            }
        }
        extent=index;
    }
    return changes;
}


// Kill selection
FXbool IconList::killSelection(FXbool notify)
{
    register FXbool changes=FALSE;
    register int i;
    for (i=0; i<items.no(); i++)
    {
        if (items[i]->isSelected())
        {
            items[i]->setSelected(FALSE);
            updateItem(i);
            changes=TRUE;
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)(FXival)i);
            }
        }
    }
    return changes;
}


// Lasso changed, so select/unselect items based on difference between new and old lasso box
void IconList::lassoChanged(int ox,int oy,int ow,int oh,int nx,int ny,int nw,int nh,FXbool notify)
{
    register int r,c;
    int ohit,nhit,index;
    if (options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
    {
        for (r=0; r<nrows; r++)
        {
            for (c=0; c<ncols; c++)
            {
                index=(options&_ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;
                if (index<items.no())
                {
                    ohit=hitItem(index,ox,oy,ow,oh);
                    nhit=hitItem(index,nx,ny,nw,nh);
                    if (ohit && !nhit)      // In old rectangle and not in new rectangle
                    {
                        deselectItem(index,notify);
                    }
                    else if (!ohit && nhit) // Not in old rectangle and in new rectangle
                    {
                        selectItem(index,notify);
                    }
                }
            }
        }
    }
    else
    {
        for (index=0; index<items.no(); index++)
        {
            ohit=hitItem(index,ox,oy,ow,oh);
            nhit=hitItem(index,nx,ny,nw,nh);
            if (ohit && !nhit)          // Was in old, not in new
            {
                deselectItem(index,notify);
            }
            else if (!ohit && nhit)     // Not in old, but in new
            {
                selectItem(index,notify);
            }
        }
    }
}


// Update value from a message
long IconList::onCmdSetValue(FXObject*,FXSelector,void* ptr)
{
    setCurrentItem((int)(FXival)ptr);
    return 1;
}


// Obtain value from list
long IconList::onCmdGetIntValue(FXObject*,FXSelector,void* ptr)
{
    *((int*)ptr)=getCurrentItem();
    return 1;
}


// Update value from a message
long IconList::onCmdSetIntValue(FXObject*,FXSelector,void* ptr)
{
    setCurrentItem(*((int*)ptr));
    return 1;
}


// Start motion timer while in this window
long IconList::onEnter(FXObject* sender,FXSelector sel,void* ptr)
{
    FXScrollArea::onEnter(sender,sel,ptr);
    getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());
    cursor=-1;
    return 1;
}


// Stop motion timer when leaving window
long IconList::onLeave(FXObject* sender,FXSelector sel,void* ptr)
{
    FXScrollArea::onLeave(sender,sel,ptr);
    getApp()->removeTimeout(this,ID_TIPTIMER);
    cursor=-1;
    return 1;
}


// We timed out, i.e. the user didn't move for a while
long IconList::onTipTimer(FXObject*,FXSelector,void*)
{
    flags|=FLAG_TIP;
    return 1;
}



// Hack to display more informations in the tool tip
long IconList::onQueryTip(FXObject* sender,FXSelector sel,void* ptr)
{    
	if (FXWindow::onQueryTip(sender,sel,ptr))
        return 1;
    
	// File tooltips are optional
	if (file_tooltips)
	{
		// In detailed mode, avoid displaying the tooltip when the mouse is not on the first column
		if (allowTooltip && (flags&FLAG_TIP) && (0<=cursor))
		{
			FXString string;

			// Get the item text
			FXString str=items[cursor]->getText();
		
			// Add name, size, type, permissions, etc. to the tool tip
			FXString name=str.section('\t',0);
			FXString size=str.section('\t',1);
			FXString type=str.section('\t',2);
			FXString date=str.section('\t',4);
			FXString user=str.section('\t',5);
			FXString group=str.section('\t',6);
			FXString perms=str.section('\t',7);
			FXString origpath=str.section('\t',8);
			FXString deldate=str.section('\t',9);
			FXString pathname=str.section('\t',10);

			// Don't display tooltip for the dotdot directory
			if (name=="..")
				string="";
			else
			{
				// Folder or mount point
				if (type==_("Folder") || type==_("Mount point"))
				{
					// Compute root file size
					FXulong dnsize;
					char dsize[64];
					dnsize=::dirsize(pathname.text());
					snprintf(dsize,sizeof(dsize)-1,"%llu",dnsize);
					size=::hSize(dsize);
					
					if (deldate.empty())
						string=_("Name: ")+name+"\n"+_("Size in root: ")+size+"\n"+_("Type: ")+type+"\n"
							   +_("Modified date: ")+date+"\n"+_("User: ")+user+" - "+_("Group: ")+group+"\n"
							   +_("Permissions: ")+perms;
					else
						string=_("Name: ")+name+"\n"+
							   +_("Original path: ")+origpath+"\n"
							   +_("Size in root: ")+size+"\n"+_("Type: ")+type+"\n"
							   +_("Modified date: ")+date+"\n"
							   +_("Deletion date: ")+deldate+"\n"
							   +_("User: ")+user+" - "+_("Group: ")+group+"\n"+_("Permissions: ")+perms;
				}
				// Regular file
				else
				{
					if (deldate.empty())
						string=_("Name: ")+name+"\n"+_("Size: ")+size+"\n"+_("Type: ")+type+"\n"
							   +_("Modified date: ")+date+"\n"+_("User: ")+user+" - "+_("Group: ")+group
							   +"\n"+_("Permissions: ")+perms;
					else
						string=_("Name: ")+name+"\n"+
						       +_("Original path: ")+origpath+"\n"
							   +_("Size: ")+size+"\n"+_("Type: ")+type
							   +"\n"+_("Modified date: ")+date+"\n"
							   +_("Deletion date: ")+deldate+"\n"
							   +_("User: ")+user+" - "+_("Group: ")+group+"\n"+_("Permissions: ")+perms;
						
				}
			}
			sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&string);
			return 1;
		}
		else if(!allowTooltip)
		{
			// Don't change cursor while the wait cursor is displayed
			if (::setWaitCursor(getApp(),QUERY_CURSOR)==0)
				setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
		}
	}

    return 0;
}


// We were asked about status text
long IconList::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr)
{
    if (FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
    if ((flags&FLAG_HELP) && !help.empty())
    {
        sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
        return 1;
    }
    return 0;
}


// Gained focus
long IconList::onFocusIn(FXObject* sender,FXSelector sel,void* ptr)
{
    FXScrollArea::onFocusIn(sender,sel,ptr);
    if (0<=current)
    {
        FXASSERT(current<items.no());
        items[current]->setFocus(TRUE);
        updateItem(current);
    }
    return 1;
}


// Lost focus
long IconList::onFocusOut(FXObject* sender,FXSelector sel,void* ptr)
{
    FXScrollArea::onFocusOut(sender,sel,ptr);
    if (0<=current)
    {
        FXASSERT(current<items.no());
        items[current]->setFocus(FALSE);
        updateItem(current);
    }
    return 1;
}


// Draw item list
long IconList::onPaint(FXObject*,FXSelector,void* ptr)
{
    register int rlo,rhi,clo,chi,yy,xx;
    register int x,y,r,c,index;
    FXEvent* event=(FXEvent*)ptr;
    FXDCWindow dc(this,event);

    // Set font
    dc.setFont(font);

    // Icon mode
    if (options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS))
    {

        // Exposed rows
        rlo=(event->rect.y-pos_y)/itemHeight;
        rhi=(event->rect.y+event->rect.h-pos_y)/itemHeight;
        if (rlo<0)
        	rlo=0;
        if (rhi>=nrows)
        	rhi=nrows-1;

        // Exposed columns
        clo=(event->rect.x-pos_x)/itemSpace;
        chi=(event->rect.x+event->rect.w-pos_x)/itemSpace;
        if (clo<0)
        	clo=0;
        if (chi>=ncols)
        	chi=ncols-1;

        // Big Icons
        if (options&_ICONLIST_BIG_ICONS)
        {
            for (r=rlo; r<=rhi; r++)
            {
                y=pos_y+r*itemHeight;
                for (c=clo; c<=chi; c++)
                {
                    x=pos_x+c*itemSpace;
                    index=(options&_ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;					
					dc.setForeground(backColor);
                    dc.fillRectangle(x,y,itemSpace,itemHeight);
                    if (index<items.no())
                    {
                        items[index]->draw(this,dc,x,y,itemSpace,itemHeight);
                    }
                }
            }
        }

        // Mini icons
        else
        {
            for (r=rlo; r<=rhi; r++)
            {
                y=pos_y+r*itemHeight;
                for (c=clo; c<=chi; c++)
                {
                    x=pos_x+c*itemSpace;
                    index=(options&_ICONLIST_COLUMNS) ? ncols*r+c : nrows*c+r;
                    dc.setForeground(backColor);
                    dc.fillRectangle(x,y,itemSpace,itemHeight);
                    if (index<items.no())
                    {
                        items[index]->draw(this,dc,x,y,itemSpace,itemHeight);
                    }
                }
            }
        }

        // Repaint left-over background
        yy=(rhi+1)*itemHeight;
        if (yy<event->rect.y+event->rect.h)
        {
            dc.setForeground(backColor);
            dc.fillRectangle(event->rect.x,yy,event->rect.w,event->rect.y+event->rect.h-yy);
        }
        xx=(chi+1)*itemSpace;
        if (xx<event->rect.x+event->rect.w)
        {
            dc.setForeground(backColor);
            dc.fillRectangle(xx,event->rect.y,event->rect.x+event->rect.w-xx,event->rect.h);
        }
    }

    // Detailed mode
    else
    {				
		// Exposed rows        
		rlo=(event->rect.y-pos_y-header->getDefaultHeight())/itemHeight;
        rhi=(event->rect.y+event->rect.h-pos_y-header->getDefaultHeight())/itemHeight;
        if (rlo<0)
        	rlo=0;
        if (rhi>=items.no())
        	rhi=items.no()-1;

        // Repaint the items
        y=pos_y+rlo*itemHeight+header->getDefaultHeight();
        for (index=rlo; index<=rhi; index++,y+=itemHeight)
        {
            dc.setForeground(backColor);
            dc.fillRectangle(0,y,width,itemHeight);
            items[index]->draw(this,dc,pos_x,y,width,itemHeight);
        }

		// Repaint left-over background
        if (y<event->rect.y+event->rect.h)
        {
            dc.setForeground(backColor);
            dc.fillRectangle(event->rect.x,y,event->rect.w,event->rect.y+event->rect.h-y);
        }
    }

    return 1;
}


// Draw Lasso rectangle
void IconList::drawLasso(int x0,int y0,int x1,int y1)
{
    FXDCWindow dc(this);
    dc.setFunction(BLT_NOT_DST);
    x0+=pos_x;
    x1+=pos_x;
    y0+=pos_y;
    y1+=pos_y;
    dc.drawLine(x0,y0,x1,y0);
    dc.drawLine(x1,y0,x1,y1);
    dc.drawLine(x1,y1,x0,y1);
    dc.drawLine(x0,y1,x0,y0);
}


// Arrange by rows
long IconList::onCmdArrangeByRows(FXObject*,FXSelector,void*)
{
    options&=~_ICONLIST_COLUMNS;
    recalc();
    return 1;
}


// Update sender
long IconList::onUpdArrangeByRows(FXObject* sender,FXSelector,void*)
{
    sender->handle(this,(options&_ICONLIST_COLUMNS)?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    sender->handle(this,(options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS))?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    return 1;
}


// Arrange by columns
long IconList::onCmdArrangeByColumns(FXObject*,FXSelector,void*)
{
    options|=_ICONLIST_COLUMNS;
    recalc();
    return 1;
}


// Update sender
long IconList::onUpdArrangeByColumns(FXObject* sender,FXSelector,void*)
{
    sender->handle(this,(options&_ICONLIST_COLUMNS)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    sender->handle(this,(options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS))?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    return 1;
}


// Toggle autosize items
long IconList::onCmdToggleAutosize(FXObject*,FXSelector,void*)
{
	//autosize=!autosize;
	//if (autosize)
	if (options&_ICONLIST_AUTOSIZE)
     	options&=~_ICONLIST_AUTOSIZE;
    else
    	options|=_ICONLIST_AUTOSIZE;
    recalc();
    return 1;
}


// Update sender
long IconList::onUpdToggleAutosize(FXObject* sender,FXSelector,void*)
{
    sender->handle(this,(options&_ICONLIST_AUTOSIZE)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    sender->handle(this,(options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS))?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    return 1;
}


// Show detailed list
long IconList::onCmdShowDetails(FXObject*,FXSelector,void*)
{
    options&=~_ICONLIST_MINI_ICONS;
    options&=~_ICONLIST_BIG_ICONS;
    recalc();
    return 1;
}


// Update sender
long IconList::onUpdShowDetails(FXObject* sender,FXSelector,void*)
{
    sender->handle(this,(options&(_ICONLIST_MINI_ICONS|_ICONLIST_BIG_ICONS))?FXSEL(SEL_COMMAND,ID_UNCHECK):FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    return 1;
}


// Show big icons
long IconList::onCmdShowBigIcons(FXObject*,FXSelector,void*)
{
    options&=~_ICONLIST_MINI_ICONS;
    options|=_ICONLIST_BIG_ICONS;
    recalc();
    return 1;
}


// Update sender
long IconList::onUpdShowBigIcons(FXObject* sender,FXSelector,void*)
{
    sender->handle(this,(options&_ICONLIST_BIG_ICONS)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Show small icons
long IconList::onCmdShowMiniIcons(FXObject*,FXSelector,void*)
{
    options|=_ICONLIST_MINI_ICONS;
    options&=~_ICONLIST_BIG_ICONS;
    recalc();
    return 1;
}


// Update sender
long IconList::onUpdShowMiniIcons(FXObject* sender,FXSelector,void*)
{
    sender->handle(this,(options&_ICONLIST_MINI_ICONS)?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Select all items
long IconList::onCmdSelectAll(FXObject*,FXSelector,void*)
{
    for (int i=0; i<items.no(); i++) selectItem(i,TRUE);
	if (!(options&_ICONLIST_SEARCH) && !(options&_ICONLIST_STANDARD))
		deselectItem(0,TRUE);
    return 1;
}


// Deselect all items
long IconList::onCmdDeselectAll(FXObject*,FXSelector,void*)
{
    for (int i=0; i<items.no(); i++) deselectItem(i,TRUE);
    return 1;
}


// Select inverse of current selection
long IconList::onCmdSelectInverse(FXObject*,FXSelector,void*)
{
    for (int i=0; i<items.no(); i++) toggleItem(i,TRUE);
	if (!(options&_ICONLIST_SEARCH) && !(options&_ICONLIST_STANDARD))
		deselectItem(0,TRUE);
   return 1;
}


// Compare sectioned strings
int IconList::compareSection(const char *p,const char* q,int s)
{
    register int c1,c2,x;
    for (x=s; x && *p; x-=(*p++=='\t'));
    for (x=s; x && *q; x-=(*q++=='\t'));
    do
    {
        c1=(unsigned char)(*p++);
        c2=(unsigned char)(*q++);
    }
    while ('\t'<c1 && (c1==c2));
    return c1-c2;
}


// Compare sectioned strings, case-insensitive
int IconList::compareSectionCase(const char *p,const char* q,int s)
{
    register int c1,c2,x;
    for (x=s; x && *p; x-=(*p++=='\t'));
    for (x=s; x && *q; x-=(*q++=='\t'));
    do
    {
        if ((*p & 0x80) && (*q & 0x80))
        {
            c1=Unicode::toLower(wc(p));
            p+=wclen(p);
            c2=Unicode::toLower(wc(q));
            q+=wclen(q);
        }
        else
        {
            c1=Ascii::toLower(*p);
            p+=1;
            c2=Ascii::toLower(*q);
            q+=1;
        }
    }
    while ('\t'<c1 && (c1==c2));
    return c1-c2;
}


// Sort items in ascending order
int IconList::ascending(const IconItem* a,const IconItem* b)
{
    return compareSection(a->getText().text(),b->getText().text(),0);
}


// Sort items in descending order
int IconList::descending(const IconItem* a,const IconItem* b)
{
    return compareSection(b->getText().text(),a->getText().text(),0);
}


// Sort items in ascending order, case insensitive
int IconList::ascendingCase(const IconItem* a,const IconItem* b)
{
    return compareSectionCase(a->getText().text(),b->getText().text(),0);
}


// Sort items in descending order, case insensitive
int IconList::descendingCase(const IconItem* a,const IconItem* b)
{
    return compareSectionCase(b->getText().text(),a->getText().text(),0);
}


// Sort the items based on the sort function
void IconList::sortItems()
{
    register IconItem *v,*c=0;
    register FXbool exch=FALSE;
    register int i,j,h;
    if (sortfunc)
    {
        if (0<=current)
        {
            c=items[current];
        }
        for (h=1; h<=items.no()/9; h=3*h+1);
        for (; h>0; h/=3)
        {
            for (i=h+1;i<=items.no();i++)
            {
                v=items[i-1];
                j=i;
                while (j>h && sortfunc(items[j-h-1],v)>0)
                {
                    items[j-1]=items[j-h-1];
                    exch=TRUE;
                    j-=h;
                }
                items[j-1]=v;
            }
        }
        if (0<=current)
        {
            for (i=0; i<items.no(); i++)
            {
                if (items[i]==c)
                {
                    current=i;
                    break;
                }
            }
        }
        if (exch)
        	recalc();
    }
}


// Set current item
void IconList::setCurrentItem(int index,FXbool notify)
{
    if (index<-1 || items.no()<=index)
    {
        fxerror("%s::setCurrentItem: index out of range.\n",getClassName());
    }
    if (index!=current)
    {

        // Deactivate old item
        if (0<=current)
        {

            // No visible change if it doen't have the focus
            if (hasFocus())
            {
                items[current]->setFocus(FALSE);
                updateItem(current);
            }
        }

        current=index;

        // Activate new item
        if (0<=current)
        {

            // No visible change if it doen't have the focus
            if (hasFocus())
            {
                items[current]->setFocus(TRUE);
                updateItem(current);
            }
        }

        // Notify item change
        if (notify && target)
        {
            target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);
        }
    }

    // In browse selection mode, select item
    if ((options&SELECT_MASK)==_ICONLIST_BROWSESELECT && 0<=current && items[current]->isEnabled())
    {
        selectItem(current,notify);
    }
}


// Set anchor item
void IconList::setAnchorItem(int index)
{
    if (index<-1 || items.no()<=index)
    {
        fxerror("%s::setAnchorItem: index out of range.\n",getClassName());
    }
    anchor=index;
    extent=index;
}


// Zero out lookup string
long IconList::onLookupTimer(FXObject*,FXSelector,void*)
{
    lookup=FXString::null;
    return 1;
}


long IconList::onKeyPress(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    int index=current;
    flags&=~FLAG_TIP;
    if (!isEnabled())
        return 0;
    if (target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr))
        return 1;
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
        lookup=FXString::null;
        setPosition(pos_x,pos_y+verticalScrollBar()->getPage());
		// To select an item in the current page
		index-=(int)(verticalScrollBar()->getPage()/verticalScrollBar()->getLine());
		goto hop;
        return 1;
    case KEY_Page_Down:
    case KEY_KP_Page_Down:
        lookup=FXString::null;
        setPosition(pos_x,pos_y-verticalScrollBar()->getPage());
		// To select an item in the current page
		index+=(int)(verticalScrollBar()->getPage()/verticalScrollBar()->getLine());
		goto hop;
		return 1;
    case KEY_Right:
    case KEY_KP_Right:
        if (!(options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS)))
        {
            setPosition(pos_x-10,pos_y);
            return 1;
        }
        if (options&_ICONLIST_COLUMNS)
            index+=1;
        else
            index+=nrows;
        goto hop;
    case KEY_Left:
    case KEY_KP_Left:
        if (!(options&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS)))
        {
            setPosition(pos_x+10,pos_y);
            return 1;
        }
        if (options&_ICONLIST_COLUMNS)
            index-=1;
        else
            index-=nrows;
        goto hop;
    case KEY_Up:
    case KEY_KP_Up:
        if (options&_ICONLIST_COLUMNS)
            index-=ncols;
        else
            index-=1;
        goto hop;
    case KEY_Down:
    case KEY_KP_Down:
        if (options&_ICONLIST_COLUMNS)
            index+=ncols;
        else
            index+=1;
        goto hop;
    case KEY_Home:
    case KEY_KP_Home:
        index=0;
        goto hop;
    case KEY_End:
    case KEY_KP_End:
        index=items.no()-1;
hop:
        lookup=FXString::null;
        if (0<=index && index<items.no())
        {
            setCurrentItem(index,TRUE);
            makeItemVisible(index);
            if (items[index]->isEnabled())
            {
                if ((options&SELECT_MASK)==_ICONLIST_EXTENDEDSELECT)
                {
                    if (event->state&SHIFTMASK)
                    {
                        if (0<=anchor)
                        {
                            selectItem(anchor,TRUE);
                            extendSelection(index,TRUE);
                        }
                        else
                            selectItem(index,TRUE);
                    }
                    else if (!(event->state&CONTROLMASK))
                    {
                        killSelection(TRUE);
                        selectItem(index,TRUE);
                        setAnchorItem(index);
                    }
                }
            }
        }

        // Commented out to allow single click navigation in the FileList
        //handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);

        if (0<=current && items[current]->isEnabled())
            handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
        return 1;
    case KEY_space:
    case KEY_KP_Space:
        lookup=FXString::null;
        if (0<=current && items[current]->isEnabled())
        {
            switch (options&SELECT_MASK)
            {
            case _ICONLIST_EXTENDEDSELECT:
                if (event->state&SHIFTMASK)
                {
                    if (0<=anchor)
                    {
                        selectItem(anchor,TRUE);
                        extendSelection(current,TRUE);
                    }
                    else
                        selectItem(current,TRUE);
                }
                else if (event->state&CONTROLMASK)
                    toggleItem(current,TRUE);
                else
                {
                    killSelection(TRUE);
                    selectItem(current,TRUE);
                }
                break;
            case _ICONLIST_MULTIPLESELECT:
            case _ICONLIST_SINGLESELECT:
                toggleItem(current,TRUE);
                break;
            }
            setAnchorItem(current);
        }
        handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);
        if (0<=current && items[current]->isEnabled())
            handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
        return 1;
    case KEY_Return:
    case KEY_KP_Enter:
        lookup=FXString::null;
        handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)(FXival)current);
        if (0<=current && items[current]->isEnabled())
            handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
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
		
		// String lookup can be case insensitive now
		if (ignorecase)
			index=findItem(lookup,current,SEARCH_FORWARD|SEARCH_WRAP|SEARCH_PREFIX|SEARCH_IGNORECASE);
		else
			index=findItem(lookup,current,SEARCH_FORWARD|SEARCH_WRAP|SEARCH_PREFIX);
		
        if (0<=index)
        {
            setCurrentItem(index,TRUE);
            makeItemVisible(index);
            if (items[index]->isEnabled())
            {
                if ((options&SELECT_MASK)==_ICONLIST_EXTENDEDSELECT)
                {
                    killSelection(TRUE);
                    selectItem(index,TRUE);
                }
                setAnchorItem(index);
            }
        }
        handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);
        if (0<=current && items[current]->isEnabled())
            handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
        return 1;
    }
    return 0;
}


// Key Release
long IconList::onKeyRelease(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    if (!isEnabled())
    	return 0;
    if (target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr))
    	return 1;
    switch (event->code)
    {
    case KEY_Shift_L:
    case KEY_Shift_R:
    case KEY_Control_L:
    case KEY_Control_R:
    case KEY_Alt_L:
    case KEY_Alt_R:
        if (flags&FLAG_DODRAG)
        {
            handle(this,FXSEL(SEL_DRAGGED,0),ptr);
        }
        return 1;
    }
    return 0;
}


// Autoscrolling timer
long IconList::onAutoScroll(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    int olx,orx,oty,oby,nlx,nrx,nty,nby;

    // Lasso mode
    if (flags&FLAG_LASSO)
    {

        // Hide the lasso before scrolling
        drawLasso(anchorx,anchory,currentx,currenty);

        // Scroll the content
        FXScrollArea::onAutoScroll(sender,sel,ptr);

        // Select items in lasso
        FXMINMAX(olx,orx,anchorx,currentx);
        FXMINMAX(oty,oby,anchory,currenty);
        currentx=event->win_x-pos_x;
        currenty=event->win_y-pos_y;
        FXMINMAX(nlx,nrx,anchorx,currentx);
        FXMINMAX(nty,nby,anchory,currenty);
        lassoChanged(pos_x+olx,pos_y+oty,orx-olx+1,oby-oty+1,pos_x+nlx,pos_y+nty,nrx-nlx+1,nby-nty+1,TRUE);

        // Force repaint on this window
        repaint();

        // Show lasso again
        drawLasso(anchorx,anchory,currentx,currenty);
        return 1;
    }

    // Scroll the content
    FXScrollArea::onAutoScroll(sender,sel,ptr);

    // Content scrolled, so perhaps something else under cursor
    if (flags&FLAG_DODRAG)
    {
        handle(this,FXSEL(SEL_DRAGGED,0),ptr);
        return 1;
    }

    return 0;
}


// Mouse moved
long IconList::onMotion(FXObject*,FXSelector,void* ptr)
{
    int olx,orx,oty,oby,nlx,nrx,nty,nby;
    FXEvent* event=(FXEvent*)ptr;
    int oldcursor=cursor;
    unsigned int flg=flags;

    // Kill the tip
    flags&=~FLAG_TIP;

    // Kill the tip timer
    getApp()->removeTimeout(this,ID_TIPTIMER);

    // Right mouse scrolling
    if (flags&FLAG_SCROLLING)
    {
        setPosition(event->win_x-grabx,event->win_y-graby);
        return 1;
    }

    // Lasso selection mode
    if (flags&FLAG_LASSO)
    {
        if (startAutoScroll(event,FALSE))
        	return 1;

        // Hide lasso
        drawLasso(anchorx,anchory,currentx,currenty);

        // Select items in lasso
        FXMINMAX(olx,orx,anchorx,currentx);
        FXMINMAX(oty,oby,anchory,currenty);
        currentx=event->win_x-pos_x;
        currenty=event->win_y-pos_y;
        FXMINMAX(nlx,nrx,anchorx,currentx);
        FXMINMAX(nty,nby,anchory,currenty);
        lassoChanged(pos_x+olx,pos_y+oty,orx-olx+1,oby-oty+1,pos_x+nlx,pos_y+nty,nrx-nlx+1,nby-nty+1,TRUE);

        // Force repaint on this window
        repaint();

        // Show lasso again
        drawLasso(anchorx,anchory,currentx,currenty);
        return 1;
    }

    // Drag and drop mode
    if (flags&FLAG_DODRAG)
    {
        if (startAutoScroll(event,TRUE))
        	return 1;
        handle(this,FXSEL(SEL_DRAGGED,0),ptr);
        return 1;
    }

    // Tentative drag and drop
    if (flags&FLAG_TRYDRAG)
    {
        if (event->moved)
        {
            flags&=~FLAG_TRYDRAG;
            if (handle(this,FXSEL(SEL_BEGINDRAG,0),ptr))
            {
                flags|=FLAG_DODRAG;
            }
        }
        return 1;
    }

    // Reset tip timer if nothing's going on
    getApp()->addTimeout(this,ID_TIPTIMER,getApp()->getMenuPause());

    // Get item we're over
    cursor=getItemAt(event->win_x,event->win_y);

    // Force GUI update only when needed
    return (cursor!=oldcursor)||(flg&FLAG_TIP);
}


// Pressed a button
long IconList::onLeftBtnPress(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    int index,code;
    flags&=~FLAG_TIP;
    handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
    if (isEnabled())
    {
        grab();
        flags&=~FLAG_UPDATE;

        // First change callback
        if (target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr))
        	return 1;

        // Locate item
        index=getItemAt(event->win_x,event->win_y);

        // No item
        if (index<0)
        {
            // Start lasso
            if ((options&SELECT_MASK)==_ICONLIST_EXTENDEDSELECT)
            {
                // Kill selection
                if (!(event->state&(SHIFTMASK|CONTROLMASK)))
                {
                    killSelection(TRUE);
                }

                anchorx=currentx=event->win_x-pos_x;
                anchory=currenty=event->win_y-pos_y;
                drawLasso(anchorx,anchory,currentx,currenty);
                flags|=FLAG_LASSO;
            }
            return 1;
        }

        // Find out where hit
        code=hitItem(index,event->win_x,event->win_y);

        // Change current item
        setCurrentItem(index,TRUE);

        // Change item selection
        state=items[index]->isSelected();
        switch (options&SELECT_MASK)
        {
        case _ICONLIST_EXTENDEDSELECT:
            if (event->state&SHIFTMASK)
            {
                if (0<=anchor)
                {
                    if (items[anchor]->isEnabled())
                    	selectItem(anchor,TRUE);
                    extendSelection(index,TRUE);
                }
                else
                {
                    if (items[index]->isEnabled())
                    	selectItem(index,TRUE);
                    setAnchorItem(index);
                }
            }
            else if (event->state&CONTROLMASK)
            {
                if (items[index]->isEnabled() && !state)
                	selectItem(index,TRUE);
                setAnchorItem(index);
            }
            else
            {
                if (items[index]->isEnabled() && !state)
                {
                    killSelection(TRUE);
                    selectItem(index,TRUE);
                }
                setAnchorItem(index);
            }
            break;
        case _ICONLIST_MULTIPLESELECT:
        case _ICONLIST_SINGLESELECT:
            if (items[index]->isEnabled() && !state)
            	selectItem(index,TRUE);
            break;
        }

        // Are we dragging?
        if (code && items[index]->isSelected() && items[index]->isDraggable())
        {
            flags|=FLAG_TRYDRAG;
        }

        flags|=FLAG_PRESSED;
        return 1;
    }
    return 0;
}


// Released button
long IconList::onLeftBtnRelease(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    unsigned int flg=flags;
    if (isEnabled())
    {
        ungrab();
        stopAutoScroll();
        flags|=FLAG_UPDATE;
        flags&=~(FLAG_PRESSED|FLAG_TRYDRAG|FLAG_LASSO|FLAG_DODRAG);

        // First chance callback
        if (target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr))
        	return 1;

        // Was lassoing
        if (flg&FLAG_LASSO)
        {
            drawLasso(anchorx,anchory,currentx,currenty);
            return 1;
        }

        // Was dragging
        if (flg&FLAG_DODRAG)
        {
            handle(this,FXSEL(SEL_ENDDRAG,0),ptr);
            return 1;
        }

        // Must have pressed
        if (flg&FLAG_PRESSED)
        {
            // Selection change
            switch (options&SELECT_MASK)
            {
            case _ICONLIST_EXTENDEDSELECT:
                if (0<=current && items[current]->isEnabled())
                {
                    if (event->state&CONTROLMASK)
                    {
                        if (state) deselectItem(current,TRUE);
                    }
                    else if (!(event->state&SHIFTMASK))
                    {
                        if (state)
                        {
                            killSelection(TRUE);
                            selectItem(current,TRUE);
                        }
                    }
                }
                break;
            case _ICONLIST_MULTIPLESELECT:
            case _ICONLIST_SINGLESELECT:
                if (0<=current && items[current]->isEnabled())
                {
                    if (state)
                    	deselectItem(current,TRUE);
                }
                break;
            }

            // Scroll to make item visible
            makeItemVisible(current);

            // Update anchor
            setAnchorItem(current);

            // Generate clicked callbacks
            if (event->click_count==1)
            {
                handle(this,FXSEL(SEL_CLICKED,0),(void*)(FXival)current);
            }
            else if (event->click_count==2)
            {
                handle(this,FXSEL(SEL_DOUBLECLICKED,0),(void*)(FXival)current);
            }
            else if (event->click_count==3)
            {
                handle(this,FXSEL(SEL_TRIPLECLICKED,0),(void*)(FXival)current);
            }

            // Command callback only when clicked on item
            if (0<=current && items[current]->isEnabled())
            {
                handle(this,FXSEL(SEL_COMMAND,0),(void*)(FXival)current);
            }
        }
        return 1;
    }
    return 0;
}


// Pressed right button
long IconList::onRightBtnPress(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    flags&=~FLAG_TIP;
    handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
    if (isEnabled())
    {
        grab();
        flags&=~FLAG_UPDATE;
        if (target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONPRESS,message),ptr))
        	return 1;
        flags|=FLAG_SCROLLING;
        grabx=event->win_x-pos_x;
        graby=event->win_y-pos_y;
        return 1;
    }
    return 0;
}


// Released right button
long IconList::onRightBtnRelease(FXObject*,FXSelector,void* ptr)
{
    if (isEnabled())
    {
        ungrab();
        flags&=~FLAG_SCROLLING;
        flags|=FLAG_UPDATE;
        if (target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONRELEASE,message),ptr))
        	return 1;
        return 1;
    }
    return 0;
}


// The widget lost the grab for some reason
long IconList::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr)
{
    FXScrollArea::onUngrabbed(sender,sel,ptr);
    flags&=~(FLAG_DODRAG|FLAG_LASSO|FLAG_TRYDRAG|FLAG_PRESSED|FLAG_CHANGED|FLAG_SCROLLING);
    flags|=FLAG_UPDATE;
    stopAutoScroll();
    return 1;
}


// Command message
long IconList::onCommand(FXObject*,FXSelector,void* ptr)
{
    return target && target->tryHandle(this,FXSEL(SEL_COMMAND,message),ptr);
}


// Clicked in list
long IconList::onClicked(FXObject*,FXSelector,void* ptr)
{
    return target && target->tryHandle(this,FXSEL(SEL_CLICKED,message),ptr);
}


// Double Clicked in list; ptr may or may not point to an item
long IconList::onDoubleClicked(FXObject*,FXSelector,void* ptr)
{
    return target && target->tryHandle(this,FXSEL(SEL_DOUBLECLICKED,message),ptr);
}


// Triple Clicked in list; ptr may or may not point to an item
long IconList::onTripleClicked(FXObject*,FXSelector,void* ptr)
{
    return target && target->tryHandle(this,FXSEL(SEL_TRIPLECLICKED,message),ptr);
}


// Create custom item
IconItem *IconList::createItem(const FXString& text,FXIcon *big,FXIcon* mini,void* ptr)
{
    return new IconItem(text,big,mini,ptr);
}


// Retrieve item
IconItem *IconList::getItem(int index) const
{
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::getItem: index out of range.\n",getClassName());
    }
    return items[index];
}


// Replace item with another
int IconList::setItem(int index,IconItem* item,FXbool notify)
{

    // Must have item
    if (!item)
    {
        fxerror("%s::setItem: item is NULL.\n",getClassName());
    }

    // Must be in range
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::setItem: index out of range.\n",getClassName());
    }

    // Notify item will be replaced
    if (notify && target)
    {
        target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)(FXival)index);
    }

    // Copy the state over
    item->state=items[index]->state;

    // Delete old
    delete items[index];

    // Add new
    items[index]=item;

    // Redo layout
    recalc();
    return index;
}


// Replace item with another
int IconList::setItem(int index,const FXString& text,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify)
{
    return setItem(index,createItem(text,big,mini,ptr),notify);
}


// Insert item
int IconList::insertItem(int index,IconItem* item,FXbool notify)
{
    register int old=current;

    // Must have item
    if (!item)
    {
        fxerror("%s::insertItem: item is NULL.\n",getClassName());
    }

    // Must be in range
    if (index<0 || items.no()<index)
    {
        fxerror("%s::insertItem: index out of range.\n",getClassName());
    }

    // Add item to list
    items.insert(index,item);

    // Adjust indices
    if (anchor>=index) 
    	anchor++;
    if (extent>=index) 
    	extent++;
    if (current>=index)
    	current++;
    if (viewable>=index)
    	viewable++;
    if (current<0 && items.no()==1)
    	current=0;

    // Notify item has been inserted
    if (notify && target)
    {
        target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)(FXival)index);
    }

    // Current item may have changed
    if (old!=current)
    {
        if (notify && target)
        {
            target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);
        }
    }

    // Was new item
    if (0<=current && current==index)
    {
        if (hasFocus())
        {
            items[current]->setFocus(TRUE);
        }
        if ((options&SELECT_MASK)==_ICONLIST_BROWSESELECT && items[current]->isEnabled())
        {
            selectItem(current,notify);
        }
    }

    // Redo layout
    recalc();
    return index;
}


// Insert item
int IconList::insertItem(int index,const FXString& text,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify)
{
    return insertItem(index,createItem(text,big,mini,ptr),notify);
}


// Append item
int IconList::appendItem(IconItem* item,FXbool notify)
{
    return insertItem(items.no(),item,notify);
}


// Append item
int IconList::appendItem(const FXString& text,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify)
{
    return insertItem(items.no(),createItem(text,big,mini,ptr),notify);
}


// Prepend item
int IconList::prependItem(IconItem* item,FXbool notify)
{
    return insertItem(0,item,notify);
}


// Prepend item
int IconList::prependItem(const FXString& text,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify)
{
    return insertItem(0,createItem(text,big,mini,ptr),notify);
}


// Fill list by appending items from array of strings
int IconList::fillItems(const char** strings,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify)
{
    register int n=0;
    if (strings)
    {
        while (strings[n])
        {
            appendItem(strings[n++],big,mini,ptr,notify);
        }
    }
    return n;
}


// Fill list by appending items from newline separated strings
int IconList::fillItems(const FXString& strings,FXIcon *big,FXIcon* mini,void* ptr,FXbool notify)
{
    register int n=0;
    FXString text;
    while (!(text=strings.section('\n',n)).empty())
    {
        appendItem(text,big,mini,ptr,notify);
        n++;
    }
    return n;
}


// Move item from oldindex to newindex
int IconList::moveItem(int newindex,int oldindex,FXbool notify)
{
    register int old=current;
    register IconItem *item;

    // Must be in range
    if (newindex<0 || oldindex<0 || items.no()<=newindex || items.no()<=oldindex)
    {
        fxerror("%s::moveItem: index out of range.\n",getClassName());
    }

    // Did it change?
    if (oldindex!=newindex)
    {
        // Move item
        item=items[oldindex];
        items.erase(oldindex);
        items.insert(newindex,item);

        // Move item down
        if (newindex<oldindex)
        {
            if (newindex<=anchor && anchor<oldindex)
            	anchor++;
            if (newindex<=extent && extent<oldindex)
            	extent++;
            if (newindex<=current && current<oldindex)
            	current++;
            if (newindex<=viewable && viewable<oldindex)
            	viewable++;
        }

        // Move item up
        else
        {
            if (oldindex<anchor && anchor<=newindex)
            	anchor--;
            if (oldindex<extent && extent<=newindex)
            	extent--;
            if (oldindex<current && current<=newindex)
            	current--;
            if (oldindex<viewable && viewable<=newindex)
            	viewable--;
        }

        // Adjust if it was equal
        if (anchor==oldindex)
        	anchor=newindex;
        if (extent==oldindex)
        	extent=newindex;
        if (current==oldindex)
        	current=newindex;
        if (viewable==oldindex)
        	viewable=newindex;

        // Current item may have changed
        if (old!=current)
        {
            if (notify && target)
            {
                target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);
            }
        }

        // Redo layout
        recalc();
    }
    return newindex;
}


// Extract node from list
IconItem* IconList::extractItem(int index,FXbool notify)
{
    register IconItem *result;
    register int old=current;

    // Must be in range
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::extractItem: index out of range.\n",getClassName());
    }

    // Notify item will be deleted
    if (notify && target)
    {
        target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);
    }

    // Extract item
    result=items[index];

    // Remove from list
    items.erase(index);

    // Adjust indices
    if (anchor>index || anchor>=items.no())
    	anchor--;
    if (extent>index || extent>=items.no())
    	extent--;
    if (current>index || current>=items.no())
    	current--;
    if (viewable>index || viewable>=items.no())
    	viewable--;

    // Current item has changed
    if (index<=old)
    {
        if (notify && target)
        {
            target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);
        }
    }

    // Deleted current item
    if (0<=current && index==old)
    {
        if (hasFocus())
        {
            items[current]->setFocus(TRUE);
        }
        if ((options&SELECT_MASK)==_ICONLIST_BROWSESELECT && items[current]->isEnabled())
        {
            selectItem(current,notify);
        }
    }

    // Redo layout
    recalc();

    // Return item
    return result;
}


// Remove node from list
void IconList::removeItem(int index,FXbool notify)
{
    register int old=current;

    // Must be in range
    if (index<0 || items.no()<=index)
    {
        fxerror("%s::removeItem: index out of range.\n",getClassName());
    }

    // Notify item will be deleted
    if (notify && target)
    {
        target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);
    }

    // Delete item
    delete items[index];

    // Remove from list
    items.erase(index);

    // Adjust indices
    if (anchor>index || anchor>=items.no())
    	anchor--;
    if (extent>index || extent>=items.no())
    	extent--;
    if (current>index || current>=items.no())
    	current--;
    if (viewable>index || viewable>=items.no())
    	viewable--;

    // Current item has changed
    if (index<=old)
    {
        if (notify && target)
        {
            target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)current);
        }
    }

    // Deleted current item
    if (0<=current && index==old)
    {
        if (hasFocus())
        {
            items[current]->setFocus(TRUE);
        }
        if ((options&SELECT_MASK)==_ICONLIST_BROWSESELECT && items[current]->isEnabled())
        {
            selectItem(current,notify);
        }
    }

    // Redo layout
    recalc();
}


// Remove all items
void IconList::clearItems(FXbool notify)
{
    register int old=current;

    // Delete items
    for (int index=items.no()-1; 0<=index; index--)
    {
        if (notify && target)
        {
            target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)(FXival)index);
        }
        delete items[index];
    }

    // Free array
    items.clear();

    // Adjust indices
    current=-1;
    anchor=-1;
    extent=-1;
    viewable=-1;

    // Current item has changed
    if (old!=-1)
    {
        if (notify && target)
        {
            target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)-1);
        }
    }

    // Redo layout
    recalc();
}


// Change the font
void IconList::setFont(FXFont* fnt)
{
    if (!fnt)
    {
        fxerror("%s::setFont: NULL font specified.\n",getClassName());
    }
    if (font!=fnt)
    {
        font=fnt;
        recalc();
        update();
    }
}


// Set text color
void IconList::setTextColor(FXColor clr)
{
    if (clr!=textColor)
    {
        textColor=clr;
        update();
    }
}


// Set select background color
void IconList::setSelBackColor(FXColor clr)
{
    if (clr!=selbackColor)
    {
        selbackColor=clr;
        update();
    }
}


// Set selected text color
void IconList::setSelTextColor(FXColor clr)
{
    if (clr!=seltextColor)
    {
        seltextColor=clr;
        update();
    }
}


// Set text width
void IconList::setItemSpace(int s)
{
    if (s<1) s=1;
    if (itemSpace!=s)
    {
        itemSpace=s;
        recalc();
    }
}


// Change list style
void IconList::setListStyle(unsigned int style)
{
    unsigned int opts=(options&~ICONLIST_MASK) | (style&ICONLIST_MASK);
    if (options!=opts)
    {
        options=opts;
        recalc();
    }
}


// Get list style
unsigned int IconList::getListStyle() const
{
    return (options&ICONLIST_MASK);
}


// Change help text
void IconList::setHelpText(const FXString& text)
{
    help=text;
}


// Set ignore case flag
void IconList::setIgnoreCase(FXbool icase)
{
	ignorecase=icase;
}


// Save data
void IconList::save(FXStream& store) const
{
    FXScrollArea::save(store);
    store << header;
    items.save(store);
    store << nrows;
    store << ncols;
    store << anchor;
    store << current;
    store << extent;
    store << font;
    store << textColor;
    store << selbackColor;
    store << seltextColor;
    store << itemSpace;
    store << itemWidth;
    store << itemHeight;
    store << help;
}


// Load data
void IconList::load(FXStream& store)
{
    FXScrollArea::load(store);
    store >> header;
    items.load(store);
    store >> nrows;
    store >> ncols;
    store >> anchor;
    store >> current;
    store >> extent;
    store >> font;
    store >> textColor;
    store >> selbackColor;
    store >> seltextColor;
    store >> itemSpace;
    store >> itemWidth;
    store >> itemHeight;
    store >> help;
}


// Cleanup
IconList::~IconList()
{
    getApp()->removeTimeout(this,ID_TIPTIMER);
    getApp()->removeTimeout(this,ID_LOOKUPTIMER);
    clearItems(FALSE);
    header=(FXHeader*)-1L;
    font=(FXFont*)-1L;
}

