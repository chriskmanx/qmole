// Text label with selection/copy/paste capabilities
// Based on the FXTextField widget

#include <fx.h>
#include <fxkeys.h>
#include <FX88591Codec.h>
#include <FXUTF16Codec.h>

#include "TextLabel.h"

#define JUSTIFY_MASK    (JUSTIFY_HZ_APART|JUSTIFY_VT_APART)


// Map
FXDEFMAP(TextLabel) TextLabelMap[]=
{
    FXMAPFUNC(SEL_PAINT,0,TextLabel::onPaint),
    FXMAPFUNC(SEL_UPDATE,0,TextLabel::onUpdate),
    FXMAPFUNC(SEL_MOTION,0,TextLabel::onMotion),
    FXMAPFUNC(SEL_TIMEOUT,TextLabel::ID_AUTOSCROLL,TextLabel::onAutoScroll),
    FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,TextLabel::onLeftBtnPress),
    FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,TextLabel::onLeftBtnRelease),
    FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,TextLabel::onMiddleBtnPress),
    FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,TextLabel::onMiddleBtnRelease),
    FXMAPFUNC(SEL_KEYPRESS,0,TextLabel::onKeyPress),
    FXMAPFUNC(SEL_KEYRELEASE,0,TextLabel::onKeyRelease),
    FXMAPFUNC(SEL_SELECTION_LOST,0,TextLabel::onSelectionLost),
    FXMAPFUNC(SEL_SELECTION_GAINED,0,TextLabel::onSelectionGained),
    FXMAPFUNC(SEL_SELECTION_REQUEST,0,TextLabel::onSelectionRequest),
    FXMAPFUNC(SEL_CLIPBOARD_LOST,0,TextLabel::onClipboardLost),
    FXMAPFUNC(SEL_CLIPBOARD_GAINED,0,TextLabel::onClipboardGained),
    FXMAPFUNC(SEL_CLIPBOARD_REQUEST,0,TextLabel::onClipboardRequest),
    FXMAPFUNC(SEL_FOCUSIN,0,TextLabel::onFocusIn),
    FXMAPFUNC(SEL_FOCUSOUT,0,TextLabel::onFocusOut),
    FXMAPFUNC(SEL_FOCUS_SELF,0,TextLabel::onFocusSelf),
    FXMAPFUNC(SEL_UPDATE,TextLabel::ID_COPY_SEL,TextLabel::onUpdHaveSelection),
    FXMAPFUNC(SEL_UPDATE,TextLabel::ID_SELECT_ALL,TextLabel::onUpdSelectAll),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_CURSOR_HOME,TextLabel::onCmdCursorHome),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_CURSOR_END,TextLabel::onCmdCursorEnd),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_CURSOR_RIGHT,TextLabel::onCmdCursorRight),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_CURSOR_LEFT,TextLabel::onCmdCursorLeft),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_CURSOR_WORD_LEFT,TextLabel::onCmdCursorWordLeft),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_CURSOR_WORD_RIGHT,TextLabel::onCmdCursorWordRight),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_CURSOR_WORD_START,TextLabel::onCmdCursorWordStart),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_CURSOR_WORD_END,TextLabel::onCmdCursorWordEnd),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_MARK,TextLabel::onCmdMark),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_EXTEND,TextLabel::onCmdExtend),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_SELECT_ALL,TextLabel::onCmdSelectAll),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_DESELECT_ALL,TextLabel::onCmdDeselectAll),
    FXMAPFUNC(SEL_COMMAND,TextLabel::ID_COPY_SEL,TextLabel::onCmdCopySel),
};


// Object implementation
FXIMPLEMENT(TextLabel,FXFrame,TextLabelMap,ARRAYNUMBER(TextLabelMap))


// Delimiters
const char TextLabel::textDelimiters[]="~.,/\\`'!@#$%^&*()-=+{}|[]\":;<>?";


// Construct and init
TextLabel::TextLabel(FXComposite* p,int ncols,FXObject* tgt,FXSelector sel,unsigned int opts,int x,int y,int w,int h,int pl,int pr,int pt,int pb):
        FXFrame(p,opts,x,y,w,h,pl,pr,pt,pb)
{
    if (ncols<0)
    	ncols=0;
    flags|=FLAG_ENABLED;
    target=tgt;
    message=sel;
    if (!(options&JUSTIFY_RIGHT))
    	options|=JUSTIFY_LEFT;
 
     // Note : cursor is not changed if the object is constructed with ncols=0
    if (ncols>0)
    {
		defaultCursor=getApp()->getDefaultCursor(DEF_TEXT_CURSOR);
		dragCursor=getApp()->getDefaultCursor(DEF_TEXT_CURSOR);
	}

    delimiters=textDelimiters;
    font=getApp()->getNormalFont();
    backColor=getApp()->getBackColor();
    textColor=getApp()->getForeColor();
    selbackColor=getApp()->getSelbackColor();
    seltextColor=getApp()->getSelforeColor();
    cursorColor=getApp()->getForeColor();
    cursor=0;
    anchor=0;
    columns=ncols;
    shift=0;
}


// Create X window
void TextLabel::create()
{
    FXFrame::create();
    if (!textType)
        textType=getApp()->registerDragType(textTypeName);
    if (!utf8Type)
        utf8Type=getApp()->registerDragType(utf8TypeName);
    if (!utf16Type)
        utf16Type=getApp()->registerDragType(utf16TypeName);
    font->create();
}


// Change the font
void TextLabel::setFont(FXFont* fnt)
{
    if (!fnt)
        fxerror("%s::setFont: NULL font specified.\n",getClassName());
    if (font!=fnt)
    {
        font=fnt;
        recalc();
        update();
    }
}


// Enable the window
void TextLabel::enable()
{
    if (!(flags&FLAG_ENABLED))
    {
        FXFrame::enable();
        update();
    }
}


// Disable the window
void TextLabel::disable()
{
    if (flags&FLAG_ENABLED)
    {
        FXFrame::disable();
        update();
    }
}


// Get default width
int TextLabel::getDefaultWidth()
{
    return padleft+padright+(border<<1)+columns*font->getTextWidth("8",1);
}


// Get default height
int TextLabel::getDefaultHeight()
{
    return padtop+padbottom+(border<<1)+font->getFontHeight();
}


// Implement auto-hide or auto-gray modes
long TextLabel::onUpdate(FXObject* sender,FXSelector sel,void* ptr)
{
    if (!FXFrame::onUpdate(sender,sel,ptr))
    {
        if (options&TEXTFIELD_AUTOHIDE)
        {
            if (shown())
            {
                hide();
                recalc();
            }
        }
        if (options&TEXTFIELD_AUTOGRAY)
            disable();
    }
    return 1;
}


// We now really do have the selection; repaint the text field
long TextLabel::onSelectionGained(FXObject* sender,FXSelector sel,void* ptr)
{
    FXFrame::onSelectionGained(sender,sel,ptr);
    update();
    return 1;
}


// We lost the selection somehow; repaint the text field
long TextLabel::onSelectionLost(FXObject* sender,FXSelector sel,void* ptr)
{
    FXFrame::onSelectionLost(sender,sel,ptr);
    update();
    return 1;
}


// Somebody wants our selection; the text field will furnish it if the target doesn't
long TextLabel::onSelectionRequest(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent *event=(FXEvent*)ptr;
    FXString string;
    unsigned int   start;
    unsigned int   len;

    // Make sure
    FXASSERT(0<=anchor && anchor<=contents.length());
    FXASSERT(0<=cursor && cursor<=contents.length());

    // Perhaps the target wants to supply its own data for the selection
    if (FXFrame::onSelectionRequest(sender,sel,ptr))
    	return 1;

    // Recognize the request?
    if (event->target==stringType || event->target==textType || event->target==utf8Type || event->target==utf16Type)
    {

        // Figure selected bytes
        if (anchor<cursor)
        {
            start=anchor;
            len=cursor-anchor;
        }
        else
        {
            start=cursor;
            len=anchor-cursor;
        }

        // Get selected fragment
        string=contents.mid(start,len);

        // If password mode, replace by stars
        if (options&TEXTFIELD_PASSWD)
        	string.assign('*',string.count());

        // Return text of the selection as UTF-8
        if (event->target==utf8Type)
        {
            setDNDData(FROM_SELECTION,event->target,string);
            return 1;
        }

        // Return text of the selection translated to 8859-1
        if (event->target==stringType || event->target==textType)
        {
            FX88591Codec ascii;
            setDNDData(FROM_SELECTION,event->target,ascii.utf2mb(string));
            return 1;
        }

        // Return text of the selection translated to UTF-16
        if (event->target==utf16Type)
        {
            FXUTF16LECodec unicode;           // FIXME maybe other endianness for unix
            setDNDData(FROM_SELECTION,event->target,unicode.utf2mb(string));
            return 1;
        }
    }
    return 0;
}


// We now really do have the clipboard, keep clipped text
long TextLabel::onClipboardGained(FXObject* sender,FXSelector sel,void* ptr)
{
    FXFrame::onClipboardGained(sender,sel,ptr);
    return 1;
}


// We lost the clipboard, free clipped text
long TextLabel::onClipboardLost(FXObject* sender,FXSelector sel,void* ptr)
{
    FXFrame::onClipboardLost(sender,sel,ptr);
    clipped.clear();
    return 1;
}


// Somebody wants our clipped text
long TextLabel::onClipboardRequest(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent *event=(FXEvent*)ptr;
    FXString string;

    // Perhaps the target wants to supply its own data for the clipboard
    if (FXFrame::onClipboardRequest(sender,sel,ptr))
    	return 1;

    // Recognize the request?
    if (event->target==stringType || event->target==textType || event->target==utf8Type || event->target==utf16Type)
    {

        // Get clipped string
        string=clipped;

        // If password mode, replace by stars
        if (options&TEXTFIELD_PASSWD)
        	string.assign('*',string.count());

        // Return clipped text as as UTF-8
        if (event->target==utf8Type)
        {
            setDNDData(FROM_CLIPBOARD,event->target,string);
            return 1;
        }

        // Return clipped text translated to 8859-1
        if (event->target==stringType || event->target==textType)
        {
            FX88591Codec ascii;
            setDNDData(FROM_CLIPBOARD,event->target,ascii.utf2mb(string));
            return 1;
        }

        // Return text of the selection translated to UTF-16
        if (event->target==utf16Type)
        {
            FXUTF16LECodec unicode;             // FIXME maybe other endianness for unix
            setDNDData(FROM_CLIPBOARD,event->target,unicode.utf2mb(string));
            return 1;
        }
    }
    return 0;
}




// Gained focus
long TextLabel::onFocusIn(FXObject* sender,FXSelector sel,void* ptr)
{
    FXFrame::onFocusIn(sender,sel,ptr);
    if (hasSelection())
        update(border,border,width-(border<<1),height-(border<<1));
    return 1;
}


// Lost focus
long TextLabel::onFocusOut(FXObject* sender,FXSelector sel,void* ptr)
{
    FXFrame::onFocusOut(sender,sel,ptr);
    if (hasSelection())
        update(border,border,width-(border<<1),height-(border<<1));
    return 1;
}


// Focus on widget itself
long TextLabel::onFocusSelf(FXObject* sender,FXSelector sel,void* ptr)
{
    if (FXFrame::onFocusSelf(sender,sel,ptr))
    {
        FXEvent *event=(FXEvent*)ptr;
        if (event->type==SEL_KEYPRESS || event->type==SEL_KEYRELEASE)
            handle(this,FXSEL(SEL_COMMAND,ID_SELECT_ALL),NULL);
        return 1;
    }
    return 0;
}


// If window can have focus
bool TextLabel::canFocus() const
{
    return true;
}


// Into focus chain
void TextLabel::setFocus()
{
    FXFrame::setFocus();
    setDefault(TRUE);
    flags&=~FLAG_UPDATE;
    if (getApp()->hasInputMethod())
        createComposeContext();
}


// Out of focus chain
void TextLabel::killFocus()
{
    FXFrame::killFocus();
    setDefault(MAYBE);
    flags|=FLAG_UPDATE;
    if (flags&FLAG_CHANGED)
    {
        flags&=~FLAG_CHANGED;
        if (!(options&TEXTFIELD_ENTER_ONLY))
            if (target) target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)contents.text());
    }
    if (getApp()->hasInputMethod())
        destroyComposeContext();
}



// Pressed left button
long TextLabel::onLeftBtnPress(FXObject*,FXSelector,void* ptr)
{
    FXEvent* ev=(FXEvent*)ptr;
    handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
    if (isEnabled())
    {
        grab();
        if (target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr))
        	return 1;
        flags&=~FLAG_UPDATE;
        if (ev->click_count==1)
        {
            setCursorPos(index(ev->win_x));
            if (ev->state&SHIFTMASK)
                extendSelection(cursor);
            else
            {
                killSelection();
                setAnchorPos(cursor);
            }
            makePositionVisible(cursor);
            flags|=FLAG_PRESSED;
        }
        else
        {
            setAnchorPos(0);
            setCursorPos(contents.length());
            extendSelection(contents.length());
            makePositionVisible(cursor);
        }
        return 1;
    }
    return 0;
}


// Released left button
long TextLabel::onLeftBtnRelease(FXObject*,FXSelector,void* ptr)
{
    if (isEnabled())
    {
        ungrab();
        flags&=~FLAG_PRESSED;
        if (target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr))
        	return 1;
        return 1;
    }
    return 0;
}



// Moved
long TextLabel::onMotion(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    int t;
    if (flags&FLAG_PRESSED)
    {
        if (event->win_x<(border+padleft) || (width-border-padright)<event->win_x)
        {
            if (!getApp()->hasTimeout(this,ID_AUTOSCROLL))
                getApp()->addTimeout(this,ID_AUTOSCROLL,getApp()->getScrollSpeed(),event);
        }
        else
        {
            getApp()->removeTimeout(this,ID_AUTOSCROLL);
            t=index(event->win_x);
            if (t!=cursor)
            {
                cursor=t;
                extendSelection(cursor);
            }
        }
        return 1;
    }
    return 0;
}


// Automatic scroll
long TextLabel::onAutoScroll(FXObject*,FXSelector,void* ptr)
{
    register FXEvent* event=(FXEvent*)ptr;
    if (flags&FLAG_PRESSED)
    {
        register int newcursor=cursor;
        register int ll=border+padleft;
        register int rr=width-border-padright;
        register int ww=rr-ll;
        register int tw;

        if (options&TEXTFIELD_PASSWD)
            tw=font->getTextWidth("*",1)*contents.count();
        else
            tw=font->getTextWidth(contents.text(),contents.length());

        // Text right-aligned
        if (options&JUSTIFY_RIGHT)
        {
            // Scroll left
            if (event->win_x<ll)
            {
                if (tw>ww)
                {
                    shift+=ll-event->win_x;
                    if (ww>tw-shift)
                        shift=tw-ww;
                    else
                        getApp()->addTimeout(this,ID_AUTOSCROLL,getApp()->getScrollSpeed(),event);
                }
                newcursor=index(ll);
            }

            // Scroll right
            if (rr<event->win_x)
            {
                if (tw>ww)
                {
                    shift+=rr-event->win_x;
                    if (shift<=0)
                        shift=0;
                    else
                        getApp()->addTimeout(this,ID_AUTOSCROLL,getApp()->getScrollSpeed(),event);
                }
                newcursor=index(rr);
            }
        }

        // Text left-aligned
        else if (options&JUSTIFY_LEFT)
        {
            // Scroll left
            if (event->win_x<ll)
            {
                if (tw>ww)
                {
                    shift+=ll-event->win_x;
                    if (shift>=0)
                        shift=0;
                    else
                        getApp()->addTimeout(this,ID_AUTOSCROLL,getApp()->getScrollSpeed(),event);
                }
                newcursor=index(ll);
            }

            // Scroll right
            if (rr<event->win_x)
            {
                if (tw>ww)
                {
                    shift+=rr-event->win_x;
                    if (shift+tw<ww)
                        shift=ww-tw;
                    else
                        getApp()->addTimeout(this,ID_AUTOSCROLL,getApp()->getScrollSpeed(),event);
                }
                newcursor=index(rr);
            }
        }

        // Text centered
        else
        {

            // Scroll left
            if (event->win_x<ll)
            {
                if (tw>ww)
                {
                    shift+=ll-event->win_x;
                    if (shift>tw/2-ww/2)
                        shift=tw/2-ww/2;
                    else
                        getApp()->addTimeout(this,ID_AUTOSCROLL,getApp()->getScrollSpeed(),event);
                }
                newcursor=index(ll);
            }

            // Scroll right
            if (rr<event->win_x)
            {
                if (tw>ww)
                {
                    shift+=rr-event->win_x;
                    if (shift<(ww-ww/2)-tw/2)
                        shift=(ww-ww/2)-tw/2;
                    else
                        getApp()->addTimeout(this,ID_AUTOSCROLL,getApp()->getScrollSpeed(),event);
                }
                newcursor=index(rr);
            }
        }

        // Extend the selection
        if (newcursor!=cursor)
        {
            cursor=newcursor;
            extendSelection(cursor);
        }
    }
    return 1;
}


// Update somebody who works on the selection
long TextLabel::onUpdHaveSelection(FXObject* sender,FXSelector,void* ptr)
{
    sender->handle(this,hasSelection()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),ptr);
    return 1;
}


// Update somebody who works on the selection
long TextLabel::onUpdSelectAll(FXObject* sender,FXSelector,void* ptr)
{
    sender->handle(this,contents.empty()?FXSEL(SEL_COMMAND,ID_DISABLE):FXSEL(SEL_COMMAND,ID_ENABLE),ptr);
    return 1;
}


// Move the cursor to new valid position
void TextLabel::setCursorPos(int pos)
{
    pos=contents.validate(FXCLAMP(0,pos,contents.length()));
    if (cursor!=pos)
        cursor=pos;
}


// Set anchor position to valid position
void TextLabel::setAnchorPos(int pos)
{
    anchor=contents.validate(FXCLAMP(0,pos,contents.length()));
}



// Fix scroll amount after text changes or widget resize
void TextLabel::layout()
{
    register int rr=width-border-padright;
    register int ll=border+padleft;
    register int ww=rr-ll;
    register int tw;
    if (!xid)
    	return;

    // Figure text width
    if (options&TEXTFIELD_PASSWD)
        tw=font->getTextWidth("*",1)*contents.count();
    else
        tw=font->getTextWidth(contents.text(),contents.length());

    // Constrain shift
    if (options&JUSTIFY_RIGHT)
    {
        if (ww>=tw)
        	shift=0;
        else if (shift<0)
        	shift=0;
        else if (shift>tw-ww)
        	shift=tw-ww;
    }
    else if (options&JUSTIFY_LEFT)
    {
        if (ww>=tw)
        	shift=0;
        else if (shift>0)
        	shift=0;
        else if (shift<ww-tw)
        	shift=ww-tw;
    }
    else
    {
        if (ww>=tw)
        	shift=0;
        else if (shift>tw/2-ww/2)
        	shift=tw/2-ww/2;
        else if (shift<(ww-ww/2)-tw/2)
        	shift=(ww-ww/2)-tw/2;
    }

    // Keep cursor in the picture if resizing field
    makePositionVisible(cursor);

    // Always redraw
    update();

    flags&=~FLAG_DIRTY;
}


// Force position to become fully visible; we assume layout is correct
void TextLabel::makePositionVisible(int pos)
{
    register int rr=width-border-padright;
    register int ll=border+padleft;
    register int ww=rr-ll;
    register int oldshift=shift;
    register int xx;
    if (!xid)
    	return;
    pos=contents.validate(FXCLAMP(0,pos,contents.length()));
    if (options&JUSTIFY_RIGHT)
    {
        if (options&TEXTFIELD_PASSWD)
            xx=font->getTextWidth("*",1)*contents.count(pos,contents.length());
        else
            xx=font->getTextWidth(&contents[pos],contents.length()-pos);
        if (shift-xx>0)
        	shift=xx;
        else if (shift-xx<-ww)
        	shift=xx-ww;
    }
    else if (options&JUSTIFY_LEFT)
    {
        if (options&TEXTFIELD_PASSWD)
            xx=font->getTextWidth("*",1)*contents.index(pos);
        else
            xx=font->getTextWidth(contents.text(),pos);
        if (shift+xx<0)
        	shift=-xx;
        else if (shift+xx>=ww)
        	shift=ww-xx;
    }
    else
    {
        if (options&TEXTFIELD_PASSWD)
            xx=font->getTextWidth("*",1)*contents.index(pos)-(font->getTextWidth("*",1)*contents.count())/2;
        else
            xx=font->getTextWidth(contents.text(),pos)-font->getTextWidth(contents.text(),contents.length())/2;
        if (shift+ww/2+xx<0)
        	shift=-ww/2-xx;
        else if (shift+ww/2+xx>=ww)
        	shift=ww-ww/2-xx;
    }
    if (shift!=oldshift)
        update(border,border,width-(border<<1),height-(border<<1));
}


// Find index from coord
int TextLabel::index(int x) const
{
    register int rr=width-border-padright;
    register int ll=border+padleft;
    register int mm=(ll+rr)/2;
    register int pos,xx,cw;
    if (options&TEXTFIELD_PASSWD)
    {
        cw=font->getTextWidth("*",1);
        if (options&JUSTIFY_RIGHT)
        	xx=rr-cw*contents.count();
        else if (options&JUSTIFY_LEFT)
        	xx=ll;
        else
        	xx=mm-(cw*contents.count())/2;
        xx+=shift;
        pos=contents.offset((x-xx+(cw>>1))/cw);
    }
    else
    {
        if (options&JUSTIFY_RIGHT)
        	xx=rr-font->getTextWidth(contents.text(),contents.length());
        else if
        	(options&JUSTIFY_LEFT) xx=ll;
        else
        	xx=mm-font->getTextWidth(contents.text(),contents.length())/2;
        xx+=shift;
        for (pos=0; pos<contents.length(); pos=contents.inc(pos))
        {
            cw=font->getTextWidth(&contents[pos],contents.extent(pos));
            if (x<(xx+(cw>>1)))
            	break;
            xx+=cw;
        }
    }
    if (pos<0)
    	pos=0;
    if (pos>contents.length())
    	pos=contents.length();
    return pos;
}


// Find coordinate from index
int TextLabel::coord(int i) const
{
    register int rr=width-border-padright;
    register int ll=border+padleft;
    register int mm=(ll+rr)/2;
    register int pos;
    FXASSERT(0<=i && i<=contents.length());
    if (options&JUSTIFY_RIGHT)
    {
        if (options&TEXTFIELD_PASSWD)
            pos=rr-font->getTextWidth("*",1)*(contents.count()-contents.index(i));
        else
            pos=rr-font->getTextWidth(&contents[i],contents.length()-i);
    }
    else if (options&JUSTIFY_LEFT)
    {
        if (options&TEXTFIELD_PASSWD)
            pos=ll+font->getTextWidth("*",1)*contents.index(i);
        else
            pos=ll+font->getTextWidth(contents.text(),i);
    }
    else
    {
        if (options&TEXTFIELD_PASSWD)
            pos=mm+font->getTextWidth("*",1)*contents.index(i)-(font->getTextWidth("*",1)*contents.count())/2;
        else
            pos=mm+font->getTextWidth(contents.text(),i)-font->getTextWidth(contents.text(),contents.length())/2;
    }
    return pos+shift;
}


// Return TRUE if position is visible
FXbool TextLabel::isPosVisible(int pos) const
{
    if (0<=pos && pos<=contents.length())
    {
        register int x=coord(contents.validate(pos));
        return border+padleft<=x && x<=width-border-padright;
    }
    return FALSE;
}


// Return TRUE if position pos is selected
FXbool TextLabel::isPosSelected(int pos) const
{
    return hasSelection() && FXMIN(anchor,cursor)<=pos && pos<=FXMAX(anchor,cursor);
}


// Draw text fragment
void TextLabel::drawTextFragment(FXDCWindow& dc,int x,int y,int fm,int to)
{
    x+=font->getTextWidth(contents.text(),fm);
    y+=font->getFontAscent();
    dc.drawText(x,y,&contents[fm],to-fm);
}



// Draw range of text
void TextLabel::drawTextRange(FXDCWindow& dc,int fm,int to)
{
    register int sx,ex,xx,yy,cw,hh,ww,si,ei,lx,rx,t;
    register int rr=width-border-padright;
    register int ll=border+padleft;
    register int mm=(ll+rr)/2;

    if (to<=fm)
    	return;

    dc.setFont(font);

    // Text color
    dc.setForeground(textColor);

    // Height
    hh=font->getFontHeight();

    // Text sticks to top of field
    if (options&JUSTIFY_TOP)
        yy=padtop+border;

    // Text sticks to bottom of field
    else if (options&JUSTIFY_BOTTOM)
        yy=height-padbottom-border-hh;

    // Text centered in y
    else
        yy=border+padtop+(height-padbottom-padtop-(border<<1)-hh)/2;

    if (anchor<cursor)
    {
        si=anchor;
        ei=cursor;
    }
    else
    {
        si=cursor;
        ei=anchor;
    }


    // Normal mode
    ww=font->getTextWidth(contents.text(),contents.length());

    // Text sticks to right of field
    if (options&JUSTIFY_RIGHT)
        xx=shift+rr-ww;

    // Text sticks on left of field
    else if (options&JUSTIFY_LEFT)
        xx=shift+ll;

    // Text centered in field
    else
        xx=shift+mm-ww/2;

    // Reduce to avoid drawing excessive amounts of text
    lx=xx+font->getTextWidth(&contents[0],fm);
    rx=lx+font->getTextWidth(&contents[fm],to-fm);
    while (fm<to)
    {
        t=contents.inc(fm);
        cw=font->getTextWidth(&contents[fm],t-fm);
        if (lx+cw>=0)
        	break;
        lx+=cw;
        fm=t;
    }
    while (fm<to)
    {
        t=contents.dec(to);
        cw=font->getTextWidth(&contents[t],to-t);
        if (rx-cw<width)
        	break;
        rx-=cw;
        to=t;
    }

    // Adjust selected range
    if (si<fm)
    	si=fm;
    if (ei>to)
    	ei=to;

    // Nothing selected
    if (!hasSelection() || to<=si || ei<=fm)
        drawTextFragment(dc,xx,yy,fm,to);

    // Stuff selected
    else
    {
        if (fm<si)
            drawTextFragment(dc,xx,yy,fm,si);
        else
            si=fm;
        if (ei<to)
            drawTextFragment(dc,xx,yy,ei,to);
        else
            ei=to;
        if (si<ei)
        {
            sx=xx+font->getTextWidth(contents.text(),si);
            ex=xx+font->getTextWidth(contents.text(),ei);
            if (hasFocus())
            {
                dc.setForeground(selbackColor);
                dc.fillRectangle(sx,padtop+border,ex-sx,height-padtop-padbottom-(border<<1));
                dc.setForeground(seltextColor);
                drawTextFragment(dc,xx,yy,si,ei);
            }
            else
            {
                dc.setForeground(baseColor);
                dc.fillRectangle(sx,padtop+border,ex-sx,height-padtop-padbottom-(border<<1));
                dc.setForeground(textColor);
                drawTextFragment(dc,xx,yy,si,ei);
            }
        }
    }
}


// Handle repaint
long TextLabel::onPaint(FXObject*,FXSelector,void* ptr)
{
    FXEvent *ev=(FXEvent*)ptr;
    FXDCWindow dc(this,ev);

    // Draw frame
    drawFrame(dc,0,0,width,height);

    // Gray background if disabled
    if (isEnabled())
        dc.setForeground(backColor);
    else
        dc.setForeground(baseColor);

    // Draw background
    dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));

    // Draw text, clipped against frame interior
    dc.setClipRectangle(border,border,width-(border<<1),height-(border<<1));
    drawTextRange(dc,0,contents.length());

    return 1;
}


// Move cursor to begin of line
long TextLabel::onCmdCursorHome(FXObject*,FXSelector,void*)
{
    setCursorPos(0);
    makePositionVisible(0);
    return 1;
}


// Move cursor to end of line
long TextLabel::onCmdCursorEnd(FXObject*,FXSelector,void*)
{
    setCursorPos(contents.length());
    makePositionVisible(cursor);
    return 1;
}


// Move cursor right
long TextLabel::onCmdCursorRight(FXObject*,FXSelector,void*)
{
    setCursorPos(contents.inc(cursor));
    makePositionVisible(cursor);
    return 1;
}


// Move cursor left
long TextLabel::onCmdCursorLeft(FXObject*,FXSelector,void*)
{
    setCursorPos(contents.dec(cursor));
    makePositionVisible(cursor);
    return 1;
}


// Check if w is delimiter
static FXbool isdelimiter(const char *delimiters,FXwchar w)
{
    return w<256 && strchr(delimiters,w); // FIXME for w>256
}


// Find end of previous word
int TextLabel::leftWord(int pos) const
{
    register int pp=pos,p;

    // Ensure input is valid
    FXASSERT(0<=pos && pos<=contents.length());

    // Back up until space or delimiter
    while (0<=(p=contents.dec(pp)) && !Unicode::isSpace(contents.wc(p)) && !isdelimiter(delimiters,contents.wc(p))) pp=p;

    // Back up over run of spaces
    while (0<=(p=contents.dec(pp)) && Unicode::isSpace(contents.wc(p))) pp=p;

    // One more in case we didn't move
    if ((pos==pp) && 0<=(p=contents.dec(pp)))
    	pp=p;

    return pp;
}


// Find begin of next word
int TextLabel::rightWord(int pos) const
{
    register int pp=pos;

    // Ensure input is valid
    FXASSERT(0<=pos && pos<=contents.length());

    // Advance until space or delimiter
    while (pp<contents.length() && !Unicode::isSpace(contents.wc(pp)) && !isdelimiter(delimiters,contents.wc(pp))) pp=contents.inc(pp);

    // Advance over run of spaces
    while (pp<contents.length() && Unicode::isSpace(contents.wc(pp))) pp=contents.inc(pp);

    // One more in case we didn't move
    if ((pos==pp) && pp<contents.length())
    	pp=contents.inc(pp);

    return pp;
}


// Find begin of a word
int TextLabel::wordStart(int pos) const
{
    register int p;
    FXASSERT(0<=pos && pos<=contents.length());
    if (pos==contents.length() || Unicode::isSpace(contents.wc(pos)))
    {
        while (0<=(p=contents.dec(pos)) && Unicode::isSpace(contents.wc(p))) pos=p;
    }
    else if (isdelimiter(delimiters,contents.wc(pos)))
    {
        while (0<=(p=contents.dec(pos)) && isdelimiter(delimiters,contents.wc(p))) pos=p;
    }
    else
    {
        while (0<=(p=contents.dec(pos)) && !isdelimiter(delimiters,contents.wc(p)) && !Unicode::isSpace(contents.wc(p))) pos=p;
    }
    return pos;
}


// Find end of word
int TextLabel::wordEnd(int pos) const
{
    FXASSERT(0<=pos && pos<=contents.length());
    if (pos==contents.length() || Unicode::isSpace(contents.wc(pos)))
        while (pos<contents.length() && Unicode::isSpace(contents.wc(pos))) pos=contents.inc(pos);
    else if (isdelimiter(delimiters,contents.wc(pos)))
        while (pos<contents.length() && isdelimiter(delimiters,contents.wc(pos))) pos=contents.inc(pos);
    else
        while (pos<contents.length() && !isdelimiter(delimiters,contents.wc(pos)) && !Unicode::isSpace(contents.wc(pos))) pos=contents.inc(pos);
    return pos;
}


// Move cursor word right
long TextLabel::onCmdCursorWordRight(FXObject*,FXSelector,void*)
{
    setCursorPos(rightWord(cursor));
    makePositionVisible(cursor);
    return 1;
}


// Move cursor word left
long TextLabel::onCmdCursorWordLeft(FXObject*,FXSelector,void*)
{
    setCursorPos(leftWord(cursor));
    makePositionVisible(cursor);
    return 1;
}


// Move cursor to word start
long TextLabel::onCmdCursorWordStart(FXObject*,FXSelector,void*)
{
    setCursorPos(wordStart(cursor));
    makePositionVisible(cursor);
    return 1;
}


// Move cursor to word end
long TextLabel::onCmdCursorWordEnd(FXObject*,FXSelector,void*)
{
    setCursorPos(wordEnd(cursor));
    makePositionVisible(cursor);
    return 1;
}


// Mark
long TextLabel::onCmdMark(FXObject*,FXSelector,void*)
{
    setAnchorPos(cursor);
    return 1;
}


// Extend
long TextLabel::onCmdExtend(FXObject*,FXSelector,void*)
{
    extendSelection(cursor);
    return 1;
}


// Select All
long TextLabel::onCmdSelectAll(FXObject*,FXSelector,void*)
{
    selectAll();
    makePositionVisible(cursor);
    return 1;
}


// Deselect All
long TextLabel::onCmdDeselectAll(FXObject*,FXSelector,void*)
{
    killSelection();
    return 1;
}



// Copy onto cliboard
long TextLabel::onCmdCopySel(FXObject*,FXSelector,void*)
{
    if (hasSelection())
    {
        FXDragType types[4];
        types[0]=stringType;
        types[1]=textType;
        types[2]=utf8Type;
        types[3]=utf16Type;
        if (acquireClipboard(types,4))
        {
            if (anchor<cursor)
                clipped=contents.mid(anchor,cursor-anchor);
            else
                clipped=contents.mid(cursor,anchor-cursor);
        }
    }
    return 1;
}


// Pressed a key
long TextLabel::onKeyPress(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    if (isEnabled())
    {
        if (target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr))
        	return 1;
        flags&=~FLAG_UPDATE;
        switch (event->code)
        {
        case KEY_Right:
        case KEY_KP_Right:
            if (!(event->state&SHIFTMASK))
                handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
            if (event->state&CONTROLMASK)
                handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_WORD_RIGHT),NULL);
            else
                handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_RIGHT),NULL);
            if (event->state&SHIFTMASK)
                handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
            else
                handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
            return 1;
        case KEY_Left:
        case KEY_KP_Left:
            if (!(event->state&SHIFTMASK))
                handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
            if (event->state&CONTROLMASK)
                handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_WORD_LEFT),NULL);
            else
                handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_LEFT),NULL);
            if (event->state&SHIFTMASK)
                handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
            else
                handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
            return 1;
        case KEY_Home:
        case KEY_KP_Home:
            if (!(event->state&SHIFTMASK))
                handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
            handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_HOME),NULL);
            if (event->state&SHIFTMASK)
                handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
            else
                handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
            return 1;
        case KEY_End:
        case KEY_KP_End:
            if (!(event->state&SHIFTMASK))
                handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
            handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_END),NULL);
            if (event->state&SHIFTMASK)
                handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
            else
                handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
            return 1;
        case KEY_Return:
        case KEY_KP_Enter:
            getApp()->beep();
            return 1;
        case KEY_a:
            if (!(event->state&CONTROLMASK))
            	goto ins;
            handle(this,FXSEL(SEL_COMMAND,ID_SELECT_ALL),NULL);
            return 1;
        case KEY_c:
            if (!(event->state&CONTROLMASK))
            	goto ins;
        case KEY_F16:                             // Sun Copy key
            handle(this,FXSEL(SEL_COMMAND,ID_COPY_SEL),NULL);
            return 1;
        default:
ins:
            if ((event->state&(CONTROLMASK|ALTMASK)) || ((unsigned char)event->text[0]<32))
            	return 0;
            return 1;
        }
    }
    return 0;
}


// Key Release
long TextLabel::onKeyRelease(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    if (isEnabled())
    {
        if (target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr))
        	return 1;
        switch (event->code)
        {
        case KEY_Right:
        case KEY_KP_Right:
        case KEY_Left:
        case KEY_KP_Left:
        case KEY_Home:
        case KEY_KP_Home:
        case KEY_End:
        case KEY_KP_End:
        case KEY_Insert:
        case KEY_KP_Insert:
        case KEY_Delete:
        case KEY_KP_Delete:
        case KEY_BackSpace:
        case KEY_Return:
        case KEY_F20:                             // Sun Cut key
        case KEY_F16:                             // Sun Copy key
        case KEY_F18:                             // Sun Paste key
            return 1;
        case KEY_a:
        case KEY_x:
        case KEY_c:
        case KEY_v:
            if (event->state&CONTROLMASK)
            	return 1;
        default:
            if ((event->state&(CONTROLMASK|ALTMASK)) || ((unsigned char)event->text[0]<32))
            	return 0;
            return 1;
        }
    }
    return 0;
}


// Kill the selection
FXbool TextLabel::killSelection()
{
    if (hasSelection())
    {
        releaseSelection();
        update(border,border,width-(border<<1),height-(border<<1));
        return TRUE;
    }
    return FALSE;
}


// Select all text
FXbool TextLabel::selectAll()
{
    setAnchorPos(0);
    setCursorPos(contents.length());
    extendSelection(cursor);
    return TRUE;
}


// Set selection
FXbool TextLabel::setSelection(int pos,int len)
{
    setAnchorPos(pos);
    setCursorPos(pos+len);
    extendSelection(cursor);
    return TRUE;
}


// Extend selection
FXbool TextLabel::extendSelection(int pos)
{
	// Don't select text if ncols=0
	if (columns==0)
		return TRUE;
		
	FXDragType types[4];

    // Validate position to start of character
    pos=contents.validate(FXCLAMP(0,pos,contents.length()));

    // Got a selection at all?
    if (anchor!=pos)
    {
        types[0]=stringType;
        types[1]=textType;
        types[2]=utf8Type;
        types[3]=utf16Type;
        if (!hasSelection())
            acquireSelection(types,4);
    }
    else
    {
        if (hasSelection())
            releaseSelection();
    }

    update(border,border,width-(border<<1),height-(border<<1));
    return TRUE;
}


// Change the text and move cursor to end
void TextLabel::setText(const FXString& text,FXbool notify)
{
    killSelection();
    if (contents!=text)
    {
        contents=text;
        anchor=contents.length();
        cursor=contents.length();
        if (xid)
        	layout();
        if (notify && target)
            target->tryHandle(this,FXSEL(SEL_COMMAND,message),(void*)contents.text());
    }
}


// Set text color
void TextLabel::setTextColor(FXColor clr)
{
    if (textColor!=clr)
    {
        textColor=clr;
        update();
    }
}


// Set select background color
void TextLabel::setSelBackColor(FXColor clr)
{
    if (selbackColor!=clr)
    {
        selbackColor=clr;
        update();
    }
}


// Set selected text color
void TextLabel::setSelTextColor(FXColor clr)
{
    if (seltextColor!=clr)
    {
        seltextColor=clr;
        update();
    }
}


// Set cursor color
void TextLabel::setCursorColor(FXColor clr)
{
    if (clr!=cursorColor)
    {
        cursorColor=clr;
        update();
    }
}


// Change number of columns
void TextLabel::setNumColumns(int ncols)
{
    if (ncols<0)
    	ncols=0;
    if (columns!=ncols)
    {
        shift=0;
        columns=ncols;
        layout();   // This may not be necessary!
        recalc();
        update();
    }
}


// Set text justify style
void TextLabel::setJustify(unsigned int style)
{
    unsigned int opts=(options&~JUSTIFY_MASK) | (style&JUSTIFY_MASK);
    if (options!=opts)
    {
        shift=0;
        options=opts;
        recalc();
        update();
    }
}


// Get text justify style
unsigned int TextLabel::getJustify() const
{
    return (options&JUSTIFY_MASK);
}


// Clean up
TextLabel::~TextLabel()
{
    getApp()->removeTimeout(this,ID_AUTOSCROLL);
    font=(FXFont*)-1L;
}

