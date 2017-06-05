// This file contains all FOX functions redefinitions (FOX hacks for various purposes)

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#ifdef HAVE_XFT_H
#include <X11/Xft/Xft.h>
#endif

extern FXbool file_tooltips;
extern FXString xdgconfighome;


// Hack to fix issues with drag and drop within, from and to the dirList
#define SELECT_MASK         (TREELIST_SINGLESELECT|TREELIST_BROWSESELECT)

// Remove all siblings from [fm,to]
void FXTreeList::removeItems(FXTreeItem* fm,FXTreeItem* to,FXbool notify)
{
    register FXTreeItem *olditem=currentitem;
    register FXTreeItem *prv;
    register FXTreeItem *nxt;
    register FXTreeItem *par;
    if(fm && to)
    {
        if(fm->parent!=to->parent)
            fxerror("%s::removeItems: arguments have different parent.\n",getClassName());

        // Delete items
        while(1)
        {
            // Scan till end
            while(to->last) to=to->last;
            do
            {
                // Notify item will be deleted
                if(notify && target)
                    target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)to);

                // Remember hookups
                nxt=to->next;
                prv=to->prev;
                par=to->parent;

                // !!! Hack to go back to the parent when an item disappeared

                // Adjust pointers; suggested by Alan Ott <ott@acusoft.com>
                //if(anchoritem==to){ anchoritem=par; if(prv) anchoritem=prv; if(nxt) anchoritem=nxt; }
                //if(currentitem==to){ currentitem=par; if(prv) currentitem=prv; if(nxt) currentitem=nxt; }
                //if(extentitem==to){ extentitem=par; if(prv) extentitem=prv; if(nxt) extentitem=nxt; }
                //if(viewableitem==to){ viewableitem=par; if(prv) viewableitem=prv; if(nxt) viewableitem=nxt; }
                anchoritem=par;
                currentitem=par;
                extentitem=par;
                viewableitem=par;

                // !!! End of hack

                // Remove item from list
                if(prv) prv->next=nxt;
                else if(par) par->first=nxt;
                else firstitem=nxt;
                if(nxt) nxt->prev=prv;
                else if(par) par->last=prv;
                else lastitem=prv;

                // Delete it
                delete to;

                // Was last one?
                if(to==fm) goto x;
                to=par;
            }
            while(!prv);
            to=prv;
        }

        // Current item has changed
x:
        if(olditem!=currentitem)
        {
            if(notify && target)
                target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)currentitem);
        }

        // Deleted current item
        if(currentitem && currentitem!=olditem)
        {
            if(hasFocus())
                currentitem->setFocus(TRUE);
            if((options&SELECT_MASK)==TREELIST_BROWSESELECT && currentitem->isEnabled())
                selectItem(currentitem,notify);
        }

        // Redo layout
        recalc();
    }
}


// Hack to display a tooltip with name, size, date, etc.
// We were asked about tip text
long FXTreeList::onQueryTip(FXObject* sender,FXSelector sel,void* ptr)
{
    if (FXWindow::onQueryTip(sender,sel,ptr))
        return 1;

    // File tooltips are optional
    if (file_tooltips)
    {
        if ((flags&FLAG_TIP) && !(options&TREELIST_AUTOSELECT)) // No tip when autoselect!
        {
            int x,y;
            unsigned int buttons;

            getCursorPosition(x,y,buttons);
            DirItem *item=(DirItem*)getItemAt(x,y);
            if (item)
            {
                // !!! Hack to display a tooltip with name, size, date, etc.
                FXString string;

                // Root folder
                if (item->getText()==ROOTDIR)
                    string=_("Root directory");

                // Other folders
                else
                {
                    // Get tooltip data
                    FXString str=item->getTooltipData();
                    if (str=="")
                        return 0;

                    // Add name, type, permissions, etc. to the tool tip
                    FXString name=str.section('\t',0);
                    FXString type=str.section('\t',1);
                    FXString date=str.section('\t',2);
                    FXString user=str.section('\t',3);
                    FXString group=str.section('\t',4);
                    FXString perms=str.section('\t',5);
                    FXString deldate=str.section('\t',6);
                    FXString pathname=str.section('\t',7);

                    // Compute root file size
                    FXulong dnsize;
                    char dsize[64];
                    dnsize=::dirsize(pathname.text());
                    snprintf(dsize,sizeof(dsize)-1,"%llu",dnsize);
                    FXString size=::hSize(dsize);
                    if (deldate.empty())
                        string=_("Name: ")+name+"\n"+_("Size in root: ")+size+"\n"+_("Type: ")+type
                               +"\n"+_("Modified date: ")+date+"\n"+_("User: ")+user+" - "+_("Group: ")+group
                               +"\n"+_("Permissions: ")+perms;
                    else
                        string=_("Name: ")+name+"\n"+_("Size in root: ")+size+"\n"+_("Type: ")+type
                               +"\n"+_("Modified date: ")+date+"\n"+_("Deletion date: ")+deldate+"\n"+_("User: ")+user+" - "+_("Group: ")+group
                               +"\n"+_("Permissions: ")+perms;
                }
                // !!! End of hack !!!

                sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&string);
                return 1;
            }
        }
    }
    return 0;
}


//
// Hack of FXDCWindow
//

#define DISPLAY(app) ((Display*)((app)->display))
#define FS ((XFontStruct*)(font->font))

#ifndef HAVE_XFT_H
static int utf2db(XChar2b *dst,const char *src,int n)
{
    register int len,p;
    register FXwchar w;
    for (p=len=0; p<n; p+=wclen(src+p),len++)
    {
        w=wc(src+p);
        dst[len].byte1=(w>>8);
        dst[len].byte2=(w&255);
    }
    return len;
}
#endif


// Hack to take into account non UTF-8 strings
void FXDCWindow::drawText(int x,int y,const char* string,unsigned int length)
{
    if (!surface)
        fxerror("FXDCWindow::drawText: DC not connected to drawable.\n");
    if (!font)
        fxerror("FXDCWindow::drawText: no font selected.\n");

#ifdef HAVE_XFT_H
    XftColor color;
    color.pixel=devfg;
    color.color.red=FXREDVAL(fg)*257;
    color.color.green=FXGREENVAL(fg)*257;
    color.color.blue=FXBLUEVAL(fg)*257;
    color.color.alpha=FXALPHAVAL(fg)*257;

    // !!! Hack to draw string depending on its encoding !!!
    if (isUtf8(string,length))
        XftDrawStringUtf8((XftDraw*)xftDraw,&color,(XftFont*)font->font,x,y,(const FcChar8*)string,length);
    else
        XftDrawString8((XftDraw*)xftDraw,&color,(XftFont*)font->font,x,y,(const FcChar8*)string,length);
    // !!! End of hack !!!
#else
    register int count,escapement,defwidth,ww,size,i;
    register double ang,ux,uy;
    register unsigned char r,c;
    XChar2b sbuffer[4096];
    count=utf2db(sbuffer,string,FXMIN(length,4096));
    if (font->getAngle())
    {
        ang=font->getAngle()*0.00027270769562411399179;
        defwidth=FS->min_bounds.width;
        ux=cos(ang);
        uy=sin(ang);
        if (FS->per_char)
        {
            r=FS->default_char>>8;
            c=FS->default_char&255;
            size=(FS->max_char_or_byte2-FS->min_char_or_byte2+1);
            if (FS->min_char_or_byte2<=c && c<=FS->max_char_or_byte2 && FS->min_byte1<=r && r<=FS->max_byte1)
                defwidth=FS->per_char[(r-FS->min_byte1)*size+(c-FS->min_char_or_byte2)].width;
            for (i=escapement=0; i<count; i++)
            {
                XDrawString16(DISPLAY(getApp()),surface->id(),(GC)ctx,(int)(x+escapement*ux),(int)(y-escapement*uy),&sbuffer[i],1);
                r=sbuffer[i].byte1;
                c=sbuffer[i].byte2;
                escapement+=defwidth;
                if (FS->min_char_or_byte2<=c && c<=FS->max_char_or_byte2 && FS->min_byte1<=r && r<=FS->max_byte1)
                    if ((ww=FS->per_char[(r-FS->min_byte1)*size+(c-FS->min_char_or_byte2)].width)!=0) escapement+=ww-defwidth;
            }
        }
        else
        {
            for (i=escapement=0; i<count; i++)
            {
                XDrawString16(DISPLAY(getApp()),surface->id(),(GC)ctx,(int)(x+escapement*ux),(int)(y-escapement*uy),&sbuffer[i],1);
                escapement+=defwidth;
            }
        }
    }
    else
        XDrawString16(DISPLAY(getApp()),surface->id(),(GC)ctx,x,y,sbuffer,count);
#endif
}


//
// Hack of FXFont
//

// Hack to take into account non UTF-8 strings
int FXFont::getTextWidth(const char *string,unsigned int length) const
{
    if (!string && length)
        fxerror("%s::getTextWidth: NULL string argument\n",getClassName());

    if (font)
    {
#ifdef HAVE_XFT_H
        XGlyphInfo extents;
        // This returns rotated metrics; FOX likes to work with unrotated metrics, so if angle
        // is not 0, we calculate the unrotated baseline; note however that the calculation is
        // not 100% pixel exact when the angle is not a multiple of 90 degrees.

        // !!! Hack to evaluate string extent depending on its encoding !!!
        if (isUtf8(string,length))
            XftTextExtentsUtf8(DISPLAY(getApp()),(XftFont*)font,(const FcChar8*)string,length,&extents);
        else
            XftTextExtents8(DISPLAY(getApp()),(XftFont*)font,(const FcChar8*)string,length,&extents);
        // !!! End of hack !!!

        if (angle)
            return (int)(0.5+sqrt(extents.xOff*extents.xOff+extents.yOff*extents.yOff));

        return extents.xOff;
#else
        register const XFontStruct *fs=(XFontStruct*)font;
        register int defwidth=fs->min_bounds.width;
        register int width=0,ww;
        register unsigned int p=0;
        register unsigned int s;
        register unsigned char r;
        register unsigned char c;
        register FXwchar w;
        if (fs->per_char)
        {
            r=fs->default_char>>8;
            c=fs->default_char&255;
            s=(fs->max_char_or_byte2-fs->min_char_or_byte2+1);
            if (fs->min_char_or_byte2<=c && c<=fs->max_char_or_byte2 && fs->min_byte1<=r && r<=fs->max_byte1)
                defwidth=fs->per_char[(r-fs->min_byte1)*s+(c-fs->min_char_or_byte2)].width;
            while (p<length)
            {
                w=wc(string+p);
                p+=wclen(string+p);
                r=w>>8;
                c=w&255;
                if (fs->min_char_or_byte2<=c && c<=fs->max_char_or_byte2 && fs->min_byte1<=r && r<=fs->max_byte1)
                {
                    if ((ww=fs->per_char[(r-fs->min_byte1)*s+(c-fs->min_char_or_byte2)].width)!=0)
                    {
                        width+=ww;
                        continue;
                    }
                }
                width+=defwidth;
            }
        }
        else
        {
            while (p<length)
            {
                p+=wclen(string+p);
                width+=defwidth;
            }
        }
        return width;
#endif
    }
    return length;
}


//
// Hack of FXSplitter
//
// NB : - MIN_PANEL_WIDTH is defined in xfedefs.h
//      - Don't use LAYOUT_FIX_WIDTH with this hack because it won't work!
// This function is taken from the FXSplitter class
// and hacked to set a minimum splitter width when moving splitter to right
// It replaces the normal function...
void FXSplitter::moveHSplit(int pos)
{
    register int smin,smax;
    register unsigned int hints;
    FXASSERT(window);
    hints=window->getLayoutHints();
    // !!! Hack to limit the width to a minimum value !!!
    if (options&SPLITTER_REVERSED)
    {
        smin=barsize;
        smax=window->getX()+window->getWidth();
    }
    else
    {
        smin=window->getX();
        smax=width-barsize;
    }
    smax=smax-MIN_PANEL_WIDTH;
    smin=smin+MIN_PANEL_WIDTH;
    split=pos;
    if (split<smin)
        split=smin;
    if (split>smax)
        split=smax;
    // !!! End of hack
}

void FXSplitter::moveVSplit(int pos)
{
    register int smin,smax;
    register unsigned int hints;
    FXASSERT(window);
    hints=window->getLayoutHints();
    if (options&SPLITTER_REVERSED)
    {
        smin=barsize;
        smax=window->getY()+window->getHeight();
    }
    else
    {
        smin=window->getY();
        smax=height-barsize;
    }
    smax=smax-MIN_PANEL_WIDTH;
    smin=smin+MIN_PANEL_WIDTH;
    split=pos;
    if (split<smin)
        split=smin;
    if (split>smax)
        split=smax;
}


//
// Hack of FXRegistry
//

// Hack to change the defaults directories for config files and icons
// The vendor key is not used anymore

#define DESKTOP        "xferc"
#define REGISTRYPATH   "/etc:/usr/share:/usr/local/share"

// Read registry
bool FXRegistry::read()
{
    FXString dirname;
    register bool ok=false;

    dirname=FXPath::search(REGISTRYPATH,"xfe");
    if (!dirname.empty())
        ok=readFromDir(dirname,false);

    // Try search along PATH if still not found
    if (!ok)
    {
        dirname=FXPath::search(FXSystem::getExecPath(),"xfe");
        if (!dirname.empty())
            ok=readFromDir(dirname,false);
    }

    // Get path to per-user settings directory
    dirname=xdgconfighome + PATHSEPSTRING XFECONFIGPATH;

    // Then read per-user settings; overriding system-wide ones
    if (readFromDir(dirname,true))
        ok=true;

    return ok;
}


// Try read registry from directory
bool FXRegistry::readFromDir(const FXString& dirname,bool mark)
{
    bool ok=false;

    // Directory is empty?
    if (!dirname.empty())
    {
        // First try to load desktop registry
        if (parseFile(dirname+PATHSEPSTRING DESKTOP,false))
        {
            FXString nn=dirname+PATHSEPSTRING DESKTOP;
            ok=true;
        }

        // Have application key
        if (!applicationkey.empty())
        {
            if (parseFile(dirname+PATHSEPSTRING+applicationkey + "rc",mark))
                ok=true;
        }
    }
    return ok;
}


// Write registry
bool FXRegistry::write()
{
    FXString pathname,tempname;

    // Settings have not changed
    if (!isModified()) return true;

    // We can not save if no application key given
    if (!applicationkey.empty())
    {
        // Changes written only in the per-user registry
        pathname=xdgconfighome + PATHSEPSTRING XFECONFIGPATH;

        // If this directory does not exist, make it
        if (!FXStat::exists(pathname))
        {
            if (!FXDir::create(pathname))
                return false;
        }
        else
        {
            if (!FXStat::isDirectory(pathname))
                return false;
        }

        // Add application key
        pathname.append(PATHSEPSTRING+applicationkey+"rc");

        // Construct temp name
        tempname.format("%s_%d",pathname.text(),fxgetpid());

        // Unparse settings into temp file first
        if (unparseFile(tempname))
        {

            // Rename ATOMICALLY to proper name
            if (!FXFile::rename(tempname,pathname))
                return false;

            setModified(false);
            return true;
        }
    }
    return false;
}



//
// Hack of FXPopup
//

// The two functions below are taken from the FXPopup class
// and hacked to allow navigating using the keyboard on popup menus
// They replace the normal functions...

// !!! Global variable control keyboard scrolling on right click popup menus !!!
extern FXbool allowPopupScroll;

void FXPopup::setFocus()
{
    FXShell::setFocus();

    // !!! Hack to allow keyboard scroll on popup dialogs !!!
    if (allowPopupScroll)
        grabKeyboard();
}

void FXPopup::killFocus()
{
    FXShell::killFocus();

    // !!! Hack to allow keyboard scroll on popup dialogs !!!
    if (allowPopupScroll)
    {
        if (prevActive)
            prevActive->setFocus();
        else
            ungrabKeyboard();
    }

}


//
// Hack of FXStatusLine(translation hack)
//

// Status line construct and init
FXStatusLine::FXStatusLine(FXComposite* p,FXObject* tgt,FXSelector sel):
    FXFrame(p,LAYOUT_LEFT|LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0, 4,4,2,2)
{
    flags|=FLAG_SHOWN;
    status=normal=_("Ready.");
    font=getApp()->getNormalFont();
    textColor=getApp()->getForeColor();
    textHighlightColor=getApp()->getForeColor();
    target=tgt;
    message=sel;
}


//
// Hack of FXReplaceDialog
//

// Taken from the FXReplaceDialog class
// - translation hack
// - small hack for the Clearlooks theme

// Padding for buttons
#define HORZ_PAD      12
#define VERT_PAD      2
#define SEARCH_MASK   (SEARCH_EXACT|SEARCH_IGNORECASE|SEARCH_REGEX)

// File Open Dialog
FXReplaceDialog::FXReplaceDialog(FXWindow* owner,const FXString& caption,FXIcon* ic,unsigned int opts,int x,int y,int w,int h):
    FXDialogBox(owner,caption,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,x,y,w,h,10,10,10,10, 10,10)
{
    FXHorizontalFrame* buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH|PACK_UNIFORM_HEIGHT,0,0,0,0,0,0,0,0);
    accept=new FXButton(buttons,_("&Replace"),NULL,this,ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    every=new FXButton(buttons,_("Re&place All"),NULL,this,ID_ALL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,6,6,VERT_PAD,VERT_PAD);
    cancel=new FXButton(buttons,_("&Cancel"),NULL,this,ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    FXHorizontalFrame* pair=new FXHorizontalFrame(buttons,LAYOUT_FILL_Y|LAYOUT_RIGHT,0,0,0,0, 0,0,0,0);
    FXArrowButton* searchlast=new FXArrowButton(pair,this,ID_PREV,ARROW_LEFT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    FXArrowButton* searchnext=new FXArrowButton(pair,this,ID_NEXT,ARROW_RIGHT|FRAME_RAISED|FRAME_THICK|LAYOUT_FILL_Y,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    new FXHorizontalSeparator(this,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
    FXHorizontalFrame* toppart=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0, 10,10);
    new FXLabel(toppart,FXString::null,ic,ICON_BEFORE_TEXT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_Y|LAYOUT_FILL_X);
    FXVerticalFrame* entry=new FXVerticalFrame(toppart,LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0);
    searchlabel=new FXLabel(entry,_("Search for:"),NULL,JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X);

    // !!! Hack to remove the FRAME_THICK option (required for the Clearlooks theme)
    searchbox=new FXHorizontalFrame(entry,FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0, 0,0);
    searchtext=new FXTextField(searchbox,26,this,ID_SEARCH_TEXT,FRAME_SUNKEN|TEXTFIELD_ENTER_ONLY|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 4,4,4,4);
    // !!! End of hack

    FXVerticalFrame* searcharrows=new FXVerticalFrame(searchbox,LAYOUT_RIGHT|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
    FXArrowButton* ar1=new FXArrowButton(searcharrows,this,ID_SEARCH_UP,FRAME_RAISED|FRAME_THICK|ARROW_UP|ARROW_REPEAT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH, 0,0,16,0, 1,1,1,1);
    FXArrowButton* ar2=new FXArrowButton(searcharrows,this,ID_SEARCH_DN,FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_REPEAT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH, 0,0,16,0, 1,1,1,1);
    ar1->setArrowSize(3);
    ar2->setArrowSize(3);
    replacelabel=new FXLabel(entry,_("Replace with:"),NULL,LAYOUT_LEFT);

    // !!! Hack to remove the FRAME_THICK option (required for the Clearlooks theme)
    replacebox=new FXHorizontalFrame(entry,FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0, 0,0);
    replacetext=new FXTextField(replacebox,26,this,ID_REPLACE_TEXT,FRAME_SUNKEN|TEXTFIELD_ENTER_ONLY|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 4,4,4,4);
    // !!! End of hack

    FXVerticalFrame* replacearrows=new FXVerticalFrame(replacebox,LAYOUT_RIGHT|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
    FXArrowButton* ar3=new FXArrowButton(replacearrows,this,ID_REPLACE_UP,FRAME_RAISED|FRAME_THICK|ARROW_UP|ARROW_REPEAT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH, 0,0,16,0, 1,1,1,1);
    FXArrowButton* ar4=new FXArrowButton(replacearrows,this,ID_REPLACE_DN,FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_REPEAT|LAYOUT_FILL_Y|LAYOUT_FIX_WIDTH, 0,0,16,0, 1,1,1,1);
    ar3->setArrowSize(3);
    ar4->setArrowSize(3);
    FXHorizontalFrame* options1=new FXHorizontalFrame(entry,LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);
    new FXRadioButton(options1,_("Ex&act"),this,ID_MODE+SEARCH_EXACT,ICON_BEFORE_TEXT|LAYOUT_CENTER_X);
    new FXRadioButton(options1,_("&Ignore Case"),this,ID_MODE+SEARCH_IGNORECASE,ICON_BEFORE_TEXT|LAYOUT_CENTER_X);
    new FXRadioButton(options1,_("E&xpression"),this,ID_MODE+SEARCH_REGEX,ICON_BEFORE_TEXT|LAYOUT_CENTER_X);
    new FXCheckButton(options1,_("&Backward"),this,ID_DIR,ICON_BEFORE_TEXT|LAYOUT_CENTER_X);
    searchlast->setTipText("Ctrl-B");
    searchnext->setTipText("Ctrl-F");
    searchlast->addHotKey(MKUINT(KEY_b,CONTROLMASK));
    searchnext->addHotKey(MKUINT(KEY_f,CONTROLMASK));
    searchmode=SEARCH_EXACT|SEARCH_FORWARD;
    current=0;
}

//
// Hack of FXSearchDialog (translation hack)
//

// Taken from the FXSearchDialog class
FXSearchDialog::FXSearchDialog(FXWindow* owner,const FXString& caption,FXIcon* ic,unsigned int opts,int x,int y,int w,int h):
    FXReplaceDialog(owner,caption,ic,opts,x,y,w,h)
{
    accept->setText(_("&Search"));
    every->hide();
    replacelabel->hide();
    replacebox->hide();
}

//
// Hack of FXInputDialog (translation hack)
//

// Taken from the FXInputDialog class
void FXInputDialog::initialize(const FXString& label,FXIcon* icon)
{
    unsigned int textopts=TEXTFIELD_ENTER_ONLY|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X;
    FXHorizontalFrame* buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0,0,0,0,0);
    new FXButton(buttons,_("&OK"),NULL,this,ID_ACCEPT,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    new FXButton(buttons,_("&Cancel"),NULL,this,ID_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_RIGHT,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    new FXHorizontalSeparator(this,SEPARATOR_GROOVE|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
    FXHorizontalFrame* toppart=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0, 10,10);
    new FXLabel(toppart,FXString::null,icon,ICON_BEFORE_TEXT|JUSTIFY_CENTER_X|JUSTIFY_CENTER_Y|LAYOUT_FILL_Y|LAYOUT_FILL_X);
    FXVerticalFrame* entry=new FXVerticalFrame(toppart,LAYOUT_FILL_X|LAYOUT_CENTER_Y,0,0,0,0, 0,0,0,0);
    new FXLabel(entry,label,NULL,JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X);
    if (options&INPUTDIALOG_PASSWORD)
        textopts|=TEXTFIELD_PASSWD;
    if (options&INPUTDIALOG_INTEGER)
        textopts|=TEXTFIELD_INTEGER|JUSTIFY_RIGHT;
    if (options&INPUTDIALOG_REAL)
        textopts|=TEXTFIELD_REAL|JUSTIFY_RIGHT;
    input=new FXTextField(entry,20,this,ID_ACCEPT,textopts,0,0,0,0, 8,8,4,4);
    limlo=1.0;
    limhi=0.0;
}



//
// Hack of fxpriv (clipboard management)
//


// These two functions are hacked to reduce the timeout when the owner app of the clipboard has been closed

// Send request for selection info
Atom fxsendrequest(Display *display,Window window,Atom selection,Atom prop,Atom type,unsigned int time)
{
    // !!! Hack here to reduce timeout !!!
    unsigned int loops=10;
    XEvent ev;
    XConvertSelection(display,selection,type,prop,window,time);
    while (!XCheckTypedWindowEvent(display,window,SelectionNotify,&ev))
    {
        if (loops==0)
        {
            //fxwarning("fxsendrequest:timed out!\n");
            return None;
        }
        FXThread::sleep(10000000);  // Don't burn too much CPU here:- the other guy needs it more....
        loops--;
    }
    return ev.xselection.property;
}


// Wait for event of certain type
static FXbool fxwaitforevent(Display *display,Window window,int type,XEvent& event)
{
    // !!! Hack here to reduce timeout !!!
    unsigned int loops=10;
    while (!XCheckTypedWindowEvent(display,window,type,&event))
    {
        if (loops==0)
        {
            //fxwarning("fxwaitforevent:timed out!\n");
            return FALSE;
        }
        FXThread::sleep(10000000);  // Don't burn too much CPU here:- the other guy needs it more....
        loops--;
    }
    return TRUE;
}


// The four following functions are not modified but are necessary here because the previous ones are not called directly

// Read property in chunks smaller than maximum transfer length,
// appending to data array; returns amount read from the property.
static unsigned int fxrecvprop(Display *display,Window window,Atom prop,Atom& type,unsigned char*& data,unsigned int& size)
{
    unsigned long maxtfrsize=XMaxRequestSize(display)*4;
    unsigned long tfroffset,tfrsize,tfrleft;
    unsigned char *ptr;
    int format;
    tfroffset=0;

    // Read next chunk of data from property
    while (XGetWindowProperty(display,window,prop,tfroffset>>2,maxtfrsize>>2,False,AnyPropertyType,&type,&format,&tfrsize,&tfrleft,&ptr)==Success && type!=None)
    {
        tfrsize*=(format>>3);

        // Grow the array to accomodate new data
        if (!FXRESIZE(&data,unsigned char,size+tfrsize+1))
        {
            XFree(ptr);
            break;
        }

        // Append new data at the end, plus the extra 0.
        memcpy(&data[size],ptr,tfrsize+1);
        size+=tfrsize;
        tfroffset+=tfrsize;
        XFree(ptr);
        if (tfrleft==0)
            break;
    }

    // Delete property after we're done
    XDeleteProperty(display,window,prop);
    XFlush(display);
    return tfroffset;
}


// Receive data via property
Atom fxrecvdata(Display *display,Window window,Atom prop,Atom incr,Atom& type,unsigned char*& data,unsigned int& size)
{
    unsigned long  tfrsize,tfrleft;
    unsigned char *ptr;
    XEvent ev;
    int format;
    data=NULL;
    size=0;
    if (prop)
    {
        // First, see what we've got
        if (XGetWindowProperty(display,window,prop,0,0,False,AnyPropertyType,&type,&format,&tfrsize,&tfrleft,&ptr)==Success && type!=None)
        {
            XFree(ptr);

            // Incremental transfer
            if (type==incr)
            {
                // Delete the INCR property
                XDeleteProperty(display,window,prop);
                XFlush(display);

                // Wait for the next batch of data
                while (fxwaitforevent(display,window,PropertyNotify,ev))
                {
                    // Wrong type of notify event; perhaps stale event
                    if (ev.xproperty.atom!=prop || ev.xproperty.state!=PropertyNewValue)
                        continue;

                    // See what we've got
                    if (XGetWindowProperty(display,window,prop,0,0,False,AnyPropertyType,&type,&format,&tfrsize,&tfrleft,&ptr)==Success && type!=None)
                    {
                        XFree(ptr);

                        // if empty property, its the last one
                        if (tfrleft==0)
                        {
                            // Delete property so the other side knows we've got the data
                            XDeleteProperty(display,window,prop);
                            XFlush(display);
                            break;
                        }

                        // Read and delete the property
                        fxrecvprop(display,window,prop,type,data,size);
                    }
                }
            }

            // All data in one shot
            else
            {
                // Read and delete the property
                fxrecvprop(display,window,prop,type,data,size);
            }
        }
        return prop;
    }
    return None;
}


// Retrieve CLIPBOARD selection data
void FXApp::clipboardGetData(const FXWindow* window,FXDragType type,unsigned char*& data,unsigned int& size)
{
    FXID answer;
    data=NULL;
    size=0;
    if (clipboardWindow)
    {
        event.type=SEL_CLIPBOARD_REQUEST;
        event.target=type;
        ddeData=NULL;
        ddeSize=0;
        clipboardWindow->handle(this,FXSEL(SEL_CLIPBOARD_REQUEST,0),&event);
        data=ddeData;
        size=ddeSize;
        ddeData=NULL;
        ddeSize=0;
    }
    else
    {
        answer=fxsendrequest((Display*)display,window->id(),xcbSelection,ddeAtom,type,event.time);
        fxrecvdata((Display*)display,window->id(),answer,ddeIncr,type,data,size);
    }
}


// Get dropped data; called in response to DND enter or DND drop
bool FXWindow::getDNDData(FXDNDOrigin origin,FXDragType targettype,unsigned char*& data,unsigned int& size) const
{
    if (xid==0)
        fxerror("%s::getDNDData: window has not yet been created.\n",getClassName());

    switch (origin)
    {
    case FROM_DRAGNDROP:
        getApp()->dragdropGetData(this,targettype,data,size);
        break;
    case FROM_CLIPBOARD:
        getApp()->clipboardGetData(this,targettype,data,size);
        break;
    case FROM_SELECTION:
        getApp()->selectionGetData(this,targettype,data,size);
        break;
    }
    return data!=NULL;
}




//
// Hack of FXButton (button with gradient effect and rounded corners)
// Original author : Sander Jansen <sander@knology.net>
//


// Draw rectangle with gradient effect
// Default is vertical gradient
static void drawGradientRectangle(FXDC& dc,FXColor upper,FXColor lower,int x,int y,int w,int h, FXbool vert=TRUE)
{
    register int rr,gg,bb,dr,dg,db,r1,g1,b1,r2,g2,b2,yl,yh,yy,dy,n,t,ww;
    const int MAXSTEPS=128;
    if (0<w && 0<h)
    {
        // Horizontal gradient : exchange w and h
        if (!vert)
        {
            ww=w;
            w=h;
            h=ww;
        }

        dc.setStipple(STIPPLE_NONE);
        dc.setFillStyle(FILL_SOLID);

        r1=FXREDVAL(upper);
        r2=FXREDVAL(lower);
        dr=r2-r1;
        g1=FXGREENVAL(upper);
        g2=FXGREENVAL(lower);
        dg=g2-g1;
        b1=FXBLUEVAL(upper);
        b2=FXBLUEVAL(lower);
        db=b2-b1;

        n=FXABS(dr);
        if ((t=FXABS(dg))>n)
            n=t;
        if ((t=FXABS(db))>n)
            n=t;
        n++;
        if (n>h)
            n=h;
        if (n>MAXSTEPS)
            n=MAXSTEPS;
        rr=(r1<<16)+32767;
        gg=(g1<<16)+32767;
        bb=(b1<<16)+32767;
        yy=32767;

        dr=(dr<<16)/n;
        dg=(dg<<16)/n;
        db=(db<<16)/n;
        dy=(h<<16)/n;

        do
        {
            yl=yy>>16;
            yy+=dy;
            yh=yy>>16;
            dc.setForeground(FXRGB(rr>>16,gg>>16,bb>>16));

            // Vertical gradient
            if (vert)
                dc.fillRectangle(x,y+yl,w,yh-yl);

            // Horizontal gradient
            else
                dc.fillRectangle(x+yl,y,yh-yl,w);

            rr+=dr;
            gg+=dg;
            bb+=db;
        }
        while (yh<h);
    }
}



// Some macros to simplify the code
// They draw a button in Standard or Clearlooks mode, in up or down state


#define DRAW_CLEARLOOKS_BUTTON_UP                                      \
dc.setForeground(backColor);                                           \
dc.drawPoints(basebackground,4);                                       \
                                                                       \
dc.setForeground(bordercolor);                                         \
dc.drawRectangle(2,0,width-5,0);                                       \
dc.drawRectangle(2,height-1,width-5,height-1);                         \
dc.drawRectangle(0,2,0,height-5);                                      \
dc.drawRectangle(width-1,2,0,height-5);                                \
dc.drawPoints(bordercorners,4);                                        \
dc.setForeground(shadecolor);                                          \
dc.drawPoints(bordershade,16);                                         \
                                                                       \
drawGradientRectangle(dc,topcolor,bottomcolor,2,1,width-4,height-2);   \
dc.setForeground(topcolor);                                            \
dc.drawRectangle(1,3,0,height-7);                                      \
dc.setForeground(bottomcolor);                                         \
dc.drawRectangle(width-2,3,0,height-7);


#define DRAW_CLEARLOOKS_BUTTON_DOWN                                    \
dc.setForeground(shadecolor);                                          \
dc.fillRectangle(0,0,width,height);                                    \
                                                                       \
dc.setForeground(backColor);                                           \
dc.drawPoints(basebackground,4);                                       \
                                                                       \
dc.setForeground(bordercolor);                                         \
dc.drawRectangle(2,0,width-5,0);                                       \
dc.drawRectangle(2,height-1,width-5,height-1);                         \
dc.drawRectangle(0,2,0,height-5);                                      \
dc.drawRectangle(width-1,2,0,height-5);                                \
dc.drawPoints(bordercorners,4);                                        \
dc.setForeground(shadecolor);                                          \
dc.drawPoints(bordershade,16);


#define DRAW_STANDARD_BUTTON_UP                                        \
dc.setForeground(backColor);                                           \
dc.fillRectangle(border,border,width-border*2,height-border*2);        \
if (options&FRAME_THICK)                                               \
	drawDoubleRaisedRectangle(dc,0,0,width,height);                    \
else                                                                   \
	drawRaisedRectangle(dc,0,0,width,height);


#define DRAW_STANDARD_BUTTON_DOWN                                      \
dc.setForeground(hiliteColor);                                         \
dc.fillRectangle(border,border,width-border*2,height-border*2);        \
if (options&FRAME_THICK)                                               \
	drawDoubleSunkenRectangle(dc,0,0,width,height);                    \
else                                                                   \
	drawSunkenRectangle(dc,0,0,width,height);


#define INIT_CLEARLOOKS                                                                   \
static FXbool init=TRUE;                                                                  \
static FXbool use_clearlooks=TRUE;                                                        \
static FXColor topcolor, bottomcolor, shadecolor, bordercolor;                            \
                                                                                          \
FXPoint basebackground[4]={FXPoint(0,0),FXPoint(width-1,0),FXPoint(0,height-1),           \
                           FXPoint(width-1,height-1)};                                    \
FXPoint bordershade[16]={FXPoint(0,1),FXPoint(1,0),FXPoint(1,2),FXPoint(2,1),             \
						 FXPoint(width-2,0),FXPoint(width-1,1),FXPoint(width-3,1),        \
						 FXPoint(width-2,2),FXPoint(0,height-2),FXPoint(1,height-1),      \
						 FXPoint(1,height-3),FXPoint(2,height-2),                         \
						 FXPoint(width-1,height-2),FXPoint(width-2,height-1),             \
						 FXPoint(width-2,height-3),FXPoint(width-3,height-2)              \
						};                                                                \
FXPoint bordercorners[4]={FXPoint(1,1),FXPoint(1,height-2),FXPoint(width-2,1),            \
                          FXPoint(width-2,height-2)};                                     \
                                                                                          \
if (init)                                                                                 \
{                                                                                         \
	use_clearlooks=getApp()->reg().readUnsignedEntry("SETTINGS","use_clearlooks",TRUE);   \
                                                                                          \
	if (use_clearlooks)                                                                   \
	{                                                                                     \
		unsigned int r=FXREDVAL(backColor);                                               \
		unsigned int g=FXGREENVAL(backColor);                                             \
		unsigned int b=FXBLUEVAL(backColor);                                              \
                                                                                          \
		topcolor=FXRGB(FXMIN(1.1*r,255),FXMIN(1.1*g,255),FXMIN(1.1*b,255));               \
		bottomcolor=FXRGB(0.9*r,0.9*g,0.9*b);                                             \
		shadecolor=FXRGB(0.9*r,0.9*g,0.9*b);                                              \
		bordercolor=FXRGB(0.5*r,0.5*g,0.5*b);                                             \
	}                                                                                     \
	init=FALSE;                                                                           \
}



// Handle repaint
long FXButton::onPaint(FXObject*,FXSelector,void* ptr)
{
    // Initialise Clearlooks
    INIT_CLEARLOOKS

    FXEvent*ev=(FXEvent*)ptr;
    FXDCWindow dc(this,ev);
    int tw=0,th=0,iw=0,ih=0,tx,ty,ix,iy;

    // Button with nice gradient effect and rounded corners (Clearlooks)
    if (use_clearlooks)
    {
        // Enabled and checked
        if (state==STATE_ENGAGED && options&BUTTON_TOOLBAR)
        {
            DRAW_CLEARLOOKS_BUTTON_UP
        }
        else if (options&BUTTON_TOOLBAR && !underCursor())
        {
            dc.setForeground(backColor);
            dc.fillRectangle(0,0,width,height);
        }
        else if (state==STATE_UP && ((options&BUTTON_TOOLBAR)==0 || (options&BUTTON_TOOLBAR && underCursor())))
        {
            DRAW_CLEARLOOKS_BUTTON_UP
        }
        else
        {
            DRAW_CLEARLOOKS_BUTTON_DOWN
        }

    }	// End of gradient painting

    // Normal flat rectangular button
    else
    {
        // Got a border at all?
        if (options&(FRAME_RAISED|FRAME_SUNKEN))
        {
            // Toolbar style
            if (options&BUTTON_TOOLBAR)
            {
                // Enabled and cursor inside, and up
                if (isEnabled() && underCursor() && (state==STATE_UP))
                {
                    DRAW_STANDARD_BUTTON_UP
                }

                // Enabled and cursor inside and down
                else if (isEnabled() && underCursor() && (state==STATE_DOWN))
                {
                    DRAW_STANDARD_BUTTON_DOWN
                }

                // Enabled and checked
                else if (isEnabled() && (state==STATE_ENGAGED))
                {
                    DRAW_STANDARD_BUTTON_DOWN
                }

                // Disabled or unchecked or not under cursor
                else
                {
                    dc.setForeground(backColor);
                    dc.fillRectangle(0,0,width,height);
                }
            }

            // Normal style
            else
            {
                // Draw in up state if disabled or up
                if (!isEnabled() || (state==STATE_UP))
                {
                    DRAW_STANDARD_BUTTON_UP
                }

                // Draw sunken if enabled and either checked or pressed
                // Caution! This one is different!
                else
                {
                    if (state==STATE_ENGAGED)
                        dc.setForeground(hiliteColor);
                    else
                        dc.setForeground(backColor);
                    dc.fillRectangle(border,border,width-border*2,height-border*2);
                    if (options&FRAME_THICK)
                        drawDoubleSunkenRectangle(dc,0,0,width,height);
                    else
                        drawSunkenRectangle(dc,0,0,width,height);
                }
            }
        }

        // No borders
        else
        {
            if (isEnabled() && (state==STATE_ENGAGED))
            {
                dc.setForeground(hiliteColor);
                dc.fillRectangle(0,0,width,height);
            }
            else
            {
                dc.setForeground(backColor);
                dc.fillRectangle(0,0,width,height);
            }
        }

    }  	// End of normal painting

    // Place text & icon
    if (!label.empty())
    {
        tw=labelWidth(label);
        th=labelHeight(label);
    }
    if (icon)
    {
        iw=icon->getWidth();
        ih=icon->getHeight();
    }

    just_x(tx,ix,tw,iw);
    just_y(ty,iy,th,ih);

    // Shift a bit when pressed
    if (state && (options&(FRAME_RAISED|FRAME_SUNKEN)))
    {
        ++tx;
        ++ty;
        ++ix;
        ++iy;
    }

    // Draw enabled state
    if (isEnabled())
    {
        if (icon)
            dc.drawIcon(icon,ix,iy);
        if (!label.empty())
        {
            dc.setFont(font);
            dc.setForeground(textColor);
            drawLabel(dc,label,hotoff,tx,ty,tw,th);
        }
        if (hasFocus())
            dc.drawFocusRectangle(border+1,border+1,width-2*border-2,height-2*border-2);
    }

    // Draw grayed-out state
    else
    {
        if (icon)
            dc.drawIconSunken(icon,ix,iy);
        if (!label.empty())
        {
            dc.setFont(font);
            dc.setForeground(hiliteColor);
            drawLabel(dc,label,hotoff,tx+1,ty+1,tw,th);
            dc.setForeground(shadowColor);
            drawLabel(dc,label,hotoff,tx,ty,tw,th);
        }
    }

    return 1;
}



//
// Hack of FXTextField
//

// Function taken from the FXTextField class and hacked to get an optional rounded rectangle shape
long FXTextField::onPaint(FXObject*,FXSelector,void* ptr)
{
    // Initialise Clearlooks
    INIT_CLEARLOOKS

    FXEvent *ev=(FXEvent*)ptr;
    FXDCWindow dc(this,ev);

    // Draw frame
    drawFrame(dc,0,0,width,height);

    // Draw background
    dc.setForeground(backColor);
    dc.fillRectangle(border,border,width-(border<<1),height-(border<<1));

    // !!! Hack to get an optional rounded rectangle shape
    if (use_clearlooks)
    {
        // Outside Background
        dc.setForeground(baseColor);
        dc.fillRectangle(0,0,width,height);
        dc.drawPoints(basebackground,4);

        // Border
        dc.setForeground(bordercolor);
        dc.drawRectangle(2,0,width-5,0);
        dc.drawRectangle(2,height-1,width-5,height-1);
        dc.drawRectangle(0,2,0,height-5);
        dc.drawRectangle(width-1,2,0,height-5);
        dc.drawPoints(bordercorners,4);
        dc.setForeground(shadecolor);
        dc.drawPoints(bordershade,16);
        dc.setForeground(backColor);
        dc.fillRectangle(2,1,width-4,height-2);
    }
    // !!! End of hack

    // Draw text, clipped against frame interior
    dc.setClipRectangle(border,border,width-(border<<1),height-(border<<1));
    drawTextRange(dc,0,contents.length());

    // Draw caret
    if (flags&FLAG_CARET)
    {
        int xx=coord(cursor)-1;
        dc.setForeground(cursorColor);
        dc.fillRectangle(xx,padtop+border,1,height-padbottom-padtop-(border<<1));
        dc.fillRectangle(xx-2,padtop+border,5,1);
        dc.fillRectangle(xx-2,height-border-padbottom-1,5,1);
    }

    return 1;
}



//
// Hack of FXToggleButton
//

// Hack to optionally display a button with a nice gradient effect (Clearlooks theme)
long FXToggleButton::onPaint(FXObject*,FXSelector,void* ptr)
{
    // Initialise Clearlooks
    INIT_CLEARLOOKS

    int tw=0,th=0,iw=0,ih=0,tx,ty,ix,iy;
    FXEvent *ev=(FXEvent*)ptr;
    FXDCWindow dc(this,ev);

    // Button with nice gradient effect and rounded corners (Clearlooks)
    if (use_clearlooks)
    {
        // Button style is toolbar
        if (options&TOGGLEBUTTON_TOOLBAR)
        {
            // Enabled and cursor inside and button down
            if (down || ((options&TOGGLEBUTTON_KEEPSTATE) && state))
            {
                DRAW_CLEARLOOKS_BUTTON_DOWN
            }
            // Enabled and cursor inside but button not down
            else if (isEnabled() && underCursor())
            {
                DRAW_CLEARLOOKS_BUTTON_UP
            }

            // Disabled or unchecked or not under cursor
            else
            {
                dc.setForeground(backColor);
                dc.fillRectangle(0,0,width,height);
            }
        }

        // Button style is normal
        else
        {
            // Button down
            if (down || ((options&TOGGLEBUTTON_KEEPSTATE) && state))
            {
                DRAW_CLEARLOOKS_BUTTON_DOWN
            }

            // Button up
            else
            {
                DRAW_CLEARLOOKS_BUTTON_UP
            }
        }

    }	// End of gradient painting

    // Normal flat rectangular button
    else
    {
        // Got a border at all?
        if (options&(FRAME_RAISED|FRAME_SUNKEN))
        {
            // Button style is normal
            if (options&TOGGLEBUTTON_TOOLBAR)
            {
                // Enabled and cursor inside and down
                if (down || ((options&TOGGLEBUTTON_KEEPSTATE) && state))
                {
                    DRAW_STANDARD_BUTTON_DOWN
                }

                // Enabled and cursor inside, and up
                else if (isEnabled() && underCursor())
                {
                    DRAW_STANDARD_BUTTON_UP
                }

                // Disabled or unchecked or not under cursor
                else
                {
                    dc.setForeground(backColor);
                    dc.fillRectangle(0,0,width,height);
                }
            }

            // Button style is normal
            else
            {
                // Draw sunken if pressed
                if (down || ((options&TOGGLEBUTTON_KEEPSTATE) && state))
                {
                    DRAW_STANDARD_BUTTON_DOWN
                }

                // Draw raised if not currently pressed down
                else
                {
                    DRAW_STANDARD_BUTTON_UP
                }
            }
        }

        // No borders
        else
        {
            dc.setForeground(backColor);
            dc.fillRectangle(0,0,width,height);
        }

    }  	// End of normal painting

    // Place text & icon
    if (state && !altlabel.empty())
    {
        tw=labelWidth(altlabel);
        th=labelHeight(altlabel);
    }
    else if (!label.empty())
    {
        tw=labelWidth(label);
        th=labelHeight(label);
    }
    if (state && alticon)
    {
        iw=alticon->getWidth();
        ih=alticon->getHeight();
    }
    else if (icon)
    {
        iw=icon->getWidth();
        ih=icon->getHeight();
    }

    just_x(tx,ix,tw,iw);
    just_y(ty,iy,th,ih);

    // Shift a bit when pressed
    if ((down || ((options&TOGGLEBUTTON_KEEPSTATE) && state)) && (options&(FRAME_RAISED|FRAME_SUNKEN)))
    {
        ++tx;
        ++ty;
        ++ix;
        ++iy;
    }

    // Draw enabled state
    if (isEnabled())
    {
        if (state && alticon)
            dc.drawIcon(alticon,ix,iy);
        else if (icon)
            dc.drawIcon(icon,ix,iy);
        if (state && !altlabel.empty())
        {
            dc.setFont(font);
            dc.setForeground(textColor);
            drawLabel(dc,altlabel,althotoff,tx,ty,tw,th);
        }
        else if (!label.empty())
        {
            dc.setFont(font);
            dc.setForeground(textColor);
            drawLabel(dc,label,hotoff,tx,ty,tw,th);
        }
        if (hasFocus())
            dc.drawFocusRectangle(border+1,border+1,width-2*border-2,height-2*border-2);
    }

    // Draw grayed-out state
    else
    {
        if (state && alticon)
            dc.drawIconSunken(alticon,ix,iy);
        else if (icon)
            dc.drawIconSunken(icon,ix,iy);
        if (state && !altlabel.empty())
        {
            dc.setFont(font);
            dc.setForeground(hiliteColor);
            drawLabel(dc,altlabel,althotoff,tx+1,ty+1,tw,th);
            dc.setForeground(shadowColor);
            drawLabel(dc,altlabel,althotoff,tx,ty,tw,th);
        }
        else if (!label.empty())
        {
            dc.setFont(font);
            dc.setForeground(hiliteColor);
            drawLabel(dc,label,hotoff,tx+1,ty+1,tw,th);
            dc.setForeground(shadowColor);
            drawLabel(dc,label,hotoff,tx,ty,tw,th);
        }
    }

    return 1;
}


//
// Hack of FXWindow
//

// This hack fixes a bug in FOX that prevent any character to be entered
// when FOX is compiled with the --with-xim option
// The bug is fixed in FOX 1.6.35 and above
// However, the hack is still here because the latest FOX is not necessarily present
// on the user's Linux distribution

#include "FXComposeContext.h"

// Create compose context
void FXWindow::createComposeContext()
{
    if (!composeContext)
    {
        composeContext=new FXComposeContext(getApp(),this,0);

        // !!! This line was missing !!!
        composeContext->create();
    }
}



//
// Hack of FXTextField
//

// This hack fixes a bug in FOX that make some input fields crash the application
// when FOX was compiled with the --with-xim option
// The bug is not fixed yet in FOX 1.6.36

// Into focus chain
void FXTextField::setFocus()
{
    FXFrame::setFocus();
    setDefault(TRUE);
    flags&=~FLAG_UPDATE;
    if (getApp()->hasInputMethod() && this->id() )
        createComposeContext();
}


//
// Hack of FXApp
//
// This hack fixes a bug in FOX that prevent to enter composed characters when the mouse pointer
// lies outside the text field
// The bug is not fixed yet in FOX 1.6.36


namespace FX
{

// Callback Record
struct FXCBSpec
{
    FXObject      *target;            // Receiver object
    FXSelector     message;           // Message sent to receiver
};


// Timer record
struct FXTimer
{
    FXTimer       *next;              // Next timeout in list
    FXObject      *target;            // Receiver object
    void          *data;              // User data
    FXSelector     message;           // Message sent to receiver
    FXlong         due;               // When timer is due (ns)
};


// Signal record
struct FXSignal
{
    FXObject      *target;            // Receiver object
    FXSelector     message;           // Message sent to receiver
    FXbool         handlerset;        // Handler was already set
    FXbool         notified;          // Signal has fired
};


// Idle record
struct FXChore
{
    FXChore       *next;              // Next chore in list
    FXObject      *target;            // Receiver object
    void          *data;              // User data
    FXSelector     message;           // Message sent to receiver
};


// Input record
struct FXInput
{
    FXCBSpec       read;              // Callback spec for read
    FXCBSpec       write;             // Callback spec for write
    FXCBSpec       excpt;             // Callback spec for except
};


// A repaint event record
struct FXRepaint
{
    FXRepaint     *next;              // Next repaint in list
    FXID           window;            // Window ID of the dirty window
    FXRectangle    rect;              // Dirty rectangle
    int          hint;              // Hint for compositing
    FXbool         synth;             // Synthetic expose event or real one?
};


// Recursive Event Loop Invocation
struct FXInvocation
{
    FXInvocation **invocation;  // Pointer to variable holding pointer to current invocation
    FXInvocation  *upper;       // Invocation above this one
    FXWindow      *window;      // Modal window (if any)
    FXModality     modality;    // Modality mode
    int          code;        // Return code
    FXbool         done;        // True if breaking out

    // Enter modal loop
    FXInvocation(FXInvocation** inv,FXModality mode,FXWindow* win):invocation(inv),upper(*inv),window(win),modality(mode),code(0),done(FALSE)
    {
        *invocation=this;
    }

    // Exit modal loop
    ~FXInvocation()
    {
        *invocation=upper;
    }
};

} // namespace FX


// Largest number of signals on this system
#define MAXSIGNALS 64

// Regular define
#define SELECT(n,r,w,e,t)  select(n,r,w,e,t)

// Get an event
bool FXApp::getNextEvent(FXRawEvent& ev,bool blocking)
{
    XEvent e;

    // Set to no-op just in case
    ev.xany.type=0;

    // Handle all past due timers
    if (timers)
        handleTimeouts();

    // Check non-immediate signals that may have fired
    if (nsignals)
    {
        for (int sig=0; sig<MAXSIGNALS; sig++)
        {
            if (signals[sig].notified)
            {
                signals[sig].notified=FALSE;
                if (signals[sig].target && signals[sig].target->tryHandle(this,FXSEL(SEL_SIGNAL,signals[sig].message),(void*)(FXival)sig))
                {
                    refresh();
                    return false;
                }
            }
        }
    }

    // Are there no events already queued up?
    if (!initialized || !XEventsQueued((Display*)display,QueuedAfterFlush))
    {
        struct timeval delta;
        fd_set readfds;
        fd_set writefds;
        fd_set exceptfds;
        int maxfds;
        int nfds;

        // Prepare fd's to check
        maxfds=maxinput;
        readfds=*((fd_set*)r_fds);
        writefds=*((fd_set*)w_fds);
        exceptfds=*((fd_set*)e_fds);

        // Add connection to display if its open
        if (initialized)
        {
            FD_SET(ConnectionNumber((Display*)display),&readfds);
            if (ConnectionNumber((Display*)display)>maxfds)
                maxfds=ConnectionNumber((Display*)display);
        }

        delta.tv_usec=0;
        delta.tv_sec=0;

        // Do a quick poll for any ready events or inputs
        nfds=SELECT(maxfds+1,&readfds,&writefds,&exceptfds,&delta);

        // Nothing to do, so perform idle processing
        if (nfds==0)
        {
            // Release the expose events
            if (repaints)
            {
                register FXRepaint *r=repaints;
                ev.xany.type=Expose;
                ev.xexpose.window=r->window;
                ev.xexpose.send_event=r->synth;
                ev.xexpose.x=r->rect.x;
                ev.xexpose.y=r->rect.y;
                ev.xexpose.width=r->rect.w-r->rect.x;
                ev.xexpose.height=r->rect.h-r->rect.y;
                repaints=r->next;
                r->next=repaintrecs;
                repaintrecs=r;
                return true;
            }

            // Do our chores :-)
            if (chores)
            {
                register FXChore *c=chores;
                chores=c->next;
                if (c->target && c->target->tryHandle(this,FXSEL(SEL_CHORE,c->message),c->data))
                    refresh();
                c->next=chorerecs;
                chorerecs=c;
            }

            // GUI updating:- walk the whole widget tree.
            if (refresher)
            {
                refresher->handle(this,FXSEL(SEL_UPDATE,0),NULL);
                if (refresher->getFirst())
                    refresher=refresher->getFirst();
                else
                {
                    while (refresher->getParent())
                    {
                        if (refresher->getNext())
                        {
                            refresher=refresher->getNext();
                            break;
                        }
                        refresher=refresher->getParent();
                    }
                }
                FXASSERT(refresher);
                if (refresher!=refresherstop)
                    return false;
                refresher=refresherstop=NULL;
            }

            // There are more chores to do
            if (chores)
                return false;

            // We're not blocking
            if (!blocking)
                return false;

            // Now, block till timeout, i/o, or event
            maxfds=maxinput;
            readfds=*((fd_set*)r_fds);
            writefds=*((fd_set*)w_fds);
            exceptfds=*((fd_set*)e_fds);

            // Add connection to display if its open
            if (initialized)
            {
                FD_SET(ConnectionNumber((Display*)display),&readfds);
                if (ConnectionNumber((Display*)display)>maxfds)
                    maxfds=ConnectionNumber((Display*)display);
            }

            // If there are timers, we block only for a little while.
            if (timers)
            {
                // All that testing above may have taken some time...
                FXlong interval=timers->due-FXThread::time();

                // Some timers are already due; do them right away!
                if (interval<=0)
                    return false;

                // Compute how long to wait
                delta.tv_usec=(interval/1000)%1000000;
                delta.tv_sec=interval/1000000000;

                // Exit critical section
                appMutex.unlock();

                // Block till timer or event or interrupt
                nfds=SELECT(maxfds+1,&readfds,&writefds,&exceptfds,&delta);

                // Enter critical section
                appMutex.lock();
            }

            // If no timers, we block till event or interrupt
            else
            {
                // Exit critical section
                appMutex.unlock();

                // Block until something happens
                nfds=SELECT(maxfds+1,&readfds,&writefds,&exceptfds,NULL);

                // Enter critical section
                appMutex.lock();
            }
        }

        // Timed out or interrupted
        if (nfds<=0)
        {
            if (nfds<0 && errno!=EAGAIN && errno!=EINTR)
                fxerror("Application terminated: interrupt or lost connection errno=%d\n",errno);
            return false;
        }

        // Any other file descriptors set?
        if (0<=maxinput)
        {
            // Examine I/O file descriptors
            for (FXInputHandle fff=0; fff<=maxinput; fff++)
            {
                // Copy the record as the callbacks may try to change things
                FXInput in=inputs[fff];

                // Skip the display connection, which is treated differently
                if (initialized && (fff==ConnectionNumber((Display*)display)))
                    continue;

                // Check file descriptors
                if (FD_ISSET(fff,&readfds))
                    if (in.read.target && in.read.target->tryHandle(this,FXSEL(SEL_IO_READ,in.read.message),(void*)(FXival)fff))
                        refresh();
                if (FD_ISSET(fff,&writefds))
                    if (in.write.target && in.write.target->tryHandle(this,FXSEL(SEL_IO_WRITE,in.write.message),(void*)(FXival)fff))
                        refresh();
                if (FD_ISSET(fff,&exceptfds))
                    if (in.excpt.target && in.excpt.target->tryHandle(this,FXSEL(SEL_IO_EXCEPT,in.read.message),(void*)(FXival)fff))
                        refresh();
            }
        }

        // If there is no event, we're done
        if (!initialized || !FD_ISSET(ConnectionNumber((Display*)display),&readfds) || !XEventsQueued((Display*)display,QueuedAfterReading))
            return false;
    }

    // Get an event
    XNextEvent((Display*)display,&ev);

    // Filter event through input method context, if any

    // !!! Hack to fix the bug with composed characters !!!
    FXWindow* focuswin;
    focuswin=getFocusWindow();
    if (xim && focuswin && XFilterEvent(&ev,(Window)focuswin->id()))
        return false;
    // !!! End of hack !!!

    // Save expose events for later...
    if (ev.xany.type==Expose || ev.xany.type==GraphicsExpose)
    {
        addRepaint((FXID)ev.xexpose.window,ev.xexpose.x,ev.xexpose.y,ev.xexpose.width,ev.xexpose.height,0);
        return false;
    }

    // Compress motion events
    if (ev.xany.type==MotionNotify)
    {
        while (XPending((Display*)display))
        {
            XPeekEvent((Display*)display,&e);
            if ((e.xany.type!=MotionNotify) || (ev.xmotion.window!=e.xmotion.window) || (ev.xmotion.state != e.xmotion.state))
                break;
            XNextEvent((Display*)display,&ev);
        }
    }

    // Compress wheel events
    else if ((ev.xany.type==ButtonPress) && (ev.xbutton.button==Button4 || ev.xbutton.button==Button5))
    {
        int ticks=1;
        while (XPending((Display*)display))
        {
            XPeekEvent((Display*)display,&e);
            if ((e.xany.type!=ButtonPress && e.xany.type!=ButtonRelease) || (ev.xany.window!=e.xany.window) || (ev.xbutton.button != e.xbutton.button))
                break;
            ticks+=(e.xany.type==ButtonPress);
            XNextEvent((Display*)display,&ev);
        }
        ev.xbutton.subwindow=(Window)ticks;   // Stick it here for later
    }

    // Compress configure events
    else if (ev.xany.type==ConfigureNotify)
    {
        while (XCheckTypedWindowEvent((Display*)display,ev.xconfigure.window,ConfigureNotify,&e))
        {
            ev.xconfigure.width=e.xconfigure.width;
            ev.xconfigure.height=e.xconfigure.height;
            if (e.xconfigure.send_event)
            {
                ev.xconfigure.x=e.xconfigure.x;
                ev.xconfigure.y=e.xconfigure.y;
            }
        }
    }

    // Regular event
    return true;
}


//
// Hack of FXScrollArea
//

// This hack allows to scroll in horizontal mode when we are in row and small/big icons mode of a FileList

// Mouse wheel used for vertical scrolling
long FXScrollArea::onVMouseWheel(FXObject* sender,FXSelector sel,void* ptr)
{
    // !!! Hack to scroll in horizontal mode !!!
    if (!(options&ICONLIST_COLUMNS) && options&(ICONLIST_BIG_ICONS|ICONLIST_MINI_ICONS) && streq(this->getClassName(),"FileList") )
        horizontal->handle(sender,sel,ptr);
    else
        // !!! End of hack !!!
        vertical->handle(sender,sel,ptr);

    return 1;
}



//
// Hack of FXScrollBar
//

// This hack adds an optional gradient with rounded corner theme to the scrollbar (Clearlooks)


// Draw scrollbar button with gradient effect and nice grip
static void drawGradientScrollButton(FXDCWindow& dc, FXColor topcolor, FXColor bottomcolor, FXColor shadecolor, FXColor lightcolor,
                                     unsigned int options, int x, int y, int w, int h)
{
    // Fill rectangle with gradient in the right direction (vertical or horizontal)
    FXbool vertical=((options&SCROLLBAR_HORIZONTAL) ? TRUE : FALSE);
    drawGradientRectangle(dc,topcolor,bottomcolor,x,y,w,h,vertical);

    // Draw button borders
    dc.setForeground(lightcolor);
    dc.fillRectangle(x+1,y+1,w-1,1);
    dc.fillRectangle(x+1,y+1,1,h-2);
    dc.setForeground(shadecolor);
    dc.fillRectangle(x,y,w,1);
    dc.fillRectangle(x,y,1,h-1);
    dc.fillRectangle(x,y+h-1,w,1);
    dc.fillRectangle(x+w-1,y,1,h);

    // Draw grip lines for horizontal scrollbar
    if ((options&SCROLLBAR_HORIZONTAL))
    {
        dc.setForeground(shadecolor);
        dc.fillRectangle(x+w/2-3,y+4,1,h-7);
        dc.fillRectangle(x+w/2,y+4,1,h-7);
        dc.fillRectangle(x+w/2+3,y+4,1,h-7);
        dc.setForeground(lightcolor);
        dc.fillRectangle(x+w/2-2,y+4,1,h-7);
        dc.fillRectangle(x+w/2+1,y+4,1,h-7);
        dc.fillRectangle(x+w/2+4,y+4,1,h-7);
    }

    // Draw grip lines for vertical scrollbar
    else
    {
        dc.setForeground(shadecolor);
        dc.fillRectangle(x+4,y+h/2-3,w-7,1);
        dc.fillRectangle(x+4,y+h/2,w-7,1);
        dc.fillRectangle(x+4,y+h/2+3,w-7,1);
        dc.setForeground(lightcolor);
        dc.fillRectangle(x+4,y+h/2-2,w-7,1);
        dc.fillRectangle(x+4,y+h/2+1,w-7,1);
        dc.fillRectangle(x+4,y+h/2+4,w-7,1);
    }
}


// Small hack to set the minimum length of the scrollbar button to barsize*2 instead of barsize/2
void FXScrollBar::setPosition(int p)
{
    int total,travel,lo,hi,l,h;
    pos=p;
    if (pos<0)
        pos=0;
    if (pos>(range-page))
        pos=range-page;
    lo=thumbpos;
    hi=thumbpos+thumbsize;
    if (options&SCROLLBAR_HORIZONTAL)
    {
        total=width-height-height;
        thumbsize=(total*page)/range;
        // !!! Hack to change the minimum button size !!!
        if (thumbsize<(barsize<<1))
            thumbsize=(barsize<<1);
        // !!! End of hack !!!
        travel=total-thumbsize;
        if (range>page)
            thumbpos=height+(int)((((double)pos)*travel)/(range-page));
        else
            thumbpos=height;
        l=thumbpos;
        h=thumbpos+thumbsize;
        if (l!=lo || h!=hi)
            update(FXMIN(l,lo),0,FXMAX(h,hi)-FXMIN(l,lo),height);
    }
    else
    {
        total=height-width-width;
        thumbsize=(total*page)/range;
        // !!! Hack to change the minimum button size !!!
        if (thumbsize<(barsize<<1))
            thumbsize=(barsize<<1);
        // !!! End of hack !!!
        travel=total-thumbsize;
        if (range>page)
            thumbpos=width+(int)((((double)pos)*travel)/(range-page));
        else
            thumbpos=width;
        l=thumbpos;
        h=thumbpos+thumbsize;
        if (l!=lo || h!=hi)
            update(0,FXMIN(l,lo),width,FXMAX(h,hi)-FXMIN(l,lo));
    }
}


// Arrow directions
enum
{
    _ARROW_LEFT,
    _ARROW_RIGHT,
    _ARROW_UP,
    _ARROW_DOWN
};


// Draw arrow button in scrollbar with gradient effect and rounded corners (Clearlooks)
static void drawGradientArrowButton(FXDCWindow& dc, FXColor backcolor, FXColor topcolor, FXColor bottomcolor, FXColor shadecolor,
                                    FXColor lightcolor, FXColor bordercolor, FXColor arrowcolor,
                                    unsigned int options, int x, int y, int w, int h, FXbool down, unsigned int direction)
{
    FXPoint arrowpoints[3];
    int xx, yy, ah, ab;

    FXPoint basebackground[2];
    FXPoint bordershade[8];
    FXPoint bordercorners[2];

    // Rounded corner and arrow point coordinates depend on the button direction
    if (direction == _ARROW_UP)
    {
        // Rounded corners
        basebackground[0]=FXPoint(0,0);
        basebackground[1]=FXPoint(w-1,0);
        bordercorners[0]=FXPoint(1,1);
        bordercorners[1]=FXPoint(w-2,1);
        bordershade[0]=FXPoint(0,1);
        bordershade[1]=FXPoint(1,0);
        bordershade[2]=FXPoint(1,2);
        bordershade[3]=FXPoint(2,1);
        bordershade[4]=FXPoint(w-2,0);
        bordershade[5]=FXPoint(w-1,1);
        bordershade[6]=FXPoint(w-3,1);
        bordershade[7]=FXPoint(w-2,2);

        // Arrow points
        ab=(w-7)|1;
        ah=ab>>1;
        xx=x+((w-ab)>>1);
        yy=y+((h-ah)>>1);
        if (down)
        {
            ++xx;
            ++yy;
        }
        arrowpoints[0]=FXPoint(xx+(ab>>1),yy-1);
        arrowpoints[1]=FXPoint(xx,yy+ah);
        arrowpoints[2]=FXPoint(xx+ab,yy+ah);
    }
    else if (direction == _ARROW_DOWN)
    {
        // Rounded corners
        basebackground[0]=FXPoint(x,y+h-1);
        basebackground[1]=FXPoint(x+w-1,y+h-1);
        bordercorners[0]=FXPoint(x+1,y+h-2);
        bordercorners[1]=FXPoint(x+w-2,y+h-2);
        bordershade[0]=FXPoint(x,y+h-2);
        bordershade[1]=FXPoint(x+1,y+h-1);
        bordershade[2]=FXPoint(x+1,y+h-3);
        bordershade[3]=FXPoint(x+2,y+h-2);
        bordershade[4]=FXPoint(x+w-1,y+h-2);
        bordershade[5]=FXPoint(x+w-2,y+h-1);
        bordershade[6]=FXPoint(x+w-2,y+h-3);
        bordershade[7]=FXPoint(x+w-3,y+h-2);

        // Arrow points
        ab=(w-7)|1;
        ah=ab>>1;
        xx=x+((w-ab)>>1);
        yy=y+((h-ah)>>1);
        if (down)
        {
            ++xx;
            ++yy;
        }
        arrowpoints[0]=FXPoint(xx+1,yy);
        arrowpoints[1]=FXPoint(xx+ab-1,yy);
        arrowpoints[2]=FXPoint(xx+(ab>>1),yy+ah);
    }
    else if (direction == _ARROW_LEFT)
    {
        // Rounded corners
        basebackground[0]=FXPoint(0,0);
        basebackground[1]=FXPoint(0,h-1);
        bordercorners[0]=FXPoint(1,1);
        bordercorners[1]=FXPoint(1,h-2);
        bordershade[0]=FXPoint(0,1);
        bordershade[1]=FXPoint(1,0);
        bordershade[2]=FXPoint(1,2);
        bordershade[3]=FXPoint(2,1);
        bordershade[4]=FXPoint(0,h-2);
        bordershade[5]=FXPoint(1,h-1);
        bordershade[6]=FXPoint(1,h-3);
        bordershade[7]=FXPoint(2,h-2);

        // Arrow points
        ab=(h-7)|1;
        ah=ab>>1;
        xx=x+((w-ah)>>1);
        yy=y+((h-ab)>>1);
        if (down)
        {
            ++xx;
            ++yy;
        }
        arrowpoints[0]=FXPoint(xx+ah,yy);
        arrowpoints[1]=FXPoint(xx+ah,yy+ab-1);
        arrowpoints[2]=FXPoint(xx,yy+(ab>>1));
    }
    else // _ARROW_RIGHT
    {
        // Rounded corners
        basebackground[0]=FXPoint(x+w-1,y);
        basebackground[1]=FXPoint(x+w-1,y+h-1);
        bordercorners[0]=FXPoint(x+w-2,y+1);
        bordercorners[1]=FXPoint(x+w-2,y+h-2);
        bordershade[0]=FXPoint(x+w-2,y);
        bordershade[1]=FXPoint(x+w-1,y+1);
        bordershade[2]=FXPoint(x+w-3,y+1);
        bordershade[3]=FXPoint(x+w-2,y+2);
        bordershade[4]=FXPoint(x+w-1,y+h-2);
        bordershade[5]=FXPoint(x+w-2,y+h-1);
        bordershade[6]=FXPoint(x+w-2,y+h-3);
        bordershade[7]=FXPoint(x+w-3,y+h-2);

        // Arrow points
        ab=(h-7)|1;
        ah=ab>>1;
        xx=x+((w-ah)>>1);
        yy=y+((h-ab)>>1);
        if (down)
        {
            ++xx;
            ++yy;
        }
        arrowpoints[0]=FXPoint(xx,yy);
        arrowpoints[1]=FXPoint(xx,yy+ab-1);
        arrowpoints[2]=FXPoint(xx+ah,yy+(ab>>1));
    }

    // Draw button when up
    if (!down)
    {
        // Fill rectangle with gradient in the right direction (vertical or horizontal)
        FXbool vertical=((options&SCROLLBAR_HORIZONTAL) ? TRUE : FALSE);
        drawGradientRectangle(dc,topcolor,bottomcolor,x,y,w,h,vertical);

        // Button borders
        dc.setForeground(lightcolor);
        dc.fillRectangle(x+1,y+1,w-1,1);
        dc.fillRectangle(x+1,y+1,1,h-2);
        dc.setForeground(shadecolor);
        dc.fillRectangle(x,y,w,1);
        dc.fillRectangle(x,y,1,h-1);
        dc.fillRectangle(x,y+h-1,w,1);
        dc.fillRectangle(x+w-1,y,1,h);

        // Rounded corners
        dc.setForeground(backcolor);
        dc.drawPoints(basebackground,2);
        dc.setForeground(shadecolor);
        dc.drawPoints(bordercorners,2);
        dc.setForeground(bordercolor);
        dc.drawPoints(bordershade,8);

        // Arrow
        dc.setForeground(arrowcolor);
        dc.fillPolygon(arrowpoints,3);
    }

    // Draw button when down (pressed)
    else
    {
        // Dark background
        dc.setForeground(bordercolor);
        dc.fillRectangle(x,y,w,h);

        // Button borders
        dc.setForeground(lightcolor);
        dc.fillRectangle(x+1,y+1,w-1,1);
        dc.fillRectangle(x+1,y+1,1,h-2);
        dc.setForeground(shadecolor);
        dc.fillRectangle(x,y,w,1);
        dc.fillRectangle(x,y,1,h-1);
        dc.fillRectangle(x,y+h-1,w,1);
        dc.fillRectangle(x+w-1,y,1,h);

        // Rounded corners
        dc.setForeground(backcolor);
        dc.drawPoints(basebackground,2);
        dc.setForeground(shadecolor);
        dc.drawPoints(bordercorners,2);
        dc.setForeground(bordercolor);
        dc.drawPoints(bordershade,8);

        // Arrow
        dc.setForeground(arrowcolor);
        dc.fillPolygon(arrowpoints,3);
    }
}


// Draw flat scrollbar button with selected colors
static void drawFlatScrollButton(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h,FXbool down, FXColor hilitecolor, FXColor shadowcolor, FXColor bordercolor, FXColor scrollbarcolor)
{
	dc.setForeground(scrollbarcolor);
	dc.fillRectangle(x+2,y+2,w-4,h-4);
	if(!down)
	{
		dc.setForeground(scrollbarcolor);
		dc.fillRectangle(x,y,w-1,1);
		dc.fillRectangle(x,y,1,h-1);
		dc.setForeground(hilitecolor);
		dc.fillRectangle(x+1,y+1,w-2,1);
		dc.fillRectangle(x+1,y+1,1,h-2);
		dc.setForeground(shadowcolor);
		dc.fillRectangle(x+1,y+h-2,w-2,1);
		dc.fillRectangle(x+w-2,y+1,1,h-2);
		dc.setForeground(bordercolor);
		dc.fillRectangle(x,y+h-1,w,1);
		dc.fillRectangle(x+w-1,y,1,h);
	}
	else
	{
		dc.setForeground(bordercolor);
		dc.fillRectangle(x,y,w-2,1);
		dc.fillRectangle(x,y,1,h-2);
		dc.setForeground(shadowcolor);
		dc.fillRectangle(x+1,y+1,w-3,1);
		dc.fillRectangle(x+1,y+1,1,h-3);
		dc.setForeground(hilitecolor);
		dc.fillRectangle(x,y+h-1,w-1,1);
		dc.fillRectangle(x+w-1,y+1,1,h-1);
		dc.setForeground(scrollbarcolor);
		dc.fillRectangle(x+1,y+h-2,w-1,1);
		dc.fillRectangle(x+w-2,y+2,1,h-2);
	}
}


// Handle repaint
long FXScrollBar::onPaint(FXObject*,FXSelector,void* ptr)
{
    // Caution! Don't use the macro here because it's slightly different

    static FXbool init=TRUE;
    static FXbool use_clearlooks=TRUE;
    static FXColor bg_topcolor, bg_bottomcolor, bg_shadecolor, bg_bordercolor, bg_lightcolor;
    static FXColor sb_topcolor, sb_bottomcolor, sb_shadecolor, sb_bordercolor, sb_lightcolor, scrollbarcolor;

    register FXEvent *ev=(FXEvent*)ptr;
    register int total;
    FXDCWindow dc(this,ev);

    // At first run, select the scrollbar style and color
    if (init)
    {
        use_clearlooks=getApp()->reg().readUnsignedEntry("SETTINGS","use_clearlooks",TRUE);
  		scrollbarcolor=getApp()->reg().readColorEntry("SETTINGS","scrollbarcolor",FXRGB(237,233,227));

        // Compute gradient colors from the base color
        if (use_clearlooks)
        {
            // Decompose the base color
            unsigned int r=FXREDVAL(backColor);
            unsigned int g=FXGREENVAL(backColor);
            unsigned int b=FXBLUEVAL(backColor);

            // Compute the gradient colors from the base color (background)
            bg_topcolor=FXRGB(FXMIN(1.1*r,255),FXMIN(1.1*g,255),FXMIN(1.1*b,255));
            bg_bottomcolor=FXRGB(0.9*r,0.9*g,0.9*b);
            bg_shadecolor=FXRGB(0.8*r,0.8*g,0.8*b);
            bg_bordercolor=FXRGB(0.9*r,0.9*g,0.9*b);
            bg_lightcolor=FXRGB(FXMIN(1.3*r,255),FXMIN(1.3*g,255),FXMIN(1.3*b,255));

            // Compute the gradient colors from the base color (scrollbar)
            r=FXREDVAL(scrollbarcolor);
            g=FXGREENVAL(scrollbarcolor);
            b=FXBLUEVAL(scrollbarcolor);
            sb_topcolor=FXRGB(FXMIN(1.1*r,255),FXMIN(1.1*g,255),FXMIN(1.1*b,255));
            sb_bottomcolor=FXRGB(0.9*r,0.9*g,0.9*b);
            sb_shadecolor=FXRGB(0.8*r,0.8*g,0.8*b);
            sb_bordercolor=FXRGB(0.9*r,0.9*g,0.9*b);
            sb_lightcolor=FXRGB(FXMIN(1.3*r,255),FXMIN(1.3*g,255),FXMIN(1.3*b,255));
        }
        init=FALSE;
    }

    // Nice scrollbar with gradient and rounded corners
    if (use_clearlooks)
    {
        if (options&SCROLLBAR_HORIZONTAL)
        {
            total=width-height-height;
            if (thumbsize<total)                                    // Scrollable
            {
                drawGradientScrollButton(dc,sb_topcolor,sb_bottomcolor,sb_shadecolor,sb_lightcolor,options,thumbpos,0,thumbsize,height);
                dc.setForeground(bg_bordercolor);
                dc.setBackground(backColor);
                dc.fillRectangle(height,0,thumbpos-height,height);
                dc.fillRectangle(thumbpos+thumbsize,0,width-height-thumbpos-thumbsize,height);
            }
            else                                                    // Non-scrollable
            {
                dc.setForeground(bg_bordercolor);
                dc.setBackground(backColor);
                dc.fillRectangle(height,0,total,height);
            }
            drawGradientArrowButton(dc,backColor,bg_topcolor,bg_bottomcolor,bg_shadecolor,bg_lightcolor,bg_bordercolor,arrowColor,options,width-height,0,height,height,(mode==MODE_INC),_ARROW_RIGHT);
            drawGradientArrowButton(dc,backColor,bg_topcolor,bg_bottomcolor,bg_shadecolor,bg_lightcolor,bg_bordercolor,arrowColor,options,0,0,height,height,(mode==MODE_DEC),_ARROW_LEFT);
        }

        // Vertical
        else
        {
            total=height-width-width;
            if (thumbsize<total)                                    // Scrollable
            {
                drawGradientScrollButton(dc,sb_topcolor,sb_bottomcolor,sb_shadecolor,sb_lightcolor,options,0,thumbpos,width,thumbsize);
                dc.setForeground(bg_bordercolor);
                dc.setBackground(backColor);
                dc.fillRectangle(0,width,width,thumbpos-width);
                dc.fillRectangle(0,thumbpos+thumbsize,width,height-width-thumbpos-thumbsize);
            }
            else                                                    // Non-scrollable
            {
                dc.setForeground(bg_bordercolor);
                dc.setBackground(backColor);
                dc.fillRectangle(0,width,width,total);
            }
            drawGradientArrowButton(dc,backColor,bg_topcolor,bg_bottomcolor,bg_shadecolor,bg_lightcolor,bg_bordercolor,arrowColor,options,0,height-width,width,width,(mode==MODE_INC),_ARROW_DOWN);
            drawGradientArrowButton(dc,backColor,bg_topcolor,bg_bottomcolor,bg_shadecolor,bg_lightcolor,bg_bordercolor,arrowColor,options,0,0,width,width,(mode==MODE_DEC),_ARROW_UP);
        }
    }

    // Standard (flat) scrollbar with selected color
    else
    {
      	if (options&SCROLLBAR_HORIZONTAL)
        {
            total=width-height-height;
            if (thumbsize<total)                                    // Scrollable
            {
                drawFlatScrollButton(dc,thumbpos,0,thumbsize,height,0,hiliteColor,shadowColor,borderColor,scrollbarcolor);
                dc.setStipple(STIPPLE_GRAY);
                dc.setFillStyle(FILL_OPAQUESTIPPLED);
                if (mode==MODE_PAGE_DEC)
                {
                    dc.setForeground(backColor);
                    dc.setBackground(shadowColor);
                }
                else
                {
                    dc.setForeground(hiliteColor);
                    dc.setBackground(backColor);
                }
                dc.fillRectangle(height,0,thumbpos-height,height);
                if (mode==MODE_PAGE_INC)
                {
                    dc.setForeground(backColor);
                    dc.setBackground(shadowColor);
                }
                else
                {
                    dc.setForeground(hiliteColor);
                    dc.setBackground(backColor);
                }
                dc.fillRectangle(thumbpos+thumbsize,0,width-height-thumbpos-thumbsize,height);
            }
            else                                                    // Non-scrollable
            {
                dc.setStipple(STIPPLE_GRAY);
                dc.setFillStyle(FILL_OPAQUESTIPPLED);
                dc.setForeground(hiliteColor);
                dc.setBackground(backColor);
                dc.fillRectangle(height,0,total,height);
            }
            dc.setFillStyle(FILL_SOLID);
            drawButton(dc,width-height,0,height,height,(mode==MODE_INC));
            drawRightArrow(dc,width-height,0,height,height,(mode==MODE_INC));
            drawButton(dc,0,0,height,height,(mode==MODE_DEC));
            drawLeftArrow(dc,0,0,height,height,(mode==MODE_DEC));
        }
        else
        {
            total=height-width-width;
            if (thumbsize<total)                                    // Scrollable
            {
                drawFlatScrollButton(dc,0,thumbpos,width,thumbsize,0,hiliteColor,shadowColor,borderColor,scrollbarcolor);
                dc.setStipple(STIPPLE_GRAY);
                dc.setFillStyle(FILL_OPAQUESTIPPLED);
                if (mode==MODE_PAGE_DEC)
                {
                    dc.setForeground(backColor);
                    dc.setBackground(shadowColor);
                }
                else
                {
                    dc.setForeground(hiliteColor);
                    dc.setBackground(backColor);
                }
                dc.fillRectangle(0,width,width,thumbpos-width);
                if (mode==MODE_PAGE_INC)
                {
                    dc.setForeground(backColor);
                    dc.setBackground(shadowColor);
                }
                else
                {
                    dc.setForeground(hiliteColor);
                    dc.setBackground(backColor);
                }
                dc.fillRectangle(0,thumbpos+thumbsize,width,height-width-thumbpos-thumbsize);
            }
            else                                                    // Non-scrollable
            {
                dc.setStipple(STIPPLE_GRAY);
                dc.setFillStyle(FILL_OPAQUESTIPPLED);
                dc.setForeground(hiliteColor);
                dc.setBackground(backColor);
                dc.fillRectangle(0,width,width,total);
            }
            dc.setFillStyle(FILL_SOLID);
            drawButton(dc,0,height-width,width,width,(mode==MODE_INC));
            drawDownArrow(dc,0,height-width,width,width,(mode==MODE_INC));
            drawButton(dc,0,0,width,width,(mode==MODE_DEC));
            drawUpArrow(dc,0,0,width,width,(mode==MODE_DEC));
        }
    }
    return 1;
}


//
// Hack of FXComboBox
//

// This hack adds an optional gradient with rounded corner theme to the combobox button (Clearlooks)

#define MENUBUTTONARROW_WIDTH   11
#define MENUBUTTONARROW_HEIGHT  5


// Small hack related to the Clearlooks theme
FXComboBox::FXComboBox(FXComposite *p,int cols,FXObject* tgt,FXSelector sel,unsigned int opts,int x,int y,int w,int h,int pl,int pr,int pt,int pb):
    FXPacker(p,opts,x,y,w,h, 0,0,0,0, 0,0)
{
    flags|=FLAG_ENABLED;
    target=tgt;
    message=sel;

    // !!! Hack to set options to TEXTFIELD_NORMAL instead of 0 (used by the Clearlooks theme)
    field=new FXTextField(this,cols,this,FXComboBox::ID_TEXT,TEXTFIELD_NORMAL, 0,0,0,0, pl,pr,pt,pb);
    // !!! End of hack

    if (options&COMBOBOX_STATIC)
        field->setEditable(FALSE);
    pane=new FXPopup(this,FRAME_LINE);
    list=new FXList(pane,this,FXComboBox::ID_LIST,LIST_BROWSESELECT|LIST_AUTOSELECT|LAYOUT_FILL_X|LAYOUT_FILL_Y|SCROLLERS_TRACK|HSCROLLER_NEVER);
    if (options&COMBOBOX_STATIC)
        list->setScrollStyle(SCROLLERS_TRACK|HSCROLLING_OFF);
    button=new FXMenuButton(this,FXString::null,NULL,pane,FRAME_RAISED|FRAME_THICK|MENUBUTTON_DOWN|MENUBUTTON_ATTACH_RIGHT, 0,0,0,0, 0,0,0,0);
    button->setXOffset(border);
    button->setYOffset(border);

    flags&=~FLAG_UPDATE;  // Never GUI update
}


//
// Hack of FXMenuButton
//

// This hack adds an optional gradient with rounded corner theme to the combobox button (Clearlooks)


// Handle repaint
long FXMenuButton::onPaint(FXObject*,FXSelector,void* ptr)
{
    // Initialise Clearlooks
    INIT_CLEARLOOKS

    int tw=0,th=0,iw=0,ih=0,tx,ty,ix,iy;
    FXEvent *ev=(FXEvent*)ptr;
    FXPoint points[3];
    FXDCWindow dc(this,ev);

    // Button with nice gradient effect and rounded corners (Clearlooks)
    if (use_clearlooks)
    {
        // Toolbar style
        if (options&MENUBUTTON_TOOLBAR)
        {
            // Enabled and cursor inside, and not popped up
            if (isEnabled() && underCursor() && !state)
            {
                DRAW_CLEARLOOKS_BUTTON_DOWN
            }

            // Enabled and popped up
            else if (isEnabled() && state)
            {
                DRAW_CLEARLOOKS_BUTTON_UP
            }

            // Disabled or unchecked or not under cursor
            else
            {
                dc.setForeground(backColor);
                dc.fillRectangle(0,0,width,height);
            }
        }

        // Normal style
        else
        {
            // Draw in up state if disabled or up
            if (!isEnabled() || !state)
            {
                DRAW_CLEARLOOKS_BUTTON_UP
            }

            // If enabled and either checked or pressed
            else
            {
                DRAW_CLEARLOOKS_BUTTON_DOWN
            }
        }

    }	// End of gradient painting


    // Normal flat rectangular button
    else
    {
        // Got a border at all?
        if (options&(FRAME_RAISED|FRAME_SUNKEN))
        {
            // Toolbar style
            if (options&MENUBUTTON_TOOLBAR)
            {
                // Enabled and cursor inside, and not popped up
                if (isEnabled() && underCursor() && !state)
                {
                    DRAW_STANDARD_BUTTON_DOWN
                }

                // Enabled and popped up
                else if (isEnabled() && state)
                {
                    DRAW_STANDARD_BUTTON_UP
                }

                // Disabled or unchecked or not under cursor
                else
                {
                    dc.setForeground(backColor);
                    dc.fillRectangle(0,0,width,height);
                }
            }

            // Normal style
            else
            {

                // Draw in up state if disabled or up
                if (!isEnabled() || !state)
                {
                    DRAW_STANDARD_BUTTON_UP
                }

                // Draw sunken if enabled and either checked or pressed
                else
                {
                    DRAW_STANDARD_BUTTON_DOWN
                }
            }
        }

        // No borders
        else
        {
            if (isEnabled() && state)
            {
                dc.setForeground(hiliteColor);
                dc.fillRectangle(0,0,width,height);
            }
            else
            {
                dc.setForeground(backColor);
                dc.fillRectangle(0,0,width,height);
            }
        }

    }  	// End of normal painting

    // Position text & icon
    if (!label.empty())
    {
        tw=labelWidth(label);
        th=labelHeight(label);
    }

    // Icon?
    if (icon)
    {
        iw=icon->getWidth();
        ih=icon->getHeight();
    }

    // Arrows?
    else if (!(options&MENUBUTTON_NOARROWS))
    {
        if (options&MENUBUTTON_LEFT)
        {
            ih=MENUBUTTONARROW_WIDTH;
            iw=MENUBUTTONARROW_HEIGHT;
        }
        else
        {
            iw=MENUBUTTONARROW_WIDTH;
            ih=MENUBUTTONARROW_HEIGHT;
        }
    }

    // Keep some room for the arrow!
    just_x(tx,ix,tw,iw);
    just_y(ty,iy,th,ih);

    // Move a bit when pressed
    if (state)
    {
        ++tx;
        ++ty;
        ++ix;
        ++iy;
    }

    // Draw icon
    if (icon)
    {
        if (isEnabled())
            dc.drawIcon(icon,ix,iy);
        else
            dc.drawIconSunken(icon,ix,iy);
    }

    // Draw arrows
    else if (!(options&MENUBUTTON_NOARROWS))
    {
        // Right arrow
        if ((options&MENUBUTTON_RIGHT)==MENUBUTTON_RIGHT)
        {
            if (isEnabled())
                dc.setForeground(textColor);
            else
                dc.setForeground(shadowColor);
            points[0].x=ix;
            points[0].y=iy;
            points[1].x=ix;
            points[1].y=iy+MENUBUTTONARROW_WIDTH-1;
            points[2].x=ix+MENUBUTTONARROW_HEIGHT;
            points[2].y=(short)(iy+(MENUBUTTONARROW_WIDTH>>1));
            dc.fillPolygon(points,3);
        }

        // Left arrow
        else if (options&MENUBUTTON_LEFT)
        {
            if (isEnabled())
                dc.setForeground(textColor);
            else
                dc.setForeground(shadowColor);
            points[0].x=ix+MENUBUTTONARROW_HEIGHT;
            points[0].y=iy;
            points[1].x=ix+MENUBUTTONARROW_HEIGHT;
            points[1].y=iy+MENUBUTTONARROW_WIDTH-1;
            points[2].x=ix;
            points[2].y=(short)(iy+(MENUBUTTONARROW_WIDTH>>1));
            dc.fillPolygon(points,3);
        }

        // Up arrow
        else if (options&MENUBUTTON_UP)
        {
            if (isEnabled())
                dc.setForeground(textColor);
            else
                dc.setForeground(shadowColor);
            points[0].x=(short)(ix+(MENUBUTTONARROW_WIDTH>>1));
            points[0].y=iy-1;
            points[1].x=ix;
            points[1].y=iy+MENUBUTTONARROW_HEIGHT;
            points[2].x=ix+MENUBUTTONARROW_WIDTH;
            points[2].y=iy+MENUBUTTONARROW_HEIGHT;
            dc.fillPolygon(points,3);
        }

        // Down arrow
        else
        {
            if (isEnabled())
                dc.setForeground(textColor);
            else
                dc.setForeground(shadowColor);
            points[0].x=ix+1;
            points[0].y=iy;
            points[2].x=ix+MENUBUTTONARROW_WIDTH-1;
            points[2].y=iy;
            points[1].x=(short)(ix+(MENUBUTTONARROW_WIDTH>>1));
            points[1].y=iy+MENUBUTTONARROW_HEIGHT;
            dc.fillPolygon(points,3);
        }
    }

    // Draw text
    if (!label.empty())
    {
        dc.setFont(font);
        if (isEnabled())
        {
            dc.setForeground(textColor);
            drawLabel(dc,label,hotoff,tx,ty,tw,th);
        }
        else
        {
            dc.setForeground(hiliteColor);
            drawLabel(dc,label,hotoff,tx+1,ty+1,tw,th);
            dc.setForeground(shadowColor);
            drawLabel(dc,label,hotoff,tx,ty,tw,th);
        }
    }

    // Draw focus
    if (hasFocus())
    {
        if (isEnabled())
            dc.drawFocusRectangle(border+1,border+1,width-2*border-2,height-2*border-2);
    }
    return 1;
}



//
// Hack of FXArrowButton
//

// This hack adds an optional gradient with rounded corner theme to the arrow button (Clearlooks)


// Handle repaint
long FXArrowButton::onPaint(FXObject*,FXSelector,void* ptr)
{
    // Initialise Clearlooks
    INIT_CLEARLOOKS

    FXEvent   *ev=(FXEvent*)ptr;
    FXDCWindow dc(this,ev);
    FXPoint    points[3];
    int      xx,yy,ww,hh,q;

    // Button with nice gradient effect and rounded corners (Clearlooks)
    if (use_clearlooks)
    {
        // Toolbar style
        if (options&ARROW_TOOLBAR)
        {
            // Enabled and cursor inside, and up
            if (isEnabled() && underCursor() && !state)
            {
                DRAW_CLEARLOOKS_BUTTON_UP
            }

            // Enabled and cursor inside and down
            else if (isEnabled() && state)
            {
                DRAW_CLEARLOOKS_BUTTON_DOWN
            }

            // Disabled or unchecked or not under cursor
            else
            {
                dc.setForeground(backColor);
                dc.fillRectangle(0,0,width,height);
            }
        }

        // Normal style
        else
        {
            // Draw sunken if enabled and pressed
            if (isEnabled() && state)
            {
                DRAW_CLEARLOOKS_BUTTON_DOWN
            }

            // Draw in up state if disabled or up
            else
            {
                DRAW_CLEARLOOKS_BUTTON_UP
            }
        }

    }	// End of gradient painting

    // Normal flat rectangular button
    else
    {
        // With borders
        if (options&(FRAME_RAISED|FRAME_SUNKEN))
        {
            // Toolbar style
            if (options&ARROW_TOOLBAR)
            {
                // Enabled and cursor inside, and up
                if (isEnabled() && underCursor() && !state)
                {
                    DRAW_STANDARD_BUTTON_UP
                }

                // Enabled and cursor inside and down
                else if (isEnabled() && state)
                {
                    DRAW_STANDARD_BUTTON_DOWN
                }

                // Disabled or unchecked or not under cursor
                else
                {
                    dc.setForeground(backColor);
                    dc.fillRectangle(0,0,width,height);
                }
            }

            // Normal style
            else
            {
                // Draw sunken if enabled and pressed
                if (isEnabled() && state)
                {
                    DRAW_STANDARD_BUTTON_DOWN
                }

                // Draw in up state if disabled or up
                else
                {
                    DRAW_STANDARD_BUTTON_UP
                }
            }
        }

        // No borders
        else
        {
            if (isEnabled() && state)
            {
                dc.setForeground(hiliteColor);
                dc.fillRectangle(0,0,width,height);
            }
            else
            {
                dc.setForeground(backColor);
                dc.fillRectangle(0,0,width,height);
            }
        }

    }  	// End of normal painting

    // Compute size of the arrows....
    ww=width-padleft-padright-(border<<1);
    hh=height-padtop-padbottom-(border<<1);
    if (options&(ARROW_UP|ARROW_DOWN))
    {
        q=ww|1;
        if (q>(hh<<1)) q=(hh<<1)-1;
        ww=q;
        hh=q>>1;
    }
    else
    {
        q=hh|1;
        if (q>(ww<<1)) q=(ww<<1)-1;
        ww=q>>1;
        hh=q;
    }

    if (options&JUSTIFY_LEFT) xx=padleft+border;
    else if (options&JUSTIFY_RIGHT) xx=width-ww-padright-border;
    else xx=(width-ww)/2;

    if (options&JUSTIFY_TOP) yy=padtop+border;
    else if (options&JUSTIFY_BOTTOM) yy=height-hh-padbottom-border;
    else yy=(height-hh)/2;

    if (state)
    {
        ++xx;
        ++yy;
    }

    if (isEnabled())
        dc.setForeground(arrowColor);
    else
        dc.setForeground(shadowColor);

    // NB Size of arrow should stretch
    if (options&ARROW_UP)
    {
        points[0].x=xx+(ww>>1);
        points[0].y=yy-1;
        points[1].x=xx;
        points[1].y=yy+hh;
        points[2].x=xx+ww;
        points[2].y=yy+hh;
        dc.fillPolygon(points,3);
    }
    else if (options&ARROW_DOWN)
    {
        points[0].x=xx+1;
        points[0].y=yy;
        points[1].x=xx+ww-1;
        points[1].y=yy;
        points[2].x=xx+(ww>>1);
        points[2].y=yy+hh;
        dc.fillPolygon(points,3);
    }
    else if (options&ARROW_LEFT)
    {
        points[0].x=xx+ww;
        points[0].y=yy;
        points[1].x=xx+ww;
        points[1].y=yy+hh-1;
        points[2].x=xx;
        points[2].y=yy+(hh>>1);
        dc.fillPolygon(points,3);
    }
    else if (options&ARROW_RIGHT)
    {
        points[0].x=xx;
        points[0].y=yy;
        points[1].x=xx;
        points[1].y=yy+hh-1;
        points[2].x=xx+ww;
        points[2].y=yy+(hh>>1);
        dc.fillPolygon(points,3);
    }
    return 1;
}



//
// Hack of FXProgressBar
//

// This hack adds an optional gradient theme to the progress bar (Clearlooks)
// Note : not implemented for the dial progress bar!


// Draw only the interior, i.e. the part that changes
void FXProgressBar::drawInterior(FXDCWindow& dc)
{
    static FXbool init=TRUE;
    static FXbool use_clearlooks=TRUE;
    static FXColor topcolor, bottomcolor;

    // Init Clearlooks (don't use the macro because here it's different)
    if (init)
    {
        use_clearlooks=getApp()->reg().readUnsignedEntry("SETTINGS","use_clearlooks",TRUE);

        if (use_clearlooks)
        {
            unsigned int r=FXREDVAL(barColor);
            unsigned int g=FXGREENVAL(barColor);
            unsigned int b=FXBLUEVAL(barColor);

            topcolor=FXRGB(FXMIN(1.2*r,255),FXMIN(1.2*g,255),FXMIN(1.2*b,255));
            bottomcolor=FXRGB(0.9*r,0.9*g,0.9*b);
        }
        init=FALSE;
    }

    int percent,barlength,barfilled,tx,ty,tw,th,n,d;
    char numtext[5];

    if (options&PROGRESSBAR_DIAL)
    {
        // If total is 0, it's 100%
        barfilled=23040;
        percent=100;
        if (total!=0)
        {
            barfilled=(unsigned int) (((double)progress * (double)23040) / (double)total);
            percent=(unsigned int) (((double)progress * 100.0) / (double)total);
        }

        tw=width-(border<<1)-padleft-padright;
        th=height-(border<<1)-padtop-padbottom;
        d=FXMIN(tw,th)-1;

        tx=border+padleft+((tw-d)/2);
        ty=border+padtop+((th-d)/2);

        if (barfilled!=23040)
        {
            dc.setForeground(barBGColor);
            dc.fillArc(tx,ty,d,d,5760,23040-barfilled);
        }
        if (barfilled!=0)
        {
            dc.setForeground(barColor);
            dc.fillArc(tx,ty,d,d,5760,-barfilled);
        }

        // Draw outside circle
        dc.setForeground(borderColor);
        dc.drawArc(tx+1,ty,d,d,90*64,45*64);
        dc.drawArc(tx,ty+1,d,d,135*64,45*64);
        dc.setForeground(baseColor);
        dc.drawArc(tx-1,ty,d,d,270*64,45*64);
        dc.drawArc(tx,ty-1,d,d,315*64,45*64);

        dc.setForeground(shadowColor);
        dc.drawArc(tx,ty,d,d,45*64,180*64);
        dc.setForeground(hiliteColor);
        dc.drawArc(tx,ty,d,d,225*64,180*64);

        // Draw text
        if (options&PROGRESSBAR_PERCENTAGE)
        {
            dc.setFont(font);
            tw=font->getTextWidth("100%",4);
            if (tw>(10*d)/16) return;
            th=font->getFontHeight();
            if (th>d/2) return;
            sprintf(numtext,"%d%%",percent);
            n=strlen(numtext);
            tw=font->getTextWidth(numtext,n);
            th=font->getFontHeight();
            tx=tx+d/2-tw/2;
            ty=ty+d/2+font->getFontAscent()+5;
            //dc.setForeground(textNumColor);
#ifdef HAVE_XFT_H
            dc.setForeground(barBGColor);             // Code for XFT until XFT can use BLT_SRC_XOR_DST
            dc.drawText(tx-1,ty,numtext,n);
            dc.drawText(tx+1,ty,numtext,n);
            dc.drawText(tx,ty-1,numtext,n);
            dc.drawText(tx,ty+1,numtext,n);
            dc.setForeground(textNumColor);
            dc.drawText(tx,ty,numtext,n);
#else
            dc.setForeground(FXRGB(255,255,255));     // Original code
            dc.setFunction(BLT_SRC_XOR_DST);
            dc.drawText(tx,ty,numtext,n);
#endif
        }
    }

    // Vertical bar
    else if (options&PROGRESSBAR_VERTICAL)
    {
        // If total is 0, it's 100%
        barlength=height-border-border;
        barfilled=barlength;
        percent=100;
        if (total!=0)
        {
            barfilled=(unsigned int) (((double)progress * (double)barlength) / (double)total);
            percent=(unsigned int) (((double)progress * 100.0) / (double)total);
        }

        // Draw completed bar
        if (0<barfilled)
        {
            // Clearlooks (simple gradient)
            if (use_clearlooks)
            {
                dc.setForeground(barColor);
                drawGradientRectangle(dc,topcolor,bottomcolor,border,height-border-barfilled,width-(border<<1),barfilled,FALSE);
            }
            // Standard (flat)
            else
            {
                dc.setForeground(barColor);
                dc.fillRectangle(border,height-border-barfilled,width-(border<<1),barfilled);
            }
        }

        // Draw uncompleted bar
        if (barfilled<barlength)
        {
            dc.setForeground(barBGColor);
            dc.fillRectangle(border,border,width-(border<<1),barlength-barfilled);
        }

        // Draw text
        if (options&PROGRESSBAR_PERCENTAGE)
        {
            dc.setFont(font);
            sprintf(numtext,"%d%%",percent);
            n=strlen(numtext);
            tw=font->getTextWidth(numtext,n);
            th=font->getFontHeight();
            ty=(height-th)/2+font->getFontAscent();
            tx=(width-tw)/2;
            if (height-border-barfilled>ty)           // In upper side
            {
                dc.setForeground(textNumColor);
                dc.setClipRectangle(border,border,width-(border<<1),height-(border<<1));
                dc.drawText(tx,ty,numtext,n);
            }
            else if (ty-th>height-border-barfilled)   // In lower side
            {
                dc.setForeground(textAltColor);
                dc.setClipRectangle(border,border,width-(border<<1),height-(border<<1));
                dc.drawText(tx,ty,numtext,n);
            }
            else                                      // In between!
            {
                dc.setForeground(textAltColor);
                dc.setClipRectangle(border,height-border-barfilled,width-(border<<1),barfilled);
                dc.drawText(tx,ty,numtext,n);
                dc.setForeground(textNumColor);
                dc.setClipRectangle(border,border,width-(border<<1),barlength-barfilled);
                dc.drawText(tx,ty,numtext,n);
                dc.clearClipRectangle();
            }
        }
    }

    // Horizontal bar
    else
    {
        // If total is 0, it's 100%
        barlength=width-border-border;
        barfilled=barlength;
        percent=100;
        if (total!=0)
        {
            barfilled=(unsigned int) (((double)progress * (double)barlength) / (double)total);
            percent=(unsigned int) (((double)progress * 100.0) / (double)total);
        }

        // Draw completed bar
        if (0<barfilled)
        {
            // Clearlooks (simple gradient)
            if (use_clearlooks)
            {
                dc.setForeground(barColor);
                drawGradientRectangle(dc,topcolor,bottomcolor,border,border,barfilled,height-(border<<1),TRUE);
            }
            // Standard (flat)
            else
            {
                dc.setForeground(barColor);
                dc.fillRectangle(border,border,barfilled,height-(border<<1));
            }
        }

        // Draw uncompleted bar
        if (barfilled<barlength)
        {
            dc.setForeground(barBGColor);
            dc.fillRectangle(border+barfilled,border,barlength-barfilled,height-(border<<1));
        }

        // Draw text
        if (options&PROGRESSBAR_PERCENTAGE)
        {
            dc.setFont(font);
            sprintf(numtext,"%d%%",percent);
            n=strlen(numtext);
            tw=font->getTextWidth(numtext,n);
            th=font->getFontHeight();
            ty=(height-th)/2+font->getFontAscent();
            tx=(width-tw)/2;
            if (border+barfilled<=tx)           // In right side
            {
                dc.setForeground(textNumColor);
                dc.setClipRectangle(border,border,width-(border<<1),height-(border<<1));
                dc.drawText(tx,ty,numtext,n);
            }
            else if (tx+tw<=border+barfilled)   // In left side
            {
                dc.setForeground(textAltColor);
                dc.setClipRectangle(border,border,width-(border<<1),height-(border<<1));
                dc.drawText(tx,ty,numtext,n);
            }
            else                                // In between!
            {
                dc.setForeground(textAltColor);
                dc.setClipRectangle(border,border,barfilled,height);
                dc.drawText(tx,ty,numtext,n);
                dc.setForeground(textNumColor);
                dc.setClipRectangle(border+barfilled,border,barlength-barfilled,height);
                dc.drawText(tx,ty,numtext,n);
                dc.clearClipRectangle();
            }
        }
    }
}


//
// Hack of FXButton
//

// This hack fixes a focus problem on the panels when activating a button which is already activated
// Now, the focus on the active panel is not lost anymore


// Pressed mouse button
long FXButton::onLeftBtnPress(FXObject*,FXSelector,void* ptr)
{
    handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
    flags&=~FLAG_TIP;
    if (isEnabled() && !(flags&FLAG_PRESSED))
    {
        grab();
        if (target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr))
            return 1;
        //if(state!=STATE_ENGAGED) // !!! Hack here !!!
        setState(STATE_DOWN);
        flags|=FLAG_PRESSED;
        flags&=~FLAG_UPDATE;
        return 1;
    }
    return 0;
}

// Hot key combination pressed
long FXButton::onHotKeyPress(FXObject*,FXSelector,void* ptr)
{
    flags&=~FLAG_TIP;
    handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
    if (isEnabled() && !(flags&FLAG_PRESSED))
    {
        //if(state!=STATE_ENGAGED)  // !!! Hack here !!!
        setState(STATE_DOWN);
        flags&=~FLAG_UPDATE;
        flags|=FLAG_PRESSED;
    }
    return 1;
}


//
// Hack of FXTopWindow
//

// This hack fixes a problem with some window managers like Icewm or Openbox
// These WMs do not deal with StaticGravity the same way as e.g. Metacity
// and then the window border can be invisible when launching the applications

// Request for toplevel window resize
void FXTopWindow::resize(int w,int h)
{
    if ((flags&FLAG_DIRTY) || (w!=width) || (h!=height))
    {
        width=FXMAX(w,1);
        height=FXMAX(h,1);
        if (xid)
        {
            XWindowChanges changes;
            XSizeHints size;
            size.flags=USSize|PSize|PWinGravity|USPosition|PPosition;
            size.x=xpos;
            size.y=ypos;
            size.width=width;
            size.height=height;
            size.min_width=0;
            size.min_height=0;
            size.max_width=0;
            size.max_height=0;
            size.width_inc=0;
            size.height_inc=0;
            size.min_aspect.x=0;
            size.min_aspect.y=0;
            size.max_aspect.x=0;
            size.max_aspect.y=0;
            size.base_width=0;
            size.base_height=0;

            // !!! Hack here !!!
            size.win_gravity=NorthWestGravity;                      // Tim Alexeevsky <realtim@mail.ru>
            //size.win_gravity=StaticGravity;                       // Account for border (ICCCM)
            // !!! End of hack !!!

            if (!(options&DECOR_SHRINKABLE))
            {
                if (!(options&DECOR_STRETCHABLE))                       // Cannot change at all
                {
                    size.flags|=PMinSize|PMaxSize;
                    size.min_width=size.max_width=width;
                    size.min_height=size.max_height=height;
                }
                else                                                    // Cannot get smaller than default
                {
                    size.flags|=PMinSize;
                    size.min_width=getDefaultWidth();
                    size.min_height=getDefaultHeight();
                }
            }
            else if (!(options&DECOR_STRETCHABLE))                      // Cannot get larger than default
            {
                size.flags|=PMaxSize;
                size.max_width=getDefaultWidth();
                size.max_height=getDefaultHeight();
            }
            XSetWMNormalHints(DISPLAY(getApp()),xid,&size);
            changes.x=0;
            changes.y=0;
            changes.width=width;
            changes.height=height;
            changes.border_width=0;
            changes.sibling=None;
            changes.stack_mode=Above;
            XReconfigureWMWindow(DISPLAY(getApp()),xid,DefaultScreen(DISPLAY(getApp())),CWWidth|CWHeight,&changes);
            layout();
        }
    }
}

// Request for toplevel window reposition
void FXTopWindow::position(int x,int y,int w,int h)
{
    if ((flags&FLAG_DIRTY) || (x!=xpos) || (y!=ypos) || (w!=width) || (h!=height))
    {
        xpos=x;
        ypos=y;
        width=FXMAX(w,1);
        height=FXMAX(h,1);
        if (xid)
        {
            XWindowChanges changes;
            XSizeHints size;
            size.flags=USSize|PSize|PWinGravity|USPosition|PPosition;
            size.x=xpos;
            size.y=ypos;
            size.width=width;
            size.height=height;
            size.min_width=0;
            size.min_height=0;
            size.max_width=0;
            size.max_height=0;
            size.width_inc=0;
            size.height_inc=0;
            size.min_aspect.x=0;
            size.min_aspect.y=0;
            size.max_aspect.x=0;
            size.max_aspect.y=0;
            size.base_width=0;
            size.base_height=0;

            // !!! Hack here !!!
            size.win_gravity=NorthWestGravity;                      // Tim Alexeevsky <realtim@mail.ru>
            //size.win_gravity=StaticGravity;                       // Account for border (ICCCM)
            // !!! End of hack !!!

            if (!(options&DECOR_SHRINKABLE))
            {
                if (!(options&DECOR_STRETCHABLE))                         // Cannot change at all
                {
                    size.flags|=PMinSize|PMaxSize;
                    size.min_width=size.max_width=width;
                    size.min_height=size.max_height=height;
                }
                else                                                      // Cannot get smaller than default
                {
                    size.flags|=PMinSize;
                    size.min_width=getDefaultWidth();
                    size.min_height=getDefaultHeight();
                }
            }
            else if (!(options&DECOR_STRETCHABLE))                        // Cannot get larger than default
            {
                size.flags|=PMaxSize;
                size.max_width=getDefaultWidth();
                size.max_height=getDefaultHeight();
            }
            XSetWMNormalHints(DISPLAY(getApp()),xid,&size);
            changes.x=xpos;
            changes.y=ypos;
            changes.width=width;
            changes.height=height;
            changes.border_width=0;
            changes.sibling=None;
            changes.stack_mode=Above;
            XReconfigureWMWindow(DISPLAY(getApp()),xid,DefaultScreen(DISPLAY(getApp())),CWX|CWY|CWWidth|CWHeight,&changes);
            layout();
        }
    }
}


// Position the window based on placement
void FXTopWindow::place(unsigned int placement)
{
    int rx,ry,rw,rh,ox,oy,ow,oh,wx,wy,ww,wh,x,y;
    unsigned int state;
    FXWindow *over;

    // Default placement:- leave it where it was
    wx=getX();
    wy=getY();
    ww=getWidth();
    wh=getHeight();

    // Get root window size
    rx=getRoot()->getX();
    ry=getRoot()->getY();
    rw=getRoot()->getWidth();
    rh=getRoot()->getHeight();

    // Placement policy
    switch (placement)
    {

        // Place such that it contains the cursor
    case PLACEMENT_CURSOR:

        // Get dialog location in root coordinates
        translateCoordinatesTo(wx,wy,getRoot(),0,0);

        // Where's the mouse?
        getRoot()->getCursorPosition(x,y,state);

        // Place such that mouse in the middle, placing it as
        // close as possible in the center of the owner window.
        // Don't move the window unless the mouse is not inside.

        // !!! Hack here !!!
        //if (!shown() || x<wx || y<wy || wx+ww<=x || wy+wh<=y)
        if (x<wx || y<wy || wx+ww<=x || wy+wh<=y)
            // !!! End of hack !!!
        {

            // Get the owner
            over=getOwner()?getOwner():getRoot();

            // Get owner window size
            ow=over->getWidth();
            oh=over->getHeight();

            // Owner's coordinates to root coordinates
            over->translateCoordinatesTo(ox,oy,getRoot(),0,0);

            // Adjust position
            wx=ox+(ow-ww)/2;
            wy=oy+(oh-wh)/2;

            // Move by the minimal amount
            if (x<wx)
                wx=x-20;
            else if (wx+ww<=x)
                wx=x-ww+20;
            if (y<wy)
                wy=y-20;
            else if (wy+wh<=y)
                wy=y-wh+20;
        }

        // Adjust so dialog is fully visible
        if (wx<rx)
            wx=rx+10;
        if (wy<ry)
            wy=ry+10;
        if (wx+ww>rx+rw)
            wx=rx+rw-ww-10;
        if (wy+wh>ry+rh)
            wy=ry+rh-wh-10;
        break;

        // Place centered over the owner
    case PLACEMENT_OWNER:

        // Get the owner
        over=getOwner()?getOwner():getRoot();

        // Get owner window size
        ow=over->getWidth();
        oh=over->getHeight();

        // Owner's coordinates to root coordinates
        over->translateCoordinatesTo(ox,oy,getRoot(),0,0);

        // Adjust position
        wx=ox+(ow-ww)/2;
        wy=oy+(oh-wh)/2;

        // Adjust so dialog is fully visible
        if (wx<rx)
            wx=rx+10;
        if (wy<ry)
            wy=ry+10;
        if (wx+ww>rx+rw)
            wx=rx+rw-ww-10;
        if (wy+wh>ry+rh)
            wy=ry+rh-wh-10;
        break;

        // Place centered on the screen
    case PLACEMENT_SCREEN:

        // Adjust position
        wx=rx+(rw-ww)/2;
        wy=ry+(rh-wh)/2;
        break;

        // Place to make it fully visible
    case PLACEMENT_VISIBLE:

        // Adjust so dialog is fully visible
        if (wx<rx)
            wx=rx+10;
        if (wy<ry)
            wy=ry+10;
        if (wx+ww>rx+rw)
            wx=rx+rw-ww-10;
        if (wy+wh>ry+rh)
            wy=ry+rh-wh-10;
        break;

        // Place maximized
    case PLACEMENT_MAXIMIZED:
        wx=rx;
        wy=ry;
        ww=rw;                // Yes, I know:- we should substract the borders;
        wh=rh;                // trouble is, no way to know how big those are....
        break;

        // Default placement
    case PLACEMENT_DEFAULT:
    default:
        break;
    }

    // Place it
    position(wx,wy,ww,wh);
}


//
// Hack of FXPacker
//

// This hack optionally draws a rectangle with rounded corners (Clearlooks)
void FXPacker::drawGrooveRectangle(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h)
{
    static FXbool init=TRUE;
    static FXbool use_clearlooks=TRUE;
    static FXColor shadecolor, bordercolor;

    FXPoint bordershade[16]= {FXPoint(x,y+1),FXPoint(x+1,y),FXPoint(x+1,y+2),FXPoint(x+2,y+1),
                              FXPoint(x+w-2,y),FXPoint(x+w-1,y+1),FXPoint(x+w-3,y+1),
                              FXPoint(x+w-2,y+2),FXPoint(x,y+h-2),FXPoint(x+1,y+h-1),
                              FXPoint(x+1,y+h-3),FXPoint(x+2,y+h-2),
                              FXPoint(x+w-1,y+h-2),FXPoint(x+w-2,y+h-1),
                              FXPoint(x+w-2,y+h-3),FXPoint(x+w-3,y+h-2)
                             };
    FXPoint bordercorners[4]= {FXPoint(x+1,y+1),FXPoint(x+1,y+h-2),FXPoint(x+w-2,y+1),
                               FXPoint(x+w-2,y+h-2)
                              };

    if (init)
    {
        use_clearlooks=getApp()->reg().readUnsignedEntry("SETTINGS","use_clearlooks",TRUE);

        if (use_clearlooks)
        {
            unsigned int r=FXREDVAL(backColor);
            unsigned int g=FXGREENVAL(backColor);
            unsigned int b=FXBLUEVAL(backColor);

            shadecolor=FXRGB(0.9*r,0.9*g,0.9*b);
            bordercolor=FXRGB(0.5*r,0.5*g,0.5*b);
        }
        init=FALSE;
    }

    if (0<w && 0<h)
    {
        // Rectangle with rounded corners (Clearlooks)
        if (use_clearlooks)
        {
            // Draw the 4 edges
            dc.setForeground(shadowColor);
            dc.drawRectangle(x+w-1,y+2,0,h-5); // right
            dc.drawRectangle(x,y+2,0,h-5); // left
            dc.drawRectangle(x+2,y,w-5,0); // up
            dc.drawRectangle(x+2,y+h-1,w-5,0); // down

            // Draw the 4 rounded corners (with shade)
            dc.setForeground(shadowColor);
            dc.drawPoints(bordercorners,4);
            dc.setForeground(shadecolor);
            dc.drawPoints(bordershade,16);
        }

        // Standard rectangle
        else
        {
            dc.setForeground(shadowColor);
            dc.fillRectangle(x,y,w,1);
            dc.fillRectangle(x,y,1,h);
            dc.setForeground(hiliteColor);
            dc.fillRectangle(x,y+h-1,w,1);
            dc.fillRectangle(x+w-1,y,1,h);
            if (1<w && 1<h)
            {
                dc.setForeground(shadowColor);
                dc.fillRectangle(x+1,y+h-2,w-2,1);
                dc.fillRectangle(x+w-2,y+1,1,h-2);
                dc.setForeground(hiliteColor);
                dc.fillRectangle(x+1,y+1,w-3,1);
                dc.fillRectangle(x+1,y+1,1,h-3);
            }
        }
    }
}


//
// Hack of FXAccelTable
//

// This hack allows to ignore caps lock when using keyboard shortcuts

#define EMPTYSLOT       0xfffffffe   // Previously used, now empty slot
#define UNUSEDSLOT      0xffffffff   // Unsused slot marker

// Keyboard press; forward to accelerator target
long FXAccelTable::onKeyPress(FXObject* sender,FXSelector,void* ptr)
{
    FXTRACE((200,"%p->FXAccelTable::onKeyPress keysym=0x%04x state=%04x\n",this,((FXEvent*)ptr)->code,((FXEvent*)ptr)->state));
    register FXEvent* event=(FXEvent*)ptr;

    // !!! Hack here !!!
    //register FXuint code=MKUINT(event->code,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK));
    register FXuint code;
    if (event->state&CAPSLOCKMASK)
        code=MKUINT(event->code+32,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK));
    else
        code=MKUINT(event->code,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK));
    // !!! End of hack !!!

    register FXuint p=(code*13)&max;
    register FXuint c;
    FXASSERT(code!=UNUSEDSLOT);
    FXASSERT(code!=EMPTYSLOT);
    while((c=key[p].code)!=code)
    {
        if(c==UNUSEDSLOT) return 0;
        p=(p+1)&max;
    }
    if(key[p].target && key[p].messagedn)
    {
        key[p].target->tryHandle(sender,key[p].messagedn,ptr);
    }
    return 1;
}


// Keyboard release; forward to accelerator target
long FXAccelTable::onKeyRelease(FXObject* sender,FXSelector,void* ptr)
{
    FXTRACE((200,"%p->FXAccelTable::onKeyRelease keysym=0x%04x state=%04x\n",this,((FXEvent*)ptr)->code,((FXEvent*)ptr)->state));
    register FXEvent* event=(FXEvent*)ptr;

    // !!! Hack here !!!
    //register FXuint code=MKUINT(event->code,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK));
    register FXuint code;
    if (event->state&CAPSLOCKMASK)
        code=MKUINT(event->code+32,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK));
    else
        code=MKUINT(event->code,event->state&(SHIFTMASK|CONTROLMASK|ALTMASK|METAMASK));
    // !!! End of hack !!!

    register FXuint p=(code*13)&max;
    register FXuint c;
    FXASSERT(code!=UNUSEDSLOT);
    FXASSERT(code!=EMPTYSLOT);
    while((c=key[p].code)!=code)
    {
        if(c==UNUSEDSLOT) return 0;
        p=(p+1)&max;
    }
    if(key[p].target && key[p].messageup)
    {
        key[p].target->tryHandle(sender,key[p].messageup,ptr);
    }
    return 1;
}


//
// Hack of FXURL
//

// Backport from Fox 1.7.37 to fix a bug when filenames contain '%' characters

// Hexadecimal digit of value
const FXchar value2Digit[36]=
{
    '0','1','2','3','4','5','6','7','8','9','A','B',
    'C','D','E','F','G','H','I','J','K','L','M','N',
    'O','P','Q','R','S','T','U','V','W','X','Y','Z',
};

// Encode control characters and characters from set using %-encoding
FXString FXURL::encode(const FXString& url)
{
    FXString result;
    if(!url.empty())
    {
        register FXint p,q,c;
        for(p=q=0; p<url.length(); ++p)
        {
            c=(FXuchar)url[p];
            if(c<0x20 || c=='%')
            {
                q+=3;
                continue;
            }
            q++;
        }
        result.length(q);
        for(p=q=0; p<url.length(); ++p)
        {
            c=(FXuchar)url[p];
            if(c<0x20 || c=='%')
            {
                result[q++]='%';
                result[q++]=value2Digit[c>>4];
                result[q++]=value2Digit[c&15];
                continue;
            }
            result[q++]=c;
        }
    }
    return result;
}


// Decode string containing %-encoded characters
FXString FXURL::decode(const FXString& url)
{
    FXString result;
    if(!url.empty())
    {
        register FXint p,q,c;
        for(p=q=0; p<url.length(); ++p)
        {
            c=(FXuchar)url[p];
            if(c=='%' && Ascii::isHexDigit(url[p+1]) && Ascii::isHexDigit(url[p+2]))
            {
                p+=2;
            }
            q++;
        }
        result.length(q);
        for(p=q=0; p<url.length(); ++p)
        {
            c=(FXuchar)url[p];
            if(c=='%' && Ascii::isHexDigit(url[p+1]) && Ascii::isHexDigit(url[p+2]))
            {
                c=(Ascii::digitValue(url[p+1])<<4)+Ascii::digitValue(url[p+2]);
                p+=2;
            }
            result[q++]=c;
        }
    }
    return result;
}

