#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxdefs.h>
#include <FXPNGIcon.h>

#include "icons.h"
#include "OverwriteBox.h"

// Padding for message box buttons
#define HORZ_PAD 30
#define VERT_PAD 2


// Map
FXDEFMAP(OverwriteBox) OverwriteBoxMap[]=
{
	FXMAPFUNCS(SEL_COMMAND,OverwriteBox::ID_CLICKED_CANCEL,OverwriteBox::ID_CLICKED_SKIP_ALL,OverwriteBox::onCmdClicked),
};



// Object implementation
FXIMPLEMENT(OverwriteBox,DialogBox,OverwriteBoxMap,ARRAYNUMBER(OverwriteBoxMap))


// Create message box
OverwriteBox::OverwriteBox(FXWindow *win,const FXString& name,const FXString& text,unsigned int opts,int x,int y):
        DialogBox(win,name,opts|DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,x,y,0,0)
{
    FXVerticalFrame* content=new FXVerticalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y);
    FXHorizontalFrame* info=new FXHorizontalFrame(content,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,10,10,10,10);
    new FXLabel(info,FXString::null,questionbigicon,ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    new FXLabel(info,text,NULL,JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    new FXHorizontalSeparator(content,SEPARATOR_GROOVE|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X);
    FXHorizontalFrame* buttons=new FXHorizontalFrame(content,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0,10,10,10,10);
    new FXButton(buttons,_("&Cancel"),NULL,this,ID_CLICKED_CANCEL,FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    new FXButton(buttons,_("&Skip"),NULL,this,ID_CLICKED_SKIP,FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    new FXButton(buttons,_("Skip A&ll"),NULL,this,ID_CLICKED_SKIP_ALL,FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    new FXButton(buttons,_("&Yes"),NULL,this,ID_CLICKED_OVERWRITE,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
    new FXButton(buttons,_("Yes for &All"),NULL,this,ID_CLICKED_OVERWRITE_ALL,FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
}


// Close dialog
long OverwriteBox::onCmdClicked(FXObject*,FXSelector sel,void*)
{
    getApp()->stopModal(this,OVWBOX_CLICKED_CANCEL+(FXSELID(sel)-ID_CLICKED_CANCEL));
    hide();
    return 1;
}

