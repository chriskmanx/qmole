// Display a history list box and allows the user to select a string
// This is based on FXChoiceBox

#include "config.h"
#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>

#include "xfedefs.h"
#include "DirHistBox.h"


#define VISIBLE_LINES 10


// Map
FXDEFMAP(DirHistBox) DirHistBoxMap[]=
{
	FXMAPFUNC(SEL_KEYPRESS,0,DirHistBox::onKeyPress),
	FXMAPFUNC(SEL_KEYRELEASE,0,DirHistBox::onKeyRelease),
	FXMAPFUNC(SEL_FOCUSOUT,0,DirHistBox::onCmdClose),
	FXMAPFUNC(SEL_COMMAND,DirHistBox::ID_CLOSE,DirHistBox::onCmdClose),
	FXMAPFUNC(SEL_CLICKED,DirHistBox::ID_CLICKED,DirHistBox::onCmdClicked),
};


// Object implementation
FXIMPLEMENT(DirHistBox,DialogBox,DirHistBoxMap,ARRAYNUMBER(DirHistBoxMap))


// Construct list box with given caption, icon, message text, and with choices from array of strings
DirHistBox::DirHistBox(FXWindow* owner,const char** choices,unsigned int opts,int x,int y,int w,int h):
        DialogBox(owner,"",opts,x,y,w,h,0,0,0,0, 0,0)
{
    register int n;
	FXHorizontalFrame* hor=new FXHorizontalFrame(this,FRAME_RAISED|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
    list=new FXList(hor,this,ID_CLICKED,LIST_BROWSESELECT|LAYOUT_FILL_Y|LAYOUT_FILL_X|HSCROLLING_OFF);
	list->setBackColor(this->getBackColor());
    n=list->fillItems(choices);
    list->setNumVisible(FXMIN(n,VISIBLE_LINES));
}


// Construct list box with given caption, icon, message text, and with choices from newline separated strings
DirHistBox::DirHistBox(FXWindow* owner,const FXString& choices,unsigned int opts,int x,int y,int w,int h):
        DialogBox(owner,"",opts,x,y,w,h,0,0,0,0, 0,0)
{
    register int n;
	FXHorizontalFrame* hor=new FXHorizontalFrame(this,FRAME_RAISED|FRAME_THICK|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
    list=new FXList(hor,this,ID_CLICKED,LIST_BROWSESELECT|LAYOUT_FILL_Y|LAYOUT_FILL_X|HSCROLLING_OFF);
	list->setBackColor(this->getBackColor());
    n=list->fillItems(choices);
    list->setNumVisible(FXMIN(n,VISIBLE_LINES));
}

// Select item when click in list
long DirHistBox::onCmdClicked(FXObject*,FXSelector,void*)
{
    getApp()->stopModal(this,list->getCurrentItem());
    hide();
    return 1;
}


// Close dialog
long DirHistBox::onCmdClose(FXObject*,FXSelector,void*)
{
    getApp()->stopModal(this,-1);
    hide();
    return 1;
}


// Destroy list box
DirHistBox::~DirHistBox()
{
    list=(FXList*)-1L;
}


// Show a modal list dialog
int DirHistBox::box(FXWindow* owner,unsigned int opts,const char** choices,int x,int y,int w,int h)
{
    DirHistBox box(owner,choices,opts,x,y,w,h);
    return box.execute(PLACEMENT_DEFAULT);
}


// Show a modal list dialog
int DirHistBox::box(FXWindow* owner,unsigned int opts,const FXString& choices,int x,int y,int w,int h)
{
    DirHistBox box(owner,choices,opts,x,y,w,h);
    return box.execute(PLACEMENT_DEFAULT);
}


// Keyboard press; handle escape to close the dialog
long DirHistBox::onKeyPress(FXObject* sender,FXSelector sel,void* ptr)
{
	if(FXTopWindow::onKeyPress(sender,sel,ptr))
		return 1;
	if(((FXEvent*)ptr)->code==KEY_Escape)
	{
		handle(this,FXSEL(SEL_COMMAND,ID_CLOSE),NULL);
		return 1;
    }
	return 0;
}


// Keyboard release; handle escape to close the dialog
long DirHistBox::onKeyRelease(FXObject* sender,FXSelector sel,void* ptr)
{
	if(FXTopWindow::onKeyRelease(sender,sel,ptr))
		return 1;
	if(((FXEvent*)ptr)->code==KEY_Escape)
		return 1;
	return 0;
}

