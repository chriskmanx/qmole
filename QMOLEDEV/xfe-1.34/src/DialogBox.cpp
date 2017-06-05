// Dialog Box with additional toggle option
#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>

#include "DialogBox.h"



// Map
FXDEFMAP(DialogBox) DialogBoxMap[]=
{
	FXMAPFUNC(SEL_KEYPRESS,0,DialogBox::onKeyPress),
	FXMAPFUNC(SEL_CLOSE,0,DialogBox::onClose),
	FXMAPFUNC(SEL_COMMAND,DialogBox::ID_CANCEL,DialogBox::onCmdCancel),
	FXMAPFUNC(SEL_COMMAND,DialogBox::ID_ACCEPT,DialogBox::onCmdAccept),
	FXMAPFUNC(SEL_COMMAND,DialogBox::ID_TOGGLE_OPTION,DialogBox::onCmdToggleOption)
};


// Object implementation
FXIMPLEMENT(DialogBox,FXTopWindow,DialogBoxMap,ARRAYNUMBER(DialogBoxMap))


// Contruct dialog which will stay on top of owner
DialogBox::DialogBox(FXWindow* win,const FXString& name,unsigned int opts,int x,int y,int w,int h,int pl,int pr,int pt,int pb,int hs,int vs):
        FXTopWindow(win,name,NULL,NULL,opts,x,y,w,h,pl,pr,pt,pb,hs,vs)
{
    _option = 0;
}

// Contruct free floating dialog
DialogBox::DialogBox(FXApp* a,const FXString& name,unsigned int opts,int x,int y,int w,int h,int pl,int pr,int pt,int pb,int hs,int vs):
        FXTopWindow(a,name,NULL,NULL,opts,x,y,w,h,pl,pr,pt,pb,hs,vs)
{
    _option = 0;
}


// Close window & cancel out of dialog
long DialogBox::onClose(FXObject*,FXSelector,void*)
{
    if(target && target->handle(this,FXSEL(SEL_CLOSE,message),NULL))
        return 1;
    handle(this,FXSEL(SEL_COMMAND,DialogBox::ID_CANCEL),NULL);
    return 1;
}


// Close dialog with an accept
long DialogBox::onCmdAccept(FXObject*,FXSelector,void*)
{
    getApp()->stopModal(this,TRUE);
    hide();
    return 1;
}


// Close dialog with a cancel
long DialogBox::onCmdCancel(FXObject*,FXSelector,void*)
{
    getApp()->stopModal(this,FALSE);
    hide();
    return 1;
}

// Toggle option
long DialogBox::onCmdToggleOption(FXObject*,FXSelector,void*)
{
    _option = !_option;
    return 1;
}

// Get option state
unsigned int DialogBox::getOption()
{
    return _option;
}



// Create window
void DialogBox::create()
{
    FXTopWindow::create();
}

// Show window such that the cursor is in it
void DialogBox::show(unsigned int placement)
{
    int rw,rh,wx,wy,ww,wh,x,y;
    unsigned int state;

    // Get dialog size
    translateCoordinatesTo(wx,wy,getRoot(),0,0);
    ww=getWidth();
    wh=getHeight();

    // Where's the mouse?
    getRoot()->getCursorPosition(x,y,state);

    // Place such that mouse in the middle
    if(x<wx || y<wy || wx+ww<=x || wy+wh<=y)
    {

        // Get root window size
        rw=getRoot()->getWidth();
        rh=getRoot()->getHeight();

        // Move by the minimal amount
        if(x<wx)
            wx=x-20;
        else if(wx+ww<=x)
            wx=x-ww+20;
        if(y<wy)
            wy=y-20;
        else if(wy+wh<=y)
            wy=y-wh+20;

        // Adjust so dialog is fully visible
        if(wx<0)
            wx=10;
        if(wy<0)
            wy=10;
        if(wx+ww>rw)
            wx=rw-ww-10;
        if(wy+wh>rh)
            wy=rh-wh-10;

        move(wx,wy);
    }

    // Pop the window
    FXTopWindow::show(placement);
}

// Keyboard press; handle escape and return to close the dialog
long DialogBox::onKeyPress(FXObject* sender,FXSelector sel,void* ptr)
{
	if (FXTopWindow::onKeyPress(sender,sel,ptr))
		return 1;
	if (((FXEvent*)ptr)->code==KEY_Escape)
	{
		handle(this,FXSEL(SEL_COMMAND,ID_CANCEL),NULL);
		return 1;
	}
	return 0;
}


// Execute dialog box modally
unsigned int DialogBox::execute(unsigned int placement)
{
    create();
    show(placement);
	getApp()->refresh();
    return getApp()->runModalFor(this);
}

