// Message box. Taken from the FOX library and slightly modified for translation purpose.
// Also added a SU button

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>

#include "icons.h"
#include "xfedefs.h"
#include "xfeutils.h"
#include "MessageBox.h"



// Padding for message box buttons
#define HORZ_PAD 30
#define VERT_PAD 2

#define BOX_BUTTON_MASK  (BOX_OK|BOX_OK_CANCEL|BOX_YES_NO|BOX_YES_NO_CANCEL|BOX_QUIT_CANCEL|BOX_QUIT_SAVE_CANCEL|BOX_OK_SU|BOX_YES_NO_ALL_CANCEL)


// Map
FXDEFMAP(MessageBox) MessageBoxMap[]=
{
	FXMAPFUNC(SEL_COMMAND,MessageBox::ID_CANCEL,MessageBox::onCmdCancel),
	FXMAPFUNCS(SEL_COMMAND,MessageBox::ID_CLICKED_YES,MessageBox::ID_CLICKED_ALL,MessageBox::onCmdClicked),
	FXMAPFUNC(SEL_COMMAND,MessageBox::ID_CLICKED_SU,MessageBox::onCmdSu),
};



// Object implementation
FXIMPLEMENT(MessageBox,DialogBox,MessageBoxMap,ARRAYNUMBER(MessageBoxMap))


// Construct message box with given caption, icon, and message text
MessageBox::MessageBox(FXWindow* owner,const FXString& caption,const FXString& text,FXIcon* ic,unsigned int opts,unsigned int textopts,int x,int y):
        DialogBox(owner,caption,opts|DECOR_TITLE|DECOR_BORDER|DECOR_STRETCHABLE,x,y,0,0, 0,0,0,0, 4,4)
{
    initialize(text,ic,opts&BOX_BUTTON_MASK,textopts);
}


// Construct free floating message box with given caption, icon, and message text
MessageBox::MessageBox(FXApp* a,const FXString& caption,const FXString& text,FXIcon* ic,unsigned int opts,unsigned int textopts,int x,int y):
        DialogBox(a,caption,opts|DECOR_TITLE|DECOR_BORDER|DECOR_STRETCHABLE,x,y,0,0, 0,0,0,0, 4,4)
{
    initialize(text,ic,opts&BOX_BUTTON_MASK,textopts);
}


// Build contents
void MessageBox::initialize(const FXString& text,FXIcon* ic, unsigned int whichbuttons, unsigned int textoptions)
{
    FXButton *initial;
    FXVerticalFrame* content=new FXVerticalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y);
    FXHorizontalFrame* info=new FXHorizontalFrame(content,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0,10,10,10,10);

    // Message text
    msg=new FXLabel(info,text,ic,textoptions);

    new FXHorizontalSeparator(content,SEPARATOR_GROOVE|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X);
    FXHorizontalFrame* buttons=new FXHorizontalFrame(content,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|PACK_UNIFORM_WIDTH,0,0,0,0,10,10,10,10);
    if(whichbuttons==BOX_OK)
    {
        initial=new FXButton(buttons,_("&OK"),NULL,this,ID_CLICKED_OK,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        initial->setFocus();
    }
    else if(whichbuttons==BOX_OK_SU)
    {
        initial=new FXButton(buttons,_("&OK"),NULL,this,ID_CLICKED_OK,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
		
		// Su button only if allowed
		FXbool root_mode=getApp()->reg().readUnsignedEntry("OPTIONS","root_mode",TRUE);
		if (root_mode)
		{
			FXString key=getApp()->reg().readStringEntry("KEYBINDINGS","new_root_window","Shift-F3");
			// Space before tab is used to set the correct button height
			FXButton *btn=new FXButton(buttons," "+TAB+_("Launch Xfe as root")+PARS(key),minixferooticon,this,ID_CLICKED_SU,BUTTON_DEFAULT|ICON_AFTER_TEXT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
			FXHotKey hotkey=_parseAccel(key);
			btn->addHotKey(hotkey);
		}
		initial->setFocus();
    }
    else if(whichbuttons==BOX_OK_CANCEL)
    {
        initial=new FXButton(buttons,_("&Cancel"),NULL,this,ID_CLICKED_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("&OK"),NULL,this,ID_CLICKED_OK,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        initial->setFocus();
    }
    else if(whichbuttons==BOX_YES_NO)
    {
        initial=new FXButton(buttons,_("&No"),NULL,this,ID_CLICKED_NO,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
		new FXButton(buttons,_("&Yes"),NULL,this,ID_CLICKED_YES,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        initial->setFocus();
    }
    else if(whichbuttons==BOX_YES_NO_CANCEL)
    {
        initial=new FXButton(buttons,_("&Cancel"),NULL,this,ID_CLICKED_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("&Yes"),NULL,this,ID_CLICKED_YES,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("&No"),NULL,this,ID_CLICKED_NO,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        initial->setFocus();
    }
    else if(whichbuttons==BOX_QUIT_CANCEL)
    {
        initial=new FXButton(buttons,_("&Cancel"),NULL,this,ID_CLICKED_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("&Quit"),NULL,this,ID_CLICKED_QUIT,BUTTON_INITIAL|BUTTON_DEFAULT|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        initial->setFocus();
    }
    else if(whichbuttons==BOX_QUIT_SAVE_CANCEL)
    {
        initial=new FXButton(buttons,_("&Cancel"),NULL,this,ID_CLICKED_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("&Quit"),NULL,this,ID_CLICKED_QUIT,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("&Save"),NULL,this,ID_CLICKED_SAVE,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        initial->setFocus();
    }
    else if(whichbuttons==BOX_YES_NO_ALL_CANCEL)
    {
        initial=new FXButton(buttons,_("&Cancel"),NULL,this,ID_CLICKED_CANCEL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("&Yes"),NULL,this,ID_CLICKED_YES,BUTTON_INITIAL|BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("&No"),NULL,this,ID_CLICKED_NO,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        new FXButton(buttons,_("Yes for &All"),NULL,this,ID_CLICKED_ALL,BUTTON_DEFAULT|FRAME_RAISED|FRAME_THICK|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_CENTER_X,0,0,0,0,HORZ_PAD,HORZ_PAD,VERT_PAD,VERT_PAD);
        initial->setFocus();
    }
}


// Close dialog
long MessageBox::onCmdClicked(FXObject*,FXSelector sel,void*)
{
    getApp()->stopModal(this,BOX_CLICKED_YES+(FXSELID(sel)-ID_CLICKED_YES));
    hide();
    return 1;
}

// Launch a root Xfe (su mode)
long MessageBox::onCmdSu(FXObject*,FXSelector sel,void*)
{
    getApp()->stopModal(this,BOX_CLICKED_YES+(FXSELID(sel)-ID_CLICKED_YES));
    hide();
    
	// Wait cursor
	getApp()->beginWaitCursor();

	// Obtain preferred root mode
	FXbool use_sudo=getApp()->reg().readUnsignedEntry("OPTIONS","use_sudo",FALSE);

	// Use sudo or su to launch xfe as root
	FXString title, sucmd;
	if (use_sudo)
	{
		title = _("Enter the user password:");
		sucmd = SUDOCMD;
	}
	else
	{
		title = _("Enter the root password:");
		sucmd = SUCMD;
	}
	
	// Use appropriate background and foreground colors for Xvt
	char color[64];
	fxnamefromcolor(color,getApp()->getBackColor());
	FXString bg=" -bg ";
	bg = bg + color;
	fxnamefromcolor(color,getApp()->getForeColor());
	FXString fg=" -fg ";
	fg = fg + color + " ";
	
	// Command string
	FXString command = "xvt -title " + ::quote(title) + bg + fg + sucmd;

	// Execute su or sudo command in an internal Xvt terminal
	int status=runinxvt(command);
	
	// If error
	if (status<0)
	{
		MessageBox::error(getApp(),BOX_OK,_("Error"),_("An error has occurred!"));
		getApp()->endWaitCursor();
		return 0;
	}	

 	// Wait cursor
	getApp()->endWaitCursor();

    return 1;
}

// Close dialog with a cancel
long MessageBox::onCmdCancel(FXObject* sender,FXSelector,void* ptr)
{
    return MessageBox::onCmdClicked(sender,FXSEL(SEL_COMMAND,ID_CLICKED_CANCEL),ptr);
}

// Show a modal error message
unsigned int MessageBox::error(FXWindow* owner,unsigned int opts,const char* caption,const char* message,...)
{
    va_list arguments;
    va_start(arguments,message);
    MessageBox box(owner,caption,FXStringVFormat(message,arguments),errorbigicon,opts|DECOR_TITLE|DECOR_BORDER);
    va_end(arguments);
    return box.execute();
}


// Show a modal error message, in free floating window
unsigned int MessageBox::error(FXApp* app,unsigned int opts,const char* caption,const char* message,...)
{
    va_list arguments;
    va_start(arguments,message);
    MessageBox box(app,caption,FXStringVFormat(message,arguments),errorbigicon,opts|DECOR_TITLE|DECOR_BORDER);
    va_end(arguments);
    return box.execute();
}


// Show a modal warning message
unsigned int MessageBox::warning(FXWindow* owner,unsigned int opts,const char* caption,const char* message,...)
{
    va_list arguments;
    va_start(arguments,message);
    MessageBox box(owner,caption,FXStringVFormat(message,arguments),warningbigicon,opts|DECOR_TITLE|DECOR_BORDER);
    va_end(arguments);
    return box.execute();
}


// Show a modal warning message, in free floating window
unsigned int MessageBox::warning(FXApp* app,unsigned int opts,const char* caption,const char* message,...)
{
    va_list arguments;
    va_start(arguments,message);
    MessageBox box(app,caption,FXStringVFormat(message,arguments),warningbigicon,opts|DECOR_TITLE|DECOR_BORDER);
    va_end(arguments);
    return box.execute();
}


// Show a modal question dialog
unsigned int MessageBox::question(FXWindow* owner,unsigned int opts,const char* caption,const char* message,...)
{
    va_list arguments;
    va_start(arguments,message);
    MessageBox box(owner,caption,FXStringVFormat(message,arguments),questionbigicon,opts|DECOR_TITLE|DECOR_BORDER);
    va_end(arguments);
    return box.execute();
}


// Show a modal question dialog, in free floating window
unsigned int MessageBox::question(FXApp* app,unsigned int opts,const char* caption,const char* message,...)
{
    va_list arguments;
    va_start(arguments,message);
    MessageBox box(app,caption,FXStringVFormat(message,arguments),questionbigicon,opts|DECOR_TITLE|DECOR_BORDER);
    va_end(arguments);
    return box.execute();
}


// Show a modal information dialog
unsigned int MessageBox::information(FXWindow* owner,unsigned int opts,const char* caption,const char* message,...)
{
    va_list arguments;
    va_start(arguments,message);
    MessageBox box(owner,caption,FXStringVFormat(message,arguments),infobigicon,opts|DECOR_TITLE|DECOR_BORDER);
    va_end(arguments);
    return box.execute();
}


// Show a modal information dialog, in free floating window
unsigned int MessageBox::information(FXApp* app,unsigned int opts,const char* caption,const char* message,...)
{
    va_list arguments;
    va_start(arguments,message);
    MessageBox box(app,caption,FXStringVFormat(message,arguments),infobigicon,opts|DECOR_TITLE|DECOR_BORDER);
    va_end(arguments);
    return box.execute();
}
