// Dialog to let the user enter a key binding

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>

#include "xfeutils.h"
#include "KeybindingsDialog.h"



FXDEFMAP(KeybindingsDialog) KeybindingsDialogMap[]=
{
	FXMAPFUNC(SEL_KEYPRESS,0,KeybindingsDialog::onCmdKeyPress),
};

// Object implementation
FXIMPLEMENT(KeybindingsDialog,DialogBox,KeybindingsDialogMap,ARRAYNUMBER(KeybindingsDialogMap))

// Construct a dialog box
KeybindingsDialog::KeybindingsDialog(FXWindow *win, FXString input, FXString message, FXString title, FXIcon *icon):
        DialogBox(win,title,DECOR_TITLE|DECOR_BORDER|DECOR_STRETCHABLE)
{
    // Buttons
    FXHorizontalFrame *buttons=new FXHorizontalFrame(this,PACK_UNIFORM_WIDTH|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0,10,10,5,5);
    
	// Accept
    new FXButton(buttons,_("&Accept"),NULL,this,ID_ACCEPT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,20,20);
    
	// Cancel
    new FXButton(buttons,_("&Cancel"),NULL,this,ID_CANCEL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT,0,0,0,0,20,20);
    
	// Separator
    new FXHorizontalSeparator(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|SEPARATOR_GROOVE);

    // Vertical frame
	FXVerticalFrame *contents=new FXVerticalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

	// Icon and message line
	FXMatrix *matrix = new FXMatrix(contents,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	new FXLabel(matrix,"",icon,LAYOUT_LEFT);    
	new FXLabel(matrix,message.text(),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);

	// Label
	keylabel=new FXLabel(contents,input.text(),NULL,LAYOUT_CENTER_X);    
}


void KeybindingsDialog::create()
{
    DialogBox::create();
}


// A key was pressed
long KeybindingsDialog::onCmdKeyPress(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;

	switch(event->code)
    {
    case KEY_Escape:
        handle(this,FXSEL(SEL_COMMAND,ID_CANCEL),NULL);
		return 1;
    case KEY_KP_Enter:
    case KEY_Return:
        handle(this,FXSEL(SEL_COMMAND,ID_ACCEPT),NULL);
		return 1;
    default:
        FXTopWindow::onKeyPress(sender,sel,ptr);
		
		// Get and display key binding string from user input
		FXString key=getKeybinding(event);
		keylabel->setText(key);
		return 1;
    }
	return 0;
}

