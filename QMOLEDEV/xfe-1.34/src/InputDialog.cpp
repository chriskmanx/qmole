// Simple input dialog (without history)

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>

#include "xfeutils.h"
#include "InputDialog.h"



FXDEFMAP(InputDialog) InputDialogMap[]=
{
	FXMAPFUNC(SEL_KEYPRESS,0,InputDialog::onCmdKeyPress),
};

// Object implementation
FXIMPLEMENT(InputDialog,DialogBox,InputDialogMap,ARRAYNUMBER(InputDialogMap))

// Construct a dialog box
InputDialog::InputDialog(FXWindow *win,FXString inp,FXString message,FXString title,FXString label,FXIcon *icon, FXbool option, FXString optiontext):
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

	// Optional check box
	checkbutton=new FXHorizontalFrame(this,JUSTIFY_RIGHT|LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0,10,10,0,0);
    
	if (option)
        new FXCheckButton(checkbutton,optiontext,this,ID_TOGGLE_OPTION);

    // Vertical frame
	FXVerticalFrame *contents=new FXVerticalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

	// Icon and message line
	FXMatrix *matrix = new FXMatrix(contents,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	new FXLabel(matrix,"",icon,LAYOUT_LEFT);    
	new FXLabel(matrix,message.text(),NULL,JUSTIFY_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
	
	// Label and input field
	new FXLabel(matrix,label,NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
	input = new FXTextField(matrix,40,0,0,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	input->setText(inp);
}

void InputDialog::create()
{
    DialogBox::create();
    input->setFocus();
}


long InputDialog::onCmdKeyPress(FXObject* sender,FXSelector sel,void* ptr)
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
		return 1;
    }
	return 0;
}

