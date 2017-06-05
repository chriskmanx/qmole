// Input dialog with file browse icon

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "FileDialog.h"
#include "TextLabel.h"
#include "BrowseInputDialog.h"

// Used to limit the dialog size when long file names are used
#define MAX_MESSAGE_LENGTH 128

extern FXString homedir;


FXDEFMAP(BrowseInputDialog) BrowseInputDialogMap[]=
{
	FXMAPFUNC(SEL_KEYPRESS,0,BrowseInputDialog::onCmdKeyPress),
	FXMAPFUNC(SEL_COMMAND,BrowseInputDialog::ID_BROWSE_PATH,BrowseInputDialog::onCmdBrowsePath),
};

// Object implementation
FXIMPLEMENT(BrowseInputDialog,DialogBox,BrowseInputDialogMap,ARRAYNUMBER(BrowseInputDialogMap))

// Construct a dialog box
BrowseInputDialog::BrowseInputDialog(FXWindow *win,FXString inp,FXString message,FXString title,FXString label,FXIcon *ic, unsigned int browse, FXbool option, FXString optiontext):
        DialogBox(win,title,DECOR_TITLE|DECOR_BORDER|DECOR_STRETCHABLE)
{
    // Browse type flag
	browsetype=browse;
	
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

	// Icon and text label
	// Note : we display the message in a TextLabel. This allows to copy/paste the file name to the input text field
	FXMatrix *matrix1 = new FXMatrix(contents,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	iconlabel = new FXLabel(matrix1,"",ic,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW);
	msg = new TextLabel(matrix1,30,0,0,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW|FRAME_NONE);
	msg->setText(message);
	msg->setBackColor(getApp()->getBaseColor());

	// Label and input field
	FXMatrix *matrix2 = new FXMatrix(contents,3,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	new FXLabel(matrix2,label,NULL,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW);
	input = new FXTextField(matrix2,40,0,0,LAYOUT_CENTER_Y|LAYOUT_CENTER_X|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	input->setText(inp);
	new FXButton(matrix2,_("\tSelect destination..."),filedialogicon,this,ID_BROWSE_PATH,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
	if (!isUtf8(message.text(),message.length()))
		new FXLabel(contents,_("=> Warning: file name is not UTF-8 encoded!"),NULL,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW);
	
	// Initial directory for browsing
	initialdir=homedir;
}


void BrowseInputDialog::create()
{
    DialogBox::create();
    input->setFocus();
}


BrowseInputDialog::~BrowseInputDialog()
{
	delete input;
	delete msg;
	delete iconlabel;
}

long BrowseInputDialog::onCmdKeyPress(FXObject* sender,FXSelector sel,void* ptr)
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


long BrowseInputDialog::onCmdBrowsePath(FXObject* o,FXSelector s,void* p)
{	
	FXString title;
	if (browsetype==BROWSE_INPUT_FOLDER)
		title=_("Select a destination folder");
	else if (browsetype==BROWSE_INPUT_FILE)
		title=_("Select a file");
	else
		title=_("Select a file or a destination folder");
			
	// File dialog
	FileDialog browse(this,title);	
		
	const char *patterns[]=
		{
			_("All Files"),          "*",	NULL
		};

	browse.setDirectory(initialdir);
	browse.setPatternList(patterns);
	
	// Browse files in directory or mixed mode depending on the flag
	if (browsetype==BROWSE_INPUT_FOLDER)
		browse.setSelectMode(SELECT_FILE_DIRECTORY);
	else if (browsetype==BROWSE_INPUT_FILE)
		browse.setSelectMode(SELECT_FILE_EXISTING);
	else
		browse.setSelectMode(SELECT_FILE_MIXED);
		
	if(browse.execute())
	{
		FXString path=browse.getFilename();
		input->setText(path);
	}
		
	return 1;
}


// Adjust message size
void BrowseInputDialog::setMessage(FXString message)
{
	// Compute the equivalent size in number of columns of '8' of the message string,
	// taking into account the real size of the font characters
	FXFont *font=getApp()->getNormalFont();
	int nbcols = (int)ceil ( (double)font->getTextWidth(message) / (double)font->getCharWidth('8') );

	// Tricks to adjust the dialog width to the real text size
	this->setWidth(1);
	if (message.length() > MAX_MESSAGE_LENGTH)
		msg->setNumColumns(MAX_MESSAGE_LENGTH);
	else
		msg->setNumColumns(nbcols);
	msg->setText(message);
}


// Set initial directory
void BrowseInputDialog::setDirectory(const FXString& path)
{
	initialdir=path;
}


FXString BrowseInputDialog::getText()
{
	return input->getText();
}


void BrowseInputDialog::setText(const FXString& text)
{
	input->setText(text);
}

// Change dialog icon
void BrowseInputDialog::setIcon(FXIcon* icon)
{
	iconlabel->setIcon(icon);
}

void BrowseInputDialog::SelectAll()
{
	input->setSelection(0,(input->getText()).length());
}


void BrowseInputDialog::CursorEnd()
{
	input->onCmdCursorEnd(0,0,0);
}


FXbool BrowseInputDialog::setSelection(FXint pos, FXint len)
{
	input->setSelection(pos,len);
}
