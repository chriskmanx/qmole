// This is adapted from 'adie', a text editor found
// in the FOX library and written by Jeroen van der Zijp.

#include "config.h"
#include "i18n.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>
#include <FX88591Codec.h>
#include <FXUTF16Codec.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "startupnotification.h"
#include "FileDialog.h"
#include "FontDialog.h"
#include "MessageBox.h"
#include "InputDialog.h"
#include "WriteWindow.h"
#include "XFileWrite.h"


extern FXbool allowPopupScroll;
FXbool save_win_pos;


FXIMPLEMENT_ABSTRACT(FXTextCommand,FXCommand,NULL,0)

// Return size of record plus any data kept here
unsigned int FXTextCommand::size() const
{
    return sizeof(FXTextCommand)+ndel;
}


FXIMPLEMENT_ABSTRACT(FXTextInsert,FXTextCommand,NULL,0)

// Insert command
FXTextInsert::FXTextInsert(FXText* txt,int p,int ni,const char* ins):FXTextCommand(txt,p,0,ni)
{
    FXMALLOC(&buffer,char,ni);
    memcpy(buffer,ins,ni);
}


// Undo an insert removes the inserted text
void FXTextInsert::undo()
{
    text->removeText(pos,nins,TRUE);
    text->setCursorPos(pos);
    text->makePositionVisible(pos);
}


// Redo an insert inserts the same old text again
void FXTextInsert::redo()
{
    text->insertText(pos,buffer,nins,TRUE);
    text->setCursorPos(pos+nins);
    text->makePositionVisible(pos+nins);
}


FXIMPLEMENT_ABSTRACT(FXTextDelete,FXTextCommand,NULL,0)

// Delete command
FXTextDelete::FXTextDelete(FXText* txt,int p,int nd,const char* del):FXTextCommand(txt,p,nd,0)
{
    FXMALLOC(&buffer,char,nd);
    memcpy(buffer,del,nd);
}


// Undo a delete reinserts the old text
void FXTextDelete::undo()
{
    text->insertText(pos,buffer,ndel,TRUE);
    text->setCursorPos(pos+ndel);
    text->makePositionVisible(pos+ndel);
}


// Redo a delete removes it again
void FXTextDelete::redo()
{
    text->removeText(pos,ndel,TRUE);
    text->setCursorPos(pos);
    text->makePositionVisible(pos);
}


FXIMPLEMENT_ABSTRACT(FXTextReplace,FXTextCommand,NULL,0)

// Replace command
FXTextReplace::FXTextReplace(FXText* txt,int p,int nd,int ni,const char* del,const char* ins):FXTextCommand(txt,p,nd,ni)
{
    FXMALLOC(&buffer,char,nd+ni);
    memcpy(buffer,del,nd);
    memcpy(buffer+nd,ins,ni);
}


// Undo a replace reinserts the old text
void FXTextReplace::undo()
{
    text->replaceText(pos,nins,buffer,ndel,TRUE);
    text->setCursorPos(pos+ndel);
    text->makePositionVisible(pos+ndel);
}


// Redo a replace reinserts the new text
void FXTextReplace::redo()
{
    text->replaceText(pos,ndel,buffer+ndel,nins,TRUE);
    text->setCursorPos(pos+nins);
    text->makePositionVisible(pos+nins);
}



// Preferences class

// Map
FXDEFMAP(Preferences) PreferencesMap[]=
{
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_ACCEPT,Preferences::onCmdAccept),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_CANCEL,Preferences::onCmdCancel),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_BACK,Preferences::onCmdTextBackColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_BACK,Preferences::onCmdTextBackColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_BACK,Preferences::onUpdTextBackColor),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_FORE,Preferences::onCmdTextForeColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_FORE,Preferences::onCmdTextForeColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_FORE,Preferences::onUpdTextForeColor),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_SELBACK,Preferences::onCmdTextSelBackColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_SELBACK,Preferences::onCmdTextSelBackColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_SELBACK,Preferences::onUpdTextSelBackColor),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_SELFORE,Preferences::onCmdTextSelForeColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_SELFORE,Preferences::onCmdTextSelForeColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_SELFORE,Preferences::onUpdTextSelForeColor),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_HILITEBACK,Preferences::onCmdTextHiliteBackColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_HILITEBACK,Preferences::onCmdTextHiliteBackColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_HILITEBACK,Preferences::onUpdTextHiliteBackColor),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_HILITEFORE,Preferences::onCmdTextHiliteForeColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_HILITEFORE,Preferences::onCmdTextHiliteForeColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_HILITEFORE,Preferences::onUpdTextHiliteForeColor),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_CURSOR,Preferences::onCmdTextCursorColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_CURSOR,Preferences::onCmdTextCursorColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_CURSOR,Preferences::onUpdTextCursorColor),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_NUMBACK,Preferences::onCmdTextBarColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_NUMBACK,Preferences::onCmdTextBarColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_NUMBACK,Preferences::onUpdTextBarColor),
	FXMAPFUNC(SEL_COMMAND,Preferences::ID_TEXT_NUMFORE,Preferences::onCmdTextNumberColor),
	FXMAPFUNC(SEL_CHANGED,Preferences::ID_TEXT_NUMFORE,Preferences::onCmdTextNumberColor),
	FXMAPFUNC(SEL_UPDATE,Preferences::ID_TEXT_NUMFORE,Preferences::onUpdTextNumberColor),
};

// Object implementation
FXIMPLEMENT(Preferences,DialogBox,PreferencesMap,ARRAYNUMBER(PreferencesMap))

// Construct
Preferences::Preferences(WriteWindow *owner):DialogBox(owner,_("XFileWrite Preferences"),DECOR_TITLE|DECOR_BORDER)
{
	// Get the editor text widget from the owner
	editwin=owner;
	editor=owner->getEditor();
	
    // Set title
    setTitle(_("XFileWrite Preferences"));

    // Buttons
    FXHorizontalFrame *buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0,10,10,5,5);

    // Contents
    FXHorizontalFrame *contents=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH);

    // Accept
    FXButton *ok = new FXButton(buttons,_("&Accept"),NULL,this,Preferences::ID_ACCEPT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    ok->addHotKey(KEY_Return);

    // Cancel
    new FXButton(buttons,_("&Cancel"),NULL,this,Preferences::ID_CANCEL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    // Switcher
    FXTabBook *tabbook = new FXTabBook(contents,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_RIGHT);

    // First tab - Editor
    new FXTabItem(tabbook,_("&Editor"),NULL);
    FXVerticalFrame *editor=new FXVerticalFrame(tabbook,FRAME_RAISED);
    FXGroupBox *group=new FXGroupBox(editor,_("Text"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
    FXMatrix *matrix = new FXMatrix(group,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    
	new FXLabel(matrix,_("Wrap margin:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    wrapmargin = new FXTextField(matrix,10,NULL,0,TEXTFIELD_INTEGER|JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 2,2,1,1);
    int wrapcols=getApp()->reg().readIntEntry("OPTIONS","wrapcols",80);
	wrapmargin->setText(FXStringVal(wrapcols));
	
	new FXLabel(matrix,_("Tabulation size:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    tabsize = new FXTextField(matrix,10,NULL,0,TEXTFIELD_INTEGER|JUSTIFY_RIGHT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_CENTER_Y|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 2,2,1,1);
    int tabcols=getApp()->reg().readIntEntry("OPTIONS","tabcols",80);
	tabsize->setText(FXStringVal(tabcols));

    new FXLabel(matrix,_("Strip carriage returns:")+(FXString)"                ",NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    stripcr = new FXCheckButton(matrix,FXString::null,NULL,0,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW,0,0,0,0, 0,0,0,0);
    int stripreturn=getApp()->reg().readIntEntry("OPTIONS","stripreturn",FALSE);
	stripcr->setCheck(stripreturn);

    // Second tab - Colors
    new FXTabItem(tabbook,_("&Colors"),NULL);
    FXVerticalFrame *colors=new FXVerticalFrame(tabbook,FRAME_RAISED);
    group=new FXGroupBox(colors,_("Text"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
    FXMatrix *matrix1 = new FXMatrix(group,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    group=new FXGroupBox(colors,_("Lines"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
    FXMatrix *matrix2 = new FXMatrix(group,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);

    new FXLabel(matrix1,_("Background:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix1,FXRGB(0,0,0),this,Preferences::ID_TEXT_BACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

    new FXLabel(matrix1,_("Text:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix1,FXRGB(0,0,0),this,Preferences::ID_TEXT_FORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

    new FXLabel(matrix1,_("Selected text background:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix1,FXRGB(0,0,0),this,Preferences::ID_TEXT_SELBACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

    new FXLabel(matrix1,_("Selected text:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix1,FXRGB(0,0,0),this,Preferences::ID_TEXT_SELFORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

    new FXLabel(matrix1,_("Highlighted text background:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix1,FXRGB(0,0,0),this,Preferences::ID_TEXT_HILITEBACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

    new FXLabel(matrix1,_("Highlighted text:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix1,FXRGB(0,0,0),this,Preferences::ID_TEXT_HILITEFORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

    new FXLabel(matrix1,_("Cursor:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix1,FXRGB(0,0,0),this,Preferences::ID_TEXT_CURSOR,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

    new FXLabel(matrix2,_("Line numbers background:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix2,FXRGB(0,0,0),this,Preferences::ID_TEXT_NUMBACK,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

    new FXLabel(matrix2,_("Line numbers foreground:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXColorWell(matrix2,FXRGB(0,0,0),this,Preferences::ID_TEXT_NUMFORE,FRAME_SUNKEN|FRAME_THICK|LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT|LAYOUT_FILL_ROW,0,0,40,24);

}

long Preferences::onCmdAccept(FXObject* o,FXSelector s,void* p)
{
	// Update preferences to their new values
	
	editor->setWrapColumns(FXIntVal(wrapmargin->getText()));	
	getApp()->reg().writeIntEntry("OPTIONS","wrapcols",FXIntVal(wrapmargin->getText()));
	
	editor->setTabColumns(FXIntVal(tabsize->getText()));	
	getApp()->reg().writeIntEntry("OPTIONS","tabcols",FXIntVal(tabsize->getText()));

	editor->setTabColumns(FXIntVal(tabsize->getText()));	
	getApp()->reg().writeIntEntry("OPTIONS","tabcols",FXIntVal(tabsize->getText()));

    editwin->setStripcr(stripcr->getCheck());
	getApp()->reg().writeIntEntry("OPTIONS","stripreturn",stripcr->getCheck());


	// Finally, update the registry
	getApp()->reg().write();
	
    DialogBox::onCmdAccept(o,s,p);
	
	return 1;
}


long Preferences::onCmdCancel(FXObject* o,FXSelector s,void* p)
{
	// Reset preferences to their previous values
	
	// First tab - Editor
	wrapmargin->setText(wrapmargin_prev);
	tabsize->setText(tabsize_prev);
	stripcr->setCheck(stripcr_prev);

	// Second tab - Colors
	editor->setTextColor(textcolor_prev);
	editor->setBackColor(backcolor_prev);	
	editor->setSelTextColor(seltextcolor_prev);
	editor->setSelBackColor(selbackcolor_prev);
	editor->setHiliteTextColor(hilitetextcolor_prev);	
	editor->setHiliteBackColor(hilitebackcolor_prev);	
	editor->setCursorColor(cursorcolor_prev);
	editor->setBarColor(barcolor_prev);
	editor->setNumberColor(numbercolor_prev);	

    DialogBox::onCmdCancel(o,s,p);
    return 1;
}


// Execute dialog box modally
unsigned int Preferences::execute(unsigned int placement)
{
	// Save current preferences to restore them if cancel is pressed
	
	// First tab - Editor
	wrapmargin_prev=wrapmargin->getText();
	tabsize_prev=tabsize->getText();
	stripcr_prev=stripcr->getCheck();
	
	// Second tab - Colors
	textcolor_prev=editor->getTextColor();	
	backcolor_prev=editor->getBackColor();	
	seltextcolor_prev=editor->getSelTextColor();
	selbackcolor_prev=editor->getSelBackColor();
	hilitetextcolor_prev=editor->getHiliteTextColor();	
	hilitebackcolor_prev=editor->getHiliteBackColor();	
	cursorcolor_prev=editor->getCursorColor();
	barcolor_prev=editor->getBarColor();
	numbercolor_prev=editor->getNumberColor();	
	
	// Display dialog
	create();
    show(placement);
	getApp()->refresh();
    return getApp()->runModalFor(this);
}


// Change text color
long Preferences::onCmdTextForeColor(FXObject*,FXSelector,void* ptr)
{
    editor->setTextColor((FXColor)(FXuval)ptr);
    return 1;
}

// Update text color
long Preferences::onUpdTextForeColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getTextColor();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    return 1;
}


// Change text background color
long Preferences::onCmdTextBackColor(FXObject*,FXSelector,void* ptr)
{
    editor->setBackColor((FXColor)(FXuval)ptr);
    return 1;
}

// Update background color
long Preferences::onUpdTextBackColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getBackColor();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    return 1;
}


// Change selected text foreground color
long Preferences::onCmdTextSelForeColor(FXObject*,FXSelector,void* ptr)
{
    editor->setSelTextColor((FXColor)(FXuval)ptr);
    return 1;
}


// Update selected text foregoround color
long Preferences::onUpdTextSelForeColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getSelTextColor();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    return 1;
}


// Change selected text background color
long Preferences::onCmdTextSelBackColor(FXObject*,FXSelector,void* ptr)
{
    editor->setSelBackColor((FXColor)(FXuval)ptr);
    return 1;
}

// Update selected text background color
long Preferences::onUpdTextSelBackColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getSelBackColor();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    return 1;
}


// Change hilight text color
long Preferences::onCmdTextHiliteForeColor(FXObject*,FXSelector,void* ptr)
{
    editor->setHiliteTextColor((FXColor)(FXuval)ptr);
    return 1;
}

// Update hilight text color
long Preferences::onUpdTextHiliteForeColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getHiliteTextColor();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    return 1;
}


// Change hilight text background color
long Preferences::onCmdTextHiliteBackColor(FXObject*,FXSelector,void* ptr)
{
    editor->setHiliteBackColor((FXColor)(FXuval)ptr);
    return 1;
}

// Update hilight text background color
long Preferences::onUpdTextHiliteBackColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getHiliteBackColor();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    return 1;
}


// Change cursor color
long Preferences::onCmdTextCursorColor(FXObject*,FXSelector,void* ptr)
{
    editor->setCursorColor((FXColor)(FXuval)ptr);
    return 1;
}

// Update cursor color
long Preferences::onUpdTextCursorColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getCursorColor();
    sender->handle(sender,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&color);
    return 1;
}


// Change line numbers background color
long Preferences::onCmdTextBarColor(FXObject*,FXSelector,void* ptr)
{
    editor->setBarColor((FXColor)(FXuval)ptr);
    return 1;
}


// Update line numbers background color
long Preferences::onUpdTextBarColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getBarColor();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    return 1;
}


// Change line numbers color
long Preferences::onCmdTextNumberColor(FXObject*,FXSelector,void* ptr)
{
    editor->setNumberColor((FXColor)(FXuval)ptr);
    return 1;
}


// Update line numbers color
long Preferences::onUpdTextNumberColor(FXObject* sender,FXSelector,void*)
{
    FXColor color=editor->getNumberColor();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&color);
    return 1;
}




// WriteWindow class


// Map
FXDEFMAP(WriteWindow) WriteWindowMap[]=
{
	FXMAPFUNC(SEL_UPDATE,0,WriteWindow::onUpdateTitle),
	FXMAPFUNC(SEL_FOCUSIN,0,WriteWindow::onFocusIn),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_ABOUT,WriteWindow::onCmdAbout),
	FXMAPFUNC(SEL_SIGNAL,WriteWindow::ID_HARVEST,WriteWindow::onSigHarvest),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_NEW,WriteWindow::onCmdNew),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_OPEN,WriteWindow::onCmdOpen),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_OPEN_RECENT,WriteWindow::onCmdOpenRecent),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_SAVE,WriteWindow::onCmdSave),
	FXMAPFUNC(SEL_UPDATE,WriteWindow::ID_SAVE,WriteWindow::onUpdSave),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_SAVEAS,WriteWindow::onCmdSaveAs),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_FONT,WriteWindow::onCmdFont),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_PRINT,WriteWindow::onCmdPrint),
	FXMAPFUNC(SEL_UPDATE,WriteWindow::ID_TOGGLE_WRAP,WriteWindow::onUpdWrap),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_TOGGLE_WRAP,WriteWindow::onCmdWrap),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_TOGGLE_LINES_NUM,WriteWindow::onCmdLinesNum),
	FXMAPFUNC(SEL_UPDATE,WriteWindow::ID_TOGGLE_LINES_NUM,WriteWindow::onUpdLinesNum),
	FXMAPFUNC(SEL_INSERTED,WriteWindow::ID_TEXT,WriteWindow::onTextInserted),
	FXMAPFUNC(SEL_REPLACED,WriteWindow::ID_TEXT,WriteWindow::onTextReplaced),
	FXMAPFUNC(SEL_DELETED,WriteWindow::ID_TEXT,WriteWindow::onTextDeleted),
	FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,WriteWindow::ID_TEXT,WriteWindow::onTextRightMouse),
	FXMAPFUNC(SEL_DND_MOTION,WriteWindow::ID_TEXT,WriteWindow::onEditDNDMotion),
	FXMAPFUNC(SEL_DND_DROP,WriteWindow::ID_TEXT,WriteWindow::onEditDNDDrop),
	FXMAPFUNC(SEL_UPDATE,WriteWindow::ID_OVERSTRIKE,WriteWindow::onUpdOverstrike),
	FXMAPFUNC(SEL_UPDATE,WriteWindow::ID_NUM_ROWS,WriteWindow::onUpdNumRows),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_PREFERENCES,WriteWindow::onCmdMorePrefs),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_SEARCH,WriteWindow::onCmdSearch),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_REPLACE,WriteWindow::onCmdReplace),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_SEARCH_FORW_SEL,WriteWindow::onCmdSearchSel),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_SEARCH_BACK_SEL,WriteWindow::onCmdSearchSel),
	FXMAPFUNC(SEL_COMMAND,WriteWindow::ID_GOTO_LINE,WriteWindow::onCmdGotoLine),
	FXMAPFUNCS(SEL_UPDATE, WriteWindow::ID_WINDOW_1,WriteWindow::ID_WINDOW_50,WriteWindow::onUpdWindow),
	FXMAPFUNCS(SEL_COMMAND,WriteWindow::ID_WINDOW_1,WriteWindow::ID_WINDOW_50,WriteWindow::onCmdWindow),
};


// Object implementation
FXIMPLEMENT(WriteWindow,FXMainWindow,WriteWindowMap,ARRAYNUMBER(WriteWindowMap))

// Make some windows
WriteWindow::WriteWindow(XFileWrite* a,const FXString& file):FXMainWindow(a,"XFileWrite",NULL,NULL,DECOR_ALL),mrufiles(a)
{
    FXHotKey hotkey;
	FXString key;

    // Add to list of windows
    getApp()->windowlist.append(this);

    // Default font
    font=NULL;

    // Application icons
    setIcon(xfwicon);

    // Status bar
    statusbar=new FXStatusBar(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|STATUSBAR_WITH_DRAGCORNER|FRAME_RAISED);

    // Make menu bar
    menubar=new FXMenuBar(this,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);

    // Sites where to dock
    FXDockSite* topdock=new FXDockSite(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
    new FXDockSite(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
    new FXDockSite(this,LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y);
    new FXDockSite(this,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y);

    // Toolbar
    dragshell=new FXToolBarShell(this,FRAME_RAISED);
    toolbar=new FXToolBar(topdock,dragshell,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED);
    new FXToolBarGrip(toolbar,toolbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

    // File menu
    filemenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&File"),NULL,filemenu);

    // Edit Menu
    editmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Edit"),NULL,editmenu);

    // Search Menu
    searchmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Search"),NULL,searchmenu);

    // Preferences Menu
    prefsmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Preferences"),NULL,prefsmenu);

    // View menu
    viewmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&View"),NULL,viewmenu);

    // Window menu
    windowmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Window"),NULL,windowmenu);

    // Help menu
    helpmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Help"),NULL,helpmenu,LAYOUT_LEFT);

    // Make editor window
    FXHorizontalFrame *textbox=new FXHorizontalFrame(this,FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0);
    editor=new FXText(textbox,this,ID_TEXT,LAYOUT_FILL_X|LAYOUT_FILL_Y|TEXT_SHOWACTIVE);
    editor->setHiliteMatchTime(2000);
    editor->setTextStyle(editor->getTextStyle()|TEXT_FIXEDWRAP);
    editor->setTextStyle(editor->getTextStyle()&~TEXT_NO_TABS);
    editor->setScrollStyle(editor->getScrollStyle()&~SCROLLERS_DONT_TRACK);
    editor->setTextStyle(editor->getTextStyle()&~TEXT_SHOWACTIVE);
    editor->setMarginLeft(10);

    // Show insert mode in status bar
    FXLabel* overstrike=new FXLabel(statusbar,FXString::null,NULL,FRAME_SUNKEN|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
    overstrike->setTarget(this);
    overstrike->setSelector(ID_OVERSTRIKE);

    // Show size of text in status bar
    FXTextField* numchars=new FXTextField(statusbar,7,this,ID_NUM_ROWS,TEXTFIELD_READONLY|FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
    numchars->setBackColor(statusbar->getBackColor());

    // Caption before number
    new FXLabel(statusbar,_("  Lines:"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);

    // Show column number in status bar
    FXTextField* columnno=new FXTextField(statusbar,7,editor,FXText::ID_CURSOR_COLUMN,FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
    columnno->setBackColor(statusbar->getBackColor());

    // Caption before number
    new FXLabel(statusbar,_("  Col:"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);

    // Show line number in status bar
    FXTextField* rowno=new FXTextField(statusbar,7,editor,FXText::ID_CURSOR_ROW,FRAME_SUNKEN|JUSTIFY_RIGHT|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,2,2,1,1);
    rowno->setBackColor(statusbar->getBackColor());

    // Caption before number
    new FXLabel(statusbar,_("  Line:"),NULL,LAYOUT_RIGHT|LAYOUT_CENTER_Y);

    // Toolbar buttons: File manipulation
	key=getApp()->reg().readStringEntry("KEYBINDINGS","new","Ctrl-N");
    new FXButton(toolbar,TAB+_("New")+PARS(key)+TAB+_("Create new document.")+PARS(key),newfileicon,this,ID_NEW,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
    new FXButton(toolbar,TAB+_("Open")+PARS(key)+TAB+_("Open document file.")+PARS(key),fileopenicon,this,ID_OPEN,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","save","Ctrl-S");
    new FXButton(toolbar,TAB+_("Save")+PARS(key)+TAB+_("Save document.")+PARS(key),savefileicon,this,ID_SAVE,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","close","Ctrl-W");
    new FXButton(toolbar,TAB+_("Close")+PARS(key)+TAB+_("Close document file.")+PARS(key),closefileicon,this,ID_CLOSE,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // Spacer
    new FXSeparator(toolbar,SEPARATOR_GROOVE);

    // Toolbar buttons: Print and Quit
	key=getApp()->reg().readStringEntry("KEYBINDINGS","print","Ctrl-P");
    new FXButton(toolbar,TAB+_("Print")+PARS(key)+TAB+_("Print document.")+PARS(key),printicon,this,ID_PRINT,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);
 
	key=getApp()->reg().readStringEntry("KEYBINDINGS","quit","Ctrl-Q");
    new FXButton(toolbar,TAB+_("Quit")+PARS(key)+TAB+_("Quit X File Write.")+PARS(key),quiticon,getApp(),XFileWrite::ID_CLOSEALL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // Spacer
    new FXSeparator(toolbar,SEPARATOR_GROOVE);

    // Toolbar buttons: Editing
	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy","Ctrl-C");
    new FXButton(toolbar,TAB+_("Copy")+PARS(key)+TAB+_("Copy selection to clipboard.")+PARS(key),copy_clpicon,editor,FXText::ID_COPY_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","cut","Ctrl-X");
    new FXButton(toolbar,TAB+_("Cut")+PARS(key)+TAB+_("Cut selection to clipboard.")+PARS(key),cut_clpicon,editor,FXText::ID_CUT_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","paste","Ctrl-V");
    new FXButton(toolbar,TAB+_("Paste")+PARS(key)+TAB+_("Paste clipboard.")+PARS(key),paste_clpicon,editor,FXText::ID_PASTE_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // Spacer
    new FXSeparator(toolbar,SEPARATOR_GROOVE);

    // Goto line
	key=getApp()->reg().readStringEntry("KEYBINDINGS","goto_line","Ctrl-L");
    new FXButton(toolbar,TAB+_("Goto line")+PARS(key)+TAB+_("Goto line number."),gotolineicon,this,WriteWindow::ID_GOTO_LINE,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // Spacer
    new FXSeparator(toolbar,SEPARATOR_GROOVE);

    // Undo/redo
	key=getApp()->reg().readStringEntry("KEYBINDINGS","undo","Ctrl-Z");
    new FXButton(toolbar,TAB+_("Undo")+PARS(key)+TAB+_("Undo last change.")+PARS(key),undoicon,&undolist,FXUndoList::ID_UNDO,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","redo","Ctrl-Y");
    new FXButton(toolbar,TAB+_("Redo")+PARS(key)+TAB+_("Redo last undo.")+PARS(key),redoicon,&undolist,FXUndoList::ID_REDO,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // Spacer
    new FXSeparator(toolbar,SEPARATOR_GROOVE);

    // Search
	key=getApp()->reg().readStringEntry("KEYBINDINGS","search","Ctrl-F");
    new FXButton(toolbar,TAB+_("Search")+PARS(key)+TAB+_("Search text.")+PARS(key),searchicon,this,WriteWindow::ID_SEARCH,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","search_prev","Shift-Ctrl-G");
    new FXButton(toolbar,TAB+_("Search selection backward")+PARS(key)+TAB+_("Search backward for selected text.")+PARS(key),searchprevicon,this,WriteWindow::ID_SEARCH_BACK_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","search_next","Ctrl-G");
    new FXButton(toolbar,TAB+_("Search selection forward")+PARS(key)+TAB+_("Search forward for selected text.")+PARS(key),searchnexticon,this,WriteWindow::ID_SEARCH_FORW_SEL,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED|LAYOUT_TOP|LAYOUT_LEFT);

    // Spacer
    new FXSeparator(toolbar,SEPARATOR_GROOVE);

    // Preferences
	key=getApp()->reg().readStringEntry("KEYBINDINGS","word_wrap","Ctrl-K");
    new FXToggleButton(toolbar,TAB+_("Word wrap on")+PARS(key)+TAB+_("Set word wrap on.")+PARS(key),TAB+_("Word wrap off")+PARS(key)+TAB+_("Set word wrap off.")+PARS(key),wrapofficon,wraponicon,this,ID_TOGGLE_WRAP,BUTTON_TOOLBAR|LAYOUT_LEFT|ICON_BEFORE_TEXT);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","line_numbers","Ctrl-T");
    new FXToggleButton(toolbar,TAB+_("Show line numbers")+PARS(key)+TAB+_("Show line numbers.")+PARS(key),TAB+_("Hide line numbers")+PARS(key)+TAB+_("Hide line numbers.")+PARS(key),hidenumbersicon,shownumbersicon,this,ID_TOGGLE_LINES_NUM,BUTTON_TOOLBAR|LAYOUT_LEFT|ICON_BEFORE_TEXT);


    // File Menu entries
	FXMenuCommand* mc = NULL;
	FXString text;

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new","Ctrl-N");
	text=_("&New...")+TABS(key)+_("Create new document.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,newfileicon,this,ID_NEW);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
	text=_("&Open...")+TABS(key)+_("Open document file.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,fileopenicon,this,ID_OPEN);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","save","Ctrl-S");
	text=_("&Save")+TABS(key)+_("Save changes to file.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,savefileicon,this,ID_SAVE);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuCommand(filemenu,_("Save &As...")+TAB2+_("Save document to another file."),saveasicon,this,ID_SAVEAS);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","close","Ctrl-W");
	text=_("&Close")+TABS(key)+_("Close document.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,closefileicon,this,ID_CLOSE);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(filemenu);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","print","Ctrl-P");
	text=_("&Print...")+TABS(key)+_("Print document.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,printicon,this,ID_PRINT);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    // Recent file menu; this automatically hides if there are no files
    FXMenuSeparator* sep1=new FXMenuSeparator(filemenu);
    sep1->setTarget(&mrufiles);
    sep1->setSelector(FXRecentFiles::ID_ANYFILES);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_1);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_2);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_3);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_4);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_5);
    new FXMenuCommand(filemenu,_("&Clear Recent Files"),NULL,&mrufiles,FXRecentFiles::ID_CLEAR);
    FXMenuSeparator* sep2=new FXMenuSeparator(filemenu);
    sep2->setTarget(&mrufiles);
    sep2->setSelector(FXRecentFiles::ID_ANYFILES);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","quit","Ctrl-Q");
	text=_("&Quit")+TABS(key)+_("Quit X File Write.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,quiticon,getApp(),XFileWrite::ID_CLOSEALL);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    // Edit Menu entries
	key=getApp()->reg().readStringEntry("KEYBINDINGS","undo","Ctrl-Z");
	text=_("&Undo")+TABS(key)+_("Undo last change.")+PARS(key);
    mc=new FXMenuCommand(editmenu,text,undoicon,&undolist,FXUndoList::ID_UNDO);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","redo","Ctrl-Y");
	text=_("&Redo")+TABS(key)+_("Redo last undo.")+PARS(key);
    mc=new FXMenuCommand(editmenu,text,redoicon,&undolist,FXUndoList::ID_REDO);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuCommand(editmenu,_("Revert to &saved")+TAB2+_("Revert to saved document."),reverticon,&undolist,FXUndoList::ID_REVERT);

    new FXMenuSeparator(editmenu);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy","Ctrl-C");
	text=_("&Copy")+TABS(key)+_("Copy selection to clipboard.")+PARS(key);
    mc=new FXMenuCommand(editmenu,text,copy_clpicon,editor,FXText::ID_COPY_SEL);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","cut","Ctrl-X");
	text=_("Cu&t")+TABS(key)+_("Cut selection to clipboard.")+PARS(key);
    mc=new FXMenuCommand(editmenu,text,cut_clpicon,editor,FXText::ID_CUT_SEL);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","paste","Ctrl-V");
	text=_("&Paste")+TABS(key)+_("Paste from clipboard.")+PARS(key);
    mc=new FXMenuCommand(editmenu,text,paste_clpicon,editor,FXText::ID_PASTE_SEL);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(editmenu);
	key=getApp()->reg().readStringEntry("KEYBINDINGS","lower_case","Ctrl-U");
	text=_("Lo&wer-case")+TABS(key)+_("Change to lower case.")+PARS(key);
    mc=new FXMenuCommand(editmenu,text,lowercaseicon,editor,FXText::ID_LOWER_CASE);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","upper_case","Shift-Ctrl-U");
	text=_("Upp&er-case")+TABS(key)+_("Change to upper case.")+PARS(key);
    mc=new FXMenuCommand(editmenu,text,uppercaseicon,editor,FXText::ID_UPPER_CASE);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","goto_line","Ctrl-L");
	text=_("&Goto line...")+TABS(key)+_("Goto line number.")+PARS(key);
    mc=new FXMenuCommand(editmenu,text,gotolineicon,this,WriteWindow::ID_GOTO_LINE);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    // Right mouse popup
    popupmenu=new FXMenuPane(this);
    new FXMenuCommand(popupmenu,_("&Undo"),undoicon,&undolist,FXUndoList::ID_UNDO);
    new FXMenuCommand(popupmenu,_("&Redo"),redoicon,&undolist,FXUndoList::ID_REDO);
    new FXMenuSeparator(popupmenu);
    new FXMenuCommand(popupmenu,_("&Copy"),copy_clpicon,editor,FXText::ID_COPY_SEL);
    new FXMenuCommand(popupmenu,_("Cu&t"),cut_clpicon,editor,FXText::ID_CUT_SEL);
    new FXMenuCommand(popupmenu,_("&Paste"),paste_clpicon,editor,FXText::ID_PASTE_SEL);
    new FXMenuCommand(popupmenu,_("Select &All"),NULL,editor,FXText::ID_SELECT_ALL);
    new FXMenuSeparator(popupmenu);

    // Search Menu entries
	key=getApp()->reg().readStringEntry("KEYBINDINGS","search","Ctrl-F");
	text=_("&Search...")+TABS(key)+_("Search for a string.")+PARS(key);
    mc=new FXMenuCommand(searchmenu,text,searchicon,this,WriteWindow::ID_SEARCH);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","replace","Ctrl-R");
	text=_("&Replace...")+TABS(key)+_("Search for a string and replace with another.")+PARS(key);
    mc=new FXMenuCommand(searchmenu,text,replaceicon,this,WriteWindow::ID_REPLACE);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","search_prev","Shift-Ctrl-G");
	text=_("Search sel. &backward")+TABS(key)+_("Search backward for selected text.")+PARS(key);
    mc=new FXMenuCommand(searchmenu,text,searchprevicon,this,WriteWindow::ID_SEARCH_BACK_SEL);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
    
	key=getApp()->reg().readStringEntry("KEYBINDINGS","search_next","Ctrl-G");
	text=_("Search sel. &forward")+TABS(key)+_("Search forward for selected text.")+PARS(key);
	mc=new FXMenuCommand(searchmenu,text,searchnexticon,this,WriteWindow::ID_SEARCH_FORW_SEL);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));


    // Preferences menu
	key=getApp()->reg().readStringEntry("KEYBINDINGS","word_wrap","Ctrl-K");
	text=_("&Word wrap")+TABS(key)+_("Toggle word wrap mode.")+PARS(key);
    mc=new FXMenuCheck(prefsmenu,text,this,ID_TOGGLE_WRAP);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","line_numbers","Ctrl-T");
	text=_("&Line numbers")+TABS(key)+_("Toggle line numbers mode.")+PARS(key);
    mc=new FXMenuCheck(prefsmenu,text,this,ID_TOGGLE_LINES_NUM);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuCheck(prefsmenu,_("&Overstrike")+TAB2+_("Toggle overstrike mode."),editor,FXText::ID_TOGGLE_OVERSTRIKE);
    new FXMenuSeparator(prefsmenu);
    new FXMenuCommand(prefsmenu,_("&Font...")+TAB2+_("Change text font."),fontsicon,this,ID_FONT);
    new FXMenuCommand(prefsmenu,_("&More preferences...")+TAB2+_("Change other options."),prefsicon,this,ID_PREFERENCES);

    // View Menu entries
    new FXMenuCheck(viewmenu,_("&Toolbar")+TAB2+_("Display toolbar."),toolbar,FXWindow::ID_TOGGLESHOWN);
    new FXMenuCheck(viewmenu,_("&Status line")+TAB2+_("Display status line."),statusbar,FXWindow::ID_TOGGLESHOWN);

    // Window menu
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_1);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_2);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_3);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_4);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_5);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_6);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_7);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_8);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_9);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_10);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_11);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_12);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_13);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_14);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_15);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_16);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_17);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_18);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_19);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_20);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_21);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_22);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_23);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_24);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_25);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_26);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_27);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_28);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_29);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_30);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_31);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_32);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_33);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_34);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_35);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_36);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_37);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_38);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_39);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_40);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_41);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_42);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_43);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_44);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_45);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_46);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_47);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_48);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_49);
    new FXMenuRadio(windowmenu,FXString::null,this,ID_WINDOW_50);

    // Help Menu entries
	key=getApp()->reg().readStringEntry("KEYBINDINGS","help","F1");
	text=_("&About X File Write...")+TABS(key)+_("About X File Write.")+PARS(key);
    mc=new FXMenuCommand(helpmenu,text,NULL,this,ID_ABOUT,0);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    // Dialogs
    printdialog=NULL;
    prefsdialog=NULL;
    searchdialog=NULL;
    replacedialog=NULL;

    // Recent files
    mrufiles.setTarget(this);
    mrufiles.setSelector(ID_OPEN_RECENT);

    // Initialize file name
    filename=file;
    filenameset=FALSE;
    filetime=0;

    // Initialize other stuff
    stripcr=FALSE;
    linesnum=FALSE;
    undolist.mark();

	// Initialize window position and size
	fromreg=TRUE;
	ww=0;
	hh=0;
	xx=0;
	yy=0;
}


// Create and show window
void WriteWindow::create()
{
    loadConfig();
    FXMainWindow::create();
    dragshell->create();
    filemenu->create();
    editmenu->create();
    searchmenu->create();
    prefsmenu->create();
    viewmenu->create();
    windowmenu->create();
    helpmenu->create();
    popupmenu->create();
    if(!urilistType)
        urilistType=getApp()->registerDragType(urilistTypeName);
    show(PLACEMENT_DEFAULT);
    editor->setFocus();

#ifdef STARTUP_NOTIFICATION
	startup_completed();
#endif
}


// Detach window
void WriteWindow::detach()
{
    FXMainWindow::detach();
    dragshell->detach();
    urilistType=0;
}


// Clean up
WriteWindow::~WriteWindow()
{
    getApp()->windowlist.remove(this);
    delete font;
    delete toolbar;
    delete menubar;
    delete dragshell;
    delete filemenu;
    delete editmenu;
    delete searchmenu;
    delete prefsmenu;
    delete viewmenu;
    delete windowmenu;
    delete helpmenu;
    delete popupmenu;
    delete editor;
    delete printdialog;
    delete prefsdialog;
    delete searchdialog;
    delete replacedialog;
}


// Is it modified
FXbool WriteWindow::isModified() const
{
    return !undolist.marked();
}


// Load file
FXbool WriteWindow::loadFile(const FXString& file)
{
    FXFile textfile(file,FXFile::Reading);
    int size,n,i,j,c;
    char *text;

    // Opened file?
    if(!textfile.isOpen())
    {
        MessageBox::error(this,BOX_OK,_("Error Loading File"),_("Unable to open file: %s"),file.text());
        return FALSE;
    }

    // Get file size
    size=textfile.size();

    // Make buffer to load file
    if(!FXMALLOC(&text,char,size))
    {
        MessageBox::error(this,BOX_OK,_("Error Loading File"),_("File is too big: %s (%d bytes)"),file.text(),size);
        return FALSE;
    }

    // Set wait cursor
    getApp()->beginWaitCursor();

    // Read the file
    n=textfile.readBlock(text,size);
    if(n<0)
    {
        FXFREE(&text);
        MessageBox::error(this,BOX_OK,_("Error Loading File"),_("Unable to read file: %s"),file.text());
        getApp()->endWaitCursor();
        return FALSE;
    }

    // Strip carriage returns
    if(stripcr)
    {
        for(i=j=0; j<n; j++)
        {
            c=text[j];
            if(c!='\r')
                text[i++]=c;
        }
        n=i;
    }

    // Set text
    editor->setText(text,n);
    FXFREE(&text);

    // Lines numbering
    if (linesnum)
    {
        unsigned int size=editor->getNumRows();
        unsigned int cols=(unsigned int)ceil(log10(size));
        editor->setBarColumns(cols);
    }
    else
        editor->setBarColumns(0);

    // Kill wait cursor
    getApp()->endWaitCursor();

    mrufiles.appendFile(file);
    filetime=FXStat::modified(file);
    filenameset=TRUE;
    filename=file;

    // Clear undo records
    undolist.clear();

    // Mark undo state as clean (saved)
    undolist.mark();

    return TRUE;
}




// Save file
FXbool WriteWindow::saveFile(const FXString& file)
{
    FXFile textfile(file,FXFile::Writing);
    int size,n;
    char *text;

    // Opened file?
    if(!textfile.isOpen())
    {
        MessageBox::error(this,BOX_OK,_("Error Saving File"),_("Unable to open file: %s"),file.text());
        return FALSE;
    }

    // Get size
    size=editor->getLength();

    // Alloc buffer
    if(!FXMALLOC(&text,char,size+1))
    {
        MessageBox::error(this,BOX_OK,_("Error Saving File"),_("File is too big: %s"),file.text());
        return FALSE;
    }

    // Set wait cursor
    getApp()->beginWaitCursor();

    // Get text from editor
    editor->getText(text,size);

    // Write the file
    n=textfile.writeBlock(text,size);

    // Ditch buffer
    FXFREE(&text);

    // Kill wait cursor
    getApp()->endWaitCursor();

    // Were we able to write it all?
    if(n!=size)
    {
        MessageBox::error(this,BOX_OK,_("Error Saving File"),_("File: %s truncated."),file.text());
        return FALSE;
    }

    mrufiles.appendFile(file);
    filetime=FXStat::modified(file);
    filenameset=TRUE;
    filename=file;
    undolist.mark();
    return TRUE;
}


// Generate unique name for a new window
FXString WriteWindow::unique() const
{
    FXString name=_("untitled");
    for(int i=1; i<2147483647; i++)
    {
        if(!findWindow(name))
        	break;
        name.format(_("untitled%d"),i);
    }
    return name;
}


// Find an as yet untitled, unedited window
WriteWindow *WriteWindow::findUnused() const
{
    for(int w=0; w<getApp()->windowlist.no(); w++)
    {
        if(!getApp()->windowlist[w]->isFilenameSet() && !getApp()->windowlist[w]->isModified())
            return getApp()->windowlist[w];
    }
    return NULL;
}


// Find window, if any, currently editing the given file
WriteWindow *WriteWindow::findWindow(const FXString& file) const
{
    for(int w=0; w<getApp()->windowlist.no(); w++)
    {
        if(getApp()->windowlist[w]->getFilename()==file)
            return getApp()->windowlist[w];
    }
    return NULL;
}


// Visit given line
void WriteWindow::visitLine(int line)
{
    int pos=editor->nextLine(0,line-1);
    editor->setCursorPos(pos);
    editor->setCenterLine(pos);
}


// Read configuration from registry
void WriteWindow::loadConfig()
{
    FXColor textback,textfore,textselback,textselfore,textcursor,texthilitefore,texthiliteback;
    FXColor textbar,textnumber;
    int wrapping,wrapcols,tabcols;
    int hidestatus,hidetoolbar,hilitematchtime;
    FXString fontspec;

    // Text colors
    textback=getApp()->reg().readColorEntry("OPTIONS","textbackground",editor->getBackColor());
    textfore=getApp()->reg().readColorEntry("OPTIONS","textforeground",editor->getTextColor());
    textselback=getApp()->reg().readColorEntry("OPTIONS","textselbackground",editor->getSelBackColor());
    textselfore=getApp()->reg().readColorEntry("OPTIONS","textselforeground",editor->getSelTextColor());
    textcursor=getApp()->reg().readColorEntry("OPTIONS","textcursor",editor->getCursorColor());
    texthiliteback=getApp()->reg().readColorEntry("OPTIONS","texthilitebackground",editor->getHiliteBackColor());
    texthilitefore=getApp()->reg().readColorEntry("OPTIONS","texthiliteforeground",editor->getHiliteTextColor());
    textbar=getApp()->reg().readColorEntry("OPTIONS","textnumberbackground",editor->getBarColor());
    textnumber=getApp()->reg().readColorEntry("OPTIONS","textnumberforeground",editor->getNumberColor());

    // Font
    fontspec=getApp()->reg().readStringEntry("OPTIONS","textfont","");
    if(!fontspec.empty())
    {
        font=new FXFont(getApp(),fontspec);
        font->create();
        editor->setFont(font);
    }

    // Read the Xfe registry
	FXRegistry* reg_xfe=new FXRegistry(XFEAPPNAME,"");
	reg_xfe->read();
	
    // Get value of the retain window position flag
	save_win_pos=reg_xfe->readUnsignedEntry("SETTINGS","save_win_pos",FALSE);

	delete reg_xfe;

    // Get size and position from registry
	if (fromreg)
	{
		ww=getApp()->reg().readUnsignedEntry("OPTIONS","width",DEFAULT_WINDOW_WIDTH);
		hh=getApp()->reg().readUnsignedEntry("OPTIONS","height",DEFAULT_WINDOW_HEIGHT);
	}

    // Showing the status line?
    hidestatus=getApp()->reg().readIntEntry("OPTIONS","hidestatus",FALSE);

    // Showing the tool bar?
    hidetoolbar=getApp()->reg().readIntEntry("OPTIONS","hidetoolbar",FALSE);

    // Highlight match time
    hilitematchtime=getApp()->reg().readIntEntry("OPTIONS","hilitematchtime",3000);

    // Word wrapping
    wrapping=getApp()->reg().readIntEntry("OPTIONS","wordwrap",0);
    wrapcols=getApp()->reg().readIntEntry("OPTIONS","wrapcols",80);

    // Tab settings
    tabcols=getApp()->reg().readIntEntry("OPTIONS","tabcols",8);

    // Various flags
    stripcr=getApp()->reg().readIntEntry("OPTIONS","stripreturn",FALSE);
    linesnum=getApp()->reg().readIntEntry("OPTIONS","linesnum",FALSE);

    // Change the colors
    editor->setTextColor(textfore);
    editor->setBackColor(textback);
    editor->setSelBackColor(textselback);
    editor->setSelTextColor(textselfore);
    editor->setCursorColor(textcursor);
    editor->setHiliteBackColor(texthiliteback);
    editor->setHiliteTextColor(texthilitefore);
    editor->setBarColor(textbar);
    editor->setNumberColor(textnumber);


    // Hide statusline
    if(hidestatus)
    	statusbar->hide();

    // Hide toolbar
    if(hidetoolbar)
    	toolbar->hide();

    // Wrap mode
    if(wrapping)
        editor->setTextStyle(editor->getTextStyle()|TEXT_WORDWRAP);
    else
        editor->setTextStyle(editor->getTextStyle()&~TEXT_WORDWRAP);

    // Wrap and tab columns
	editor->setWrapColumns(wrapcols);
    editor->setTabColumns(tabcols);

    // Highlight match time
    editor->setHiliteMatchTime(hilitematchtime);

	// Get position and position window
    if (save_win_pos && fromreg)
	{
		int xpos=getApp()->reg().readIntEntry("OPTIONS","xpos",DEFAULT_WINDOW_XPOS);
		int ypos=getApp()->reg().readIntEntry("OPTIONS","ypos",DEFAULT_WINDOW_YPOS);
		position(xpos,ypos,ww,hh);
	}
	else
    	position(getX(),getY(),ww,hh);
}


// Save configuration to registry
void WriteWindow::saveConfig()
{
    FXString fontspec;

    // Colors of text
    getApp()->reg().writeColorEntry("OPTIONS","textbackground",editor->getBackColor());
    getApp()->reg().writeColorEntry("OPTIONS","textforeground",editor->getTextColor());
    getApp()->reg().writeColorEntry("OPTIONS","textselbackground",editor->getSelBackColor());
    getApp()->reg().writeColorEntry("OPTIONS","textselforeground",editor->getSelTextColor());
    getApp()->reg().writeColorEntry("OPTIONS","textcursor",editor->getCursorColor());
    getApp()->reg().writeColorEntry("OPTIONS","texthilitebackground",editor->getHiliteBackColor());
    getApp()->reg().writeColorEntry("OPTIONS","texthiliteforeground",editor->getHiliteTextColor());
    getApp()->reg().writeColorEntry("OPTIONS","textnumberbackground",editor->getBarColor());
    getApp()->reg().writeColorEntry("OPTIONS","textnumberforeground",editor->getNumberColor());


    // Write new window size back to registry
    getApp()->reg().writeUnsignedEntry("OPTIONS","width",getWidth());
    getApp()->reg().writeUnsignedEntry("OPTIONS","height",getHeight());
	if (save_win_pos)
	{
		// Account for the Window Manager border size
		XWindowAttributes xwattr;
		if (XGetWindowAttributes((Display*)getApp()->getDisplay(),this->id(),&xwattr))
		{
			getApp()->reg().writeIntEntry("OPTIONS","xpos",getX()-xwattr.x);
			getApp()->reg().writeIntEntry("OPTIONS","ypos",getY()-xwattr.y);
		}
		else
		{
			getApp()->reg().writeIntEntry("OPTIONS","xpos",getX());
			getApp()->reg().writeIntEntry("OPTIONS","ypos",getY());
		}
	}

    // Was status line shown
    getApp()->reg().writeIntEntry("OPTIONS","hidestatus",!statusbar->shown());

    // Was toolbar shown
    getApp()->reg().writeIntEntry("OPTIONS","hidetoolbar",!toolbar->shown());

    // Highlight match time
    getApp()->reg().writeIntEntry("OPTIONS","hilitematchtime",editor->getHiliteMatchTime());

    // Wrap mode
    getApp()->reg().writeIntEntry("OPTIONS","wordwrap",(editor->getTextStyle()&TEXT_WORDWRAP)!=0);
	getApp()->reg().writeIntEntry("OPTIONS","wrapcols",editor->getWrapColumns());

    // Tab settings
    getApp()->reg().writeIntEntry("OPTIONS","tabcols",editor->getTabColumns());

    // Strip returns
    getApp()->reg().writeIntEntry("OPTIONS","stripreturn",stripcr);
    getApp()->reg().writeIntEntry("OPTIONS","linesnum",linesnum);

    // Search path
    getApp()->reg().writeStringEntry("OPTIONS","searchpath",searchpath.text());

    // Font
    fontspec=editor->getFont()->getFont();
    getApp()->reg().writeStringEntry("OPTIONS","textfont",fontspec.text());

	// Write registry options
	getApp()->reg().write();
}


// Harvest the zombies
long WriteWindow::onSigHarvest(FXObject*,FXSelector,void*)
{
	while(waitpid(-1,NULL,WNOHANG)>0);
	return 1;
}


// About box
long WriteWindow::onCmdAbout(FXObject*,FXSelector,void*)
{
	FXString msg;
	msg.format(_("X File Write Version %s is a simple text editor.\n\n"),VERSION);
	msg += COPYRIGHT;
	MessageBox about(this,_("About X File Write"),msg.text(),xfwicon,BOX_OK|DECOR_TITLE|DECOR_BORDER,
	                 JUSTIFY_CENTER_X|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    about.execute(PLACEMENT_OWNER);
    return 1;
}

// Show preferences dialog
long WriteWindow::onCmdMorePrefs(FXObject*,FXSelector,void*)
{
    if (prefsdialog==NULL)
        prefsdialog=new Preferences(this);
    prefsdialog->execute(PLACEMENT_OWNER);
    return 1;
}


// Change text font
long WriteWindow::onCmdFont(FXObject*,FXSelector,void*)
{
    FontDialog fontdlg(this,_("Change Font"),DECOR_BORDER|DECOR_TITLE);
    FXFontDesc fontdesc;
    editor->getFont()->getFontDesc(fontdesc);
    fontdlg.setFontSelection(fontdesc);
    if(fontdlg.execute())
    {
        FXFont *oldfont=font;
        fontdlg.getFontSelection(fontdesc);
        font=new FXFont(getApp(),fontdesc);
        font->create();
        editor->setFont(font);
        delete oldfont;
    }
    saveConfig();
    return 1;
}


// New
long WriteWindow::onCmdNew(FXObject*,FXSelector,void*)
{
    WriteWindow *window=new WriteWindow(getApp(),unique());

	// Smooth scrolling
	window->setSmoothScroll(smoothscroll);

    window->create();
    window->raise();
    window->setFocus();
    return 1;
}


// Open
long WriteWindow::onCmdOpen(FXObject*,FXSelector,void*)
{
    const char *patterns[]=
        {
            _("All Files"),          "*",
            _("Text Files"),         "*.txt",
            _("C Source Files"),     "*.c",
            _("C++ Source Files"),   "*.cpp",
            _("C++ Source Files"),   "*.cc",
            _("C++ Source Files"),   "*.cxx",
            _("C/C++ Header Files"), "*.h",
            _("HTML Files"),         "*.html",
            _("HTML Files"),         "*.htm",
            _("PHP Files"),          "*.php",NULL
        };

    FileDialog opendialog(this,_("Open Document"));	
    opendialog.setSelectMode(SELECTFILE_MULTIPLE);
    opendialog.setPatternList(patterns);
    opendialog.setDirectory(FXPath::directory(filename));
    if(opendialog.execute())
    {
        FXString* files=opendialog.getFilenames();
        unsigned int i=0;
        while (files[i]!=FXString::null)
        {
            WriteWindow *window=findWindow(files[i]);
            if(!window)
            {
                window=findUnused();
                if(!window)
                {
 					// New window
					window=new WriteWindow(getApp(),unique());
					
					// Smooth scrolling
					window->setSmoothScroll(smoothscroll);

					// Set the size and position of the new window
					window->fromreg=FALSE;
					window->ww=getWidth();
					window->hh=getHeight();
					window->xx=getX();
					window->yy=getY();
					
					// Create window
					window->create();
                }
                window->loadFile(files[i]);
            }
            window->raise();
            window->setFocus();
            i++;
        }
        delete[] files;
    }

    return 1;
}


// Open recent file
long WriteWindow::onCmdOpenRecent(FXObject*,FXSelector,void* ptr)
{
    FXString file=(const char*)ptr;
    WriteWindow *window=findWindow(file);
    if(!window)
    {
        window=findUnused();
        if(!window)
        {
            window=new WriteWindow(getApp(),unique());

			// Smooth scrolling
			window->setSmoothScroll(smoothscroll);

            window->create();
        }
        window->loadFile(file);
    }
    window->raise();
    window->setFocus();
    return 1;
}


// See if we can get it as a filename
long WriteWindow::onEditDNDDrop(FXObject*,FXSelector,void*)
{
    char *data;
    unsigned int len;
    if(getDNDData(FROM_DRAGNDROP,urilistType,(unsigned char*&)data,len))
    {
        FXString urilist(data,len);
        FXString file=FXURL::fileFromURL(urilist.before('\r'));
        FXFREE(&data);
        if(file.empty())
            return 1;
        if(!saveChanges())
            return 1;
        loadFile(file);
        return 1;
    }
    return 0;
}


// See if a filename is being dragged over the window
long WriteWindow::onEditDNDMotion(FXObject*,FXSelector,void*)
{
    if(offeredDNDType(FROM_DRAGNDROP,urilistType))
    {
        acceptDrop(DRAG_COPY);
        return 1;
    }
    return 0;
}


// Save changes, prompt for new filename
FXbool WriteWindow::saveChanges()
{
    unsigned int answer;
    FXString file;
    if(isModified())
    {
        answer=MessageBox::question(this,BOX_YES_NO_CANCEL,_("Unsaved Document"),_("Save %s to file?"),FXPath::name(filename).text());
        if(answer==BOX_CLICKED_CANCEL)
        	return FALSE;
        if(answer==BOX_CLICKED_YES)
        {
            file=filename;
            if(!filenameset)
            {
				FileDialog savedialog(this,_("Save Document"));                
				savedialog.setSelectMode(SELECTFILE_ANY);
                savedialog.setFilename(file);
                if(!savedialog.execute())
                	return FALSE;
                file=savedialog.getFilename();
                if(FXStat::exists(file))
                    if(BOX_CLICKED_NO==MessageBox::question(this,BOX_YES_NO,_("Overwrite Document"),_("Overwrite existing document: %s?"),file.text()))
                    	return FALSE;
           }
            saveFile(file);
        }
    }
    return TRUE;
}


// Save
long WriteWindow::onCmdSave(FXObject* sender,FXSelector sel,void* ptr)
{
    if(!filenameset)
    	return onCmdSaveAs(sender,sel,ptr);
    saveFile(filename);
    return 1;
}


// Save Update
long WriteWindow::onUpdSave(FXObject* sender,FXSelector,void*)
{
    sender->handle(this,isModified()?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    return 1;
}


// Save As
long WriteWindow::onCmdSaveAs(FXObject*,FXSelector,void*)
{
	FileDialog savedialog(this,_("Save Document"));    
    FXString file=filename;
    savedialog.setSelectMode(SELECTFILE_ANY);
    savedialog.setFilename(file);
    if(savedialog.execute())
    {
        file=savedialog.getFilename();
        if(FXStat::exists(file))
        {
            if(BOX_CLICKED_NO==MessageBox::question(this,BOX_YES_NO,_("Overwrite Document"),_("Overwrite existing document: %s?"),file.text()))
                return 1;
        }
        saveFile(file);
    }

    return 1;
}


// Close window
FXbool WriteWindow::close(FXbool notify)
{
    // Prompt to save changes
    if(!saveChanges())
        return FALSE;

    // Save settings
    saveConfig();

    // Perform normal close stuff
    return FXMainWindow::close(notify);
}


// User clicks on one of the window menus
long WriteWindow::onCmdWindow(FXObject*,FXSelector sel,void*)
{
    int which=FXSELID(sel)-ID_WINDOW_1;
    if(which<getApp()->windowlist.no())
    {
        getApp()->windowlist[which]->raise();
        getApp()->windowlist[which]->setFocus();
    }
    return 1;
}


// Update handler for window menus
long WriteWindow::onUpdWindow(FXObject *sender,FXSelector sel,void*)
{
    int which=FXSELID(sel)-ID_WINDOW_1;
    if(which<getApp()->windowlist.no())
    {
        WriteWindow *window=getApp()->windowlist[which];
        FXString string;
        if(which<49)
            string.format("&%d %s",which+1,window->getTitle().text());
        else
            string.format("5&0 %s",window->getTitle().text());

        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);

        if(window==getApp()->getActiveWindow())
            sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_CHECK),NULL);

        else
            sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_UNCHECK),NULL);
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SHOW),NULL);
    }
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
    return 1;
}


// Update title from current filename
long WriteWindow::onUpdateTitle(FXObject* sender,FXSelector sel,void* ptr)
{
    FXMainWindow::onUpdate(sender,sel,ptr);
    FXString title=FXPath::name(getFilename());
    if(isModified())
        title+=_(" (changed)");
    FXString directory=FXPath::directory(getFilename());
    if(!directory.empty())
        title+=" - " + directory;
    setTitle(title);
    return 1;
}


// Print the text
long WriteWindow::onCmdPrint(FXObject*,FXSelector,void*)
{
    // Read the current print command from the registry
    FXString printcommand, command;
    printcommand=getApp()->reg().readStringEntry("OPTIONS","print_command","lpr -P printer");

    // Open print dialog filled with the current print command
    int rc=1;
    if (printdialog==NULL)
        printdialog=new InputDialog(this,printcommand,_("Print command: \n(ex: lpr -P <printer>)"),_("Print"),"",printbigicon);
    printdialog->setText(printcommand);
    printdialog->CursorEnd();
    rc=printdialog->execute(PLACEMENT_CURSOR);
    printcommand=printdialog->getText();

    // If cancel was pressed, exit
    if (!rc)
        return 0;

    // Write the new print command to the registry
    getApp()->reg().writeStringEntry("OPTIONS","print_command",printcommand.text());

    // Perform the print command
    command =  "cat " + filename + " |" + printcommand + " &";
    system(command.text());

    return 1;
}


// Toggle wrap mode
long WriteWindow::onCmdWrap(FXObject*,FXSelector,void*)
{
    editor->setTextStyle(editor->getTextStyle()^TEXT_WORDWRAP);
    return 1;
}


// Update toggle wrap mode
long WriteWindow::onUpdWrap(FXObject* sender,FXSelector,void*)
{
    if(editor->getTextStyle()&TEXT_WORDWRAP)
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}

// Toggle lines numbering
long WriteWindow::onCmdLinesNum(FXObject*,FXSelector,void*)
{
    linesnum=!linesnum;
    if (linesnum)
    {
        unsigned int size=editor->getNumRows();
        unsigned int cols=(unsigned int)ceil(log10(size));
        editor->setBarColumns(cols);
    }
    else
        editor->setBarColumns(0);

    return 1;
}


// Update toggle line numbers
long WriteWindow::onUpdLinesNum(FXObject* sender,FXSelector,void*)
{
    if(linesnum)
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Update box for overstrike mode display
long WriteWindow::onUpdOverstrike(FXObject* sender,FXSelector,void*)
{
    FXString mode((editor->getTextStyle()&TEXT_OVERSTRIKE)?_("OVR"):_("INS"));
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&mode);
    return 1;
}


// Update box for size display
long WriteWindow::onUpdNumRows(FXObject* sender,FXSelector,void*)
{
    unsigned int size=editor->getNumRows();
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&size);
    return 1;
}




// Text inserted; callback has [pos nins]
long WriteWindow::onTextInserted(FXObject*,FXSelector,void* ptr)
{
    const FXTextChange *change=(const FXTextChange*)ptr;

    // Log undo record
    if(!undolist.busy())
    {
        undolist.add(new FXTextInsert(editor,change->pos,change->nins,change->ins));
        if(undolist.size()>MAXUNDOSIZE)
        	undolist.trimSize(KEEPUNDOSIZE);
    }


    return 1;
}


// Text deleted; callback has [pos ndel]
long WriteWindow::onTextDeleted(FXObject*,FXSelector,void* ptr)
{
    const FXTextChange *change=(const FXTextChange*)ptr;

    // Log undo record
    if(!undolist.busy())
    {
        undolist.add(new FXTextDelete(editor,change->pos,change->ndel,change->del));
        if(undolist.size()>MAXUNDOSIZE)
        	undolist.trimSize(KEEPUNDOSIZE);
    }

    return 1;
}


// Text replaced; callback has [pos ndel nins]
long WriteWindow::onTextReplaced(FXObject*,FXSelector,void* ptr)
{
    const FXTextChange *change=(const FXTextChange*)ptr;

    // Log undo record
    if(!undolist.busy())
    {
        undolist.add(new FXTextReplace(editor,change->pos,change->ndel,change->nins,change->del,change->ins));
        if(undolist.size()>MAXUNDOSIZE)
        	undolist.trimSize(KEEPUNDOSIZE);
    }

    return 1;
}


// Released right button
long WriteWindow::onTextRightMouse(FXObject*,FXSelector,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    if(!event->moved)
    {
        allowPopupScroll=TRUE; // Allow keyboard scrolling
        popupmenu->popup(NULL,event->root_x,event->root_y);
        getApp()->runModalWhileShown(popupmenu);
        allowPopupScroll=FALSE;
    }
    return 1;
}


// Check file when focus moves in
long WriteWindow::onFocusIn(FXObject* sender,FXSelector sel,void* ptr)
{
    register FXTime t;
    FXMainWindow::onFocusIn(sender,sel,ptr);
    if(filetime!=0)
    {
        t=FXStat::modified(filename);
        if(t && t!=filetime)
        {
            filetime=t;
            if(BOX_CLICKED_OK==MessageBox::warning(this,BOX_OK_CANCEL,_("File Was Changed"),_("%s\nwas changed by another program. Reload this file from disk?"),filename.text()))
            {
                int top=editor->getTopLine();
                int pos=editor->getCursorPos();
                loadFile(filename);
                editor->setTopLine(top);
                editor->setCursorPos(pos);
            }
        }
    }
    return 1;
}


// Search text
long WriteWindow::onCmdSearch(FXObject*,FXSelector,void*)
{
    if (searchdialog==NULL)
        searchdialog=new FXSearchDialog(this,_("Search"),searchicon);
    int beg[10];
    int end[10];
    int pos;
    unsigned int code;
    FXString searchstring;
    unsigned int searchflags;
    do
    {
        code=searchdialog->execute();
        if(code==FXSearchDialog::DONE)
            return 1;
        searchstring=searchdialog->getSearchText();
        searchflags=searchdialog->getSearchMode();
        pos=editor->isPosSelected(editor->getCursorPos()) ? (searchflags&SEARCH_BACKWARD) ? editor->getSelStartPos()-1 : editor->getSelEndPos() : editor->getCursorPos();
        if(editor->findText(searchstring,beg,end,pos,searchflags|SEARCH_WRAP,10))
        {
            editor->setAnchorPos(beg[0]);
            editor->extendSelection(end[0],SELECT_CHARS,TRUE);
            editor->setCursorPos(end[0],TRUE);
            editor->makePositionVisible(beg[0]);
            editor->makePositionVisible(end[0]);
        }
        else
        {
            getApp()->beep();
        }
    }
    while(code==FXSearchDialog::SEARCH_NEXT);
    return 1;
}


// Replace text; we assume that findText has called squeezegap()!
long WriteWindow::onCmdReplace(FXObject*,FXSelector,void*)
{
    replacedialog=new FXReplaceDialog(this,_("Replace"),replaceicon);
    int beg[10],end[10],fm,to,len,pos;
    unsigned int searchflags,code;
    FXString searchstring;
    FXString replacestring;
    FXString replacevalue;

    // Get text into buffer
    char* buffer;
    int length=editor->getLength();
    FXMALLOC(&buffer,char,length);
    editor->getText(buffer,length);

    do
    {
        code=replacedialog->execute();
        if(code==FXReplaceDialog::DONE)
            goto ret;
        searchflags=replacedialog->getSearchMode();
        searchstring=replacedialog->getSearchText();
        replacestring=replacedialog->getReplaceText();
        replacevalue=FXString::null;
        fm=-1;
        to=-1;
        if(code==FXReplaceDialog::REPLACE_ALL)
        {
            searchflags&=~SEARCH_BACKWARD;
            pos=0;
            while(editor->findText(searchstring,beg,end,pos,searchflags,10))
            {
                if(0<=fm)
                    replacevalue.append(&buffer[pos],beg[0]-pos);
                replacevalue.append(FXRex::substitute(buffer,length,beg,end,replacestring,10));
                if(fm<0)
                    fm=beg[0];
                to=end[0];
                pos=end[0];
                if(beg[0]==end[0])
                    pos++;
            }
        }
        else
        {
            pos=editor->isPosSelected(editor->getCursorPos()) ? (searchflags&SEARCH_BACKWARD) ? editor->getSelStartPos()-1 : editor->getSelEndPos() : editor->getCursorPos();
            if(editor->findText(searchstring,beg,end,pos,searchflags|SEARCH_WRAP,10))
            {
                replacevalue=FXRex::substitute(buffer,length,beg,end,replacestring,10);
                fm=beg[0];
                to=end[0];
            }
        }
        if(0<=fm)
        {
            len=replacevalue.length();
            editor->replaceText(fm,to-fm,replacevalue.text(),len,TRUE);
            editor->setCursorPos(fm+len,TRUE);
            editor->makePositionVisible(editor->getCursorPos());
            editor->setModified(TRUE);
        }
        else
        {
            getApp()->beep();
        }
    }
    while(code==FXReplaceDialog::REPLACE_NEXT);

ret:
    FXFREE(&buffer);
    return 1;
}



// Search for selected text
long WriteWindow::onCmdSearchSel(FXObject*,FXSelector sel,void*)
{
    FXString string;
    unsigned int searchflags;
    FXString searchstring;
    int pos=editor->getCursorPos();
    int beg,end;

    // First, try UTF-8
    if(getDNDData(FROM_SELECTION,utf8Type,string))
        searchstring=string;

    // Next, try UTF-16
    else if(getDNDData(FROM_SELECTION,utf16Type,string))
    {
        FXUTF16LECodec unicode;                 // FIXME maybe other endianness for unix
        searchstring=unicode.mb2utf(string);
    }

    // Finally, try good old 8859-1
    else if(getDNDData(FROM_SELECTION,stringType,string))
    {
        FX88591Codec ascii;
        searchstring=ascii.mb2utf(string);
    }

    // No dice!
    else
    {
        goto x;
    }
	
    // Search direction
    if(FXSELID(sel)==ID_SEARCH_FORW_SEL)
    {
        if(editor->isPosSelected(pos))
        	pos=editor->getSelEndPos();
        searchflags=SEARCH_EXACT|SEARCH_FORWARD;
    }
    else
    {
        if(editor->isPosSelected(pos))
        	pos=editor->getSelStartPos()-1;
        searchflags=SEARCH_EXACT|SEARCH_BACKWARD;
    }

    // Perform search
    if(editor->findText(searchstring,&beg,&end,pos,searchflags|SEARCH_WRAP))
    {
        if(beg!=editor->getSelStartPos() || end!=editor->getSelEndPos())
        {
            editor->setAnchorPos(beg);
            editor->extendSelection(end,SELECT_CHARS,TRUE);
            editor->setCursorPos(end);
            editor->makePositionVisible(beg);
            editor->makePositionVisible(end);
            return 1;
        }
    }

    // Beep
x:
    getApp()->beep();
    return 1;
}


// Go to line number
long WriteWindow::onCmdGotoLine(FXObject*,FXSelector,void*)
{
    int row=editor->getCursorRow()+1;
    if(FXInputDialog::getInteger(row,this,_("Goto Line"),_("&Goto line number:"),gotobigicon,1,2147483647))
    {
        update();
        editor->setCursorRow(row-1,TRUE);
        editor->makePositionVisible(editor->getCursorPos());
    }
    return 1;
}
