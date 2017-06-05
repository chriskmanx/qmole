#ifndef BROWSEINPUTDIALOG_H
#define BROWSEINPUTDIALOG_H

#include "TextLabel.h"
#include "DialogBox.h"

// Browse types
enum
{
    BROWSE_INPUT_FILE,
	BROWSE_INPUT_FOLDER,
	BROWSE_INPUT_MIXED
};

class XComApp;

class BrowseInputDialog : public DialogBox
{
    FXDECLARE(BrowseInputDialog)
protected:
    FXTextField*       input;
    TextLabel*         msg;
    FXLabel*			iconlabel;
    FXDataTarget*      string_target;
    FXHorizontalFrame* checkbutton;
	unsigned int			   browsetype;
	FXString           initialdir;
private:
    BrowseInputDialog()
    {
        ;
    }
public:
    enum
	{
        ID_BROWSE_PATH=DialogBox::ID_LAST,
        ID_LAST
    };
    BrowseInputDialog(FXWindow*,FXString,FXString,FXString,FXString label="",FXIcon *ic=NULL, unsigned int browse=BROWSE_INPUT_FILE, FXbool option=FALSE, FXString=FXString::null);
    virtual void create();
    virtual ~BrowseInputDialog();
    long onCmdKeyPress(FXObject*,FXSelector,void*);
	long onCmdBrowsePath(FXObject*,FXSelector,void*);
	void setMessage(FXString);
	void setIcon(FXIcon*);
	void setDirectory(const FXString&);
    FXString getText();
	void setText(const FXString&);
    void SelectAll();
    void CursorEnd();
  	FXbool setSelection(FXint, FXint);
};
#endif
