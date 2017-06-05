#ifndef DIRHISTBOX_H
#define DIRHISTBOX_H

#ifndef DIRHISTBOX_H
#include "DirHistBox.h"
#endif


#include "DialogBox.h"


class FXAPI DirHistBox : public DialogBox
{
    FXDECLARE(DirHistBox)
protected:
    FXList *list;
protected:
    DirHistBox()
    {}
private:
    DirHistBox(const DirHistBox&);
    DirHistBox &operator=(const DirHistBox&);
public:
    long onCmdClicked(FXObject*,FXSelector,void*);
    long onCmdClose(FXObject*,FXSelector,void*);
	long onKeyPress(FXObject*,FXSelector,void*);
	long onKeyRelease(FXObject*,FXSelector,void*);
public:
    enum{
        ID_CLICKED=DialogBox::ID_LAST,
		ID_CLOSE,
        ID_LAST
    };
public:

    // Construct list box with given caption, icon, message text, and with choices from array of strings
    DirHistBox(FXWindow* owner,const char** choices,unsigned int opts=0,int x=0,int y=0,int w=0,int h=0);

    // Construct list box with given caption, icon, message text, and with choices from newline separated strings
    DirHistBox(FXWindow* owner,const FXString& choices,unsigned int opts=0,int x=0,int y=0,int w=0,int h=0);

	// Show a modal list dialog. Prompt the user using a dialog with given caption, icon, message text, and with choices from newline array of strings.
    // The return value is -1 if cancelled, or the given choice
    static int box(FXWindow* owner,unsigned int opts,const char** choices,int x=0,int y=0,int w=0,int h=0);

    // Show a modal list dialog. Prompt the user using a dialog with given caption, icon, message text, and with choices from newline separated strings.
    // The return value is -1 if cancelled, or the given choice
    static int box(FXWindow* owner,unsigned int opts,const FXString& choices,int x=0,int y=0,int w=0,int h=0);

    // Destroy list box
    virtual ~DirHistBox();
};

#endif
