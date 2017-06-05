#ifndef EXECUTEBOX_H
#define EXECUTEBOX_H


#include "DialogBox.h"

// Return values
enum ExecuteBoxReturn
{
    EXECBOX_CLICKED_CANCEL       = 0,
    EXECBOX_CLICKED_EXECUTE      = 1,
    EXECBOX_CLICKED_CONSOLE      = 2,
    EXECBOX_CLICKED_EDIT         = 3,
};


// Message box
class FXAPI ExecuteBox : public DialogBox
{
    FXDECLARE(ExecuteBox)
protected:
    ExecuteBox()
    {}
    ExecuteBox(const ExecuteBox&)
    {}
public:
    long onCmdClicked(FXObject*,FXSelector,void*);
public:
    enum{
        ID_CLICKED_CANCEL=DialogBox::ID_LAST,
		ID_CLICKED_EXECUTE,
        ID_CLICKED_CONSOLE,
        ID_CLICKED_EDIT,
        ID_LAST
    };
public:
    ExecuteBox(FXWindow *win,const FXString& name,const FXString& text,unsigned int opts=DECOR_TITLE|DECOR_BORDER,int x=0,int y=0);
};

#endif
