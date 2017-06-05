#ifndef OVERWRITEBOX_H
#define OVERWRITEBOX_H


#include "DialogBox.h"

// Return values
enum OverwriteBoxReturn
{
    OVWBOX_CLICKED_CANCEL       = 0,
    OVWBOX_CLICKED_OVERWRITE    = 1,
    OVWBOX_CLICKED_OVERWRITE_ALL= 2,
    OVWBOX_CLICKED_SKIP         = 3,
    OVWBOX_CLICKED_SKIP_ALL     = 4,
};


// Message box
class FXAPI OverwriteBox : public DialogBox
{
    FXDECLARE(OverwriteBox)
protected:
    OverwriteBox()
    {}
    OverwriteBox(const OverwriteBox&)
    {}
public:
    long onCmdClicked(FXObject*,FXSelector,void*);
public:
    enum{
        ID_CLICKED_CANCEL=DialogBox::ID_LAST,
		ID_CLICKED_OVERWRITE,
        ID_CLICKED_OVERWRITE_ALL,
        ID_CLICKED_SKIP,
        ID_CLICKED_SKIP_ALL,
        ID_LAST
    };
public:
    OverwriteBox(FXWindow *win,const FXString& name,const FXString& text,unsigned int opts=DECOR_TITLE|DECOR_BORDER,int x=0,int y=0);
};

#endif
