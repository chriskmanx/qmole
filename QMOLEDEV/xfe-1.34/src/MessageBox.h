#ifndef MESSAGEBOX_H
#define MESSAGEBOX_H

#include "DialogBox.h"

// Message box buttons
enum {
		BOX_OK                   = 0x10000000, // Message box has a only an OK button
		BOX_OK_CANCEL            = 0x20000000, // Message box has OK and CANCEL buttons
		BOX_YES_NO               = 0x30000000, // Message box has YES and NO buttons
		BOX_YES_NO_CANCEL        = 0x40000000, // Message box has YES, NO, and CANCEL buttons
		BOX_QUIT_CANCEL          = 0x50000000, // Message box has QUIT and CANCEL buttons
		BOX_QUIT_SAVE_CANCEL     = 0x60000000, // Message box has QUIT, SAVE, and CANCEL buttons
		BOX_YES_NO_ALL_CANCEL 	 = 0x70000000, // Message box has YES, NO, ALL and CANCEL buttons
		BOX_OK_SU 			     = 0x80000000  // Message box has OK and SU buttons
};
enum {
		BOX_CLICKED_YES      = 1,            // The YES button was clicked
		BOX_CLICKED_NO       = 2,            // The NO button was clicked
		BOX_CLICKED_OK       = 3,            // The OK button was clicked
		BOX_CLICKED_CANCEL   = 4,            // The CANCEL button was clicked
		BOX_CLICKED_QUIT     = 5,            // The QUIT button was clicked
		BOX_CLICKED_SAVE     = 6,            // The SAVE button was clicked
		BOX_CLICKED_ALL      = 7,            // The ALL button was clicked
		BOX_CLICKED_SU       = 8             // The SU button was clicked
 };
  
  
// Message box
class FXAPI MessageBox : public DialogBox
{
    FXDECLARE(MessageBox)
protected:
    MessageBox()
    {}
private:
    MessageBox(const MessageBox&);
    MessageBox &operator=(const MessageBox&);
    void initialize(const FXString&,FXIcon*,unsigned int,unsigned int);
    FXLabel* msg;
public:
    long onCmdClicked(FXObject*,FXSelector,void*);
    long onCmdCancel(FXObject*,FXSelector,void*);
	long onCmdSu(FXObject*,FXSelector sel,void*);
public:
    enum {
        ID_CLICKED_YES=DialogBox::ID_LAST,
        ID_CLICKED_NO,
        ID_CLICKED_OK,
        ID_CLICKED_CANCEL,
        ID_CLICKED_QUIT,
        ID_CLICKED_SAVE,
        ID_CLICKED_ALL,
        ID_CLICKED_SU,
        ID_LAST
    };
public:

    // Construct message box with given caption, icon, and message text
    MessageBox(FXWindow* owner,const FXString& caption,const FXString& text,FXIcon* ic=NULL,unsigned int opts=0,
	           unsigned int textopts=JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y,
			   int x=0,int y=0);

    // Construct free floating message box with given caption, icon, and message text
    MessageBox(FXApp* a,const FXString& caption,const FXString& text,FXIcon* ic=NULL,unsigned int opts=0,
	           unsigned int textopts=JUSTIFY_LEFT|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y,
			   int x=0,int y=0);

    // Show a modal error message.
    // The text message may contain printf-tyle formatting commands.
    static unsigned int error(FXWindow* owner,unsigned int opts,const char* caption,const char* message,...) FX_PRINTF(4,5) ;
    static unsigned int error(FXApp* app,unsigned int opts,const char* caption,const char* message,...) FX_PRINTF(4,5) ;

    // Show a modal warning message
    // The text message may contain printf-tyle formatting commands.
    static unsigned int warning(FXWindow* owner,unsigned int opts,const char* caption,const char* message,...) FX_PRINTF(4,5) ;
    static unsigned int warning(FXApp* app,unsigned int opts,const char* caption,const char* message,...) FX_PRINTF(4,5) ;

    // Show a modal question dialog
    // The text message may contain printf-tyle formatting commands.
    static unsigned int question(FXWindow* owner,unsigned int opts,const char* caption,const char* message,...) FX_PRINTF(4,5) ;
    static unsigned int question(FXApp* app,unsigned int opts,const char* caption,const char* message,...) FX_PRINTF(4,5) ;

    // Show a modal information dialog
    // The text message may contain printf-tyle formatting commands.
    static unsigned int information(FXWindow* owner,unsigned int opts,const char* caption,const char* message,...) FX_PRINTF(4,5) ;
    static unsigned int information(FXApp* app,unsigned int opts,const char* caption,const char* message,...) FX_PRINTF(4,5) ;

	// Get message text
	FXString getText(void)
	{
		return msg->getText();
	}
	
	// Set message text
	void setText(FXString text)
	{
		msg->setText(text);
	}

};

#endif
