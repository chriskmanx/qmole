#ifndef AplusDialogHEADER
#define AplusDialogHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPopup.H>

//
// AplusDialog is a dialog box that displays a message. This is a base class
// for all standard dialogs, which simply provide different defaults.
// This dialog has modal behavior. Use modal(MSFalse) to unset that.
// This dialog will destroy itself when any button is pressed, unless destroyDialog 
// is overidden
//
// The following are the parameters to the constructor of
// this dialog box:  
//
// title_        - specified the title of the dialog.
// transientFor_ - shell widget that this dialog is transient for.
// msg_          - string that appears in the dialog.
// showFlag_     - specifies which buttons to display, can be any combination
//                 of DialogFlag value logically or'ed together.
// focusFlag_    - specifies which button should have focus when dialog is 
//                 first shown.  This button is also activated when the 'Close'
//                 item of the window manager menu is selected.
// pix_          - one of PixmapFlag. Specifies which of predefines pixmaps to display.
// pixmap_       - specifies custom pixmap to use.
//

class MSLabel;
class MSButton;
class MSLayout;

class AplusDialog : public MSPopup 
{
public:
  enum DialogFlag { OK=1,Yes=2,No=4,Cancel=8,Help=16};
  enum PixmapFlag { Error=0,Information,Question,Warning,Working };
  
  AplusDialog(const char *title_,unsigned buttonFlag_=OK,DialogFlag focusFlag_=OK);
  AplusDialog(MSDisplayServer *server_,const char *title_,
	   unsigned showFlag_=OK,DialogFlag focusFlag_=OK);
  AplusDialog(MSShell *transientFor_,const char *title_,
	   unsigned showFlag_=OK,DialogFlag focusFlag_=OK);

  AplusDialog(const char *title_,const char *msg_, PixmapFlag pix_=Information,
	   unsigned buttonFlag_=OK,DialogFlag focusFlag_=OK);
  AplusDialog(const char *title_,const char *msg_, const MSPixmap& pixmap_,
	   unsigned showFlag_=OK,DialogFlag focusFlag_=OK);

  AplusDialog(MSDisplayServer *server_,const char *title_,const char *msg_, 
	   PixmapFlag pix_=Information,unsigned showFlag_=OK,DialogFlag focusFlag_=OK);
  AplusDialog(MSDisplayServer *server_,const char *title_,const char *msg_, 
	   const MSPixmap& pixmap_,unsigned showFlag_=OK,DialogFlag focusFlag_=OK);
  
  AplusDialog(MSShell *transientFor_,const char *title_,const char *msg_, 
	   PixmapFlag pix_,unsigned showFlag_=OK,DialogFlag focusFlag_=OK);
  AplusDialog(MSShell *transientFor_,const char *title_,const char *msg_, 
	   const MSPixmap& pix_,unsigned showFlag_=OK,DialogFlag focusFlag_=OK);

  ~AplusDialog();
             
  virtual void show(void);
  virtual void naturalSize(void);  

  DialogFlag query(void);

  // accessors to buttons. some may return 0 if button not used.
  MSButton *okButton(void)         { return _okButton; }
  MSButton *yesButton(void)        { return _yesButton; }
  MSButton *noButton(void)         { return _noButton; }
  MSButton *cancelButton(void)     { return _cancelButton; }
  MSButton *helpButton(void)       { return _helpButton; }

  MSLayout *layout(void)           { return _layout; }
  MSLabel  *label(void)            { return _label; }

protected:
  MSLayout    *_layout;
  MSLabel     *_label;
  MSButton    *_okButton;
  MSButton    *_cancelButton;
  MSButton    *_yesButton;
  MSButton    *_noButton;
  MSButton    *_helpButton;

  MSLayout    *_buttonLayout;
  DialogFlag   _focusFlag;
  
  DialogFlag focusFlag(void) const { return _focusFlag; }

  void init(unsigned);
  void buildStandardLayout(const char *, const MSPixmap&);
  
  void doOK(void)       { ok(); }
  void doCancel(void)   { cancel(); }
  void doYes(void)      { yes(); }
  void doNo(void)       { no(); }
  void doHelp(void)     { help(); }

  virtual void dismiss(void);
  virtual void destroyDialog(void);   // Can be ovveriden to prevent auto-destruction

  // Subclass can override the following actions.
  virtual void ok(void);      // OK button activation
  virtual void cancel(void);  // Cancel button activation
  virtual void yes(void);     // Yes button activation
  virtual void no(void);      // No button activation
  virtual void help(void);    // Help button activation

};


#endif
