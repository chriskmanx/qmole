#ifndef MSRadioMenuItemHEADER
#define MSRadioMenuItemHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSToggleMenuItem.H>

class MSGUIExport MSRadioMenuItem : public MSToggleMenuItem 
{
protected:
  virtual void drawSymbol(void);
  virtual void radioDisarm(void);
  virtual int symbolHeight(void) const;
  virtual int symbolWidth(void) const;

public:
  MSRadioMenuItem(MSMenu *);
  MSRadioMenuItem(MSMenu *,const char *,char mnemonic_=0,int tag_=0);
  MSRadioMenuItem(MSMenu *,const MSString&,char mnemonic_=0,int tag_=0);
  MSRadioMenuItem(MSMenu *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,int tag_=0);
  MSRadioMenuItem(MSMenu *,const MSString &,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
                  const MSPixmap &insensitivePixmap_,char mnemonic_=0,int tag_=0);
  MSRadioMenuItem(MSMenu *,const MSString &,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
                  char mnemonic_=0,int tag_=0);
  MSRadioMenuItem(MSMenu *,const char *,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,
                  const MSPixmap &insensitivePixmap_,char mnemonic_=0,int tag_=0);
  MSRadioMenuItem(MSMenu *,const char *,const MSPixmap &pixmap_,const MSPixmap &armedPixmap_,char mnemonic_=0,int tag_=0);
  ~MSRadioMenuItem(void);

  virtual void activate(void);
};

#endif
