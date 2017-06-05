#ifndef MSCascadeMenuItemHEADER
#define MSCascadeMenuItemHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenuItem.H>

class MSGUIExport MSCascadeMenuItem : public MSMenuItem 
{
friend class MSCascadeMenu;
protected:
  MSArrow        *_arrow;
  MSCascadeMenu  *_cascadeMenu;

  MSArrow *arrow(void) const { return _arrow; }  

  void init(void);
  virtual MSMenu* cascadedMenu(void);
  virtual const MSMenu* cascadedMenu(void) const;
  void computeSize(void);
  void cascadeMenu(MSCascadeMenu *);

  virtual void computePixmapDrawingCoord(const MSPixmap *,int&,int&,int&,int&,int&,int&);
  virtual void drawSymbol(void);
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);

  void menuDestroy(MSCascadeMenu *);

  virtual void menushow(void);
  virtual MSAttrValueList& get(MSAttrValueList&);

public:
  MSCascadeMenuItem(MSMenu *);
  MSCascadeMenuItem(MSMenu *,const char *,char mnemonic_=0,int tag_=0);
  MSCascadeMenuItem(MSMenu *,const MSString&,char mnemonic_=0,int tag_=0);
  MSCascadeMenuItem(MSMenu *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,int tag_=0);
  ~MSCascadeMenuItem(void);

  MSCascadeMenu *cascadeMenu(void)             { return _cascadeMenu; }
  const MSCascadeMenu *cascadeMenu(void) const { return _cascadeMenu; }

  virtual void grab(void);
  virtual void arm(void);
  virtual void disarm(void);
};

#endif
