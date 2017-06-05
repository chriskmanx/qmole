#ifndef MSLayoutHEADER
#define MSLayoutHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSLayoutManager.H>
#include <MSGUI/MSLabel.H>

class MSGUIExport MSLayout : public MSLayoutManager
{
protected:
  MSLabel *_label;

  MSLabel *label(void)             {return _label;}
  const MSLabel *label(void) const {return _label;}

  void init(void);
  
  virtual int innerHeight(void) const;
  virtual int realHeight(void) const;
  virtual int idealHeight(void) const;
  virtual int realWidth(void) const;
  virtual int idealWidth(void) const;

  virtual void configure(void);
  virtual void childConfigure(MSWidget *);
  virtual void childMap(MSWidget *);
  virtual void childUnmap(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childDestroy(MSWidget *);

  virtual void updateTitle(void);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);

  virtual void positionLabel(void);
  virtual void computeSize(void);
  
public:
  MSLayout(MSWidget *,const char *title_=0);
  MSLayout(MSWidget *,const MSStringVector&);
  ~MSLayout(void);

  virtual void naturalSize(void);
};

#endif




