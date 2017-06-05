#ifndef MSTitleMenuItemHEADER
#define MSTitleMenuItemHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSMenuItem.H>

class MSGUIExport MSTitleMenuItem : public MSMenuItem 
{
public:
  MSTitleMenuItem(MSMenu *);
  MSTitleMenuItem(MSMenu *, const char *title_);
  MSTitleMenuItem(MSMenu *, const MSString& title_);
  ~MSTitleMenuItem(void);

  void alignment(MSAlignment);
  MSAlignment alignment(void) const { return _alignment; }

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);

protected:
  MSAlignment _alignment;
  
  void init(void);

  virtual void updateSensitivity(void);
  virtual int computeXCoord(void);

};

#endif
