#ifndef MSTabStringListHEADER
#define MSTabStringListHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSStringList.H>
#include <MSTypes/MSUnsignedLongVector.H>

class MSGUIExport MSTabStringList : public MSStringList
{
public:

  MSTabStringList(MSWidget *,const char *title_=0);
  MSTabStringList(MSWidget *,const MSStringVector& title_);
  MSTabStringList(MSWidget *,MSStringVector&, const char*title_=0);
  MSTabStringList(MSWidget *,MSStringVector&, const MSStringVector& title_);
  
  ~MSTabStringList(void);

  const unsigned long& tabSize(void) const;
  void tabSize(unsigned long);
  const MSUnsignedLongVector& tabStops(void) const;    
  void tabStops(const MSUnsignedLongVector&);

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);
protected:
  virtual void calculateMaxLength(void);
  virtual unsigned rowLength(unsigned) const;

  virtual void drawString(Display *,Window,GC,const XFontStruct *,int,int,const char *,int);

  virtual int nextTabPosition(int,int,int&) const;
  
  unsigned long _tabSize;
  MSUnsignedLongVector _tabStops;
};

#endif
