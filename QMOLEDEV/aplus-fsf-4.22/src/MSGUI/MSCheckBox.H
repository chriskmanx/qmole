#ifndef MSCheckBoxHEADER
#define MSCheckBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSActionBox.H>
#include <MSGUI/MSCheckButton.H>

class MSGUIExport MSCheckBox : public MSActionBox
{
friend class MSCheckButton;
public:  
  MSCheckBox(MSWidget *,const char *title_=0);
  MSCheckBox(MSWidget *,const MSStringVector&);
  ~MSCheckBox(void);

  MSIntVector integerState(void) const;
  void integerState(const MSIntVector &);

  MSSymbolVector symbolicState(void) const;
  void symbolicState(const MSSymbolVector &);
   
  virtual const MSSymbol& widgetType(void) const;

protected:
  virtual void arm(MSCheckButton *);
  virtual void disarm(MSCheckButton *);

  static const MSSymbol& symbol(void);
};

inline MSIntVector MSCheckBox::integerState(void) const
{ return MSActionBox::integerState(); }

inline MSSymbolVector MSCheckBox::symbolicState(void) const
{ return MSActionBox::symbolicState(); }

#endif












