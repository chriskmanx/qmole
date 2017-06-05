#ifndef MSActionBoxHEADER
#define MSActionBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSLayout.H>
#include <MSGUI/MSFontObject.H>
#include <MSTypes/MSInt.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSSymbolVector.H>

class MSActionButton;

class MSGUIExport MSActionBox : public MSLayout
{
friend class MSActionButton;
public:  
  MSActionBox(MSWidget *,const char *title_=0);
  MSActionBox(MSWidget *,const MSStringVector&);
  ~MSActionBox(void);

  const MSActionButton *activeButton(void) const;
  MSActionButton *activeButton(void);

  const MSActionButton *button(const MSSymbol&) const;
  MSActionButton *button(const MSSymbol&);

  const MSActionButton *button(int) const;
  MSActionButton *button(int);

  MSSymbolVector symbolicState(void) const;
  MSIntVector integerState(void) const;

  unsigned alignment(void) const;
  void alignment(unsigned);

  virtual void activate(void);

  virtual const MSSymbol& widgetType(void) const;

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);
  
protected:
  MSActionButton *_activeButton;

  void init(void);
  virtual void placement(void);
  virtual void activate(MSActionButton *);
  virtual void updateBackground(unsigned long);
  virtual void updateFont(Font);
  virtual void childDestroy(MSWidget *);
  
  static const MSSymbol& symbol(void);
};

inline MSActionButton *MSActionBox::activeButton(void)
{ return _activeButton; }
inline const MSActionButton *MSActionBox::activeButton(void) const
{ return _activeButton; }

#endif












