#ifndef MSSymbolListHEADER
#define MSSymbolListHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSList.H>
#include <MSTypes/MSSymbolVector.H>

class MSGUIExport MSSymbolList : public MSList
{
public:
  MSSymbolList(MSWidget *,const char *title_=0);
  MSSymbolList(MSWidget *,const MSStringVector&);
  MSSymbolList(MSWidget *,MSSymbolVector&,const char *title_=0);
  MSSymbolList(MSWidget *,MSSymbolVector&,const MSStringVector&);
  ~MSSymbolList(void);

  void model(MSSymbolVector&);
  void model(const MSSymbolVector&);

  MSSymbolVector& viewModel(void)             { return *((MSSymbolVector*)_model); }
  const MSSymbolVector& viewModel(void) const { return *((MSSymbolVector*)_model); }

  MSSymbolList& operator=(const MSSymbolVector&);
  operator MSSymbolVector&() const;

protected:
  virtual unsigned numColumns(void) const;
  virtual unsigned numRows(void) const;
  virtual void calculateMaxLength(void);
  virtual unsigned rowLength(unsigned) const;
  virtual void incrementalSearch(unsigned row_=0);
  virtual void moveRow(int,int);

  virtual MSBoolean validate(const char *pString_,unsigned row_);
  virtual const char *formatOutput(MSString &,unsigned row_);

  MSSymbolVector& list(void) { return *((MSSymbolVector *)_model); } 
  const MSSymbolVector& list(void) const { return *((MSSymbolVector *)_model); }   
  void list(const MSSymbolVector&);
};

inline MSSymbolList& MSSymbolList::operator=(const MSSymbolVector& list_)
{ list(list_); return *this; }
inline MSSymbolList::operator MSSymbolVector&() const
{ return *((MSSymbolVector *)_model); } 

#endif
