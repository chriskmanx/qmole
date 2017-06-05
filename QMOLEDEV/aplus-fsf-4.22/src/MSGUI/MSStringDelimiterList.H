#ifndef MSStringDelimiterListHEADER
#define MSStringDelimiterListHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSDelimiterList.H>
#include <MSTypes/MSStringVector.H>

class MSGUIExport MSStringDelimiterList : public MSDelimiterList
{
public:
  MSStringDelimiterList(MSWidget *,const char *title_=0);
  MSStringDelimiterList(MSWidget *,const MSStringVector&);
  MSStringDelimiterList(MSWidget *,MSStringVector&,const char *title_=0);
  MSStringDelimiterList(MSWidget *,MSStringVector&,const MSStringVector&);
  ~MSStringDelimiterList(void);

  void model(MSStringVector&);

  MSStringVector& viewModel(void)             { return *((MSStringVector*)_model); }
  const MSStringVector& viewModel(void) const { return *((MSStringVector*)_model); }

  MSStringDelimiterList& operator=(const MSStringVector& list_);
  operator MSStringVector&() const;

protected:
  virtual unsigned numColumns(void) const;
  virtual unsigned numRows(void) const;
  virtual void calculateMaxLength(void);
  virtual unsigned rowLength(unsigned) const;
  virtual void incrementalSearch(unsigned row_=0);

  virtual MSBoolean validate(const char *string_,unsigned row_);
  virtual const char *formatOutput(MSString &,unsigned row_);

  void moveRow(int from_,int to_);

  MSStringVector& list(void) { return (MSStringVector&)*_model; }
  const MSStringVector& list(void) const { return (MSStringVector&)*_model; }  
  void list(const MSStringVector&);
};

inline MSStringDelimiterList& MSStringDelimiterList::operator=(const MSStringVector& list_)
{ list(list_); return *this; }
inline MSStringDelimiterList ::operator MSStringVector&() const
{ return *((MSStringVector *)_model); } 

#endif




