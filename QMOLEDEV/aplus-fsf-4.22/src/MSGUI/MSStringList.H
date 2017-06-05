#ifndef MSStringListHEADER
#define MSStringListHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSList.H>
#include <MSTypes/MSStringVector.H>

class MSGUIExport MSStringList : public MSList
{
public:
  MSStringList(MSWidget *,const char *title_=0);
  MSStringList(MSWidget *,const MSStringVector&);
  MSStringList(MSWidget *,MSStringVector&,const char *title_=0);
  MSStringList(MSWidget *,MSStringVector&,const MSStringVector&);
  ~MSStringList(void);

  void model(MSStringVector&);
  void model(const MSStringVector&);

  MSStringVector& viewModel(void)             { return *((MSStringVector*)_model); }
  const MSStringVector& viewModel(void) const { return *((MSStringVector*)_model); }

  MSStringList& operator=(const MSStringVector& list_);
  operator MSStringVector&() const;

protected:
  virtual unsigned numColumns(void) const;
  virtual unsigned numRows(void) const;
  virtual void calculateMaxLength(void);
  virtual unsigned rowLength(unsigned) const;
  virtual void incrementalSearch(unsigned row_=0);
  virtual void moveRow(int,int);
  
  virtual MSBoolean validate(const char *string_,unsigned row_);
  virtual const char *formatOutput(MSString &,unsigned row_);

  MSStringVector& list(void) { return (MSStringVector&)*_model; }
  const MSStringVector& list(void) const { return (MSStringVector&)*_model; }  
  void list(const MSStringVector&);
};

inline MSStringList& MSStringList::operator=(const MSStringVector& list_)
{ list(list_); return *this; }
inline MSStringList::operator MSStringVector&() const
{ return *((MSStringVector *)_model); } 

#endif




