#ifndef MSIntArrayViewHEADER
#define MSIntArrayViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSNumericArrayView.H>
#include <MSTypes/MSIntVector.H>

class MSGUIExport MSIntArrayView : public MSNumericArrayView
{
protected:
  virtual unsigned numColumns(void) const;
  virtual unsigned numRows(void) const;

  virtual const char *formatOutput(MSString &,unsigned,unsigned);
  virtual MSBoolean validate(const char *,unsigned,unsigned);
  virtual void moveRow(int, int);

  MSIntVector& array(void)             {return (MSIntVector&)*_model;}
  const MSIntVector& array(void) const {return (MSIntVector&)*_model;}  
  void array(const MSIntVector&);
public:
  MSIntArrayView(MSWidget *,const char *title_=0);
  MSIntArrayView(MSWidget *,const MSStringVector&);
  MSIntArrayView(MSWidget *,MSIntVector&,const char *title_=0);
  MSIntArrayView(MSWidget *,MSIntVector&,const MSStringVector&);
  ~MSIntArrayView(void);

  void model(MSIntVector&);
  void model(const MSIntVector&);

  MSIntVector& viewModel(void)             { return *((MSIntVector*)_model); }
  const MSIntVector& viewModel(void) const { return *((MSIntVector*)_model); }
  MSIntArrayView& operator=(const MSIntVector& array_)       {array(array_); return *this;}
  operator MSIntVector&()                              const {return *((MSIntVector *)_model);} 
};

typedef MSIntArrayView MSIntArray; // backward compatibility

#endif









