#ifndef MSFloatArrayViewHEADER
#define MSFloatArrayViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSNumericArrayView.H>
#include <MSTypes/MSFloatVector.H>

class MSGUIExport MSFloatArrayView : public MSNumericArrayView
{
protected:
  virtual unsigned numColumns(void) const;
  virtual unsigned numRows(void) const;

  virtual const char *formatOutput(MSString &,unsigned,unsigned);
  virtual MSBoolean validate(const char *,unsigned,unsigned);
  virtual void moveRow(int, int);

  MSFloatVector& array(void)             { return (MSFloatVector&)*_model;}
  const MSFloatVector& array(void) const {return (MSFloatVector&)*_model;}  
  void array(const MSFloatVector&);
public:
  MSFloatArrayView(MSWidget *,const char *title_=0);
  MSFloatArrayView(MSWidget *,const MSStringVector&);
  MSFloatArrayView(MSWidget *,MSFloatVector&,const char *title_=0);
  MSFloatArrayView(MSWidget *,MSFloatVector&,const MSStringVector&);
  ~MSFloatArrayView(void);

  void model(MSFloatVector&);
  void model(const MSFloatVector&);

  MSFloatVector& viewModel(void)             { return *((MSFloatVector*)_model); }
  const MSFloatVector& viewModel(void) const { return *((MSFloatVector*)_model); }
  MSFloatArrayView& operator=(const MSFloatVector& array_)  {array(array_); return *this;}
  operator MSFloatVector&()                           const {return *((MSFloatVector *)_model);} 
};

typedef MSFloatArrayView MSFloatArray; // backward compatibility

#endif









