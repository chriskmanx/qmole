#ifndef MSNumericArrayViewHEADER
#define MSNumericArrayViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSFormat.H>
#include <MSGUI/MSArrayView.H>

class MSString;

class MSGUIExport MSNumericArrayView : public MSArrayView
{
public:
  MSNumericArrayView(MSWidget *,const char *title_=0);
  MSNumericArrayView(MSWidget *,const MSStringVector&);
  ~MSNumericArrayView(void);

  const MSFormat& format(void) const;
  void format(const MSFormat&);
  
  const char *formatValue(MSString &,double);
  const char *formatValue(MSString &,int);

protected:
  virtual void updateFormat(void);

  MSClipMode columnClipMode(unsigned) const;
  
private:
  MSFormat  _format;   
};

inline const MSFormat& MSNumericArrayView::format(void) const 
{ return _format; }       
#endif









