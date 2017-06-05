#ifndef MSWidgetCursorHEADER
#define MSWidgetCursorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetVector.H>
#include <MSGUI/MSGUIEnum.H>

class MSGUIExport MSWidgetCursor
{
protected:
  MSWidgetVector _vector;
  unsigned       _index;
  
  MSWidgetVector &vector(void)       {return _vector;}
  unsigned index(void)         const {return _index;}

  void buildBreadthFirst(MSWidgetVector &);
  void buildDepthFirst(MSWidget *);
   
public:
  MSWidgetCursor(MSWidget *,MSWidgetIterationType type_=MSBreadthFirst);

  MSBoolean setToFirst(void);
  MSBoolean setToLast(void);
  MSBoolean setToNext(void);
  MSBoolean setToPrevious(void);
  void invalidate(void)   {_index=vector().length();}

  MSBoolean isValid(void) {return (index()==vector().length()?MSFalse:MSTrue);}
  MSWidget *widget(void)  {return (index()==vector().length()?0:_vector(index()));}
};

#endif











