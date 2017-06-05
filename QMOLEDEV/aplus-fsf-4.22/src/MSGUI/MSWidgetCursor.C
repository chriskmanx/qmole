///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSWidgetCursor.H>

#ifndef MSWidgetHEADER
#include <MSGUI/MSWidget.H>
#endif

MSWidgetCursor::MSWidgetCursor(MSWidget *pWidget_,MSWidgetIterationType type_)
{
  _index=0;
  if (pWidget_!=0)
   {
     if (type_==MSBreadthFirst) 
      {
	MSWidgetVector vector(pWidget_);
	buildBreadthFirst(vector);
      }
     else buildDepthFirst(pWidget_);
   }
}

void MSWidgetCursor::buildBreadthFirst(MSWidgetVector &aWidgetVector_)
{
  unsigned i,len=aWidgetVector_.length();
  vector().append(aWidgetVector_);
  MSWidgetVector breadthVector;
  for (i=0;i<len;i++) breadthVector.append(aWidgetVector_(i)->children());
  if (breadthVector.length()>0) buildBreadthFirst(breadthVector);
}

void MSWidgetCursor::buildDepthFirst(MSWidget *pWidget_)
{
  MSWidgetVector childVector(pWidget_->children());
  unsigned len=childVector.length();
  for (unsigned i=0;i<len;i++) buildDepthFirst(childVector(i));
  vector().append(pWidget_);
}

MSBoolean MSWidgetCursor::setToFirst(void)
{
  _index=0;
  if (vector().length()>0) return MSTrue;
  else return MSFalse;
}

MSBoolean MSWidgetCursor::setToLast(void)
{
  if (vector().length()>0)
   {
     _index=vector().length()-1;
     return MSTrue;
   }
  else
   {
     invalidate();
     return MSFalse;
   }
}

MSBoolean MSWidgetCursor::setToNext(void)
{
  if (++_index<vector().length()) return MSTrue;
  else
   {
     invalidate();
     return MSFalse;
   }
}

MSBoolean MSWidgetCursor::setToPrevious(void)
{
  if (_index==0) 
   {
     invalidate();
     return MSFalse;
   }
  else
   {
     _index--;
     return MSTrue;
   }
}
