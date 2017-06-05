///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSPrimitive.H>

//static const unsigned long MSPrimitiveEventMask=(ExposureMask);
static const int MSPrimitiveDefaultHighlightThickness=1;
static const int MSPrimitiveDefaultShadowThickness=2;

MSPrimitive::MSPrimitive(MSWidget *owner_,const char *title_) :
MSWidgetCommon(owner_,title_) 
{ init(); }

MSPrimitive::MSPrimitive(MSWidget *owner_,const MSStringVector& title_) :
MSWidgetCommon(owner_,title_) 
{ init(); }

MSPrimitive::~MSPrimitive(void) {}

void MSPrimitive::init(void)
{
  _highlightThickness=MSPrimitiveDefaultHighlightThickness;
  _shadowThickness=MSPrimitiveDefaultShadowThickness;
  _highlighted=MSFalse;
  shadowStyle(MSRaised);
}

void MSPrimitive::verifyBell(void) 
{ XBell(display(),0); }

void MSPrimitive::focusIn(void)      
{ highlight(); }
void MSPrimitive::focusOut(void)     
{ unHighlight(); }

void MSPrimitive::redraw(void)
{
  if (mapped()==MSTrue) 
   { 
     drawBackground(); 
     drawShadow();
   }
}

void MSPrimitive::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;
  MSBoolean open=MSTrue; 

  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if ((open=displayPrintOpen(this))==MSTrue)
      {
	fileOpen=MSTrue;
	outputMode(Print);
	displayPrintXorigin(0);
	displayPrintYorigin(0);
      }
   }
  if (open==MSTrue)
   {
     redraw();
     if (fileOpen==MSTrue) 
      {
	displayPrintClose();
	outputMode(Draw);
      }
   }
}


