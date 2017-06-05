///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSComposite.H>

static const unsigned long MSCompositeEventMask=ExposureMask;
static const int MSCompositeDefaultShadowThickness=2;
static const int MSCompositeDefaultHighlightThickness=0;

MSComposite::MSComposite(MSWidget *owner_) : MSWidgetCommon(owner_)
{ init();}

MSComposite::~MSComposite(void) {}

void MSComposite::init(void)
{
  _shadowThickness=MSCompositeDefaultShadowThickness;
  _highlightThickness=MSCompositeDefaultHighlightThickness;
  shadowStyle(MSRaised);
  selectInput(MSCompositeEventMask); 
}

void MSComposite::redraw(void) 
{ 
  if (highlighted()==MSTrue) drawHighlight(); 
  drawShadow(); 
}

void MSComposite::focusIn(void)      
{ highlight(); }
void MSComposite::focusOut(void)     
{ unHighlight(); }

void MSComposite::configure(void) 
{ placement(); }


// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSComposite::computeSize(void) {}
void MSComposite::placement(void) {}







