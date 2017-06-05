#ifndef MSWidgetCommonINLINES
#define MSWidgetCommonINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else 
#define INLINELINKAGE
#endif

INLINELINKAGE const MSStringVector& MSWidgetCommon::title(void) const
{ return _title; }
INLINELINKAGE Font MSWidgetCommon::titleFont(void) const
{ return _titleFont; }
INLINELINKAGE unsigned long MSWidgetCommon::titleForeground(void) const
{ return _titleFg; }
INLINELINKAGE unsigned long MSWidgetCommon::titleAlignment(void) const
{ return _titleAlignment; }

#endif







