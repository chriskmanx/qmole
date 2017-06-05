#ifndef MSTableINLINES
#define MSTableINLINES

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

INLINELINKAGE Window MSTable::dragWindow(void)             
{ return _dragWindow; }
INLINELINKAGE MSDisplayCursor *MSTable::dragCursor(void)   
{ return _dragCursor; }
INLINELINKAGE MSDisplayCursor *MSTable::resizeCursor(void) 
{ return _resizeCursor; }

INLINELINKAGE int MSTable::columnCount(void) const 
{ return columnList()->count(); }
INLINELINKAGE MSBoolean MSTable::showBreaks(void) const 
{ return _showBreaks; }
INLINELINKAGE MSBoolean MSTable::dynamicRecompute(void) const 
{ return _dynamicRecompute; }


INLINELINKAGE Font MSTable::headingFont(void) const
{ return _headingFont;}
INLINELINKAGE unsigned long MSTable::headingAlignment(void) const
{ return _headingAlignment;}
INLINELINKAGE MSBoolean MSTable::columnDragDrop(void) const 
{ return _columnDragDrop; }
INLINELINKAGE void MSTable::columnDragDrop(MSBoolean columnDragDrop_) 
{ _columnDragDrop=columnDragDrop_; }
INLINELINKAGE MSBoolean MSTable::columnResize(void) const
{ return _columnResize; }
INLINELINKAGE const MSIndexVector& MSTable::viewVector(void) const {return _viewVector;}

INLINELINKAGE MSIntVector &MSTable::groupHeadingsHeightVector(void) {return _groupHeadingsHeightVector;}
INLINELINKAGE const MSIntVector &MSTable::groupHeadingsHeightVector(void) const {return _groupHeadingsHeightVector;}

INLINELINKAGE MSTable::ColumnGroupList &MSTable::columnGroupList(void) {return _columnGroupList;}
INLINELINKAGE const MSTable::ColumnGroupList &MSTable::columnGroupList(void) const {return _columnGroupList;}

INLINELINKAGE int MSTable::columnHeadingsHeight(void) const
{ return _columnHeadingsHeight; }
INLINELINKAGE void MSTable::columnHeadingsHeight(int columnHeadingsHeight_)
{ _columnHeadingsHeight=columnHeadingsHeight_; }

INLINELINKAGE int MSTable::groupHeadingsHeight(void) const
{ return _groupHeadingsHeight; }
INLINELINKAGE void MSTable::groupHeadingsHeight(int groupHeadingsHeight_)
{ _groupHeadingsHeight=groupHeadingsHeight_; }

#endif









