#ifndef MSArrayViewINLINES
#define MSArrayViewINLINES

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

INLINELINKAGE int MSArrayView::rowSeparator(void) const                        
{ return _rowSeparator; }
INLINELINKAGE int MSArrayView::columnSeparator(void) const                     
{ return _columnSeparator; }
INLINELINKAGE int MSArrayView::fixedColumns(void) const                             
{ return _fixedColumns; }
INLINELINKAGE int MSArrayView::columnWidth(void) const                         
{ return _columnWidth; } 
INLINELINKAGE int MSArrayView::selectedColumn(void) const                      
{ return _selectedColumn; }
INLINELINKAGE unsigned long MSArrayView::selectedCellBackground(void) const       
{ return _selectBg; }   
INLINELINKAGE MSBoolean MSArrayView::needRowSep(int row_) 
{ if (rowSeparator()>0) 
     return MSBoolean((row_%rowSeparator())==(rowSeparator()-1)||row_==numRows()-1);
  else return MSFalse;
}
INLINELINKAGE MSBoolean MSArrayView::needColSep(int col_)
{ if (columnSeparator()>0)
     return MSBoolean((col_%columnSeparator())==(columnSeparator()-1)||col_==numColumns()-1);
  else return MSFalse;
}

#endif









