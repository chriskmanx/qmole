#ifndef MSRowColumnViewINLINES
#define MSRowColumnViewINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


INLINELINKAGE MSSelectionMode MSRowColumnView::selectionMode(void) const 
{ return _selectionMode; }
INLINELINKAGE MSRowColumnView::Panner *MSRowColumnView::panner(void) const         
{ return _panner; }
INLINELINKAGE MSRowColumnView::Vsb *MSRowColumnView::vsb(void) const               
{ return _vsb; }
INLINELINKAGE MSRowColumnView::Hsb *MSRowColumnView::hsb(void) const               
{ return _hsb; }
INLINELINKAGE MSLabel *MSRowColumnView::label(void) const                
{ return _label; }
INLINELINKAGE MSRowColumnView::Editor *MSRowColumnView::editor(void) const         
{ return _editor; }
INLINELINKAGE MSBackingStorePixmap *MSRowColumnView::redrawPixmap(void) const       
{ return _redrawPixmap; }
INLINELINKAGE MSGC& MSRowColumnView::selectionVectorMSGC(void)
{ return _selectionVectorMSGC; }
INLINELINKAGE GC MSRowColumnView::selectionVectorGC(void) const          
{ return _selectionVectorMSGC.gc(); }
INLINELINKAGE unsigned long MSRowColumnView::sizeState(void) const       
{ return _sizeState; }
INLINELINKAGE unsigned long MSRowColumnView::scrollBarState(void) const  
{ return _scrollBarState; }
INLINELINKAGE MSBoolean MSRowColumnView::isVsbEnabled(void) const                  
{ return MSBoolean((VsbEnabled&scrollBarState())==VsbEnabled); }
INLINELINKAGE MSBoolean MSRowColumnView::isHsbEnabled(void) const                  
{ return MSBoolean((HsbEnabled&scrollBarState())==HsbEnabled); }    
INLINELINKAGE int MSRowColumnView::naturalRows(void) const               
{ return _naturalRows; }
INLINELINKAGE int MSRowColumnView::naturalCols(void) const               
{ return _naturalCols; }
INLINELINKAGE MSUnsignedLongVector &MSRowColumnView::cycleList(void)
{ return _cycleList; }
INLINELINKAGE MSRowColumnView::CycleTimer *MSRowColumnView::cycleTimer(void) const 
{ return _cycleTimer; }
INLINELINKAGE const MSIndexVector& MSRowColumnView::selectionVector(void) const      
{ return _selectionVector; }
INLINELINKAGE MSCycleColorMode MSRowColumnView::cycleColorMode(void) const          
{ return _cycleMode; }
INLINELINKAGE const MSUnsignedLongVector& MSRowColumnView::foregroundColors(void) const          
{ return _foregroundColors; }
INLINELINKAGE const MSUnsignedLongVector& MSRowColumnView::backgroundColors(void) const          
{ return _backgroundColors; }
INLINELINKAGE const MSUnsignedLongVector& MSRowColumnView::cycleColors(void) const          
{ return _cycleColors; }
INLINELINKAGE unsigned long MSRowColumnView::cycleInterval(void) const             
{ return _cycleInterval; }
INLINELINKAGE int MSRowColumnView::firstRow(void) const                            
{ return _firstRow; }
INLINELINKAGE int MSRowColumnView::firstColumn(void) const                         
{ return _firstColumn; }
INLINELINKAGE int MSRowColumnView::selectedRow(void) const                         
{ return _selectedRow; }
INLINELINKAGE int MSRowColumnView::rows(void) const                                
{ return _rows; }
INLINELINKAGE int MSRowColumnView::columns(void) const                             
{ return _columns; }
INLINELINKAGE int MSRowColumnView::columnSpacing(void) const                       
{ return _columnSpacing; } 
INLINELINKAGE int MSRowColumnView::rowSpacing(void) const                          
{ return _rowSpacing; } 
INLINELINKAGE int MSRowColumnView::spacing(void) const                             
{ return _spacing; } 
INLINELINKAGE unsigned long MSRowColumnView::selectedRowBackground(void) const             
{ return _rowBg; }   
INLINELINKAGE unsigned long MSRowColumnView::editorBackground(void) const          
{ return _editor->background(); }
INLINELINKAGE unsigned long MSRowColumnView::editorForeground(void) const          
{ return _editor->foreground(); }
INLINELINKAGE unsigned long MSRowColumnView::vsbBackground(void) const             
{ return _vsb->background(); }
INLINELINKAGE unsigned long MSRowColumnView::hsbBackground(void) const             
{ return _hsb->background(); }
INLINELINKAGE MSBoolean MSRowColumnView::editing(void) const                       
{ return _editor->mapped(); }
INLINELINKAGE int MSRowColumnView::scrollBarSize(void) const                       
{ return _vsb->width(); }
INLINELINKAGE int MSRowColumnView::rowHeight(void)  
{ return _rowHeight; }
INLINELINKAGE void MSRowColumnView::rowHeight(int rh_) 
{ _rowHeight=rh_; }
INLINELINKAGE int MSRowColumnView::headingsHeight(void)  
{ return _headingsHeight; }
INLINELINKAGE void MSRowColumnView::headingsHeight(int hh_)  
{ _headingsHeight=hh_; }
INLINELINKAGE int MSRowColumnView::lastBlock(void) const
{ return _lastBlock; }
INLINELINKAGE void MSRowColumnView::lastBlock(int block_)
{ _lastBlock=block_; }
INLINELINKAGE unsigned long MSRowColumnView::addEditorKeyCallback( const char* pString_,MSKeyCallback* keyCallback_)
{return editor()->addKeyCallback(pString_,keyCallback_);}
INLINELINKAGE void MSRowColumnView::removeEditorKeyCallback(unsigned long id_)
{ editor()->removeKeyCallback(id_); }
INLINELINKAGE void MSRowColumnView::removeEditorKeyCallback(const char* pString_)
{ editor()->removeKeyCallback(pString_); }
#endif









