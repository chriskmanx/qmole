///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <string.h>
#include <MSTypes/MSUtil.H>
#include <MSGUI/MSBusy.H>
#include <MSGUI/MSGC.H>
#include <MSGUI/MSFontObject.H>
#include <MSGUI/MSRowColumnView.H>
#include <MSGUI/MSKeyClassCallback.H>

static const unsigned DefaultScrollBarSize=15;
static const char *DefaultSelectedRowBackground="lightsteelblue3";
static const unsigned long MSRowColumnViewDefaultCycleInterval=1000;
static const unsigned long MSRowColumnViewEventMask=(ButtonPressMask|ButtonReleaseMask|ExposureMask);
static const int MSRowColumnViewDefaultRowSpacing=2;
static const int MSRowColumnViewDefaultColumnSpacing=2;

#ifdef MS_NO_INLINES
#include <MSGUI/MSColumnViewInlines.C>
#endif

typedef void (MSRowColumnView::*PMFV)(void);
typedef struct 
{
  char *_pString;
  PMFV  _func;
} Translations;

typedef MSKeyClassCallback<MSRowColumnView> KT; 

static Translations KeyTable[]=
{ 
 { "!<Key>F29",                      &MSRowColumnView::pageUp },
 { "Ctrl<Key>u",                     &MSRowColumnView::pageUp },
 { "<Key>Prior",                     &MSRowColumnView::pageUp },
 { "!<Key>F35",                      &MSRowColumnView::pageDown },
 { "Ctrl<Key>d",                     &MSRowColumnView::pageDown },
 { "<Key>Next",                      &MSRowColumnView::pageDown },
 { "!<Key>F27",                      &MSRowColumnView::home },
 { "<Key>Home",                      &MSRowColumnView::home },
 { "!<Key>R13",                      &MSRowColumnView::end },
 { "<Key>End",                       &MSRowColumnView::end },
 { "<Key>Up",                        &MSRowColumnView::up },
 { "Ctrl<Key>p",                     &MSRowColumnView::up },
 { "!<Key>Down",                     &MSRowColumnView::down },
 { "Ctrl<Key>n",                     &MSRowColumnView::down },
 { "<Key>Right",                     &MSRowColumnView::right },
 { "Ctrl<Key>f",                     &MSRowColumnView::right },
 { "<Key>Left",                      &MSRowColumnView::left },
 { "Ctrl<Key>b",                     &MSRowColumnView::left },
 { "<Key>KP_Add",                    &MSRowColumnView::increment },
 { "<Key>KP_Subtract",               &MSRowColumnView::decrement },
 { "<Key>F24",                       &MSRowColumnView::decrement },
 { "~Ctrl~Shift Meta<Key>Insert",    &MSRowColumnView::insertBelow },
 { "~Ctrl~Shift Meta<Key>KP_0",      &MSRowColumnView::insertBelow },
 { "~Ctrl Shift Meta<Key>Insert",    &MSRowColumnView::insertAbove },
 { "~Ctrl Shift Meta<Key>KP_0",      &MSRowColumnView::insertAbove },
 { "<Key>Insert",                    &MSRowColumnView::edit },
 { "~Ctrl~Shift Meta<Key>Delete",    &MSRowColumnView::deleteKey },
 { "~Ctrl~Shift Meta<Key>KP_Decimal",&MSRowColumnView::deleteKey },
 { "<Key>BackSpace",                 &MSRowColumnView::backspace },
 { "~Meta~Ctrl~Shift<Key>Tab",       &MSRowColumnView::tab },
 { "~Meta~Ctrl Shift<Key>Tab",       &MSRowColumnView::shiftTab },
 { "<Key>Return",                    &MSRowColumnView::returnKey },    
 { "<Key>KP_Enter",                  &MSRowColumnView::returnKey },    
 { 0,0 }
};

MSRowColumnView::Editor::Editor(MSWidget *owner_) : MSTextField(owner_)
{
  _highlightThickness=0;
  _shadowThickness=2;
  margin(0);  
  color(owner()->background(),owner()->foreground());
}

MSRowColumnView::Editor::~Editor(void) {}

void MSRowColumnView::Editor::up(void)       { ((MSRowColumnView *)owner())->up(); }
void MSRowColumnView::Editor::down(void)     { ((MSRowColumnView *)owner())->down(); }
void MSRowColumnView::Editor::tab(void)      { ((MSRowColumnView *)owner())->tab(); }
void MSRowColumnView::Editor::shiftTab(void) { ((MSRowColumnView *)owner())->shiftTab(); }
void MSRowColumnView::Editor::activate(void) { ((MSRowColumnView *)owner())->editorActivate(); }
void MSRowColumnView::Editor::escape(void)   { ((MSRowColumnView *)owner())->unmapEditor(); }

MSRowColumnView::Hsb::Hsb(MSWidget *owner_) : MSHScrollBar(owner_)
{
  inc(1);  
  pageInc(1);
  _highlightThickness=0;
  acceptFocus(MSFalse);
  height(DefaultScrollBarSize);
}

MSRowColumnView::Vsb::Vsb(MSWidget *owner_) : MSVScrollBar(owner_)
{
  inc(1);  
  _highlightThickness=0;
  acceptFocus(MSFalse);
  width(DefaultScrollBarSize);
}

void MSRowColumnView::Hsb::change(void) 
{ ((MSRowColumnView *)owner())->hsbValueUpdate(); }

void MSRowColumnView::Vsb::change(void) 
{ ((MSRowColumnView *)owner())->vsbValueUpdate(); }

MSRowColumnView::Panner::Panner(MSWidget *owner_) : MSPrimitive(owner_)
{
  _shadowThickness=2;
  _highlightThickness=0;
  backingStore(WhenMapped);
  shadowStyle(MSEtchedIn);
}

MSRowColumnView::Panner::~Panner(void) {}

void MSRowColumnView::Panner::redraw(void)
{ drawShadow(); }

void MSRowColumnView::Panner::expose(const XEvent *pEvent_)
{
  if (pEvent_->xexpose.count==0)
   {
     XEvent aEvent;
     while (XCheckWindowEvent(display(),window(),ExposureMask,&aEvent)==True);
     ((MSRowColumnView *)owner())->redrawImmediately();
   }
}

MSRowColumnView::CycleTimer::CycleTimer(MSRowColumnView *array_,unsigned long ival_):
MSIntervalTimer(ival_)
{ _array=array_; }

MSRowColumnView::CycleTimer::~CycleTimer(void) {}

void MSRowColumnView::CycleTimer::process(void)
{ _array->processCycleTimer(); }

MSRowColumnView::MSRowColumnView(MSWidget *owner_,const char *title_) : MSCompositeText(owner_) 
{
  _label=0;
  _label=new MSLabel(this,title_);
  init(); 
}
MSRowColumnView::MSRowColumnView(MSWidget *owner_,const MSStringVector& title_) : MSCompositeText(owner_) 
{
  _label=0;
  _label=new MSLabel(this,title_);
  init(); 
}

MSRowColumnView::~MSRowColumnView(void)
{
  // delete all created and stored objects 
  selectInput(); // do not want expose events when destroying subwindows
  removeAllCycles();
  if (dragRowWindow()!=0)       XDestroyWindow(display(),dragRowWindow());
  if (dragRowCursor()!=0)       delete _dragRowCursor;
  if (vsb()!=0)                 safeDestroy(_vsb);
  if (hsb()!=0)                 safeDestroy(_hsb);
  if (panner()!=0)              safeDestroy(_panner);
  if (label()!=0)               safeDestroy(_label);
  if (editor()!=0)              safeDestroy(_editor);
  if (redrawPixmap()!=0)        delete _redrawPixmap;
  if (cycleTimer()!=0)          delete _cycleTimer;
}

void MSRowColumnView::init(void)
{
  //Widget attributes
  _shadowThickness=0;    
  _highlightThickness=2; 
  _acceptTab=MSTrue;

  //Set default values for attributes
  _rows=5;
  _columns=2;
  _firstRow=0;
  _firstColumn=0;
  _spacing=0;
  _selectedRow=-1;
  _selectionMode=MSSingle;
  _rowBg=server()->pixel(DefaultSelectedRowBackground);
  _cycleMode=MSForeground;
  _cycleInterval=MSRowColumnViewDefaultCycleInterval;

  //Set implementation data member
  _hsb=0;
  _vsb=0;
  _panner=0;
  _editor=0;
  _redrawPixmap=0;
  _naturalRows=5;
  _naturalCols=2;
  _rowSpacing=MSRowColumnViewDefaultRowSpacing;
  _columnSpacing=MSRowColumnViewDefaultColumnSpacing;  
  _sizeState=Invalid;
  _scrollBarState=VsbEnabled|HsbEnabled;
  _cycleTimer=0;
  _rowHeight=textHeight()+(2*rowSpacing());
  _headingsHeight=0;
  _lastBlock=-1;

  _rowDragDrop=MSFalse;
  _dragRowWindow=0;
  _dragRowCursor=0;
  
  initHsb();
  initVsb();
  _panner=new Panner(this);
  _editor=new Editor(this);
  _redrawPixmap=new MSBackingStorePixmap(server(),"MSGeneral");
  XGCValues values;    
  values.foreground=selectedRowBackground();
  _selectionVectorMSGC.setGCValues(server(),MSTrue,&values,GCForeground);
  
  _title=label()->label();
  label()->dynamic(MSTrue);
  if (label()->columns()>0) label()->map();
  panner()->map();

  initKeyTranslations();
  selectInput(MSRowColumnViewEventMask);
  freeze();
  addToFocusList();
}

void MSRowColumnView::initKeyTranslations(void)
{
  if (MSKeyTranslationTable::keyTableData("MSRowColumnView")==MSFalse)
   {
     keyTranslationTable()->addKeyTableData("MSRowColumnView");
     int i=0;
     KT *entry=0;
     while (KeyTable[i]._pString!=0)
      {
	entry = new KT(KeyTable[i]._func );
	keyTranslationTable()->addCallback(KeyTable[i]._pString,entry,"MSRowColumnView");
	i++;
      }
   }
  else keyTranslationTable()->addKeyTableData("MSRowColumnView");
}

void MSRowColumnView::initHsb(void) 
{ if (hsb()==0) _hsb=new Hsb(this); }

void MSRowColumnView::initVsb(void) 
{ if (vsb()==0) _vsb=new Vsb(this); }


// All the Translated Key Methods
void MSRowColumnView::pageUp(void)
{
  if (firstRow()!=0)
   {
     clearSelection();
     firstRow(firstRow()-(rows()-1));
     if (selectionMode()==MSMultiple) 
      {
	lastBlock(firstRow()+rows()-1);
	_selectionVector.append(firstRow()+rows()-1);
      }
     selectedRow(firstRow()+rows()-1);
   }
}
void MSRowColumnView::pageDown(void)
{
  if (rows()<numRows()&&firstRow()<(numRows()-rows()))
   {
     firstRow(firstRow()+(rows()-1));
     clearSelection();
     if (selectionMode()==MSMultiple) 
      {
	lastBlock(firstRow());
	_selectionVector.append(firstRow());
      }
     selectedRow(firstRow());
   }
}
void MSRowColumnView::home(void)
{
  clearSelection();
  if (firstRow()!=0) firstRow(0);
  if (selectionMode()==MSMultiple) 
   {
     lastBlock(0);
     _selectionVector.append(0);
   }
  selectedRow(0);
}
void MSRowColumnView::end(void)
{
  if (firstRow()!=(numRows()-rows())) firstRow(numRows()-rows());
  clearSelection();
  if (selectionMode()==MSMultiple) 
   {
     lastBlock(numRows()-1);
     _selectionVector.append(numRows()-1);
   }
  selectedRow(numRows()-1);
}
void MSRowColumnView::up(void)
{ 
  if (editorActivate()==MSTrue) 
   {
     if (selectedRow()>0) 
      {
	int newRow=selectedRow()-1;
	clearSelection();
	if (selectionMode()==MSMultiple) 
	 {
	   lastBlock(newRow);
	   _selectionVector.append(newRow);
	 }
	selectedRow(newRow);
      }
   }
}
void MSRowColumnView::down(void)
{ 
  if (editorActivate()==MSTrue) 
   {
     if (selectedRow()<numRows()-1) 
      {
	int newRow=selectedRow()+1;
	clearSelection();
	if (selectionMode()==MSMultiple) 
	 {
	   lastBlock(newRow);
	   _selectionVector.append(newRow);
	 }
	selectedRow(newRow);
      }
   }
}
void MSRowColumnView::left(void)
{
  if (editorActivate()==MSTrue) 
   {
     if (firstColumn()>0) 
      {
	clearSelection();
	if (selectionMode()==MSMultiple) 
	 {
	   lastBlock(selectedRow());
	   _selectionVector.append(selectedRow());
	 }
	firstColumn(firstColumn()-1);
      }
   }
}
void MSRowColumnView::right(void)
{
  if (editorActivate()==MSTrue) 
   { 
     if (firstColumn()+columns()-1<numColumns()-1)
      {
	clearSelection();
	if (selectionMode()==MSMultiple) 
	 {
	   lastBlock(selectedRow());
	   _selectionVector.append(selectedRow());
	 }
	firstColumn(firstColumn()+1);
      }
   }
}
void MSRowColumnView::increment(void) {}
void MSRowColumnView::decrement(void) {}
void MSRowColumnView::tab(void) {}
void MSRowColumnView::shiftTab(void) {}
void MSRowColumnView::returnKey(void) { doubleClick(); }
void MSRowColumnView::backspace(void)
{
  if (editor()->mapped()==MSFalse)
   {
     editor()->string("");
     moveEditorToSelection(MSString());
   }
}
void MSRowColumnView::doubleClick(void) {}
void MSRowColumnView::insertBelow(void) { activateCallback(MSWidgetCallback::insertbelow); }
void MSRowColumnView::insertAbove(void) { activateCallback(MSWidgetCallback::insertabove); }
void MSRowColumnView::deleteKey(void)   { activateCallback(MSWidgetCallback::deleterow); }
void MSRowColumnView::edit(void)
{
  if (editor()->mapped()==MSFalse) 
   {
     editor()->editMode(MSTextField::InsertMode);
     moveEditorToSelection(selection());
   }
}

// End of Translated Key Methods

MSBoolean MSRowColumnView::editorActivate(void) {return MSFalse;}

MSString MSRowColumnView::selection(void) {return MSString();}

void MSRowColumnView::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;
  MSBoolean open=MSTrue; 

  MSApplicationBusy busy;
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
     clear();
     if (label()->mapped()==MSTrue) 
      {
	displayPrintOriginInc(label());
	label()->print();
	displayPrintOriginDec(label());
      }
     displayPrintOriginInc(panner());
     redrawImmediately();
     displayPrintOriginDec(panner());
     
     if (fileOpen==MSTrue) 
      {
	displayPrintClose();
	outputMode(Draw);
      }
   }
}

void MSRowColumnView::expose(const XEvent *pEvent_)
{
  if (pEvent_->xexpose.count==0)
   {
     XEvent aEvent;
     while (XCheckWindowEvent(display(),window(),ExposureMask,&aEvent)==True);
     if (highlighted()==MSTrue) drawHighlight();
   }
}

void MSRowColumnView::redraw(void)
{ redrawImmediately(); }

void MSRowColumnView::naturalSize(void)
{
  freeze();
  _rows=naturalRows();
  _columns=naturalCols();
  _firstRow=0;
  _firstColumn=0;
  adjustFirstRow();
  adjustFirstColumn();
  computeSize();
  updateHsb();
  updateVsb();
  unfreeze();
}

void MSRowColumnView::unfreeze(void) 
{
  updateInternalState();
  if (frozen()==MSTrue) 
   { 
     freezeStatus(MSFalse); 
     calculateRowHeight();
     calculateHeadingsHeight();
     adjustNumVisible();
     redrawImmediately(); 
   } 
}

void MSRowColumnView::redrawImmediately(void)
{}
void MSRowColumnView::configure(void) 
{
  _redrawPixmap->resize(width(),height());
  placement();
  if ((sizeState()&AdjustSize)==AdjustSize) adjustView();
}

void MSRowColumnView::computeSize(void)
{
  if (editor()!=0&&vsb()!=0&&hsb()!=0&&label()!=0)
   {
     int offset=((highlightThickness()+shadowThickness())<<1);
     int poffset=((panner()->highlightThickness()+panner()->shadowThickness())<<1);
     int w=drawWidth()+poffset; 
     int h=drawHeight()+headingsHeight()+poffset;
     
     if (label()->mapped()==MSTrue) h+=label()->height();
     if (isHsbEnabled()==MSTrue) h+=(hsb()->height()+spacing());
     if (isVsbEnabled()==MSTrue) w+=(vsb()->width()+spacing());
     resize(w+offset,h+offset);
   }
}

void MSRowColumnView::placement(void)
{
  if (editor()!=0&&vsb()!=0&&hsb()!=0&&label()!=0) 
   {
     if (editor()->mapped()==MSTrue) unmapEditor();
     
     int offset=highlightThickness()+shadowThickness();
     int offset2=offset<<1;
     int pH=height()-offset2;
     int pW=width()-offset2;
     
     if (label()->mapped()==MSTrue) pH-=label()->height();
     if (isVsbEnabled()==MSTrue) pW-=(vsb()->width()+spacing());
     if (isHsbEnabled()==MSTrue) pH-=(hsb()->height()+spacing());

     label()->moveTo(offset,offset);
     label()->width(width()-offset2);
     editor()->height(rowHeight());
     
     int y=offset;
     if (label()->mapped()==MSTrue) y+=label()->height();

     panner()->moveTo(offset,y);
     panner()->resize(pW,pH);

     offset=panner()->highlightThickness()+panner()->shadowThickness();
     offset2=offset<<1;
     XRectangle clipRect[1];
     clipRect[0].x=0;
     clipRect[0].y=0;
     clipRect[0].width=panner()->width()-offset2;
     clipRect[0].height=panner()->height()-offset2;
     XSetClipRectangles(display(),textGC(),offset,offset,&clipRect[0],1,Unsorted);

     adjustNumVisible();
   }
}

void MSRowColumnView::updateData(void)
{
  if (firstMap()==MSTrue)
   {
     if (editor()->mapped()==MSTrue) unmapEditor();
     updateInternalState();
     calculateRowHeight();
     calculateHeadingsHeight();
     adjustNumVisible();
     redrawImmediately();
   }
}

void MSRowColumnView::adjustSize(void) 
{
  //This method is called when resize is necessary
  computeSize();
}

void MSRowColumnView::firstMapNotify(void)
{
  freezeStatus(MSTrue);
  _sizeState|=AdjustSize;
  updateInternalState();
  defaultNumVisible();
  calculateRowHeight();
  calculateHeadingsHeight();
  computeSize();
  adjustNumVisible();
  freezeStatus(MSFalse);
  _naturalRows=rows();
  _naturalCols=columns();
}

void MSRowColumnView::calculateRowHeight(void)
{
  rowHeight(textHeight()+(2*rowSpacing()));
}

void MSRowColumnView::calculateHeadingsHeight(void)
{
  headingsHeight(0);
}

void MSRowColumnView::updateBackground(unsigned long oldbg_)
{ 
  MSCompositeText::updateBackground(oldbg_);
  label()->background(background()); 
  vsbBackground(background());
  hsbBackground(background());
  if (panner()->background()!=background()) 
   { 
     panner()->background(background());
     redrawImmediately();
   }
}

void MSRowColumnView::updateForeground(unsigned long oldfg_)
{ 
  MSCompositeText::updateForeground(oldfg_);
  if (label()->foreground()==oldfg_) label()->foreground(foreground());
  redrawImmediately(); 
}

void MSRowColumnView::updateFont(Font oldfid_)
{ 
  MSCompositeText::updateFont(oldfid_);
  _rowHeight=textHeight()+(2*rowSpacing());
  adjustNumVisible();
  redrawImmediately(); 
}

void MSRowColumnView::updateTitle(void)
{
  label()->freeze();
  label()->foreground(titleForeground());
  label()->alignment(titleAlignment());
  
  int h=label()->height();
  int w=label()->width();
  MSBoolean doPlace=MSFalse;

  label()->label(title());
  if (label()->columns()==0&&label()->mapped()==MSTrue)
   {
     label()->unmap();
     doPlace=MSTrue;
   }
  else if (label()->columns()>0&&label()->mapped()==MSFalse)
   {
     label()->map();
     doPlace=MSTrue;
   }
  else if (label()->font()!=titleFont()&&label()->mapped()==MSTrue)
  {
     label()->font(titleFont());
     doPlace=MSTrue;
  }
  int offset=highlightThickness()+shadowThickness();
  label()->moveTo(offset,offset);

  if (label()->height()!=h||doPlace==MSTrue)
   {
     label()->unfreeze();
     placement();
   }
  else
   {
     label()->width(width()-2*offset);
     label()->unfreeze();
     if (label()->mapped()==MSTrue) label()->redraw();
   }
}

void MSRowColumnView::removeAllCycles(void)
{
   MSColorCycle *cycle;
   for (unsigned i=0;i<cycleList().length();i++)
   {
      cycle=(MSColorCycle *)cycleList()(i);
      delete cycle;
   }
   cycleList().removeAll();
   if (cycleTimer()!=0) cycleTimer()->stop();
}
void MSRowColumnView::removeCycle(MSColorCycle *cycle_)
{
  unsigned index;
  if ((index=cycleList().indexOf((unsigned long)cycle_))!=cycleList().length())
  {
     delete cycle_;
     cycleList().removeAt(index);
  }
  if (cycleList().length()==0&&cycleTimer()!=0) cycleTimer()->stop();
}

void MSRowColumnView::startCycle(int row_,int column_,const MSUnsignedLongVector& colors_,MSCycleColorMode mode_)
{
  MSColorCycle *cycle=new MSColorCycle(this,row_,column_,colors_,mode_);
  cycleCell(cycle);
  cycle->increment();
  cycleList().append((unsigned long)cycle);
}

void MSRowColumnView::processCycleTimer(void)
{
   MSColorCycle *cycle;
   MSIndexVector deleteList;
   struct timeval diff;
   struct timeval *now=tod();
   for (unsigned i=0;i<cycleList().length();i++)
   {
      cycle=(MSColorCycle *)cycleList()(i);
      tvdiff(now,(struct timeval *)&cycle->lastUpdate(),&diff);
      unsigned long actual=diff.tv_sec*1000+diff.tv_usec/1000;
      if (actual>=cycleInterval())
       {
	 cycleCell(cycle);
	 if (cycle->count()==cycle->numCycles()) 
	  {
	    deleteList.append(i);
	    delete cycle;
	  }
	 else
	  {
	    cycle->lastUpdate(*now);
	    cycle->increment();
	  }
       }
    }
   cycleList().remove(deleteList);
   if (cycleList().length()==0&&cycleTimer()!=0) cycleTimer()->stop();
}


void MSRowColumnView::updateFirstRow(int firstRow_)
{ _firstRow=firstRow_; redraw(); }
void MSRowColumnView::updateFirstColumn(int firstColumn_)
{ _firstColumn=firstColumn_; redraw(); }
void MSRowColumnView::updateSelectionVector(const MSIndexVector& selectionVector_)
{ _selectionVector=selectionVector_; redraw(); }
void MSRowColumnView::updateSelectedRow(int)
{}

void MSRowColumnView::updateSelectionMode(MSSelectionMode selectionMode_)
{
  //we only support MSSingle && MSMultiple
  //subclasses will have to overide that to support other modes.
  if (selectionMode()!=selectionMode_ &&
      (selectionMode_==MSMultiple || selectionMode_==MSSingle))
   {
     _selectionMode=selectionMode_;
     if (_selectionMode==MSMultiple)
      {
	lastBlock(selectedRow());
	if (selectedRow()>=0) _selectionVector.append(selectedRow());
      }
     else clearSelection();
   }
}

void MSRowColumnView::keyPress(const XEvent *e_,KeySym k_,unsigned int s_,const char *b_)
{
  if (isProtected()==MSFalse)
   {
     if (editor()->mapped()==MSTrue) keyPressNotify(editor(),e_,k_,s_,b_);
     else
      {
	MSKeyPress key(k_,s_);
	if( keyTranslate(key) == MSFalse)
         {
           editor()->string("");
           keyPressNotify(editor(),e_,k_,s_,b_);
           if (editor()->length()>0) moveEditorToSelection(editor()->text());
         }
      }
   }
}

void MSRowColumnView::buttonPress(const XEvent *pEvent_)
{ if (sensitive()==MSTrue) defaultButtonBehavior(pEvent_);}

void MSRowColumnView::buttonRelease(const XEvent *pEvent_)
{ if (sensitive()==MSTrue) MSCompositeText::buttonRelease(pEvent_); }

void MSRowColumnView::defaultDoubleClickBehavior(const XEvent *pEvent_)
{
  if (hasCallback(MSWidgetCallback::doubleclick)==MSTrue) doubleClick();
  else startEditing(pEvent_);
}

void MSRowColumnView::defaultButtonBehavior(const XEvent *pEvent_)
{
  if (pEvent_->xbutton.button==Button1)      defaultButton1Behavior(pEvent_);
  else if (pEvent_->xbutton.button==Button2) defaultButton2Behavior(pEvent_);
  else if (pEvent_->xbutton.button==Button3) defaultButton3Behavior(pEvent_);
}

void MSRowColumnView::defaultButton1Behavior(const XEvent *)
{}

void MSRowColumnView::defaultButton2Behavior(const XEvent *pEvent_)
{
  if (rowDragDrop()==MSTrue) dragRow(pEvent_);
  else startEditing(pEvent_);
}

void MSRowColumnView::defaultButton3Behavior(const XEvent *)
{}

MSBoolean MSRowColumnView::selected(unsigned row_)
{
  MSBoolean sel=MSFalse;
  if (selectionMode()!=MSSingle)
   {
     if (selectionVector().length()>0)
      {
	unsigned i=selectionVector().indexOf(row_);
	sel=(i<selectionVector().length())?MSTrue:MSFalse;
      }
   }
  else
   {
     if (selectedRow()!=-1)
      {
	sel=(row_==(unsigned)selectedRow())?MSTrue:MSFalse;
      }
   }
  return sel;
}

void MSRowColumnView::rows(int num_)   
{ 
  _sizeState|=RowsValid;
  _rows=num_;
  adjustSize(); 
}

void MSRowColumnView::columns(int num_)
{ 
  _sizeState|=ColsValid; 
  _columns=num_;
  adjustSize(); 
}

void MSRowColumnView::firstRow(int firstRow_)
{ updateFirstRow(firstRow_); }

void MSRowColumnView::firstColumn(int firstColumn_)
{ updateFirstColumn(firstColumn_); }

void MSRowColumnView::spacing(int spacing_) 
{ if (spacing_!=_spacing) { _spacing=spacing_; placement(); } }

void MSRowColumnView::selectedRow(int selectedRow_)
{ updateSelectedRow(selectedRow_);}

void MSRowColumnView::selectionMode(MSSelectionMode selectionMode_)
{ updateSelectionMode(selectionMode_); }

void MSRowColumnView::foregroundColors(const MSStringVector& colors_)
{
   MSUnsignedLongVector cols(colors_.length());
   for (int i=0;i<colors_.length();i++) cols[i]=(server()->pixel(colors_(i)));
   foregroundColors(cols);
}
void MSRowColumnView::foregroundColors(const MSUnsignedLongVector& colors_)
{
  _foregroundColors=colors_;
  redraw();
}

void MSRowColumnView::backgroundColors(const MSStringVector& colors_)
{
   MSUnsignedLongVector cols(colors_.length());
   for (int i=0;i<colors_.length();i++) cols[i]=(server()->pixel(colors_(i)));
   backgroundColors(cols);
}
void MSRowColumnView::backgroundColors(const MSUnsignedLongVector& colors_)
{
  _backgroundColors=colors_;
  redraw();
}

void MSRowColumnView::cycleColors(const MSStringVector& colors_)
{
   MSUnsignedLongVector cols(colors_.length());
   for (int i=0;i<colors_.length();i++) cols[i]=(server()->pixel(colors_(i)));
   cycleColors(cols);
}
void MSRowColumnView::cycleColors(const MSUnsignedLongVector& colors_)
{  _cycleColors=colors_; }

void MSRowColumnView::cycleColorMode(MSCycleColorMode cycleMode_)
{
  if (_cycleMode!=cycleMode_) _cycleMode=cycleMode_;
}

void MSRowColumnView::cycleInterval(unsigned long interval_)
{
  if (cycleInterval()!=interval_)
   {
     _cycleInterval=interval_;
     MSBoolean stop=(cycleList().length()==0)?MSTrue:MSFalse;
     if (cycleTimer()!=0) delete _cycleTimer;
     _cycleTimer=new CycleTimer(this,cycleInterval());
     if (stop==MSTrue) cycleTimer()->stop();
   }
}

void MSRowColumnView::scrollBarSize(int size_)
{ if (size_!=vsb()->width()) { vsb()->width(size_);hsb()->height(size_); placement(); }}

int MSRowColumnView::vsbSize(void) const
{ return (vsb()->mapped()==MSFalse&&vsb()->width()==1)?0:vsb()->width(); }
void MSRowColumnView::vsbSize(int size_) 
{ if (size_!=vsb()->width()) { vsb()->width(size_); placement(); }}

int MSRowColumnView::hsbSize(void) const
{ return (hsb()->mapped()==MSFalse&&hsb()->height()==1)?0:hsb()->height(); }
void MSRowColumnView::hsbSize(int size_) 
{ if (size_!=hsb()->height()) { hsb()->height(size_); placement();}}

void MSRowColumnView::showVsb(void)
{
  if (isVsbEnabled()==MSFalse)
   {
     _scrollBarState|=VsbEnabled;
     vsb()->map();     
     placement();
   }
}
void MSRowColumnView::hideVsb(void)
{
  if (isVsbEnabled()==MSTrue)
   {
     _scrollBarState&=~VsbEnabled;
     vsb()->unmap();
     placement();
   }
}

void MSRowColumnView::showHsb(void)
{
  if (isHsbEnabled()==MSFalse)
   {
     _scrollBarState|=HsbEnabled;
     hsb()->map();     
     placement();
   }
}
void MSRowColumnView::hideHsb(void)
{
  if (isHsbEnabled()==MSTrue)
   {
     _scrollBarState&=~HsbEnabled;
     hsb()->unmap();     
     placement();
   }
}

void MSRowColumnView::vsbBackground(const char *color_)
{ vsbBackground(server()->pixel(color_)); }
void MSRowColumnView::vsbBackground(unsigned long pixel_)    
{ vsb()->background(pixel_); }

void MSRowColumnView::hsbBackground(const char *color_)
{ hsbBackground(server()->pixel(color_)); }
void MSRowColumnView::hsbBackground(unsigned long pixel_)    
{ hsb()->background(pixel_); }

void MSRowColumnView::editorBackground(const char *color_)
{ editorBackground(server()->pixel(color_)); }
void MSRowColumnView::editorBackground(unsigned long pixel_) 
{ editor()->background(pixel_); }

void MSRowColumnView::editorForeground(const char *color_)
{ editorForeground(server()->pixel(color_)); }
void MSRowColumnView::editorForeground(unsigned long pixel_) 
{ editor()->foreground(pixel_); }

void MSRowColumnView::selectionVector(const MSIndexVector& sv_)
{ updateSelectionVector(sv_); }

void MSRowColumnView::selectedRowBackground(const char *color_)
{ selectedRowBackground(server()->pixel(color_)); }
void MSRowColumnView::selectedRowBackground(unsigned long pixel_) 
{ 
  if (selectedRowBackground()!=pixel_)
   { 
     _rowBg=pixel_;
     selectionVectorMSGC().foreground(pixel_);
     redraw();
   }
}

int MSRowColumnView::clearSelection(void)
{
  // This method clears all the rows in the selection vector except for
  // the current selected row
  if (selectionVector().length()>0)
   {
     const MSIndexVector osv=selectionVector(); // make a local copy of old select vector
     unsigned i;
     _selectionVector.removeAll();
     for (i=0;i<osv.length();i++)
      {
	unsigned index=osv(i);
	if (selectedRow()!=-1)
	 {
	   if (index!=selectedRow()) drawRow(index);
	 }
	else drawRow(index);
      }
     return (osv.length());
   }
  else return 0;
}

int MSRowColumnView::lastColumn(void) 
{ return (_columns>0)?_firstColumn+_columns-1:_firstColumn;}
int MSRowColumnView::lastRow(void)    
{ return (_rows>0)?_firstRow+_rows-1:_firstRow; }

void MSRowColumnView::updateScrollBars(void)
{
  if (firstRow()!=vsb()->value()) vsb()->valueChange(firstRow());
  if (firstColumn()!=hsb()->value()) hsb()->valueChange(firstColumn());
}

void MSRowColumnView::firstColumnChangeNotify(void) { activateCallback(MSWidgetCallback::firstcolumnchange); }
void MSRowColumnView::firstRowChangeNotify(void) { activateCallback(MSWidgetCallback::firstrowchange); }
void MSRowColumnView::rowSelection(void)  { activateCallback(MSWidgetCallback::rowselection); }

void MSRowColumnView::rowMovedNotify(void) { activateCallback(MSWidgetCallback::rowmoved); }

void MSRowColumnView::fillSelection(int from_,int to_)
{
  if (to_>=from_)
   {
     int fr=firstRow();
     int lr=lastRow();
     for (int i=fr;i<=lr;i++)
      {
	if (i>=from_&&i<=to_&&selected(i)==MSFalse) drawSelected(i);
      }
   }
}

void MSRowColumnView::unfillSelection(int from_,int to_)
{
  if (to_>=from_)
   {
     int fr=firstRow();
     int lr=lastRow();
     for (int i=fr;i<=lr;i++)
      {
	if (i>=from_&&i<=to_&&selected(i)==MSTrue) undrawSelected(i);
      }
   }
}

void MSRowColumnView::scrollLeft(int count_)
{
  if (firstColumn()>0&&count_>0)
   {
     if (firstColumn()-count_<0) count_=firstColumn();
     _firstColumn-=count_;
     redrawImmediately();
     firstColumnChangeNotify();
   }
}

void MSRowColumnView::scrollRight(int count_)
{
  int numCols=numColumns();
  if (firstColumn()+columns()<numCols&&count_>0)
   {
     if (firstColumn()+columns()+count_>numCols)
      {
        count_=numCols-(firstColumn()+columns());
      }  
     _firstColumn+=count_;
     redrawImmediately();
     firstColumnChangeNotify();
   }
}

void MSRowColumnView::scrollUp(int count_)
{ scrollUp(count_,selectedRow(),MSFalse);}

void MSRowColumnView::scrollUp(int count_,int selectedRow_,MSBoolean drawNewRow_)
{
  if (firstRow()+rows()<numRows()&&count_>0)
   {
     int oldFirstRow=firstRow();  
     if (firstRow()+rows()+count_>numRows())
      {
        count_=numRows()-(firstRow()+rows());
      }  
     if (count_>=rows()-1)
      {
        _firstRow+=count_;
        _selectedRow=selectedRow_;
	redrawImmediately();
      }
     else  
      {
        int delta=count_*rowHeight();
        int offset=panner()->highlightThickness()+panner()->shadowThickness();
        int src_x=offset;
        int dest_x=src_x;
        int dest_y=offset+headingsHeight(); 
        int src_y=dest_y+delta;
        int w=panner()->width()-2*offset;
        int h=(rows()-count_)*rowHeight();

	int oldRow=selectedRow();
	_selectedRow=selectedRow_;	   
	if (oldRow!=selectedRow()) undrawSelectedRow(oldRow);
        _firstRow+=count_;
        XCopyArea(display(),panner()->window(),panner()->window(),
		  backgroundShadowGC(),
		  src_x,src_y,w,h,dest_x,dest_y);
        int rs=firstRow()+rows()-count_;
        int re=firstRow()+rows()-1;
        postVerticalScrollDraw(rs,re,drawNewRow_);
        updateScrollBars();
     }
     if (oldFirstRow!=firstRow()) firstRowChangeNotify();  
   }
}

void MSRowColumnView::scrollDown(int count_)
{ scrollDown(count_,selectedRow(),MSFalse); }

void MSRowColumnView::scrollDown(int count_,int selectedRow_,MSBoolean drawNewRow_)
{
  if (firstRow()>0&&count_>0)
   {
     int oldFirstRow=firstRow();  
     if (firstRow()-count_<0) count_=firstRow();
     if (count_>=rows()-1)
      {
        _firstRow-=count_;
        _selectedRow=selectedRow_;
	redrawImmediately();
      }
     else  
      {
        int delta=count_*rowHeight();
        int offset=panner()->highlightThickness()+panner()->shadowThickness();
        int src_x=offset;
        int dest_x=src_x;
        int src_y=offset+headingsHeight();
        int dest_y=src_y+delta;
        int w=panner()->width()-2*offset;
        int h=(rows()-count_)*rowHeight();

	int oldRow=selectedRow();
	_selectedRow=selectedRow_;
	if (oldRow!=selectedRow()) undrawSelectedRow(oldRow);
        _firstRow-=count_;
        XCopyArea(display(),panner()->window(),panner()->window(),
		  panner()->backgroundShadowGC(),
		  src_x,src_y,w,h,dest_x,dest_y);
        int rs=firstRow();
        int re=firstRow()+count_;
        postVerticalScrollDraw(rs,re,drawNewRow_);
  	updateScrollBars();
      }
     if (oldFirstRow!=firstRow()) firstRowChangeNotify();  
   }
}

void MSRowColumnView::postVerticalScrollDraw(int rs_,int re_,MSBoolean drawNewRow_)
{
  if (drawNewRow_==MSTrue) drawRows(rs_,re_);
}

void MSRowColumnView::set(MSAttrValueList& avList_)
{
  MSCompositeText::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="rows")
      {
	rows(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="rowDragDrop")
      {
        rowDragDrop(avList_[i].value().asBoolean());
        index<<i;
      }
     else if (avList_[i].attribute()=="columns")
      {
	columns(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="selectionMode")
      {
	if (avList_[i].value()=="MSMultiple") selectionMode(MSMultiple);
        else if (avList_[i].value()=="MSToggle") selectionMode(MSToggle);
	else selectionMode(MSSingle);
      }
     else if (avList_[i].attribute()=="cycleColors")
      {
        cycleColors(MSAttrValue::stringToStringVector(avList_[i].value()));
        index<<i;
      }
     else if (avList_[i].attribute()=="cycleColorMode")
      {
	if (avList_[i].value()=="MSBackground") cycleColorMode(MSBackground);
	else if (avList_[i].value()=="MSReversevideo") cycleColorMode(MSReverseVideo);
	else cycleColorMode(MSForeground);
	index<<i;
      }
     else if (avList_[i].attribute()=="cycleInterval")
      {
	cycleInterval(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="foregroundColors")
      {
        foregroundColors(MSAttrValue::stringToStringVector(avList_[i].value()));
	index<<i;
      }     
     else if (avList_[i].attribute()=="backgroundColors")
      {
        backgroundColors(MSAttrValue::stringToStringVector(avList_[i].value()));
	index<<i;
      }     
     else if (avList_[i].attribute()=="selectedRowBackground")
      {
	selectedRowBackground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="editorBackground")
      {
	editorBackground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="editorForeground")
      {
	editorForeground(avList_[i].value());
	index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSRowColumnView::get(MSAttrValueList& avList_)
{
  MSStringVector aStringVector("MSFalse\nMSTrue");
  avList_<<MSAttrValue("rowDragDrop",aStringVector(rowDragDrop()),aStringVector);

  avList_<<MSAttrValue("rows",MSString(rows()));
  avList_<<MSAttrValue("columns",MSString(columns()));

  avList_<<MSAttrValue("selectionMode",
		       (selectionMode()==MSMultiple)?"MSMultiple":
                       (selectionMode()==MSToggle)?"MSToggle":"MSSingle",
		       MSStringVector("MSSingle\nMSMultiple\nMSToggle"));

  avList_<<MSAttrValue("cycleColors",MSAttrValue::colorVectorToString(cycleColors(),server()),
                       MSAttrValue::Color|MSAttrValue::List|MSAttrValue::StringVector);

  const char *value;
  switch (cycleColorMode())
  {
  case MSBackground:   value="MSBackground";   break;
  case MSReverseVideo: value="MSReversevideo"; break;
  case MSForeground:
  default:             value="MSForeground";   break;
  }
  avList_<<MSAttrValue("cycleColorMode",value,
		       MSStringVector("MSBackground\nMSForeground\nMSReversevideo"));

  avList_<<MSAttrValue("cycleInterval",MSString(cycleInterval()));
  avList_<<MSAttrValue("foregroundColors",MSAttrValue::colorVectorToString(foregroundColors(),server()),
                       MSAttrValue::Color|MSAttrValue::List|MSAttrValue::StringVector);
  avList_<<MSAttrValue("backgroundColors",MSAttrValue::colorVectorToString(backgroundColors(),server()),
                       MSAttrValue::Color|MSAttrValue::List|MSAttrValue::StringVector);
  avList_<<MSAttrValue("selectedRowBackground",
		       server()->colorName(selectedRowBackground()),
		       MSAttrValue::Color);
  avList_<<MSAttrValue("editorBackground",
		       server()->colorName(editorBackground()),
		       MSAttrValue::Color);
  avList_<<MSAttrValue("editorForeground",
		       server()->colorName(editorForeground()),
		       MSAttrValue::Color);
  avList_<<MSAttrValue("rowmoved","",MSAttrValue::Callback);
  avList_<<MSAttrValue("deleterow","",MSAttrValue::Callback);
  avList_<<MSAttrValue("doubleclick","",MSAttrValue::Callback);
  avList_<<MSAttrValue("editbegin","",MSAttrValue::Callback);
  avList_<<MSAttrValue("editend","",MSAttrValue::Callback);
  avList_<<MSAttrValue("firstcolumnchange","",MSAttrValue::Callback);
  avList_<<MSAttrValue("firstrowchange","",MSAttrValue::Callback);
  avList_<<MSAttrValue("insertabove","",MSAttrValue::Callback);
  avList_<<MSAttrValue("insertbelow","",MSAttrValue::Callback);
  avList_<<MSAttrValue("selection","",MSAttrValue::Callback);

  return MSCompositeText::get(avList_);
}

int MSRowColumnView::yToRow(int y_)
{
  int row;
  if (y_<0) row=0;
  else if (y_>panner()->height()) row=rows();
  else 
   {
     int h=rowHeight();
     int i=firstRow();
     while(h<=y_&&i<=lastRow())
      {
        i++;
        h+=rowHeight();
      }
     row=i-firstRow();
   }
  return (row>=0)?row:0;
}

int MSRowColumnView::inRowRange(int r_)
{ return(r_>=firstRow()&&r_<=lastRow())?MSTrue:MSFalse; }

int MSRowColumnView::rowFromEvent(const XEvent *pEvent_)
{
  int ey=pEvent_->xbutton.y;
  if (ey>headingsHeight()) return yToRow(ey-headingsHeight())+firstRow();
  return -1;
}

int MSRowColumnView::xToColumn(int)
{return 0;}

int MSRowColumnView::inColRange(int c_)
{ return((c_>=firstColumn()&&c_<=lastColumn())?MSTrue:MSFalse); }

int MSRowColumnView::columnFromEvent(const XEvent *pEvent_)
{
  int x=pEvent_->xbutton.x-panner()->x_origin();
  int col=xToColumn(x);
  return col;
}

void MSRowColumnView::adjustFirstRow(void)
{
  //This method ensures the first row setting makes sense
  int oldFirstRow=firstRow();  
  if (firstRow()!=0&&firstRow()+rows()>=numRows())
   {
     _firstRow=(numRows()>rows())?(numRows()-rows()):0;
   }
  _firstRow=(firstRow()>=0)?firstRow():0;
  if (oldFirstRow!=firstRow()) firstRowChangeNotify();    
}

void MSRowColumnView::unmapEditor(void)
{
  loseFocusNotify(editor());
  if (editor()->mapped()==MSTrue)
   {
     editor()->unmap();
     activateCallback(MSWidgetCallback::editend);
   }
}

void MSRowColumnView::mapEditor(void)
{
  MSBoolean wasMapped=MSFalse;
  if (editor()->mapped()==MSFalse)
   {
     wasMapped=MSTrue;
     editor()->map();
   }
  editor()->raise();
  takeFocusNotify(editor());
  if(wasMapped==MSTrue) activateCallback(MSWidgetCallback::editbegin);
}

int MSRowColumnView::dragRowFromEvent(const XEvent*)
{ return selectedRow(); }

void MSRowColumnView::dragRow(const XEvent *pEvent_)
{
  int row = dragRowFromEvent(pEvent_);
  if (row!=-1)
   {
     int dragWindowBorderWidth=2;
     if (dragRowWindow()==0)
      {
	_dragRowCursor=new MSDisplayCursor(server(),XC_sb_v_double_arrow,
					server()->pixel("black"),server()->pixel("white")); 
	XSetWindowAttributes attributes;
	attributes.save_under=True;
	attributes.override_redirect=True;
	attributes.backing_store=WhenMapped;
	attributes.cursor=dragRowCursor()->cursor();
	attributes.border_pixel=server()->pixel("yellow");
	_dragRowWindow=XCreateWindow(display(),server()->root(),
				  0,0,1,1,dragWindowBorderWidth,
				  CopyFromParent,InputOutput,CopyFromParent,
				  CWOverrideRedirect|CWSaveUnder|CWBackingStore|CWCursor|CWBorderPixel,
				  &attributes);
      }
     
     server()->grabPointer(window(),False,ButtonPressMask|ButtonReleaseMask,
			   GrabModeAsync,GrabModeAsync,None,
			   dragRowCursor()->cursor(),CurrentTime);
     int x=panner()->highlightThickness()+panner()->shadowThickness();
     int y=computeYCoord(row);
     int rootx,rooty;
     panner()->rootXY(rootx,rooty);
     int pannerY=rooty;
     rootx+=(x-dragWindowBorderWidth);
     rooty+=(y-dragWindowBorderWidth);
     int offset=pEvent_->xbutton.y_root-rooty;
     int width=panner()->width()-panner()->shadowThickness()*2
               -panner()->highlightThickness()*2;
     int height=rowHeight();
     XWindowChanges values;
     values.x=rootx;
     values.y=rooty;
     values.width=width;
     values.height=height;
     XConfigureWindow(display(),dragRowWindow(),CWX|CWY|CWWidth|CWHeight,&values);
     XMapRaised(display(),dragRowWindow());
     XCopyArea(display(),panner()->window(),dragRowWindow(),textGC(),x,y,width,height,0,0);
     Window root,child;
     int rx,ry,ix,iy;
     unsigned keys;
     int oldY=rooty;
     int sameScreen=XQueryPointer(display(),panner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     while (keys&Button2Mask)
      {
	if (sameScreen==True)
	 {
	   int newY=ry-offset;
	   if (newY!=oldY)
	    {
	      XMoveWindow(display(),dragRowWindow(),rootx,newY);
	      server()->flush();
	      oldY=newY;
	    }
	   if (newY<pannerY+headingsHeight())
	    {
	      newY=newY>0?newY:0;
	      int start=pannerY+headingsHeight();
	      int numScroll=(int)(((double)(start-newY)/((double)start+1))*rows()+1);
	      int newFirstRow=(int)(firstRow()-numScroll);
	      newFirstRow=(newFirstRow<0)?0:newFirstRow;
	      firstRow(newFirstRow);
	    }
	   else if (newY>pannerY+panner()->height())
	    {
	      if (numRows()>(unsigned)rows())
	       {
		 newY=newY>server()->height()?server()->height():newY;
		 int start=pannerY+panner()->height();
		 int numScroll=(int)(((double)(newY-start)/((double)server()->height()-start-1))*rows()+1);
		 int newFirstRow=firstRow()+numScroll;
		 int max=numRows()-rows();
		 newFirstRow=(newFirstRow>max)?max:newFirstRow;
		 firstRow(newFirstRow);
	       }
	    }
	 }
	sameScreen=XQueryPointer(display(),panner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
      }
     XUnmapWindow(display(),dragRowWindow());
     server()->ungrabPointer(window(),CurrentTime);
     int newRow;
     if (oldY<pannerY+headingsHeight()) newRow=firstRow();
     else if (oldY>pannerY+panner()->height())
      {
	if (numRows()>(unsigned)rows()) newRow=lastRow();
	else newRow=numRows()-1;
      }
     else
      {
	newRow=yToRow(oldY-pannerY-headingsHeight())+firstRow();
	if (newRow<row) newRow+=1;
	//if the calculated new row pointed to area that has no data,
	//assign it to the last row
	if (newRow>numRows()-1)  newRow=numRows()-1;
	// if the calcualted new row passes the last row,
	// make it the last row
	else if (newRow>lastRow()) newRow=lastRow();
      }
     
     if (newRow!=row) shuffleRow(row,newRow);
   }
}


int MSRowColumnView::computeYCoord(int row_)
{
  //This method compute the Y coordinate of the upper left corner of the specified row
  int offset=panner()->highlightThickness()+panner()->shadowThickness()+headingsHeight();
  return (int) (offset+(row_-firstRow())*rowHeight());
}

void MSRowColumnView::shuffleRow(int from_, int to_)
{
  moveRow(from_,to_);
  rowMovedNotify();
}

void MSRowColumnView::rowDragDrop(MSBoolean rowDragDrop_)
{ _rowDragDrop=rowDragDrop_; }

MSBoolean MSRowColumnView::rowDragDrop(void) const
{ return _rowDragDrop; }

void MSRowColumnView::moveRow(int, int) {}
void MSRowColumnView::startEditing(const XEvent *) {}
void MSRowColumnView::updateHsb(void) {}
void MSRowColumnView::updateVsb(void) {}
void MSRowColumnView::adjustView(void) {}
void MSRowColumnView::adjustNumVisible(void) {}
void MSRowColumnView::adjustFirstColumn(void) {}
void MSRowColumnView::defaultNumVisible(void) {}
void MSRowColumnView::cycleCell(MSColorCycle *) {}
void MSRowColumnView::drawRow(int) {}
void MSRowColumnView::drawRows(int,int) {}
void MSRowColumnView::drawSelectedRow(int,MSBoolean) {}
void MSRowColumnView::undrawSelectedRow(int row_) { drawSelectedRow(row_,MSFalse); }
void MSRowColumnView::moveEditorToSelection(const MSString &) {}
int MSRowColumnView::drawWidth(void) {return 0;}
int MSRowColumnView::drawHeight(void) {return 0;}
void MSRowColumnView::drawSelected(int) {}
void MSRowColumnView::undrawSelected(int) {}
void MSRowColumnView::hsbValueUpdate(void) {}
void MSRowColumnView::vsbValueUpdate(void) {}
void MSRowColumnView::updateInternalState(void) {}
unsigned MSRowColumnView::numColumns(void) const { return 0; }
unsigned MSRowColumnView::numRows(void) const { return 0; }

