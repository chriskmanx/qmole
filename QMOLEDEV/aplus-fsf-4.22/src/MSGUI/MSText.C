///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <math.h>
#if HAVE_NEW
#include <new>
#else
#include <new.h>
#endif
#include <stdlib.h>
#include <MSGUI/MSText.H>
#include <MSGUI/MSGC.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSFontManager.H>
#include <MSTypes/MSMessageLog.H>
#include <MSGUI/MSKeyClassCallback.H>
static MSBoolean debug=MSFalse;

typedef void (MSText::*PMFV)(void);

typedef struct 
{
  char      *_str;
  PMFV       _func;
} TextKeys;

typedef MSKeyClassCallback<MSText> KT;

static TextKeys KeyTable[]=
{ 
  {"<Key>Escape",    &MSText::escape},
  {"<Key>Return",    &MSText::returnKey},
  {"<Key>BackSpace", &MSText::backspace},
  {"<Key>Delete",    &MSText::backspace},
  {"<Key>Up",        &MSText::up},
  {"<Key>Down",      &MSText::down},
  {"<Key>Right",     &MSText::forwardChar},
  {"<Key>Left",      &MSText::backwardChar},
  {"Ctrl<Key>a",     &MSText::startOfLine},
  {"Ctrl<Key>e",     &MSText::endOfLine},
  {"Ctrl<Key>k",     &MSText::kill},
  {"Ctrl<Key>d",     &MSText::deleteChar},
  {"Ctrl<Key>f",     &MSText::forwardChar},
  {"Ctrl<Key>b",     &MSText::backwardChar},
  {"Ctrl<Key>y",     &MSText::yank},
  {"Ctrl<Key>i",     &MSText::insertMode},
  {"Ctrl<Key>p",     &MSText::up},
  {"Ctrl<Key>n",     &MSText::down },
  {"Ctrl<Key>l",     &MSText::refresh},
  {"Ctrl<Key>s",     &MSText::save},
  {"<Key>Tab",       &MSText::tab},
  {"Shift<Key>Tab",  &MSText::shiftTab},
  {"!<Key>F27",      &MSText::home},
  {"<Key>Home",      &MSText::home},
  {"!<Key>R13",      &MSText::end},
  {"<Key>End",       &MSText::end},
  {"!<Key>F29",      &MSText::pageUp},
  {"Ctrl<Key>u",     &MSText::pageUp},
  {"<Key>Prior",     &MSText::pageUp},
  {"!<Key>F35",      &MSText::pageDown},
  {"Ctrl<Key>d",     &MSText::pageDown},
  {"<Key>Next",      &MSText::pageDown},
  {0,0}
};

static const char *MSTextTabString="     ";
static const char NewLineChar=0x0A;
static const int MSTextDefaultHighlightThickness=2;
static const int MSTextDefaultShadowThickness=2;
static const unsigned long MSTextDefaultBlinkRate=500;
static const int DefaultScrollBarSize=15;
static const unsigned long MSTextEventMask=(ExposureMask|ButtonPressMask|
					    ButtonReleaseMask|Button1MotionMask);

MSText::CursorTimer::CursorTimer(MSText *t_,unsigned long interval_) : 
MSIntervalTimer(interval_) { _text=t_; }

MSText::CursorTimer::~CursorTimer(void) {}

void MSText::CursorTimer::process(void)
{ _text->processCursorTimer(); }

MSText::Vsb::Vsb(MSWidget *owner_) : MSVScrollBar(owner_)
{
  inc(1);  
  _highlightThickness=0;
  acceptFocus(MSFalse);
  width(DefaultScrollBarSize);
}

MSText::Vsb::~Vsb(void) {}

void MSText::Vsb::change(void)
{
  MSText *p=(MSText *)owner();
  p->vsbValueUpdate();
}

void MSText::Vsb::drag(void)
{ change(); }

MSText::Panner::Panner(MSWidget *owner_) : MSPrimitive(owner_)
{
  _shadowThickness=2;
  _highlightThickness=0;
  shadowStyle(MSSunken);
  selectInput(ExposureMask);
}

MSText::Panner::~Panner(void) {}

void MSText::Panner::expose(const XEvent *pEvent_)
{
  if (pEvent_->xexpose.count==0)
   {
     MSText *p=(MSText *)owner();
     XEvent er;
     while (XCheckWindowEvent(display(),window(),ExposureMask,&er)==True);
     p->redraw();
   }
}

MSText::MSText(MSWidget *owner_,const char *title_) : MSCompositeText(owner_) 
{
  _label=0;
  _label=new MSLabel(this,title_);
  init();
}

MSText::MSText(MSWidget *owner_,const MSStringVector& title_) : MSCompositeText(owner_) 
{
  _label=0;
  _label=new MSLabel(this,title_);
  init();
}

void MSText::init(void)
{
  _title=label()->label();
  _cursorPixmap=0;
  _imageMSGC=0;
  _lines=0;
  
  _highlightThickness=MSTextDefaultHighlightThickness;
  _shadowThickness=MSTextDefaultShadowThickness;

  acceptFocus(MSTrue);
  acceptTab(MSTrue);
  shadowStyle(MSEtchedIn);

  _cursorX=0;
  _cursorY=0;
  _firstLine=0;
  _cursorPosition=0;
  _cursorOn=MSFalse;
  _blinkOn=MSFalse;
  _haveFocus=MSFalse;
  _vsb=new Vsb(this);
  _panner=new Panner(this);
  label()->dynamic(MSTrue);
  if (label()->columns()>0) label()->map();

  if (MSKeyTranslationTable::keyTableData("MSText")==MSFalse)
   {
     keyTranslationTable()->addKeyTableData("MSText");
     unsigned i=0;
     KT *entry=0;
     while (KeyTable[i]._str!=0)
      {
	entry = new KT(KeyTable[i]._func );
	keyTranslationTable()->addCallback(KeyTable[i]._str,entry,"MSText");
	i++;
      }
   }
  else keyTranslationTable()->addKeyTableData("MSText");

  _blinkTimer=new CursorTimer(this,MSTextDefaultBlinkRate);
  _blinkOn=MSTrue;
  stopBlinking();
  _rows=5;
  _columns=40;
  _naturalRows=5;
  _naturalCols=40;
  _numLines=0;
  _maxNumLines=0;
  _maxEditLength=UINT_MAX;
  _selectionStart=0;
  _selectionLength=0;
  _startDrag=-1;
  _scrollTimer=0;
  selectInput(MSTextEventMask); 
  freeze();
}

MSText::~MSText(void)
{
  if (imageMSGC()!=0)    delete _imageMSGC;
  if (cursorPixmap()!=0) delete _cursorPixmap;
  if (blinkTimer()!=0)   delete _blinkTimer;
  if (label()!=0)        delete _label;
  if (panner()!=0)       delete _panner;
  if (vsb()!=0)          delete _vsb;
  if (scrollTimer()!=0)  delete _scrollTimer;
  if (lines()!=0)  
   {
     for (unsigned i=0;i<numLines();i++) delete _lines[i];
     delete [] _lines;
   }
  if (server()->primarySelectionOwner()==this) server()->primarySelectionOwner(0);
}

GC MSText::imageGC(void) const
{ return _imageMSGC->gc(); }

void MSText::naturalSize(void)
{
  _rows=naturalRows();
  _columns=naturalCols();  
  computeSize();
}

void MSText::computeSize(void)
{
  if (vsb()!=0&&panner()!=0&&label()!=0&&frozen()==MSFalse)
   {
     unsigned offset=highlightThickness() + shadowThickness();
     unsigned poffset=panner()->highlightThickness()+panner()->shadowThickness();
     unsigned w=columns()*charWidth()+2*poffset+vsb()->width(); 
     unsigned h=rows()*textHeight()+2*poffset; 
     if (label()->mapped()==MSTrue) h+=label()->height();
     resize(w+2*offset,h+2*offset);
   }
}

void MSText::placement(void)
{
  if (vsb()!=0&&panner()!=0&&label()!=0&&frozen()==MSFalse)
   {
     unsigned offset=highlightThickness()+shadowThickness();
     unsigned pH=height()-2*offset;
     unsigned pW=width()-2*offset;
     
     if (label()->mapped()==MSTrue) pH-=label()->height();
     pW-=vsb()->width();
     
     positionLabel();
     
     unsigned y=offset;
     if (label()->mapped()==MSTrue) y+=label()->height();
     panner()->moveTo(offset,y);
     panner()->resize(pW,pH);
     vsb()->moveTo(offset+pW,y);
     vsb()->height(pH);
     if (vsb()->mapped()==MSFalse) vsb()->map();
     if (panner()->mapped()==MSFalse) panner()->map();     
   }
}

void MSText::startBlinking(void) 
{
  if (isProtected()==MSFalse)
   {
     if (blinkOn()==MSFalse&&haveFocus()==MSTrue) 
      {
	blinkTimer()->reset();
	_blinkOn=MSTrue;
      }
   }
}

void MSText::stopBlinking(void) 
{
  if (blinkOn()==MSTrue) 
   { 
     blinkTimer()->stop();
     _blinkOn=MSFalse;
   }
}

void MSText::updateSensitivity(void)
{ clearCursor(); }

void MSText::updateReadOnly(void)
{ clearCursor(); }

void MSText::rows(unsigned rows_)    
{ 
  if (rows()!=rows_) 
   { 
     _rows=rows_; 
     computeSize(); 
   } 
} 
void MSText::columns(unsigned cols_) 
{ 
  if (columns()!=cols_) 
   { 
     _columns=cols_; 
     computeSize(); 
   } 
} 

unsigned MSText::drawWidth(void)
{ return panner()->width()-2*(panner()->highlightThickness()+panner()->shadowThickness()); }

unsigned MSText::positionToCol(unsigned pos_)
{ return (pos_<length())?pos_-line(positionToRow(pos_))->start():0; }

unsigned MSText::positionToRow(unsigned pos_)
{
  if (pos_<=length())
   {
     for (unsigned i=0;i<numLines();i++) 
      if (pos_>=line(i)->start()&&pos_<=line(i)->end()) return i;
   }
  return 0;
}

void MSText::positionToRowCol(unsigned pos_,unsigned& row_,unsigned& col_)
{
  unsigned row=0;
  unsigned col=0;
  for (unsigned i=0;i<numLines();i++)
   {
     if (pos_>=line(i)->start()&&pos_<=line(i)->end()) 
      {	
	row=i;
	col=pos_-line(i)->start();
	break;
      }
   }
  row_=row;
  col_=col;
}

unsigned MSText::lastLine(void)
{
  if (numLines()>0)
   {
     unsigned result=numLines()-1;
     Line *pline=line(result);
     while (result>0&&pline->start()>=length()) pline=line(--result);
     return result;
   }
  return 0;
}

void MSText::numLines(unsigned n_)
{
  if (numLines()!=n_&&n_>0)
   {
     Line **newLines=new Line*[n_];
     unsigned i;
     for (i=0;i<numLines();i++) 
      {
	if (i<n_) newLines[i]=_lines[i];
        else delete _lines[i];
	_lines[i]=0;
      }
     if (lines()!=0) delete [] _lines;
     for (i=numLines();i<n_;i++) newLines[i]=new Line;
     _numLines=n_;
     _lines=newLines;
   }
  resetLines();  
  updateVsb();
}

void MSText::initLines(unsigned n_)
{
  unsigned i;
  for (i=0;i<numLines();i++) { delete _lines[i]; _lines[i]=0; }
  if (lines()!=0) delete [] _lines;
  _lines=new Line*[n_];
  _numLines=n_;
  for (i=0;i<numLines();i++) _lines[i]=new Line;
  resetLines();  
  updateVsb();
}

void MSText::resetLinesAfterInsert(unsigned line_,unsigned cp_,unsigned slen_,MSBoolean insert_)
{
  if (line_<numLines())
   {
     unsigned pos=line(line_)->start();
     unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
     char *str=(char *)string();
     int w=panner()->width()-2*offset;
     int tw;
     unsigned i,j;
     unsigned len=length();
     unsigned sp,ep;
     unsigned lineDelta;
     MSBoolean lineChange=MSFalse;
     MSBoolean done=MSFalse;

     for (i=line_;i<numLines();i++)
      {
        sp=line(i)->start();
	ep=line(i)->end();
	line(i)->start(pos);
	tw=0;
	if (pos<len)
	 {
	   for(j=pos;j<len;j++,pos++)
	    {
	      if (str[j]==NewLineChar) break; 
	      tw+=charWidth(str[j]);
	      if (tw>w) { pos--; break; }
	    }
	   line(i)->end(pos++);
	 }
	else line(i)->end(pos);
        
	if (i==line_)
	 {
           if (insert_==MSFalse) // delete
	    {
   	      lineDelta=(line(i)->end()-line(i)->start())-((ep-sp));
              if (lineDelta>0) lineChange=MSTrue;
              else if (lineDelta==0&&tw>w) lineChange=MSTrue;
	      else if (tw<w&&ep!=cp_) done=MSTrue;
	    }
	   else // insert
	    {
              // check if insert crosses line boundary
	      if (cp_+slen_<=line(i)->end()) {if (tw<w) done=MSTrue; }
	      else lineChange=MSTrue;
	    }
           line(i)->dirty(MSTrue);
	 }
        else if (lineChange==MSTrue) line(i)->dirty(MSTrue);
        else if (done==MSFalse&&lineChange==MSFalse)
	 {
           if (ep==line(i)->start()||sp==line(i)->end()) lineChange=MSTrue;
           else
	    {
   	      lineDelta=(line(i)->end()-line(i)->start())-((ep-sp));
              if (lineDelta!=0&&tw<w) done=MSTrue;
	    }
           line(i)->dirty(MSTrue);
	 }
      }
     if (lineChange==MSTrue) resetVsb();
     lineStatus();
   }
  else MSMessageLog::warningMessage("MSText::resetFromInsert: line out of bounds");
}

void MSText::resetLinesFrom(unsigned start_)
{
  if (start_<numLines())
   {
     unsigned pos=line(start_)->start();
     unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
     char *cp=(char *)string();
     int w=panner()->width()-2*offset;
     int tw;
     unsigned i,j;
     unsigned len=length();
     for (i=start_;i<numLines();i++)
      {
	line(i)->start(pos);
        line(i)->dirty(MSTrue);
	tw=0;
	if (pos<len)
	 {
	   for(j=pos;j<len;j++,pos++)
	    {
	      if (cp[j]==NewLineChar) break; 
	      tw+=charWidth(cp[j]);
	      if (tw>w) { pos--; break; }
	    }
	   line(i)->end(pos++);
	 }
	else line(i)->end(pos);
      }
     lineStatus();
   }
  else MSMessageLog::warningMessage("MSText::resetLinesFrom: line out of bounds");
}

unsigned MSText::computeCursorX(unsigned start_,unsigned x_) 
{
  unsigned xoffset=panner()->highlightThickness()+panner()->shadowThickness();
  unsigned pos=0;
  unsigned col=0;
  
  if (x_<=xoffset||length()==0) col=0;
  else
   {
     int xpress=x_-xoffset;
     char *cp=(char *)string();
     unsigned sum=0;
     for (pos=start_;pos<length()&&(sum+(charWidth(cp[pos])>>1))<xpress;pos++)
      {
        col++;
        sum+=charWidth(cp[pos]);
      }
   }
  return col;
}

unsigned MSText::computeCursorPosition(void) 
{ return line(cursorY())->start()+cursorX(); }

unsigned MSText::lineLength(unsigned row_) 
{ return (row_<numLines())?(1+(line(row_)->end()-line(row_)->start())):0; }

unsigned MSText::computeX(unsigned row_,unsigned col_)
{
  unsigned xoffset=panner()->highlightThickness()+panner()->shadowThickness();
  if (row_<numLines())
   {
     unsigned start=line(row_)->start();
     char *cp=(char *)string();
     int len=lineLength(row_);
     if (col_<len) len=col_;
     return xoffset+(cp!=0?(textWidth((char *)cp+start,len)-1):0);
   }
  else return xoffset;
}

void MSText::processCursorTimer(void)
{ drawCursor(); }

void MSText::drawCursor(void)
{
  if (firstMap()==MSTrue&&isProtected()==MSFalse&&haveFocus()==MSTrue&&cursorPixmap()!=0)
   {
     _cursorOn=(cursorOn()==MSTrue)?MSFalse:MSTrue;
     unsigned yy=panner()->shadowThickness()+panner()->highlightThickness()+cursorY()*textHeight();
     unsigned xx=computeX(cursorY(),cursorX());
     xx-=(cursorPixmap()->width()>>1)-1; 
     
     XCopyArea(display(),cursorPixmap()->pixmap(),panner()->window(),
	       imageGC(),0,0,cursorPixmap()->width(),cursorPixmap()->height(),
	       xx,yy+textHeight()-cursorPixmap()->height());
   }
}

void MSText::moveCursor(int row_,int col_)
{
  clearCursor();
  _cursorY=(row_<numLines())?row_:numLines()-1;
  if (firstLine()+cursorY()>=maxNumLines()) _cursorY=maxNumLines()-firstLine()-1;
  _cursorX=col_;
  _cursorY=(cursorY()<numLines())?cursorY():numLines()-1;

  if (line(cursorY())->start()>length()) _cursorY=lastLine();
  if (cursorX()>=lineLength(cursorY())) _cursorX=lineLength(cursorY())-1;

  _cursorPosition=line(cursorY())->start()+cursorX();
  drawCursor();
}

unsigned MSText::yToRow(unsigned y_) 
{
  int row=0;
  if (y_==0) row=0;
  else row=y_/textHeight();
  row=(row>=numLines()?numLines()-1:row);
  return row;
}

void MSText::clearCursor(void) 
{ if (cursorOn()==MSTrue) drawCursor(); }

void MSText::configure(void)
{
  placement();

  unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
  int th=panner()->height()-2*offset;
  int tw=panner()->width()-2*offset;
  XRectangle clipRect[1];
  clipRect[0].x=0;
  clipRect[0].y=0;
  clipRect[0].width=tw;
  clipRect[0].height=th;
  XSetClipRectangles(display(),textGC(),offset,offset,&clipRect[0],1,Unsorted);

  _rows=(unsigned)(floor((double)th)/(double)textHeight());
  _columns=(unsigned)(floor((double)tw)/(double)charWidth());
  _rows=(rows()<=0)?1:rows(); 
  _cursorX=0;
  _cursorY=0;
  _cursorPosition=0;
  numLines(rows());
  updateVsb();
  redraw();
}

void MSText::firstMapNotify(void)
{
  unfreeze();
  createGCs();
  computeSize(); 
  makeIBeamCursor();
  _naturalRows=rows();
  _naturalCols=columns();
}

void MSText::motionNotify(const XEvent *pEvent_)
{
  if (pEvent_->xmotion.same_screen)
   {
     if (pEvent_->xmotion.subwindow==panner()->window())
      {
	if (scrollTimer()!=0) scrollTimer()->stop();
	unsigned col,row=yToRow(pEvent_->xmotion.y-panner()->y_origin());
	if (row<numLines())
	 {
	   if (line(row)->start()<length())
	    {
	      col=computeCursorX(line(row)->start(),pEvent_->xmotion.x-panner()->x_origin());
	      moveCursor(row,col);
	      int lineLen=lineLength(row);
	      int offset=(col>lineLen)?lineLen:col;
	      int end=line(row)->start()+offset;
	      end=(end<0)?0:end;
	      if (end<startDrag()) selectRange(end,startDrag()-end);
	      else selectRange(startDrag(),end-startDrag());
	      
	    }
	 }
      }
     else
      {
	if (scrollTimer()==0) _scrollTimer=new ScrollTimer(this);
	if (pEvent_->xmotion.y<=panner()->y())
	 {
   	   scrollTimer()->direction(ScrollTimer::Up);
	   scrollTimer()->reset();
	 }
	else if (pEvent_->xmotion.y>panner()->y()+panner()->height())
	 {
	   scrollTimer()->direction(ScrollTimer::Down);
	   scrollTimer()->reset();
	 }
	else scrollTimer()->stop();
      }	
	    
   }
}

void MSText::buttonPress(const XEvent *pEvent_)
{
  if (sensitive()==MSTrue) 
   {  
     if (pEvent_->xbutton.subwindow==panner()->window())
      {
        unsigned col=0,row=yToRow(pEvent_->xbutton.y-panner()->y_origin());
        if (row<numLines())
         {
           if (line(row)->start()<length())
            {
              col=computeCursorX(line(row)->start(),pEvent_->xbutton.x-panner()->x_origin());
            }
         }
        int lineLen=lineLength(row);
        int lineCol=(col>lineLen)?lineLen:col;
        int pos=line(row)->start()+lineCol;
        if (pEvent_->xbutton.button==Button2)
         {
           if (isProtected()==MSFalse&&traverseFocus(this)==MSTrue)
            {
              // If I own the selection, then disallow pasting into the highlighted
              // region
              if (primaryIsOurs()==MSTrue)
               {
                 if (pos>selectionStart()&&pos<=selectionEnd())
                  {
                    server()->bell();
                  }
                 else
                  {
                    moveCursor(row,col);
                    MSString selected=selectedString();
                    insertString(selected);
                    if (pos<=selectionStart()) selectRange(selectionStart()+selected.length(),selectionLength());
                  }
               }
              else
               {
                 moveCursor(row,col);
		 convertSelection();
               }
            }
           else server()->bell();
         }
        else if (pEvent_->xbutton.button==Button1&&traverseFocus(this)==MSTrue)
         {
           moveCursor(row,col);
           clearSelection();
           _selectionStart=pos;
           _startDrag=selectionStart();
         }
      }
   }
}

void MSText::buttonRelease(const XEvent *pEvent_)
{
  if (pEvent_->xbutton.button==Button1)
   {
     if (selectionLength()>0) ownSelection(XA_PRIMARY);
     if (scrollTimer()!=0) scrollTimer()->stop();
     _startDrag=-1;
   }
}

void MSText::updateCursor(void)
{
  if (firstMap()==MSTrue) 
   {
     clearCursor();
     createGCs();
     makeIBeamCursor(); 
     redraw(); 
   }
}

void MSText::updateBackground(unsigned long oldbg_)
{ 
  MSCompositeText::updateBackground(oldbg_);
  panner()->background(background());
  vsb()->background(background());
  label()->background(background());
  updateCursor();
}

void MSText::updateForeground(unsigned long oldfg_)
{ 
  MSCompositeText::updateForeground(oldfg_); 
  if (label()->foreground()==oldfg_) label()->foreground(foreground());
  updateCursor();
}

void MSText::updateFont(Font oldfid_)
{
  MSCompositeText::updateFont(oldfid_);
  if (cursorPixmap()!=0&&cursorPixmap()->height()!=textHeight()) makeIBeamCursor(); 
  if (frozen()==MSFalse)
   {
     panner()->clear();
     unsigned offset=(panner()->highlightThickness()+panner()->shadowThickness())<<1;
     int th=panner()->height()-offset;
     int tw=panner()->width()-offset;
     _rows=(unsigned)(floor((double)th)/(double)textHeight());
     _columns=(unsigned)(floor((double)tw)/(double)charWidth());
     _rows=(rows()<=0)?1:rows(); 
     numLines(rows());
     refresh();
   }
}

void MSText::redraw(void)
{
  if (mapped()==MSTrue)
   {
     clearCursor(); 
     drawShadow();
     for (unsigned i=0;i<numLines();i++) line(i)->dirty(MSTrue);
     drawLines(0,numLines());
     panner()->drawShadow();
     if (highlighted()==MSTrue) drawHighlight();
   }
}

void MSText::refresh(void)
{
  if (mapped()==MSTrue)
   {
     clearCursor(); 
     panner()->clear();
     for (unsigned i=0;i<numLines();i++) line(i)->dirty(MSTrue);
     drawLines(0,numLines());
   }
}

void MSText::print(const char *file_)
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
     clear();
     if (label()->mapped()==MSTrue) 
      {
	displayPrintOriginInc(label());
	label()->print();
	displayPrintOriginDec(label());
      }
     displayPrintOriginInc(panner());
     panner()->drawShadow();
     for (unsigned i=0;i<numLines();i++) line(i)->dirty(MSTrue);
     drawLines(0,numLines());
     displayPrintOriginDec(panner());
     if (fileOpen==MSTrue) 
      {
	displayPrintClose();
	outputMode(Draw);
      }
   }
}

void MSText::insertString(const char *pString_)
{
  if (pString_!=0)
   {
     if (text().length()<maxEditLength())
      {
	unsigned cp=cursorPosition();
	text().insert(pString_,cp);
	unsigned r,c;
	unsigned slen=strlen(pString_);
	clearCursor();
	unsigned l=positionToRow(cp);
	unsigned sp=line(l)->start();
	unsigned ep=line(l)->end();
	resetLinesAfterInsert(l,cp,slen,MSTrue);
	if (inRange(cp+slen)==MSTrue)
	 {
	   unsigned lineDelta=(line(l)->end()-line(l)->start())-((ep-sp));
	   (l==numLines()-1&&lineDelta==0)?scrollUp(1):drawTextFrom(cp);
	 }
	else
	 {
	   unsigned nl=computeNumLines(cp,cp+slen);
	   scrollUp(nl);
	 }
	positionToRowCol(cp+slen,r,c);
	moveCursor(r,c);
      }
     else server()->bell();
   }
  else server()->bell();
}

void MSText::deleteString(int startPos_,unsigned num_)
{
  if (startPos_>=0&&startPos_<text().length())
   {
     unsigned r,c;
     clearCursor();
     (void)text().remove(startPos_,num_);
     unsigned l=positionToRow(startPos_);
     resetLinesAfterInsert(l,startPos_,num_,MSFalse);
     drawTextFrom(startPos_);
     positionToRowCol(startPos_,r,c);
     moveCursor(r,c);
   }
  else server()->bell();
}

void MSText::cursorPosition(unsigned position_) 
{ _cursorPosition=position_; }

void MSText::focusIn(void)
{
  _haveFocus=MSTrue;
  highlight();
  if (isProtected()==MSFalse) startBlinking();
}

void MSText::focusOut(void)
{
  unHighlight();
  clearCursor();
  stopBlinking();
  _haveFocus=MSFalse;
}

void MSText::keyPress(const XEvent *e_,KeySym k_,unsigned int state_,const char *b_)
{
  MSKeyPress keyPress(k_,state_);
  if (isProtected()==MSFalse)
    {
      MSBoolean hasMatch = keyTranslationTable()->hasMatch(keyPress);
      if ((hasMatch == MSTrue)||strlen(b_)>0)
	{
	  MSBoolean cleared=MSFalse;
	  if (selectionLength()>0)
            {
              unsigned row,col;
              positionToRowCol(selectionStart(),row,col);
              moveCursor(row,col);
              text().remove(selectionStart(),selectionLength());
              resetLinesFrom(row);
              clearSelection();
              cleared=MSTrue;
            }
	  MSBoolean done = MSFalse;
	  if(cleared == MSTrue && (k_==XK_BackSpace || k_==XK_Delete))
	    {
	      done = MSTrue;
	    }
	  else if(hasMatch == MSTrue)
	    {
	      done = keyTranslate(keyPress);
	    }
	  if ( strlen(b_)>0 && done == MSFalse) insertString(b_);
	}
    }
  else if(sensitive() == MSTrue) 
    {
      //We are "ReadOnly" but still sensitive, so process Up/Down,etc....
      keyTranslate(keyPress); 
    }
}

void MSText::makeIBeamCursor(void)
{
  if (frozen()==MSFalse)
   {
     XSegment segments[3];
     int cursorWidth=5;
     int cursorHeight=textHeight();
     unsigned line_width=1;
     unsigned offset2=2*(highlightThickness()+shadowThickness());
     
     if (cursorHeight>19) 
      {
	cursorWidth++;
	line_width=2; 
      }
     
     if (cursorHeight>height()-offset2) cursorHeight=height()-offset2;
     if (cursorWidth>width()-offset2) cursorWidth=width()-offset2;
     
     cursorHeight=(cursorHeight>0)?cursorHeight:1;
     cursorWidth=(cursorWidth>0)?cursorWidth:1;
     
     if (cursorPixmap()!=0) delete cursorPixmap();
     
     _cursorPixmap=new MSPixmap(server(),"_textCursor",cursorWidth,cursorHeight,
				 foreground()^background(),background());
     
     GC fillGC=DefaultGC(display(),DefaultScreen(display()));
     XSetForeground(display(),fillGC,0);
     XSetBackground(display(),fillGC,1);
     
     // Fill the cursor with 0's 
     XFillRectangle(display(),cursorPixmap()->pixmap(),fillGC,
                    0,0,cursorPixmap()->width(),cursorPixmap()->height());

     // Draw the segments of the I-MSBeam 
     // 1st segment is the top horizontal line of the 'I' 
     segments[0].x1=0;
     segments[0].y1=line_width-1;
     segments[0].x2=cursorWidth;
     segments[0].y2=segments[0].y1;
  
     // 2nd segment is the bottom horizontal line of the 'I' 
     segments[1].x1=0;
     segments[1].y1=cursorHeight-1;
     segments[1].x2=cursorWidth;
     segments[1].y2=segments[1].y1;
  
     // 3rd segment is the vertical line of the 'I' 
     segments[2].x1=cursorWidth>>1;
     segments[2].y1=line_width-1;
     segments[2].x2=segments[2].x1;
     segments[2].y2=cursorHeight-1;
  
     XDrawSegments(display(),cursorPixmap()->pixmap(),imageGC(),segments,3);
  }
}

void MSText::createGCs(void)
{
  if (imageMSGC()==0) 
   {
     XGCValues values;
     unsigned long valueMask=(GCFunction|GCForeground|GCBackground);
     
     values.function=GXxor;
     values.foreground=foreground()^background();
     values.background=background();
     
     _imageMSGC=new MSGC(server(),MSTrue,&values,valueMask);
   }
  else imageMSGC()->color(foreground()^background(),background());
}

void MSText::kill(void)
{
  if (isProtected()==MSFalse&&text().length()>0)
   {
     int len=lineLength(cursorY())-cursorX();
     if (len>=0)
      {
	char *src=(char *)string();
	unsigned offset=line(cursorY())->start()+cursorX();
	if (len==0&&src[offset]==NewLineChar) len=1;
        else if (len>1)
         {
           unsigned index=len+offset-1;
           if (index<text().length())
            {
              if (src[index]==NewLineChar) len--;
            }
         }
	char *dest=new char[len+1];
	for (unsigned i=0;i<len;i++) dest[i]=src[i+offset];
	dest[len]='\0';
	yankBuffer()=dest;
	delete [] dest;
	deleteString(cursorPosition(),len);
      }
   }
}

void MSText::yank(void)
{ if (isProtected()==MSFalse&&yankBuffer().length()>0) insertString(yankBuffer().string()); }

void MSText::model(MSString& model_)
{ couple(&model_); }

void MSText::model(const MSString& model_)
{ constCouple(&model_); }

void MSText::update(const MSIndexVector&)
{ updateData(); }
void MSText::updateData(void)
{
  if (MSView::model()!=0)
   {
     MSString *cv=(MSString *)_model;
     string(cv->string());
   }
  else string("");
}

void MSText::string(const char *pString_)
{
  if (pString_!=0)
   {
     clearCursor();
     text()=pString_;
     if (firstMap()==MSTrue)
      {
	_firstLine=0;
	_cursorPosition=0;
	_cursorX=0;
	_cursorY=0;
	initLines(rows());
      }
     if (mapped()==MSTrue) refresh();
   }
}

void MSText::pageUp(void)
{
  if (firstLine()!=0)
   {
     firstLine(firstLine()-(numLines()-1));
     moveCursor(numLines()-1,0);
   }
}

void MSText::pageDown(void)
{
  if (firstLine()<(maxNumLines()-numLines()))
   {
     firstLine(firstLine()+(numLines()-1));
     moveCursor(0,0);
   }
}

void MSText::end(void) 
{ 
  firstLine(maxNumLines()-numLines()); 
  moveCursor(lastLine(),lineLength(lastLine()));
}

void MSText::home(void)         
{ 
  firstLine(0); 
  moveCursor(0,0);
}

void MSText::startOfLine(void)  { if (isProtected()==MSFalse) moveCursorX(0);} 
void MSText::endOfLine(void)    { if (isProtected()==MSFalse) moveCursorX(lineLength(cursorY())-1); } 
void MSText::up(void)           { if (isProtected()==MSFalse) moveCursorY(cursorY()-1); }
void MSText::down(void)         { if (isProtected()==MSFalse) moveCursorY(cursorY()+1); }
void MSText::deleteChar(void)   { if (isProtected()==MSFalse) deleteString(cursorPosition(),1); }
void MSText::backspace(void)    { if (isProtected()==MSFalse) deleteString(cursorPosition()-1,1); }
void MSText::forwardChar(void)  { if (isProtected()==MSFalse) moveCursorX(cursorX()+1); }
void MSText::backwardChar(void) { if (isProtected()==MSFalse) moveCursorX(cursorX()-1); }
void MSText::save(void)         { if (isProtected()==MSFalse) activateCallback(MSWidgetCallback::save); }

void MSText::shiftTab(void)
{ if (sensitive()==MSFalse) top()->traverseToPrevious(); }
void MSText::tab(void)          
{ 
  if (sensitive()==MSTrue&&isProtected()==MSFalse) insertString(MSTextTabString);
  else top()->traverseToNext();
}

void MSText::returnKey(void)
{
  char buf[2];
  buf[0]=NewLineChar;
  buf[1]='\0';
  insertString(buf);
}

void MSText::positionToXY(unsigned pos_,unsigned& x_,unsigned &y_) 
{
  unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
  if (pos_<=length())
   {
     unsigned row=positionToRow(pos_);
     y_=offset+row*textHeight();
     unsigned start=line(row)->start();
     unsigned len=pos_-start;
     char *cp=(char *)string();
     x_=offset+textWidth(cp+start,len);
   }
  else
   {
     x_=offset;
     y_=offset;
   }
}

MSBoolean MSText::inRange(unsigned pos_) 
{ return (pos_>=line(0)->start()&&pos_<=line(numLines()-1)->end())?MSTrue:MSFalse; }

unsigned MSText::computeNumLines(unsigned start_,unsigned end_) 
{
  unsigned r=0;
  unsigned pos;
  unsigned nl=0;
  if (inRange(start_)==MSTrue) 
   {
     r=positionToRow(start_);
     if (end_>line(r)->end())
      {
	pos=line(r)->end()+1;
        nl=1;
      }
     else pos=line(r)->start();
   }
  else
   {
     pos=line(numLines()-1)->end()+1;
     nl=1;
   }

  unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
  char *cp=(char *)string();
  int w=panner()->width()-2*offset;
  int tw=0;
  unsigned len=(end_<length())?end_:length();
  for(;pos>0&&pos<len;pos++)
   {
     if (cp[pos]==NewLineChar) { nl++,tw=0; }
     else
      {
	tw+=charWidth(cp[pos]);
	if (tw>w) { pos--,nl++,tw=0; }
      }
   }
  return nl;
}

unsigned MSText::lineToPosition(unsigned line_) 
{
  unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
  int w=panner()->width()-2*offset;
  char *cp=(char *)string();
  unsigned l=0;
  int tw=0;
  unsigned len=length();
  unsigned sp=0;
  if (line_>0)
   {
     for(unsigned pos=0;pos<len;pos++)
      {
	if (cp[pos]==NewLineChar) 
	 { 
	   l++; 
	   sp=pos+1;
	   if (l==line_) break;
	   tw=0; 
	 }
	else
	 {
	   tw+=charWidth(cp[pos]);
	   if (tw>w) 
	    { 
	      pos--; 
	      l++;
	      sp=pos+1;
	      if (l==line_) break;
	      tw=0;
	    }
	 }
      }
   }
  return sp;
}

void MSText::scrollUp(unsigned n_)
{
  if (n_>0&&n_<numLines())
   {
     clearCursor();
     unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
     unsigned i=0;
     for (i=0;i<numLines()-n_;i++) 
      {
	line(i)->start(line(i+n_)->start());
	line(i)->end(line(i+n_)->end());
        line(i)->clean();
      }
     _firstLine+=n_;
     unsigned pos=line(numLines()-n_-1)->end()+1; 
     unsigned endPos;
     for (i=numLines()-n_;i<numLines();i++)
      {
        line(i)->dirty(MSTrue);
        if (pos<length())
	 {
	   endPos=computeEndPosition(pos);
	   line(i)->start(pos);
	   line(i)->end(endPos);
	   pos=endPos+1;
	 }
        else
	 {
	   line(i)->start(length());
	   line(i)->end(length());
	 }
      }
     unsigned sh=n_*textHeight();
     unsigned h=(numLines()-n_)*textHeight();
     XCopyArea(display(),panner()->window(),panner()->window(),
	       panner()->backgroundShadowGC(),
	       offset,offset+sh,
	       panner()->width()-2*offset,h,
	       offset,offset);
     XFillRectangle(display(),panner()->window(),backgroundShadowGC(),
		    offset,offset+h,
		    panner()->width()-2*offset,sh);
     drawLines(numLines()-n_,numLines());
   }
  else if (n_>0) firstLine(firstLine()+n_);
  else MSMessageLog::warningMessage("MSText::ScrollUp error: zero increment specified");
}

void MSText::scrollDown(unsigned n_)
{
  if (n_>0&&n_<numLines())
   {
     clearCursor();
     unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
     unsigned i;
     for (i=numLines()-1;i>=n_;i--) 
      {
	line(i)->start(line(i-n_)->start());
	line(i)->end(line(i-n_)->end());
        line(i)->clean();
      }
     _firstLine-=n_;
     unsigned pos=lineToPosition(firstLine());
     unsigned endPos;
     for (i=0;i<n_;i++)
      {
        endPos=computeEndPosition(pos);
        line(i)->start(pos);
	line(i)->end(endPos);
        line(i)->dirty(MSTrue);
	pos=endPos+1;
      }
     lineStatus();
     unsigned sh=n_*textHeight();
     unsigned h=(numLines()-n_)*textHeight();
     XCopyArea(display(),panner()->window(),panner()->window(),
	       panner()->backgroundShadowGC(),
	       offset,offset,
	       panner()->width()-2*offset,h,
	       offset,offset+sh);
     XFillRectangle(display(),panner()->window(),backgroundShadowGC(),
		    offset,offset,
		    panner()->width()-2*offset,sh);
     drawLines(0,n_);
   }
  else if (n_>0) firstLine(firstLine()-n_);
  else MSMessageLog::warningMessage("MSText::ScrollDown error: zero increment specified");
}

void MSText::resetLinePosition(unsigned line_,unsigned pos_)
{
  if (line_<numLines())
   {
     unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
     int w=panner()->width()-2*offset;
     char *cp=(char *)string();
     unsigned pos=pos_;
     unsigned len=length();
     int tw;
     unsigned i,j;
     for (i=line_;i<numLines();i++)
      {
	line(i)->start(pos);
	line(i)->dirty(MSTrue);
	tw=0;
	if (pos<len)
	 {
	   for(j=pos;j<len;j++,pos++)
	    {
	      if (cp[j]==NewLineChar) break;
              else
	       {
  	         tw+=charWidth(cp[j]);
	         if (tw>w) { pos--; break; }
	       }
	    }
	   line(i)->end(pos++);
	 }
	else line(i)->end(pos);
      }
     lineStatus();
   }
  else MSMessageLog::warningMessage("MSText::resetLinesPosition: line out of bounds");
}

void MSText::moveCursorX(int x_)
{
  clearCursor();  
  unsigned x=cursorX();
  if (x_<0)
   {
     if (cursorY()==0)
      {
        if (firstLine()>0)
	 {
	   unsigned n=1;
	   n=((firstLine()-n)>0)?n:firstLine();
	   scrollDown(n);
	   moveCursor(0,line(0)->end());
         }
        else server()->bell();
      }
     else if (cursorY()>0&&cursorY()<numLines())
      {
        moveCursor(cursorY()-1,lineLength(cursorY()-1));
      }
   }
  else if (x_>=0)
   {
     if (x_<=lineLength(cursorY()))
      {
        moveCursor(cursorY(),x_);
      }
     else if (x_>lineLength(cursorY()))
      {
        if (cursorY()<numLines()-1)
	 {
      	   if (line(cursorY()+1)->start()<length())
	    {
              moveCursor(cursorY()+1,0);
            }
	   else server()->bell();
	 }
        else if (cursorY()==numLines()-1)
	 {
	   if (line(cursorY())->end()<length())
	    {
	      scrollUp(1);
              moveCursor(numLines()-1,0);
	    } 
           else server()->bell();
	 }
        else server()->bell();
      }
     else server()->bell();
   }
  else server()->bell();
}

void MSText::moveCursorY(int y_)
{
  clearCursor();  
  unsigned y=cursorY();
  if (y_<0&&firstLine()>0)
   { 
     unsigned n=-y_;
     n=((firstLine()-n)>0)?n:firstLine();
     scrollDown(n);
     moveCursor(0,cursorX());
   }
  else if (y_>=0)
   {
     if (y_>=numLines())
      {
        if (line(numLines()-1)->end()<length())
	 {
           scrollUp(y_-numLines()+1);
           moveCursor((numLines()-1),cursorX());
	 }
        else server()->bell();
      }
     else if (y_>=0&&y_<numLines())
      {
        if (line(y_)->start()<length()) moveCursor(y_,cursorX());
	else server()->bell();
      }
     else server()->bell();
   }
  else server()->bell();
}

unsigned MSText::computeEndPosition(unsigned start_) 
{
  unsigned r=start_;
  if (start_<length())
   {
     unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
     int w=panner()->width()-2*offset;
     char *cp=(char *)string();
     int tw=0;
     unsigned pos=start_;
     unsigned len=length();
     if (pos<len)
      {
        for(;pos<len;pos++)
         {
	   if (cp[pos]==NewLineChar) break; 
	   tw+=charWidth(cp[pos]);
	   if (tw>w) { pos--; break; }
         }
        r=pos;
      }	
     else r=len;
   }
  return r;
}

void MSText::drawTextFrom(unsigned pos_)
{
  if (string()!=0&&mapped()==MSTrue&&numLines()>0)
   {
     unsigned dl=0;
     unsigned len=length();
     unsigned row=positionToRow(pos_);
     unsigned r=row;
     unsigned pos=pos_;
     unsigned x,y;
     unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
     char *cp=(char *)string();
 
     y=offset+r*textHeight();
     for (;r<numLines();) 
      {
        if (line(r)->dirty()==MSTrue)
	 {
	   line(r)->clean();
	   if (r==row)
	    {
	      x=offset+textWidth(cp+line(r)->start(),pos-line(r)->start());
	      dl=(line(r)->end()-pos)+1;
	    }
	   else
	    {
	      x=offset;
	      pos=line(r)->start();
	      dl=lineLength(r);
	    }
	   dl=(pos+dl<len)?dl:len-pos;
	   XFillRectangle(display(),panner()->window(),backgroundShadowGC(),
			  x,y,panner()->width()-offset-x,textHeight());
	   if (dl>0)
	    {
	      XDrawImageString(display(),panner()->window(),textGC(),textFontStruct(),
			       x,y+textAscent(),(char *)cp+pos,dl);
	    }
	 }
        r++;
        y+=textHeight();
      }
   }
}

void MSText::drawLines(unsigned sl_,unsigned el_)
{
  if (string()!=0&&mapped()==MSTrue&&numLines()>0)
   {
     sl_=(sl_>0)?sl_:0;
     el_=(el_>0)?el_:0;
     sl_=(sl_<numLines())?sl_:numLines()-1;
     el_=(el_<=numLines())?el_:numLines();
     
     char *cp=(char *)string();
     unsigned dl=0;
     unsigned len=length();
     unsigned r=sl_;
     unsigned pos;
     unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
     unsigned x=offset;
     unsigned y=offset+r*textHeight();

     for (;r<el_;) 
      {
        if (line(r)->dirty()==MSTrue)
	 {
           line(r)->clean();
	   pos=line(r)->start();
	   dl=lineLength(r);
	   dl=(pos+dl<len)?dl:len-pos;
	   XFillRectangle(display(),panner()->window(),backgroundShadowGC(),
			  x,y,panner()->width()-offset-x,textHeight());
	   if (dl>0)
	    {
	      // First filter out all possiblities that require highlighting
	      if (selectionLength()==0||
		  (pos>selectionEnd())||
		  (pos+dl<selectionStart()))
	       {
		 XDrawImageString(display(),panner()->window(),textGC(),textFontStruct(),
				  x,y+textAscent(),(char *)cp+pos,dl);
	       }
	      else
	       {
		 if (pos>=selectionStart())
		  {
		    int len1,len2;
		    if (pos+dl>selectionEnd())
		     { 
		       len1=selectionEnd()-pos+1;
 		       len2=dl-len1;
		     }
		    else
		     {
		       len1=dl;
		       len2=0;
		     }
		    XSetForeground(display(),textGC(),background());
		    XSetBackground(display(),textGC(),foreground());
		    XDrawImageString(display(),panner()->window(),textGC(),textFontStruct(),
				     x,y+textAscent(),(char *)cp+pos,len1);
		    XSetForeground(display(),textGC(),foreground());
		    XSetBackground(display(),textGC(),background());
		    if (len2>0)
		     {
		       XDrawImageString(display(),panner()->window(),textGC(),textFontStruct(),
					x+textWidth((char *)cp+pos,len1),
					y+textAscent(),(char *)cp+pos+len1,len2);
		     }
		  }
		 else
		  {
		    int len1,len2,len3;
		    len1=selectionStart()-pos;
		    if (pos+dl>selectionEnd())
		     {
		       len2=selectionLength();
		       len3=dl-len2-len1;
		     }
		    else
		     {
		       len2=dl-len1;
		       len3=0;
		     }
		    XDrawImageString(display(),panner()->window(),textGC(),textFontStruct(),
				     x,y+textAscent(),(char *)cp+pos,len1);
		    
		    XSetForeground(display(),textGC(),background());
		    XSetBackground(display(),textGC(),foreground());
		    XDrawImageString(display(),panner()->window(),textGC(),textFontStruct(),
				     x+textWidth((char *)cp+pos,len1),
				     y+textAscent(),(char *)cp+pos+len1,len2);
		    
		    XSetForeground(display(),textGC(),foreground());
		    XSetBackground(display(),textGC(),background());
		    if (len3>0)
		     {
		       XDrawImageString(display(),panner()->window(),textGC(),textFontStruct(),
					x+textWidth((char *)cp+pos,len1+len2),
					y+textAscent(),(char *)cp+pos+len1+len2,len3);
		     }
		  }
	       }
	    }
	 }
	r++;
	y+=textHeight();
      }
     if (vsb()->value()!=firstLine()) vsb()->valueChange(firstLine());
   }
}

void MSText::firstLine(int fl_)
{
  if (fl_!=firstLine())
   {
     clearCursor();
     _firstLine=(fl_>=0)?fl_:0;
     unsigned pos=lineToPosition(firstLine());
     unsigned endPos;
     for (unsigned i=0;i<numLines();i++)
      {
        line(i)->dirty(MSTrue);
        if (pos<length())
	 {
	   endPos=computeEndPosition(pos);
	   line(i)->start(pos);
	   line(i)->end(endPos);
	   pos=endPos+1;
	 }
        else
	 {
	   line(i)->start(length());
	   line(i)->end(length());
	 }
      }
     drawLines(0,numLines());
   }
}

unsigned MSText::computeMaxNumLines(void) 
{
  unsigned nl=firstLine()+numLines();
  unsigned pos=line(numLines()-1)->start();
  unsigned offset=panner()->highlightThickness()+panner()->shadowThickness();
  int w=panner()->width()-2*offset;
  char *cp=(char *)string();
  int tw=0;
  unsigned len=length();
  for(;pos>0&&pos<len;pos++)
   {
     if (cp[pos]==NewLineChar) { nl++,tw=0; }
     else
      {
	tw+=charWidth(cp[pos]);
	if (tw>w) { pos--,nl++,tw=0; }
      }
   }
  _maxNumLines=nl;
  return nl;
}

void MSText::resetVsb(void)
{ vsb()->max(computeMaxNumLines()); }

void MSText::updateVsb(void)
{
  if (vsb()->width()>1)
   {
     vsb()->viewSize(numLines());
     vsb()->max(computeMaxNumLines());		
     vsb()->pageInc(numLines()-1);
     vsb()->valueChange(firstLine());
     if (vsb()->mapped()==MSFalse) vsb()->map();
   }
  else if (vsb()->mapped()==MSTrue) vsb()->unmap();
}

void MSText::vsbValueUpdate(void)
{
  if (vsb()->value()<firstLine()) scrollDown(firstLine()-vsb()->value());     
  else if (vsb()->value()>firstLine()) scrollUp(vsb()->value()-firstLine());
}

void MSText::selectionClear(const XEvent *)
{
  if (server()->primarySelectionOwner()==this) server()->primarySelectionOwner(0);
  clearSelection();
}

void MSText::insertPrimarySelection(void)
{
  insertString(_server->pasteBuffer().string());
}

const char * MSText::getPrimarySelection(MSString& buffer_, int& len_)
{
  if (primaryIsOurs()==MSFalse) return 0;
  char *data=(char *)string() +selectionStart();
  len_=selectionLength();
  return data;
}

void MSText::debugMode(MSBoolean mode_) { debug=mode_; }   

void MSText::lineStatus(void)
{
//   if (debug==MSTrue)
//    {
//      for (unsigned i=0;i<numLines();i++)
//       {
// 	cerr<<"reset line("<<i<<"): ";
// 	cerr<<line(i)->start()<<"\t"<<line(i)->end();
// 	cerr<<"\t"<<unsigned(line(i)->dirty())<<endl;
//       }
//    }
}

void MSText::updateTitle(void)
{
  label()->freeze();
  label()->foreground(titleForeground()); 
  label()->font(titleFont());
  label()->alignment(titleAlignment());

  unsigned h=label()->height();
  unsigned w=label()->width();
  MSBoolean doPlacement=MSFalse;
  label()->label(title());

  unsigned ml=label()->columns();
  if (ml==0&&label()->mapped()==MSTrue)
   {
     label()->unmap();
     doPlacement=MSTrue;
   }
  else if (ml>0&&label()->mapped()==MSFalse)
   {
     label()->map();
     doPlacement=MSTrue;
   }
  positionLabel();
  label()->unfreeze();
  if (label()->height()!=h||label()->width()!=w||doPlacement==MSTrue) placement();
  else if (label()->mapped()==MSTrue) label()->redraw();
}

// this code was taken from MSNotebook and
// is duplicated here -- it would be nice if
// the label behavior could be encapsulated
// and thus we would not have to multiple copies
// of the same code running around.
void MSText::positionLabel(void)
{
  if (label()->columns()>0)
   {
     int offset=highlightThickness()+shadowThickness();
     int xpos,ypos;
     int indent;
     if (titleAlignment()&MSLeft) 
      {
	indent=XTextWidth(fontManager()->fontStruct(label()->font()),"M",1);
	xpos=offset+indent;
      }
     else if (titleAlignment()&MSRight) 
      {
	indent=XTextWidth(fontManager()->fontStruct(label()->font()),"M",1);
	xpos=width()-offset-indent-label()->width();
      }
     else xpos=width()/2-label()->width()/2;

     int shadowOffset;
     if (titleAlignment()&MSTop) 
      {
	shadowOffset=label()->height();
	ypos=0;
      }
     else if (titleAlignment()&MSBottom) 
      {
	shadowOffset=0;
	ypos=offset;
      }
     else
      { 
	shadowOffset=label()->height()/2+offset;
	ypos=offset;
      }
     if (shadowOffset!=topShadowOffset())
      {
	undrawShadow();
	topShadowOffset(shadowOffset);
      }
     label()->moveTo(xpos,ypos);
     label()->map();
   }
  else
   { 
     label()->unmap();
     topShadowOffset(0);
   }
}

void MSText::set(MSAttrValueList& avList_)
{
  MSCompositeText::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="rows")         rows(avList_[i].value().asInt()),index<<i;  
     else if (avList_[i].attribute()=="columns") columns(avList_[i].value().asInt()),index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSText::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("rows",      MSString(rows())  );
  avList_<<MSAttrValue("columns",   MSString(columns()));
  return MSCompositeText::get(avList_);
}

void MSText::insertMode(void) {}
void MSText::escape(void) {}

void MSText::clearSelection(void)
{
  selectRange(0,0);
}

void MSText::selectRange(unsigned selectionStart_,unsigned selectionLength_)
{
  if (selectionStart()!=selectionStart_||selectionLength()!=selectionLength_)
   {
     if ((length()==0&&selectionStart_==0,selectionLength_==0)||
	 (selectionStart_<length()&&selectionStart_+selectionLength_<=length()))
      {
	int beforeStart,beforeEnd,afterStart,afterEnd,start,end;
	if (selectionLength()>0)
	 {
	   beforeStart=positionToRow(selectionStart());
	   beforeEnd=positionToRow(selectionEnd());
	 }
	else
	 {
	   beforeStart=-1;
	   beforeEnd=-1;
	 }
	int previousStart=selectionStart();
	_selectionStart=selectionStart_;
	_selectionLength=selectionLength_;
	if (selectionLength()>0)
	 {
	   afterStart=positionToRow(selectionStart());
	   afterEnd=positionToRow(selectionEnd());
	 }
	else
	 {
	   afterStart=-1;
	   afterEnd=-1;
	 }
	if (beforeStart==-1||afterStart==-1)
	 {
	   start=0;
	   end=numLines()-1;
	 }
	else if (beforeEnd==afterEnd)
	 {
	   if (afterStart<beforeStart)
	    {
	      start=afterStart;
	      end=beforeStart;
	    }
	   else if (beforeStart<afterStart)
	    {
	      start=beforeStart;
	      end=afterStart;
	    }
	   else
	    {
	      if (previousStart>selectionStart())
	       {
		 start=beforeStart;
		 end=beforeStart;
	       }
	      else
	       {
		 start=beforeEnd;
		 end=afterEnd;
	       }
	    }
	   
	 }
	else if (beforeStart==afterStart)
	 {
	   if (beforeEnd<afterEnd)
	    {
	      start=beforeEnd;
	      end=afterEnd;
	    }
	   else
	    {
	      start=afterEnd;
	      end=beforeEnd;
	    }
	 }
	else
	 {
	   start=0;
	   end=numLines()-1;
	 }
	for (unsigned i=start;i<=end;i++) line(i)->dirty(MSTrue);
	drawLines(start,end+1);
      }
     if (selectionLength()>0) ownSelection(XA_PRIMARY);
     else if (primaryIsOurs()==MSTrue) disownSelection(XA_PRIMARY);
   }
}

MSString MSText::selectedString(void) const
{
  if (selectionLength()==0) return MSString();
  else return text().subString(selectionStart(),selectionLength());
}


MSText::ScrollTimer::ScrollTimer(MSText *text_,Direction direction_)
: MSIntervalTimer(50),_text(text_),_direction(direction_)
{}

void MSText::ScrollTimer::process(void)
{
  if (direction()==Up)
   {
     if (text()->firstLine()!=0)
      {
	text()->firstLine(text()->firstLine()-1);
	int newPos=text()->lineToPosition(text()->firstLine());
	if (text()->startDrag()>newPos)
	 {
	   text()->selectRange(newPos,text()->startDrag()-newPos);
	 }
	else 
	 {
	   text()->selectRange(text()->startDrag(),newPos-text()->startDrag());
	 }
	unsigned row,col;
	text()->positionToRowCol(text()->selectionStart(),row,col);
	text()->moveCursor(row,col);
      }
     else stop();
   }
  else
   {
     if (text()->firstLine()<(text()->maxNumLines()-text()->numLines()))
      {
	text()->firstLine(text()->firstLine()+1);
	int last=text()->firstLine()+text()->numLines();
	int newPos=text()->lineToPosition(last);
	if (text()->startDrag()<newPos)
	 {
	   text()->selectRange(text()->startDrag(),newPos-text()->startDrag());
	 }
	else
	 {
	   text()->selectRange(newPos,text()->startDrag()-newPos);
	 }
	unsigned row,col;
	text()->positionToRowCol(text()->selectionEnd(),row,col);
	text()->moveCursor(row,col);
      }
     else stop();
   }
}

MSBoolean MSText::primaryIsOurs(void) const
{
  return MSBoolean(server()->primarySelectionOwner()==this);
}

