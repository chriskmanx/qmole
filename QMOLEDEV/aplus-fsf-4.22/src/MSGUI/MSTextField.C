///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <MSGUI/MSTextField.H>
#include <MSGUI/MSGC.H>
#include <MSGUI/MSKeyClassCallback.H> 

static const int MSTextFieldDefaultHighlightThickness=2;
static const int MSTextFieldDefaultShadowThickness=2;
static const unsigned MSTextFieldDefaultMargin=0;
static const unsigned long MSTextFieldDefaultBlinkRate=500;
static const unsigned MSTextFieldDefaultMaxLength=128;
static const char MSTextFieldDefaultInputMaskCharacter = '-';

typedef void (MSTextField::*PMFV)(void);

typedef struct 
{
  char *_pString;
  PMFV  _func;
} TextKeys;

typedef MSKeyClassCallback<MSTextField> KT;

static TextKeys KeyTable[]=
{ 
 { "<Key>Return",              &MSTextField::returnKey },
 { "<Key>KP_Enter",            &MSTextField::returnKey },
 { "<Key>Escape",              &MSTextField::escape },
 { "<Key>BackSpace",           &MSTextField::backspace },
 { "<Key>Delete",              &MSTextField::backspace },
 { "<Key>Right",               &MSTextField::forwardChar },
 { "<Key>Left",                &MSTextField::backwardChar },
 { "<Key>Up",                  &MSTextField::up },
 { "<Key>Down",                &MSTextField::down },
 { "!<Key>F27",                &MSTextField::home },
 { "<Key>Home",                &MSTextField::home },
 { "!<Key>R13",                &MSTextField::end },
 { "<Key>End",                 &MSTextField::end },
 { "Ctrl<Key>a",               &MSTextField::home },
 { "Ctrl<Key>e",               &MSTextField::end },
 { "Ctrl<Key>k",               &MSTextField::kill },
 { "Ctrl<Key>d",               &MSTextField::deleteChar },
 { "Ctrl<Key>f",               &MSTextField::forwardChar },
 { "Ctrl<Key>b",               &MSTextField::backwardChar },
 { "Ctrl<Key>y",               &MSTextField::yank },
 { "~Meta~Ctrl~Shift<Key>Tab", &MSTextField::tab },
 { "~Meta~Ctrl Shift<Key>Tab", &MSTextField::shiftTab },
 { "!<Key>Insert",             &MSTextField::insertKey },
 { 0,0 }
};

MSTextField::KeyTranslationFunction MSTextField::_keyTranslationFunction=0;

MSTextField::CursorTimer::CursorTimer(MSTextField *textField_,unsigned long interval_) : 
MSIntervalTimer(interval_)
{ _textField=textField_; }

MSTextField::CursorTimer::~CursorTimer(void) {}

void MSTextField::CursorTimer::process(void)
{ _textField->processCursorTimer(); }

MSTextField::MSTextField(MSWidget *owner_) : MSPrimitiveText(owner_) 
{ init();}

MSTextField::~MSTextField(void)
{
  if (_imageMSGC!=0)     delete _imageMSGC;
  if (cursorPixmap()!=0) delete _cursorPixmap;
  if (_blinkTimer!=0)    delete _blinkTimer;
}

void MSTextField::init(void)
{
  _cursorPixmap=0;
  _imageMSGC=0;
  _editMode=InsertMode;

  _highlightThickness=MSTextFieldDefaultHighlightThickness;
  _shadowThickness=MSTextFieldDefaultShadowThickness;
  _xMargin=MSTextFieldDefaultMargin;
  _yMargin=MSTextFieldDefaultMargin;
  shadowStyle(MSSunken);
  acceptFocus(MSTrue);
  maxLength(MSTextFieldDefaultMaxLength);

  _cursorPosition=length();
  _scrollIndex=0;
  _cursorOn=MSFalse;
  _blinkOn=MSFalse;
  _haveFocus=MSFalse;
  _selectionStart=0;
  _selectionEnd=0;

  initKeyTranslations();

  _blinkTimer=new CursorTimer(this,MSTextFieldDefaultBlinkRate);
  _blinkOn=MSTrue;
  stopBlinking();
  _alignment=(MSAlignment)MSLeft;

  _inputMaskCharacter = MSTextFieldDefaultInputMaskCharacter;
  _masking = MSFalse;

  //Added stuff
  selectInput(ExposureMask|ButtonPressMask|ButtonReleaseMask);
}


GC MSTextField::imageGC(void) const
{ return _imageMSGC->gc(); }

const char *MSTextField::string(void)
{ return (const char *)text(); }

void MSTextField::initKeyTranslations(void)
{
  if (MSKeyTranslationTable::keyTableData("MSTextField")==MSFalse)
   {
     keyTranslationTable()->addKeyTableData("MSTextField");
     unsigned i=0;
     KT *entry;
     while (KeyTable[i]._pString!=0)
      {
	entry = new KT(KeyTable[i]._func );
	keyTranslationTable()->addCallback(KeyTable[i]._pString,entry,"MSTextField");
	i++;
      }
   }
  else keyTranslationTable()->addKeyTableData("MSTextField");
}

void MSTextField::selectRange(unsigned startPos_,unsigned endPos_)
{
  if (startPos_<endPos_)
   {
     _selectionStart=startPos_;
     _selectionEnd=endPos_;
     clearCursor();
     drawText(MSFalse);
     drawCursor();
   }
  else clearSelection();
}

void MSTextField::clearSelection(void)
{
  _selectionStart=0;
  _selectionEnd=0;
}

void MSTextField::selectAll(void)
{
  if (length()>0)
   {
     _selectionStart=0;
     _selectionEnd=length();
     clearCursor();
     drawText(MSFalse);
     drawCursor();
   }
}

void MSTextField::margin(unsigned margin_) 
{ 
  if (xMargin()!=margin_||yMargin()!=margin_) 
   {
     _xMargin=margin_;
     _yMargin=margin_;
     redraw();
   }
}

void MSTextField::xMargin(unsigned xMargin_) 
{ 
  if (xMargin()!=xMargin_) 
   {
     _xMargin=xMargin_; 
     redraw();
   }
}

void MSTextField::yMargin(unsigned yMargin_) 
{ 
  if (yMargin()!=yMargin_) 
   {
     _yMargin=yMargin_; 
     redraw();
   }
}

void MSTextField::editMode(EditingMode mode_) 
{ _editMode=mode_; }
void MSTextField::maxLength(unsigned len_)    
{ _maxLength=len_; }

void MSTextField::processCursorTimer(void) 
{ drawCursor(); }

void MSTextField::startBlinking(void) 
{ 
  if (blinkOn()==MSFalse&&haveFocus()==MSTrue) 
   { 
     blinkTimer()->reset(); 
     _blinkOn=MSTrue; 
     if (cursorOn()==MSFalse) drawCursor();
   }
}

void MSTextField::stopBlinking(void) 
{ 
  blinkTimer()->stop(); 
  clearCursor();
  _blinkOn=MSFalse;
}

unsigned MSTextField::textX(void) 
{ return (highlightThickness()+shadowThickness()+xMargin()); }

unsigned MSTextField::textY(void) 
{
  int offset=highlightThickness()+shadowThickness()+yMargin();
  int delta=height()-2*offset-textHeight();
  delta=(delta>0)?delta>>1:0;
  return offset+delta+textAscent();
}

void MSTextField::computeSize(void)
{
  unsigned offset=highlightThickness()+shadowThickness()+yMargin();
  height(textHeight()+2*offset);
}

unsigned MSTextField::computeVisibleLength(void) 
{
  unsigned offset=highlightThickness()+shadowThickness();
  int w=width()-(2*offset+xMargin());
  unsigned len=0;
  int tw=0;
  const char *pString=string();
  for (unsigned pos=scrollIndex();pos<length();pos++)
   {
     tw+=charWidth(pString[pos]);
     if (tw<=w) len++;
     else break;
   }
  return (len<length())?len:length();
}

unsigned MSTextField::computeCursorPosition(unsigned x_) 
{
  unsigned pos=0;
  if (x_<=textX()||length()==0) pos=0;
  else
   {
     int xpress=x_-textX();
     const char *pString=string();
     unsigned sum=0;
     if (editMode()==InsertMode)
      {
        for (pos=scrollIndex();pos<length()&&sum+(charWidth(pString[pos])>>1)<xpress;pos++)
         { sum+=charWidth(pString[pos]); }
      }
     else
      {
        for (pos=scrollIndex();pos<length()&&sum+charWidth(pString[pos])<xpress;pos++)
         { sum+=charWidth(pString[pos]); }
      }
   }
  return pos;
}

unsigned MSTextField::positionToX(unsigned position_) 
{
  if (position_>length()) position_=length();
  const char *pString=string();
  return (textX()+textWidth(pString+scrollIndex(),position_-scrollIndex())-1);
}

void MSTextField::clearCursor(void) 
{ if (cursorOn()==MSTrue) drawCursor(); }

void MSTextField::drawCursor(void)
{
  if (blinkOn()==MSTrue&&haveFocus()==MSTrue)
   { 
     _cursorOn=(cursorOn()==MSTrue)?MSFalse:MSTrue;
     (editMode()==InsertMode)?drawInsertCursor():drawOverstrikeCursor();
   }
}

void MSTextField::drawInsertCursor(void)
{
  if (cursorPixmap()!=0&&imageMSGC()!=0)
   {
     int offset=shadowThickness()+highlightThickness()+xMargin();
     unsigned pos=cursorPosition();
     int xx=positionToX(pos);
     xx-=(cursorPixmap()->width()>>1)-1; 
     int src_x=0;
     int src_w=cursorPixmap()->width();
     
     if (xx<offset)
      {
	src_w=cursorPixmap()->width()-(offset-xx);
	src_x=cursorPixmap()->width()-src_w;
	xx=offset;
      }     
     else if (xx>width()-offset) src_w=cursorPixmap()->width()-xx+width()-offset;

     int yy=(textY()-textAscent())+(textHeight()-cursorPixmap()->height());
     XCopyArea(display(),cursorPixmap()->pixmap(),window(),imageGC(),
	       src_x,0,src_w,cursorPixmap()->height(),xx,yy);
   }
}

void MSTextField::drawOverstrikeCursor(void)
{
  if (imageMSGC()!=0)
   {
     unsigned pos=cursorPosition();     
     unsigned x=positionToX(pos)+1;
     if (pos<length())
      {
	const char *pString=string();
	if (cursorOn()==MSTrue)
	 {
	   XSetForeground(display(),textGC(),background());
	   XSetBackground(display(),textGC(),foreground());
	 }
	XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+pos,1);        
	if (cursorOn()==MSTrue)
	 {
	   XSetForeground(display(),textGC(),foreground());
	   XSetBackground(display(),textGC(),background());
	 }
      }
     else if (pos==length())
      {
	XFillRectangle(display(),window(),imageGC(),x,textY()-textAscent(),charWidth('M'),textHeight());
      }
   }
}

void MSTextField::cursorPosition(unsigned position_)
{ moveCursor(position_); }

void MSTextField::cursorPositionForw(unsigned position_)
{
  if(masking() == MSTrue) position_= _inputMask.indexOf(inputMaskCharacter(),position_);
  moveCursor(position_);
}

void MSTextField::cursorPositionBack(unsigned position_)
{
  if(masking() == MSTrue)
   {
     unsigned newPos = _inputMask.lastIndexOf(inputMaskCharacter(), position_);
     if(newPos == _inputMask.length()) newPos = 0;
     moveCursor(newPos);
   }
  else moveCursor(position_);
}

  
void MSTextField::moveCursor(unsigned newPosition_)
{
  unsigned si=scrollIndex();
  if (newPosition_!=cursorPosition())
   {
     unsigned len=computeVisibleLength();
     if (newPosition_>=length())
      {
	newPosition_=length();
	si=length()-len;
      }
     else if (scrollIndex()>0||cursorPosition()>=len)
      {
	if (newPosition_>cursorPosition())
	 {
	   if (scrollIndex()+len==cursorPosition())
	    {
	      si+=(newPosition_-cursorPosition());
	      si=si<=(maxLength()-len)?si:(maxLength()-len);
	    }
	 }
	else if (newPosition_<cursorPosition())
	 {
	   if (scrollIndex()>0&&cursorPosition()==scrollIndex()) 
	    {
	      si-=(cursorPosition()-newPosition_);
	      si=(si>0)?si:0;
	    }
	   else if (scrollIndex()>0&&newPosition_<scrollIndex()) si=newPosition_;
	 }
      }
     if (cursorPosition()!=newPosition_)
      {
	clearCursor();
	_cursorPosition=newPosition_;  
	if (si!=scrollIndex()) 
	 {
	   _scrollIndex=si;
	   drawText();
	 }
	drawCursor();
      }
   }
}

void MSTextField::drawText(MSBoolean clear_)
{
  if (clear_==MSTrue) clearTextArea();
  if (string()!=0)
   {
     const char *pString=string();
     unsigned len=computeVisibleLength();
     if (length()>0)
      {
	unsigned startPos=scrollIndex();
	unsigned x=textX();
	if (selectionStart()!=selectionEnd())
	 {
	   if (selectionStart()==0&&selectionEnd()==length())
	    {
	      XSetForeground(display(),textGC(),background());
	      XSetBackground(display(),textGC(),foreground());
	      XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos,len);	      
	    }
	   else
	    {
	      // draw in three sections
	      unsigned endPos=startPos+len;
	      unsigned i,dl1=0,dl2=0,dl3=0;
	      // section 1 - unselected
	      for (i=startPos;i<endPos;i++)
	       {
		 if (i<selectionStart()) dl1++;
		 else break;
	       }
	      if (dl1>0)
	       {
		 XSetForeground(display(),textGC(),foreground());
		 XSetBackground(display(),textGC(),background());
		 XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos,dl1);
		 x+=textWidth(pString+startPos,dl1);
	       }
	      // section 2 - selected
	      startPos+=dl1;
	      for (i=startPos;i<endPos;i++)
	       {
		 if (i<selectionEnd()) dl2++;
		 else break;
	       }
	      if (dl2>0)
	       {
		 XSetForeground(display(),textGC(),background());
		 XSetBackground(display(),textGC(),foreground());
		 XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos,dl2);
		 x+=textWidth(pString+startPos,dl2);
	       }
	      // section 3 - unselected	      
	      startPos+=dl2;
	      if (startPos<endPos)
	       {
		 dl3=endPos-startPos;
		 XSetForeground(display(),textGC(),foreground());
		 XSetBackground(display(),textGC(),background());
		 XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos,dl3);
	       }
	    }
	 }
	else
	 {
	   XSetForeground(display(),textGC(),foreground());
	   XSetBackground(display(),textGC(),background());
	   XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos,len);
	 }
      }
     if (haveFocus()==MSTrue) XFlush(display());
   }
}

void MSTextField::drawText(unsigned startPos_)
{
  if (scrollIndex()>0) drawText();
  else if (string()!=0)
   {
     const char *pString=string();
     int offset=shadowThickness()+highlightThickness();
     unsigned x=textX()+textWidth(string(),startPos_);
     int w=width()-(offset+xMargin());
     int tw=x;
     unsigned len=0;
     unsigned long fg=foreground(),bg=background();
     for (int pos=startPos_;pos<length();pos++)
      {
        tw+=charWidth(pString[pos]);
        if (tw<=w) len++;
        else break;
      }
     len=(len<length())?len:length();
     XFillRectangle(display(),window(),backgroundShadowGC(),
                    x,offset,width()-offset-x,height()-(offset<<1));
     if (length()>0)
      {
	if (selectionStart()!=selectionEnd())
	 {
	   if (selectionStart()==0&&selectionEnd()==length())
	    {
	      XSetForeground(display(),textGC(),background());
	      XSetBackground(display(),textGC(),foreground());
	      XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos_,len);	      
	    }
	   else
	    {
	      // draw in three sections
	      unsigned startPos=startPos_;
	      unsigned endPos=startPos_+len;
	      unsigned i,dl1=0,dl2=0,dl3=0;
	      // section 1 - unselected
	      for (i=startPos;i<endPos;i++)
	       {
		 if (i<selectionStart()) dl1++;
		 else break;
	       }
	      if (dl1>0)
	       {
		 XSetForeground(display(),textGC(),foreground());
		 XSetBackground(display(),textGC(),background());
		 XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos,dl1);
		 x+=textWidth(pString+startPos,dl1);
	       }
	      // section 2 - selected
	      startPos+=dl1;
	      for (i=startPos;i<endPos;i++)
	       {
		 if (i<selectionEnd()) dl2++;
		 else break;
	       }
	      if (dl2>0)
	       {
		 XSetForeground(display(),textGC(),background());
		 XSetBackground(display(),textGC(),foreground());
		 XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos,dl2);
		 x+=textWidth(pString+startPos,dl2);
	       }
	      // section 3 - unselected
	      startPos+=dl2;
	      if (startPos<endPos)
	       {
		 dl3=endPos-startPos;
		 XSetForeground(display(),textGC(),foreground());
		 XSetBackground(display(),textGC(),background());
		 XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos,dl3);
	       }
	    }
	 }     
	else
	 {
	   XSetForeground(display(),textGC(),foreground());
	   XSetBackground(display(),textGC(),background());
	   XDrawImageString(display(),window(),textGC(),textFontStruct(),x,textY(),pString+startPos_,len);
	 }
      }
     if (haveFocus()==MSTrue) XFlush(display());
   }
}

void MSTextField::configure(void)
{
  unsigned offset=highlightThickness()+shadowThickness();
  XRectangle clipRect[1];
  clipRect[0].x=0;
  clipRect[0].y=0;
  clipRect[0].width=width()-2*offset;
  clipRect[0].height=height()-2*offset;
  XSetClipRectangles(display(),textGC(),offset,offset,&clipRect[0],1,Unsorted);

  _scrollIndex=0;
  redraw();
}

void MSTextField::defaultDoubleClickBehavior(const XEvent *)
{ if (masking() == MSFalse) selectAll(); }

void MSTextField::buttonPress(const XEvent *pEvent_)
{ if (sensitive()==MSTrue) buttonPressNotify(this,pEvent_); }

void MSTextField::button1Press(const XEvent *pEvent_)
{
  if (isSelected()==MSTrue)
   {
     clearSelection();
     clearCursor();
     drawText(MSFalse);
   }
  startEditing(InsertMode,pEvent_->xbutton.x);
  if (isDoubleClick(pEvent_)==MSTrue) defaultDoubleClickBehavior(pEvent_);
  else trackSelection(pEvent_);
}

void MSTextField::button2Press(const XEvent *pEvent_)
{
  if (isSelected()==MSTrue)
   {
     clearSelection();
     clearCursor();
     drawText(MSFalse);
   }
  startEditing(OverstrikeMode,pEvent_->xbutton.x);  
}

void MSTextField::button3Press(const XEvent *pEvent_)
{
  if (isSelected()==MSTrue)
   {
     clearSelection();
     clearCursor();
     drawText(MSFalse);
   }
  startEditing(InsertMode,pEvent_->xbutton.x);
}

void MSTextField::startEditing(MSTextField::EditingMode mode_,unsigned x_)
{
  // forse OverstrikeMode if we are masking.
  if(masking() == MSTrue) mode_ = OverstrikeMode;
  if (editMode()!=mode_)
   {
     clearCursor();
     _editMode=mode_;
   }
  cursorPositionForw(computeCursorPosition(x_)); 
}

void MSTextField::firstMapNotify(void)
{
  createGCs();
  if (width()==MSDefaultWidth&&height()==MSDefaultHeight) computeSize();
  makeIBeamCursor();
}

void MSTextField::redraw(void)
{
  if (mapped()==MSTrue)
   {
     drawBackground();
     drawShadow();
     drawText();
     if (haveFocus()==MSTrue&&cursorOn()==MSTrue)
      { (editMode()==InsertMode)?drawInsertCursor():drawOverstrikeCursor(); }
   }
}

void MSTextField::clearTextArea(void)
{
  unsigned offset=shadowThickness()+highlightThickness();
  XFillRectangle(display(),window(),backgroundShadowGC(),
                 offset,offset,width()-2*offset,height()-2*offset);
}

void MSTextField::string(const char *pString_)
{
  if (pString_!=0)
   {
     clearSelection();
     stopBlinking();
     unsigned newPos;
     
     if(_inputMask.length() > 0)
      {
        MSString aString = pString_;
        if(aString == "")
         {
           _masking = MSTrue;
           maxLength(_inputMask.length());
           editMode(OverstrikeMode);
           _string = _inputMask;
           newPos = firstCursorPosition();
         }
        else
         {
           // go to mask mode only if string matches with the mask.
           _masking=matchStringToMask(aString);
           if(_masking == MSFalse) maxLength(MSTextFieldDefaultMaxLength);
           else 
            {
              maxLength(_inputMask.length());
              editMode(OverstrikeMode);
            }
           _string = aString;
           newPos = length();
         }
      }
     else
      {
        unsigned len=strlen(pString_);
        if (len<=maxLength()) _string=pString_; 
        else _string="";
        newPos = length();
      }
     
     clearCursor();
     clearTextArea(); 
     _scrollIndex=0;
     _cursorPosition=0;
     moveCursor(newPos);
     drawText();
     drawCursor();
     startBlinking();
   }
}

void MSTextField::insertString(const char *pString_)
{
  if (pString_!=0)
   {
     unsigned slen=strlen(pString_);
     MSBoolean fullRedraw=MSFalse;
     stopBlinking();

     if (selectionStart()!=selectionEnd())
      {
        _string.remove(selectionStart(),selectionEnd()-selectionStart());
        cursorPositionForw(selectionStart());
	_string.insert(pString_,selectionStart());
	clearSelection();
	fullRedraw=MSTrue;
      }
     else if (editMode()==InsertMode||cursorPosition()==length())
      {
        if (length()+slen<=maxLength()) (void)_string.insert(pString_,cursorPosition());
	else 
	 {
	   verifyBell();
	   startBlinking();
	   return;
	 }
      }
     else (void)_string.overlayWith(pString_,cursorPosition());

      unsigned oldPos=cursorPosition();
      int offset=highlightThickness()+shadowThickness();
      int tw=width()-(2*offset+xMargin());
      const char *pString=string();
      cursorPositionForw(cursorPosition()+slen);           
      if (textWidth(pString+scrollIndex(),cursorPosition()-scrollIndex())>tw)
       {
	 unsigned len=0;
	 for (int pos=cursorPosition()-1;tw>0&&pos>=0;pos--)
	  {
	    tw-=charWidth(pString[pos]);
	    if (tw>=0) len++;
	  }
	 _scrollIndex=cursorPosition()-len;
	 drawText();
       }
     else if (fullRedraw==MSFalse) drawText(oldPos);
     else drawText();
     
     drawCursor();  
     startBlinking();
     clearSelection();
   }
  else verifyBell();
}

void MSTextField::deleteString(unsigned startIndex_,unsigned numChars_)
{
  if (startIndex_<text().length())
   {
     stopBlinking();
     if(masking() == MSTrue)
      {
        int oldStart =startIndex_;
        startIndex_ = _inputMask.lastIndexOf(inputMaskCharacter(), startIndex_);
        numChars_ += oldStart - startIndex_;
        _string.overlayWith(_inputMask.subString(startIndex_,numChars_),startIndex_);
      }
     else _string.remove(startIndex_,numChars_);

     if (cursorPosition()!=scrollIndex()||startIndex_>=cursorPosition())
      {
        if (scrollIndex()>0)
	 {
	   _scrollIndex-=numChars_;
	   _scrollIndex=(scrollIndex()>0)?scrollIndex():0;
	 }
      }
     else _scrollIndex=startIndex_;
     cursorPosition(startIndex_);
     clearSelection();     
     drawText();  	   
     drawCursor();  	   
     startBlinking();
   }
  else verifyBell();
}

void MSTextField::focusIn(void)
{
  _haveFocus=MSTrue;
  highlight();
  startBlinking();
}

void MSTextField::focusOut(void)
{
  unHighlight();
  stopBlinking();
  _haveFocus=MSFalse;
}

void MSTextField::keyPress(const XEvent *pEvent_,KeySym keysym_,unsigned int state_,const char *pString_)
{
  MSKeyPress keyPress(keysym_,state_);
  if (sensitive()==MSTrue)
   {
     if (keyTranslationTable()->hasMatch(keyPress)==MSTrue) 
      {
        if( keyTranslate(keyPress) ==MSTrue) return;
      }
     else if (keyTranslationFunction()!=0)
      {
        const char *pString=(*_keyTranslationFunction)(pEvent_);
        if (pString!=0)
	 {
           insertString(pString);
           return;
	 }
      }
     if (isprint(pString_[0])) insertString(pString_);
     else key(keysym_,state_,pString_);
   }
}

void MSTextField::makeIBeamCursor(void)
{
  if (firstMap()==MSTrue)
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

void MSTextField::createGCs(void)
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

void MSTextField::selfInsert(void)
{
  insertString(selfInsertData());  
  selfInsertData(0);
}

void MSTextField::kill(void)
{
  if(masking() == MSFalse)
   {
     const char *pString=string();
     yankBuffer()=(pString+cursorPosition());
     deleteString(cursorPosition(),length()-cursorPosition());
   }
}

void MSTextField::yank(void)
{ if (masking() == MSFalse && yankBuffer().length()>0) insertString(yankBuffer()); }
void MSTextField::tab(void)          
{ activate(); }

void MSTextField::home(void)         
{
  if (selectionStart()!=selectionEnd())
   {
     clearSelection();
     clearCursor();
     drawText();
   }
  cursorPosition(firstCursorPosition());
} 

void MSTextField::end(void)          
{
  if (selectionStart()!=selectionEnd())
   {
     clearSelection();
     clearCursor();     
     drawText();
   }
  moveCursor(length());
} 

void MSTextField::deleteChar(void)   
{ deleteString(cursorPosition(),1); }

void MSTextField::backspace(void)    
{
  if (selectionStart()!=selectionEnd())
   {
     unsigned startPos=selectionStart(),numChars=selectionEnd()-selectionStart();
     clearSelection();
     deleteString(startPos,numChars);
   }
  else if (cursorPosition() > firstCursorPosition()) deleteString(cursorPosition()-1,1); 
  else verifyBell();
}

void MSTextField::forwardChar(void)  
{
  if (selectionStart()!=selectionEnd())
   {
     clearSelection();
     clearCursor();     
     drawText();
   }
  cursorPositionForw(cursorPosition()+1);
}
void MSTextField::backwardChar(void) 
{
  if (selectionStart()!=selectionEnd())
   {
     clearSelection();
     clearCursor();     
     drawText();
   }
  if (cursorPosition() > firstCursorPosition()) cursorPositionBack(cursorPosition()-1);
}

void MSTextField::returnKey(void)
{ activate(); }

void MSTextField::insertKey(void)
{
  if(masking() == MSFalse)
   {
     clearCursor();
     _editMode=(editMode()==InsertMode)?OverstrikeMode:InsertMode;
   }
}


void MSTextField::color(unsigned long fg_,unsigned long bg_) 
{ 
  if (fg_!=foreground()||bg_!=background())
   { 
     // try to optimize GC change by doing this here
     if (firstMap()==MSTrue&&imageMSGC()!=0) imageMSGC()->color(fg_^bg_,bg_);
     foreground(fg_);
     background(bg_);
   }
}

void MSTextField::updateBackground(unsigned long oldbg_)
{ MSPrimitiveText::updateBackground(oldbg_);updateCursor(); }
void MSTextField::updateForeground(unsigned long oldfg_)
{ MSPrimitiveText::updateForeground(oldfg_);updateCursor(); }
void MSTextField::updateFont(Font oldfid_)
{ MSPrimitiveText::updateFont(oldfid_);updateCursor(); }

void MSTextField::updateCursor(void) 
{
  if (firstMap()==MSTrue)
   {
     clearCursor(); 
     createGCs();
     makeIBeamCursor(); 
     redraw(); 
   }
}

void MSTextField::scrollIndex(unsigned scrollIndex_)
{
  if (_scrollIndex!=scrollIndex_)
   {
     _scrollIndex=scrollIndex_;
     redraw();
   }
}

unsigned MSTextField::firstCursorPosition(void) const
{ return (masking() == MSFalse) ? 0 :inputMask().indexOf(inputMaskCharacter()); }

void MSTextField::inputMask(const MSString& mask_)
{
  _inputMask = mask_;
  if(_inputMask == "")
   {
     maxLength(MSTextFieldDefaultMaxLength);
     _masking = MSFalse;
   }
  else
   {
     string(_inputMask);
     _masking = MSTrue;
     maxLength(_inputMask.length());
     editMode(OverstrikeMode);
   }
}

MSBoolean MSTextField::matchStringToMask(const MSString& aString_) const
{
  MSBoolean match = MSFalse;
  if(_inputMask.length() == aString_.length())
   {
     int i;
     for(i=0; i <_inputMask.length(); i++)
      {
        if(_inputMask(i)!=inputMaskCharacter() && _inputMask(i) != aString_(i)) break;
      }
     if (i == _inputMask.length()) match = MSTrue;
   }
  return match;
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSTextField::activate(void) {}   
void MSTextField::escape(void) {}   
void MSTextField::shiftTab(void) {}
void MSTextField::up(void) {}
void MSTextField::down(void) {}

MSBoolean MSTextField::isSelected(void) const
{
  if (selectionStart()!=0||selectionEnd()!=0) return MSTrue;
  else return MSFalse;
}

void MSTextField::trackSelection(const XEvent *pEvent_)
{
  unsigned len=length();
  if (len>0)
   {
     int startX=pEvent_->xbutton.x;
     int startPos=computeCursorPosition(startX);
     int pos=startPos;
     int lastPos=startPos;
     Window root,child;
     int rx,ry,winx,winy;
     unsigned keys;
     int sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&winx,&winy,&keys);
     while (keys&Button1Mask)
      {
        if (sameScreen==True)
         {
           if (winx<=(int)textX())
            {
              if (scrollIndex()>0)
               {
                 _scrollIndex--;
               }
              if (--pos<0) pos=0;
            }
           else if (winx>width())
            {
              unsigned len=computeVisibleLength();
              if (scrollIndex()+len<length())
               {
                 _scrollIndex++;
               }
              pos=scrollIndex()+len;
            }
           else pos=computeCursorPosition(winx);
           if (pos!=lastPos)
            {
              if (startPos<pos)
               {
                 _selectionStart=startPos;
                 _selectionEnd=pos;
               }
              else
               {
                 _selectionStart=pos;
                 _selectionEnd=startPos;
               }
              drawText(MSFalse);
              _cursorPosition=pos;
              lastPos=pos;
            }
         }
        sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&winx,&winy,&keys);
      }
   }
}

