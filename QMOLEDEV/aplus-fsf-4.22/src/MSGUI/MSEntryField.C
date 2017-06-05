///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSEntryField.H>
#include <MSTypes/MSUtil.H>

static const unsigned long MSEntryFieldDefaultCycleInterval=1000;
static const unsigned long MSEntryFieldEventMask=(ExposureMask|ButtonPressMask|ButtonReleaseMask);
static const unsigned MSEntryFieldEditorShadowThickness=2;
static const unsigned MSEntryFieldEditorHighlightThickness=0;

MSEntryField::CycleTimer::CycleTimer(MSEntryField *entryField_,unsigned long interval_) : 
MSIntervalTimer(interval_) 
{ _entryField=entryField_; }

MSEntryField::CycleTimer::~CycleTimer(void)
{}

void MSEntryField::CycleTimer::process(void) 
{ _entryField->processCycleTimer(); }

MSEntryField::FieldEditor::FieldEditor(MSWidget *owner_) : MSTextField(owner_)
{
  _highlightThickness=MSEntryFieldEditorHighlightThickness;
  _shadowThickness=MSEntryFieldEditorShadowThickness;
  margin(0);
  color(owner()->background(),owner()->foreground());
}

MSEntryField::FieldEditor::~FieldEditor(void)
{}

void MSEntryField::FieldEditor::returnKey(void) 
{ if (owner()!=0) ((MSEntryField *)owner())->returnKey(); }
void MSEntryField::FieldEditor::activate(void) 
{ if (owner()!=0) ((MSEntryField *)owner())->activate(); }
void MSEntryField::FieldEditor::escape(void)   
{ if (owner()!=0) ((MSEntryField *)owner())->escape(); }
void MSEntryField::FieldEditor::up(void)
{ if (owner()!=0) ((MSEntryField *)owner())->up(); }
void MSEntryField::FieldEditor::down(void)
{ if (owner()!=0) ((MSEntryField *)owner())->down(); }

MSEntryField::MSEntryField(MSWidget *owner_,const MSSymbol& tag_) : 
MSCompositeField(owner_,tag_) 
{ 
  init(); 
}

MSEntryField::MSEntryField(MSWidget *owner_,const char *label_,const MSSymbol& tag_) : 
MSCompositeField(owner_,label_,tag_) 
{ 
  init(); 
}

MSEntryField::~MSEntryField(void)
{
  removeCycle();
  if (_cycleTimer!=0)  delete _cycleTimer;
  if (_editor!=0) safeDestroy(_editor);
}

void MSEntryField::init(void)
{
  freeze();
  _hadFocus=MSFalse;
  _cycle=0;
  _cycleTimer=0;
  _cycleInterval=MSEntryFieldDefaultCycleInterval;
  _cycleColorMode=MSForeground;
  _editor=0;
  _editor=new FieldEditor(this);
  _autoMaskInput=MSFalse;  
  selectInput(MSEntryFieldEventMask);

  _selectionStart=-1;
  _selectionEnd=-1;
  _firstCharacter=0;
  _insertCursor=-1;
  _supportPasting=MSTrue;
  _pasting=MSFalse;
}

void MSEntryField::updateBackground(unsigned long oldbg_)
{
  MSCompositeField::updateBackground(oldbg_);
  if (oldbg_==editorForeground()) editorForeground(background());
}

void MSEntryField::updateForeground(unsigned long oldfg_)
{
  MSCompositeField::updateForeground(oldfg_);
  if (oldfg_==editorBackground()) editorBackground(foreground());  
}

void MSEntryField::updateFont(Font oldfid_)
{
  MSCompositeField::updateFont(oldfid_);
}

void MSEntryField::updateSensitivity(void)
{
  if (sensitive()==MSFalse)
   {
     _firstCharacter=0;
     _selectionStart=-1;
     _selectionEnd=-1;
     disownSelection(XA_PRIMARY);
     removeCycle();
   }
  MSCompositeField::updateSensitivity();
}

void MSEntryField::placement(void)
{
   if (_editor->mapped()==MSTrue) unmapEditor();
   MSCompositeField::placement();
}

void MSEntryField::editorBackground(const char *color_) 
{ _editor->background(color_); } 
void MSEntryField::editorForeground(const char *color_) 
{ _editor->foreground(color_); } 

void MSEntryField::editorBackground(unsigned long pixel_) 
{ _editor->background(pixel_); } 
void MSEntryField::editorForeground(unsigned long pixel_) 
{ _editor->foreground(pixel_); } 

void MSEntryField::updateData(void) 
{
  clearSelection(MSTrue);
  disownSelection(XA_PRIMARY);
}

void MSEntryField::update(const MSIndexVector&) 
{
  if (pasting()==MSFalse)
   {
     clearSelection(MSFalse);
     disownSelection(XA_PRIMARY);
   }
  if (frozen()==MSFalse&&mapped()==MSTrue)  
   {
     createCycle();
     XFlush(display()); // get update out to the screen asap
   }
}

void MSEntryField::editSelection(void)
{
  if (_editor->mapped()==MSFalse)
   {
     MSString buffer;
     const char *pString=formatOutput(buffer);
     if (pString!=0) _editor->string(pString);
     else _editor->string(""); 
     _editor->cursorPosition(_editor->firstCursorPosition());
     mapEditor();
   }
}

void MSEntryField::edit(void) 
{ editSelection(); }

void MSEntryField::configureEditor(void)
{
  int sht = valueShadowThickness();
  _editor->shadowThickness((sht < MSEntryFieldEditorShadowThickness)?
                           sht : MSEntryFieldEditorShadowThickness);

  if (_editor->inputMaskMode() == MSFalse) _editor->maxLength(editWidth());
  _editor->font(_fieldValue->font());
  _editor->color(editorForeground(),editorBackground());
  _editor->moveTo(_fieldValue->x(),_fieldValue->y());
  _editor->resize(_fieldValue->width(),_fieldValue->height());
}

void MSEntryField::clearEditor(void) 
{ _editor->string(""); }

void MSEntryField::mapEditor(void)
{
  configureEditor();
  _editor->map();
  focusInNotify(_editor);
  activateCallback(MSWidgetCallback::editbegin);
}

void MSEntryField::unmapEditor(void)
{
  focusOutNotify(_editor);
  _editor->unmap();
  clearEditor();
  activateCallback(MSWidgetCallback::editend);
}

MSBoolean MSEntryField::validateInput(MSString &input_)
{
  if (_validationCallback!=0)
   {
     // If user validation returns MSFalse, just return
     if (_validationCallback->validate(input_)==MSFalse)
      {
        return MSFalse;
      }
   }
  return (validate(input_.string()));
}

void MSEntryField::activate(void)
{
  if (_editor->mapped()==MSTrue)
   {
     if (hasModel()==MSTrue)
      {
	MSString aString=_editor->string();
        if (validateInput(aString)==MSTrue)
         {
           unmapEditor();
           valueChange();
         }
      }
     else escape();
   }
}

MSBoolean MSEntryField::activateEditor(void)
{
  activate();
  return (_editor->mapped()==MSTrue)?MSFalse:MSTrue;
}

void MSEntryField::escape(void)
{
  unmapEditor();
  drawFieldValue();
}

void MSEntryField::returnKey(void)
{
  if (_editor->mapped()==MSTrue)
   {
     if (activateEditor()==MSTrue) activateCallback(MSWidgetCallback::activate);
   }
  else reference();
}
   
void MSEntryField::valueChange(void) 
{ activateCallback(MSWidgetCallback::valuechange); }
void MSEntryField::reference(void)   
{ activateCallback(MSWidgetCallback::reference); }

void MSEntryField::buttonPress(const XEvent *pEvent_)
{
  if (sensitive()==MSTrue)
   {
     if (_editor->mapped()==MSTrue)
      {
	XEvent *ep=(XEvent *)pEvent_;
	ep->xbutton.x-=_editor->x_origin();
	ep->xbutton.y-=_editor->y_origin();
	buttonPressNotify(_editor,ep);
      }
     else 
      {
        MSBoolean focusOK;
        if (acceptFocus()==MSTrue)
         {
           _hadFocus=(inputFocus()==this)?MSTrue:MSFalse;
           focusOK=traverseFocus(this);
         }
        else
         {
           _hadFocus=MSFalse;
           focusOK=MSTrue;
         }
	if (focusOK==MSTrue)
	 {
           if (pEvent_->xbutton.x>=_fieldValue->x()&&pEvent_->xbutton.y>=_fieldValue->y())
            {
	      buttonPressNotify(this,pEvent_);
	    }
	 }
      }
   }
}

void MSEntryField::button1Press(const XEvent *pEvent_)
{
  clearSelection(MSFalse);
  trackSelection(pEvent_);
}

void MSEntryField::button2Press(const XEvent *pEvent_) 
{
  if (isProtected()==MSFalse)
   {
     if (supportPasting()==MSTrue)
      {
        MSString buffer;
        formatOutput(buffer);
        int pos=locateCursorPosition(pEvent_->xbutton.x,buffer);
        if (pos==-1) pos=0;
        if (server()->primarySelectionOwner()==this)
         {
           if (selectionStart()==-1||selectionEnd()==-1||(pos>selectionStart()&&pos<=selectionEnd()))
            {
              server()->bell();
            }
           else
            {
              MSString buffer;
              formatOutput(buffer);
              MSString subString=buffer.subString(selectionStart(),selectionEnd()-selectionStart()+1);
              insertString(pos,subString);
            }
         }
        else
         {
           insertCursor(pos);
	   convertSelection();
         }
      }
     else startEditing(pEvent_);
   }
  else server()->bell();
}

void MSEntryField::button3Press(const XEvent *pEvent_) 
{ if (isProtected()==MSFalse) startEditing(pEvent_); }

void MSEntryField::startEditing(const XEvent *pEvent_)
{
  XEvent *ep=(XEvent *)pEvent_;
  ep->xbutton.x-=_fieldValue->x();
  ep->xbutton.y-=_fieldValue->y();
  editSelection();
  buttonPressNotify(_editor,ep);
}

void MSEntryField::keyPress(const XEvent *pEvent_,KeySym keysym_,
			    unsigned int state_,const char *buffer_)
{
  MSKeyPress keyPress( keysym_,state_);
  if (sensitive()==MSTrue &&  keyTranslate(keyPress)==MSFalse)
   {
     if (_editor->mapped()==MSTrue) keyPressNotify(_editor,pEvent_,keysym_,state_,buffer_);
     else 
      {
	if (keysym_==XK_Return) returnKey();
	else if (isProtected()==MSFalse)
	 {
           if (keysym_==XK_KP_Add) increment();
           else if (keysym_==XK_KP_Subtract||keysym_==XK_F24) decrement();
           else if (keysym_==XK_Up) up();
           else if (keysym_==XK_Down) down();
           else if (keysym_==XK_Left) left();
           else if (keysym_==XK_Right) right();
	   else if (keysym_==XK_Insert) editSelection();
	   else if (keysym_==XK_BackSpace)
	    {
              if (isSelected()==MSTrue)
               {
                 updateEditor();
               }
              else clearEditor();
	      mapEditor();
	    }
	   else if (strlen(buffer_)>0)
	    {
              if (isSelected()==MSTrue)
               {
                 updateEditor();
               }
              else clearEditor();
              if(_editor->inputMaskMode()==MSFalse)
               {
                 _editor->editMode(MSTextField::InsertMode);
                 keyPressNotify(_editor,pEvent_,keysym_,state_,buffer_);
                 if (_editor->length()>0) mapEditor();
               }
              else
               {
                 _editor->editMode(MSTextField::OverstrikeMode);
                 keyPressNotify(_editor,pEvent_,keysym_,state_,buffer_);
                 int pos = _editor->firstCursorPosition();
                 if(_editor->text()(pos) != inputMask()(pos)) mapEditor();
               }
	    }
	 }
      }
   }
}

void MSEntryField::drawFieldValue(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue&&hasModel()==MSTrue)
   {
     unsigned long fg,bg;
     currentColors(fg,bg);
     drawFieldValue(fg,bg);
   }
}

void MSEntryField::removeCycle(void)
{
  if (cycle()!=0)
   {
     delete _cycle;
     _cycle=0;
   }
  if (cycleTimer()!=0) cycleTimer()->stop();
}

void MSEntryField::processCycleTimer(void)
{
  cycle()->increment();
  if (cycle()->count()==cycle()->numCycles())
   {
     cycleTimer()->stop();
     drawFieldValue();
   }
  else
   {
     cycleValue();
   }
}

void MSEntryField::createCycle(void)
{
  if (cycleColorMode()==MSReverseVideo||cycleColors().length()>0) startCycle(cycleColors());
  else drawFieldValue();
}

void MSEntryField::cycleInterval(unsigned long interval_)
{
  if (cycleInterval()!=interval_)
   {
     _cycleInterval=interval_;
     MSBoolean stop=(cycle()==0)?MSTrue:MSFalse;
     if (cycleTimer()!=0) delete _cycleTimer;
     _cycleTimer=new CycleTimer(this,cycleInterval());
     if (stop==MSTrue) cycleTimer()->stop();
   }
}

void MSEntryField::startCycle(const MSUnsignedLongVector& colors_)
{
  if (cycleTimer()==0) _cycleTimer=new CycleTimer(this,cycleInterval());
  else cycleTimer()->reset();
  if (_cycle==0)
   {
     _cycle=new MSColorCycle(this,0,0,colors_,cycleColorMode());
   }
  else
   {
     cycle()->reset();
   }
  cycleValue();
}

void MSEntryField::cycleValue(void)
{
  unsigned long fg,bg;
  currentColors(fg,bg);
  drawFieldValue(fg,bg);
}

void MSEntryField::cycleColors(const MSStringVector& colors_)
{
   MSUnsignedLongVector colors(colors_.length());
   for (int i=0;i<colors_.length();i++) colors[i]=(server()->pixel(colors_(i)));
   cycleColors(colors);
}

void MSEntryField::cycleColors(const MSUnsignedLongVector& colors_)
{
  MSBoolean redrawNecessary=MSFalse;
  if (cycle()!=0&&cycle()->count()<cycle()->numCycles())
   {
     redrawNecessary=MSTrue;
   }
  removeCycle();
  _cycleColors=colors_;
  if (redrawNecessary==MSTrue) drawFieldValue();
}

void MSEntryField::focusIn(void) 
{
  highlight(); 
  if (_editor->mapped()==MSTrue) focusInNotify(_editor); 
}

void MSEntryField::focusOut(void) 
{ 
  unHighlight(); 
  if (_editor->mapped()==MSTrue) focusOutNotify(_editor); 
}

MSBoolean MSEntryField::loseFocus(void) 
{
  if (_editor->mapped()==MSTrue) activate(); 
  if (_editor->mapped()==MSTrue) return MSFalse;
  else 
   {
     unHighlight(); 
     return MSTrue; 
   }
}

void MSEntryField::format(const MSFormat& aFormat_)
{
  if (format().formatType()==MSFormat::NoFormat||
      aFormat_.formatType()==format().formatType())
   {
     _format=aFormat_;
     drawFieldValue();
     if(autoMaskInput() == MSTrue) generateInputMask();
   }
}

void MSEntryField::set(MSAttrValueList& avList_)
{
  MSCompositeField::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     // use member functions inherited from MSCompositeField
     if (avList_[i].attribute()=="editorForeground")
      editorForeground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="editorBackground")
      editorBackground(avList_[i].value()),index<<i;
     else if (avList_[i].attribute()=="format")
      format(MSFormat(avList_[i].value())),index<<i;
     else if(avList_[i].attribute()=="inputMask")
       inputMask(avList_[i].value()),index<<i;
     else if(avList_[i].attribute()=="inputMaskCharacter")
       inputMaskCharacter((avList_[i].value().length()>0)?avList_[i].value()(0):'-'),index<<i;
     else if(avList_[i].attribute()=="autoMaskInput")
       autoMaskInput(avList_[i].value().asBoolean()), index << i;
     else if (avList_[i].attribute()=="supportPasting")
      supportPasting(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="cycleColors")
      {
        cycleColors(MSAttrValue::stringToStringVector(avList_[i].value()));
        index<<i;
      }
     else if (avList_[i].attribute()=="cycleColorMode")
      {
	if (avList_[i].value()=="MSBackground") cycleColorMode(MSBackground);
	else if (avList_[i].value()=="MSReverseVideo") cycleColorMode(MSReverseVideo);
	else cycleColorMode(MSForeground);
	index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSEntryField::get(MSAttrValueList& avList_)
{
  MSStringVector booleanVector("MSFalse\nMSTrue");

  avList_<<MSAttrValue("editorForeground",
		       server()->colorName(editorForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("editorBackground",
		       server()->colorName(editorBackground()),MSAttrValue::Color);
  avList_<<MSAttrValue("format",format().asString(),format().formats(),MSAttrValue::String);  
  avList_<<MSAttrValue("autoMaskInput",(autoMaskInput()==MSTrue) ?"MSTrue":"MSFalse",
                       MSStringVector("MSFalse\nMSTrue"));
  avList_<<MSAttrValue("inputMask",inputMask(),MSAttrValue::String);
  avList_<<MSAttrValue("inputMaskCharacter",MSString(inputMaskCharacter()),MSAttrValue::Char);
  avList_<<MSAttrValue("supportPasting",booleanVector(supportPasting()),booleanVector);
  avList_<<MSAttrValue("cycleColors",MSAttrValue::colorVectorToString(cycleColors(),server()),
                       MSAttrValue::Color|MSAttrValue::List|MSAttrValue::StringVector);
  const char *value;
  switch (cycleColorMode())
  {
  case MSBackground:   value="MSBackground";   break;
  case MSReverseVideo: value="MSReverseVideo"; break;
  case MSForeground:
  default:             value="MSForeground";   break;
  }
  avList_<<MSAttrValue("cycleColorMode",value,
		       MSStringVector("MSBackground\nMSForeground\nMSReverseVideo"));
  avList_<<MSAttrValue("activate","",MSAttrValue::Callback);
  avList_<<MSAttrValue("valuechange","",MSAttrValue::Callback);  
  avList_<<MSAttrValue("editbegin","",MSAttrValue::Callback);
  avList_<<MSAttrValue("editend","",MSAttrValue::Callback);
  return MSCompositeField::get(avList_);
}

// ##################################################################
// inline methods
// ##################################################################

const MSFormat& MSEntryField::format(void) const
{return _format;}
MSColorCycle *MSEntryField::cycle(void) const                     
{ return _cycle; }
MSEntryField::CycleTimer *MSEntryField::cycleTimer(void) const
{ return _cycleTimer; }
MSEntryField::FieldEditor *MSEntryField::fieldEditor(void) const
{ return _editor; }

const MSUnsignedLongVector& MSEntryField::cycleColors(void) const
{ return _cycleColors; }
MSCycleColorMode MSEntryField::cycleColorMode(void) const
{ return _cycleColorMode;}
unsigned long MSEntryField::cycleInterval(void) const
{ return _cycleInterval; }
MSBoolean MSEntryField::editing(void) const
{ return _editor->mapped(); }
unsigned long MSEntryField::editorBackground(void) const
{ return _editor->background(); }   
unsigned long MSEntryField::editorForeground(void) const
{ return _editor->foreground(); } 

void MSEntryField::inputMask(const MSString& mask_)
{ if(autoMaskInput() == MSFalse) _editor->inputMask(mask_); }

const MSString& MSEntryField::inputMask(void) const
{ return _editor->inputMask(); }

char MSEntryField::inputMaskCharacter(void) const
{ return _editor->inputMaskCharacter(); }

void MSEntryField::inputMaskCharacter(char inputMaskCharacter_)
{
  _editor->inputMaskCharacter(inputMaskCharacter_);
  //this could either take a new inputMaskCharacter into account, or reset it back.
  if(autoMaskInput() == MSTrue) generateInputMask();
}



void MSEntryField::autoMaskInput(MSBoolean mask_)
{
  if(_autoMaskInput != mask_)
   {
     _autoMaskInput = mask_;
     if(_autoMaskInput == MSTrue)  generateInputMask();
     else inputMask("");
   }
}

MSBoolean MSEntryField::autoMaskInput(void) const
{ return _autoMaskInput; }
  
  
// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSEntryField::up(void) {}
void MSEntryField::down(void) {}
void MSEntryField::left(void)
{}

MSBoolean MSEntryField::scrollLeft(void)
{
  if (firstCharacter()!=0)
   {
     firstCharacter(firstCharacter()-1);
     return MSTrue;
   }
  else return MSFalse;
}

void MSEntryField::right(void)
{}

MSBoolean MSEntryField::scrollRight(const MSString &string_,int &lastCharacter_)
{
  int oldFirstCharacter=firstCharacter();
  lastCharacter_=-1;
  if (string_.length()>0)
   {
     const char *pString=string_.string();
     pString+=firstCharacter();
     int nCharToDisplay=(int)string_.length()-firstCharacter();
     if (nCharToDisplay>0)
      {
        int dfw=displayableFieldWidth(pString,nCharToDisplay);
        if (dfw>0)
         {
           int nCharDisplayable=nCharToDisplay;
           while (_fieldValue->textWidth(pString,nCharDisplayable)>dfw) nCharDisplayable--;
           if (nCharDisplayable>0)
            {
              if (nCharDisplayable<nCharToDisplay)
               {
                 firstCharacter(firstCharacter()+1);
                 lastCharacter_=firstCharacter()+nCharDisplayable;
               }
            }
         }
      }
   }
  return (MSBoolean(oldFirstCharacter!=firstCharacter()));
}

void MSEntryField::increment(void)
{ activateCallback(MSWidgetCallback::increment); }

void MSEntryField::decrement(void)
{ activateCallback(MSWidgetCallback::decrement); }

void MSEntryField::generateInputMask(void) {}

void MSEntryField::setSelection(int start_,int end_)
{
  if (start_==-1||end_==-1)
   {
     _selectionStart=-1;
     _selectionEnd=-1;
     drawFieldValue();
     disownSelection(XA_PRIMARY);
   }
  else if (ownSelection(XA_PRIMARY)==MSSuccess)
   {
     _selectionStart=start_;
     _selectionEnd=end_;
     drawFieldValue();
   }
}

void MSEntryField::clearSelection(MSBoolean redrawAlways_)
{
  MSBoolean redrawn=MSFalse;
  if (selectionStart()!=-1||selectionEnd()!=-1||firstCharacter()!=0)
   {
     _firstCharacter=0;
     _selectionStart=-1;
     _selectionEnd=-1;
     drawFieldValue();
     redrawn=MSTrue;
   }
  if (redrawAlways_==MSTrue&&redrawn==MSFalse) drawFieldValue();
}

void MSEntryField::drawFieldValue(unsigned long fg_,unsigned long bg_)
{
  // If entry field is insensitive, then let the super class do the drawing,
  // because XDrawImageString doesn't support fill style.
  if (sensitive()==MSFalse) MSCompositeField::drawFieldValue(fg_,bg_);
  else if (frozen()==MSFalse&&owner()->mapped()==MSTrue&&mapped()==MSTrue)
   {
     MSString buffer;
     const char *pString=formatOutput(buffer);
     pString+=firstCharacter();
     int len=(int)buffer.length()-firstCharacter();
     int sht=_fieldValue->shadowThickness();                 
     if (len>0)
      {
	int fulllen=len;
	int clipIndicatorWidth=0;
	int clipIndicatorMargin=2;
        int starLength=0;
	int dw=_fieldValue->width()-2*_fieldValue->offset();
	if (clipMode()!=MSNoClipping)
	 {
	   if (_fieldValue->textWidth(pString,len)>dw)
	    {
              if(clipMode()==MSClipIndicator)
               {
                 clipIndicatorWidth=_fieldValue->charWidth()+clipIndicatorMargin;
                 dw-=clipIndicatorWidth;
                 if (dw<0)
                  {
                    dw=0;
                    clipIndicatorWidth=0;
                  }
               }
              else  //MSClipStars
               {
                 starLength=(int)(dw/fieldValue()->charWidth('*'));
               }
            }
	 }
        int xx=_fieldValue->x()+_fieldValue->offset();
	if (starLength>0)
         {
           char *cp = new char[starLength+1];
           for(int ii=0;ii<starLength;ii++) cp[ii]='*';
           cp[starLength]='\0';
           
           int offset=_fieldValue->highlightThickness()+_fieldValue->shadowThickness();
           int diff = (_fieldValue->height()-(2*offset+_fieldValue->textHeight()));
           int margin=diff/2;
           int yy=_fieldValue->y()+offset+((margin>0)?margin:0)+_fieldValue->textAscent();
           
           XSetForeground(display(),drawGC(),bg_);
           XFillRectangle(display(),window(),
                          drawGC(),
                          _fieldValue->x()+sht,
                          _fieldValue->y()+sht,
                          _fieldValue->width()-(2*sht),
                          _fieldValue->height()-(2*sht));

           XDrawString(display(),window(),fieldValue()->textGC(),
                       fieldValue()->textFontStruct(),xx,yy,cp,starLength);
           
         }
        else if(dw>0)
	 {
	   while (_fieldValue->textWidth(pString,len)>dw) len--;
	   if (len>0)
	    {
	      int offset=_fieldValue->highlightThickness()+_fieldValue->shadowThickness();
              int diff = (_fieldValue->height()-(2*offset+_fieldValue->textHeight()));
	      int margin=diff/2;
	      int yy=_fieldValue->y()+offset+((margin>0)?margin:0)+_fieldValue->textAscent();
              int textX;
              
              if (valueAlignment()==MSCenter&&fulllen==len)
               {
                 int tw=_fieldValue->textWidth(pString,len);
                 xx+=(dw-tw)/2;
               }
	      else if (valueAlignment()==MSRight||(clipIndicatorWidth>0&&fulllen>len))
	       {
		 int tw=_fieldValue->textWidth(pString,len);
		 xx=_fieldValue->x()+_fieldValue->width()-_fieldValue->offset()-tw-clipIndicatorWidth;
	       }
              textX=xx;
              //if textAsent() is not the same textFontStruct()->ascent then we
              //have either a variable-width or some other weird (e.g. courier) font.
              //This also means that XImageDrawString will not paint the bacground
              //as high as necessary, so we do this here:
              if(_fieldValue->textFontStruct()->ascent < _fieldValue->textAscent())
               {
                 XSetForeground(display(),drawGC(),bg_);
                 XFillRectangle(display(),window(),drawGC(),
                                textX,_fieldValue->y()+sht,
                                _fieldValue->textWidth(pString,len),
                                _fieldValue->textAscent()-_fieldValue->textFontStruct()->ascent);
               }
              XSetForeground(display(),drawGC(),bg_);
              XFillRectangle(display(),window(),
                             drawGC(),
                             _fieldValue->x()+sht,
                             _fieldValue->y()+sht,
                             xx-_fieldValue->x()-sht,
                             _fieldValue->height()-(2*sht));
              int fc=firstCharacter();
              int lc=firstCharacter()+len-1;
              int ss=selectionStart();
              int se=selectionEnd();
              //Nothing is selected
              if ((ss<fc&&se<fc)||(ss>lc&&se>lc))
               {
                 XSetForeground(display(),drawGC(),fg_);
                 XSetBackground(display(),drawGC(),bg_);
                 XDrawImageString(display(),window(),
                                  drawGC(),_fieldValue->textFontStruct(),
                                  xx,yy,pString,len);
                 xx+=_fieldValue->textWidth(pString,len);
               }
              //Everything visible is selected
              else if (ss<=fc&&se>=lc)
               {
                 XSetForeground(display(),drawGC(),bg_);
                 XSetBackground(display(),drawGC(),fg_);
                 XDrawImageString(display(),window(),
                                  drawGC(),_fieldValue->textFontStruct(),
                                  xx,yy,pString,len);
                 xx+=_fieldValue->textWidth(pString,len);
               }
              //Selection begins before the first character and ends before the end of the
              //entry field
              else if (ss<=fc)
               {
                 int slen=se-fc+1;
                 slen=(slen>len)?len:slen;
                 XSetForeground(display(),drawGC(),bg_);
                 XSetBackground(display(),drawGC(),fg_);
                 XDrawImageString(display(),window(),
                                  drawGC(),_fieldValue->textFontStruct(),
                                  xx,yy,pString,slen);
                 xx+=_fieldValue->textWidth(pString,slen);
                 pString+=slen;
                 slen=len-slen;
                 XSetForeground(display(),drawGC(),fg_);
                 XSetBackground(display(),drawGC(),bg_);
                 XDrawImageString(display(),window(),
                                  drawGC(),_fieldValue->textFontStruct(),
                                  xx,yy,pString,slen);
                 xx+=_fieldValue->textWidth(pString,slen);
               }
              //Selection begins inside the entry field and ends either before the end of
              //the entry field or within the entry field
              else
               {
                 int slen=ss-fc;
                 slen=(slen>len)?len:slen;
                 XSetForeground(display(),drawGC(),fg_);
                 XSetBackground(display(),drawGC(),bg_);
                 XDrawImageString(display(),window(),
                                  drawGC(),_fieldValue->textFontStruct(),
                                  xx,yy,pString,slen);
                 xx+=_fieldValue->textWidth(pString,slen);
                 int lenLeft=len-slen;
                 if (lenLeft>0)
                  {
                    pString+=slen;
                    slen=se-ss+1;
                    slen=(slen>lenLeft)?lenLeft:slen;
                    XSetForeground(display(),drawGC(),bg_);
                    XSetBackground(display(),drawGC(),fg_);
                    XDrawImageString(display(),window(),
                                     drawGC(),_fieldValue->textFontStruct(),
                                     xx,yy,pString,slen);
                    xx+=_fieldValue->textWidth(pString,slen);
                    lenLeft-=slen;
                    if (lenLeft>0)
                     {
                       pString+=slen;
                       slen=lenLeft;
                       XSetForeground(display(),drawGC(),fg_);
                       XSetBackground(display(),drawGC(),bg_);
                       XDrawImageString(display(),window(),
                                        drawGC(),_fieldValue->textFontStruct(),
                                        xx,yy,pString,slen);
                       xx+=_fieldValue->textWidth(pString,slen);
                     }
                  }
               }
              //See if font is small and we need to redraw
              //sections above and below text.
              if(diff>0)
               {
                 XSetForeground(display(),drawGC(),bg_);
                 XFillRectangle(display(),window(),drawGC(),
                                textX,_fieldValue->y()+sht,
                                xx-textX,margin);
                 //The bottom margin could be 1 pixel bigger then margin,
                 //because of integer division rounding (if diff is odd).
                 //Therefore compute it by subsctracting.
                 margin=diff-margin;
                 XFillRectangle(display(),window(),drawGC(),
                                textX,_fieldValue->y()+_fieldValue->height()-sht-margin,
                                xx-textX,margin);
               }
	    }
	 }
        if(starLength==0)
         {
           XSetForeground(display(),drawGC(),bg_);
           XFillRectangle(display(),window(),
                          drawGC(),
                          xx,
                          _fieldValue->y()+sht,
                          _fieldValue->width()-(2*sht)-(xx-_fieldValue->x()-_fieldValue->offset()),
                          _fieldValue->height()-(2*sht));
         }
        if (clipMode()==MSClipIndicator)
         {
           if (fulllen>len&&clipIndicatorWidth>0)
            {
              int offset=_fieldValue->highlightThickness()+_fieldValue->shadowThickness()+3;
              int xx=_fieldValue->x()+_fieldValue->width()-offset-clipIndicatorWidth+clipIndicatorMargin;
              XPoint points[3];
              points[0].x=xx;
              points[0].y=offset;
              points[1].x=clipIndicatorWidth;
              points[1].y=(height()-2*offset)/2;
              points[2].x=-clipIndicatorWidth;
              points[2].y=points[1].y;
              XSetForeground(display(),drawGC(),clipIndicatorForeground());
              XFillPolygon(display(),window(),drawGC(),points,3,
                           Convex,CoordModePrevious);
            }
         }
      }
     else
      {
        XSetForeground(display(),drawGC(),bg_);
        XFillRectangle(display(),window(),
                       drawGC(),
                       _fieldValue->x()+sht,
                       _fieldValue->y()+sht,
                       _fieldValue->width()-(2*sht),
                       _fieldValue->height()-(2*sht));
      }
     drawFieldValueShadow();
   }
}

int MSEntryField::locateCursorPosition(int x_,const MSString &string_)
{
  const char *pString=string_.string();
  pString+=firstCharacter();
  int len=(int)string_.length()-firstCharacter();
  int fulllen=len;
  int clipIndicatorWidth=0;
  int clipIndicatorMargin=2;
  int dw=_fieldValue->width()-2*_fieldValue->offset();
  if (clipMode()!=MSNoClipping)
   {
     if (_fieldValue->textWidth(pString,len)>dw)
      {
        if(clipMode()==MSClipIndicator)
         {
           clipIndicatorWidth=_fieldValue->charWidth()+clipIndicatorMargin;
           dw-=clipIndicatorWidth;
           if (dw<0)
            {
              dw=0;
              clipIndicatorWidth=0;
            }
         }
        else // MSClipStars , selection is disabled.
         {
           return -1;
         }
      }
   }
  if (dw>0)
   {
     while (_fieldValue->textWidth(pString,len)>dw) len--;
     if (len>0)
      {
        int xx=_fieldValue->x()+_fieldValue->offset();
        if (valueAlignment()==MSCenter&&fulllen==len)
         {
           int tw=_fieldValue->textWidth(pString,len);
           xx+=(dw-tw)/2;
         }
        else if (valueAlignment()==MSRight||(clipIndicatorWidth>0&&fulllen>len))
         {
           int tw=_fieldValue->textWidth(pString,len);
           xx=_fieldValue->x()+_fieldValue->width()-_fieldValue->offset()-tw-clipIndicatorWidth;
         }
        if (x_>xx)
         {
           int diff=x_-xx;
           while (_fieldValue->textWidth(pString,len)>diff) len--;
           return firstCharacter()+len;
         }
        else return firstCharacter();
      }
     else return -1;
   }
  else return -1;
}

void MSEntryField::button1Release(const XEvent *pEvent_)
{
  if (inputFocus()==this&&hadFocus()==MSTrue)
   {
     if (hasCallback(MSWidgetCallback::reference)==MSTrue) reference();
     else if (isProtected()==MSFalse) startEditing(pEvent_);
   }
}

const char *MSEntryField::getPrimarySelection(MSString& buffer_, int& len_)
{
  const char *cp=0;
  if (selectionStart()!=-1&&selectionEnd()!=-1)
    {
      cp=formatOutput(buffer_);
      cp=cp+selectionStart();
      len_=selectionEnd()-selectionStart()+1;
    }
  
  return cp;
}

void MSEntryField::selectionClear(const XEvent *)
{
  clearSelection();
}

void MSEntryField::insertPrimarySelection(void)
{
  insertString(insertCursor(),server()->pasteBuffer());
}

MSBoolean MSEntryField::insertString(int pos_,const MSString &string_)
{
  pasting(MSTrue);
  MSString buffer;
  formatOutput(buffer);
  if (pos_==-1) buffer<<string_;
  else
   {
     buffer.insert(string_,pos_);
     if (pos_<=selectionStart())
      {
        selectionStart(selectionStart()+string_.length());
        selectionEnd(selectionEnd()+string_.length());
      }
   }
  MSBoolean validated=validateInput(buffer);
  if (validated==MSTrue)
   {
     valueChange();
   }
  else
   {
     if (pos_<=selectionStart())
      {
        selectionStart(selectionStart()-string_.length());
        selectionEnd(selectionEnd()-string_.length());
      }
     server()->bell();
   }
  pasting(MSFalse);
  return validated;
}

MSBoolean MSEntryField::isSelected(void)
{
  return (selectionStart()>=0&&selectionEnd()>=0&&selectionEnd()>=selectionStart())?MSTrue:MSFalse;
}

void MSEntryField::updateEditor(void)
{
  MSString buffer;
  formatOutput(buffer);
  buffer.remove(selectionStart(),selectionEnd()-selectionStart()+1);
  _editor->string(buffer.string());
  _editor->cursorPosition(selectionStart());
  if (selectionStart()<firstCharacter())
   {
     int index=selectionStart()-1;
     index=(index>0)?index:0;
     _editor->scrollIndex(index);
   }
  else
   {
     _editor->scrollIndex(firstCharacter());
   }
  _selectionStart=-1;
  _selectionEnd=-1;
  _firstCharacter=0;
  disownSelection(XA_PRIMARY);
}

void MSEntryField::trackSelection(const XEvent *pEvent_)
{
  MSString buffer;
  formatOutput(buffer);
  if (buffer.length()>0)
   {
     int startX=pEvent_->xbutton.x;
     int startPos=locateCursorPosition(startX,buffer);
     if (startPos!=-1)
      {
        int pos=startPos;
        Window root,child;
        int rx,ry,winx,winy;
        unsigned keys;
        int sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&winx,&winy,&keys);
        while (keys&Button1Mask)
         {
           if (sameScreen==True)
            {
              int cursor=-1;
              if (winx<_fieldValue->x())
               {
                 scrollLeft();
                 cursor=firstCharacter();
               }
              else if (winx>_fieldValue->x()+_fieldValue->width())
               {
                 if (scrollRight(buffer,cursor)==MSFalse) cursor=buffer.length();
               }
              else
               {
                 cursor=locateCursorPosition(winx,buffer);
               }
              if (cursor!=-1)
               {
                 if (cursor!=pos)
                  {
                    pos=cursor;
                    //Set hadFocus to MSFalse here so button1Release will not bring up the editor
                    _hadFocus=MSFalse;
                    if (startPos==pos)
                     {
                       setSelection(-1,-1);
                     }
                    if (startPos>pos)
                     {
                       setSelection(pos,startPos-1);
                     }
                    else
                     {
                       setSelection(startPos,pos-1);
                     }
                  }
               }
            }
           sameScreen=XQueryPointer(display(),window(),&root,&child,&rx,&ry,&winx,&winy,&keys);
         }
      }
   }
}

int MSEntryField::displayableFieldWidth(const char *pString_,int len_)
{
  int dfw=_fieldValue->width()-2*_fieldValue->offset();
  if (clipMode()==MSClipIndicator)
   {
     if (_fieldValue->textWidth(pString_,len_)>dfw)
      {
        int clipIndicatorMargin=2;
        int clipIndicatorWidth=_fieldValue->charWidth()+clipIndicatorMargin;
        dfw-=clipIndicatorWidth;
      }
   }
  return (dfw>0)?dfw:0;
}

void MSEntryField::currentColors(unsigned long &fg_,unsigned long &bg_)
{
  if (cycle()!=0&&cycle()->count()<cycle()->numCycles())
   {
     if (cycleColorMode()==MSReverseVideo)
      {
        fg_=valueBackground();
        bg_=valueForeground();
      }
     else if (cycleColorMode()==MSBackground)
      {
        fg_=valueForeground();
        bg_=cycle()->color(cycle()->count());
      }
     else
      {
        fg_=cycle()->color(cycle()->count());
        bg_=valueBackground();
      }
   }
  else
   {
     fg_=valueForeground();
     bg_=valueBackground();
   }
}

void MSEntryField::cycleColorMode(MSCycleColorMode cycleColorMode_)
{
  if (_cycleColorMode!=cycleColorMode_)
   {
     MSBoolean redrawNecessary=MSFalse;
     if (cycle()!=0&&cycle()->count()<cycle()->numCycles())
      {
        redrawNecessary=MSTrue;
      }
     removeCycle();
     _cycleColorMode=cycleColorMode_;
     if (redrawNecessary==MSTrue) drawFieldValue();
   }
}

unsigned long MSEntryField::addEditorKeyCallback( const char* pString_,MSKeyCallback* keyCallback_)
{ return _editor->addKeyCallback(pString_,keyCallback_);}

void MSEntryField::removeEditorKeyCallback(unsigned long id_)
{ _editor->removeKeyCallback(id_); }

void MSEntryField::removeEditorKeyCallback(const char* pString_)
{ _editor->removeKeyCallback(pString_); }










