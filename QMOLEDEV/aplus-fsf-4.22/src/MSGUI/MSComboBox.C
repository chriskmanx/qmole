///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSComboBox.H>

static const int MSComboBoxDefaultScollbarWidth=12;

const unsigned int grabMask=(unsigned int)(ButtonPressMask|ButtonReleaseMask|Button1MotionMask);

MSComboList::MSComboList(MSComboListShell *owner_) : MSStringList(owner_)
{
  _scrollBarState=VsbEnabled;
  _acceptFocus=MSFalse;
  _highlighted=MSTrue;
  _displayAllColumns=MSTrue;
  _selectionMode=MSSingle;
  vsbSize(MSComboBoxDefaultScollbarWidth);
}

void MSComboList::defaultDoubleClickBehavior(const XEvent *)
{ doubleClick(); }

void MSComboList::buttonPress(const XEvent *pEvent_)
{
  if (pEvent_->xbutton.subwindow==0)
   {
     unsigned keys;
     int ix=0,iy=0,rx=0,ry=0;
     Window root,child;
     XQueryPointer(display(),comboBox()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     if (child==comboBox()->fieldEditor()->window()) activate();
     else escape();
   } 
  else 
   {
     defaultButtonBehavior(pEvent_);
     if (pEvent_->xbutton.subwindow==panner()->window()&&
	 comboBox()->selectionMode()==MSSingle) doubleClick();
   }
}

void MSComboList::keyPressEvent(const XEvent *pEvent_)
{
  char    buf[16];
  KeySym  keysym;
  int     len=XLookupString((XKeyEvent *)pEvent_,buf,8,&keysym,NULL);
  buf[len]='\0';
  unsigned searchStart;
  switch (keysym)
   {
     case XK_Escape: escape();   break;
     case XK_KP_Enter:
     case XK_Return: activate(); break;     
     case XK_Up:     up();       selectEditableText(); break;
     case XK_Down:   down();     selectEditableText(); break;
     case XK_F29:    pageUp();   selectEditableText(); break;
     case XK_Prior:  pageUp();   selectEditableText(); break;
     case XK_F35:    pageDown(); selectEditableText(); break;
     case XK_Next:   pageDown(); selectEditableText(); break;
     case XK_F27:    home();     selectEditableText(); break;
     case XK_Home:   home();     selectEditableText(); break;
     case XK_R13:    end();      selectEditableText(); break;
     case XK_End:    end();      selectEditableText(); break;
     default:
       keyPressNotify(comboBox(),pEvent_,keysym,pEvent_->xkey.state,buf);
       if (MSView::model()!=0&&selectionMode()!=MSMultiple)
       {
	  if (selectedText()==MSTrue) searchStart=0;
	  else searchStart=selectedRow();
	  if (keysym==XK_BackSpace) selectString(searchStart>0?searchStart-1:0,comboBox()->editString());
	  else selectString(searchStart,comboBox()->editString());
	  _selectedText=MSFalse;
       }
       break;
   }
}

void MSComboList::keyPress(const XEvent *pEvent_,KeySym keysym_,unsigned int state_,const char *pString_)
{ MSStringList::keyPress(pEvent_,keysym_,state_,pString_); }

unsigned MSComboList::selectString(int startRow_,const char *pString_)
{
  if (MSView::model()!=0)
   {
     if (pString_!=0&&strlen(pString_)>0)
      {
	if (startRow_<0||startRow_>=list().length()) startRow_=0;
	if (startRow_>=0)
	 {
	   const MSStringVector &aStringVector=list();
	   unsigned i,n=aStringVector.length(),startRow=startRow_;
	   // search forward starting from startRow
	   for (i=startRow;i<n;i++)
	    {
	      if (aStringVector(i).indexOf(pString_)==0)
	       {
		 selectedRow(i);
		 return i;
	       }
	    }
	   // no match found
	   // search backward starting from startRow-1
	   for (i=startRow-1;i<n;i--)
	    {
	      if (aStringVector(i).indexOf(pString_)==0)
	       {
		 selectedRow(i);
		 return i;
	       }
	    }
	   return selectedRow();
	 }
      }
     return list().length();
   }
  else return 0;
}

void MSComboList::selectEditableText(void)
{
  if (selectionMode()!=MSMultiple)
   {
     comboBox()->selectEditableText();
     _selectedText=MSTrue;
   }
}

void MSComboList::setSelectedRowFrom(const char *pString_)
{
  if (MSView::model()!=0&&pString_!=0)
   {
     unsigned index=list().indexOf(pString_);
     if (index<list().length()) selectedRow(index);
     comboBox()->replaceEditableText(selection());
     selectEditableText();
   }
}

void MSComboList::resetVisibleColumns(void)
{ 
  if (displayAllColumns()==MSTrue)
   {
     if (MSView::model()!=0) columns(list().maxLength()); 
   }
}

void MSComboList::show(void)
{
  resetVisibleColumns();
  switch (comboBox()->selectionMode())
   {
   case MSMultiple: selectionMode(MSMultiple); break;
   case MSBrowse:
   case MSSingle:   
   default:         selectionMode(MSSingle);   break;
   }
  MSStringList::show();
}

void MSComboList::reset(void)
{
  selectionVector(MSIndexVector::nullVector());
  home();
}

void MSComboList::escape(void)
{
  shell()->escape();
  reset();
}

void MSComboList::activate(void)
{
  if (comboBox()->selectionMode()==MSSingle)
   {
     if(selectedRow()!=-1) comboBox()->replaceEditableText(selection());
   }
  else
   {
     MSIndexVector aSelectionVector(selectionVector());
     aSelectionVector.sortUp();
     MSString buffer;
     for (unsigned i=0;i<aSelectionVector.length();i++) 
      {
        comboBox()->appendEditableText(formatOutput(buffer.removeAll(),aSelectionVector(i)));
      }
   }
  shell()->activate();
  reset();
}

void MSComboList::doubleClick(void)
{
  if (comboBox()->selectionMode()==MSSingle)
   {
     comboBox()->replaceEditableText(selection()); 
     activate();
   }
  else if (comboBox()->selectionMode()==MSBrowse)
   {
     comboBox()->appendEditableText(selection());
   }
}

MSComboListShell::MSComboListShell(MSComboBox *box_) : MSWidget(box_->server())
{
  _comboBox=box_;
  _bg=comboBox()->background();
  _fg=comboBox()->foreground();
  _fontID=comboBox()->font();
  _comboList=0;
  _cursor=new MSDisplayCursor(server(),XC_left_ptr,server()->pixel("black"),server()->pixel("white"));
  XSetWindowAttributes attributes;
  attributes.background_pixel=comboBox()->background();
  attributes.border_pixel=comboBox()->foreground();
  attributes.override_redirect=(int)MSTrue;
  attributes.cursor=cursor()->cursor();
  _window=(Window)XCreateWindow(display(),
                    server()->root(),
                    MSRect::x(),MSRect::y(),MSRect::width(),MSRect::height(),
                    1,(int)CopyFromParent,InputOutput,CopyFromParent,
                    (unsigned long)(CWBackPixel|CWBorderPixel|CWOverrideRedirect|CWCursor),
                    (XSetWindowAttributes *)&attributes);  
  _eventMask=0;
  server()->widgetHashTable()->add(window(),this);
  _comboList=new MSComboList(this);
}    

MSComboListShell::~MSComboListShell(void)
{
  safeDestroy(comboList()); 
  delete _cursor;
}

void MSComboListShell::childConfigure(MSWidget *pWidget_)
{ resize(pWidget_->width(),pWidget_->height()); }

void MSComboListShell::configure(void)
{ if (comboList()!=0) comboList()->resize(width(),height()); }

void MSComboListShell::grab(void)
{
  server()->grabKeyboard(comboList()->window(),False,GrabModeAsync,GrabModeAsync,CurrentTime,MSTrue);
  server()->grabPointer(comboList()->window(),False,
			grabMask,GrabModeAsync,GrabModeAsync,None,None,CurrentTime,MSTrue);
  XFlush(display());
}

void MSComboListShell::ungrab(void)
{
  server()->ungrabPointer(comboList()->window(),CurrentTime);
  server()->ungrabKeyboard(comboList()->window(),CurrentTime);
  XFlush(display());
}

void MSComboListShell::escape(void)
{
  hide();
  comboBox()->listDone();
}

void MSComboListShell::activate(void)
{
  hide();
  comboBox()->listActivate();
}

void MSComboListShell::showAt(int x_,int y_)
{  
  moveTo(x_,y_);
  show(); 
}

void MSComboListShell::show(void)
{
  if (mapped()==MSFalse)
   {
     if (comboBox()->selectionMode()==MSSingle)
      {
	MSString buffer;
	comboList()->setSelectedRowFrom(comboBox()->formatOutput(buffer));
      }
     comboList()->show();
     map();
     XFlush(display());
     raise();
     grab();
   }
}

void MSComboListShell::hide(void)
{
  if (mapped()==MSTrue)
   {
     ungrab();
     unmap();
     comboList()->hide();
   }
}

MSComboBox::MSComboBox(MSWidget *owner_,const char *label_,const MSSymbol& tag_) :
MSComboField(owner_,label_,tag_) 
{ internalCouple(new MSString); init(); }

MSComboBox::MSComboBox(MSWidget *owner_,MSString& model_,
		       const char *label_,const MSSymbol& tag_) :
MSComboField(owner_,label_,tag_) 
{ model(model_); init(); }

MSComboBox::~MSComboBox(void) 
{ safeDestroy(listShell()); }

MSBoolean MSComboBox::validate(const char *pString_)
{
  if (MSView::model()!=0)
   {
     return (value().set(pString_)==MSError::MSSuccess)?MSTrue:MSFalse;
   }
  return MSFalse;
}

void MSComboBox::updateData(void)
{
  if (MSView::model()==0) internalCouple(new MSString);
  MSComboField::updateData();
}

const char *MSComboBox::formatOutput(MSString &buffer_)
{
  if (MSView::model()!=0) buffer_=value();
  return buffer_.string();
}

void MSComboBox::listModel(MSStringVector& listModel_)
{ 
  listShell()->comboList()->model(listModel_); 
  listShell()->comboList()->columns(listModel().maxLength());
}

MSStringVector& MSComboBox::listModel(void)
{ return listShell()->comboList()->list(); }

void MSComboBox::init(void)
{
  _listShell=0;
  _selectionMode=MSSingle;
  _separator=",";
  createListShell();
}

void MSComboBox::createListShell(void)
{ if (listShell()==0) _listShell=new MSComboListShell(this); }

void MSComboBox::selectEditableText(void)
{ fieldEditor()->selectAll(); }

void MSComboBox::replaceEditableText(const char *pString_)
{ fieldEditor()->string(pString_); }

void MSComboBox::appendEditableText(const char *pString_)
{
  if (fieldEditor()->length()>0)
   { 
     MSString aString(fieldEditor()->string(),fieldEditor()->length(),
		      separator().string(),separator().length(),
		      pString_,(pString_!=0)?strlen(pString_):0);
     fieldEditor()->string(aString); 
   }
  else fieldEditor()->string(pString_);
}

void MSComboBox::showList(void)
{
  drawComboButton(MSTrue);
  clearEditor();
  mapEditor();
  int x,y;
  rootXY(x,y);
  y+=height();
  // force size computation.
  listShell()->showAt(server()->width(),server()->height());
  x+=buttonRect().x()-listShell()->width();
  listShell()->showAt(x,y);
}

void MSComboBox::listDone(void)
{
  drawComboButton(MSFalse);
  escape();
}

void MSComboBox::listActivate(void)
{
  drawComboButton(MSFalse);
  activate();
}

void MSComboBox::buttonActivate(void)
{ showList(); }

void MSComboBox::updateFont(Font oldfid_)
{ 
  MSComboField::updateFont(oldfid_);
  if (oldfid_==listBox()->font()) listBox()->font(font()); 
}

void MSComboBox::updateBackground(unsigned long oldbg_)
{ 
  MSComboField::updateBackground(oldbg_); 
  if (oldbg_==listBox()->background()) listBox()->background(background()); 
}

void MSComboBox::updateForeground(unsigned long oldfg_)
{
  MSComboField::updateForeground(oldfg_);
  if (oldfg_==listBox()->foreground()) listBox()->foreground(foreground()); 
}


void MSComboBox::set(MSAttrValueList& avList_)
{
  MSComboField::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="selectionMode")
      {
	const MSString& aString=avList_[i].value();
	selectionMode((aString=="MSBrowse"?MSBrowse:(aString=="MSSingle"?MSSingle:MSMultiple)));
	index<<i;
      }
     else if (avList_[i].attribute()=="separator")
      {
        separator(avList_[i].value());
	index<<i;
      }
   }
  avList_.remove(index);
}

MSAttrValueList& MSComboBox::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("selectionMode",
		       (selectionMode()==MSBrowse?"MSBrowse":
			(selectionMode()==MSSingle?"MSSingle":"MSMultiple")),
		       MSStringVector("MSSingle\nMSBrowse\nMSMultiple"));
  avList_<<MSAttrValue("separator",separator(),MSAttrValue::String);
  return MSComboField::get(avList_);
}

