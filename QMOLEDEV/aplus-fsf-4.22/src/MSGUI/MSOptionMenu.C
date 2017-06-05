///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSOptionMenu.H>

static const int MSOptionMenuSymbolHeight=8;
static const int MSOptionMenuSymbolWidth=12;
static const int MSOptionMenuSymbolThickness=2;
static const int MSOptionMenuDefaultMargin=5;
static const int MSOptionMenuDefaultMarginHeight=2;
static const unsigned long MSOptionMenuEventMask=(ExposureMask|ButtonPressMask|
						  ButtonReleaseMask|Button1MotionMask);

MSOptionPopupMenu::MSOptionPopupMenu(MSOptionMenu *owner_) :
MSPopupMenu(owner_->server()) 
{ 
  _optionMenu=owner_;
  init();
  internalCouple(new MSStringVector);
}

MSOptionPopupMenu::MSOptionPopupMenu(MSOptionMenu *owner_,
				     MSStringVector& options_) :
MSPopupMenu(owner_->server()) 
{ 
  _optionMenu=owner_;
  init();
  model(options_);
}

void MSOptionPopupMenu::init(void)
{
  _lastShowTime=0;
  _threshold=500;
  foreground(optionMenu()->foreground());
  background(optionMenu()->background());
  font(optionMenu()->font());
}

void MSOptionPopupMenu::model(MSStringVector& options_) 
{ couple(&options_); }

void MSOptionPopupMenu::options(const MSStringVector& options_) 
{ internalCouple(new MSStringVector(options_)); }

void MSOptionPopupMenu::rebuildMenu(void)
{
  removeAllItems();
  if (MSView::model()!=0)
   {
     unsigned n=optionsModel().length();
     if (n>0)
      {
	for (unsigned i=0;i<n;i++)
	 {
	   MSMenuItem *pMenuItem=new MSMenuItem(this,optionsModel()(i),0,i);
	   setItem(pMenuItem,i);
	 }
      }
   }
  computeSize();
  optionMenu()->setSelectedItem(0);
  optionMenu()->computeSize();
}

// your model has changed
void MSOptionPopupMenu::updateData(void)
{ rebuildMenu(); }

void MSOptionPopupMenu::update(const MSIndexVector& index_)
{
  if (MSView::model()!=0)
   {
     if (index_.length()==0)
      {
	if (optionsModel().length()==itemCount())
	 {
	   MSMenuItem *mi;
           int i,n=itemCount();
           for(i=0;i<n;i++)
	    {
	      mi=(MSMenuItem *)itemVector()(i);
	      mi->label(optionsModel()(i));
	    }
	   computeSize();
	   optionMenu()->setSelectedItem(0);
	   optionMenu()->computeSize();
	 }
	else rebuildMenu();
      }
     else 
      {
	MSIndexVector iv(index_);
	iv.sortUp();
	if (iv(0)==itemCount())
	 {
	   for (unsigned i=0,j=itemCount();i<iv.length();i++,j++)
	    {
	      MSMenuItem *pMenuItem=new MSMenuItem(this,optionsModel()(j),0,j);
	      setItem(pMenuItem,j);
	    }
	 }
	else
	 {
	   for (unsigned i=0;i<iv.length();i++)
	    {
	      unsigned index=iv(i);
	      MSMenuItem *mi=menuItem(index);
	      if (mi!=0) mi->label(optionsModel()(index));
	    }
	 }
      }
     computeSize();
     optionMenu()->computeSize();
   }
}

void MSOptionPopupMenu::activate(void)
{
  optionMenu()->activate(activeMenuItem()->tag());
  done();
}

void MSOptionPopupMenu::popup(MSBoolean warp_,unsigned long eventTime_)
{
   if (warp_==MSTrue)
   {
      selectedItem(optionMenu()->selectedItem());
      MSMenuItem *item=menuItem(selectedItem());
      if (item!=0)
	 XWarpPointer(display(),None,window(),0,0,0,0,
		      item->x()+item->width()/2,item->y()+item->height()/2);
   }
   show();
   lastShowTime(eventTime_);
}

int MSOptionPopupMenu::menuItemYOffset(int item_)
{
  MSMenuItem *mi=menuItem(item_);
  return (mi!=0)?mi->y():0;
}

int MSOptionPopupMenu::menuItemXOffset(int item_)
{
  MSMenuItem *mi=menuItem(item_);
  return (mi!=0)?mi->x():0;
}

void MSOptionPopupMenu::motionNotify(const XEvent *event_)
{
  int lastSelectedItem=selectedItem();
  MSMenu::motionNotify(event_);
  if (selectedItem()!=lastSelectedItem) lastShowTime(0);
}

void MSOptionPopupMenu::buttonRelease(const XEvent *event_)
{
  if (event_->xbutton.time-lastShowTime()>threshold())
   {
     MSMenu::buttonRelease(event_);
   }
}

void MSOptionPopupMenu::buttonPress(const XEvent *event_)
{
  if (grabber()==this)
   {
     lastShowTime(0);
   }
  else
   {
     lastShowTime(event_->xbutton.time);
   }
  MSMenu::buttonPress(event_);
}

MSOptionMenu::MSOptionMenu(MSWidget *owner_,const char *label_) : 
MSCompositeField(owner_,label_) 
{ 
  init(); 
}

MSOptionMenu::MSOptionMenu(MSWidget *owner_,const char *label_,MSStringVector& options_) : 
MSCompositeField(owner_,label_) 
{ 
  init(); 
  optionsModel(options_);
}

MSOptionMenu::MSOptionMenu(MSWidget *owner_,MSStringVector& options_,const char *label_) : 
MSCompositeField(owner_,label_) 
{ 
  init(); 
  optionsModel(options_);
}

MSOptionMenu::MSOptionMenu(MSWidget *owner_,MSString& model_,
			   MSStringVector& options_,const char *label_) : 
MSCompositeField(owner_,label_) 
{ 
  init(); 
  optionsModel(options_);
  model(model_);
}


MSOptionMenu::~MSOptionMenu(void)
{ if (optionMenu()!=0) safeDestroy(optionMenu()); }

void MSOptionMenu::init(void)
{
  freeze();
  _marginHeight=MSOptionMenuDefaultMarginHeight;
  _selectedItem=0;
  _optionMenu=0;
  _columns=1;
  _internalEvent=MSFalse;
  selectInput(MSOptionMenuEventMask);
}

void MSOptionMenu::model(MSString &model_)
{ couple(&model_); }

void MSOptionMenu::options(const MSStringVector& options_) 
{
  if (optionMenu()==0) _optionMenu=new MSOptionPopupMenu(this);
  optionMenu()->columns(columns());
  optionMenu()->options(options_);  
}

void MSOptionMenu::optionsModel(MSStringVector& options_) 
{
  if (optionMenu()!=0) optionMenu()->model(options_);
  else
   {
     _optionMenu=new MSOptionPopupMenu(this,options_);
     optionMenu()->columns(columns());
   }
}


void MSOptionMenu::setSelectedItem(int item_)
{
  _selectedItem=item_;
  if(hasModel()==MSTrue)
   {
     if(optionMenu()!=0)
       {
	 _internalEvent=MSTrue;
	 if(_selectedItem<optionsModel().length())
	   {
	     viewModel()=optionsModel()(_selectedItem);
	   }
	 else viewModel().removeAll();
	 _internalEvent=MSFalse;
       }
   }
}

  
void MSOptionMenu::selectedItem(int item_)
{
  if (item_!=selectedItem())
   {
     setSelectedItem(item_);
     drawFieldValue();
   }
}

void MSOptionMenu::updateData(void)
{
  if(hasModel() == MSTrue && _internalEvent == MSFalse)
    {
      _internalEvent=MSTrue;
      unsigned index;

      if(optionMenu()==0)
	{
	  if(viewModel().length()!=0)
	    {
	      MSStringVector sv(viewModel());
	      options(sv);
	      _selectedItem=0;
	    }
	}
      else
	{
	  if(viewModel().length()!=0)
	    {
	      index = optionsModel().indexOf(viewModel());
	      
	      if(index == optionsModel().length())
		{
		  //append the new value to the list.
		  optionsModel().appendSingle(viewModel());
		}
	      _selectedItem=index;
	      drawFieldValue();
	    }
	  else
	    {
	      if(_selectedItem<optionsModel().length())
		{
		  viewModel()=optionsModel()(_selectedItem);
		}
	      else viewModel().removeAll();
	    }
	}
      _internalEvent=MSFalse;
    }
}

void MSOptionMenu::update(const MSIndexVector& )
{ updateData(); }

void MSOptionMenu::escape(void)
{}
void MSOptionMenu::activate(void)
{ activateCallback(MSWidgetCallback::activate); }

void MSOptionMenu::activate(int item_)
{
  selectedItem(item_);
  activate();
}

void MSOptionMenu::showMenu(unsigned long eventTime_)
{
  if (optionMenu()!=0)
   {
     // If option menu has never been mapped, call calculateNaturalSize & placeMenuItems
     // to compute its size and positions of individual items.
     int w,h;
     if (optionMenu()->firstMap()==MSFalse)
       {
	 optionMenu()->calculateNaturalSize(w,h);
	 optionMenu()->placeMenuItems();
       }
     else
      {
        w=optionMenu()->width();
        h=optionMenu()->height();
      }
     int rootx,rooty,xx,yy;
     rootXY(rootx,rooty);
     yy=rooty+fieldValue()->y()-optionMenu()->menuItemYOffset(selectedItem());
     if (yy<0) yy=0;
     else if (yy+h>server()->height())
      {
	yy=server()->height()-h;
      }
     xx=rootx+fieldValue()->x()-optionMenu()->menuItemXOffset(selectedItem());
     if (xx<0) xx=0;
     else if (xx+w>server()->width())
      {
        xx=server()->width()-w;
      }
     optionMenu()->moveTo(xx,yy);
     optionMenu()->popup(MSTrue,eventTime_);  // Always warp the pointer
   }
}

void MSOptionMenu::motionNotify(const XEvent *)
{ if (optionMenu()!=0&&optionMenu()->mapped()==MSTrue) optionMenu()->grab(); }
void MSOptionMenu::buttonRelease(const XEvent *)
{ if (optionMenu()!=0&&optionMenu()->mapped()==MSTrue) optionMenu()->grab(); }  

void MSOptionMenu::buttonPress(const XEvent *pEvent_)
{
  if (isProtected()==MSFalse)
   {
     if (pEvent_->xbutton.x>=fieldValue()->x()&&
	 pEvent_->xbutton.x<=fieldValue()->x()+fieldValue()->width()&&
         pEvent_->xbutton.y>=fieldValue()->y()&&
	 pEvent_->xbutton.y<=fieldValue()->y()+fieldValue()->height())     
      {
        if (acceptFocus()==MSFalse||traverseFocus(this)==MSTrue)
         {
           showMenu(pEvent_->xbutton.time);
         }
      }
   }
}

void MSOptionMenu::keyPress(const XEvent *pEvent_,KeySym keysym_,unsigned int state_,const char *)
{
  MSKeyPress keyPress(keysym_, state_);
  
  if (isProtected()==MSFalse&&optionMenu()!=0&& keyTranslate(keyPress)==MSFalse)
   {
     switch (keysym_)
      {
      case XK_Return:
      case XK_Up:      	
      case XK_Down:
	showMenu(pEvent_->xkey.time);
	break;
      default:
	break;
      }
   }
}

unsigned MSOptionMenu::computeValuePixelWidth(void)
{
  unsigned vw = valueWidth()*_fieldValue->charWidth()+2*_fieldValue->offset();
  unsigned ovw= maxValueWidth()+fieldValue()->offset()+2*MSOptionMenuDefaultMargin+MSOptionMenuSymbolWidth;
  return (vw>ovw?vw:ovw);
}

void MSOptionMenu::placement(void) 
{
  if (fieldValue()!=0&&fieldLabel()!=0)
   {
     int offset=highlightThickness()+shadowThickness();
     int offset2=offset<<1;
     int vh=fieldValue()->textHeight()+
            2*(marginHeight()+fieldValue()->shadowThickness()+fieldValue()->highlightThickness());
     int lh=fieldLabel()->textHeight()+
            2*(fieldLabel()->shadowThickness()+fieldLabel()->highlightThickness());
     int lw=fieldLabel()->width();
     int vw=fieldValue()->width();
     int trueWidth=width()-offset2;

     if (labelAlignment()==MSTop)
      {
	fieldLabel()->moveTo(offset,offset);
	fieldValue()->moveTo(offset,offset+lh+labelSpacing());
	fieldLabel()->width(lw);
	fieldValue()->width(vw);
      }
     else
      {
	vw=trueWidth-fieldLabel()->width();
	int h=(vh>lh)?vh:lh;
	if (vh==lh) height(h+offset2);
	else if (h+offset2>height()) height(h+offset2);   
	fieldLabel()->height(h);
	fieldValue()->resize(vw,h);
	fieldLabel()->moveTo(offset,offset);
	fieldValue()->moveTo(offset+fieldLabel()->width(),offset);
	
      }
     redraw();
   }
}

void MSOptionMenu::drawFieldValue(unsigned long,unsigned long)
{}

void MSOptionMenu::drawFieldValue(void)
{
  if (frozen()==MSFalse&&owner()->mapped()==MSTrue&&mapped()==MSTrue)
   {
     int sht=fieldValue()->shadowThickness();
     XFillRectangle(display(),window(),backgroundShadowGC(),
		    fieldValue()->x()+sht,fieldValue()->y()+sht,
		    fieldValue()->width()-(sht<<1),fieldValue()->height()-(sht<<1));

     MSString buffer;
     const char *pString=formatOutput(buffer);
     if (pString!=0&&buffer.length()>0)
      {   
	int len=buffer.length();
	if (len>0)
	 {
	   int xx=fieldValue()->x()+fieldValue()->offset();
	   int offset=fieldValue()->highlightThickness()+fieldValue()->shadowThickness();
	   int margin=(fieldValue()->height()-(2*offset+fieldValue()->textHeight()))>>1;
	   int yy=offset+((margin>0)?margin:0)+fieldValue()->textAscent();
	   
	   int vw=fieldValue()->width()-2*fieldValue()->offset()-
	          2*MSOptionMenuDefaultMargin-MSOptionMenuSymbolWidth;
           int tw=fieldValue()->textWidth(pString,len);
           int delta=(vw>tw)?((vw-tw)>>1):0;
	   
	   fieldValue()->foreground(itemForeground(selectedItem()));
	   XDrawString(display(),window(),fieldValue()->textGC(),fieldValue()->textFontStruct(),
		       xx+delta,fieldValue()->y()+yy,pString,len);
	 }
      }
     int offset=highlightThickness()+shadowThickness();
     MSRect aRect(fieldValue()->x()+fieldValue()->width()-MSOptionMenuDefaultMargin-MSOptionMenuSymbolWidth,
		  fieldValue()->y()+(fieldValue()->height()-MSOptionMenuSymbolHeight)/2,
		  MSOptionMenuSymbolWidth,MSOptionMenuSymbolHeight);
     drawBevel(aRect,MSRaised,MSOptionMenuSymbolThickness);

     aRect.configuration(fieldValue()->x(),fieldValue()->y(),
			 fieldValue()->width(),fieldValue()->height());
     drawBevel(aRect,MSRaised,fieldValue()->shadowThickness());
   }
}

void MSOptionMenu::updateFont(Font oldfid_)
{ 
  MSCompositeField::updateFont(oldfid_);
  if (optionMenu()!=0) optionMenu()->font(font());
}

void MSOptionMenu::updateBackground(unsigned long oldbg_)
{ 
  MSCompositeField::updateBackground(oldbg_); 
  if (optionMenu()!=0) optionMenu()->background(background()); 
}

void MSOptionMenu::updateForeground(unsigned long oldfg_)
{
  MSCompositeField::updateForeground(oldfg_);
  if (optionMenu()!=0) optionMenu()->foreground(foreground()); 
}

void MSOptionMenu::updateSensitivity(void)
{
  MSCompositeField::updateSensitivity();
  if (optionMenu()!=0) optionMenu()->sensitive(sensitive());
}

MSBoolean MSOptionMenu::loseFocus(void) 
{
  if (optionMenu()!=0&&optionMenu()->mapped()==MSTrue) escape(); 
  unHighlight(); 
  return MSTrue; 
}

// The following virtual methods can be overriden by subclassers to
// obtain model specific behavior:
// numberOfItems - number of options in the model
// itemLabel     - return the label for the nth item
// formatOuput   - format the selectedItem for the entry part of the display
// maxValueWidth - return the max pixel width over all item labels in the option Menu

unsigned MSOptionMenu::numberOfItems(void) const
{ 
  if (optionMenu()!=0&&optionMenu()->hasModel()==MSTrue)
   {
     return optionsModel().length();
   }
  return 0;
}

unsigned MSOptionMenu::maxValueWidth(void)
{
  if (optionMenu()!=0&&optionMenu()->MSView::model()!=0)
   {
     int max=0,w=0;
     unsigned numItems=numberOfItems();
     for (unsigned i=0;i<numItems;i++)
      {
	const char *pString=itemLabel(i);
	if (pString!=0)
	 {
	   w=fieldValue()->textWidth(pString);
	   max=(w>max?w:max);
	 }
      }
     return max+optionMenu()->shadowThickness()*2;
   }
  else return 0;
}

const char *MSOptionMenu::formatOutput(MSString &buffer_)
{
  if (optionMenu()!=0&&optionMenu()->MSView::model()!=0&&selectedItem()<numberOfItems())
   {
     buffer_=optionsModel()(selectedItem());
   }
  return buffer_.string();
}

const char *MSOptionMenu::itemLabel(unsigned itemIndex_)
{
  if (optionMenu()!=0&&optionMenu()->MSView::model()!=0&&itemIndex_<numberOfItems())
   {
     return (const char *)optionsModel()(itemIndex_);
   }
  return 0;
}

void MSOptionMenu::columns(unsigned columns_)
{
  if (columns_>0&&_columns!=columns_)
   {
     _columns=columns_;
     if (optionMenu()!=0) optionMenu()->columns(columns());
   }
}

unsigned long MSOptionMenu::itemForeground(unsigned)
{ return foreground(); }

MSAttrValueList& MSOptionMenu::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("columns",MSString(columns()));
  avList_<<MSAttrValue("activate","",MSAttrValue::Callback);
  return MSCompositeField::get(avList_);
}

void MSOptionMenu::set(MSAttrValueList& avList_)
{
  MSCompositeField::set(avList_);
  MSIndexVector index;
  for(unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="columns")
     columns(avList_[i].value().asInt()),index<<i;
   }
  avList_.remove(index);
}

