///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSUtil.H>
#include <MSGUI/MSMonthView.H>

static const unsigned long MSMonthViewEventMask=(ExposureMask|ButtonPressMask);

static const int MSMonthViewArrowRepeatInterval   = 100;
static const int MSMonthViewArrowRepeatThreshold  = 200;

void MSMonthView::MonthViewArrow::activate(void)
{
  if (type() == MSArrow::Left) monthView()->prevMonth();
  else monthView()->nextMonth();
}

void MSMonthView::MonthViewArrow::buttonPress(const XEvent *pEvent_)
{
  if (monthView()->isProtected() == MSFalse)
  {
    traverseFocus(monthView());
    buttonPressNotify(this, pEvent_);
  }
}
  
MSMonthView::MonthViewArrow::MonthViewArrow(MSWidget *owner_, MSArrow::ArrowType type_)
: MSArrowButton(owner_, type_)
{
  shadowThickness(0);
  repeatInterval(MSMonthViewArrowRepeatInterval);
  repeatThreshold(MSMonthViewArrowRepeatThreshold);
}

MSMonthView::MSMonthView(MSWidget *pOwner_, const char *title_)
: MSPrimitiveText(pOwner_)
{
  _title=title_;
  init();
  internalCouple(new MSDate(MSDate::today()));
}

MSMonthView::MSMonthView(MSWidget *pOwner_, const MSDate& date_)
: MSPrimitiveText(pOwner_)
{
  init();
  internalCouple(new MSDate(date_));
}

MSMonthView::MSMonthView(MSWidget *pOwner_, MSDate& date_)
: MSPrimitiveText(pOwner_)
{
  init();
  model(date_);
}

MSMonthView::~MSMonthView(void)
{
  if(_leftArrow != 0) delete _leftArrow;
  if(_rightArrow != 0) delete _rightArrow;
}

void MSMonthView::init(void)
{
  _shadowThickness=0;
  _highlightThickness=1;
  _shadowStyle=MSRaised;
  _margin=2;
  _leftArrow = 0;
  _rightArrow = 0;
  _arrowButtons = MSFalse;
  _showSelection = MSTrue;
  _showGrid = MSFalse;
  _titleFormat = MonthYear;
  freeze();
  acceptFocus(MSTrue);
  selectInput(MSMonthViewEventMask);
  addToFocusList();
}

void MSMonthView::viewDate(const MSDate& aDate_)
{ if (MSView::model()!= 0) (MSDate&)*_model = aDate_; }

void MSMonthView::model(MSDate& aDate_)
{ couple(&aDate_); }

void MSMonthView::model(const MSDate& aDate_)
{ constCouple(&aDate_); }


void MSMonthView::arrowButtons(MSBoolean arrowButtons_)
{
  if(_arrowButtons != arrowButtons_)
   {
     _arrowButtons = arrowButtons_;
     if(arrowButtons() == MSTrue)
      {
	if( _leftArrow == 0) _leftArrow = new MonthViewArrow(this, MSArrow::Left);
	if( _rightArrow == 0) _rightArrow = new MonthViewArrow(this, MSArrow::Right);
	_leftArrow->show();
	_rightArrow->show();
        configure();
      }
     else
      {
	if(_leftArrow != 0) _leftArrow->hide();
	if(_rightArrow != 0) _rightArrow->hide();
      }
     redraw();
   }
}

void MSMonthView::titleFormat(TitleFormat format_)
{
  if(_titleFormat != format_)
   {
     _titleFormat = format_;
     redraw();
   }
}

void MSMonthView::showSelection(MSBoolean showSelection_)
{
  if(_showSelection != showSelection_)
   {
     _showSelection = showSelection_;
     redraw();
   }
}

void MSMonthView::update(const MSIndexVector& /*index_*/)
{
  if (MSView::model() != 0)
   {
     MSDate aDate=viewDate();
     aDate.setFirstDayOfMonth();
     _firstDayOffset=aDate.weekDay()%7;
     redraw();
   }
}

void MSMonthView::updateData(void)
{
  if(MSView::model()==0)  internalCouple(new MSDate(MSDate::today()));
  else
   {
     MSDate aDate=viewDate();
     aDate.setFirstDayOfMonth();
     _firstDayOffset=aDate.weekDay()%7;
     redraw();
   }
}

void MSMonthView::computeSize(void)
{ resize(idealWidth(),idealHeight()); }

void MSMonthView::firstMapNotify(void)
{
  computeSize();
  unfreeze();
}

void MSMonthView::redraw(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
   {
     drawBackground();
     drawShadow();
     if (highlighted() == MSTrue) drawHighlight();
     drawSeparators();
     drawCalendar();
   }
}

void MSMonthView::drawCalendar(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue)
   {
     drawTitle();
     drawDayHeadings();
     drawDayGrid();
   }
}


void MSMonthView::drawDayGrid(void)  
{
  int yy = highlightThickness()+shadowThickness();
  int xoffset= yy + outerMargin();
  int xx=xoffset;
  MSDay k=1;
  MSDay day=1;
  unsigned daysInMonth=viewDate().daysInMonth();
  
  yy+=2*_dayRect.height();
  int textOffset = (_dayRect.height()+ textAscent())/2;
  
  for (unsigned i=0;i<6;i++)
   {
     for (unsigned j=0;j<7;j++)
      {
        if (k>_firstDayOffset&&day<=daysInMonth)
	 {
	   MSString aString(day);
	   int tw=textWidth(aString.string(),aString.length());
	   int lx=xx+((_dayRect.width()-tw)/2);
	   XDrawString(display(),window(),textGC(),textFontStruct(),
		       lx,yy+textOffset,aString.string(),aString.length());
	   if (day==selectedDay())
	    {
	      MSRect aRect(xx,yy,_dayRect.width()+2,_dayRect.height()+2);
	      drawBevel(aRect,MSRaised,2);
	    }
	   day++;	   
	 }
	xx+=_dayRect.width();
	k++;
      }
     xx=xoffset;
     yy+=_dayRect.height();
   }
}

void MSMonthView::drawSelection(MSBoolean draw_)
{
  GridLocation aGridLocation(gridLocationFor(selectedDay()));
  
  int offset=highlightThickness()+shadowThickness();
  int xx=offset + outerMargin() + _dayRect.width()*aGridLocation.column();
  int yy=offset +_dayRect.height()*(2+aGridLocation.row());

  MSRect aRect(xx,yy,_dayRect.width()+2,_dayRect.height()+2);
  if (draw_==MSTrue) drawBevel(aRect,MSRaised,2);
  else undrawBevel(aRect,2);
}

void MSMonthView::drawDay(MSDay day_)
{
  unsigned daysInMonth=viewDate().daysInMonth();
  if (day_>0&&day_<daysInMonth)
   {
     GridLocation aGridLocation(gridLocationFor(day_));
     
     int offset=highlightThickness()+shadowThickness();
     int xx=offset + outerMargin() + _dayRect.width()*aGridLocation.column();
     int yy=offset + _dayRect.height()*(2+aGridLocation.row());
     int textOffset = (_dayRect.height() + textAscent())/2;
     
     MSString aString(day_);
     int tw=textWidth(aString.string(),aString.length());
     int lx=xx+((_dayRect.width()-tw)/2);
     XDrawString(display(),window(),textGC(),textFontStruct(),
		 lx,yy+textOffset,aString.string(),aString.length());
     if (day_==selectedDay())
      {
	MSRect aRect(xx,yy,_dayRect.width()+2,_dayRect.height()+2);
	drawBevel(aRect,MSRaised,2);
      }
   }
}

MSMonthView::GridLocation MSMonthView::gridLocationFor(MSDay day_)
{
  MSDay gridNumber=(day_+_firstDayOffset)-1;
  unsigned row=gridNumber/7;
  unsigned column=gridNumber%7;
  return GridLocation(row,column);
}
  
void MSMonthView::drawTitle(void)
{
  MSString aString;
  aString<<viewDate().nameOfMonth();

  if (titleFormat() == MonthYear) aString <<" "<<MSString(viewDate().year());

  int offset=highlightThickness()+shadowThickness();
  int yy=offset + _dayRect.y();
  int xx=(width()-textWidth(aString.string(),aString.length()))/2;

  XSetForeground(display(),textGC(), titleForeground());
  XDrawString(display(),window(),textGC(),textFontStruct(),
	      xx, yy+textAscent(),aString.string(),aString.length());
  XSetForeground(display(),textGC(), foreground());
  
}

void MSMonthView::drawDayHeadings(void)
{
  static const char *dayLabels[]={"S","M","T","W","T","F","S"};

  int offset=highlightThickness()+shadowThickness();
  int xx=offset + outerMargin();
  int yy=offset +_dayRect.height() +(_dayRect.height()-textAscent())/2;
  for (unsigned i=0;i<7;i++)
   {
     int tw=textWidth(dayLabels[i],1);
     int lx=xx+((_dayRect.width()-tw)/2);
     XDrawString(display(),window(),textGC(),textFontStruct(),
		 lx,yy+textAscent(),dayLabels[i],1);
     xx+=_dayRect.width();
   }
}

int MSMonthView::idealWidth(void)
{
  int cw=MSUtil::max(charWidth('W'),2*charWidth('0'));
  int dayWidth=cw+2*margin();
  int cumWidth=7*dayWidth+2*(highlightThickness()+shadowThickness() + outerMargin());
  return cumWidth; 
}

int MSMonthView::idealHeight(void)
{
  int dayHeight=textHeight()+2*margin();
  int cumHeight=8*dayHeight+2*(highlightThickness()+shadowThickness()) +2;
  return cumHeight;
}


void MSMonthView::configure(void)
{
  int offset=highlightThickness()+shadowThickness();
  int th=textHeight();
  int cw=MSUtil::max(charWidth('W'),2*charWidth('0'));

  int h = height() - 2 *offset -2;
  int dayHeight= h/8;
  int hMargin=dayHeight-th;

  int w = width() -2 *(offset +outerMargin());
  int dayWidth=w/7;
  int wMargin=dayWidth-cw;
  
  _dayRect.configuration(wMargin/2,hMargin/2,dayWidth,dayHeight);
  
  if(arrowButtons() == MSTrue)
   {
     int ah = charWidth('e');
     int aw = ah;
     int xoffset = offset + wMargin/2;
     int yoffset = offset + hMargin/2 + textAscent()/2 -ah/4;
     yoffset=(yoffset<0)?offset:yoffset;
     xoffset=(xoffset<0)?offset:xoffset;
     
     _leftArrow->resize(aw, ah);
     _rightArrow->resize(aw, ah);
     _leftArrow->moveTo(xoffset, yoffset);
     _rightArrow->moveTo(width() - xoffset  - aw, yoffset);
   }

  redraw();
}

void MSMonthView::showGrid(MSBoolean showGrid_)
{
  if(showGrid() != showGrid_)
   {
     _showGrid=showGrid_;
     redraw();
   }
}

void MSMonthView::margin(int margin_)
{
  if (margin_!=margin())
   {
     _margin=margin_;
     computeSize();
   }
}

unsigned long MSMonthView::arrowColor(void) const
{
  if(_leftArrow!=0) return _leftArrow->arrowColor();
  else return background();
}

void MSMonthView::arrowColor(const char * color_)
{ arrowColor(server()->pixel(color_)); }

void MSMonthView::arrowColor(unsigned long pixel_)
{
  if(_leftArrow!=0) _leftArrow->arrowColor(pixel_);
  if(_rightArrow!=0) _rightArrow->arrowColor(pixel_);
}


void MSMonthView::updateSensitivity(void)
{}

void MSMonthView::updateTitle(void)
{ redraw(); }


void MSMonthView::updateBackground(unsigned long oldbg_)
{
  MSPrimitiveText::updateBackground(oldbg_);
  if(_leftArrow!=0) _leftArrow->background(background());
  if(_rightArrow!=0) _rightArrow->background(background());
  redraw();
}

void MSMonthView::updateForeground(unsigned long oldfg_)
{
  MSPrimitiveText::updateForeground(oldfg_);
  if(oldfg_==titleForeground()) _titleFg=foreground();
  redraw();
}

void MSMonthView::updateFont(Font oldfid_)
{
  MSPrimitiveText::updateFont(oldfid_);
  computeSize();
  redraw();
}

void MSMonthView::up(void)
{ if (selectedDay()>7) selectedDay(selectedDay()-7); }
void MSMonthView::down(void)
{ selectedDay(selectedDay()+7); }
void MSMonthView::right(void)
{ selectedDay(selectedDay()+1); }
void MSMonthView::left(void)
{ selectedDay(selectedDay()-1); }

void MSMonthView::pageUp(void)
{ if(arrowButtons() == MSTrue) nextMonth();}
void MSMonthView::pageDown(void)
{ if(arrowButtons() == MSTrue) prevMonth();}

void MSMonthView::home(void)
{}
void MSMonthView::end(void)
{}
void MSMonthView::escape(void)
{}

void MSMonthView::returnKey(void)
{  activate();}

void MSMonthView::nextMonth(void)
{
  viewDate() += MSTerm(0, 1);
  dateChanged();
}

void MSMonthView::prevMonth(void)
{
  viewDate() -= MSTerm(0, 1);
  dateChanged();
}

void MSMonthView::keyPress(const XEvent *pEvent_,KeySym keysym_,
			      unsigned int state_,const char *)
{
  MSKeyPress keyPress(keysym_, state_);
  if (isProtected()==MSFalse &&keyTranslate(keyPress) == MSFalse)
   {
     switch (keysym_) 
      {
      case XK_Up:     up();        break;
      case XK_Down:   down();      break;
      case XK_Left:   left();      break;
      case XK_Right:  right();     break;
      case XK_Return: returnKey(); break;
      case XK_F29:
      case XK_Prior:  pageUp();    break;
      case XK_F35:
      case XK_Next:   pageDown();  break;
      case XK_F27:
      case XK_Home:   home();      break;
      case XK_R13:
      case XK_End:    end();       break;
      case XK_Escape: escape();    break;
      }
   }
}

void MSMonthView::selectedDay(MSDay selectedDay_)
{
  if (showSelection() == MSTrue && selectedDay_!=selectedDay())
   {
     if (selectedDay_>0&&selectedDay_<=viewDate().daysInMonth())
      {
	drawSelection(MSFalse);
	freeze();
	viewDate() = MSDate(viewDate().month(), selectedDay_, viewDate().year());
	unfreeze();
        drawSeparators();
	drawSelection(MSTrue);
        dateChanged();
      }
   }
}

void MSMonthView::buttonPress(const XEvent *pEvent_)
{
  if (isProtected() == MSTrue) return;
  traverseFocus(this);

  if( showSelection() == MSFalse)
   {
     if (isDoubleClick(pEvent_) == MSTrue) activate();
   }
  else
   {
     int px=pEvent_->xbutton.x;
     int py=pEvent_->xbutton.y;
     
     int offset=highlightThickness()+shadowThickness();
     int xx=offset +outerMargin();
     int yy=offset + 2*_dayRect.height();

     MSRect aRect(xx,yy,width()-xx-offset,height()-yy-offset);
     if (px>aRect.x()&&py>aRect.y()&&
	 px<aRect.x()+aRect.width()&&py<aRect.y()+aRect.height())
      {
	px-=offset;
	int row=(py-aRect.y())/_dayRect.height();
	int column=(px-aRect.x())/_dayRect.width();
        if(column>6) column=6;
	int day=(row*7)+column;

	day+=1;
	day-=_firstDayOffset;
	if(isDoubleClick(pEvent_)==MSTrue && day == selectedDay()) activate();
	else selectedDay(day);
      }
   }
}

int MSMonthView::outerMargin(void) const
{ return charWidth('e')/2; }

void MSMonthView::drawSeparators(void)
{
  if(showGrid() == MSFalse) return;
  int thickness=1 ; //separatorThickness();
  if (thickness>0&&mapped()!=MSFalse&&frozen()!=MSTrue) 
   {
     int offset=shadowThickness() +highlightThickness();
     int y=offset +_dayRect.height();
     int x=offset +outerMargin();

     XRectangle *top=new XRectangle[8];
     XRectangle *bottom=new XRectangle[8];
     int i,n=0;
     int w = _dayRect.width()*7;
     for (i=0;i<8;i++)
      {
        top[n].x=bottom[n].x=x;
        top[n].y=y;
        bottom[n].y=top[n].y+thickness;
        if (i==8)
         {
           top[n].width=w-thickness;
           bottom[n].width=w; 
         }
        else top[n].width=bottom[n].width=w;
        top[n].height=bottom[n].height=thickness;
        n++;
        y+=_dayRect.height();
      }
     if (n>0)
      {
        XBFillRectangles(display(),window(),bottomShadowGC(),top,n);
        XFillRectangles(display(),window(),topShadowGC(),bottom,n);
      }
///Vertical
     n=0;
     y= offset + textHeight() +2*_dayRect.y()+1;
     x= offset+outerMargin();
     int h=_dayRect.height()*7;
     
     for (i=0;i<8;i++)  
      {
        bottom[n].x=x;
        top[n].x=bottom[n].x+thickness;
        bottom[n].y=top[n].y=y;
        if (i==7)
         {
           bottom[n].height=h+thickness;
           top[n].height=h;
         }
        else bottom[n].height=top[n].height=h;
        bottom[n].width=top[n].width=thickness;
        n++;
        x+=_dayRect.width();
      }
     if (n>0)
      {
        XBFillRectangles(display(),window(),bottomShadowGC(),bottom,n);
        XFillRectangles(display(),window(),topShadowGC(),top,n);
      }
     delete [] bottom;
     delete [] top;
   }
}

void MSMonthView::print(const char *file_)
{
  MSBoolean   fileOpen=MSFalse;

  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if (displayPrintOpen(this)==MSTrue) 
      {
	fileOpen=MSTrue;
	outputMode(Print);
	displayPrintXorigin(0);
	displayPrintYorigin(0);
      }
     else return;
   }
  if (mapped()==MSTrue) redraw();
  if (fileOpen==MSTrue)
   {
     displayPrintClose();
     outputMode(Draw);
   }
}

void MSMonthView::activate(void)
{ activateCallback(MSWidgetCallback::activate); }

void MSMonthView::dateChanged(void)
{ activateCallback(MSWidgetCallback::valuechange); }

void MSMonthView::set(MSAttrValueList& avList_)
{
  MSPrimitiveText::set(avList_);
  MSIndexVector index;
  for(unsigned i=0; i<avList_.length(); i++)
   {
     if (avList_[i].attribute()=="arrowButtons")
      arrowButtons(avList_[i].value().asBoolean()), index<<i;
     else if(avList_[i].attribute()=="arrowColor")
      arrowColor(avList_[i].value()), index<<i;
     else if (avList_[i].attribute()=="margin")
      margin(avList_[i].value().asInt()), index<<i;
     else if (avList_[i].attribute()=="titleFormat")
      titleFormat((avList_[i].value()=="Month")? Month : MonthYear),index<<i;
     else if (avList_[i].attribute()=="showSelection")
      showSelection(avList_[i].value().asBoolean()), index<<i;
     else if (avList_[i].attribute()=="showGrid")
      showGrid(avList_[i].value().asBoolean()), index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSMonthView::get(MSAttrValueList& avList_)
{
  MSStringVector aBoolVector = "MSFalse\nMSTrue";
  
  avList_ << MSAttrValue("arrowButtons",
			 (arrowButtons()==MSTrue)?"MSTrue":"MSFalse",aBoolVector);
  avList_ <<MSAttrValue("arrowColor",
                        server()->colorName(arrowColor()),MSAttrValue::Color);
  avList_ << MSAttrValue("margin", MSString(margin()));
  avList_ << MSAttrValue("titleFormat",
			 (titleFormat() == MonthYear)?"MonthYear":"Month",
			 MSStringVector("MonthYear\nMonth"));
  avList_ << MSAttrValue("showSelection",
			 (showSelection()==MSTrue)?"MSTrue":"MSFalse",aBoolVector);
  
  avList_ << MSAttrValue("showGrid",
			 (showGrid() == MSTrue)?"MSTrue":"MSFalse",aBoolVector);
  avList_ <<MSAttrValue("activate","",MSAttrValue::Callback);
  avList_ <<MSAttrValue("valuechange","",MSAttrValue::Callback);
  
  return MSPrimitiveText::get(avList_);
}


