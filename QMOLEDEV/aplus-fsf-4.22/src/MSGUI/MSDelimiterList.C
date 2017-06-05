///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSDelimiterList.H>
#include <MSGUI/MSGC.H>

const char * const MSDelimiterDefaultForeground="black";
const char * const MSDelimiterDefaultHighlightForeground="yellow";
const char * const MSDelimiterDefaultSelectionBackground="deepskyblue4";
const int MSDelimiterPadding=4;

MSDelimiterList::MSDelimiterList(MSWidget *owner_,const char *title_)
: MSList(owner_,title_)
{
  init();
}

MSDelimiterList::MSDelimiterList(MSWidget *owner_,const MSStringVector &title_)
: MSList(owner_,title_)
{
  init();
}

MSDelimiterList::~MSDelimiterList(void)
{
  if (stipple()!=0) delete stipple();
}

void MSDelimiterList::init(void)
{
  _stipple=0;
  _delimiterColor=server()->pixel(MSDelimiterDefaultForeground);
  _delimiterHighlightColor=server()->pixel(MSDelimiterDefaultHighlightForeground);
  _delimiterSelectionBackground=server()->pixel(MSDelimiterDefaultSelectionBackground);
  _delimiterTitleForeground=foreground();
  _listEdit=MSTrue;
  _delimiterEdit=MSTrue;
  _delimiterSelection=MSTrue;
  _selectedDelimiter=-1;

  XGCValues values;
  values.foreground=delimiterSelectionBackground();
  _segmentGC.setGCValues(server(),MSTrue,&values,GCForeground);

  values.foreground=delimiterColor();
  _delimiterGC.setGCValues(server(),MSFalse,&values,GCForeground);

  values.foreground=delimiterHighlightColor()^background();
  values.line_width=2;
  values.function=GXxor;
  values.subwindow_mode=IncludeInferiors;
  _trackGC.setGCValues(server(),MSTrue,&values,
                       GCForeground|GCLineWidth|GCFunction|GCSubwindowMode);
}

void MSDelimiterList::calculateHeadingsHeight(void)
{
  if (delimiterTitle().length()==0) _headingsHeight=rowHeight()/2+MSDelimiterPadding;
  else _headingsHeight=(int)(rowHeight()*1.5);
}

void MSDelimiterList::updateFont(Font oldfid_)
{
  MSCompositeText::updateFont(oldfid_);
  _rowHeight=textHeight()+(2*rowSpacing());
  calculateHeadingsHeight();
  adjustNumVisible();
  redrawImmediately(); 
}

void MSDelimiterList::updateBackground(unsigned long oldbg_)
{
  trackGC().foreground(delimiterHighlightColor()^background());
  MSList::updateBackground(oldbg_);
}

void MSDelimiterList::redrawImmediately(void)
{
  if (mapped()==MSTrue&&frozen()==MSFalse)
   {
     redrawPixmap()->lock();
     XFillRectangle(display(),redrawPixmap()->pixmap(),backgroundShadowGC(),
		    0,0,panner()->width(),panner()->height());
     drawRows(redrawPixmap()->pixmap(),firstRow(),firstRow()+rows()-1);
     int ht=panner()->highlightThickness();
     MSRect aRect(ht,ht+headingsHeight(),panner()->width()-2*ht,panner()->height()-headingsHeight()-2*ht);
     drawBevel(redrawPixmap()->pixmap(),aRect,panner()->shadowStyle(),panner()->shadowThickness()); 
     drawDelimiterHeaders(redrawPixmap()->pixmap());
     XCopyArea(display(),redrawPixmap()->pixmap(),panner()->window(),
	       backgroundShadowGC(),
	       0,0,panner()->width(),panner()->height(),0,0);
     moveSelection(selectedRow());
     updateScrollBars();
     XFlush(display());
     redrawPixmap()->unlock();
   }
}

void MSDelimiterList::drawDelimiters(Window window_)
{
  int y0=headingsHeight()+panner()->shadowThickness();
  int y1=panner()->height()-panner()->shadowThickness()-1;
  int start=firstColumn();
  int end=lastColumn();
  int widthOffset=panner()->shadowThickness()+panner()->highlightThickness()+columnSpacing();
  for (unsigned i=0;i<delimiterVector().length();i++)
   {
     int index=delimiterVector()(i);
     if (index>=start&&index<=end)
      {
	int x=widthOffset+charWidth()*(index-start+1);
	XDrawLine(display(),window_,delimiterGC().gc(),x,y0,x,y1);
      }
   }
}

void MSDelimiterList::drawDelimiters(Window window_,int row_)
{
  int y0=computeYCoord(row_);
  int y1=(row_==lastRow()?y0+rowHeight():panner()->height()-panner()->shadowThickness()-1);
  int start=firstColumn();
  int end=lastColumn();
  int widthOffset=panner()->shadowThickness()+panner()->highlightThickness()+columnSpacing();
  for (unsigned i=0;i<delimiterVector().length();i++)
   {
     int index=delimiterVector()(i);
     if (index>=start&&index<=end)
      {
	int x=widthOffset+charWidth()*(index-start+1);
	XDrawLine(display(),window_,delimiterGC().gc(),x,y0,x,y1);
      }
   }
}

void MSDelimiterList::drawDelimiterHeaders(Window window_)
{
  int start=firstColumn();
  int end=lastColumn();
  int widthOffset=panner()->shadowThickness()+panner()->highlightThickness()+columnSpacing();
  int xx=widthOffset-(firstColumn()*charWidth());
  int arrowHeight;
  if (delimiterTitle().length()!=0)
   {
     XSetForeground(display(),textGC(),delimiterTitleForeground());
     XDrawString(display(),window_,textGC(),textFontStruct(),xx,headingsHeight()-rowSpacing()-textDescent(),
		 delimiterTitle().string(),delimiterTitle().length());
     arrowHeight=headingsHeight()-rowHeight();
   }
  else arrowHeight=headingsHeight()-MSDelimiterPadding;
  XPoint points[3];
  points[0].y=0;
  points[1].y=points[0].y;
  points[2].y=arrowHeight;
  int y0=arrowHeight;
  int y1=headingsHeight()-1;
  for (unsigned i=0;i<delimiterVector().length();i++)
   {
     int index=delimiterVector()(i);
     if (index>=start&&index<=end)
      {
	int x=widthOffset+charWidth()*(index-start+1);
	points[0].x=x-charWidth()/2;
	points[1].x=points[0].x+charWidth();
	points[2].x=x;
	if (delimiterEdit()==MSFalse) XSetFillStyle(display(),delimiterGC().gc(),FillStippled);
	XBFillPolygon(display(),window_,delimiterGC().gc(),points,3,Convex,CoordModeOrigin);
	if (delimiterEdit()==MSFalse) XSetFillStyle(display(),delimiterGC().gc(),FillSolid);
	XDrawLine(display(),window_,delimiterGC().gc(),x,y0,x,y1);
      }
   }

}

void MSDelimiterList::highlightDelimiter(int col_)
{
  if (col_>=firstColumn()&&col_<=lastColumn()) 
   {
     int arrowHeight;
     if (delimiterTitle().length()!=0) arrowHeight=headingsHeight()-rowHeight();
     else arrowHeight=headingsHeight()-MSDelimiterPadding;
     XSegment segments[2];
     int x=computeDelimiterXCoord(col_);
     segments[0].x1=x;
     segments[0].y1=arrowHeight;
     segments[0].x2=x;
     segments[0].y2=headingsHeight()-1;
     segments[1].x1=x;
     segments[1].y1=headingsHeight()+panner()->shadowThickness();
     segments[1].x2=x;
     segments[1].y2=panner()->height()-panner()->shadowThickness();
     delimiterGC().foreground(background());
     XDrawSegments(display(),panner()->window(),delimiterGC().gc(),segments,2);
     XPoint points[3];
     points[0].x=x-charWidth()/2;
     points[0].y=0;
     points[1].x=points[0].x+charWidth();
     points[1].y=points[0].y;
     points[2].y=arrowHeight;
     points[2].x=x;
     delimiterGC().foreground(delimiterHighlightColor());
     XFillPolygon(display(),panner()->window(),delimiterGC().gc(),points,3,Convex,CoordModeOrigin);
     delimiterGC().foreground(delimiterColor());
   }
}



void MSDelimiterList::delimiterVector(const MSIndexVector &delimiterVector_,int selectedDelimiter_)
{
  _delimiterVector=delimiterVector_;
  _delimiterVector.sortUp();
  // Make sure none of the values of the new vector is out of bound
  int max=actualNumColumns();
  for (unsigned i=0;i<delimiterVector().length();i++)
   {
     if (delimiterVector()(i)>=max)
      {
	_delimiterVector.drop(i-delimiterVector().length());
	break;
      }
   }
  if (selectedDelimiter_<delimiterVector().length())
  _selectedDelimiter=selectedDelimiter_;
  else
  _selectedDelimiter=-1;
  redraw();
}

void MSDelimiterList::delimiterVector(const MSIndexVector &delimiterVector_)
{
  delimiterVector(delimiterVector_,selectedDelimiter());
}

void MSDelimiterList::selectedDelimiter(int selectedDelimiter_)
{
  if (_selectedDelimiter!=selectedDelimiter_)
   {
     if (selectedDelimiter_<delimiterVector().length())
     _selectedDelimiter=selectedDelimiter_;
     else
     _selectedDelimiter=-1;
     redraw();
   }
}

void MSDelimiterList::drawActualRow(Window window_,int row_,const char *pString_,int slen_,
                                    const MSPixmap *pmap_,unsigned long fg_,unsigned long bg_,Font font_,
                                    MSBoolean selected_,MSBoolean selectOutline_)
{
  if (row_>=firstRow()&&row_<firstRow()+rows())
   {
     int offset=panner()->highlightThickness()+panner()->shadowThickness();
     int xoff=offset+columnSpacing();
     int yy=computeYCoord(row_)+rowSpacing();
     int xx=xoff-(firstColumn()*charWidth());
     int ww=panner()->width()-(offset*2);
     //First fill the background, the size of the rectangle is dependent on
     //whether the row is selected or not
     if (selected_==MSTrue)
      {
        int selectedRowHeight=rowHeight()-2*rowSpacing();
	XSetForeground(display(),textGC(),bg_);
	XFillRectangle(display(),window_,textGC(),xoff,yy,
                       ww-(columnSpacing()<<1),selectedRowHeight);
      }
     else
      {
        int y=yy-rowSpacing();
        int h=rowHeight();
	XSetForeground(display(),textGC(),bg_);
	XFillRectangle(display(),window_,textGC(),offset,y,ww,h);
        if (selectedDelimiter()!=-1)
         {
           int selected=delimiterVector()(selectedDelimiter());
           if (selected>=firstColumn())
            {
              int x,w;
              if (selectedDelimiter()==0) x=offset;
              else x=computeDelimiterXCoord(delimiterVector()(selectedDelimiter()-1));
              x=(x<offset?offset:x);
              w=computeDelimiterXCoord(selected)-x+1;
              w=(w>ww?ww:w);
              XFillRectangle(display(),window_,segmentGC().gc(),x,y,w,h);
            }
         }
      }
     //Draw the pixmap if it is there
     if (pmap_!=0)
      {
        int tHeight=textHeight();
        int pHeight=pmap_->height();
        int startx=xx;
        int starty=yy;
        if (tHeight>pHeight) starty+=((tHeight-pHeight)/2);
        else yy+=((pHeight-tHeight)/2);
        int pw=maxPixmapWidth();
        //Optimization: if pixmap is completely offscreen, why draw it
        if (startx+pw>=xoff)
         {
           GC gc=pixmapGC();
	   XSetForeground(display(),gc,fg_);
	   XSetBackground(display(),gc,bg_);
           int psx=xoff-startx;
           int psw=pw-psx;
	   copyPixmap(display(),*pmap_,window_,gc,psx,0,psw,pmap_->height(),
		      xoff,starty,startx,starty);
         }
      }
     xx+=(numPixmapColumns()*charWidth()); 
     //Draw the string
     if (pString_!=0&&slen_>0)
      {
        XSetForeground(display(),textGC(),fg_);
        XSetFont(display(),textGC(),font_);
        const XFontStruct *fontStruct=(font_==font()?
                                       textFontStruct():
                                       server()->fontStruct(font_));
        drawString(display(),window_,textGC(),fontStruct,
                   xx,yy+textAscent(),pString_,slen_);
      }
     if (selectOutline_==MSTrue) drawSelectOutline(window_,row_,highlighted());
     else if (selected_==MSTrue) drawSelectOutline(window_,row_,MSFalse);
   }
  drawDelimiters(window_,row_);  
}

void MSDelimiterList::headingAreaSelection(const XEvent *pEvent_)
{
  if (pEvent_->xbutton.button==Button1)
   {
     delimiterButton1Event(pEvent_);
   }
  else if (pEvent_->xbutton.button==Button2)
   {
     delimiterButton2Event(pEvent_);
   }
}

void MSDelimiterList::dataAreaSelection(const XEvent *pEvent_)
{
  if (listEdit()==MSTrue) MSList::dataAreaSelection(pEvent_);
  else server()->bell();
}

void MSDelimiterList::dragDelimiter(const XEvent *pEvent_)
{
  
  int startCol=columnFromEvent(pEvent_);
  if (delimiterVector().indexOf(startCol)!=delimiterVector().length())
   {
     highlightDelimiter(startCol);
     int rootX,rootY;
     panner()->rootXY(rootX,rootY);
     int x=rootX+computeDelimiterXCoord(startCol);
     int y0,y1;
     if (delimiterTitle().length()!=0) y0=rootY+headingsHeight()-rowHeight();
     else y0=rootY+headingsHeight()-MSDelimiterPadding;
     y1=rootY+panner()->height();
     XDrawLine(display(),server()->root(),trackGC().gc(),x,y0,x,y1);
     MSBoolean lineDrawn=MSTrue;
     Window root,child;
     int rx,ry,ix,iy;
     unsigned keys;
     int col=startCol;
     int last=computeDelimiterXCoord(actualNumColumns()-1);
     int sameScreen=XQueryPointer(display(),panner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
     while (keys&Button1Mask||keys&Button2Mask)
      {
	int newCol=xToColumn(ix);
	if (!sameScreen||ix<0||ix>=panner()->width()||ix>last||
	    iy<-20||iy>=panner()->height()+20||newCol>lastColumn())
	 {
	   if (lineDrawn==MSTrue)
	    {
	      XDrawLine(display(),server()->root(),trackGC().gc(),x,y0,x,y1);
	      lineDrawn=MSFalse;
	      col=-1;
	    }
	 }
	else
	 {
	   if (newCol!=col)
	    {
	      if (lineDrawn==MSTrue) XDrawLine(display(),server()->root(),trackGC().gc(),x,y0,x,y1);
	      col=newCol;
	      x=rootX+computeDelimiterXCoord(col);
	      XDrawLine(display(),server()->root(),trackGC().gc(),x,y0,x,y1);
	      lineDrawn=MSTrue;
	    }
	 }
	sameScreen=XQueryPointer(display(),panner()->window(),&root,&child,&rx,&ry,&ix,&iy,&keys);
      }
     if (lineDrawn==MSTrue) XDrawLine(display(),server()->root(),trackGC().gc(),x,y0,x,y1);

     if (col==-1)
      {
	unsigned index=delimiterVector().indexOf(startCol);
	if (index!=delimiterVector().length()) _delimiterVector.removeAt(index);
	if (selectedDelimiter()>=delimiterVector().length()) _selectedDelimiter=-1;
	delimiterChangeNotify();
      }
     else if (col!=startCol)
      {
	unsigned index=delimiterVector().indexOf(startCol);
	if (index!=delimiterVector().length()) _delimiterVector.removeAt(index);
	if (delimiterVector().indexOf(col)==delimiterVector().length())
	 {
	   _delimiterVector.append(col);
	   _delimiterVector.sortUp();
	 }
	if (selectedDelimiter()>=delimiterVector().length()) _selectedDelimiter=-1;
	delimiterChangeNotify();
      }
     redraw();
   }
}

void MSDelimiterList::selectDelimiter(const XEvent *pEvent_)
{
  int col=xToColumn(pEvent_->xbutton.x-panner()->x_origin()+charWidth()/2);
  for (int i=0;i<delimiterVector().length();i++)
   {
     if (col<=delimiterVector()(i))
      {
	if (_selectedDelimiter!=i)
	 {
	   _selectedDelimiter=i;
	   redraw();
	   delimiterSelectionNotify();
	 }
	return;
      }
   }
  // If we get here, then the last segment is selected
  int max=actualNumColumns()-1;
  _delimiterVector.append(max);
  _selectedDelimiter=delimiterVector().length()-1;
  redraw();
  delimiterSelectionNotify();
}

int MSDelimiterList::computeDelimiterXCoord(int col_)
{
  int widthOffset=panner()->shadowThickness()+panner()->highlightThickness()+columnSpacing();
  return widthOffset+(col_-firstColumn()+1)*charWidth();
}

void MSDelimiterList::addDelimiter(int col_)
{
  if (delimiterVector().indexOf(col_)==delimiterVector().length())
   {
     _delimiterVector.append(col_);
     _delimiterVector.sortUp();
     delimiterChangeNotify();
   }
}

void MSDelimiterList::removeDelimiter(int col_)
{
  unsigned index=delimiterVector().indexOf(col_);
  if (index!=delimiterVector().length())
   {
     _delimiterVector.removeAt(index);
     if (selectedDelimiter()>=delimiterVector().length()) _selectedDelimiter=-1;
     delimiterChangeNotify();
   }
}

int MSDelimiterList::xToColumn(int x_)
{
  int col;
  x_-=(charWidth()/2);
  if (x_<0) col=0;
  else col=(x_-panner()->shadowThickness()-panner()->highlightThickness()-
	    columnSpacing())/charWidth()+firstColumn();
  int max=actualNumColumns();
  return((col>=max)?max-1:col);

}

void MSDelimiterList::delimiterTitle(const MSString &delimiterTitle_)
{
  _delimiterTitle=delimiterTitle_;
  calculateHeadingsHeight();
  redraw();
}

void MSDelimiterList::delimiterColor(const char *color_)
{
  delimiterColor(server()->pixel(color_));
}

void MSDelimiterList::delimiterColor(unsigned long delimiterColor_)
{
  if (_delimiterColor!=delimiterColor_)
   {
     _delimiterColor=delimiterColor_;
     delimiterGC().foreground(delimiterColor());
     redraw();
   }
}

void MSDelimiterList::delimiterHighlightColor(const char *color_)
{
  delimiterHighlightColor(server()->pixel(color_));
}

void MSDelimiterList::delimiterHighlightColor(unsigned long delimiterHighlightColor_)
{
  if (_delimiterHighlightColor!=delimiterHighlightColor_)
   {
     _delimiterHighlightColor=delimiterHighlightColor_;
     delimiterGC().foreground(delimiterHighlightColor()^background());
   }
}

void MSDelimiterList::delimiterSelectionBackground(const char *color_)
{
  delimiterSelectionBackground(server()->pixel(color_));
}

void MSDelimiterList::delimiterSelectionBackground(unsigned long delimiterSelectionBackground_)
{
  if (_delimiterSelectionBackground!=delimiterSelectionBackground_)
   {
     _delimiterSelectionBackground=delimiterSelectionBackground_;
     segmentGC().foreground(delimiterSelectionBackground());
     redraw();
   }
}

void MSDelimiterList::delimiterTitleForeground(const char *color_)
{
  delimiterTitleForeground(server()->pixel(color_));
}

void MSDelimiterList::delimiterTitleForeground(unsigned long delimiterTitleForeground_)
{
  if (_delimiterTitleForeground!=delimiterTitleForeground_)
   {
     _delimiterTitleForeground=delimiterTitleForeground_;
     if (delimiterTitle().length()>0) redraw();
   }
}

void MSDelimiterList::delimiterEdit(MSBoolean delimiterEdit_)
{
  if (_delimiterEdit!=delimiterEdit_)
   {
     _delimiterEdit=delimiterEdit_;
     if (delimiterEdit()==MSFalse)
      {
	if (stipple()==0)
	 {
	   _stipple=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,1,0,1);
	   delimiterGC().stipple(stipple()->pixmap());
	 }
      }
     redraw();
   }
}

void MSDelimiterList::delimiterChangeNotify(void)
{
  activateCallback(MSWidgetCallback::delimiterchange);
}

void MSDelimiterList::delimiterSelectionNotify(void)
{
  activateCallback(MSWidgetCallback::delimiterselection);
}

void MSDelimiterList::updateData(void)
{
  _delimiterVector.removeAll();
  _selectedDelimiter=-1;
  MSList::updateData();
}

void MSDelimiterList::delimiterButton1Event(const XEvent *pEvent_)
{
  if (delimiterEdit()==MSTrue)
   {
     int col=columnFromEvent(pEvent_);
     unsigned index=delimiterVector().indexOf(col);
     if (index==delimiterVector().length())
      {
	addDelimiter(col);
	redraw();
      }
     else if (isDoubleClick(pEvent_)==MSTrue)
      {
	removeDelimiter(col);
	redraw();
      }
     else dragDelimiter(pEvent_);
   }
  else server()->bell();
}

void MSDelimiterList::delimiterButton2Event(const XEvent *pEvent_)
{
  if (delimiterSelection()==MSTrue)
   {
     if (delimiterEdit()==MSFalse&&delimiterVector().length()==0) server()->bell();
     else selectDelimiter(pEvent_);
   }
  else server()->bell();
}


void MSDelimiterList::set(MSAttrValueList& avList_)
{
  MSList::set(avList_);
  MSIndexVector index;
  for(int i=0;i<avList_.length();i++)
   {
     if(avList_[i].attribute()=="delimiterTitle")
        delimiterTitle(avList_[i].value()),index<<i;
     else if(avList_[i].attribute()=="delimiterEdit")
        delimiterEdit(avList_[i].value().asBoolean()),index<<i;
     else if(avList_[i].attribute()=="listEdit")
        listEdit(avList_[i].value().asBoolean()),index<<i;
     else if(avList_[i].attribute()=="delimiterSelection")
        delimiterSelection(avList_[i].value().asBoolean()),index<<i;
     else if(avList_[i].attribute()=="delimiterColor")
        delimiterColor(avList_[i].value()),index<<i;
     else if(avList_[i].attribute()=="delimiterHighlightColor")
        delimiterHighlightColor(avList_[i].value()),index<<i;
     else if(avList_[i].attribute()=="delimiterSelectionBackground")
        delimiterSelectionBackground(avList_[i].value()),index<<i;
     else if(avList_[i].attribute()=="delimiterTitleForeground")
        delimiterTitleForeground(avList_[i].value()),index<<i;
     else if(avList_[i].attribute()=="delimiterVector")
        delimiterVector(MSIndexVector(avList_[i].value())),index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSDelimiterList::get(MSAttrValueList& avList_)
{
  MSStringVector aBoolVector("MSFalse\nMSTrue");
  avList_<<MSAttrValue("delimiterchange","",MSAttrValue::Callback);
  avList_<<MSAttrValue("delimiterselection","",MSAttrValue::Callback);
  avList_<<MSAttrValue("delimiterTitle",delimiterTitle(),MSAttrValue::String);
  avList_<<MSAttrValue("delimiterEdit",aBoolVector(delimiterEdit()),aBoolVector);
  avList_<<MSAttrValue("listEdit",aBoolVector(listEdit()),aBoolVector);
  avList_<<MSAttrValue("delimiterSelection",aBoolVector(delimiterSelection()),aBoolVector);
  avList_<<MSAttrValue("delimiterColor",server()->colorName(delimiterColor()),MSAttrValue::Color);
  avList_<<MSAttrValue("delimiterHighlightColor",
                       server()->colorName(delimiterHighlightColor()),MSAttrValue::Color);
  avList_<<MSAttrValue("delimiterSelectionBackground",
                       server()->colorName(delimiterSelectionBackground()),MSAttrValue::Color);
  avList_<<MSAttrValue("delimiterTitleForeground",
                       server()->colorName(delimiterTitleForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("delimiterVector",delimiterVector().asString(),MSAttrValue::String);
  
  return MSList::get(avList_);
}
