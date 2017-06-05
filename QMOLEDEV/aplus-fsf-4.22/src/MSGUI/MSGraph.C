///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include <unistd.h>
#include <MSTypes/MSMessageLog.H>
#include <MSGUI/MSGraph.H>
#include <MSGUI/MSShadow.H>
#include <MSGUI/MSPrintTool.H>

static const char *DefaultWindowFont="fixed";
static const char *DefaultGraphForeground="white";
static const char *DefaultAxisForeground="white";
static const char *DefaultGridForeground="skyblue";
static const char *DefaultZeroForeground="skyblue";
static const char *DefaultLegendFont="lucidasanstypewriter-10";
static const char *DefaultTitlesFont="lucidasanstypewriter-12";
static const char *DefaultLabelFont="lucidasanstypewriter-bold-12";
static const int   DefaultGraphHeight=200;
static const int   DefaultGraphWidth=300;
static const int   DefaultGraphMargin=0;
static const int   DefaultGraphDepth=0;
static const int   DefaultRuleWidth=0;
static const int   DefaultXlabelInc=0;
static const int   DefaultYlabelInc=0;
static const float DefaultPieOffsetMargin=0.30;
static const int   DefaultBarWidth=10;
static const int   DefaultXnumMinorTicks=1;
static const int   DefaultYnumMinorTicks=1;
static const int   DefaultMinorTickSize=6;
static const int   DefaultMajorTickSize=10;
static const float DefaultShowSizeRatio=0.75;
static const int   DefaultSelectDistance=10;  
static const int   MSGraphMaxBarWidth=35;  
static const int   MSGraphBackingStore=WhenMapped;  
static const unsigned long DefaultRepeatInterval=300;
static const unsigned long DefaultMSGraphDoubleClickTime=500; // miliseconds
static const unsigned long DefaultLongPressTime=500;   // milliseconds
static const MSStringVector DefaultTraceColors("deepskyblue\nyellow\nspringgreen\nturquoise\nplum\nskyblue\nred\npowderblue\ngreen\norange\npurple\nblack");
unsigned long MSGraph::_doubleClickEventTime=0;
unsigned long MSGraph::_doubleClickTime=DefaultMSGraphDoubleClickTime;
unsigned long MSGraph::_longPressTime=DefaultLongPressTime;
MSBoolean     MSGraph::_debugStatus=MSFalse;
MSBoolean     MSGraph::debug(void) {return _debugStatus;}
void	      MSGraph::debug(MSBoolean status_) {_debugStatus=status_;}
MSLabelFormat MSGraph::_outFormat;
MSLabelOut    MSGraph::_labelOut;
MSLabelOut    MSGraph::_timeLabelOut;

const unsigned int MSGraph::_UnsetValue=0x5F5F5F5F;
const double       MSGraph::_PrecisionRange=1e13;
const int          MSGraph::_MinBufSize=10;
const int          MSGraph::_SHRT_MAX=(int)(SHRT_MAX/2);
const int          MSGraph::_SHRT_MIN=(int)(SHRT_MIN/2);

MSString MSGraph::_defaultTraceTag="trend"; 
unsigned long MSGraphEventMask=ExposureMask|ButtonPressMask|ButtonReleaseMask;

MSGraphStatusWin::MSGraphStatusWin(MSWidget *owner_):MSPrimitive(owner_)
{
  _gc=XCreateGC(display(),window(),0,0);
  _font=server()->fontID(DefaultWindowFont);
  _fontInfo=(XFontStruct *)server()->fontStruct(_font);
  _spacing=_fontInfo->ascent;
  _valueWidth=0;
  _x1=0;
  _x2=0;
  _minWidth=0;
  _leading=4;
  _insideMargin=5;
  _highlightThickness=1;
  _shadowThickness=0;
  background(owner_->background());
  foreground(owner_->foreground());
  backingStore(MSGraphBackingStore);
  highlightColor(foreground());
  XSetForeground(display(),gc(),foreground());
  XSetBackground(display(),gc(),background());
}
 
MSGraphStatusWin::~MSGraphStatusWin(void) { XFreeGC(display(),_gc); }

void MSGraphStatusWin::font(Font font_) 
{
  if (font_!=0) 
   {
     _font=font_;
     _fontInfo=(XFontStruct *)graph()->server()->fontStruct(font_);
     XSetFont(display(),_gc,font_); 
     spacing(fontInfo()->ascent);
   }
}

void MSGraphStatusWin::updateBackground(unsigned long)
{
   shadow().color(background());
   if (mapped()==MSTrue)
   {
      if (highlighted()==MSTrue) drawHighlight();
      drawShadow();
   }
}

MSGraphLegend::MSGraphLegend(MSWidget *owner_):MSGraphStatusWin(owner_)
{ 
  _firstDraw=MSTrue; 
  _symbolWidth=2*spacing();
  font(server()->fontID(DefaultLegendFont));
  background(owner_->background());
  foreground(owner_->foreground());
  highlightColor(foreground());
  _textFieldWidth=0;
  highlightThickness(1);
  shadowThickness(1);
  insideMargin(5);
  selectInput(ExposureMask);
}

void MSGraphLegend::buttonPress(const XEvent *event_) 
{
  if (event_->xbutton.button==Button1)
   {
     if (graph()->editor()->mapped()==MSTrue) graph()->editorEscape();
     else if (graph()->doubleClick(event_)==MSTrue&&graph()->selectTrace()==0)
      {
	graph()->highlightLegendStatus(MSTrue);
	graph()->moveLegend(event_);
      }
     else if (graph()->highlightLegendStatus()==MSTrue) graph()->moveLegend(event_);
   }
}

void MSGraphLegend::font(Font font_) 
{
  MSGraphStatusWin::font(font_); 
  symbolWidth(2*spacing());
}

void MSGraphLegend::symbolFont(Font font_) 
{
  if (font_!=symbolFont()) 
   {
     _symbolFont=font_; 
     XSetFont(display(),gc(),font_);
   }
}

void MSGraphLegend::redraw(void)
{
  graph()->computeLegendSize();
  graph()->drawLegends();
  graph()->positionLegend(graph()->legendAlignment());
}

/*
 *    MSGraphEditor member functions
 *
 */
MSGraphEditor::MSGraphEditor(MSWidget *owner_):MSText(owner_)
{
  highlightThickness(1);
  shadowThickness(1);
  background(owner_->background());
  foreground(owner_->foreground());
  highlightColor(foreground());
}

MSGraphEditor::~MSGraphEditor(void) {}
MSBoolean MSGraphEditor::traverseFocus(MSWidget *)    {return MSTrue;}
MSWidget *MSGraphEditor::inputFocus(void)             {return this;}
const MSWidget *MSGraphEditor::inputFocus(void) const {return this;}    

void MSGraphEditor::save(void) { graph()->editorActivate(); }
void MSGraphEditor::escape(void)   { graph()->editorEscape(); }
void MSGraphEditor::stringVector(const MSStringVector& aStringVector_)
{
  MSString aString;
  for (unsigned i=0;i<aStringVector_.length();i++) aString<<aStringVector_(i)<<'\n';
  if (aString.length()>0) aString.drop(-1);
  string(aString);
}

MSGraphRepeatTimer::MSGraphRepeatTimer(MSGraph *g_,unsigned long i_):MSIntervalTimer(i_) {_graph=g_;}
MSGraphRepeatTimer::~MSGraphRepeatTimer(void) {}
void MSGraphRepeatTimer::process(void) {graph()->processRepeatTimer();}

MSGraphDoubleClickTimer::MSGraphDoubleClickTimer(MSGraph *g_,unsigned long i_):MSRegularTimer(i_,0) {_graph=g_;}
MSGraphDoubleClickTimer::~MSGraphDoubleClickTimer(void) {}
void MSGraphDoubleClickTimer::process(void) {graph()->buttonTimerExpire(MSTrue);}

MSGraph::MSGraph(MSWidget *owner_,const char *title_,const MSSymbol& tag_) :
MSComposite(owner_), _tag(tag_) { title(title_); init(); }
MSGraph::MSGraph(MSWidget *owner_,const MSStringVector& title_, const MSSymbol& tag_) :
MSComposite(owner_), _tag(tag_) { title(title_); init(); }

MSGraph::~MSGraph(void)
{
  int i;
  freeze();
  traceSetList().freeze();
  traceList().freeze();
  for (i=0;i<traceSetList().count();i++) safeDestroy(graphTraceSet(i));

  if (graphPixmap()!=0) delete _graphPixmap;
  if (clearGC()!=0) XFreeGC(display(),clearGC());
  if (axisGC()!=0) XFreeGC(display(),axisGC());
  if (gridGC()!=0) XFreeGC(display(),gridGC());
  if (zeroGC()!=0) XFreeGC(display(),zeroGC());
  if (traceGC()!=0) XFreeGC(display(),traceGC());
  if (windowGC()!=0) XFreeGC(display(),windowGC());
  if (axisTitleGC()!=0) XFreeGC(display(),axisTitleGC());
  if (subtitleGC()!=0) XFreeGC(display(),subtitleGC());
  if (footnoteGC()!=0) XFreeGC(display(),footnoteGC());
  if (editor()!=0) safeDestroy(editor());
  if (legend()!=0) safeDestroy(legend());
  if (dataWin()!=0) safeDestroy(dataWin());
  if (_drawCursor!=0) delete _drawCursor;
  if (_lineCursor!=0) delete _lineCursor;
  if (_zoomCursor!=0) delete _zoomCursor;
  if (_repeatTimer!=0) delete _repeatTimer;
  for (i=0; i<_newtraceAllocCt; i++) if (_nt[i]!=0) {delete _nt[i];_nt[i]=0;}
  if (_nt!=0) delete [] _nt;
  if (_yGrid!=0) delete [] _yGrid;
  if (_yGridWidth!=0) delete [] _yGridWidth;
  if (_xGridWidth!=0) delete [] _xGridWidth;
  if (_xGrid!=0) delete [] _xGrid;
  if (printManager()!=0) printManager()->removePrintItem(this);
  clearPieData();
}

void MSGraph::init(void)
{
  selectInput(MSGraphEventMask);
  acceptTab(MSTrue);
  _timeLabelOut.format(MSTime::HoursMinutesSeconds);
  _xLegendPosition          =0;
  _yLegendPosition          =0;

  _xGrid                    =0;
  _yGrid                    =0;
  _yGridWidth               =0;
  _xGridWidth               =0;
  _nt                       =0;
  _newtraceCt               =0;
  _newtraceIndex            =0;
  _newtraceAllocCt          =0;
  _selectPoint              =0;
  _selectLine               =0;
  _focusLine                =0;
  
  _repeatTimer=new MSGraphRepeatTimer(this,DefaultRepeatInterval);
  _repeatTimer->stop();
  _repeatOn                 =MSFalse;
  _keyPressStatus           =MSFalse;
  _updateLegendStatus       =MSTrue;
  _buttonTimerExpire        =MSTrue;

  _subtitleForeground       =MSWidget::foreground();
  _footnoteForeground       =_subtitleForeground;
  _gridForeground           =server()->pixel(DefaultGridForeground);
  _zeroAxisForeground       =server()->pixel(DefaultZeroForeground);
  _subtitleFont     	    =server()->fontID(DefaultTitlesFont);
  _footnoteFont    	    =_subtitleFont;
  
  _titleAlign               =MSCenter;
  _subtitleAlign            =MSCenter;
  _footnoteAlign            =MSLeft;
  _orientation              =Vertical;
  _graphMode                =Standard;
  _graphUIMode              =Normal;
  _axisRuleWidth            =DefaultRuleWidth;
  _grid	                    =0;
  _gridStyle                =MSDash;
  _gridWidth                =DefaultRuleWidth;
  _gridWeight               =2;
  _zeroAxis                 =MSBottom|MSLeft;
  _zeroAxisStyle            =MSDot;
  _zeroAxisWidth            =DefaultRuleWidth;
  _zeroAxisWeight           =1;
  _cursorType               =(int)XYcursor;
  _legendAlign              =MSTop|MSLeft;
  _legendStyle              =Vertical;
  _axis                     =Std;
  _axisRule                 =Axis;
  _drawEndTicks             =MSTrue;
  _titleHeight              =0;
  _subtitleHeight           =0;
  _footnoteHeight           =0;
  _selectTrace              =0;
  _yStringWidth             =0;
  _xStringWidth             =0;
  _xMinZoom                 =0;
  _xMaxZoom                 =0;
  _yMinZoom                 =0;
  _yMaxZoom                 =0;
  
  for (int i=0; i<2; i++)
   {
     _yMode[i]               =Ascending;
     _xMode[i]               =Ascending;
     _xMinorTickSize[i]      =DefaultMinorTickSize;
     _xMajorTickSize[i]      =DefaultMajorTickSize;
     _yMinorTickSize[i]      =DefaultMinorTickSize;
     _yMajorTickSize[i]      =DefaultMajorTickSize;
     _xAxisFg[i]             =server()->pixel(DefaultAxisForeground);
     _yAxisFg[i]             =_xAxisFg[i];
     _xLabelFont[i]          =server()->fontID(DefaultLabelFont);
     _yLabelFont[i]          =_xLabelFont[i];
     _xTitleFont[i]          =_subtitleFont;
     _yTitleFont[i]          =_subtitleFont;
     _xTitleAlign[i]         =MSCenter;
     _yTitleAlign[i]         =Horizontal;
     _margin[i]              =DefaultGraphMargin;
     _margin[i+2]            =DefaultGraphMargin;
     _xMargin[i][0]          =0;
     _xMargin[i][1]          =0;
     _yMargin[i][0]          =0;
     _yMargin[i][1]          =0;
     _xTickStyle[i]          =Outside;
     _yTickStyle[i]          =Outside;
     _showYaxis[i]           =_showYrule[i]=i==0?MSTrue:MSFalse; 
     _showXaxis[i]           =_showXrule[i]=i==0?MSTrue:MSFalse;
     _xTitleFg[i]            =_subtitleForeground;
     _yTitleFg[i]            =_subtitleForeground;
     _xSubLabelAlign[i]      =MSCenter;
     _xLabelAlign[i]         =MSCenter;
     _yLabelAlign[i]         =MSCenter;
     _xMinReal[i]            =0;
     _xMaxReal[i]            =0;
     _xMin[i]                =(double)unsetValue();
     _xMax[i]                =(double)unsetValue();
     _yMin[i]                =(double)unsetValue();
     _yMax[i]                =(double)unsetValue();

     _xMinData[i]            =(double)unsetValue(); // jmiz 
     _yMinData[i]            =(double)unsetValue(); // jmiz 

     _xMaxData[i]            =(double)unsetValue(); // jmiz 
     _yMaxData[i]            =(double)unsetValue(); // jmiz 

     _xScale[i]              =(double)unsetValue(); // jmiz 
     _yScale[i]              =(double)unsetValue(); // jmiz 


     _xMinSet[i]    	     =(double)unsetValue();
     _xMaxSet[i]             =(double)unsetValue();
     _yMinSet[i]             =(double)unsetValue();
     _yMaxSet[i]             =(double)unsetValue();
     _yLabelInc[i]           =DefaultYlabelInc;
     _yLabelInc[i]           =DefaultYlabelInc;
     _xLabelInc[i][0]        =DefaultXlabelInc;
     _xLabelInc[i][1]        =DefaultXlabelInc;
     _xNumMinorTicks[i]      =DefaultXnumMinorTicks;
     _yNumMinorTicks[i]      =DefaultYnumMinorTicks;
     _xTitle[i]              ="";
     _yTitle[i]              ="";
     _yIncData[i]            =DefaultYlabelInc;
     _xIncData[i][0]         =DefaultXlabelInc;
     _xIncData[i][1]         =DefaultXlabelInc;
   }

  _x_org=_x_end=_y_org=_y_end=0;
  
  //  initialize default trace colors
  _defaultLineColors=DefaultTraceColors;
  _defaultFillColors=DefaultTraceColors;
  //  initialize graph parameters 
  _plotAreaRect.width(DefaultGraphWidth);
  _plotAreaRect.height(DefaultGraphHeight);
  _graphDepth               =DefaultGraphDepth;
  _showSizeRatio            =DefaultShowSizeRatio;
  _selectDistance           =DefaultSelectDistance;
  
  // initialize pie graph parameters
  _pieOffsetMargin          =DefaultPieOffsetMargin;
  _pieCount                 =0;

  // initialize bar graph parameters
  _maxBarWidth              =DefaultBarWidth;
  _barWidth                 =DefaultBarWidth;
  _xBar	                    =0;
  _barCount                 =0;

  // initialize MarketProfile parameters
  _marketProfileStatus      =MSFalse;
  _sessionPeriod            =86400;
  _sessionOffset            =0;
  _tpoPriceInc              =0;
  _tpoPeriod                =1800;
  _tpoOpen                  =28800;
  _tpoClose                 =57600;
  _tpoBreakChar             =0;

  // initialize status parameters
  _highlightLegendStatus    =MSFalse;
  _highlightTraceStatus     =MSFalse;
  _graphZoomStatus 	    =MSFalse;
  _xShowPos	            =MSFalse;
  _yShowPos		    =MSFalse;
  _onLineSegment	    =MSFalse;
  _onLinePoint		    =MSFalse;

  //  create GCs and pixmap
  XGCValues 		gcValues;
  unsigned long 	gcMask;
  gcMask=GCForeground|GCBackground;
  gcValues.foreground=MSWidget::background();
  gcValues.background=MSWidget::background();
  _clearGC=XCreateGC(display(),window(),gcMask,&gcValues);
           
  gcMask=GCForeground|GCBackground|GCFunction;
  gcValues.foreground=server()->pixel(DefaultGraphForeground)^background();
  gcValues.background=0;
  gcValues.function=GXxor;
  _windowGC=XCreateGC(display(),window(),gcMask,&gcValues);
           
  gcMask=GCForeground|GCBackground|GCLineWidth|GCFont;
  gcValues.foreground=foreground();
  gcValues.line_width=_axisRuleWidth;
  gcValues.font=_subtitleFont;
  _axisGC=XCreateGC(display(),window(),gcMask,&gcValues);
  
  gcMask=GCForeground|GCBackground|GCLineWidth|GCLineStyle;
  gcValues.foreground=gridForeground();
  gcValues.line_width=gridWidth();
  gcValues.line_style=LineOnOffDash;
  _gridGC=XCreateGC(display(),window(),gcMask,&gcValues);
  
  gcMask=GCForeground|GCBackground|GCLineWidth|GCLineStyle;
  gcValues.foreground=zeroAxisForeground();
  gcValues.line_width=zeroAxisWidth();
  gcValues.line_style=LineOnOffDash;
  _zeroGC=XCreateGC(display(),window(),gcMask,&gcValues);
  
  gcMask=GCLineStyle;
  gcValues.line_style=LineSolid;
  gcValues.fill_rule=EvenOddRule;
  _traceGC=XCreateGC(display(),window(),gcMask,&gcValues);
  
  gcMask=GCForeground|GCBackground|GCFont;
  gcValues.foreground=_xTitleFg[0];
  gcValues.font=_subtitleFont;
  _axisTitleGC=XCreateGC(display(),window(),gcMask,&gcValues);
  
  gcMask=GCForeground|GCBackground|GCFont;
  gcValues.foreground=_subtitleForeground;
  gcValues.font=_subtitleFont;
  _subtitleGC=XCreateGC(display(),window(),gcMask,&gcValues);
  
  gcMask=GCForeground|GCBackground|GCFont;
  gcValues.foreground=_footnoteForeground;
  gcValues.font=_subtitleFont;
  _footnoteGC=XCreateGC(display(),window(),gcMask,&gcValues);

  _graphPixmap=new MSBackingStorePixmap(server(),"MSGeneral");
  _zoomCursor=new MSDisplayCursor(server(),XC_left_ptr,
			    server()->pixel("white"),server()->pixel("black"));
  _drawCursor=new MSDisplayCursor(server(),XC_crosshair,
			    server()->pixel("white"),server()->pixel("black"));
  _lineCursor=new MSDisplayCursor(server(),XC_left_ptr,
                            server()->pixel("red"),server()->pixel("blue"));

  // initialize subwindows
  // it is necesary to initialize these, because otherwise
  // they are treated as traceSets in insertChild
  _legend=0;
  _dataWin=0;
  _editor=0;
  _legend=new MSGraphLegend(this);
  _dataWin=new MSGraphStatusWin(this);
  _editor=new MSGraphEditor(this);
  //temporary kludge
  _editor->map();
  _editor->unmap();
  _dataWin->map();
  _dataWin->moveTo(10,10);
  _dataWin->unmap();
  _legend->map();
  _legend->moveTo(10,10);
  _legend->unmap();
  
  //  initialize printItem stuff
  _frameLineWidth=0;
  _style=MSNone;
  topPixel(4);
  bottomPixel(4);
  _printWidth=0;
  
  //  initialize misc window stuff
  MSWidget::backingStore(MSGraphBackingStore);
  _highlightThickness=2;
  _shadowThickness=2;
  shadowStyle(MSEtchedIn);
  resize(DefaultGraphWidth,DefaultGraphHeight);
  _MaxBufSize=(int)XMaxRequestSize(display());
  addToFocusList();
  destroyable(MSFalse);

  
  // initialize pie parameters
  _startAngle=0;
  _indexOfMaxAngle=0;
  _orderDependent=MSFalse;
  _pieData=0;
  _pieLabelData=0;
}

void MSGraph::printDebugInfo(const char *str_)
{
  MSString symbol;
  MSMessageLog::debugMessage(symbol<<"::"<<str_<<"\n");
}

const MSTraceSet *MSGraph::graphTraceSet(const MSSymbol& tag_) const
{
  int ct=traceSetList().count(); for (unsigned i=0;i<ct;i++) if (graphTraceSet(i)->tag()==tag_) return graphTraceSet(i);
  MSMessageLog::errorMessage("MSGraph::graphTraceSet tag not found\n");
  return 0;
}

MSTraceSet *MSGraph::graphTraceSet(const MSSymbol& tag_)
{
  int ct=traceSetList().count();
  for (unsigned i=0;i<ct;i++) if (graphTraceSet(i)->tag()==tag_) return graphTraceSet(i);
  MSMessageLog::errorMessage("MSGraph::graphTraceSet tag not found\n");
  return 0;
}

const MSTrace *MSGraph::graphTrace(const MSSymbol& tag_) const
{
  int ct=traceList().count();
  for (unsigned i=0;i<ct;i++) if (graphTrace(i)->tag()==tag_) return graphTrace(i);
  MSMessageLog::errorMessage("MSGraph::graphTrace tag not found\n");
  return 0;
}

MSTrace *MSGraph::graphTrace(const MSSymbol& tag_)
{
  int ct=traceList().count();
  for (unsigned i=0;i<ct;i++) if (graphTrace(i)->tag()==tag_) return graphTrace(i);
  MSMessageLog::errorMessage("MSGraph::graphTrace tag not found\n");
  return 0;
}

MSBoolean MSGraph::loseFocus(void)
{
  if (editor()->mapped()==MSTrue) editorActivate(); 
  focusOut(); 
  return MSTrue;
}

void MSGraph::focusIn(void)          
{
  highlight(); 
  if (editor()->mapped()==MSTrue) takeFocusNotify(editor());
}

void MSGraph::focusOut(void)         
{
  unHighlight(); 
  if (editor()->mapped()==MSTrue) loseFocusNotify(editor());
}

void MSGraph::firstMapNotify(void)    
{
  _naturalWidth=width(); 
  _naturalHeight=height();
}

void MSGraph::redraw(void)            
{
  computeLegendSize();
  redrawImmediately();
  positionLegend(legendAlignment());
  if (highlighted()==MSTrue) drawHighlight();
}

void MSGraph::naturalSize(void)       
{resize(_naturalWidth,_naturalHeight);}
void MSGraph::computeSize(void)       
{redrawImmediately();}

void MSGraph::childInsert(MSWidget *widget_)  
{insertChild(widget_);}
void MSGraph::childRemove(MSWidget *widget_)  
{removeChild(widget_);}
void MSGraph::childCreate(MSWidget *widget_)  
{insertChild(widget_);}
void MSGraph::childDestroy(MSWidget *widget_) 
{removeChild(widget_);}

void MSGraph::insertChild(MSWidget *widget_) 
{ 
  if (editor()!=0&&dataWin()!=0&&legend()!=0)
   {
     MSTraceSet *ts=(MSTraceSet *)widget_;
     traceSetList().add(ts); 
     updateData();
     updateLegendStatus(MSTrue); 
     if (visible() !=  ts->visible())
       {
	 if(visible()==MSTrue) visibilityUnobscuredNotify(ts);
	 else visibilityObscuredNotify(ts);
       }
   }
}

void MSGraph::removeChild(MSWidget *widget_) 
{ 
  MSTraceSet *ts=(MSTraceSet *)widget_;
  traceSetList().remove(ts); 
  if (mapped()==MSTrue&&frozen()==MSFalse) updateData();
  updateLegendStatus(MSTrue); 
}

MSBoolean MSGraph::showXaxis(int i_)
{return graphMode()&PieChart?MSFalse:_showXaxis[i_];}

MSBoolean MSGraph::showYaxis(int i_)
{return graphMode()&PieChart?MSFalse:_showYaxis[i_];}

MSBoolean MSGraph::showXrule(int i_)
{return graphMode()&PieChart?MSFalse:_showXrule[i_];}

MSBoolean MSGraph::showYrule(int i_)
{return graphMode()&PieChart?MSFalse:_showYrule[i_];}

void MSGraph::barWidth(int x_)
{_barWidth=x_<=MSGraphMaxBarWidth?x_:MSGraphMaxBarWidth;}

void MSGraph::subtitle(const MSStringVector& x_)
{
  if (_subtitle!=x_)
   {
     _subtitle=x_;
     updateSubtitle();
   }
}

void MSGraph::subtitleAlignment(MSAlignment x_)
{
  if (x_!=_subtitleAlign)
   {
     _subtitleAlign=x_;
     updateSubtitle();
   }
}

void MSGraph::subtitleForeground(const char *x_)
{subtitleForeground(server()->pixel(x_));}
void MSGraph::subtitleForeground(unsigned long x_) 
{
  if (x_!=_subtitleForeground) 
   {
     _subtitleForeground=x_; 
     XSetForeground(display(),_subtitleGC,_subtitleForeground); 
     drawSubtitle(window());
   }
}

void MSGraph::subtitleFont(const char *font_) 
{subtitleFont(server()->fontID(font_));}
void MSGraph::subtitleFont(Font font_) 
{
  if (font_!=_subtitleFont) 
   {
     _subtitleFont=font_;
     XSetFont(display(),_subtitleGC,font_); 
     updateSubtitle();
   }
}

void MSGraph::footnote(const MSStringVector& x_)
{
  if (_footnote!=x_)
   {
     _footnote=x_;
     updateFootnote();
   }
}

void MSGraph::footnoteAlignment(MSAlignment x_)
{
  if (x_!=_footnoteAlign)
   {
     _footnoteAlign=x_;
     updateFootnote();
   }
}

void MSGraph::footnoteForeground(const char *x_) 
{footnoteForeground(server()->pixel(x_));}
void MSGraph::footnoteForeground(unsigned long x_) 
{
  if (x_!=_footnoteForeground)
   {
     _footnoteForeground=x_;
     XSetForeground(display(),_footnoteGC,_footnoteForeground);
     drawFootnote(window());
   }
}

void MSGraph::footnoteFont(const char *font_) 
{footnoteFont(server()->fontID(font_));}
void MSGraph::footnoteFont(Font font_) 
{
  if (font_!=_footnoteFont)
   {
     _footnoteFont=font_;
     XSetFont(display(),_footnoteGC,font_);
     updateFootnote();
   }
}

void MSGraph::axisMinimum(double x_,unsigned long i_)
{
  MSBoolean changed=MSFalse;
  double x=fabs(x_);
  if (x<DBL_MAX&&x>DBL_MIN||x==0)
   {
     if (i_&MSLeft&&fabs(_yMin[0]-x_)>DBL_MIN)   { _yMin[0]=_yMinSet[0]=x_; changed=MSTrue; }
     if (i_&MSRight&&fabs(_yMin[1]-x_)>DBL_MIN)  { _yMin[1]=_yMinSet[1]=x_; changed=MSTrue; }
     if (i_&MSBottom&&fabs(_xMin[0]-x_)>DBL_MIN) { _xMin[0]=_xMinSet[0]=x_; changed=MSTrue; }
     if (i_&MSTop&&fabs(_xMin[1]-x_)>DBL_MIN)    { _xMin[1]=_xMinSet[1]=x_; changed=MSTrue; }
     if (changed==MSTrue) redrawImmediately();
   }
}

void MSGraph::axisMaximum(double x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  double x=fabs(x_);
  if (x<DBL_MAX&&x>DBL_MIN||x==0)
   {
     if (i_&MSLeft&&fabs(_yMax[0]-x_)>DBL_MIN)   { _yMax[0]=_yMaxSet[0]=x_; changed=MSTrue; }
     if (i_&MSRight&&fabs(_yMax[1]-x_)>DBL_MIN)  { _yMax[1]=_yMaxSet[1]=x_; changed=MSTrue; }
     if (i_&MSBottom&&fabs(_xMax[0]-x_)>DBL_MIN) { _xMax[0]=_xMaxSet[0]=x_; changed=MSTrue; }
     if (i_&MSTop&&fabs(_xMax[1]-x_)>DBL_MIN)    { _xMax[1]=_xMaxSet[1]=x_; changed=MSTrue; }
     if (changed==MSTrue) redrawImmediately();
   }
}

void MSGraph::axisLabelIncrement(double x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  double x=fabs(x_);
  if (x<DBL_MAX&&x>DBL_MIN)
   {
     if (i_&MSLeft&&fabs(_yLabelInc[0]-x_)>DBL_MIN)      { _yLabelInc[0]=x_;    changed=MSTrue; }
     if (i_&MSRight&&fabs(_yLabelInc[1]-x_)>DBL_MIN)     { _yLabelInc[1]=x_;    changed=MSTrue; }
     if (i_&MSBottom&&fabs(_xLabelInc[0][0]-x_)>DBL_MIN) { _xLabelInc[0][0]=x_; changed=MSTrue; }
     if (i_&MSTop&&fabs(_xLabelInc[1][0]-x_)>DBL_MIN)    { _xLabelInc[1][0]=x_; changed=MSTrue; }
     if (changed==MSTrue) redrawImmediately();
   }
}

void MSGraph::axisSubLabelIncrement(double x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSBottom&&fabs(x_-_xLabelInc[0][1])>DBL_MIN) { _xLabelInc[0][1]=x_; changed=MSTrue; }
  if (i_&MSTop&&fabs(x_-_xLabelInc[1][1])>DBL_MIN)    { _xLabelInc[1][1]=x_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::axisTitle(const MSStringVector& str_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&str_!=_yTitle[0])   { _yTitle[0]=str_; changed=MSTrue; }
  if (i_&MSRight&&str_!=_yTitle[1])  { _yTitle[1]=str_; changed=MSTrue; }
  if (i_&MSBottom&&str_!=_xTitle[0]) { _xTitle[0]=str_; changed=MSTrue; }
  if (i_&MSTop&&str_!=_xTitle[1])    { _xTitle[1]=str_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::axisTitleForeground(const char *fg_,unsigned long i_)
{
  axisTitleForeground(server()->pixel(fg_),i_);
}

void MSGraph::axisTitleForeground(unsigned long fg_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yTitleFg[0]!=fg_)   { _yTitleFg[0]=fg_; changed=MSTrue; }
  if (i_&MSRight&&_yTitleFg[1]!=fg_)  { _yTitleFg[1]=fg_; changed=MSTrue; }
  if (i_&MSBottom&&_xTitleFg[0]!=fg_) { _xTitleFg[0]=fg_; changed=MSTrue; }
  if (i_&MSTop&&_xTitleFg[1]!=fg_)    { _xTitleFg[1]=fg_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::axisTitleFont(const char *font_,unsigned long i_)
{
  axisTitleFont(server()->fontID(font_),i_);
}

void MSGraph::axisTitleFont(unsigned long font_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yTitleFont[0]!=font_)   { _yTitleFont[0]=font_; changed=MSTrue; }
  if (i_&MSRight&&_yTitleFont[1]!=font_)  { _yTitleFont[1]=font_; changed=MSTrue; }
  if (i_&MSBottom&&_xTitleFont[0]!=font_) { _xTitleFont[0]=font_; changed=MSTrue; }
  if (i_&MSTop&&_xTitleFont[1]!=font_)    { _xTitleFont[1]=font_; changed=MSTrue; }
  if (changed==MSTrue)
   {
     XSetFont(display(),_axisTitleGC,font_);
     redrawImmediately();
   }
}

void MSGraph::axisTitleAlignment(MSAlignment align_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yTitleAlign[0]!=align_)   { _yTitleAlign[0]=align_; changed=MSTrue; }
  if (i_&MSRight&&_yTitleAlign[1]!=align_)  { _yTitleAlign[1]=align_; changed=MSTrue; }
  if (i_&MSBottom&&_xTitleAlign[0]!=align_) { _xTitleAlign[0]=align_; changed=MSTrue; }
  if (i_&MSTop&&_xTitleAlign[1]!=align_)    { _xTitleAlign[1]=align_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::axisTitleAlignment(unsigned long align_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yTitleAlign[0]!=align_)   { _yTitleAlign[0]=formatAlignment(align_); changed=MSTrue; }
  if (i_&MSRight&&_yTitleAlign[1]!=align_)  { _yTitleAlign[1]=formatAlignment(align_); changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::axisLabelAlignment(MSAlignment align_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yLabelAlign[0]!=align_)   { _yLabelAlign[0]=align_; changed=MSTrue; }
  if (i_&MSRight&&_yLabelAlign[1]!=align_)  { _yLabelAlign[1]=align_; changed=MSTrue; }
  if (i_&MSBottom&&_xLabelAlign[0]!=align_) { _xLabelAlign[0]=align_; changed=MSTrue; }
  if (i_&MSTop&&_xLabelAlign[1]!=align_)    { _xLabelAlign[1]=align_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::axisSubLabelAlignment(MSAlignment align_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSBottom&&_xSubLabelAlign[0]!=align_) { _xSubLabelAlign[0]=align_; changed=MSTrue; }
  if (i_&MSTop&&_xSubLabelAlign[1]!=align_)    { _xSubLabelAlign[1]=align_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::axisLabelOut(MSLabelOutPtr out_,unsigned long i_)
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yLabelOut[0]!=out_)   { _yLabelOut[0]=out_; changed=MSTrue; }
  if (i_&MSRight&&_yLabelOut[1]!=out_)  { _yLabelOut[1]=out_; changed=MSTrue; }
  if (i_&MSBottom&&_xLabelOut[0]!=out_) { _xLabelOut[0]=out_; changed=MSTrue; }
  if (i_&MSTop&&_xLabelOut[1]!=out_)    { _xLabelOut[1]=out_; changed=MSTrue; }
  if (changed==MSTrue)
   {
     out_->owner(this);
     redrawImmediately();
   }
}

void MSGraph::axisSubLabelOut(MSLabelOutPtr x_,unsigned long i_)
{
  MSBoolean changed=MSFalse;
  if (i_&MSBottom&&_xSubLabelOut[0]!=x_) { _xSubLabelOut[0]=x_;changed=MSTrue; }
  if (i_&MSTop&&_xSubLabelOut[1]!=x_)    { _xSubLabelOut[1]=x_;changed=MSTrue; }
  if (changed==MSTrue)
   {
     x_->owner(this);
     redrawImmediately();
   }
}

void MSGraph::axisForeground(const char *fg_,unsigned long i_)
{
  axisForeground(server()->pixel(fg_),i_);
}

void MSGraph::axisForeground(unsigned long fg_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yAxisFg[0]!=fg_)   { _yAxisFg[0]=fg_; changed=MSTrue; }
  if (i_&MSRight&&_yAxisFg[1]!=fg_)  { _yAxisFg[1]=fg_; changed=MSTrue; }
  if (i_&MSBottom&&_xAxisFg[0]!=fg_) { _xAxisFg[0]=fg_; changed=MSTrue; }
  if (i_&MSTop&&_xAxisFg[1]!=fg_)    { _xAxisFg[1]=fg_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::axisLabelFont(const char *font_,unsigned long i_)
{
  axisLabelFont(server()->fontID(font_),i_);
}

void MSGraph::axisLabelFont(unsigned long font_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yLabelFont[0]!=font_)   { _yLabelFont[0]=font_; changed=MSTrue; }
  if (i_&MSRight&&_yLabelFont[1]!=font_)  { _yLabelFont[1]=font_; changed=MSTrue; }
  if (i_&MSBottom&&_xLabelFont[0]!=font_) { _xLabelFont[0]=font_; changed=MSTrue; }
  if (i_&MSTop&&_xLabelFont[1]!=font_)    { _xLabelFont[1]=font_; changed=MSTrue; }
  if (changed==MSTrue)
   {
     XSetFont(display(),_axisGC,font_);
     redrawImmediately();
   }
}

void MSGraph::axisRuleWidth(int x_) 
{
  if (_axisRuleWidth!=x_)
   {
     _axisRuleWidth=x_>10?10:x_; 
     XSetLineAttributes(display(),axisGC(),_axisRuleWidth,LineSolid,CapButt,JoinMiter);
     redrawImmediately();
   }
}

void MSGraph::tickStyle(unsigned long x_,unsigned long i_)  
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yTickStyle[0]!=x_)   {_yTickStyle[0]=x_; changed=MSTrue; }
  if (i_&MSRight&&_yTickStyle[1]!=x_)  {_yTickStyle[1]=x_; changed=MSTrue; }
  if (i_&MSBottom&&_xTickStyle[0]!=x_) {_xTickStyle[0]=x_; changed=MSTrue; }
  if (i_&MSTop&&_xTickStyle[1]!=x_)    {_xTickStyle[1]=x_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::minorTicks(unsigned x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yNumMinorTicks[0]!=x_)   {_yNumMinorTicks[0]=x_; changed=MSTrue; }
  if (i_&MSRight&&_yNumMinorTicks[1]!=x_)  {_yNumMinorTicks[1]=x_; changed=MSTrue; }
  if (i_&MSBottom&&_xNumMinorTicks[0]!=x_) {_xNumMinorTicks[0]=x_; changed=MSTrue; }
  if (i_&MSTop&&_xNumMinorTicks[1]!=x_)    {_xNumMinorTicks[1]=x_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::majorTickSize(unsigned x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yMajorTickSize[0]!=x_)   {_yMajorTickSize[0]=x_; changed=MSTrue; }
  if (i_&MSRight&&_yMajorTickSize[1]!=x_)  {_yMajorTickSize[1]=x_; changed=MSTrue; }
  if (i_&MSBottom&&_xMajorTickSize[0]!=x_) {_xMajorTickSize[0]=x_; changed=MSTrue; }
  if (i_&MSTop&&_xMajorTickSize[1]!=x_)    {_xMajorTickSize[1]=x_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::minorTickSize(unsigned x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yMinorTickSize[0]!=x_)   {_yMinorTickSize[0]=x_; changed=MSTrue; }
  if (i_&MSRight&&_yMinorTickSize[1]!=x_)  {_yMinorTickSize[1]=x_; changed=MSTrue; }
  if (i_&MSBottom&&_xMinorTickSize[0]!=x_) {_xMinorTickSize[0]=x_; changed=MSTrue; }
  if (i_&MSTop&&_xMinorTickSize[1]!=x_)    {_xMinorTickSize[1]=x_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::maxBarWidth(unsigned x_)
{
  if (x_!=_maxBarWidth)
   {
     _maxBarWidth=x_>35?35:x_;
     redrawImmediately();
   }
}

void MSGraph::grid(unsigned long x_)                 
{
  if (grid()!=x_) 
   { 
     _grid=x_; 
     if ((_grid&MSLeft)&&(_grid&MSRight)) _grid-=MSRight;
     if ((_grid&MSTop)&&(_grid&MSBottom)) _grid-=MSTop; 
     redrawImmediately();
   }
}

void MSGraph::gridStyle(unsigned long x_) 
{
  if (gridStyle()!=x_) 
   {
     _gridStyle=x_;
     setLineAttributes(gridStyle(),gridWeight(),gridGC(),gridWidth(),CapButt,JoinMiter);
     redrawImmediately();
   }
}

void MSGraph::gridWidth(unsigned x_) 
{
  if (gridWidth()!=x_) 
   {
     _gridWidth=x_>10?10:x_; 
     setLineAttributes(gridStyle(),gridWeight(),gridGC(),gridWidth(),CapButt,JoinMiter);
     redrawImmediately();
   }
}

void MSGraph::gridWeight(unsigned x_)                     
{
  if (gridWeight()!=x_) 
   {
     _gridWeight=x_>4?4:x_;
     setLineAttributes(gridStyle(),gridWeight(),gridGC(),gridWidth(),CapButt,JoinMiter);
     redrawImmediately();
   }
}

void MSGraph::gridForeground(const char *x_) 
{
  gridForeground(server()->pixel(x_));
}

void MSGraph::gridForeground(unsigned long x_) 
{
  if (gridForeground()!=x_) 
   {
     _gridForeground=x_; 
     XSetForeground(display(),_gridGC,_gridForeground); 
     redrawImmediately();
   }
}

void MSGraph::zeroAxis(unsigned long x_)
{
  if (zeroAxis()!=x_) 
   { 
     _zeroAxis=x_; 
     if ((_zeroAxis&MSLeft)&&(_zeroAxis&MSRight)) _zeroAxis-=MSRight;
     if ((_zeroAxis&MSTop)&&(_zeroAxis&MSBottom)) _zeroAxis-=MSTop; 
     redrawImmediately();
   }
}

void MSGraph::zeroAxisStyle(unsigned long x_) 
{
  if (zeroAxisStyle()!=x_)
   {
     _zeroAxisStyle=x_; 
     setLineAttributes(zeroAxisStyle(),zeroAxisWeight(),zeroGC(),zeroAxisWidth(),CapButt,JoinMiter);
     redrawImmediately();
   }
}

void MSGraph::zeroAxisWidth(unsigned x_) 
{
  if (zeroAxisWidth()!=x_) 
   {
     _zeroAxisWidth=x_>10?10:x_; 
     setLineAttributes(zeroAxisStyle(),zeroAxisWeight(),zeroGC(),zeroAxisWidth(),CapButt,JoinMiter);
     redrawImmediately();
   }
}

void MSGraph::zeroAxisWeight(unsigned x_)                     
{
  if (zeroAxisWeight()!=x_) 
   {
     _zeroAxisWeight=x_>4?4:x_;
     setLineAttributes(zeroAxisStyle(),zeroAxisWeight(),zeroGC(),zeroAxisWidth(),CapButt,JoinMiter);
     redrawImmediately();
   }
}

void MSGraph::zeroAxisForeground(const char *x_) 
{
  zeroAxisForeground(server()->pixel(x_));
}

void MSGraph::zeroAxisForeground(unsigned long x_) 
{
  if (zeroAxisForeground()!=x_) 
   {
     _zeroAxisForeground=x_; 
     XSetForeground(display(),_zeroGC,_zeroAxisForeground); 
     redrawImmediately();
   }
}

void MSGraph::topAxisDataMargin(double x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (x_>=0.&&x_<100.)
   {
     if (i_&MSLeft&&fabs(x_-_xMargin[1][0])>DBL_MIN)  { _xMargin[1][0]=x_<1.?x_:x_/100.; changed=MSTrue; }
     if (i_&MSRight&&fabs(x_-_xMargin[1][1])>DBL_MIN) { _xMargin[1][1]=x_<1.?x_:x_/100.; changed=MSTrue; }
     if (changed==MSTrue) redrawImmediately();
   }
  else 
   {
     MSMessageLog::warningMessage("MSGraph::topAxisDataMargin value out of range\n");
   }
}

void MSGraph::bottomAxisDataMargin(double x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (x_>=0.&&x_<100.)
   {
     if (i_&MSLeft&&fabs(x_-_xMargin[0][0])>DBL_MIN)  { _xMargin[0][0]=x_<1.?x_:x_/100.; changed=MSTrue; }
     if (i_&MSRight&&fabs(x_-_xMargin[0][1])>DBL_MIN) { _xMargin[0][1]=x_<1.?x_:x_/100.; changed=MSTrue; }
     if (changed==MSTrue) redrawImmediately();
   }
  else 
   {
     MSMessageLog::warningMessage("MSGraph::bottomAxisDataMargin value out of range\n");
   }
}

void MSGraph::leftAxisDataMargin(double x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  if (x_>=0.&&x_<100.)
   {
     x_=x_>1?x_/100.:x_;
     if (i_&MSBottom&&fabs(x_-_yMargin[0][0])>DBL_MIN) { _yMargin[0][0]=x_; changed=MSTrue; }
     if (i_&MSTop&&fabs(x_-_yMargin[0][1])>DBL_MIN)    { _yMargin[0][1]=x_; changed=MSTrue; }
     if (changed==MSTrue) redrawImmediately();
   }
  else 
   {
     MSMessageLog::warningMessage("MSGraph::leftAxisDataMargin value out of range\n");
   }
}

void MSGraph::rightAxisDataMargin(double x_,unsigned long i_) 
{
  MSBoolean changed=MSFalse;
  x_=x_>1?x_/100.:x_;
  if (x_>=0.&&x_<1.)
   {
     if (i_&MSBottom&&fabs(x_-_yMargin[1][0])>DBL_MIN) { _yMargin[1][0]=x_; changed=MSTrue; }
     if (i_&MSTop&&fabs(x_-_yMargin[1][1])>DBL_MIN)    { _yMargin[1][1]=x_; changed=MSTrue; }
     if (changed==MSTrue) redrawImmediately();
   }
  else 
   {
     MSMessageLog::warningMessage("MSGraph::rightAxisDataMargin value out of range\n");
   }
}

void MSGraph::xLegendPosition(double x_) 
{
  x_=x_<1.?x_:x_/100.;
  if (x_>=0.&&x_<1.)
   {
     if (fabs(x_-_xLegendPosition)>DBL_MIN)
      {
	_xLegendPosition=x_; 
	positionLegend(_legendAlign);
      }
   }
  else 
   {
     MSMessageLog::warningMessage("MSGraph::xLegendPosition value out of range\n");
   }
}

void MSGraph::yLegendPosition(double x_) 
{
  x_=x_<1.?x_:x_/100.;
  if (x_>=0.&&x_<1.)
   {
     if (fabs(x_-_yLegendPosition)>DBL_MIN)
      {
	_yLegendPosition=x_<1.?x_:x_/100.; 
	positionLegend(_legendAlign);
      }
   }
  else 
   {
     MSMessageLog::warningMessage("MSGraph::yLegendPosition value out of range\n");
   }
}

void MSGraph::axisMode(AxisMode x_,unsigned long i_)
{
  MSBoolean changed=MSFalse;
  if (i_&MSLeft&&_yMode[0]!=x_)   { _yMode[0]=x_; changed=MSTrue; }
  if (i_&MSRight&&_yMode[1]!=x_)  { _yMode[1]=x_; changed=MSTrue; }
  if (i_&MSBottom&&_xMode[0]!=x_) { _xMode[0]=x_; changed=MSTrue; }
  if (i_&MSTop&&_xMode[1]!=x_)    { _xMode[1]=x_; changed=MSTrue; }
  if (changed==MSTrue) redrawImmediately();
}

void MSGraph::margin(double x_,unsigned long i_)
{
  if ((x_=x_>1?x_/100.:x_)>=0&&x_<1.)
   {
     MSBoolean changed=MSFalse;
     if (i_&MSLeft&&fabs(x_-_margin[0])>DBL_MIN)   { _margin[0]=x_; changed=MSTrue; }
     if (i_&MSRight&&fabs(x_-_margin[1])>DBL_MIN)  { _margin[1]=x_; changed=MSTrue; }
     if (i_&MSTop&&fabs(x_-_margin[2])>DBL_MIN)    { _margin[2]=x_; changed=MSTrue; }
     if (i_&MSBottom&&fabs(x_-_margin[3])>DBL_MIN) { _margin[3]=x_; changed=MSTrue; }
     if (changed==MSTrue) redrawImmediately();
   }
  else 
   {
     MSMessageLog::warningMessage("MSGraph::margin value out of range\n");
   }
}

void MSGraph::orientation(MSG::Orientation x_)
{
  if (x_!=_orientation)
   {
     _orientation=x_;
     redrawImmediately();
   }
}

void MSGraph::pieOffsetMargin(double x_)
{
  if ((x_=x_>1?x_/100.:x_)>=0&&x_<1.)
   {
     if (fabs(x_-_pieOffsetMargin)>DBL_MIN)
      {
        _pieOffsetMargin=x_;
        redrawImmediately();
      }
   }
  else MSMessageLog::warningMessage("MSGraph pie margin value out of range\n");
}

void MSGraph::sessionPeriod(double x_) 
{
  if (x_!=_sessionPeriod&&fabs(x_)<DBL_MAX)
   {
     _sessionPeriod=x_; 
     redrawImmediately();
   }
}

void MSGraph::sessionOffset(double x_)
{
  if (x_!=_sessionOffset&&fabs(x_)<DBL_MAX)
   {
     _sessionOffset=x_; 
     redrawImmediately();
   }
}

void MSGraph::tpoPriceInc(double x_) 
{
  if (x_!=_tpoPriceInc&&fabs(x_)<DBL_MAX) 
   {
     _tpoPriceInc=x_; 
     redrawImmediately();
   }
}

void MSGraph::tpoPeriod(double x_) 
{
  if (x_!=_tpoPeriod&&fabs(x_)<DBL_MAX) 
   {
     _tpoPeriod=x_;
     redrawImmediately();
   }
}

void MSGraph::tpoOpen(double x_) 
{
  if (x_!=_tpoOpen&&fabs(x_)<DBL_MAX) 
   {
     _tpoOpen=x_; 
     redrawImmediately();
   }
}

void MSGraph::tpoClose(double x_) 
{
  if (x_!=_tpoClose&&fabs(x_)<DBL_MAX) 
   {
     _tpoClose=x_; 
     redrawImmediately();
   }
}

Font MSGraph::legendFont(void) const
{return legend()->font();}
void MSGraph::legendFont(const char *str_)
{ legendFont(server()->fontID(str_));}
void MSGraph::legendFont(unsigned long fid_) 
{
  if (fid_>0&&fid_!=legend()->font())
   {
     legend()->font(fid_); 
     legend()->redraw(); 
   }
}

unsigned long MSGraph::legendForeground(void) const
{return legend()->foreground();}
void MSGraph::legendForeground(const char *str_)
{ legendForeground(server()->pixel(str_));}
void MSGraph::legendForeground(unsigned long color_) 
{
  if (color_!=legend()->foreground())
   {
     legend()->foreground(color_); 
     drawLegends(); 
   }
}

unsigned long MSGraph::legendBackground(void) const
{return legend()->background();}
void MSGraph::legendBackground(const char *str_)
{ legendBackground(server()->pixel(str_));}
void MSGraph::legendBackground(unsigned long color_) 
{ 
  if (color_!=legend()->background())
   { 
     legend()->background(color_); 
     drawLegends(); 
   }
}

int MSGraph::legendHighlightThickness(void) const
{return legend()->highlightThickness();}
void MSGraph::legendHighlightThickness(int width_) 
{ 
  if (width_!=legend()->highlightThickness())
   {
     legend()->highlightThickness(width_); 
     legend()->redraw();
   }
}

int MSGraph::legendShadowThickness(void) const
{return legend()->shadowThickness();}
void MSGraph::legendShadowThickness(int width_) 
{
  if (width_!=legend()->shadowThickness())
   {
     legend()->shadowThickness(width_); 
     legend()->redraw();
   }
}

void MSGraph::legendAlignment(MSAlignment align_)
{legendAlignment((unsigned long)align_);}
void MSGraph::legendAlignment(unsigned long align_) 
{
  if ((align_=formatAlignment(align_))!=_legendAlign)
   {	
     if (_legendAlign==MSNone) legend()->map();
     else if (align_==MSNone) legend()->unmap();
     unsigned long oldAlign=_legendAlign;
     _legendAlign=align_;
     if (oldAlign&Outside || align_&Outside)
      {
        // redraw every time when legend was outside or become outside
	redrawImmediately();
	legend()->redraw(); 
      }
     else positionLegend(legendAlignment()); 
   }
} 

void MSGraph::legendStyle(unsigned long style_) 
{ 
  if (style_!=_legendStyle) 
   {
     _legendStyle=style_; 
     legend()->valueWidth(0); 
     computeLegendSize();
     if (legendAlignment()&Outside)
      {
	updateLegendStatus(MSTrue);
	redrawImmediately();
      }
     else drawLegends();
     positionLegend(legendAlignment());
   }
} 

void MSGraph::updateFont(Font oldfont_)
{
  Font fid=MSWidget::font();
  if (fid!=oldfont_)
   { 
     dataWin()->font(fid);
     for (int i=0; i<2; i++)
      {
	if (oldfont_==yTitleFont(i)) _yTitleFont[i]=fid;
	if (oldfont_==xTitleFont(i)) _xTitleFont[i]=fid;
	if (oldfont_==yLabelFont(i)) _yLabelFont[i]=fid;
	if (oldfont_==xLabelFont(i)) _xLabelFont[i]=fid;
      }
     if (oldfont_==subtitleFont())
      {
	_subtitleFont=fid;
	XSetFont(display(),subtitleGC(),fid); 
      }
     if (oldfont_==footnoteFont())
      {
	_footnoteFont=fid;
	XSetFont(display(),footnoteGC(),fid); 
      }
     XSetFont(display(),dataWin()->gc(),fid);
     XSetFont(display(),windowGC(),fid);
     editor()->font(fid);
     if (oldfont_==legend()->font()) legend()->font(fid);
     updateLegendStatus(MSTrue); 
     redrawImmediately(); 
   }
}

void MSGraph::updateForeground(unsigned long oldfg_) 
{ 
  MSComposite::updateForeground(oldfg_);
  unsigned long pixel=MSWidget::foreground();
  if (pixel!=oldfg_)
   { 
     dataWin()->highlightColor(pixel);
     for (int i=0; i<2; i++)
      {
	if (oldfg_==yTitleForeground(i)) _yTitleFg[i]=pixel;
	if (oldfg_==xTitleForeground(i)) _xTitleFg[i]=pixel;
	if (oldfg_==yAxisForeground(i)) _yAxisFg[i]=pixel;
	if (oldfg_==xAxisForeground(i)) _xAxisFg[i]=pixel;
      }
     if (oldfg_==subtitleForeground())
      {
	_subtitleForeground=pixel;
	XSetForeground(display(),subtitleGC(),pixel); 
      }
     if (oldfg_==footnoteForeground())
      {
	_footnoteForeground=pixel;
	XSetForeground(display(),footnoteGC(),pixel); 
      }
     XSetForeground(display(),dataWin()->gc(),pixel);
     XSetForeground(display(),windowGC(),pixel^background());
     editor()->foreground(pixel);
     editor()->highlightColor(pixel);
     if (oldfg_==legend()->foreground()) legend()->foreground(pixel);
     updateLegendStatus(MSTrue); 
     redrawImmediately(); 
   } 
}
  
void MSGraph::updateBackground(unsigned long oldbg_)
{
  MSComposite::updateBackground(oldbg_);
  unsigned long pixel=background(); 
  if (pixel!=oldbg_)
   {
     if (legendBackground()==oldbg_) legendBackground(pixel);
     if (editor()->background()==oldbg_) editor()->background(pixel);
     XSetForeground(display(),clearGC(),pixel);
     XSetBackground(display(),clearGC(),pixel);
     XSetForeground(display(),windowGC(),foreground()^pixel);
     redrawImmediately();
   }
}

void MSGraph::updateTitle()
{
  if (mapped()==MSTrue)
   {
     XFontStruct *fi=(XFontStruct *)server()->fontStruct(titleFont());
     int h=(fi->ascent+fi->descent)*title().length();
     if (h==titleHeight())
      {
	XFillRectangle(display(),window(),clearGC(),offset(),offset(),width()-2*offset(),titleHeight());
	drawTitle(window());
      }
     else redrawImmediately();
   }
}

void MSGraph::updateSubtitle()
{
  if (mapped()==MSTrue)
   {
     XFontStruct *fi=(XFontStruct *)server()->fontStruct(subtitleFont());
     int h=(fi->ascent+fi->descent)*subtitle().length();
     if (h==subtitleHeight())
      {
	XFillRectangle(display(),window(),clearGC(),offset(),offset()+titleHeight(),
		       width()-2*offset(),subtitleHeight());
	drawSubtitle(window());
      }
     else redrawImmediately();
   }
}

void MSGraph::updateFootnote()
{
  if (mapped()==MSTrue)
   {
     XFontStruct *fi=(XFontStruct *)server()->fontStruct(footnoteFont());
     int h=(fi->ascent+fi->descent)*footnote().length();
     if (h==footnoteHeight())
      {
	XFillRectangle(display(),window(),clearGC(),offset(),height()-footnoteHeight()-offset(),
		       width()-2*offset(),footnoteHeight());
	drawFootnote(window());
      }
     else redrawImmediately();
   }
}

void MSGraph::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;

  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if (displayPrintOpen(this)==MSTrue) 
      {
	fileOpen=MSTrue;
	outputMode(Print);
      }
     else return;
   }
  redrawForPrint();
  if (fileOpen==MSTrue) 
   {
     displayPrintClose();
     outputMode(Draw);
   }
}

void MSGraph::redrawForPrint(void)
{
  redrawImmediately();
  if (legend()->mapped()==MSTrue)
   {
     displayPrintOriginInc(legend());
     drawLegends();
     displayPrintOriginDec(legend());
   }
}

void MSGraph::redrawImmediately(MSBoolean update_,MSBoolean append_)
{
  if ((mapped()==MSTrue&&frozen()!=MSTrue&&update_!=MSTrue)||outputMode()==Print)
   {
     _graphPixmap->lock();
     XUndefineCursor(display(),window());
     busyOn();
     if (debug()==MSTrue) printDebugInfo(outputMode()==Draw?"redraw":"print");
     XFillRectangle(display(),graphPixmap(),clearGC(),
		    offset(),offset(),width()-2*offset(),height()-2*offset());
     drawTitle(graphPixmap());
     drawSubtitle(graphPixmap());
     drawFootnote(graphPixmap());
     computeExtents();
     computeScales();
     computeXincrement();
     drawAxes();
     drawGrid();
     drawRule();
     drawZeroAxis();
     drawGraph();
     drawXtitle();
     drawYtitle();
     XCopyArea(display(),graphPixmap(),window(),clearGC(),offset(),offset(),
	       width()-2*offset(),height()-2*offset(),offset(),offset());
     drawShadow();
     drawLineSegments();
     drawLineHandles();
     if (selectTrace()!=0&&(selectTrace()->xOffset()!=0||selectTrace()->yOffset()!=0))
      {
	drawMoveTrace(selectTrace());
	drawLineHandles(selectTrace());
      }
     busyOff();
     if (graphUIMode()==AddTrace||graphUIMode()==MoveTrace)
      {
	XDefineCursor(display(),window(),drawCursor());
      }
     else if (graphZoomStatus()==MSTrue)
      {
	XDefineCursor(display(),window(),zoomCursor());
      }
     else XUndefineCursor(display(),window());
     if (updateLegendStatus()==MSTrue) drawLegends();
     _graphPixmap->unlock();
   }
  else if (update_==MSTrue)
   {
     _graphPixmap->lock();
     if (debug()==MSTrue) printDebugInfo(append_==MSTrue?"append":"update");
     XFillRectangle(display(),graphPixmap(),clearGC(),
		    offset(),offset(),width()-2*offset(),height()-2*offset());
     drawGrid();
     drawRule();
     drawZeroAxis();
     drawGraph(update_);
     XCopyArea(display(),graphPixmap(),window(),clearGC(),updateRect()->x(),updateRect()->y(),
	       updateRect()->width(),updateRect()->height(),updateRect()->x(),updateRect()->y());
     if (legendStyle()==LastValue) drawScanXvalues();
     if (selectTrace()!=0&&(selectTrace()->xOffset()!=0||selectTrace()->yOffset()!=0))
      {
	drawMoveTrace(selectTrace());
	drawLineHandles(selectTrace());
      }
     if (updateLegendStatus()==MSTrue) drawLegends();
     _graphPixmap->unlock();
   }     
}

void MSGraph::redrawSansRescale(void)
{
  XFillRectangle(display(),graphPixmap(),clearGC(),
		 offset(),offset(),width()-2*offset(),height()-2*offset());
  drawGrid();
  drawRule();
  drawZeroAxis();
  drawGraph();
  XCopyArea(display(),graphPixmap(),window(),clearGC(),x_org(),plotAreaRect()->y(),
	    plotAreaRect()->width(),plotAreaRect()->height(),x_org(),plotAreaRect()->y());
}

void MSGraph::configure(void)
{
  _graphPixmap->resize(width(),height());
  if (width()>0&&height()>0)
   {
     _plotAreaRect.width(width());
     _plotAreaRect.height(height());
   }
  positionLegend(legendAlignment());
/*
  if (highlightLegendStatus()!=MSTrue&&
      (legend()->height()>(_showSizeRatio*height())||
      legend()->width()>(_showSizeRatio*width())))
   {
     legend()->unmap();
   }
  drawLegends();
  
  if (legend()->mapped()!=MSTrue&&legendAlignment()!=0)
   {
     legend()->map();
   }
*/
  if (editor()->mapped()==MSTrue) editTextTrace();
}

void MSGraph::unfreeze(void)
{
  freezeStatus(MSFalse);
  legend()->redraw();
  redrawImmediately();
}

MSBoolean MSGraph::doubleClick(void)
{
  if (buttonTimerExpire()==MSTrue)
   {
     buttonTimerExpire(MSFalse);
     MSGraphDoubleClickTimer *t=new MSGraphDoubleClickTimer(this,doubleClickTime());
     return MSFalse;
   }
  return MSTrue;
}

MSBoolean MSGraph::doubleClick(const XEvent *event_)
{
  if ((event_->xbutton.time-doubleClickEventTime())>doubleClickTime())
   {
     doubleClickEventTime(event_->xbutton.time);
     return MSFalse;
   }
  return MSTrue;
}

MSBoolean MSGraph::longPress(const XEvent *event_)
{
  static unsigned long 	time;
  static int 		x,y;
  const int 		MotionThreshold=5;
  
  if (event_->xbutton.type==ButtonPress)
   {
     time=event_->xbutton.time;
     x   =event_->xbutton.x;
     y   =event_->xbutton.y;
     return MSFalse;
   }
  if (event_->xbutton.type==ButtonRelease)
   {
     if ((event_->xbutton.time-time)>=longPressTime()&&
	 (event_->xbutton.time-time)<2000&&
	 abs(event_->xbutton.x-x)<=MotionThreshold&&
	 abs(event_->xbutton.y-y)<=MotionThreshold)
     return MSTrue;
   }
  return MSFalse;
}  

void MSGraph::motionNotify(const XEvent *event_)
{
  if (graphUIMode()==AddTrace)
   {
     if ((_onLinePoint=findLineHandle(event_->xbutton.x,event_->xbutton.y))==MSTrue)
      {
	XDefineCursor(display(),window(),zoomCursor());
	_onLineSegment=MSFalse;
      }
     else if ((_onLineSegment=findLineSegment(event_->xbutton.x,event_->xbutton.y))==MSTrue)
      {
	XDefineCursor(display(),window(),lineCursor());
	_onLinePoint=MSFalse;
      }
     else XDefineCursor(display(),window(),drawCursor());
   }
  else if (selectTrace()!=0&&graphUIMode()==MoveTrace)
   {
     if ((_onLinePoint=findSelectTraceLineHandle(event_->xbutton.x,event_->xbutton.y))==MSTrue)
      {
	XDefineCursor(display(),window(),zoomCursor());
      }
     else if (findSelectableTrace(event_)==selectTrace())
      {
	XDefineCursor(display(),window(),lineCursor());
	_onLinePoint=MSFalse;
      }
     else XDefineCursor(display(),window(),drawCursor());
   }
}

void MSGraph::buttonPress(const XEvent *event_)
{
  if (isProtected()==MSFalse)
   {
     Window sw=event_->xbutton.subwindow;
     XEvent *ev=(XEvent *) event_;
     if (sw==editor()->window())
      { 
        if (traverseFocus(this)==MSTrue)
	{
	   XEvent *ep=(XEvent *)event_;
	   ep->xbutton.x-=editor()->x_origin();
	   ep->xbutton.y-=editor()->y_origin();
	   buttonPressNotify(editor(),ep);
	}
      }
     else if (sw==legend()->window()) 
      {
	ev->xbutton.x -=legend()->x_origin();
	ev->xbutton.y -=legend()->y_origin();	
	legend()->buttonPress(ev);
      }
     else if (traverseFocus(this)==MSTrue)
      {
	if (event_->xbutton.button==Button1) button1Press(event_);
	else if (event_->xbutton.button==Button2) button2Press(event_);
	else if (event_->xbutton.button==Button3) button3Press(event_);
      }
   }
}

void MSGraph::graphMode(GraphMode mode_)
{ graphMode((unsigned long)mode_);}
void MSGraph::graphMode(unsigned long mode_)
{
  if (graphMode()!=mode_&&mode_>=Standard)
   {
     _graphMode=mode_;
     if (mode_&Normalize) computeNormalizedOffsets();
     redrawImmediately();
   }
}

void MSGraph::graphUIMode(GraphUIMode mode_)
{
  switch(mode_)
   {
   case Normal:
     if (graphUIMode()==AddTrace)
      {
	selectInput(MSGraphEventMask);
	freeze();
	for (int i=0; i<newtraceCt(); i++) 
	 {
	   selectLine(i);
	   _interactivePixel.reshape(nt(selectLine())->pointCount(),2);
	   for (int j=0; j<nt(selectLine())->pointCount(); j++)
	    {
	      interactivePixel()(j,0)=nt(selectLine())->points(j)->x;
	      interactivePixel()(j,1)=nt(selectLine())->points(j)->y;
	    }
	   if (nt(i)->pointCount()>1) if (activateCallback(MSWidgetCallback::addtrace)!=MSTrue)
	    {
	      MSFloatMatrix *pFloatMatrix=new MSFloatMatrix(createInteractiveTraceData((unsigned long)MSLeft));
              MSString tag=interactiveTag();
	      MSTraceSet *ts=createTraceSet(*pFloatMatrix,tag,MSSymbol(tag));
	      ts->selectable(MSTrue);
	    }
	 }
	drawLineSegments();
	drawLineHandles();
	graphUIModeSet(mode_);
	newtraceDealloc();
	unfreeze();
      }
     else if (graphUIMode()==AddTextTrace) editorActivate();
     break;

   case AddTrace:
     if (graphUIMode()!=AddTrace)
      {
	graphUIModeSet(AddTrace);
	selectLine(0);
	XDefineCursor(display(),window(),drawCursor());
      }
     break;

   case AddTextTrace:
     if (graphUIMode()!=AddTextTrace)
      {
	graphUIModeSet(AddTextTrace);
	enterTextTrace();
	if (traverseFocus(this)==MSTrue) takeFocusNotify(editor());
      }
     break;

   case MoveTrace:
     break;
   }
} 

void MSGraph::button1Press(const XEvent *event_) 
{
  if (graphUIMode()==Normal&&(event_->xbutton.state&ControlMask))
   {
     graphUIModeSet(AddTrace);
     enterTrace(event_);
   }
  else if (graphUIMode()==AddTrace&&(event_->xbutton.state&ShiftMask))
   {
     if (_onLineSegment==MSTrue)
      {
	selectInput(MSGraphEventMask);
	moveLineSegment(event_,MSTrue);
	selectInput(MSGraphEventMask|PointerMotionMask);
      }
     else if (selectPoint()==0||selectPoint()==nt(selectLine())->pointCount()-1)
      {
	selectInput(MSGraphEventMask);
	addLineSegment(event_);
	drawLineHandles(selectPoint()==0?0:selectPoint()+1);
	selectInput(MSGraphEventMask|PointerMotionMask);
      }
   }
  else if (graphUIMode()==AddTrace)
   { 
     if (newtraceCt()==0)
      {
	graphUIModeSet(AddTrace);
	enterTrace(event_);
      }
     if (doubleClick(event_)==MSTrue) 
      {
	graphUIMode(Normal);
      }
     else 
      {
	selectInput(MSGraphEventMask);
	if (focusLine()!=selectLine())
	 {
	   focusLine(selectLine());
	   drawLineHandles();
	 }
	else
	 {
	   moveLineSegment(event_);
	   moveLineHandle(event_);
	 }
	selectInput(MSGraphEventMask|PointerMotionMask);
      }
   }
  else if (editor()->mapped()==MSTrue) editorEscape();
  else if (doubleClick(event_)==MSTrue)
   {
     if (highlightLegendStatus()==MSTrue) highlightLegendStatus(MSFalse);
     else if (graphMode()&PieChart&&(_selectTrace=findDataPoint(event_))!=0) 
      {
        activateCallback(_selectTrace->traceSet(),MSWidgetCallback::traceptreference);
      }
     else if (selectTrace()!=0&&selectTrace()!=findTextTrace(event_))
      {
	selectInput(MSGraphEventMask);
	unHighlightTrace();
      }
     else if (selectTrace()==0&&(_selectTrace=findSelectableTrace(event_))!=0)
      {
	highlightTrace();
	if (_onLinePoint==MSTrue)
	 {
	   // add move line handle method
	 }
	activateCallback(selectTrace()->traceSet(),MSWidgetCallback::tracereference);
	moveTrace(event_);
	selectInput(MSGraphEventMask|PointerMotionMask);
      }
     else if (selectTrace()==0&&(_selectTrace=findTextTrace(event_))!=0)
      {
	highlightTrace();
	moveTextTrace(event_);
      }
     else unzoom();
   }
  else if (selectTrace()!=0&&highlightLegendStatus()!=MSTrue)
   {
     if (selectTrace()==findSelectableTrace(event_)) moveTrace(event_);
     else if (selectTrace()==findTextTrace(event_)) moveTextTrace(event_);
   }
  else if (highlightLegendStatus()!=MSTrue)
   { 
     if (drawZoomRegion(event_)!=MSTrue) 
      { 
	interactivePixel().reshape(1,2);
	interactivePixel()(0,0)=event_->xbutton.x;
	interactivePixel()(0,1)=event_->xbutton.y;
	longPress(event_);
      }
   }
}

void MSGraph::buttonRelease(const XEvent *event_) 
{
  MSTrace *tp;
  if (longPress(event_)==MSTrue&&(tp=findDataPoint(event_))!=0) 
   {
     activateCallback(tp->traceSet(),MSWidgetCallback::traceptreference);
   }
  else activateCallback(MSWidgetCallback::graphreference);
}

void MSGraph::button2Press(const XEvent *event_) 
{
  if (highlightLegendStatus()==MSTrue) return;
  interactivePixel().reshape(1,2);
  interactivePixel()(0,0)=event_->xbutton.x;
  interactivePixel()(0,1)=event_->xbutton.y;
  if (editor()->mapped()==MSTrue) editorEscape();
  else if (selectTrace()!=0)
   {
     if (event_->xbutton.state&Mod1Mask)
      {
	if (selectTrace()==findTextTrace(event_))
	 {
	   editTextTrace();
	 }
      }
   }
  else if (event_->xbutton.state&ControlMask)
   {
     cursorType(XYcursor);
     scanXY(event_);
   }
  else if (event_->xbutton.state&Mod1Mask)
   {
     if ((selectTrace()!=0&&selectTrace()==findTextTrace(event_))||
	 (selectTrace()==0&&(_selectTrace=findTextTrace(event_))!=0))
      {
	editTextTrace();
      }
     else
      {
	enterTextTrace();
      }
   }
  else 
   {
     cursorType(Xcursor);
     scanXY(event_);
   }
}
      
void MSGraph::button3Press(const XEvent *event_) 
{
  if (editor()->mapped()==MSTrue) editorEscape();
  else if (selectTrace()==0) moveDataPoint(event_);
}

void MSGraph::keyPress(const XEvent *event_,KeySym keysym_,unsigned int state_,const char *str_)
{
  MSKeyPress keyPress(keysym_,state_);
  if (isProtected()==MSFalse&&keyTranslate(keyPress)==MSFalse)
   {
     if (keysym_==XK_Tab)
      {
	if (state_&(ControlMask|Mod1Mask))
	 {
	   // a kludge for now to call traverseToNext/PreviousShell
	   acceptTab(MSFalse);
	   keyPressNotify(top(),event_,keysym_,state_,str_);
	   acceptTab(MSTrue);
	 }
	else if (state_&ShiftMask) traverseToPrevious();
	else if (state_&Mod1Mask||editor()->mapped()!=MSTrue) traverseToNext();           
      }
     else if (editor()->mapped()==MSTrue) keyTranslateNotify(editor(),event_,keysym_,state_,str_);
     else if (keysym_==XK_Escape)
      { 
	if (graphUIMode()==AddTrace)
	 {
	   selectInput(MSGraphEventMask);
	   graphUIModeSet(Normal);
	   newtraceDealloc();
	   if (graphZoomStatus()==MSTrue) XDefineCursor(display(),window(),zoomCursor());
	   else XUndefineCursor(display(),window());
	   redrawImmediately();
	 }
	else if (selectTrace()!=0)
	 {
	   graphUIModeSet(Normal);
	   unHighlightTrace();
	 }
	else if (marketProfileStatus()==MSTrue) tpoBreakChar(65);
      }
     else if ((keysym_==XK_Delete||keysym_==XK_KP_Decimal)&&
	      (event_->xkey.state&Mod1Mask)==Mod1Mask)
      {
	if (selectTrace()!=0) deleteTrace();
      }
     else if (keysym_==XK_Left||keysym_==XK_Right||
	      keysym_==XK_Up||keysym_==XK_Down )
      {
	if (graphZoomStatus()==MSTrue&&keyPressStatus()==MSFalse) 
	 {
	   keyPressStatus(MSTrue);
	   server()->grabKeyboard(window(),False,GrabModeAsync,GrabModeAsync,event_->xkey.time);
	   _arrowKeysym=keysym_;
	   shiftZoomWindow(keysym_);
	 }
      }
     else if (marketProfileStatus()==MSTrue&&keysym_>65&&keysym_<125) tpoBreakChar((int)keysym_);
   }
}

void MSGraph::keyRelease(const XEvent *e_,KeySym k_,unsigned int i_,const char* c_)
{ MSComposite::keyRelease(e_,k_,i_,c_); } 

void MSGraph::keyRelease(const XEvent *event_)
{
  keyPressStatus(MSFalse);
  server()->ungrabKeyboard(window(),event_->xkey.time);
  stopRepeatTimer();
  MSComposite::keyRelease(event_);
}  

// The sun's X server isn't always reliable with keyReleases.  So we must manually check
// the keymap.
void MSGraph::processRepeatTimer(void)
{
  if (keyPressStatus()==MSTrue)
  {
    unsigned cksum=0;
    char keys_return[32];
    XQueryKeymap(display(), keys_return);
    for (unsigned i = 0; i < 32; i++) cksum+= keys_return[i];
    if (cksum==0) // No key is currently pressed
    {
      keyPressStatus(MSFalse);
      server()->ungrabKeyboard(window(), (CurrentTime));
      stopRepeatTimer();
    }
    else shiftZoomWindow(_arrowKeysym);
  }
}

void MSGraph::startRepeatTimer(void)
{
  if (_repeatOn!=MSTrue)
   {
     _repeatOn=MSTrue;
     _repeatTimer->reset();
   }
}

void MSGraph::stopRepeatTimer(void)
{
  if (_repeatOn==MSTrue)
   {
     _repeatTimer->stop();
     _repeatOn=MSFalse;
   }
}

void MSGraph::shiftZoomWindow(KeySym keysym_)
{
  int 		i,sign;
  double 	offset[2];
  MSBoolean	update=MSFalse;
  
  sign=keysym_==XK_Left||keysym_==XK_Down?-1:1; 
  if (keysym_==XK_Left||keysym_==XK_Right)
   { 
     for (i=0; i<2; i++)
      {
	if (xMax(i)!=0)
	 {
	   offset[i]=xIncData(i)*sign;
	   _xMin[i]+=offset[i];
	   _xMax[i]+=offset[i];
	   update=MSTrue;
	 }
      }
   }
  else if (keysym_==XK_Up||keysym_==XK_Down)
   {
     for (i=0; i<2; i++)
      {
	if (yMax(i)!=0)
	 {
	   offset[i]=yIncData(i)*sign;
	   _yMin[i]+=offset[i];
	   _yMax[i]+=offset[i];
	   update=MSTrue;
	 }
      }
   }
  if (update==MSTrue) redrawImmediately();
  startRepeatTimer();
}

void MSGraph::updateData(void)
{
  if (graphUIMode()==Normal)
   {
     redrawImmediately();
     legend()->redraw();
   }
}

void MSGraph::update(MSTraceSet *ts_,const MSIndexVector& i_)
{ 
  int 		x,y,xs,ys;
  int 		high=1,low=2;
  double        xvalue,yvalue;
  MSBoolean	update=MSFalse,append=MSFalse;
  MSTrace      *trace;
  int           row=i_.length()>0?i_.lastElement()/ts_->numColumns():0;
  
  if (mapped()==MSTrue&&graphUIMode()!=AddTrace&&(ts_->lastDataCount()>0&&row>=ts_->lastDataCount()-1))
   {
     for (int i=0;i<ts_->traceList().count();i++)
      {
	if ((trace=ts_->trace(i))!=0&&trace->dataCount()>2&&trace->traceSet
            ()->overlap()==MSFalse)
	 {
	   ys=trace->yAxis();
	   xs=trace->xAxis();
	   if (trace->style()>=Close)
	    {
	      if (trace->y(high+trace->offset(),row)<=yMaxData(ys)&&
		  trace->y(high+trace->offset(),row)>=yMinData(ys)&&
		  trace->y(low+trace->offset(),row)<=yMaxData(ys)&&
		  trace->y(low+trace->offset(),row)>=yMinData(ys))
	       {
		 update=MSTrue;
		 append=row>=ts_->lastDataCount()?MSTrue:MSFalse;
	       }
	      else
	       {
		 update=append=MSFalse; break;
	       }
	    }
	   else if (trace->style()!=Fill)
	    {
	      if ((yvalue=trace->y(row))<=yMaxData(ys)&&yvalue>=yMinData(ys))
	       {
		 xvalue=xValue(trace,row);
		 if(xvalue<=xMaxData(xs)&&xvalue>=xMinData(xs))
		  {
		    if (trace->style()==Scatter)
		     {
		       x=xValueToPixel(xvalue,trace->xAxis());
		       y=yValueToPixel(yvalue,trace->yAxis());
		       if (x>x_end()-trace->symbolSize()||y>y_end()-trace->symbolSize())
			{
			  update=append=MSFalse; break;
			}
		     }
		    update=MSTrue;
		    append=row>=ts_->lastDataCount()?MSTrue:MSFalse;
		  }
		 else
		  {
		    update=append=MSFalse; break;
		  }
	       }
	      else
	       {
		 update=append=MSFalse; break;
	       }
	    }
	 }
      }
   }
  if (update==MSTrue)
   { 
     computeUpdateRegion(trace);
     if (updateRect()->x()>x_end()||updateRect()->width()==0||updateRect()->height()==0) update=MSFalse;
   }
  if (i_.length()==0||ts_->lastDataCount()==0||legendStyle()==LastValue) legend()->redraw();
  if (legendStyle()==LastValue) updateLegendStatus(MSTrue);
  if (ts_->lastDataCount()<2) update=MSFalse;
  computeNormalizedOffsets();
  redrawImmediately(update,append); 
  ts_->lastDataCount(ts_->dataCount());
}
/*
 *  
 * 	Beginning of MSGraph body functions 
 *  
 */
void MSGraph::computeScales(void)
{
  int 		i,x,y,xx,yy;
  int 		min=0,max=0;
  int	        xMinLabelWidth=0;
  int	        xMaxLabelWidth=0;
  int		spacing[2]    ={0,0};
  int		xTitleHt[2]   ={0,0};
  int		xLabelHt[2]   ={0,0};
  int 		xRuleHt[2]    ={0,0};
  int		yTitleHt[2]   ={0,0};
  int		yLabelHt[2]   ={0,0};
  int		yTitleWidth[2]={0,0};
  int 		rule_width=axisRuleWidth()==0?1:axisRuleWidth();
  XFontStruct  *fi;
  
  for (i=0; i<2; i++)
   {
     _yRuleWidth[i]=0;
     _yLabelWidth[i]=0;
     xLabelHt[i]=computeXscales(min,max,i);
     xMinLabelWidth=min>xMinLabelWidth?min:xMinLabelWidth;
     xMaxLabelWidth=max>xMaxLabelWidth?max:xMaxLabelWidth;
     if (showXaxis(i))
      {
	fi=(XFontStruct *)server()->fontStruct(xTitleFont(i));
	spacing[i]=i==0?(fi->ascent/2):((fi->descent+fi->ascent)/2+2);
	xTitleHt[i]=xTitle(i).maxLength()>0?xTitle(i).length()*(fi->ascent+fi->descent):0;
	xRuleHt[i]=rule_width+(xTickStyle(i)==Inside?xLabelHt[i]:
		      (xMinorTickSize(i)>xMajorTickSize(i)+xLabelHt[i]?
		       xMinorTickSize(i):xMajorTickSize(i)+xLabelHt[i]));
      }
     else if (showXrule(i)==MSTrue||axisRule()==MSG::Box) xRuleHt[i]=rule_width;
     if (showYaxis(i))
      {
	fi=(XFontStruct *)server()->fontStruct(yLabelFont(i));
	int h=fi->ascent+fi->descent;
	yLabelHt[i]=MSBottom&yLabelAlign(i)?h:MSTop&yLabelAlign(i)?fi->descent:h/2;
	fi=(XFontStruct *)server()->fontStruct(yTitleFont(i));
	yTitleHt[i]=yTitleAlign(i)&Vertical||yTitle(i).maxLength()==0?0:yTitle(i).length()*(fi->ascent+2*fi->descent);
      }
     else spacing[i]=0;
   }
  y=xTitleHt[1]+xRuleHt[1];
  int temp=yLabelHt[0]>yLabelHt[1]?yLabelHt[0]:yLabelHt[1];
  y=y<temp?temp:y;
  int th=titleHeight()+subtitleHeight();
  y+=(titleHeight()+subtitleHeight())>0?titleHeight()+subtitleHeight():axisRuleWidth();
  y+=offset()+(yTitleHt[0]>yTitleHt[1]?yTitleHt[0]:yTitleHt[1]);
  y+=y==offset()?rule_width:0;
  y=y>topMargin()?y:topMargin();
  int space=10;
  if (legendAlignment()&Outside&&legendAlignment()&Vertical)
   {
     y+=legendAlignment()&MSTop?legendHeight()+space:!(legendAlignment()&MSBottom)?legendHeight()+space:0;
   }
  _plotAreaRect.y(y);
  yy=_footnoteHeight+xTitleHt[0]+xRuleHt[0];
  yy=offset()+(yy<temp?temp:yy);
  yy+=yy==offset()?rule_width:0;
  yy=plotAreaRect()->y()+(yy>bottomMargin()?yy:bottomMargin());
  yy=yy<height()?height()-yy:20;
  yy-=(legendAlignment()&Outside&&legendAlignment()&Vertical&&legendAlignment()&MSBottom)?legendHeight()+space:0;
  _plotAreaRect.height(yy<0?0:yy);

  for (i=0; i<2; i++)
   {  
     fi=(XFontStruct *)server()->fontStruct(yTitleFont(i));
     _yLabelWidth[i]=computeYscale(i);
     yTitleWidth[i]=yTitleAlign(i)&Vertical?(yTitle(i).length()+2)*fi->max_bounds.width:0;
     yTitleWidth[i]+=_yLabelWidth[i];
     if (showYaxis(i)==MSTrue)
      {
	spacing[i]=fi->max_bounds.width/2+1;
	_yRuleWidth[i]=rule_width+spacing[i]+(yTickStyle(i)==Inside||
			  MSLeft&yLabelAlign(i)||MSRight&yLabelAlign(i)?yTitleWidth[i]:
			  (yMinorTickSize(i)>yMajorTickSize(i)+yTitleWidth[i]?
			  yMinorTickSize(i):yMajorTickSize(i)+yTitleWidth[i]));
      }
     else if (showYrule(i)==MSTrue||axisRule()==MSG::Box) _yRuleWidth[i]=rule_width;
   }
  x=offset()+(showYaxis(0)!=MSTrue?xMinLabelWidth:
      (_yRuleWidth[0]>xMinLabelWidth?_yRuleWidth[0]:xMinLabelWidth));
  x=x>leftMargin()?x:leftMargin();
  x+=x==offset()?rule_width:0;
  if (legendAlignment()&Outside&&!(legendAlignment()&Vertical)&&!(legendAlignment()&MSRight))
   {
     x+=legendWidth()+space;
   }
  _plotAreaRect.x(x);
  spacing[1]=(showXaxis(0)==MSTrue||showXaxis(1)==MSTrue)?1:0;
  xx=offset()+(showYaxis(1)!=MSTrue?xMaxLabelWidth+spacing[1]:
      (_yRuleWidth[1]<xMaxLabelWidth?xMaxLabelWidth:_yRuleWidth[1]));
  xx+=xx==offset()?rule_width:0;
  xx=x+(xx>rightMargin()?xx:rightMargin());
  xx=xx<width()?width()-xx:20;
  if (legendAlignment()&Outside&&!(legendAlignment()&Vertical)&&legendAlignment()&MSRight)
   {
     xx-=(legendWidth()+space)>xMaxLabelWidth?legendWidth()+space:xMaxLabelWidth;
   }
  _plotAreaRect.width(xx);
}

void MSGraph::computeUpdateRegion(MSTrace *trace_)
{
  int       i,k;
  MSTrace  *trace;
  
  double x=xValue(trace_,trace_->traceSet()->lastDataCount()-2);
  int xx=xValueToPixel(x,trace_->xAxis());
  _updateRect.x(xx);  
  _updateRect.y(y_end());
  _updateRect.width(x_end()-updateRect()->x()+(axisRuleWidth()==0?1:axisRuleWidth()));
  _updateRect.height(plotAreaRect()->height());
  for (i=0;i<traceList().count();i++)
   {
     if ((trace=graphTrace(i))!=0&&trace->dataCount()>2&&trace->style()!=MSG::Text&&
	 trace->style()!=Fill&&trace->traceSet()->overlap()==MSFalse)
      {
	x=xPixelToValue(xx,trace->xAxis());
	for (k=trace->dataCount()-1; k>0; k--)
	 {
	   double xval=xValue(trace,k);
	   if ((xval>0&&xval<=x)||(xval<0&&xval>=x)) break;
	 }
	trace->traceSet()->updateIndex(k<2?0:k-2);
      }
   }
}
  
void MSGraph::computeNormalizedOffsets(void)
{
  if (graphMode()&Normalize)
   {
     MSTraceSet *traceSet,*minTraceSet=graphTraceSet(0);
     unsigned i,k;
     for (i=0;i<traceSetList().count();i++)
      {
	traceSet=graphTraceSet(i);
	minTraceSet=minTraceSet->xMin()<traceSet->xMin()?minTraceSet:traceSet;
      }
     for (k=0;k<traceSetList().count();k++)
      {
	traceSet=graphTraceSet(k);
	for (i=0;i<minTraceSet->dataCount()&&minTraceSet->x(i)<=traceSet->xMin();i++);
	traceSet->normalizedOffset(i-1);
      }
   }
}

double MSGraph::estimateNormalizedLabelValue(int inc_, double value_)
{
  if (graphMode()&Normalize)
   {
     int i,index=(int)value_;
     if (index<0)
      {
	for (i=0;i<traceSetList().count();i++)
	 {
	   MSTraceSet *traceSet=graphTraceSet(i);
	   if (traceSet->normalizedOffset()==0)
	    {
	      return traceSet->x(0)+index*normalizedLabelInc()/inc_;
	    }
	 }
      }
     else
      {
	MSTraceSet *maxTraceSet=graphTraceSet(0);
	for (i=0;i<traceSetList().count();i++)
	 {
	   MSTraceSet *traceSet=graphTraceSet(i);
	   if (index<traceSet->dataCount()&&traceSet->dataCount()>1) return traceSet->x(index);
	   maxTraceSet=traceSet->dataCount()>maxTraceSet->dataCount()?traceSet:maxTraceSet;
	 }
	return maxTraceSet->x(maxTraceSet->dataCount()-1)+index*normalizedLabelInc()/inc_;
      }
   }
  return value_;
}

void MSGraph::computeExtents(void)
{
  int 		i,xs,ys;
  double 	xminR=0,xmaxR=0;
  double 	xmin=0,xmax=0;
  double 	ymin=0,ymax=0;
  double 	minArea=0,maxArea=0;    
  double 	minStack=0,maxStack=0;
  double 	minTotal=0,maxTotal=0;
  MSBoolean	showX=MSFalse,showY=MSFalse;
  MSBoolean 	stackSet=MSFalse;
  MSBoolean 	firstX[2],firstY[2],first;
  int 		stackCount=0;
  MSTrace      *trace;
  
  for (i=0; i<2; i++)
   {
     _yMinData[i]=_yMaxData[i]=0.0;
     _xMinData[i]=_xMaxData[i]=0.0;
     firstX[i]=firstY[i]=first=MSFalse;
   }
  barCount(0);
  pieCount(0);
  for (i=0; i<traceList().count(); i++)
   {
     minArea=maxArea=0;
     minStack=maxStack=0;
     if ((trace=graphTrace(i))!=0&&trace->dataCount()>0)
      {
        if (orientation()==Horizontal&&trace->style()!=Bar&&trace->style()!=Stack
            &&trace->style()!=MSG::Text) continue;
        if (trace->style()==Pie) trace->barCount(_pieCount++);
	if (trace->style()==Bar) trace->barCount(_barCount++);
	if (trace->style()==Stack)
	 {
	   if (stackSet!=MSTrue)
	    { 
	      stackCount=_barCount++;
	      stackSet=MSTrue;
	    }
	   trace->barCount(stackCount);
	 }
	if ((xs=trace->xAxis())==1) showX=MSTrue;
	if ((ys=trace->yAxis())==1) showY=MSTrue;
        if (orientation()==Horizontal)
         {
           trace->traceSet()->xDelta(trace->dataCount()>1?fabs(yValue(trace,1)-yValue(trace,0)):1);
         }
        else
         {
           trace->traceSet()->xDelta(trace->dataCount()>1?fabs(xValue(trace,1)-xValue(trace,0)):1);
         }

      	if (graphMode()&Normalize||graphZoomStatus()!=MSTrue)
	 {
	   if (first==MSFalse)
	    {
              _yStringWidth=_xStringWidth=0;
	      first=MSTrue;
	      ymin=yValueMin(trace);
	      ymax=yValueMax(trace);
	      xminR=xValueMin(trace);
	      xmaxR=xValueMax(trace);
	    }
	   if (firstY[ys]==MSFalse)
	    {
	      firstY[ys]=MSTrue;
	      _yMinData[ys]=yValueMin(trace);
	      _yMaxData[ys]=yValueMax(trace);
	    }
	   if (firstX[xs]==MSFalse)
	    {
	      firstX[xs]=MSTrue;
	      if (graphMode()&Normalize)
	       {
		 _xMaxData[xs]=_xMinData[xs]=0;
		 _xMinReal[xs]=xValueMin(trace);
		 _xMaxReal[xs]=xValueMax(trace);
	       }
	      else 
	       {
		 _xMaxReal[xs]=_xMinReal[xs]=0;
		 _xMinData[xs]=xValueMin(trace);
		 _xMaxData[xs]=xValueMax(trace);

	       }
	    }
	   if (graphMode()&Normalize)
	    {
	      xmin=xValueMin(trace);
	      xmax=xValueMax(trace);
              xmaxR=xmax>xmaxR?xmax:xmaxR;
	      xminR=xmin>xminR?xmin:xminR;
	      xmax=trace->dataCount();
	      xmin=trace->traceSet()->normalizedOffset();
	    }
	   else
	    {
	      xmax=xValueMax(trace);
	      xmin=xValueMin(trace);
	      xmaxR=xmax>xmaxR?xmax:xmaxR;
	      xminR=xmin>xminR?xmin:xminR;
	    } 
	   _xMaxData[xs]=xmax>xMaxData(xs)?xmax:xMaxData(xs);
	   _xMinData[xs]=xmin<xMinData(xs)?xmin:xMinData(xs);
	   _xMaxReal[xs]=xmaxR>xMaxReal(xs)?xmaxR:xMaxReal(xs);
	   _xMinReal[xs]=xminR<xMinReal(xs)?xminR:xMinReal(xs);
	   ymax=yValueMax(trace)>ymax?yValueMax(trace):ymax;
	   ymin=yValueMin(trace)<ymin?yValueMin(trace):ymin;
	   if (trace->style()==Area&&orientation()!=Horizontal)
	    {
	      maxArea=yValueMax(trace)>maxArea?yValueMax(trace):maxArea;
	      minArea=yValueMin(trace)<minArea?yValueMin(trace):minArea;
	    }
	   else if (trace->style()==Stack&&orientation()!=Horizontal)
	    {
	      maxStack=yValueMax(trace)>maxStack?yValueMax(trace):maxStack;
	      minStack=yValueMin(trace)<minStack?yValueMin(trace):minStack;
	    }
	   else if (trace->style()==Area&&orientation()==Horizontal)
	    {
	      maxArea=xValueMax(trace)>maxArea?xValueMax(trace):maxArea;
	      minArea=xValueMin(trace)<minArea?xValueMin(trace):minArea;
	    }
	   else if (trace->style()==Stack&&orientation()==Horizontal)
	    {
	      maxStack=xValueMax(trace)>maxStack?xValueMax(trace):maxStack;
	      minStack=xValueMin(trace)<minStack?xValueMin(trace):minStack;
	    }
	   else if (trace->style()==MSG::Text&&orientation()==Horizontal)
            {
//	      _xMaxData[ys]=xValueMax(trace)>xMaxData(ys)?xValueMax(trace):xMaxData(ys);
//	      _xMinData[ys]=xValueMin(trace)<xMinData(ys)?xValueMin(trace):xMinData(ys);
            }
           else
	    {
	      _yMaxData[ys]=yValueMax(trace)>yMaxData(ys)?yValueMax(trace):yMaxData(ys);
	      _yMinData[ys]=yValueMin(trace)<yMinData(ys)?yValueMin(trace):yMinData(ys);
	    }  
	   if (trace->style()==Bar||trace->style()==Stack)
	    {
	      if (trace->barCount()==0&&(xValue(trace,0)!=-DBL_MAX||xValue(trace,trace->dataCount()-1)!=DBL_MAX))
	       {
                 if (orientation() == Horizontal)
                  {
                    _yMinData[ys]-=yValue(trace,0)==_yMinData[ys]?trace->traceSet()->xDelta()/2:0;
                    _yMaxData[ys]+=yValue(trace,trace->dataCount()-1)==_yMaxData[ys]?
                      trace->traceSet()->xDelta()/2:0;
                    _xMinData[xs]-=xMinData(xs)>=0?(xMaxData(xs)-xMinData(xs))*0.2:0;
                  }
                 else
                  {
                    _xMinData[xs]-=xValue(trace,0)==_xMinData[xs]?trace->traceSet()->xDelta()/2:0;
                    _xMaxData[xs]+=xValue(trace,trace->dataCount()-1)==_xMaxData[xs]?
                      trace->traceSet()->xDelta()/2:0;
                    _yMinData[ys]-=yMinData(ys)>=0?(yMaxData(ys)-yMinData(ys))*0.2:0;
                  }                 
               }
              
	      if (yMaxData(ys)<0) _yMaxData[ys]=0;
	    }
	   minTotal=minArea<0?minTotal+minArea:minArea<minTotal?minArea:minTotal;
	   minTotal=minStack<0?minTotal+minStack:minStack<minTotal?minStack:minTotal;
	   if (trace->style()==Area||trace->style()==Stack)
	    {
	      maxTotal+=maxArea>maxStack?maxArea:maxStack;
	    }
	   if (minArea!=maxArea||maxStack!=minStack)
	    {
              if(orientation()==Horizontal)
               {
                 _xMinData[xs]=xMinData(xs)<minTotal?xMinData(xs):minTotal;
                 _xMaxData[xs]=xMaxData(xs)>maxTotal?xMaxData(xs):maxTotal;
               }
              else
               {
                 _yMinData[ys]=yMinData(ys)<minTotal?yMinData(ys):minTotal;
                 _yMaxData[ys]=yMaxData(ys)>maxTotal?yMaxData(ys):maxTotal;
               }
            }
           
	   yShowPos(yMinData(ys)<0?MSTrue:MSFalse);
	   MSString formatBuffer;
	   formatAxisLabel(formatBuffer.removeAll(),yLabelOut(ys),ymin,MSTrue);
	   _yStringWidth=(formatBuffer.length()>_yStringWidth)?formatBuffer.length():_yStringWidth;
	   formatAxisLabel(formatBuffer.removeAll(),yLabelOut(ys),ymax,MSTrue);
	   _yStringWidth=(formatBuffer.length()>_yStringWidth)?formatBuffer.length():_yStringWidth;
	   formatAxisLabel(formatBuffer.removeAll(),xLabelOut(xs),xminR,MSTrue);
	   _xStringWidth=(formatBuffer.length()>_xStringWidth)?formatBuffer.length():_xStringWidth;
	   formatAxisLabel(formatBuffer.removeAll(),xLabelOut(xs),xmaxR,MSTrue);
	   _xStringWidth=(formatBuffer.length()>_xStringWidth)?formatBuffer.length():_xStringWidth;
	   if (_xSubLabelOut[xs]!=0)
	    {
	      formatAxisLabel(formatBuffer.removeAll(),xSubLabelOut(xs),xminR,MSTrue);
	      _xStringWidth=(formatBuffer.length()>_xStringWidth)?formatBuffer.length():_xStringWidth;
	      formatAxisLabel(formatBuffer.removeAll(),xSubLabelOut(xs),xmaxR,MSTrue);
	      _xStringWidth=(formatBuffer.length()>_xStringWidth)?formatBuffer.length():_xStringWidth;
            }
	 }  
      }
   }
  if (_xMinData[1]==_xMaxData[1]&&_xMinData[0]!=_xMaxData[0])
   {
     _xMinData[1]=_xMinData[0];
      _xMaxData[1]=_xMaxData[0];
   }
  else if (_xMinData[0]==_xMaxData[0]&&_xMinData[1]!=_xMaxData[1])
   {
     _xMinData[0]=_xMinData[1];
     _xMaxData[0]=_xMaxData[1];
   }
  if (_yMinData[1]==_yMaxData[1]&&(_yMinData[0]!=_yMaxData[0]||_yMinData[0]!=0.))
   {
     _yMinData[1]=_yMinData[0];
     _yMaxData[1]=_yMaxData[0];
   }
  else if (_yMinData[0]==_yMaxData[0]&&_yMinData[1]!=_yMaxData[1])
   {
     _yMinData[0]=_yMinData[1];
     _yMaxData[0]=_yMaxData[1];
   }
  for (i=0;i<2;i++)
   {
     if (yMinData(i)==yMaxData(i))
      {
	_yMinData[i]=yMinData(i==0?1:0);
	_yMaxData[i]=yMaxData(i==0?1:0);
	_yIncData[i]=yIncData(i==0?1:0);
      }
     if ((unsigned int)yMin(i)==unsetValue())
      {
	_yMinData[i]=yMinData(i)-fabs(yMaxData(i)-yMinData(i))*yMargin(i,0);
      } 
     else _yMinData[i]=yMin(i);
     if ((unsigned int)yMax(i)==unsetValue())
      {
	_yMaxData[i]=yMaxData(i)+fabs(yMaxData(i)-yMinData(i))*yMargin(i,1);
      } 
     else _yMaxData[i]=yMax(i);
     if (yMinData(i)>yMaxData(i))
      {
	if ((unsigned long)yMax(i)!=unsetValue())
	 {
	   _yMinData[i]=yMaxData(i)-100;
	   MSMessageLog::warningMessage("MSGraph::yMaximum set to less than MSGraph::yMinimum\n");
	 }
	else
	 {
	   _yMaxData[i]=yMinData(i)+100;
	   MSMessageLog::warningMessage("MSGraph::yMinimum set to greater than MSGraph::yMaximum\n");
	 }
      }
     if (yMinData(i)==yMaxData(i)||fabs(yMaxData(i)/(yMaxData(i)-yMinData(i)))>MSGraph::_PrecisionRange)
      {
	_yMinData[i]=0.;
	_yMaxData[i]*=2.;
	if (yMaxData(i)==0.) _yMaxData[i]=100.;
      }
     if (xMinData(i)==xMaxData(i))
      {
	_xMinData[i]=xMinData(i==0?1:0);
	_xMaxData[i]=xMaxData(i==0?1:0);
	_xIncData[i][0]=xIncData(i==0?1:0);
      }
     if ((unsigned int)xMin(i)==unsetValue())
      {
	_xMinData[i]=xMinData(i)-fabs(xMaxData(i)-xMinData(i))*xMargin(i,0);
	_xMinReal[i]=xMinReal(i)-fabs(xMaxReal(i)-xMinReal(i))*xMargin(i,0);
      } 
     else _xMinData[i]=xMin(i);
     if ((unsigned int)xMax(i)==unsetValue())
      {
	_xMaxData[i]=xMaxData(i)+fabs(xMaxData(i)-xMinData(i))*xMargin(i,1);
	_xMaxReal[i]=xMaxReal(i)+fabs(xMaxReal(i)-xMinReal(i))*xMargin(i,1);
      }
     else _xMaxData[i]=xMax(i);
     if (xMinData(i)>xMaxData(i))
      {
	if ((unsigned long)xMax(i)!=unsetValue())
	 {
	   _xMinData[i]=xMaxData(i)-100;
	   MSMessageLog::warningMessage("MSGraph::xMaximum set to less than MSGraph::xMinimum\n");
	 }
	else
	 {
	   _xMaxData[i]=xMinData(i)+100;
	   MSMessageLog::warningMessage("MSGraph::xMinimum set to greater than MSGraph::xMaximum\n");
	 }
      }
     if (xMinData(i)==xMaxData(i)||fabs(xMaxData(i)/(xMaxData(i)-xMinData(i)))>MSGraph::_PrecisionRange)
      {
	_xMinData[i]=0.;
	_xMaxData[i]*=2.;
	if (xMaxData(i)==0.)_xMaxData[i]=100.;
      }
     if (xMinReal(i)==xMaxReal(i)||fabs(xMaxReal(i)/(xMaxReal(i)-xMinReal(i)))>MSGraph::_PrecisionRange)
      {
	_xMinReal[i]=0.;
	_xMaxReal[i]*=2.;
	if (xMaxReal(i)==0.)_xMaxReal[i]=100.;
      }
     xShowPos(xShowPos()!=MSTrue&&xMinData(i)<0?MSTrue:MSFalse);
     if (axis()==Std)
      {
	_showXaxis[1]=showX;
	_showYaxis[1]=showY;
	if (axisRule()==Axis)
	 {
	   _showXrule[1]=showX;
	   _showYrule[1]=showY;
	 }
      }
   }
}

int MSGraph::computeXscales(int& min,int& max,int xs_)
{
  int 		submin,submax;
  int 		labelHeight=0;

  if (showXaxis(xs_)==MSTrue)
   {
//     XFontStruct *fi=(XFontStruct *)server()->fontStruct(xTitleFont(xs_));
     MSString formatBuffer;
     XFontStruct *axisInfo=(XFontStruct *)server()->fontStruct(xLabelFont(xs_));
     if (xLabelOut(xs_).formatType()!=MSFormat::NoFormat)
      {
	formatAxisLabel(formatBuffer.removeAll(),xLabelOut(xs_),xMinData(xs_)>1?rint(xMaxData(xs_)):xMinData(xs_));
	min=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	formatAxisLabel(formatBuffer.removeAll(),xLabelOut(xs_),xMaxData(xs_)>1?rint(xMaxData(xs_)):xMaxData(xs_));
	max=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	labelHeight=axisInfo->ascent+axisInfo->descent;
	min=MSLeft&xLabelAlign(xs_)?0:MSRight&xLabelAlign(xs_)?min:min/2;
	max=MSRight&xLabelAlign(xs_)?0:MSLeft&xLabelAlign(xs_)?max:max/2;
      }
     if (_xSubLabelOut[xs_]!=0&&xSubLabelOut(xs_).formatType()!=MSFormat::NoFormat)
      {
	formatAxisLabel(formatBuffer.removeAll(),xSubLabelOut(xs_),xMinData(xs_)>1?(double)((int)xMinData(xs_)):xMinData(xs_));
	submin=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	formatAxisLabel(formatBuffer.removeAll(),xSubLabelOut(xs_),xMaxData(xs_)>1?(double)((int)xMaxData(xs_)):xMaxData(xs_));
	submax=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	submin=MSLeft&xSubLabelAlign(xs_)?0:MSRight&xSubLabelAlign(xs_)?min:min/2;
	submax=MSRight&xSubLabelAlign(xs_)?0:MSLeft&xSubLabelAlign(xs_)?max:max/2;
	min=min>submin?min:submin;
	max=max>submax?max:submax;
	labelHeight+=axisInfo->ascent+axisInfo->descent;
      }
   }
  return labelHeight;
}

int MSGraph::computeYscale(int axis_)
{
  int 		 w;
  int 		 tickPositionLength,labelWidth=0;
  double	 tempInc;
  double	 r,s;
  XFontStruct   *axisInfo;
  MSString       formatBuffer;
  
  _yBase[axis_]=yMode(axis_)==Ascending?yMinData(axis_):yMaxData(axis_);
  _yScale[axis_]=plotAreaRect()->height()/(yMaxData(axis_)-yMinData(axis_));
  _yScale[axis_]*=yMode(axis_)==Ascending?1:-1;
  _y_org=plotAreaRect()->y()+plotAreaRect()->height();
  if (yMode(0)==Ascending)
   {
     _y_end=(int)((double)y_org()-(yMaxData(0)-yBase(0))*yScale(0));
   }     
  else _y_end=plotAreaRect()->y();
  if (showYaxis(axis_)==MSTrue)
   {
     axisInfo=(XFontStruct *)server()->fontStruct(yLabelFont(axis_));
     if ((tickPositionLength=yLabelOut(axis_).tickPositionLength())!=0)
      {
	int labelLength=yLabelOut(axis_).labelLength();
	for (int i=0;i<tickPositionLength;i++)
	 {
	   if (yLabelOut(axis_).tickPosition(i)<yMinData(axis_)||
	       yLabelOut(axis_).tickPosition(i)>yMaxData(axis_)) continue;
	   if (labelLength>0)
	    {
	      if (i<labelLength) yLabelOut(axis_).label(formatBuffer.removeAll(),i);
	    }
	   else formatAxisLabel(formatBuffer.removeAll(),yLabelOut(axis_),yLabelOut(axis_).tickPosition(i));
	   w=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	   labelWidth=w>labelWidth?w:labelWidth;
	 }
      }
     else if (yMinData(axis_)!=yMaxData(axis_))
      {
	if (yLabelIncrement(axis_)<=0.0)
	 {
	   r=((double)plotAreaRect()->height())/(axisInfo->ascent*3);
	   r=(yMaxData(axis_)-yMinData(axis_))/r;
	   tempInc=outFmt().snapNumber(r,yLabelOut(axis_));
	   if (fabs(yMinData(axis_)/tempInc)>MSGraph::_PrecisionRange)
	    {
	      _yIncData[axis_]=100/MSGraph::_PrecisionRange;
	    }
	   else _yIncData[axis_]=tempInc;
	 }
	else
	 {
	   _yIncData[axis_]=yLabelIncrement(axis_);
	 }
	r=yMinData(axis_)-fmod(yMinData(axis_),yIncData(axis_));
	if (drawEndTicks()==MSTrue)
	 {
	   while (r<yMinData(axis_)) r+=yIncData(axis_);
	 }
	else
	 {
	   while (r<=yMinData(axis_)) r+=yIncData(axis_);
	 }
	while (r<=yMaxData(axis_))
	 {
	   s=r;
	   if (fabs(s)/yIncData(axis_)<1.0)s=0;
	   formatAxisLabel(formatBuffer.removeAll(),yLabelOut(axis_),s);
	   w=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	   if (w>labelWidth)labelWidth=w;
	   if (r+yIncData(axis_)==r) break;
	   r+=yIncData(axis_);
	 }
      }
   }
  return yLabelOut(axis_).formatType()==MSFormat::NoFormat?0:labelWidth;
}

void MSGraph::computeXincrement(int level_)
{
  int 		w,w1,w2,x;
  int 		size;
  double 	r,scale,range;
  XFontStruct  *fi;
  double 	min_inc,inc,tempInc;
  MSString      formatBuffer;
  for (int i=0;i<2;i++)
   {
     fi=(XFontStruct *)server()->fontStruct(xLabelFont(i));
     MSBoolean evaluate=MSTrue;
     if (xLabelIncrement(0)<=0.0)
      {
	range=xMaxData(i)-xMinData(i);
	range=range<DBL_MIN?DBL_MIN:range>DBL_MAX?DBL_MAX:range;
	min_inc=outFmt().minimumNumber(xLabelOut(i));
	if (min_inc==0.) min_inc=range/plotAreaRect()->width();
	scale=1/min_inc;
	formatAxisLabel(formatBuffer.removeAll(),level_==0?xLabelOut(i):xSubLabelOut(i),graphMode()&Normalize?xMinReal(i):xMinData(i));
	w1=XTextWidth(fi,formatBuffer,formatBuffer.length());
	formatAxisLabel(formatBuffer.removeAll(),level_==0?xLabelOut(i):xSubLabelOut(i),graphMode()&Normalize?xMaxReal(i):xMaxData(i));
	w2=XTextWidth(fi,formatBuffer,formatBuffer.length());
	w=(w1=w2>w1?w2:(int)(w1*1.5))!=0?w1:100;
	inc=range*w/plotAreaRect()->width();
	tempInc=outFmt().snapNumber(inc,level_==0?xLabelOut(i):xSubLabelOut(i));
	if (fabs(xMinData(i)/tempInc)>MSGraph::_PrecisionRange) inc=100/MSGraph::_PrecisionRange;
	else inc=tempInc;
	while(evaluate==MSTrue)
	 {
	   size=0;
	   r=xMinData(i)-fmod (xMinData(i),inc);
	   if (drawEndTicks()==MSTrue) while (r<xMinData(i)) r+=inc;
	   else while (r<=xMinData(i)) r+=inc;
	   double xp=0;
	   while (r<=xMaxData(i))
	    {
	      double xv=estimateNormalizedLabelValue((int)inc,r);
	      formatAxisLabel(formatBuffer.removeAll(),level_==0?xLabelOut(i):xSubLabelOut(i),xv);
	      size=(w=XTextWidth(fi,formatBuffer,formatBuffer.length()))>size?w:size;
	      normalizedLabelInc(xv-xp);
	      xp=xv;
	      if (fabs(xMaxData(i)/inc)>MSGraph::_PrecisionRange) break;
	      r+=inc;
	    }
	   if (scale*inc<1.5*size) // too big?
	    {
	      min_inc=inc;
	      inc=outFmt().snapNumber((inc*1.001),level_==0?xLabelOut(i):xSubLabelOut(i));
	    }
	   else if (scale*inc>3.*size) // too small?
	    {
	      r=outFmt().snipNumber((inc*.999),xLabelOut(i));
	      if (r>min_inc&&r<(1/MSGraph::_PrecisionRange)) inc=r;
	      else evaluate=MSFalse;
	    }
	   else evaluate=MSFalse;
	 }
	_xIncData[i][level_]=inc;
      }
     else _xIncData[i][level_]=xLabelIncrement(i);
     if (level_==0)
      {
	_xBase[i]=xMinData(i);
	_xScale[i]=plotAreaRect()->width()/(xMaxData(i)-xMinData(i));
	_xScale[i]=(xScale(i)>INT_MAX/2)?INT_MAX/2:xScale(i);
	x=(int)((xMaxData(0)-xBase(0))*xScale(0)+plotAreaRect()->x());
	_x_end=x<(plotAreaRect()->x()+plotAreaRect()->width()-1)?plotAreaRect()->x()+plotAreaRect()->width():x;
	// for normalized graphMode
	_xBaseReal[i]=xMinReal(i);
	_xScaleReal[i]=plotAreaRect()->width()/(xMaxReal(i)-xMinReal(i));
	_xScaleReal[i]=(xScaleReal(i)>INT_MAX/2)?INT_MAX/2:xScaleReal(i);
      }
   }
}

void MSGraph::drawRule(void)
{
  if (!(graphMode()&PieChart))
   {
     for (int i=0; i<2; i++)
      {
        if (showXrule(i)==MSTrue||axisRule()==MSG::Box)
         {
           XSetForeground(display(),axisGC(),xAxisForeground(i));
           int y=i==0?y_org():y_end();
           XDrawLine(display(),graphPixmap(),axisGC(),x_org(),y,x_end(),y);
         }
        if (showYrule(i)==MSTrue||axisRule()==MSG::Box)
         {
           XSetForeground(display(),axisGC(),yAxisForeground(i));
           int offset=axisRuleWidth()/2;
           int x=i==0?x_org():x_end();
           XDrawLine(display(),graphPixmap(),axisGC(),x,y_org()+offset,x,y_end()-offset);
         }
      }
   }
}

void MSGraph::drawGrid(void)
{
  int i,lw=gridWidth();
  
  setLineAttributes(gridStyle(),gridWeight(),gridGC(),gridWidth(),CapButt,JoinMiter);
  for (i=0;i<yGridCount();i++)
   {
     if (yGridWidth()!=0&&lw!=yGridWidth(i))
      {
	lw=yGridWidth(i)-(outputMode()==Print?1:0);
	setLineAttributes(gridStyle(),gridWeight(),gridGC(),lw,CapButt,JoinMiter);
      }
     XDrawLine(display(),graphPixmap(),gridGC(),x_org(),yGrid(i),x_end(),yGrid(i));
   }
  for (i=0;i<xGridCount();i++)
   {
     if (xGridWidth()!=0&&lw!=xGridWidth(i))
      {
	lw=xGridWidth(i)-(outputMode()==Print?1:0);
	setLineAttributes(gridStyle(),gridWeight(),gridGC(),lw,CapButt,JoinMiter);
      }
     XDrawLine(display(),graphPixmap(),gridGC(),xGrid(i),y_org(),xGrid(i),y_end());
   }
}

void MSGraph::drawAxes(void)
{
  xGridCount(0); yGridCount(0);
  for (int i=0; i<2; i++)
   {
     if (showXaxis(i)==MSTrue)
      {
        drawXaxes(i);
        drawXsubLabels(i);
      }
     if (showYaxis(i)==MSTrue) drawYaxes(i);
   }
}

void MSGraph::drawYaxes(int axis_)
{
  int           x,xx,y;
  int           tickSize,bufSize,ct=0,sign;
  MSString      formatBuffer;
  int           tickPositionLength;
  
  if ((tickPositionLength=yLabelOut(axis_).tickPositionLength())!=0) bufSize=10+tickPositionLength;
  else bufSize=int(10+(yMaxData(axis_)-yMinData(axis_))/yIncData(axis_)*(1+yNumMinorTicks(axis_)));
  bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:bufSize;
  XFontStruct *axisInfo=(XFontStruct *)server()->fontStruct(yLabelFont(axis_));

  if (yGridCount()==0)
   {
     if (yGrid()!=0) delete [] yGrid();
     _yGrid=new int[bufSize];
   }
  XSetForeground(display(),axisGC(),yAxisForeground(axis_));
  XSetFont(display(),axisGC(),yLabelFont(axis_));
  XSegment *segments=new XSegment[bufSize];
  double factor=yTickStyle(axis_)==MSNone?0:yTickStyle(axis_)==Inside?-1:1;
  sign=axis_==0?1:-1;
  int y_zero=yValueToPixel(0.0,axis_);
  x=axis_==0?x_org():x_end();
  if (tickPositionLength!=0)
   {
     int gridWidthLength=yLabelOut(axis_).gridWidthLength();
     int labelLength=yLabelOut(axis_).labelLength();
     int tickSizeLength=yLabelOut(axis_).tickSizeLength();
     if (gridWidthLength>0)
      {
	if (yGridWidth()!=0) delete [] yGridWidth();
	_yGridWidth=new int[bufSize];
      }
     for (unsigned i=0;i<tickPositionLength;i++)
      {
	if (yLabelOut(axis_).tickPosition(i)<yMinData(axis_)||
	    yLabelOut(axis_).tickPosition(i)>yMaxData(axis_)) continue;
	y=yValueToPixel(yLabelOut(axis_).tickPosition(i),axis_);
	tickSize=tickSizeLength==0?yMajorTickSize(axis_):int(yMajorTickSize(axis_)*yLabelOut(axis_).tickSize(i));
	if (tickSize>0&&ct<bufSize)
	 {
	   segments[ct].x1=x+int(yTickStyle(axis_)&Inside&&yTickStyle(axis_)&Outside?tickSize:0)*sign;
	   segments[ct].y1=segments[ct].y2=y;
	   segments[ct++].x2=x-int(axisRuleWidth()+tickSize*factor)*sign;
	 }
	if (y!=y_zero&&gridWidthLength>0&&yGridCount()<bufSize)
	 {
	   yGridWidth(yGridCount(),yLabelOut(axis_).gridWidth(i));
	   yGrid(_yGridCount++,y);
	 }
	if (labelLength>0)
	 {
	   if (i<labelLength) yLabelOut(axis_).label(formatBuffer.removeAll(),i);
	 }
	else formatAxisLabel(formatBuffer.removeAll(),yLabelOut(axis_),yLabelOut(axis_).tickPosition(i));
	if (formatBuffer.length()>0)
	 {
	   xx=x-(axisRuleWidth()+yTickStyle(axis_)==Inside?2:yMajorTickSize(axis_)+3)*sign;
	   xx=MSLeft&yLabelAlign(axis_)||MSRight&yLabelAlign(axis_)?
	   x+(axis_==1?1:-1)*(axisInfo->descent/4+axisRuleWidth()):xx;
	   xx-=axis_==1?0:XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	   y+=MSBottom&yLabelAlign(axis_)?-axisRuleWidth()-axisInfo->descent/2:
	   MSTop&yLabelAlign(axis_)?axisInfo->ascent:axisInfo->ascent/2;
	   XDrawString(display(),graphPixmap(),axisGC(),axisInfo,xx,y,formatBuffer,formatBuffer.length());
	 }
      }
   }
  else
   {
     double r=yMinData(axis_)-fmod(yMinData(axis_),yIncData(axis_));
     int majorTick=0;
     int minorTick=_yNumMinorTicks[axis_]+1;
     _yIncData[axis_] /=minorTick;
  
     while (r<yMinData(axis_))
      {
	r+=yIncData(axis_);
	majorTick++;
      }
     while (r<=yMaxData(axis_)&&ct<bufSize)
      {
	y=yValueToPixel(r,axis_);
	if (majorTick%minorTick==0)
	 {
	   if ((axis_==0&&MSLeft&grid())||(axis_==1&&MSRight&grid()))
	    {
	      if (y!=y_zero||showYrule(axis_)==MSFalse&&yGridCount()<bufSize) yGrid(_yGridCount++,y);
	    }
	   if (yMajorTickSize(axis_)>0)
	    {
	      segments[ct].x1=x+int(yTickStyle(axis_)&Inside&&yTickStyle(axis_)&Outside?yMajorTickSize(axis_):0)*sign;
	      segments[ct].y1=segments[ct].y2=y;
	      tickSize=(MSBottom&yLabelAlign(axis_)||MSTop&yLabelAlign(axis_))?
	      _yLabelWidth[axis_]:yMajorTickSize(axis_);
	      segments[ct++].x2=x-int(axisRuleWidth()+tickSize*factor)*sign;
	    }
	   if (yLabelOut(axis_).formatType()!=MSFormat::NoFormat)
	    {
	      double s=(fabs(r)/yIncData(axis_)<1.0)?0:r;
	      formatAxisLabel(formatBuffer.removeAll(),yLabelOut(axis_),s);
	      xx=x-(axisRuleWidth()+((yTickStyle(axis_)==Inside?0:yMajorTickSize(axis_))+3))*sign;
	      xx=MSLeft&yLabelAlign(axis_)||MSRight&yLabelAlign(axis_)?
	      x+(axis_==1?1:-1)*(axisInfo->descent/4+axisRuleWidth()):xx;
	      xx-=axis_==1?0:XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	      y+=MSBottom&yLabelAlign(axis_)?-axisRuleWidth()-axisInfo->descent/2:
	      MSTop&yLabelAlign(axis_)?axisInfo->ascent:axisInfo->ascent/2;
	      XDrawString(display(),graphPixmap(),axisGC(),axisInfo,xx,y,formatBuffer,formatBuffer.length());
	    }
	 }
	else
	 {
	   if (yMinorTickSize(axis_)>0)
	    {
	      segments[ct].x1=x+int(yTickStyle(axis_)&Inside&&yTickStyle(axis_)&Outside?yMinorTickSize(axis_):0)*sign;
	      segments[ct].y1=segments[ct].y2=y;
	      segments[ct++].x2=x-int(axisRuleWidth()+yMinorTickSize(axis_)*factor)*sign;
	    }
	 }
	r+=_yIncData[axis_];
	majorTick++;
      }
   }
  XDrawSegments(display(),graphPixmap(),axisGC(),segments,ct);
  delete [] segments;
}  

void MSGraph::drawXaxes(int axis_)
{
  int           w,x,y,yy;
  int           bufSize,ct=0;
  double        r,s,origin_y=(double)y_org();
  MSString      formatBuffer;
  int           tickPositionLength;
  
  if ((tickPositionLength=xLabelOut(axis_).tickPositionLength())!=0) bufSize=10+tickPositionLength;
  else bufSize=(int)(10+(xMaxData(axis_)-xMinData(axis_))/xIncData(axis_)*(1+xNumMinorTicks(axis_)));
  bufSize=bufSize>MSGraph::_MaxBufSize?MSGraph::_MaxBufSize:bufSize;
  XFontStruct *axisInfo=(XFontStruct *)server()->fontStruct(xLabelFont(axis_));

  if (xGridCount()==0) 
   {
     if (xGrid()!=0) delete [] xGrid();
     _xGrid=new int[bufSize];
   }
  XSetForeground(display(),axisGC(),xAxisForeground(axis_));
  XSetFont(display(),axisGC(),xLabelFont(axis_));
  XSegment *segments=new XSegment[bufSize];
  int sign=axis_==0?1:-1;
  int x_zero=xValueToPixel(0.0,axis_);
  double factor=xTickStyle(axis_)==MSNone?0:xTickStyle(axis_)&Inside?-1:1;
  y=axis_==0?y_org():y_end();
  if (tickPositionLength!=0)
   {
     int gridWidthLength=xLabelOut(axis_).gridWidthLength();
     int labelLength=xLabelOut(axis_).labelLength();
     int tickSizeLength=xLabelOut(axis_).tickSizeLength();
     if (gridWidthLength>0)
      {
	if (xGridWidth()!=0) delete [] xGridWidth();
	_xGridWidth=new int[bufSize];
      }
     for (unsigned i=0;i<tickPositionLength;i++)
      {
	if (xLabelOut(axis_).tickPosition(i)<xMinData(axis_)||
	    xLabelOut(axis_).tickPosition(i)>xMaxData(axis_)) continue;
	x=xValueToPixel(xLabelOut(axis_).tickPosition(i),axis_);
	int tickSize=tickSizeLength==0?xMajorTickSize(axis_):int(xMajorTickSize(axis_)*xLabelOut(axis_).tickSize(i));
	
	if (tickSize>0&&ct<bufSize)
	 {
	   segments[ct].x1=segments[ct].x2=x;
	   segments[ct].y1=y-int(xTickStyle(axis_)&Inside&&xTickStyle(axis_)&Outside?tickSize:0)*sign;
	   segments[ct++].y2=y+int(axisRuleWidth()+tickSize*factor)*sign;
	 }
	if (x!=x_zero&&gridWidthLength>0&&xGridCount()<bufSize)
	 {
	   xGridWidth(xGridCount(),xLabelOut(axis_).gridWidth(i));
	   xGrid(_xGridCount++,x);
	 }
	if (labelLength>0)
	 {
	   if (i<labelLength) xLabelOut(axis_).label(formatBuffer.removeAll(),i);
	 }
	else
	 {
	   s=xLabelOut(axis_).tickPosition(i);
	   formatAxisLabel(formatBuffer.removeAll(),xLabelOut(axis_),graphMode()&Normalize?normalizedPixelToValue(x,axis_):s);
	 }
	if (formatBuffer.length()>0)
	 {
	   w=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	   x=MSRight&xLabelAlign(axis_)?x-w:MSLeft&xLabelAlign(axis_)?x:x-w/2;
	   yy=y+(axisRuleWidth()+(sign==1?axisInfo->ascent:axisInfo->descent)+
		 (xTickStyle(axis_)&Inside?0:xMajorTickSize(axis_)))*sign;
	   XDrawString(display(),graphPixmap(),axisGC(),axisInfo,x,yy,formatBuffer,formatBuffer.length());
	 }
      }
   }
  else
   {
     if (xLabelOut(axis_).formatType()==MSFormat::Time&&xMaxData(axis_)>0&&!(graphMode()&Normalize))
      {
	r=outFmt().snapTime(xMinData(axis_),xIncData(axis_));
      }
     else r=xMinData(axis_)-fmod(xMinData(axis_),xIncData(axis_));
     int majorTick=0;
     int minorTick=_xNumMinorTicks[axis_]+1;
     _xIncData[axis_][0]/=minorTick;
     
     while (r<xMinData(axis_))
      {
	r+=xIncData(axis_);
	majorTick++;
      }
     while (r<=xMaxData(axis_)&&ct<bufSize)
      {
	x=xValueToPixel(r,axis_);
	if (majorTick%minorTick==0)
	 {
	   if ((axis_==0&&MSBottom&grid())||(axis_==1&&MSTop&grid()))
	    {
	      if (x!=x_zero||showXrule(axis_)==MSFalse&&xGridCount()<bufSize) xGrid(_xGridCount++,x);
	    }
	   if (xMajorTickSize(axis_)>0)
	    {
	      segments[ct].x1=segments[ct].x2=x;
	      segments[ct].y1=y-(int)(xTickStyle(axis_)&Inside&&xTickStyle(axis_)&Outside?
				      xMajorTickSize(axis_):0)*sign;
	      segments[ct++].y2=y+(int)(axisRuleWidth()+xMajorTickSize(axis_)*factor)*sign;
	    }
	   s=(fabs(r)/xIncData(axis_)<1.0)?0:r;
	   formatAxisLabel(formatBuffer.removeAll(),xLabelOut(axis_),graphMode()&Normalize?normalizedPixelToValue(x,axis_):s);
	   if (xLabelOut(axis_).formatType()!=MSFormat::NoFormat&&formatBuffer.length()!=0)
	    {
	      w=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	      x=MSRight&xLabelAlign(axis_)?x-w:MSLeft&xLabelAlign(axis_)?x:x-w/2;
	      yy=y+(axisRuleWidth()+(sign==1?axisInfo->ascent:axisInfo->descent)+
		    (xTickStyle(axis_)&Inside?0:xMajorTickSize(axis_)))*sign;
	      XDrawString(display(),graphPixmap(),axisGC(),axisInfo,x,yy,formatBuffer,formatBuffer.length());
	    }
	 }
	else
	 {
	   if (xMinorTickSize(axis_)>0)
	    {
	      segments[ct].x1=segments[ct].x2=x;
	      segments[ct].y1=y-(int)(xTickStyle(axis_)&Inside&&xTickStyle(axis_)&Outside?
				      xMinorTickSize(axis_):0)*sign;
	      segments[ct++].y2=y+(int)(axisRuleWidth()+xMinorTickSize(axis_)*factor)*sign;
	    }
	 }
	r+=_xIncData[axis_][0];
	majorTick++;
      }
   }  
  XDrawSegments(display(),graphPixmap(),axisGC(),segments,ct);
  delete [] segments;
}

void MSGraph::drawXsubLabels(int axis_)
{
  int          majorTick,minorTick;
  int          x,y;
  int          w;
  int	       tickPositionLength,axisLabelHeight;
  double       r,s;
  MSString     formatBuffer;
  XFontStruct *axisInfo;

  if (_xSubLabelOut[axis_]&&xSubLabelOut(axis_).formatType()!=MSFormat::NoFormat)
   {
     computeXincrement(1);
     axisInfo=(XFontStruct *)server()->fontStruct(xLabelFont(axis_));
     axisLabelHeight=axisRuleWidth()+(xTickStyle(axis_)==Inside?0:xMajorTickSize(axis_))+
                       axisInfo->ascent+axisInfo->descent;
     y=axis_==0?(y_org()+axisLabelHeight+axisInfo->ascent):
		  (y_end()-axisLabelHeight-axisInfo->descent);
     if ((tickPositionLength=xSubLabelOut(axis_).tickPositionLength())!=0)
      {
	int labelLength=xSubLabelOut(axis_).labelLength();
	for (int i=0;i<tickPositionLength;i++)
	 {
	   if (xSubLabelOut(axis_).tickPosition(i)<xMinData(axis_)||
	       xSubLabelOut(axis_).tickPosition(i)>xMaxData(axis_)) continue;
	   x=xValueToPixel(xSubLabelOut(axis_).tickPosition(i),axis_);
	   if (labelLength>0)
	    {
	      if (i<labelLength) xSubLabelOut(axis_).label(formatBuffer.removeAll(),i);
	    }
	   else
	    {
	      s=xSubLabelOut(axis_).tickPosition(i);
	      formatAxisLabel(formatBuffer.removeAll(),xSubLabelOut(axis_),graphMode()&Normalize?normalizedPixelToValue(x,axis_):s);
	    }
	   if (formatBuffer.length()>0)
	    {
	      w=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	      x=MSRight&xSubLabelAlign(axis_)?x-w:MSLeft&xSubLabelAlign(axis_)?x:x-w/2;
	      XDrawString(display(),graphPixmap(),axisGC(),axisInfo,x,y,formatBuffer,formatBuffer.length());
	    }
	 }
      }
     else
      {
	if ((xSubLabelOut(axis_).formatType()==MSFormat::Time)&&xMaxData(axis_)>0)
	 {
	   r=outFmt().snapTime(xMinData(axis_),xIncData(axis_,1));
	 }
	else r=xMinData(axis_)-fmod(xMinData(axis_),xIncData(axis_,1));
	majorTick=0;
	minorTick=_xNumMinorTicks[axis_]+1;
	_xIncData[axis_][1]/=minorTick;
	
	while (r<xMinData(axis_))
	 {
	   r+=xIncData(axis_,1);
	   majorTick++;
	 }
	while (r<=xMaxData(axis_))
	 {
	   x=xValueToPixel(r,axis_);
	   if (majorTick%minorTick==0)
	    {
	      s=(fabs(r)/xIncData(axis_)<1.0)?0:r;
	      formatAxisLabel(formatBuffer.removeAll(),xSubLabelOut(axis_),graphMode()&Normalize?normalizedPixelToValue(x,axis_):s);
	      w=XTextWidth(axisInfo,formatBuffer,formatBuffer.length());
	      x=MSRight&xSubLabelAlign(axis_)?x-w:MSLeft&xSubLabelAlign(axis_)?x:x-w/2;
	      XDrawString(display(),graphPixmap(),axisGC(),axisInfo,x,y,formatBuffer,formatBuffer.length());
	    }
	   r+=_xIncData[axis_][1];
	   majorTick++;
	 }
      }
   }  
}  

void MSGraph::drawTitle(Window xwin_)
{ 
  titleHeight(0);
  if (mapped()==MSTrue&&title().maxLength()>0) 
   {
     XFontStruct *fi=(XFontStruct *)server()->fontStruct(titleFont());
     int y=offset()+fi->ascent;
     GC gc=XCreateGC(display(),window(),0,0);
     XSetFont(display(),gc,titleFont());
     XSetForeground(display(),gc,titleForeground());
     for (unsigned i=0;i<title().length();i++) 
      {
	const char *cp=title()[i].string();
	int len=title()[i].length();
        int w=XTextWidth(fi,cp,len);
	int x=MSLeft&titleAlignment()?leftMargin():(MSRight&titleAlignment()?
	      width()-rightMargin()-w:(width()-w)/2);
	XDrawString(display(),xwin_,gc,fi,x,y,cp,len);
	y+=fi->ascent+fi->descent;
	_titleHeight+=fi->ascent+fi->descent;
      }
     XFreeGC(display(),gc);
   }
}

void MSGraph::drawSubtitle(Window xwin_)
{
  subtitleHeight(0);
  if (mapped()==MSTrue&&subtitle().maxLength()!=0)
   {
     XFontStruct *fi=(XFontStruct *)server()->fontStruct(subtitleFont());
     int y=offset()+titleHeight()+fi->ascent+fi->descent;
     XSetFont(display(),subtitleGC(),subtitleFont());
     XSetForeground(display(),subtitleGC(),subtitleForeground());
     for (unsigned i=0;i<subtitle().length();i++) 
      {
        const char *cp=subtitle()[i].string();
        int len=subtitle()[i].length();
        int w=XTextWidth(fi,cp,len);
	int x=MSLeft&subtitleAlignment()?leftMargin():(MSRight&subtitleAlignment()?
	      width()-rightMargin()-w:(width()-w)/2);
	XDrawString(display(),xwin_,subtitleGC(),fi,x,y,cp,len);
	y+=fi->ascent+fi->descent;
	_subtitleHeight+=fi->ascent+fi->descent;
      }
   }
}

void MSGraph::drawFootnote(Window xwin_)
{ 
  footnoteHeight(0);
  if (mapped()==MSTrue&&footnote().maxLength()>0)
   {
     XFontStruct *fi=(XFontStruct *)server()->fontStruct(footnoteFont());
     int y=height()-offset()-fi->descent;
     XSetFont(display(),footnoteGC(),footnoteFont());
     XSetForeground(display(),footnoteGC(),footnoteForeground());
     for (int i=footnote().length()-1;i>=0;i--) 
      {
	const char *cp=footnote()[i].string();
        int len=footnote()[i].length();
        int w=XTextWidth(fi,cp,len);
	int x=MSLeft&footnoteAlignment()?leftMargin():(MSRight&footnoteAlignment()?
	      width()-rightMargin()-w:(width()-w)/2);
	XDrawString(display(),xwin_,footnoteGC(),fi,x,y,cp,len);
	y-=fi->ascent+fi->descent;
	_footnoteHeight+=fi->ascent+fi->descent;
      }
   }
}
  
void MSGraph::drawXtitle(void)
{
  for (int i=0;i<2;i++)
   {
     if (showXaxis(i)==MSTrue&&xTitle(i).maxLength()!=0)
      {
	XFontStruct *fi=(XFontStruct *)server()->fontStruct(xLabelFont(i));
        int labelHeight=(showXrule(i)==MSTrue?axisRuleWidth():0)+
		        (xTickStyle(i)==Inside?0:xMajorTickSize(i))+
		        (xLabelOut(i).formatType()==MSFormat::NoFormat?0:fi->ascent+fi->descent)+
		        (_xSubLabelOut[i]==0||xSubLabelOut(i).formatType()==MSFormat::NoFormat?
		        0:fi->ascent+fi->descent);
	fi=(XFontStruct *)server()->fontStruct(xTitleFont(i));
	int y=i==0?y_org()+labelHeight+fi->ascent:y_end()-labelHeight-fi->descent-
	      ((xTitle(i).length()-1)*(fi->ascent+fi->descent));
	for (unsigned j=0;j<xTitle(i).length();j++)
	 {
	   int w=XTextWidth(fi,xTitle(i)(j).string(),xTitle(i)(j).length());
	   int x=MSLeft&xTitleAlign(i)?x_org():MSRight&xTitleAlign(i)?x_end()-w:
	         x_org()+(plotAreaRect()->width()-w)/2;
	   XSetFont(display(),axisTitleGC(),xTitleFont(i)); 
	   XSetForeground(display(),axisTitleGC(),xTitleForeground(i));
	   XDrawString(display(),graphPixmap(),axisTitleGC(),fi,
		       x,y,xTitle(i)(j).string(),xTitle(i)(j).length());
	   y+=fi->ascent+fi->descent;
	 }
      }
   }  
}  

void MSGraph::drawYtitle(void)
{
  int x,y;
  for (int i=0;i<2;i++)
   {
     XFontStruct *fit=(XFontStruct *)server()->fontStruct(yTitleFont(i));
     if (showYaxis(i)==MSTrue&&yTitle(i).maxLength()!=0)
      {
	XSetFont(display(),axisTitleGC(),yTitleFont(i)); 
	XSetForeground(display(),axisTitleGC(),yTitleForeground(i));
	if (i==0) x=x_org()-_yRuleWidth[i];
	else x=x_end()+_yRuleWidth[i];
	if (yTitleAlign(i)&Vertical)
	 {
	   if (i==0) x+=fit->max_bounds.width;
	   else x-=(yTitle(i).length()+2)*fit->max_bounds.width;
	   for (unsigned j=0;j<yTitle(i).length();j++)
	    {
	      int ht=yTitle(i)(j).length()*(fit->ascent+fit->descent);
	      y=yTitleAlign(i)&MSTop?y_end():MSBottom&yTitleAlign(i)?y_org()-ht:
	        y_end()+(plotAreaRect()->height()-ht)/2;
	      drawVerticalString(axisTitleGC(),x,y,yTitle(i)(j).string(),yTitle(i)(j).length(),fit);
	      x+=2*fit->max_bounds.width;
	    }
	 }
	else
	 {
	   int xend=x_end()+_yStringWidth;
	   int xorg=plotAreaRect()->x()-_yStringWidth;
	   XFontStruct *fil=(XFontStruct *)server()->fontStruct(xLabelFont(1));
	   int h=showXaxis(1)!=MSTrue?0:((showXrule(1)==MSTrue?axisRuleWidth():0)+
		 (xTickStyle(1)==Inside?0:xMajorTickSize(1))+
		 (xLabelOut(1).formatType()==MSFormat::NoFormat?0:fil->ascent+fil->descent)+
		 (_xSubLabelOut[1]!=0&&xSubLabelOut(1).formatType()==MSFormat::NoFormat?0:
		 fil->ascent+fil->descent));
	   for (unsigned j=0;j<yTitle(i).length();j++)
	    {
	      int w=XTextWidth(fit,yTitle(i)(j).string(),yTitle(i)(j).length());
	      x=MSLeft&yTitleAlign(i)?i==0?x_org():(width()-offset()-x_end())>w?x_end():width()-w:
	      MSRight&yTitleAlign(i)?i==0?x_org()>w?x_org()-w:offset():x_end()-w:i==0?x_org():x_end()-w;
	      fil=(XFontStruct *)server()->fontStruct(yLabelFont(i));
	      y=y_end()-fit->descent-h-((yTitle(i).length()-j-1)*(fit->ascent+fit->descent));
	      int lh=fil->ascent+fil->descent;
	      y-=MSBottom&yLabelAlign(i)?lh:MSTop&yLabelAlign(i)?fil->descent:lh/2;
	      XDrawString(display(),graphPixmap(),axisTitleGC(),fit,x,y,
			  yTitle(i)(j).string(),yTitle(i)(j).length());
	    }
	 }
      }
   }
}

void MSGraph::drawVerticalString(GC gc_,int x_,int y_,const char* string_,int length_,XFontStruct *fi_)
{
  int fontHeight=fi_->ascent+fi_->descent;
  y_+=fi_->ascent;
  
  for (int i=0; i<length_; i++)
   {
     int p=(fi_->max_bounds.width-XTextWidth(fi_,string_+i,1))/2;
     XDrawString (display(),graphPixmap(),gc_,fi_,x_+p,y_,string_+i,1);
     y_+=fontHeight;
   }
}

void MSGraph::drawZeroAxis(void)
{
  int x=xValueToPixel(0.0,MSTop&zeroAxis()?1:0);
  int y=yValueToPixel(0.0,MSRight&zeroAxis()?1:0);
  setLineAttributes(zeroAxisStyle(),zeroAxisWeight(),zeroGC(),zeroAxisWidth(),CapButt,JoinMiter);
  if ((MSBottom&zeroAxis()||MSTop&zeroAxis())&&y>y_end()&&y<(y_org()-10)&&y>(y_end()+10))
   {
     XDrawLine(display(),graphPixmap(),zeroGC(),x_org(),y,x_end(),y);
   }
  if ((MSLeft&zeroAxis()||MSRight&zeroAxis())&&x<x_end()&&x>(x_org()+10)&&x<(x_end()-10))
   {
     XDrawLine(display(),graphPixmap(),zeroGC(),x,y_end(),x,y_org());
   }
}

void MSGraph::computeLegendSize(void)
{
  int      size=0,columnCount=1;
  int      rowCount,itemCount=0;
  int      lw,h=0,w=0;
  MSTrace *trace;
  
  if (mapped()==MSTrue&&legendAlignment()!=MSNone&&frozen()!=MSTrue)
   {
     if (graphMode()&PieChart||(traceList().count()==1&&graphTrace(0)!=0&&graphTrace(0)->style()==Pie))
      {
        if ((trace=graphTrace(0))!=0&&trace->traceSet()->pieLegendAlignment()!=MSNone)
         {
           legendWidth(w);
           legendHeight(h);
           return;
         }
      }
     legend()->textFieldWidth(0);
     if (legendStyle()==LastValue)
      {
	legend()->valueWidth(XTextWidth(legend()->fontInfo(),"0",1)*_yStringWidth+legend()->spacing());
      }
     for (unsigned i=0;i<traceList().count();i++)
      {
	if ((trace=graphTrace(i))!=0&&trace->dataCount()>0&&trace->style()!=0&&
	    trace->style()!=MSG::Text&&trace->legend()!=0&&strlen(trace->legend())>0)
	 {
           if (Pie&trace->style())
            {
              for (unsigned k=0;k<trace->dataCount();k++,itemCount++)
               {
                 int w=XTextWidth(legend()->fontInfo(),trace->legend(k),strlen(trace->legend(k)));
                 size=size>w?size:w;
               }
            }
	   else if (legend()->valueWidth()>0)
	    {
              itemCount++;
	      if (HLOC&trace->style()||Candle&trace->style())
	       {
		 size=XTextWidth(legend()->fontInfo(),"CLOSE:00",8);
		 itemCount+=4;
	       }
	      else if (HLC&trace->style())
	       {
		 size=XTextWidth(legend()->fontInfo(),"CLOSE:00",8);
		 itemCount+=3;
	       }
	      else if (HL&trace->style())
	       {
		 size=XTextWidth(legend()->fontInfo(),"HIGH:00",8);
		 itemCount+=2;
	       }
	      else size=XTextWidth(legend()->fontInfo(),trace->legend(),strlen(trace->legend()));
	    }
           else
            {
              itemCount++;
              size=XTextWidth(legend()->fontInfo(),trace->legend(),strlen(trace->legend()));
            }
	   if (size>legend()->textFieldWidth())legend()->textFieldWidth(size);
	 }
      }
     if (itemCount>0)
      {
	rowCount=itemCount;
	lw=legend()->textFieldWidth()+legend()->symbolWidth()+legend()->spacing();
	w=lw+2*legend()->insideMargin()+(legend()->valueWidth()>0?legend()->spacing()+legend()->valueWidth():0);
	h=itemCount*(legend()->textHeight()+legend()->leading())+2*legend()->insideMargin();
     
	if (legendStyle()==Horizontal)
	 {
	   if (itemCount<4)
	    {
	      columnCount=itemCount;
	      rowCount=1;
	    }
	   else if (itemCount==4)
	    {
	      columnCount=2;
	      rowCount=2;
	    }
	   else if (itemCount>4)
	    {
	      columnCount=3;
	      if ((itemCount%3)==0)rowCount=itemCount/3;
	      else rowCount=itemCount/3+1;
	    }
	   h=rowCount*(legend()->textHeight()+legend()->leading())+2*legend()->insideMargin();
	   w=columnCount*lw+(columnCount-1)*legend()->spacing()+2*legend()->insideMargin();
	 }
      }
     legend()->columnCount(columnCount);
   }
  legendWidth(w);
  legendHeight(h);
}

void MSGraph::drawLegends(void)
{
  int          itemCount=0;
  int 	       lastRow;
  MSTrace     *trace;
  const static char *hlText[]={"OPEN:","HIGH:","LOW:","CLOSE:"};
  unsigned long bg=legend()->background();
  
  if ((mapped()==MSTrue&&frozen()!=MSTrue)||outputMode()==Print)
   {
     if (legendAlignment()==MSNone||legendHeight()==0)
      {
	legend()->unmap();
      }
     else
      {
	legend()->resize(legendWidth(),legendHeight());
	legend()->map();
	positionLegend(legendAlignment());
	legend()->highlightColor(foreground());
	legend()->selectInput();
	if (highlightLegendStatus()==MSTrue) legend()->background(legend()->foreground());
	legend()->clear();
	int x=legend()->insideMargin();
	int y=legend()->insideMargin()+2;
	for (unsigned i=0;i<traceList().count();i++)
	 {
	   if ((trace=graphTrace(i))!=0&&trace->style()==Pie)
            {
              for (unsigned k=0;k<trace->dataCount();k++)
               {
                 XClearArea(display(),legend()->window(),x,y+legend()->textHeight()/2,
                            legend()->symbolWidth(),legend()->textHeight(),False);
                 drawLegendSymbols(trace,x,y+legend()->textHeight()/2,k);
                 XSetForeground(display(),legend()->gc(),highlightLegendStatus()==MSTrue?bg:legend()->foreground());
                 XDrawString(display(),legend()->window(),legend()->gc(),legend()->fontInfo(),
                             x+legend()->symbolWidth()+legend()->spacing(),
                             y+legend()->fontInfo()->ascent,
                             trace->legend(k),strlen(trace->legend(k)));
                 int lw=legend()->textFieldWidth()+legend()->symbolWidth()+legend()->spacing();
                 if (legendStyle()==Horizontal)
                  {
                    if ((itemCount%legend()->columnCount())==0)
                     {
                       y+=legend()->textHeight()+legend()->leading();
                       x=legend()->insideMargin();
                     }
                    else x+=lw+legend()->spacing();
                  }
                 else y+=legend()->textHeight()+legend()->leading();
               }
            }
	   else if ((trace=graphTrace(i))!=0&&trace->style()!=0&&
	       trace->dataCount()>0&&trace->style()!=MSG::Text&&trace->legend()!=0&&strlen(trace->legend())>0)
	    {
	      ++itemCount;
	      XClearArea(display(),legend()->window(),x,y+legend()->textHeight()/2,
			 legend()->symbolWidth(),legend()->textHeight(),False);
	      drawLegendSymbols(trace,x,y+legend()->textHeight()/2);
	      XSetForeground(display(),legend()->gc(),highlightLegendStatus()==MSTrue?bg:legend()->foreground());
	      XDrawString(display(),legend()->window(),legend()->gc(),legend()->fontInfo(),
			  x+legend()->symbolWidth()+legend()->spacing(),
			  y+legend()->fontInfo()->ascent,
			  trace->legend(),strlen(trace->legend()));
	      if (legend()->valueWidth()>0)
	       { 
		 int dataOffset=trace->offset();
		 if (HLOC&trace->style()||Candle&trace->style())
		  {
		    lastRow=4;
		    dataOffset=0;
		  }
		 else if (HLC&trace->style()) lastRow=4;
		 else if (HL&trace->style()) lastRow=3;
		 else lastRow=0;
		 for (unsigned j=dataOffset;j<lastRow;j++)
		  {
		    y+=legend()->textHeight()+legend()->leading();
		    XDrawString(display(),legend()->window(),legend()->gc(),legend()->fontInfo(),
				x+legend()->symbolWidth()+2*legend()->spacing(),
				y+legend()->fontInfo()->ascent,
				hlText[j],strlen(hlText[j])
			      );
		  }
	       }
	      int lw=legend()->textFieldWidth()+legend()->symbolWidth()+legend()->spacing();
	      if (legendStyle()==Horizontal)
	       {
		 if ((itemCount%legend()->columnCount())==0)
		  {
		    y+=legend()->textHeight()+legend()->leading();
		    x=legend()->insideMargin();
		  }
		 else x+=lw+legend()->spacing();
	       }
	      else y+=legend()->textHeight()+legend()->leading();
	    }
	 }
	if (legendStyle()==LastValue) drawScanXvalues();
	legend()->drawHighlight();
	legend()->drawShadow();
	legend()->selectInput(ExposureMask);
	legend()->background(bg);
	updateLegendStatus(MSFalse);
      }
   }
}

void MSGraph::drawLegendSymbols(MSTrace *trace_,int x_,int y_,int index_)
{
  int   	offset;
  XGCValues 	values;
  unsigned long mask=GCLineWidth|GCForeground;

  switch (trace_->style())
   {
   case MSG::Text:
   case MarketProfile:
   case ColorProfile:
     break;
     
   case Close:
   case Segment:
   case Line:
   case MSG::Outline:
   case Step:
   case HL:
   case Osc:
   case Line|Scatter:
   case Step|Scatter:
     setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),legend()->gc(),
		       trace_->lineWidth(),CapButt,JoinRound);
     values.line_width=(trace_->lineWidth()>=4)?4:trace_->lineWidth();
     values.foreground=trace_->lineColor();
     XChangeGC(display(),legend()->gc(),mask,&values);
     XDrawLine(display(),legend()->window(),legend()->gc(),x_,y_+1,x_+legend()->symbolWidth(),y_+1);
     if ((Scatter&trace_->style())==0) break;
   case Scatter:
     XSetForeground(display(),legend()->gc(),trace_->lineColor());
     XSetLineAttributes(display(),legend()->gc(),1,LineSolid,CapButt,JoinMiter);
     offset=(trace_->lineWidth()<=1)?1:0;
     XSetForeground(display(),legend()->gc(),trace_->fillColor());
     if (MSG::Text&trace_->symbol())
      {
	legend()->symbolFont(adjustFontSize(trace_->font(),legend()->textHeight()));
	XSetFont(display(),legend()->gc(),legend()->symbolFont());
      }
     drawLegendScatterSymbols(legend()->window(),legend()->gc(),trace_,x_+legend()->symbolWidth()/2,y_+offset);
     XSetFont(display(),legend()->gc(),legend()->font());
     break;
     
   case Candle:
     setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),legend()->gc(),
		       trace_->lineWidth(),CapButt,JoinRound);
     values.line_width=2;
     values.foreground=trace_->lineColor(2);
     XChangeGC (display(),legend()->gc(),mask,&values);
     XDrawLine(display(),legend()->window(),legend()->gc(),x_,y_+1,x_+legend()->symbolWidth(),y_+1);
     setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),legend()->gc(),1,CapButt,JoinRound);
     offset=legend()->symbolWidth()/4;
     PFillRectangle(display(),legend()->window(),legend()->gc(),x_+offset,y_-3,2*offset,7);
     XSetForeground(display(), legend()->gc(),trace_->lineColor(3));
     XDrawRectangle(display(),legend()->window(),legend()->gc(),x_+offset,y_-3,2*offset,7);
     break;

   case HLOC:
   case HLC:
     setLineAttributes(trace_->lineStyle(),trace_->lineWeight(),legend()->gc(),
		       trace_->lineWidth(),CapButt,JoinRound);
     values.line_width=(trace_->lineWidth()>=2)?2:trace_->lineWidth();
     values.foreground=trace_->lineColor(3+trace_->offset());
     XChangeGC (display(),legend()->gc(),mask,&values);
     offset=legend()->symbolWidth()/4;
     XDrawLine(display(),legend()->window(),legend()->gc(),x_+offset,y_,x_+offset,y_+legend()->spacing()/2);
     if (trace_->style()!=HLC)
      {
	offset=legend()->symbolWidth()-legend()->symbolWidth()/4;
	XSetForeground(display(),legend()->gc(),trace_->lineColor(1));
	XDrawLine(display(),legend()->window(),legend()->gc(),
		  x_+offset,y_,x_+offset,y_-legend()->spacing()/2);
      }
     XSetForeground(display(),legend()->gc(),trace_->lineColor(2));
     XDrawLine(display(),legend()->window(),legend()->gc(),x_,y_,x_+legend()->symbolWidth(),y_);
     break;
     
   case Fill:
   case Area:
   case Bar:
   case Pie:
   case Stack:
     XSetLineAttributes(display(),legend()->gc(),1,LineSolid,CapProjecting,JoinMiter);
     unsigned long fillColor=trace_->fillColor();
     unsigned long lineColor=trace_->lineColor();
     if (trace_->style()==Pie)
      {
        fillColor=trace_->fillColor(index_);
        lineColor=trace_->lineColor(index_);
      }
     XSetForeground(display(),legend()->gc(),fillColor);
     if (trace_->stipple()!=0)
      {
	XSetStipple(display(),legend()->gc(),trace_->stipple());
	XSetFillStyle(display(),legend()->gc(),FillOpaqueStippled);
      }
     PFillRectangle(display(),legend()->window(),legend()->gc(),
		    x_,y_-legend()->textHeight()/2+2,
		    legend()->symbolWidth(),legend()->textHeight()-4);
     if (trace_->stipple()!=0) XSetFillStyle(display(),legend()->gc(),FillSolid);
     XSetForeground(display(),legend()->gc(),lineColor);
     XDrawRectangle(display(),legend()->window(),legend()->gc(),
		    x_,y_-legend()->textHeight()/2+2,
		    legend()->symbolWidth(),legend()->textHeight()-4);
     break;
   }
}

void MSGraph::drawLegendScatterSymbols(Window xwin_,GC gc_,MSTrace *trace_,int x_,int y_)
{
  int 		 size,count=0;
  char*		 array=0;
  int 		 bufSize=MSGraph::_MinBufSize;
  XFontStruct	*fontInfo=0;
  
  size=trace_->symbolSize();
  switch (trace_->symbol())
   {
   case Square: 
   case Square|Fill: 
   case Square|Circle: 
     array=new char[sizeof(XRectangle)*bufSize];
     buildSquareSymbol(array,count,x_,y_,size);
     break;

   case Circle:
   case Circle|Fill:
     array=new char[sizeof(XArc)*bufSize];
     buildCircleSymbol(array,count,x_,y_,size);
     break;

   case X:
     array=new char[sizeof(XSegment)*2*bufSize];
     buildXSymbol(array,count,x_,y_,size);
     break;

   case Cross:
     array=new char[sizeof(XSegment)*2*bufSize];
     buildCrossSymbol(array,count,x_,y_,size);
     break;

   case X|Cross:
     array=new char[sizeof(XSegment)*4*bufSize];
     buildStarSymbol(array,count,x_,y_,size);
     break;
   case Triangle:
     if (outputMode()==Print)
      {
	array=new char[sizeof(XSegment)*3*bufSize];
	buildTrianglePrintSymbol(array,count,x_,y_,size);
	break;
      }
     array=new char[sizeof(XPoint)*size*3*bufSize];
     buildTriangleSymbol(array,count,x_,y_,size);
     break;
   case Triangle|Fill:
     array=new char[sizeof(XPoint)*size*3*bufSize];
     buildTriangleSymbol(array,count,x_,y_,size);
     PFillPolygon(display(),xwin_,gc_,(XPoint*)array,count,Convex,CoordModeOrigin);
     break;
   case Diamond:
     if (outputMode()==Print)
      {
	array=new char[sizeof(XSegment)*4*bufSize];
	buildDiamondPrintSymbol(array,count,x_,y_,size);
	break;
      }
     array=new char[sizeof(XPoint)*size*2*bufSize];
     buildDiamondSymbol(array,count,x_,y_,size);
     break;
   case Diamond|Fill:
     array=new char[sizeof(XPoint)*size*2*bufSize];
     buildDiamondSymbol(array,count,x_,y_,size);
     PFillPolygon(display(),xwin_,gc_,(XPoint*)array,count,Convex,CoordModeOrigin);
     break;
   case MSG::Text:
     array=new char[sizeof(XPoint)*bufSize];
     buildCharSymbol(array,count,x_,y_,size);
     fontInfo=(XFontStruct *)server()->fontStruct(legend()->symbolFont());
     break;

   default:
     return;
   }
  drawScatterSymbols(xwin_,gc_,trace_,array,count,trace_->lineColor(),trace_->fillColor(),fontInfo);
  delete [] array;
}

void MSGraph::positionLegend(unsigned long align_)
{
  int x,y,space=10;
  int offsets=legend()->offset()+offset();

  if (legend()->mapped()==MSTrue) 
   {
     if (align_&Outside)
      {
	int top=align_&Vertical?titleHeight()+subtitleHeight()+offset()+space:y_end();
	int bot=align_&Vertical?height()-footnoteHeight()-legend()->height()-space:y_org()-legend()->height();
	x=align_&MSLeft?space:align_&MSRight?width()-legend()->width()-space:align_&MSCenter?
	  align_&Vertical?((width()-legend()->width())/2):space:space;
	y=align_&MSTop?top:align_&MSBottom?bot:align_&MSCenter?align_&Vertical?
	  top:((height()-legend()->height())/2):top;
	if (xLegendPosition()>0&&align_&Vertical)
	 {
	   x=(int)(xLegendPosition()*width());
	   x=x<offsets?offsets:x>(width()-legend()->width()-offsets)?
	   width()-legend()->width()-offsets:x;
	 }
	if (yLegendPosition()>0&&!(align_&Vertical))
	 {
	   y=(int)(yLegendPosition()*height());
	   y=y>height()?height()-legend()->height()-offsets:y<offsets?offsets:y;
	 }
	legend()->moveTo(x,y);
      }
     else if (xLegendPosition()>0&&yLegendPosition()>0)
      {
	x=(int)(xLegendPosition()*width());
	y=(int)(yLegendPosition()*height());
	x=x<offsets?offsets:x>(width()-legend()->width()-offsets)?width()-legend()->width()-offsets:x;
	y=y>(height()-legend()->height()-offsets)?height()-legend()->height()-offsets:y<offsets?offsets:y;
	legend()->moveTo(x,y);
      }
     else
      {
	x=align_&MSLeft?x_org()+space:align_&MSRight?x_end()-legend()->width()-space:
	(x_org()+(plotAreaRect()->width()-legend()->width())/2);
	y=align_&MSTop?y_end()+space:align_&MSBottom?y_org()-legend()->height()-space:
	(y_end()+(plotAreaRect()->height()-legend()->height())/2);
	legend()->moveTo(x,y);
      }
   }
}

int MSGraph::fontSize(Font font_)
{
  const char   *numbers="0123456789";
  const char   *n,*fontStr;
  int 		ret=0;
  
  if ((fontStr=server()->fontName(font_))!=0)
   {
     if ((n=strpbrk((char*)(void*)fontStr,numbers))!=0)	// cast: bug in Borland
      {
	ret=atoi(n);
      }
   }
  return ret;
}

Font MSGraph::adjustFontSize(Font font_,int size_)
{
  const char   *numbers="0123456789";
  char 	       *n,tmp[3];
  const char   *fontStr=server()->fontName(font_);
  
  if (fontStr!=0)
   {
     if ((n=strpbrk((char*)(void*)fontStr,numbers))!=0)	// cast: bug in Borland
      {
	sprintf(tmp,"%u",size_>100?99:size_);
	strcpy(n,tmp);
      }
   }
  return server()->fontID(fontStr);
}
  
void MSGraph::visibilityObscured(void)
{
  visible(MSFalse);
  int n=traceSetList().count();
  for (int i=0;i<n;i++) visibilityObscuredNotify(graphTraceSet(i));
}

void MSGraph::visibilityUnobscured(void)
{
  visible(MSTrue);
  int n=traceSetList().count();
  for (int i=0;i<n;i++) visibilityUnobscuredNotify(graphTraceSet(i));
}

void MSGraph::axis(unsigned long mode_)  
{ 
  MSBoolean l,r,t,b;
  
  _axis=mode_;
  if (mode_==0)
   {
     l=r=t=b=MSFalse;
   }
  else if (mode_==Std)
   {
     b=l=MSTrue;
     t=r=MSFalse;
   }
  else if (mode_==MSG::Box)
   {
     l=r=t=b=MSTrue;
   }
  else
   {
     l=MSLeft&mode_?MSTrue:MSFalse;
     r=MSRight&mode_?MSTrue:MSFalse;
     t=MSTop&mode_?MSTrue:MSFalse;
     b=MSBottom&mode_?MSTrue:MSFalse;
     if (l==MSFalse&&r==MSFalse&&t==MSFalse&&b==MSFalse) return;
   }
  if (axisRule()==Axis)
   {
     _showXrule[0]=b; _showXrule[1]=t;
     _showYrule[0]=l; _showYrule[1]=r;
   }
  if (_showXaxis[0]!=b||_showXaxis[1]!=t||_showYaxis[0]!=l||_showYaxis[1]!=r)
   {
     _showXaxis[0]=b; _showXaxis[1]=t;
     _showYaxis[0]=l; _showYaxis[1]=r;
     redrawImmediately();
   }
}

void MSGraph::axisRule(unsigned long mode_)  
{ 
  MSBoolean l,r,t,b;

  _axisRule=mode_;
  if (mode_==0)
   { 
     l=r=t=b=MSFalse;
   }
  else if (mode_&Axis)
   {
     l=showYaxis(0);
     r=showYaxis(1);
     t=showXaxis(1);	
     b=showXaxis(0);	
   }
  else if (mode_&MSG::Box)
   {
     l=r=t=b=MSTrue;
   }
  else
   {
     l=MSLeft&mode_?MSTrue:MSFalse;
     r=MSRight&mode_?MSTrue:MSFalse;
     t=MSTop&mode_?MSTrue:MSFalse;
     b=MSBottom&mode_?MSTrue:MSFalse;
     if (l==MSFalse&&r==MSFalse&&t==MSFalse&&b==MSFalse) return;
   }
  if (_showXrule[0]!=b||_showXrule[1]!=t||
      _showYrule[0]!=l||_showYrule[1]!=r||axisRule()==Axis)
   {
     _showXrule[0]=b; _showXrule[1]=t;
     _showYrule[0]=l; _showYrule[1]=r;
     redrawImmediately();
   }
}

MSWidgetVector MSGraph::children(void)
{
   MSWidgetVector vector;
   for (int i=0;i<traceSetList().count();i++) vector.append(graphTraceSet(i));
   return vector;
}

MSBoolean MSGraph::backingStore(void)
{
  return DoesBackingStore(screen())!=NotUseful?MSGraphBackingStore!=NotUseful?MSTrue:MSFalse:MSFalse;
}


MSAttrValueList& MSGraph::get(MSAttrValueList& avList_)
{
  MSString              value;
  MSStringVector        optionNames;
  MSUnsignedLongVector  optionValues;
  int i,index;
  optionNames = "MSNone\nMSG::Std\nMSG::Box\nMSTop\nMSBottom\nMSLeft\nMSRight";
  optionValues.removeAll();

  optionValues.append(MSNone);
  optionValues.append(MSG::Std);
  optionValues.append(MSG::Box);
  optionValues.append(MSTop);
  optionValues.append(MSBottom);
  optionValues.append(MSLeft);
  optionValues.append(MSRight);

  value=MSAttrValue::enumToString(axis(),optionNames,optionValues,"MSNone",MSTrue);
  avList_<<MSAttrValue("axis",value,optionNames,MSAttrValue::List);

  
  MSStringVector           axisNames("MSTop\nMSBottom\nMSLeft\nMSRight");
  MSUnsignedLongVector     axisValues;

  axisValues.append(MSTop);
  axisValues.append(MSBottom);
  axisValues.append(MSLeft);
  axisValues.append(MSRight);
  
  MSStringVector xAlignStrings("MSLeft\nMSCenter\nMSRight");
  MSStringVector yAlignStrings("MSBottom\nMSCenter\nMSTop");

  for(i=0;i<4;i++)
   {
     value=MSString(server()->colorName(axisForeground((MSAlignment)axisValues(i))))+","+axisNames(i);
     avList_<<MSAttrValue("axisForeground",value,MSAttrValue::Chooser2|MSAttrValue::Color);
   }
  for(i=0;i<4;i++)
   {
     value= MSAttrValue::alignmentToString(axisLabelAlignment((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("axisLabelAlignment",value,i<2?xAlignStrings:yAlignStrings,MSAttrValue::Chooser2);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(server()->fontName(axisLabelFont((MSAlignment)axisValues(i))))+","+axisNames(i);
     avList_<<MSAttrValue("axisLabelFont",value,MSAttrValue::Chooser2|MSAttrValue::Font);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(axisLabelIncrement((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("axisLabelIncrement",value,MSAttrValue::Chooser2);
   }
  MSStringVector axisValueResults;
  
  if(_xMaxSet[1]==(double)unsetValue()) axisValueResults<<"MSGraph::Unset";
  else axisValueResults<<MSString(_xMaxSet[1]); 
  if(_xMaxSet[0]==(double)unsetValue()) axisValueResults<<"MSGraph::Unset";
  else axisValueResults<<MSString(_xMaxSet[0]);
  if(_yMaxSet[0]==(double)unsetValue()) axisValueResults<<"MSGraph::Unset";
  else axisValueResults<<MSString(_yMaxSet[0]);
  if(_yMaxSet[1]==(double)unsetValue()) axisValueResults<<"MSGraph::Unset";
  else axisValueResults<<MSString(_yMaxSet[1]);
		      
  for(i=0;i<4;i++)
   {
     value=axisValueResults(i)+","+axisNames(i);
     avList_<<MSAttrValue("axisMaximum",value,MSAttrValue::Chooser2);
   }
  axisValueResults.removeAll();
  if(_xMinSet[1]==(double)unsetValue()) axisValueResults<<"MSGraph::Unset";
  else axisValueResults<<MSString(_xMaxSet[1]); 
  if(_xMinSet[0]==(double)unsetValue()) axisValueResults<<"MSGraph::Unset";
  else axisValueResults<<MSString(_xMaxSet[0]);
  if(_yMinSet[0]==(double)unsetValue()) axisValueResults<<"MSGraph::Unset";
  else axisValueResults<<MSString(_yMaxSet[0]);
  if(_yMinSet[1]==(double)unsetValue()) axisValueResults<<"MSGraph::Unset";
  else axisValueResults<<MSString(_yMaxSet[1]);
		      
  for(i=0;i<4;i++)
   {
     value=axisValueResults(i)+","+axisNames(i);
     avList_<<MSAttrValue("axisMinimum",value,MSAttrValue::Chooser2);
   }
  MSStringVector axisModes("MSG::Ascending\nMSG::Descending");
  for(i=0;i<4;i++)
   {
     if(axisMode((MSAlignment)axisValues(i))==Ascending) index=0;
     else index=1;
     value=axisModes(index)+","+axisNames(i);
     avList_<<MSAttrValue("axisMode",value,axisModes,MSAttrValue::Chooser2);
   }
  for(i=0;i<4;i++)
   {
     value= MSAttrValue::alignmentToString(axisSubLabelAlignment((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("axisSubLabelAlignment",value,i<2?xAlignStrings:yAlignStrings,MSAttrValue::Chooser2);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(axisSubLabelIncrement((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("axisSubLabelIncrement",value,MSAttrValue::Chooser2);
   }
  for(i=0;i<4;i++)
   {
     value=MSAttrValue::stringVectorToString(axisTitle((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("axisTitle",value,MSAttrValue::Chooser2);
   }
  MSStringVector sv="MSG::Vertical\nMSG::Horizontal\nMSCenter\nMSTop\nMSBottom\nMSLeft\nMSRight";
  MSUnsignedLongVector lv;

  lv.append(MSG::Vertical);
  lv.append(MSG::Horizontal);
  lv.append(MSCenter);
  lv.append(MSTop);
  lv.append(MSBottom);
  lv.append(MSLeft);
  lv.append(MSRight);

  for(i=0;i<4;i++)
   {
     unsigned long align=axisTitleAlignment((MSAlignment)axisValues(i));
     value=MSAttrValue::enumToString(align,sv,lv,"MSNone");
     value+=","+axisNames(i);
     avList_<<MSAttrValue("axisTitleAlignment",value,i<2?xAlignStrings:sv,MSAttrValue::Chooser2|MSAttrValue::List);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(server()->colorName(axisTitleForeground((MSAlignment)axisValues(i))))+","+axisNames(i);
     avList_<<MSAttrValue("axisTitleForeground",value,MSAttrValue::Chooser2|MSAttrValue::Color);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(server()->fontName(axisTitleFont((MSAlignment)axisValues(i))))+","+axisNames(i);
     avList_<<MSAttrValue("axisTitleFont",value,MSAttrValue::Chooser2|MSAttrValue::Font);
   }
  for(i=2;i<4;i++)
   {
     value=MSString(bottomAxisDataMargin((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("bottomAxisDataMargin",value,MSAttrValue::Chooser2);
   }
  for(i=0;i<2;i++)
   {
     value=MSString(leftAxisDataMargin((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("leftAxisDataMargin",value,MSAttrValue::Chooser2);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(majorTickSize((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("majorTickSize",value,MSAttrValue::Chooser2);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(margin((int)(MSAlignment)axisValues(i)))+","+axisNames(i);


     avList_<<MSAttrValue("margin",value,MSAttrValue::Chooser2);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(minorTicks((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("minorTicks",value,MSAttrValue::Chooser2);
   }
  for(i=0;i<4;i++)
   {
     value=MSString(minorTickSize((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("minorTickSize",value,MSAttrValue::Chooser2);
   }
  for(i=0;i<2;i++)
   {
     value=MSString(rightAxisDataMargin((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("rightAxisDataMargin",value,MSAttrValue::Chooser2);
   }
  MSStringVector tickStyles="MSG::Inside\nMSG::Outside\nMSG::Inside|MSG::Outside";
  for(i=0;i<4;i++)
   {
     unsigned long style=tickStyle((MSAlignment)axisValues(i));
     if(style==Inside) index=0;
     else if(style==Outside) index=1;
     else index=2;
     value=tickStyles(index)+","+axisNames(i);
     avList_<<MSAttrValue("tickStyle",value,tickStyles,MSAttrValue::Chooser2);
   }
  for(i=2;i<4;i++)
   {
     value=MSString(topAxisDataMargin((MSAlignment)axisValues(i)))+","+axisNames(i);
     avList_<<MSAttrValue("topAxisDataMargin",value,MSAttrValue::Chooser2);
   }


  optionNames="MSNone\nMSG::Axis\nMSTop\nMSBottom\nMSLeft\nMSRight";
  optionValues.removeAll();

  optionValues.append(MSNone);
  optionValues.append(MSG::Std);
  optionValues.append(MSG::Box);
  optionValues.append(MSTop);
  optionValues.append(MSBottom);
  optionValues.append(MSLeft);
  optionValues.append(MSRight);

  value=MSAttrValue::enumToString(axisRule(),optionNames,optionValues,"MSNone",MSTrue);
  avList_<<MSAttrValue("axisRule",value,optionNames,MSAttrValue::List);
  avList_<<MSAttrValue("axisRuleWidth", MSString(axisRuleWidth()));
  avList_<<MSAttrValue("footnote",MSAttrValue::stringVectorToString(footnote()),MSAttrValue::String);
  avList_<<MSAttrValue("footnoteAlignment",MSAttrValue::alignmentToString(footnoteAlignment()),
                       MSStringVector("MSCenter\nMSLeft\nMSRight"));
  avList_<<MSAttrValue("footnoteFont",server()->fontName(footnoteFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("footnoteForeground",server()->colorName(footnoteForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("grid",MSAttrValue::alignmentToString(grid()),MSStringVector("MSNone\nMSTop\nMSBottom\nMSLeft\nMSRight"),MSAttrValue::List);
  avList_<<MSAttrValue("gridForeground",server()->colorName(gridForeground()),MSAttrValue::Color);

  optionNames="MSSolid\nMSDash\nMSDot\nMSDash|MSDot";
  optionValues.removeAll();

  optionValues.append(MSSolid);
  optionValues.append(MSDash);
  optionValues.append(MSDot);
  optionValues.append(MSDash|MSDot);
  value=MSAttrValue::enumToString(gridStyle(),optionNames,optionValues,"MSSolid",MSTrue);
 
  avList_<<MSAttrValue("gridStyle",value,optionNames);
  avList_<<MSAttrValue("gridWeight", MSString(gridWeight()));
  avList_<<MSAttrValue("gridWidth", MSString(gridWidth()));

  optionNames="MSG::Vertical\nMSG::Horizontal";
  if (orientation()==Horizontal) value = "MSG::Horizontal";
  else value = "MSG::Vertical";
  avList_<<MSAttrValue("orientation",value,optionNames);

  optionNames="MSNone\nMSG::Outside\nMSG::Vertical\nMSCenter\nMSTop\nMSBottom\nMSLeft\nMSRight";
  optionValues.removeAll();

  optionValues.append(MSNone);
  optionValues.append(MSG::Outside);
  optionValues.append(MSG::Vertical);
  optionValues.append(MSCenter);
  optionValues.append(MSTop);
  optionValues.append(MSBottom);
  optionValues.append(MSLeft);
  optionValues.append(MSRight);

  value=MSAttrValue::enumToString(legendAlignment(),optionNames,optionValues,"MSNone",MSFalse);
  avList_<<MSAttrValue("legendAlignment",value,optionNames,MSAttrValue::List);

  avList_<<MSAttrValue("legendFont",server()->fontName(legendFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("legendForeground",server()->colorName(legendForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("legendBackground",server()->colorName(legendBackground()),MSAttrValue::Color);
  avList_<<MSAttrValue("legendHighlightThickness",MSString(legendHighlightThickness()));
  avList_<<MSAttrValue("legendShadowThickness",MSString(legendShadowThickness()));

  if(legendStyle()==MSG::Vertical) value="MSG::Vertical";
  else if(legendStyle()==MSG::Horizontal) value="MSG::Horizontal";
  else value="MSG::LastValue";
  avList_<<MSAttrValue("legendStyle",value,MSStringVector("MSG::Vertical\nMSG::Horizontal\nMSG::LastValue"));
  avList_<<MSAttrValue("maxBarWidth",MSString(maxBarWidth()));

  avList_<<MSAttrValue("selectDistance", MSString(selectDistance()));
  avList_<<MSAttrValue("subtitle",MSAttrValue::stringVectorToString(subtitle()),MSAttrValue::String);
  avList_<<MSAttrValue("subtitleFont",server()->fontName(subtitleFont()),MSAttrValue::Font);
  avList_<<MSAttrValue("subtitleForeground",server()->colorName(subtitleForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("subtitleAlignment",MSAttrValue::alignmentToString(subtitleAlignment()),
                       MSStringVector("MSCenter\nMSLeft\nMSRight"));

  avList_<<MSAttrValue("zeroAxis",MSAttrValue::alignmentToString(zeroAxis()),
                       MSStringVector("MSNone\nMSTop\nMSBottom\nMSLeft\nMSRight"),MSAttrValue::List);
  avList_<<MSAttrValue("zeroAxisForeground",server()->colorName(zeroAxisForeground()),MSAttrValue::Color);

  optionNames="MSSolid\nMSDash\nMSDot\nMSDash|MSDot";
  optionValues.removeAll();

  optionValues.append(MSSolid);
  optionValues.append(MSDash);
  optionValues.append(MSDot);
  optionValues.append(MSDash|MSDot);

  value=MSAttrValue::enumToString(zeroAxisStyle(),optionNames,optionValues,"MSSolid",MSTrue);
  avList_<<MSAttrValue("zeroAxisStyle",value,optionNames);

  avList_<<MSAttrValue("zeroAxisWidth",MSString(zeroAxisWidth()));
  avList_<<MSAttrValue("zeroAxisWeight",MSString(zeroAxisWeight()));

  avList_<<MSAttrValue("addtrace","",MSAttrValue::Callback);
  avList_<<MSAttrValue("addtracetext","",MSAttrValue::Callback);
  avList_<<MSAttrValue("copytrace","",MSAttrValue::Callback);
  avList_<<MSAttrValue("copytexttrace","",MSAttrValue::Callback);
  avList_<<MSAttrValue("deletetrace","",MSAttrValue::Callback);
  avList_<<MSAttrValue("graphreference","",MSAttrValue::Callback);
  avList_<<MSAttrValue("graphzoom","",MSAttrValue::Callback);
  
  return MSComposite::get(avList_);
}

static void convertAxisAttribute(const MSString& value_, MSString& value1_, MSAlignment& align_)
{
 MSString value2;
 int index=value_.indexOf(',');
 value1_=value_.subString(0,index);
 value2=value_.subString(index+1); 
 align_=(MSAlignment)MSAttrValue::stringToAlignment(value2);
}

void MSGraph::set(MSAttrValueList& avList_)
{
  MSComposite::set(avList_);
  MSIndexVector index;
  MSString value;
  MSAlignment axisChoice;
  for(int i=0;i<avList_.length();i++)
   {
     if("axis"==avList_[i].attribute())
      {
        const MSString& value = avList_[i].value();
        if(value.indexOf("MSG::Std")!=value.length()) axis(MSG::Std);
        else if(value.indexOf("MSG::Box")!=value.length()) axis(MSG::Box);
        else axis(MSAttrValue::stringToAlignment(value));
        index<<i;
      }
     else if("axisForeground"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisForeground(value,axisChoice);
       index<<i;
      }
     else if("axisLabelFont"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisLabelFont(value,axisChoice);
       index<<i;
      }
     else if("axisLabelAlignment"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisLabelAlignment((MSAlignment)MSAttrValue::stringToAlignment(value),axisChoice);
       index<<i;
      }
     else if("axisLabelIncrement"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisLabelIncrement(value.asInt(),axisChoice);
       index<<i;
      }
     else if("axisMaximum"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       if(value.length()==0 || value=="MSGraph::Unset") axisMaximum(Unset,axisChoice);
       else axisMaximum(value.asInt(),axisChoice);
       index<<i;
      }
     else if("axisMinimum"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       if(value.length()==0 || value=="MSGraph::Unset") axisMinimum(Unset,axisChoice);
       else axisMinimum(value.asInt(),axisChoice);
       index<<i;
      }
     else if("axisSubLabelAlignment"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisSubLabelAlignment((MSAlignment)MSAttrValue::stringToAlignment(value),axisChoice);
       index<<i;
      }
     else if("axisSubLabelIncrement"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisSubLabelIncrement(value.asInt(),axisChoice);
       index<<i;
      }
     else if("axisTitle"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisTitle(MSAttrValue::stringToStringVector(value),axisChoice);
       index<<i;
      }
     else if("axisTitleAlignment"==avList_[i].attribute())
      {
       unsigned long align=0;
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       if(value.indexOf("MSG::Horizontal")!=value.length()) align|=MSG::Horizontal;
       if(value.indexOf("MSG::Vertical")!=value.length()) align|=MSG::Vertical;
       align|=MSAttrValue::stringToAlignment(value);
       axisTitleAlignment(align,axisChoice);
       index<<i;
      }
     else if("axisTitleForeground"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisTitleForeground(value,axisChoice);
       index<<i;
      }
     else if("axisTitleFont"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       axisTitleFont(value,axisChoice);
       index<<i;
      }
     else if("bottomAxisDataMargin"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       bottomAxisDataMargin(value.asInt(),axisChoice);
       index<<i;
      }
     else if("leftAxisDataMargin"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       leftAxisDataMargin(value.asInt(),axisChoice);
       index<<i;
      }
     else if("rightAxisDataMargin"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       rightAxisDataMargin(value.asInt(),axisChoice);
       index<<i;
      }
     else if("topAxisDataMargin"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       topAxisDataMargin(value.asInt(),axisChoice);
       index<<i;
      }
     else if("majorTickSize"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       majorTickSize(value.asInt(),axisChoice);
       index<<i;
      }
     else if("minorTickSize"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       minorTickSize(value.asInt(),axisChoice);
       index<<i;
      }
     else if("minorTicks"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       minorTicks(value.asInt(),axisChoice);
       index<<i;
      }
     else if("margin"==avList_[i].attribute())
      {
       convertAxisAttribute(avList_[i].value(),value,axisChoice);
       margin(value.asInt(),axisChoice);
       index<<i;
      }
     else if("maxBarWidth"==avList_[i].attribute())
      {
       maxBarWidth(avList_[i].value().asInt());
       index<<i;
      }
     else if("axisMode"==avList_[i].attribute())
      {
        AxisMode newAxisMode;
	convertAxisAttribute(avList_[i].value(),value,axisChoice);
        if(value=="MSG::Ascending") newAxisMode=Ascending;
	else newAxisMode=Descending;
	axisMode(newAxisMode,axisChoice);
        index<<i;
      }
     else if("tickStyle"==avList_[i].attribute())
      {
        unsigned long newTickStyle;
	convertAxisAttribute(avList_[i].value(),value,axisChoice);
        if(value=="MSG::Inside") newTickStyle=Inside;
	else if(value=="MSG::Outside") newTickStyle=Outside;
	else newTickStyle=Inside|Outside;
	tickStyle(newTickStyle,axisChoice);
        index<<i;
      }

     else if("axisRule"==avList_[i].attribute())
      {
        const MSString& value = avList_[i].value();
        if(value.indexOf("MSG::Axis")!=value.length()) axisRule(MSG::Axis);
        else axisRule(MSAttrValue::stringToAlignment(value));
        index<<i;
      }
     else if("axisRuleWidth"==avList_[i].attribute())
      {
        axisRuleWidth(avList_[i].value().asInt());
        index<<i;
      }
     else if("footnote"==avList_[i].attribute())
      {
        footnote(MSAttrValue::stringToStringVector(avList_[i].value()));
        index<<i;
      }
     else if("footnoteAlignment"==avList_[i].attribute())
      {
        footnoteAlignment((MSAlignment)MSAttrValue::stringToAlignment(avList_[i].value()));
        index<<i;
      }
     else if("footnoteFont"==avList_[i].attribute())
      {
        footnoteFont(avList_[i].value());
        index<<i;
      }
    else if("footnoteForeground"==avList_[i].attribute())
     {
        footnoteForeground(avList_[i].value());
        index<<i;
      }
     else if("gridForeground"==avList_[i].attribute())
      {
        gridForeground(avList_[i].value());
        index<<i;
      }
     else if("grid"==avList_[i].attribute())
      {
        grid(MSAttrValue::stringToAlignment(avList_[i].value()));
        index<<i;
      }
     else if("gridWidth"==avList_[i].attribute())
      {
        gridWidth(avList_[i].value().asInt());
        index<<i;
      }
     else if("gridWeight"==avList_[i].attribute())
      {
        gridWeight(avList_[i].value().asInt());
        index<<i;
      }
     else if(avList_[i].attribute()=="gridStyle")
      {
        unsigned long newLineStyle;
        if(avList_[i].value()=="MSSolid") newLineStyle=MSSolid;
        else if(avList_[i].value()=="MSDash") newLineStyle=MSDash;
        else if(avList_[i].value()=="MSDot") newLineStyle=MSDot;
        else newLineStyle=MSDash|MSDot;
        gridStyle(newLineStyle);
        index<<i;
      }
     else if(avList_[i].attribute()=="orientation")
      {
        if(avList_[i].value()=="MSG::Horizontal") orientation(Horizontal);
        else orientation(Vertical);
      }
     else if("legendFont"==avList_[i].attribute())
      {
        legendFont(avList_[i].value());
        index<<i;
      }
     else if("legendForeground"==avList_[i].attribute())
      {
        legendForeground(avList_[i].value());
        index<<i;
      }
     else if("legendBackground"==avList_[i].attribute())
      {
        legendBackground(avList_[i].value());
        index<<i;
      }
     else if("legendAlignment"==avList_[i].attribute())
      {
        const MSString& value = avList_[i].value();
        unsigned long align=0;
        if(value.indexOf("MSG::Outside")!=value.length()) align|=MSG::Outside;
        if(value.indexOf("MSG::Vertical")!=value.length()) align|=MSG::Vertical;
        align|=MSAttrValue::stringToAlignment(value);
        legendAlignment(align);
        index<<i;
      }
    else if("legendHighlightThickness"==avList_[i].attribute())
      {
        legendHighlightThickness(avList_[i].value().asInt());
        index<<i;
      }
     else if("legendShadowThickness"==avList_[i].attribute())
      {
        legendShadowThickness(avList_[i].value().asInt());
        index<<i;
      }
     else if("legendStyle"==avList_[i].attribute())
      {
        if(avList_[i].value()=="MSG::Horizontal") legendStyle(MSG::Horizontal);
        else if(avList_[i].value()=="MSG::Vertical") legendStyle(MSG::Vertical);
        else legendStyle(MSG::LastValue);
        index<<i;
      }
     else if("selectDistance"==avList_[i].attribute())
      {
        selectDistance(avList_[i].value().asInt());
        index<<i; 
      }
     else if (avList_[i].attribute()=="subtitle")
      {
        subtitle(MSAttrValue::stringToStringVector(avList_[i].value()));
        index<<i;
      }
     else if (avList_[i].attribute()=="subtitleFont")
      {
        subtitleFont(avList_[i].value());
        index<<i;
      }
     else if (avList_[i].attribute()=="subtitleForeground")
      {
        subtitleForeground(avList_[i].value());
        index<<i;
      }
     else if (avList_[i].attribute()=="subtitleAlignment")
      {
        subtitleAlignment((MSAlignment)MSAttrValue::stringToAlignment(avList_[i].value()));
        index<<i;
      }
     else if (avList_[i].attribute()=="zeroAxis")
      {
        zeroAxis(MSAttrValue::stringToAlignment(avList_[i].value()));
        index<<i;
      }
      else if (avList_[i].attribute()=="zeroAxisForeground")
      {
        zeroAxisForeground(avList_[i].value());
        index<<i;
      }
     else if(avList_[i].attribute()=="zeroAxisStyle")
      {
        unsigned long newLineStyle;
        if(avList_[i].value()=="MSSolid") newLineStyle=MSSolid;
        else if(avList_[i].value()=="MSDash") newLineStyle=MSDash;
        else if(avList_[i].value()=="MSDot") newLineStyle=MSDot;
        else newLineStyle=MSDash|MSDot;
        zeroAxisStyle(newLineStyle);
        index<<i;
      }
     else if("zeroAxisWidth"==avList_[i].attribute())
      {
        zeroAxisWidth(avList_[i].value().asInt());
        index<<i;
      }
     else if("zeroAxisWeight"==avList_[i].attribute())
      {
        zeroAxisWeight(avList_[i].value().asInt());
        index<<i;
      }
     
  }
  avList_.remove(index);
}

// #################################################################
// default virtual methods
// #################################################################
void MSGraph::placement(void) {}
void MSGraph::update(const MSIndexVector& x_)
{MSComposite::update(x_);}

void MSGraphStatusWin::buttonPress(const XEvent *) {}

Font MSGraphLegend::font(void)
{return MSGraphStatusWin::font();}

void MSGraphLegend::buttonRelease(const XEvent *) {}
//
// MSPrintItem methods
//
MSGraph& MSGraph::leftMargin(double x_)
{MSPrintItem::leftMargin(x_); return *this;}
MSGraph& MSGraph::rightMargin(double x_)
{MSPrintItem::rightMargin(x_); return *this;}
MSGraph& MSGraph::topOffset(double x_)
{MSPrintItem::topOffset(x_); return *this;}
MSGraph& MSGraph::bottomOffset(double x_)
{MSPrintItem::bottomOffset(x_); return *this;}
MSGraph& MSGraph::topPixel(unsigned x_)
{MSPrintItem::topPixel(x_); return *this;}
MSGraph& MSGraph::bottomPixel(unsigned x_)
{MSPrintItem::bottomPixel(x_); return *this;}
MSGraph& MSGraph::justification(MSAlignment x_)
{MSPrintItem::justification(x_); return *this;}
MSGraph& MSGraph::justification(unsigned long x_)
{MSPrintItem::justification(x_); return *this;}
MSGraph& MSGraph::printRow(int x_)
{MSPrintItem::printRow(x_); return *this;}
MSGraph& MSGraph::printColumn(int x_)
{MSPrintItem::printColumn(x_); return *this;}
MSGraph& MSGraph::style(unsigned long style_)
{
  unsigned long temp=style_&(MSLeft|MSRight|MSTop|MSBottom|MSCenter);
  if (temp>0) justification(temp);
  _style=style_^temp;
  return *this;
}
MSGraph& MSGraph::pageAlignment(unsigned long x_)
{MSPrintItem::pageAlignment(x_); return *this;}
MSGraph& MSGraph::pageAlignment(MSAlignment x_)
{return pageAlignment((unsigned long)x_);}
const MSSymbol& MSGraph::printTag(void) const
{return tag();}
double MSGraph::leftMargin(void) const
{return MSPrintItem::leftMargin();}
double MSGraph::rightMargin(void) const
{return MSPrintItem::rightMargin();}
double MSGraph::topOffset(void) const
{return MSPrintItem::topOffset();}
double MSGraph::bottomOffset(void) const
{return MSPrintItem::bottomOffset();}
unsigned MSGraph::topPixel(void) const
{return MSPrintItem::topPixel();}
unsigned MSGraph::bottomPixel(void) const
{return MSPrintItem::bottomPixel();}
unsigned long MSGraph::justification(void) const
{return MSPrintItem::justification();}
int MSGraph::printRow(void) const
{return MSPrintItem::printRow();}
int MSGraph::printColumn(void) const
{return MSPrintItem::printColumn();}
unsigned long MSGraph::style(void) const
{return _style;}
unsigned long MSGraph::pageAlignment(void) const
{return MSPrintItem::pageAlignment();}

int MSGraph::computePrintSize(MSReport *report_,int,int y_,int w_,int,int topMargins_,int margins_)
{
  reset();
  if (leftPixel()<0) leftPixel(report_->leftPixel());
  if (rightPixel()<0) rightPixel(report_->rightPixel());
  margins_=(margins_==0?leftPixel()+rightPixel():margins_);
  pageWidth(w_-margins_);
  printWidth(printWidth()>0&&printWidth()<pageWidth()?printWidth():pageWidth());
  int h=int(printWidth()*0.67);
  int remainingHeight=y_-report_->pageEnd()-topMargins_-topPixel();
  h=printHeight()>0&&printHeight()<remainingHeight?printHeight():h;
  printHeight(h);
  int top=report_->bodyTop(report_->pageCount());
  if (h>remainingHeight||(pageAlignment()&MSTop&&y_!=top))
   {
    _pageCount++;
     y_=report_->bodyTop(report_->pageCount()+1);
     remainingHeight=y_-report_->bodyBottom(report_->pageCount()+1)-topMargins_-topPixel();
   }
  int resid=0;
  if (h<remainingHeight&&(pageAlignment()&MSCenter||pageAlignment()&MSBottom))
   {
     _pageCount++;
   }
  else resid=h;
  if (resid!=0)
   {
    resid+=topPixel();
    if(resid+bottomPixel()<remainingHeight) resid+=bottomPixel();
    else
     {
      resid=0;
      _pageCount++;
     }
   }
  _residual=resid;
  return residual();
}

int MSGraph::print(MSReport *report_,int x_,int y_,int,int,int bottomIndent_,int margin_)
{
  int pageEnd=report_->pageEnd()+bottomIndent_;
  int remainingHeight=y_-pageEnd;
  if (printHeight()>=remainingHeight||
      (pageBreakRow()==-1&&pageAlignment()&MSTop&&y_!=report_->bodyTop(report_->pageCount())))
   {
     pageBreakRow(0);
     _currentPage++;
     return remainingHeight;
   }
  if (pageAlignment()&MSCenter)
   {
     y_-=(remainingHeight-printHeight())/2;
   }
  else if (pageAlignment()&MSBottom)
   {
     y_=pageEnd+printHeight();
   }
  int x=x_+(margin_==0?leftPixel():margin_);
  x=justification()&MSRight?x+pageWidth()-printWidth():
    justification()&MSCenter?x+(pageWidth()-printWidth())/2:x;
  int wScreen=width();
  int hScreen=height();
  int frameLeft=style()&MSP::Box||style()&MSP::BoxL?frameLineWidth():0;
  int frameRight=style()&MSP::Box||style()&MSP::BoxR?frameLineWidth():0;
  int frameTop=style()&MSP::Box||style()&MSP::BoxT?frameLineWidth():0;
  int frameBottom=style()&MSP::Box||style()&MSP::BoxB?frameLineWidth():0;
  MSRect::width(printWidth()-frameLeft-frameRight);
  MSRect::height(printHeight()-frameTop-frameBottom);
  outputMode(Print);
  int h=printGraph(report_,x,y_-printHeight()-topPixel());
  outputMode(Draw);
  MSRect::width(wScreen);
  MSRect::height(hScreen);
  _currentPage++;
  return residual();
}


int MSGraph::printGraph(MSReport *report_,int x_,int y_)
{
  int h=0;
  displayPrintStream().open(report_->adjustedFileName());
  
  if (displayPrintStream().fail()!=ios::failbit)
   {
     int frameThickness=frameLineWidth();
     int x=x_+(style()&MSP::Box||style()&MSP::BoxL?frameThickness:0);
     int y=y_-(style()&MSP::Box||style()&MSP::BoxT?frameThickness:0);
     displayPrintInit(this);
     printGraphFrame(report_,x_,y_+printHeight(),printWidth(),printHeight());
     report_->postScriptStackInit();
     report_->pout<<"gs 1 sg 1 w n"<< endl;
     report_->pout<<"12/"<<report_->defaultFontString()<<" font"<< endl;
     report_->translate(x,y);
     redrawForPrint();
     displayPrintClear();
     report_->pout<<"gr";
     report_->pout<<endl;

     displayPrintStream().close();
     h=printHeight();
   }
  else MSMessageLog::errorMessage("Error: Unable to attach stream to graph %s\n",tag().symbolName());
  return h;
}

void MSGraph::printGraphFrame(MSReport *report_,int x_,int y_,int w_,int h_) 
{
  report_->gcValues().line_width=frameLineWidth();
  report_->fgGrayScale(0.0);
  report_->printBox(style(),x_,y_,w_,h_);
}

