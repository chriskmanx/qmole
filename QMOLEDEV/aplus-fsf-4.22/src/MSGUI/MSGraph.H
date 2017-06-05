#ifndef MSGraphHEADER
#define MSGraphHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <MSTypes/MSMathDeclare.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSFloatMatrix.H>
#include <MSTypes/MSIntMatrix.H>
#include <MSIPC/MSRegularTimer.H>
#include <MSIPC/MSIntervalTimer.H>
#include <MSGUI/MSTraceSet.H>
#include <MSGUI/MSText.H>
#include <MSGUI/MSLabelOut.H>
#include <MSGUI/MSLabelFormat.H>
#include <MSGUI/MSPrintItem.H>
#include <MSGUI/MSBackingStorePixmap.H>
#ifndef MSPointerArrayHEADER
#include <MSGUI/MSPointerArray.H>
#endif
class MSTrace;
class MSReport;
class MSTraceSet;
class MSGraphStatusWin;
class MSGraphLegend;
class MSGraphEditor;
class MSGraphNewtrace;
class MSGraphPieData;
class MSGraphPieLabelData;

class MSGUIExport MSGraphEditor:public MSText
{
friend class MSGraph;
public:
  MSGraphEditor(MSWidget *);
  ~MSGraphEditor();

  MSGraph *graph(void) const {return (MSGraph *)owner();}
  void positionCursor(int pos_) {moveCursor(0,pos_);}
  virtual void save(void);
  virtual void escape(void);
  void stringVector(const MSStringVector&);

  // need these to make editor believe that it has the focus, but the
  // graph will forward all events
  virtual MSBoolean traverseFocus(MSWidget *);
  virtual MSWidget *inputFocus(void);
  virtual const MSWidget *inputFocus(void) const;
};

class MSGUIExport MSGraphDoubleClickTimer:public MSRegularTimer
{
protected:
  MSGraph *_graph;  
public:
  MSGraphDoubleClickTimer(MSGraph *,unsigned long);
  ~MSGraphDoubleClickTimer(void);

  MSGraph *graph(void) const {return _graph;}
  virtual void process(void);
};

class MSGUIExport MSGraphRepeatTimer : public MSIntervalTimer
{
protected:
  MSGraph *_graph;
public:
  MSGraphRepeatTimer(MSGraph *,unsigned long);
  ~MSGraphRepeatTimer(void);

  MSGraph *graph(void) const {return _graph;}
  virtual void process(void);
};

class MSGUIExport MSGraphNewtrace
{
protected:
  MSGraph	*_graph;
  XPoint  	*_points;
  int		 _pointCount;
  int		 _pointAllocCount;
  
public:
  MSGraphNewtrace(MSGraph *);
  ~MSGraphNewtrace(void);

  MSGraph *graph(void)      const {return _graph;}
  int pointCount(void)      const {return _pointCount;}
  int pointAllocCount(void) const {return _pointAllocCount;}
  void pointCount(int x_)         {_pointCount=x_;}
  void pointAllocCount(int x_)    {_pointAllocCount=x_;}
  XPoint *points(int index_);
  void pointsAlloc(void);
  void enterPoint(int,int);
  void drawLineSegments(int xOffset_=0,int yOffset=0);

  int sa(int i_)           {return points(i_+1)->x-points(i_)->x;}
  int sb(int i_)           {return points(i_+1)->y-points(i_)->y;}
  int left(int i_)         {return sa(i_)>0?points(i_)->x:points(i_+1)->x;}
  int right(int i_)        {return sa(i_)>0?points(i_+1)->x:points(i_)->x;}
  int top(int i_)          {return sb(i_)>0?points(i_)->y:points(i_+1)->y;}
  int bot(int i_)          {return sb(i_)>0?points(i_+1)->y:points(i_)->y;}
  double slope(int i_)     {return sa(i_)!=0?(double)sb(i_)/(double)sa(i_):0.0;}
};

class MSGUIExport MSGraph : public MSComposite, public MSPrintItem, public MSG
{
  friend 	class MSGraphRepeatTimer;
  friend 	class MSGraphDoubleClickTimer;
  friend 	class MSGraphStatusWin;
  friend 	class MSGraphLegend;
  friend 	class MSTrace;
  friend 	class MSTraceSet;
  friend	class MSGraphEditor;
  friend 	class MSGraphNewtrace;

public:
  MSGraph(MSWidget *,const char *title_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSGraph(MSWidget *,const MSStringVector&,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSGraph(void);

  enum UnsetType   {Unset};
  enum GraphMode   {Standard=1,Normalize=2,Optimize=4,PieChart=8};
  enum GraphUIMode {Normal=16,AddTrace=32,AddTextTrace=2048,MoveTrace=4096};

  virtual void unfreeze(void);
  virtual void redraw(void);
  virtual void print(const char *file_=0);
  virtual MSWidgetVector children(void);
  void redrawForPrint(void);
  MSBoolean backingStore(void);
  MSBoolean debug(void);
  static void debug(MSBoolean);
  const MSSymbol& tag(void)       const {return _tag;}
  void tag(const MSSymbol& x_)          {_tag=x_;}
  //
  //  	 MSPrintTool print methods
  //
  virtual int computePrintSize(MSReport*,int,int,int,int,int,int);
  virtual int print(MSReport*,int,int,int,int,int,int);
  virtual const MSSymbol& printTag(void) const;
  int printGraph(MSReport*,int,int);
  void printGraphFrame(MSReport*,int,int,int,int);

  int printWidth(void)     const {return _printWidth;}
  int pageWidth(void)      const {return _pageWidth;}
  int frameLineWidth(void) const {return _frameLineWidth;}
  void printWidth(int x_)        {_printWidth=x_;}
  void pageWidth(int x_)         {_pageWidth=x_;}
  void frameLineWidth(int x_)    {_frameLineWidth=x_;}
  //
  //  	 MSPrintItem methods
  //
  MSGraph& leftMargin(double);
  MSGraph& rightMargin(double);
  MSGraph& topOffset(double);
  MSGraph& bottomOffset(double);
  MSGraph& topPixel(unsigned);
  MSGraph& bottomPixel(unsigned);
  MSGraph& justification(MSAlignment);
  MSGraph& justification(unsigned long);
  MSGraph& printRow(int);
  MSGraph& printColumn(int);
  MSGraph& style(unsigned long);
  MSGraph& pageAlignment(MSAlignment);
  MSGraph& pageAlignment(unsigned long);
  double leftMargin(void) const;
  double rightMargin(void) const;
  double topOffset(void) const;
  double bottomOffset(void) const;
  unsigned topPixel(void) const;
  unsigned bottomPixel(void) const;
  unsigned long justification(void) const;
  int printRow(void) const;
  int printColumn(void) const;
  unsigned long style(void) const;
  unsigned long pageAlignment(void) const;

  //
  //  	 Trace interface methods
  //
  const MSTraceSet *graphTraceSet(const MSSymbol& tag_) const;
  const MSTrace *graphTrace(const MSSymbol& tag_) const;
  MSTraceSet *graphTraceSet(const MSSymbol& tag_);
  MSTrace *graphTrace(const MSSymbol& tag_);
  int traceCount(void)                                        {return traceList().count();}
  int traceSetCount(void)                                     {return traceSetList().count();}

  //
  //  	 Graph size/range attributes
  //
  inline double zoomMinimum(MSAlignment);
  inline double zoomMaximum(MSAlignment);
  inline double axisMinimum(MSAlignment) const;
  inline double axisMaximum(MSAlignment) const;
  inline double axisLabelIncrement(MSAlignment) const;
  inline double axisSubLabelIncrement(MSAlignment) const;

  void axisMinimum(UnsetType,MSAlignment i_)           {axisMinimum((double)unsetValue(),i_);}
  void axisMaximum(UnsetType,MSAlignment i_)           {axisMaximum((double)unsetValue(),i_);}
  void axisMinimum(double x_,MSAlignment i_)           {axisMinimum(x_,(unsigned long)i_);}
  void axisMaximum(double x_,MSAlignment i_)           {axisMaximum(x_,(unsigned long)i_);}
  void axisLabelIncrement(double x_,MSAlignment i_)    {axisLabelIncrement(x_,(unsigned long)i_);}
  void axisSubLabelIncrement(double x_,MSAlignment i_) {axisSubLabelIncrement(x_,(unsigned long)i_);}
  void axisMinimum(double,unsigned long);
  void axisMaximum(double,unsigned long);
  void axisLabelIncrement(double,unsigned long);
  void axisSubLabelIncrement(double,unsigned long);
  //
  //  	 Graph interactive method attributes
  //
  MSFloatMatrix createInteractiveTraceData(unsigned long);
  MSTraceSet *createTraceSet(MSFloatMatrix&,const char* legend_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSTraceSet *createTextTraceSet(MSFloatMatrix&,MSStringVector&);
  MSFloatMatrix& interactiveData()                   {return _interactiveData;}
  const MSFloatMatrix& interactiveData()       const {return _interactiveData;}
  const MSTrace *selectTrace(void)             const {return _selectTrace;}
  MSTrace *selectTrace(void)                         {return _selectTrace;}
  int selectDistance(void)                     const {return _selectDistance;}
  unsigned long graphMode(void)                const {return _graphMode;}
  GraphUIMode graphUIMode(void)                const {return _graphUIMode;}
  void selectDistance(int x_)                        {_selectDistance=x_;}
  void selectTrace(MSTrace *x_)                      {_selectTrace=x_;}
  void graphMode(unsigned long);
  void graphMode(GraphMode);
  void graphUIMode(GraphUIMode);
  void zoom(void);
  void unzoom(void);

  const MSStringVector& interactiveTraceTags() const     {return _interactiveTraceTags;}
  void interactiveTraceTags(const MSStringVector& tags_) {_interactiveTraceTags=tags_; }
  const MSString& defaultTraceTag() const                {return _defaultTraceTag;     }
  void defaultTraceTag(const MSString& tag_)             {_defaultTraceTag=tag_;       }

  void xCursorValue(MSAlignment i_,double x_)
  {if (interactivePixel().length()>0) interactivePixel()(0,0)=xValueToPixel(x_,i_&MSTop?1:0);}
  void yCursorValue(MSAlignment i_,double x_)
  {if (interactivePixel().length()>0) interactivePixel()(0,1)=yValueToPixel(x_,i_&MSRight?1:0);}
  double xCursorValue(MSAlignment i_=MSLeft)
  {return interactivePixel().length()>0?xPixelToValue(interactivePixel()(0,0),i_&MSTop?1:0):0;}
  double yCursorValue(MSAlignment i_=MSBottom)
  {return interactivePixel().length()>0?yPixelToValue(interactivePixel()(0,1),i_&MSRight?1:0):0;}
  //
  //  	 Graph axis/rule attributes
  //
  unsigned long axis(void)                    const {return _axis;}
  unsigned long axisRule(void)                const {return _axisRule;}
  unsigned long grid(void)                    const {return _grid;}
  unsigned long zeroAxis(void)                const {return _zeroAxis;}
  unsigned long gridStyle(void)               const {return _gridStyle;}
  unsigned long zeroAxisStyle(void)           const {return _zeroAxisStyle;}
  int gridWeight(void)                        const {return _gridWeight;}
  int zeroAxisWeight(void)                    const {return _zeroAxisWeight;}
  int gridWidth(void)                         const {return _gridWidth;}
  int zeroAxisWidth(void)                     const {return _zeroAxisWidth;}
  unsigned long gridForeground(void)          const {return _gridForeground;}
  unsigned long zeroAxisForeground(void)      const {return _zeroAxisForeground;}
  double leftAxisDataMargin(MSAlignment x_)   const {return _yMargin[0][MSTop&x_?0:1];}
  double rightAxisDataMargin(MSAlignment x_)  const {return _yMargin[1][MSTop&x_?0:1];}
  double topAxisDataMargin(MSAlignment x_)    const {return _xMargin[1][MSLeft&x_?0:1];}
  double bottomAxisDataMargin(MSAlignment x_) const {return _xMargin[0][MSLeft&x_?0:1];}

  inline AxisMode axisMode(MSAlignment) const;
  void axisMode(AxisMode x_,MSAlignment i_)         {axisMode(x_,(unsigned long)i_);}
  void axisMode(AxisMode,unsigned long);

  double margin(MSAlignment x_) const
  {return _margin[MSLeft&x_?0:MSTop&x_?1:MSRight&x_?2:3];}
  void margin(double,unsigned long);
  void axis(unsigned long);
  void axisRule(unsigned long);

  void grid(unsigned long);
  void gridStyle(unsigned long);
  void gridWidth(unsigned); 
  void gridWeight(unsigned);
  void gridForeground(const char *);
  void gridForeground(unsigned long);
  void zeroAxis(unsigned long);
  void zeroAxisStyle(unsigned long);
  void zeroAxisWidth(unsigned);
  void zeroAxisWeight(unsigned);
  void zeroAxisForeground(const char *);
  void zeroAxisForeground(unsigned long);
  void topAxisDataMargin(double,unsigned long); 
  void bottomAxisDataMargin(double,unsigned long);
  void leftAxisDataMargin(double,unsigned long);
  void rightAxisDataMargin(double,unsigned long);
  void topAxisDataMargin(double x_,MSAlignment i_)    {topAxisDataMargin(x_,(unsigned long)i_);}
  void bottomAxisDataMargin(double x_,MSAlignment i_) {bottomAxisDataMargin(x_,(unsigned long)i_);}
  void leftAxisDataMargin(double x_,MSAlignment i_)   {leftAxisDataMargin(x_,(unsigned long)i_);}
  void rightAxisDataMargin(double x_,MSAlignment i_)  {rightAxisDataMargin(x_,(unsigned long)i_);}
  //
  //  	 Graph legend attributes
  //
  unsigned long legendAlignment(void)   const {return _legendAlign;}
  unsigned long legendStyle(void)       const {return _legendStyle;}
  double xLegendPosition(void)          const {return _xLegendPosition;}
  double yLegendPosition(void)          const {return _yLegendPosition;}
  void legendAlignment(MSAlignment);
  void legendAlignment(unsigned long);
  void legendStyle(unsigned long);
  void xLegendPosition(double); 
  void yLegendPosition(double);
  void legendFont(const char *); 
  void legendFont(Font); 
  void legendForeground(const char *); 
  void legendForeground(unsigned long); 
  void legendBackground(const char *); 
  void legendBackground(unsigned long); 
  void legendHighlightThickness(int); 
  void legendShadowThickness(int); 
  Font legendFont(void) const;
  unsigned long legendForeground(void) const;
  unsigned long legendBackground(void) const;
  int legendShadowThickness(void) const;
  int legendHighlightThickness(void) const;
  //
  //	 subtitle methods
  //
  const MSStringVector& subtitle(void)  const {return _subtitle;}
  unsigned long subtitleForeground(void)const {return _subtitleForeground;}
  Font subtitleFont(void)               const {return _subtitleFont;}
  MSAlignment subtitleAlignment(void)   const {return _subtitleAlign;}
  void subtitle(const MSStringVector&);
  void subtitleAlignment(MSAlignment);
  void subtitleForeground(const char *);
  void subtitleForeground(unsigned long);
  void subtitleFont(const char *);
  void subtitleFont(Font);
  //
  //	 footnote methods
  //
  const MSStringVector& footnote(void)   const {return _footnote;}
  unsigned long footnoteForeground(void) const {return _footnoteForeground;}
  Font footnoteFont(void)                const {return _footnoteFont;}
  MSAlignment footnoteAlignment(void)    const {return _footnoteAlign;}
  void footnote(const MSStringVector&);
  void footnoteAlignment(MSAlignment);
  void footnoteForeground(const char *);
  void footnoteForeground(unsigned long);
  void footnoteFont(const char *);
  void footnoteFont(Font);
  //
  //	 axis title methods
  //
  inline const MSStringVector& axisTitle(MSAlignment) const;
  inline unsigned long axisTitleForeground(MSAlignment) const;
  inline Font axisTitleFont(MSAlignment) const;
  inline MSAlignment axisTitleAlignment(MSAlignment) const;
  inline MSAlignment axisLabelAlignment(MSAlignment) const;
  inline MSAlignment axisSubLabelAlignment(MSAlignment) const;
  inline MSLabelOutPtr axisLabelOut(MSAlignment) const;
  inline MSLabelOutPtr axisSubLabelOut(MSAlignment) const;
  void axisTitle(const MSStringVector& x_,MSAlignment i_)   {axisTitle(x_,(unsigned long)i_);}
  void axisTitleForeground(const char *x_,MSAlignment i_)   {axisTitleForeground(x_,(unsigned long)i_);}
  void axisTitleForeground(unsigned long x_,MSAlignment i_) {axisTitleForeground(x_,(unsigned long)i_);}
  void axisTitleFont(const char *x_,MSAlignment i_)         {axisTitleFont(x_,(unsigned long)i_);}
  void axisTitleFont(Font x_,MSAlignment i_)                {axisTitleFont(x_,(unsigned long)i_);}
  void axisTitleAlignment(MSAlignment x_,MSAlignment i_)    {axisTitleAlignment(x_,(unsigned long)i_);}
  void axisTitleAlignment(unsigned long x_,MSAlignment i_)  {axisTitleAlignment(x_,(unsigned long)i_);}
  void axisLabelAlignment(MSAlignment x_,MSAlignment i_)    {axisLabelAlignment(x_,(unsigned long)i_);}
  void axisSubLabelAlignment(MSAlignment x_,MSAlignment i_) {axisSubLabelAlignment(x_,(unsigned long)i_);}
  void axisTitle(const MSStringVector&,unsigned long); 
  void axisTitleForeground(const char *,unsigned long); 
  void axisTitleForeground(unsigned long,unsigned long); 
  void axisTitleFont(const char *,unsigned long); 
  void axisTitleFont(Font,unsigned long); 
  void axisTitleAlignment(MSAlignment,unsigned long);
  void axisTitleAlignment(unsigned long,unsigned long); 
  void axisLabelAlignment(MSAlignment,unsigned long); 
  void axisSubLabelAlignment(MSAlignment,unsigned long); 

  inline void axisLabelOut(MSLabelOut *x_,MSAlignment i_);      
  inline void axisSubLabelOut(MSLabelOut *x_,MSAlignment i_);   
  inline void axisLabelOut(MSLabelOut *,unsigned long);
  inline void axisSubLabelOut(MSLabelOut *,unsigned long);
  void axisLabelOut(MSLabelOutPtr,unsigned long);
  void axisSubLabelOut(MSLabelOutPtr,unsigned long);
  //
  //	 axis attributes  
  //
  inline unsigned long axisForeground(MSAlignment) const;
  inline Font axisLabelFont(MSAlignment) const;
  void axisRuleWidth(int);
  int axisRuleWidth(void)                        const {return _axisRuleWidth;}
  void axisForeground(const char *x_,MSAlignment i_)   {axisForeground(x_,(unsigned long)i_);}
  void axisForeground(unsigned long x_,MSAlignment i_) {axisForeground(x_,(unsigned long)i_);}
  void axisLabelFont(const char *x_,MSAlignment i_)    {axisLabelFont(x_,(unsigned long)i_);}
  void axisLabelFont(Font x_,MSAlignment i_)           {axisLabelFont(x_,(unsigned long)i_);}
  void axisForeground(const char *,unsigned long);
  void axisForeground(unsigned long,unsigned long);
  void axisLabelFont(const char *,unsigned long);
  void axisLabelFont(Font,unsigned long);
  //
  //	 tick parameters
  //
  inline unsigned long tickStyle(MSAlignment) const;
  inline int minorTicks(MSAlignment) const;
  inline int majorTickSize(MSAlignment) const;
  inline int minorTickSize(MSAlignment) const;
  void tickStyle(unsigned long x_,MSAlignment i_)  {tickStyle(x_,(unsigned long)i_);}
  void minorTicks(unsigned x_,MSAlignment i_)      {minorTicks(x_,(unsigned long)i_);}
  void majorTickSize(unsigned x_,MSAlignment i_)   {majorTickSize(x_,(unsigned long)i_);}
  void minorTickSize(unsigned x_,MSAlignment i_)   {minorTickSize(x_,(unsigned long)i_);}
  void tickStyle(unsigned long,unsigned long);
  void minorTicks(unsigned,unsigned long);
  void majorTickSize(unsigned,unsigned long); 
  void minorTickSize(unsigned,unsigned long);
  //
  //	bar graph parameters
  //
  int maxBarWidth(void)                    const {return _maxBarWidth;}
  void maxBarWidth(unsigned);
  //
  //	pie graph parameters
  //
  double pieOffsetMargin(void)             const {return _pieOffsetMargin;}
  void pieOffsetMargin(double);
  //
  //
  void orientation(Orientation x_);        
  unsigned long orientation(void) const          {return _orientation; }
  //
  //    editor interface methods
  //
  const char *editorString(void)           const {return _editor->string();}
  unsigned long editorFont(void)           const {return _editor->MSWidget::font();}
  unsigned long editorForeground(void)     const {return _editor->MSWidget::foreground();}
  unsigned long editorBackground(void)     const {return _editor->MSWidget::background();}
  void editorFont(const char *x_)                {_editor->font(server()->fontID(x_));}
  void editorFont(Font x_)                       {_editor->font(x_);}
  void editorForeground(const char *x_)          {_editor->foreground(server()->pixel(x_));}
  void editorForeground(unsigned long x_)        {_editor->foreground(x_);}
  void editorBackground(const char *x_)          {_editor->background(server()->pixel(x_));}
  void editorBackground(unsigned long x_)        {_editor->background(x_);}
  //
  //	trace defaults
  //
  const MSStringVector& defaultTraceLineColors(void) const {return _defaultLineColors;}
  const MSStringVector& defaultTraceFillColors(void) const {return _defaultFillColors;}
  void defaultTraceLineColors(MSStringVector& x_)          {_defaultLineColors=x_;}
  void defaultTraceFillColors(MSStringVector& x_)          {_defaultFillColors=x_;}
protected:
  enum CursorType {XYcursor,Xcursor,Ycursor};
  //
  //  	 Trace interface methods
  //
  const MSTraceSet *graphTraceSet(unsigned i_)          const {return traceSetList().array(i_);}
  const MSTrace *graphTrace(unsigned i_)                const {return traceList().array(i_);}
  MSTraceSet *graphTraceSet(unsigned i_)                      {return traceSetList().array(i_);}
  MSTrace *graphTrace(unsigned i_)                            {return traceList().array(i_);}
  //
  //	redraw/event methods
  //
  void init(void);
  MSLabelFormat& outFmt(void)    const {return _outFormat;}
  MSLabelOut& labelOut(void)     const {return _labelOut;}

  virtual void firstMapNotify(void);
  virtual void naturalSize(void);
  virtual void computeSize(void);
  virtual void placement(void);
  virtual void redrawImmediately(MSBoolean update_=MSFalse,MSBoolean append_=MSFalse);
  void redrawSansRescale(void);
  virtual MSBoolean loseFocus(void);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void keyRelease(const XEvent *); 
  virtual void keyRelease(const XEvent *,KeySym,unsigned,const char*);
  virtual void motionNotify(const XEvent *);
  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void button1Press(const XEvent *);
  virtual void button2Press(const XEvent *);
  virtual void button3Press(const XEvent *);
  virtual void configure(void);
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void visibilityObscured(void);
  virtual void visibilityUnobscured(void);
  virtual void insertChild(MSWidget *);
  virtual void removeChild(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childRemove(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childDestroy(MSWidget *);

  //
  //	GC methods
  //
  GC clearGC(void)                     {return _clearGC;}
  GC textGC(void)                      {return _textGC;}
  GC subtitleGC(void)                  {return _subtitleGC;}
  GC footnoteGC(void)                  {return _footnoteGC;}
  GC windowGC(void)                    {return _windowGC;}
  GC axisTitleGC(void)                 {return _axisTitleGC;}
  GC axisGC(void)                      {return _axisGC;}
  GC gridGC(void)                      {return _gridGC;}
  GC zeroGC(void)                      {return _zeroGC;}
  GC traceGC(void)                     {return _traceGC;}
  //
  //	Axis methods
  //
  int *yGridWidth(void)                {return _yGridWidth;}
  int yGridWidth(int i)                {return _yGridWidth[i];}
  int *yGrid(void)                     {return _yGrid;}
  int yGrid(int i)                     {return _yGrid[i];}
  int yGridCount(void)                 {return _yGridCount;}
  int *xGridWidth(void)                {return _xGridWidth;}
  int xGridWidth(int i)                {return _xGridWidth[i];}
  int *xGrid(void)                     {return _xGrid;}
  int xGrid(int i)                     {return _xGrid[i];}
  int xGridCount(void)                 {return _xGridCount;}

  void yGridWidth(int i,int x_)        {_yGridWidth[i]=x_;}
  void yGrid(int i,int x_)             {_yGrid[i]=x_;}
  void yGridCount(int x_)              {_yGridCount=x_;}
  void xGridWidth(int i,int x_)        {_xGridWidth[i]=x_;}
  void xGrid(int i,int x_)             {_xGrid[i]=x_;}
  void xGridCount(int x_)              {_xGridCount=x_;}
  //
  //	Object pointers
  //
  Pixmap graphPixmap(void)        const {return _graphPixmap->pixmap();}
  Cursor zoomCursor(void)         const {return _zoomCursor->cursor();}
  Cursor drawCursor(void)         const {return _drawCursor->cursor();}
  Cursor lineCursor(void)         const {return _lineCursor->cursor();}
  MSGraphStatusWin *dataWin(void) const {return _dataWin;}
  MSGraphLegend *legend(void)     const {return _legend;}
  MSGraphEditor *editor(void)     const {return _editor;}
  //
  //	Status methods
  //
  MSBoolean showXaxis(int i_);
  MSBoolean showYaxis(int i_);
  MSBoolean showXrule(int i_);
  MSBoolean showYrule(int i_);
  MSBoolean xShowPos(void)             {return _xShowPos;}
  MSBoolean yShowPos(void)             {return _yShowPos;}
  MSBoolean keyPressStatus(void)       {return _keyPressStatus;}
  MSBoolean updateLegendStatus(void)   {return _updateLegendStatus;}
  MSBoolean graphZoomStatus(void)      {return _graphZoomStatus;}
  unsigned long cursorType(void)       {return _cursorType;}

  void showXaxis(MSBoolean x_,int i_)  {_showXaxis[i_]=x_;}
  void showYaxis(MSBoolean x_,int i_)  {_showYaxis[i_]=x_;}
  void showXrule(MSBoolean x_,int i_)  {_showXrule[i_]=x_;}
  void showYrule(MSBoolean x_,int i_)  {_showYrule[i_]=x_;}
  void xShowPos(MSBoolean x_)          {_xShowPos=x_;}
  void yShowPos(MSBoolean x_)          {_yShowPos=x_;}
  void keyPressStatus(MSBoolean x_)    {_keyPressStatus=x_;}
  void updateLegendStatus(MSBoolean x_){_updateLegendStatus=x_;}
  void graphZoomStatus(MSBoolean x_)   {_graphZoomStatus=x_;}
  void cursorType(CursorType x_)       {_cursorType=x_;}
  //
  //	PlotArea methods
  //
  MSRect *plotAreaRect(void)           {return &_plotAreaRect;}
  MSRect *updateRect(void)             {return &_updateRect;}
  int y_org(void)                      {return _y_org;}
  int x_org(void)                      {return plotAreaRect()->x();}
  int y_end(void)                      {return _y_end;}
  int x_end(void)                      {return _x_end;}
  //
  //	Scale/sizing methods
  //
  int xMinZoom(void)                   {return _xMinZoom;}
  int xMaxZoom(void)                   {return _xMaxZoom;}
  int yMinZoom(void)                   {return _yMinZoom;}
  int yMaxZoom(void)                   {return _yMaxZoom;}
  double xScaleReal(int i_)            {return _xScaleReal[i_];}
  double xBaseReal(int i_)             {return _xBaseReal[i_];}
  double xMinReal(int i_)              {return _xMinReal[i_];}
  double xMaxReal(int i_)              {return _xMaxReal[i_];}
  double xMin(int i_)                  {return _xMin[i_];}
  double yMin(int i_)                  {return _yMin[i_];}
  double xMax(int i_)                  {return _xMax[i_];}
  double yMax(int i_)                  {return _yMax[i_];}
  double xScale(int i_)                {return _xScale[i_];}
  double yScale(int i_)                {return _yScale[i_];}
  double xBase(int i_)                 {return _xBase[i_];}
  double yBase(int i_)                 {return _yBase[i_];}
  double xMinData(int i_)              {return _xMinData[i_];}
  double xMaxData(int i_)              {return _xMaxData[i_];}
  double yMinData(int i_)              {return _yMinData[i_];}
  double yMaxData(int i_)              {return _yMaxData[i_];}
  double xIncData(int i_,int l_=0)     {return _xIncData[i_][l_];}
  double yIncData(int i_)              {return _yIncData[i_];}
  double normalizedLabelInc(void)      {return _normalizedLabelInc;}

  void xMinZoom(int x_)                {_xMinZoom=x_;}
  void xMaxZoom(int x_)                {_xMaxZoom=x_;}
  void yMinZoom(int x_)                {_yMinZoom=x_;}
  void yMaxZoom(int x_)                {_yMaxZoom=x_;}
  void xScaleReal(double x_,int i_)    {_xScaleReal[i_]=x_;}
  void xBaseReal(double x_,int i_)     {_xBaseReal[i_]=x_;}
  void xMinReal(double x_,int i_)      {_xMinReal[i_]=x_;}
  void xMaxReal(double x_,int i_)      {_xMaxReal[i_]=x_;}
  void xMin(double x_,int i_)          {_xMin[i_]=x_;}
  void yMin(double x_,int i_)          {_yMin[i_]=x_;}
  void xMax(double x_,int i_)          {_xMax[i_]=x_;}
  void yMax(double x_,int i_)          {_yMax[i_]=x_;}
  void xScale(double x_,int i_)        {_xScale[i_]=x_;}
  void yScale(double x_,int i_)        {_yScale[i_]=x_;}
  void xBase(double x_,int i_)         {_xBase[i_]=x_;}
  void yBase(double x_,int i_)         {_yBase[i_]=x_;}
  void xMinData(double x_,int i_)      {_xMinData[i_]=x_;}
  void xMaxData(double x_,int i_)      {_xMaxData[i_]=x_;}
  void yMinData(double x_,int i_)      {_yMinData[i_]=x_;}
  void yMaxData(double x_,int i_)      {_yMaxData[i_]=x_;}
  void xIncData(double x_,int i_,int l_=0) {_xIncData[i_][l_]=x_;}
  void yIncData(double x_,int i_)      {_yIncData[i_]=x_;}
  void xMinSet(double x_,int i_)       {_xMinSet[i_]=x_;}
  void yMinSet(double x_,int i_)       {_yMinSet[i_]=x_;}
  void xMaxSet(double x_,int i_)       {_xMaxSet[i_]=x_;}
  void yMaxSet(double x_,int i_)       {_yMaxSet[i_]=x_;}
  void normalizedLabelInc(double x_)   {_normalizedLabelInc=x_;}

  inline int xValueToPixel(double,int);
  inline int yValueToPixel(double,int);
  inline double normalizedPixelToValue(int,int);
  inline double xPixelToValue(int,int);
  inline double yPixelToValue(int,int);
  
  //
  //	Internal dimensions
  //
  int pieCount(void)                   {return _pieCount;} 
  int barCount(void)                   {return _barCount;} 
  int barWidth(void)                   {return _barWidth;}
  int xBar(void)                       {return _xBar;} 
  int offset(void)                     {return highlightThickness()+shadowThickness();}
  int leftMargin(void)                 {return offset()+(int)(_margin[0]*width());}
  int rightMargin(void)                {return offset()+(int)(_margin[1]*width());}
  int topMargin(void)                  {return offset()+(int)(_margin[2]*height());}
  int bottomMargin(void)               {return offset()+(int)(_margin[3]*height());}
  int titleHeight(void)                {return _titleHeight;} 
  int subtitleHeight(void)             {return _subtitleHeight;} 
  int footnoteHeight(void)             {return _footnoteHeight;}
  void titleHeight(int x_)             {_titleHeight=x_;}
  void subtitleHeight(int x_)          {_subtitleHeight=x_;}
  void footnoteHeight(int x_)          {_footnoteHeight=x_;}
  double estimateNormalizedLabelValue(int,double);
  int fontSize(Font);
  Font adjustFontSize(Font,int);
  int indexOfLongestString(MSStringVector&);
  int maxStringWidth(XFontStruct*,MSStringVector&);
  //
  //	Pie graph methods
  //
  void pieCount(int x_)                {_pieCount=x_;} 
  //
  //	Bar graph methods
  //
  void barWidth(int);
  void barCount(int x_)                {_barCount=x_;} 
  void xBar(int x_)                    {_xBar=x_;} 
  //
  //	Draw methods
  //
  void computeNormalizedOffsets(void);
  void computeExtents(void);
  void computeXincrement(int level_=0);
  void computeUpdateRegion(MSTrace *);
  void computeScales(void);
  int computeXscales(int&,int&,int);
  int computeYscale(int);
  void appendDataPoint(MSTrace *);
  void drawGraph(MSBoolean update_=MSFalse);
  void drawYaxes(int);
  void drawAxes(void);
  void drawXaxes(int);
  void drawXsubLabels(int);
  void drawRule(void);
  void drawGrid(void);
  void drawZeroAxis(void);
  void drawRules(void);
  void drawHtraces(MSBoolean update_=MSFalse);
  void drawVtraces(MSBoolean update_=MSFalse);
  void drawTitle(Window);
  void drawSubtitle(Window);
  void drawFootnote(Window);
  void positionLegend(unsigned long);
  void computeLegendSize(void);
  void drawLegends(void);
  void moveLegend(const XEvent *);
  void drawLegendSymbols(MSTrace *,int,int,int index=0);
  void drawLegendScatterSymbols(Window,GC,MSTrace*,int,int);
  void drawXtitle(void);
  void drawYtitle(void);
  void drawYaltAxisTitle(void);
  void plotTextTrace(MSTrace **,int);
  void drawVerticalString(GC,int,int,const char *,int,XFontStruct*);
  inline void fillPolygons(Display *,Window,GC,XPoint *,int,int,int,int);
  //
  //	Plot methods
  //
  void setLineAttributes(unsigned long,int,GC,int,int,int);
  int setLineWidth(MSTrace *);
  void plotLineTrace(MSTrace *,int,int,int);
  void plotFillTrace(MSTrace *,MSBoolean);
  void drawMoveLineTrace(MSTrace *);
  void drawMoveTrace(MSTrace *);
  void plotOutlineTrace(MSTrace *,int,int,int);
  void plotScatterTrace(MSTrace *,int,int,int);
  void drawScatterSymbols(Window,GC,MSTrace*,void*,int,unsigned long,unsigned long,XFontStruct*);
  void plotAreaTrace(MSTrace **,int,MSBoolean mode_=MSFalse);
  void plotStackTrace(MSTrace **,int&,int,MSBoolean mode_=MSFalse);
  void plotBarTrace(MSTrace *,int,int,int,int);
  void plotHBarTrace(MSTrace *);
  void plotHighLowTrace(MSTrace *,int,int,int);
  void plotOpenTicks(MSTrace *,int,int,int);
  void plotCloseTicks(MSTrace *,int,int,int);
  void plotCandleTrace(MSTrace *,int,int,int);
  void plotHStackTrace(MSTrace *);
  void plotStepTrace(MSTrace *,int,int,int);
  void plotBetweenTrace(MSTrace *);
  void plotPieTrace(MSTrace *);
  void plotSegmentTrace(MSTrace *,Window,GC);
  void plotMarketProfile(MSTrace *,int);

  //
  //    Pie drawing methods
  //
  void computePieData(MSTrace*,MSGraphPieData*);
  void computeDrawingSequence(MSGraphPieData*);
  void computePieLabels(MSTrace *,const MSGraphPieData*,MSGraphPieLabelData*);
  void drawPieSlices(MSTrace *,const MSGraphPieData*,int,int);
  void drawLabels(MSTrace *,const MSGraphPieLabelData*);
  void clearPieData(void);
  void alignPieLabels(MSTrace *,MSStringVector&,MSStringVector&,const MSStringVector&,unsigned);
  //
  //	Format methods
  //
  inline unsigned long formatAlignment(unsigned long);
  inline const char *formatAxisLabel(MSString &,MSLabelOut&,double,MSBoolean plus_=MSFalse);
  inline const char *formatTimeLabel(MSString &,double);

  // X, Y values accesss methods
  //
  
  inline double xValue(MSTrace*,int);
  inline double yValue(MSTrace*,int);
  inline double xValueMin(MSTrace*);
  inline double xValueMax(MSTrace*);
  inline double yValueMin(MSTrace*);
  inline double yValueMax(MSTrace*);
  //
  //	Interactive methods
  //
  void drawLineSegments(void);
  void drawLineHandles(int pt_=-1);
  void drawLineHandles(MSTrace *,MSTrace *refTrace=0);
  void moveLineHandle(const XEvent *);
  void drawMoveLineHandleSymbols(int,int);
  MSBoolean addLineSegment(const XEvent *);
  MSBoolean moveLineSegment(const XEvent *,MSBoolean copy_=MSFalse);
  MSBoolean findLineHandle(int,int);
  MSBoolean findLineSegment(int,int);
  MSBoolean findSelectTraceLineHandle(int,int);
  int  findSelectedSlice(MSTrace *,int,int);
  MSTrace *findSelectableTrace(const XEvent *);
  MSTrace *findTextTrace(const XEvent *);
  MSTrace *findDataPoint(const XEvent *);
  int findYvalue(MSTrace *,double);
  double findMaxYvalue(void);	
  void moveDataPoint(const XEvent *);
  void drawScanXYvalueBox(int,int);
  void drawMoveDataPointValueBox(int,int);
  void drawMoveDataPointSymbols(MSTrace *,int,int);
  void enterTextTrace(void);
  void editTextTrace(void);
  void enterTrace(const XEvent *);  
  void moveTextTrace(const XEvent *);  
  void moveTrace(const XEvent *);  
  void deleteTrace(void);
  void drawCursor(const XEvent *);
  void drawPixelPosition(const XEvent *event_);
  void scanXY(const XEvent *);
  void drawScanXvalues(int x_=0,int y_=0,int i_=0,int *lastIndex_=0);
  void drawScanXYvalues(double,double,int,int);
  void drawYdata(MSTrace *,int,int,int);
  MSBoolean drawZoomRegion(const XEvent *);
  void shiftZoomWindow(KeySym);
  virtual void copyTrace(MSTrace *);  
  virtual MSBoolean moveTraceValidate(MSTrace *);  
  const MSString& interactiveTag(void) const;

  //
  //	Highlight methods
  //
  MSBoolean highlightLegendStatus(void)    {return _highlightLegendStatus;}
  MSBoolean highlightTraceStatus(void)     {return _highlightTraceStatus;}
  void highlightLegendStatus(MSBoolean x_) {_highlightLegendStatus=x_; drawLegends();}
  void highlightTraceStatus(MSBoolean x_)  {_highlightTraceStatus=x_;}
  void highlightPoint(MSTraceSet *);
  void highlightTrace(void);
  void unHighlightTrace(void);
  void highlightTextTrace(void);
  void unHighlightTextTrace(void);
  //
  //	Trace symbol methods
  //
  void buildSquareSymbol(void*,int&,int,int,int);
  void buildCircleSymbol(void*,int&,int,int,int);
  void buildStarSymbol(void*,int&,int,int,int);
  void buildCrossSymbol(void*,int&,int,int,int);
  void buildXSymbol(void*,int&,int,int,int);
  void buildTriangleSymbol(void*,int&,int,int,int);
  void buildTrianglePrintSymbol(void*,int&,int,int,int);
  void buildDiamondSymbol(void*,int&,int,int,int);
  void buildDiamondPrintSymbol(void*,int&,int,int,int);
  void buildCharSymbol(void*,int&,int,int,int);
  //
  //	Timer methods
  //
  MSBoolean doubleClick(void);
  MSBoolean doubleClick(const XEvent *);
  MSBoolean longPress(const XEvent *);
  static unsigned long longPressTime(void)         {return _longPressTime;}
  static unsigned long doubleClickTime(void)       {return _doubleClickTime;}
  static unsigned long doubleClickEventTime(void)  {return _doubleClickEventTime;}
  MSBoolean buttonTimerExpire(void)                {return _buttonTimerExpire;}
  static void longPressTime(unsigned long x_)      {_longPressTime=x_;}
  static void doubleClickTime(unsigned long x_)    {_doubleClickTime=x_;}
  void buttonTimerExpire(MSBoolean x_)             {_buttonTimerExpire=x_;}
  void doubleClickEventTime(unsigned long x_)      {_doubleClickEventTime=x_;}
  void startRepeatTimer(void);
  void stopRepeatTimer(void);
  void processRepeatTimer(void);
  //
  //  	 Market profile attributes
  //
  MSBoolean marketProfileStatus(void)           {return _marketProfileStatus;}
  double sessionPeriod(void)                    {return _sessionPeriod;}
  double sessionOffset(void)                    {return _sessionOffset;}
  double tpoPriceInc(void)                      {return _tpoPriceInc;}
  double tpoPeriod(void)                        {return _tpoPeriod;}
  double tpoOpen(void)                          {return _tpoOpen;}
  double tpoClose(void)                         {return _tpoClose;}
  int tpoBreakChar(void)                        {return _tpoBreakChar;}
  
  void marketProfileStatus(MSBoolean x_)        {_marketProfileStatus=x_;}
  void sessionPeriod(double x_); 
  void sessionOffset(double x_); 
  void tpoPriceInc(double x_); 
  void tpoPeriod(double x_); 
  void tpoOpen(double x_); 
  void tpoClose(double x_); 
  void tpoBreakChar(int key_); 
  //
  //  	 Trace interface methods
  //
  const MSPointerArray<MSTraceSet>& traceSetList(void) const {return _traceSetList;}
  const MSPointerArray<MSTrace>& traceList(void)       const {return _traceList;}
  MSPointerArray<MSTraceSet>& traceSetList(void)             {return _traceSetList;}
  MSPointerArray<MSTrace>& traceList(void)                   {return _traceList;}
  //
  //  	 Interactive trace entry methods
  //
  void newtraceAlloc(void);
  void newtraceDealloc(void);
  MSGraphNewtrace *nt(int index_);
  int newtraceCt(void)                          {return _newtraceCt;}
  int selectPoint(void)                         {return _selectPoint;}
  int selectLine(void)                          {return _selectLine;}
  int focusLine(void)                           {return _focusLine;}
  void selectPoint(int x_)                      {_selectPoint=x_;}
  void selectLine(int x_)                       {_selectLine=x_;}
  void focusLine(int x_)                        {_focusLine=x_;}

  //
  //  	 Internal Access methods
  //
  AxisMode yMode(int i_)                        {return _yMode[i_];}
  AxisMode xMode(int i_)                        {return _xMode[i_];}
  double margin(int s_)                         {return _margin[s_];}
  double xMargin(int i_=0,int s_=0)             {return _xMargin[i_][s_];}
  double yMargin(int i_=0,int s_=0)             {return _yMargin[i_][s_];}
  MSBoolean drawEndTicks(void)                  {return _drawEndTicks;}
  int graphDepth(void)                          {return _graphDepth;}
  double xMinSet(int i_)                        {return _xMinSet[i_];}
  double xMaxSet(int i_)                        {return _xMaxSet[i_];}
  double yMinSet(int i_)                        {return _yMinSet[i_];}
  double yMaxSet(int i_)                        {return _yMaxSet[i_];}
  double xLabelIncrement(int i_,int j_=0)       {return _xLabelInc[i_][j_];}
  double yLabelIncrement(int i_)                {return _yLabelInc[i_];}
  const MSStringVector& xTitle(int i_=0)        {return _xTitle[i_];}
  const MSStringVector& yTitle(int i_)          {return _yTitle[i_];}
  unsigned long xTitleForeground(int i_)        {return _xTitleFg[i_];}
  unsigned long yTitleForeground(int i_)        {return _yTitleFg[i_];}
  Font xTitleFont(int i_)                       {return _xTitleFont[i_];}
  MSAlignment xTitleAlign(int i_)               {return _xTitleAlign[i_];}
  Font yTitleFont(int i_)                       {return _yTitleFont[i_];}
  unsigned long yTitleAlign(int i_)             {return _yTitleAlign[i_];}
  MSAlignment xLabelAlign(int i_)               {return _xLabelAlign[i_];}
  MSAlignment xSubLabelAlign(int i_)            {return _xSubLabelAlign[i_];}
  MSAlignment yLabelAlign(int i_)               {return _yLabelAlign[i_];}
  Font xLabelFont(int i_)                       {return _xLabelFont[i_];}
  Font yLabelFont(int i_)                       {return _yLabelFont[i_];}
  unsigned long xTickStyle(int i_)              {return _xTickStyle[i_];}
  unsigned long yTickStyle(int i_)              {return _yTickStyle[i_];}
  int xNumMinorTicks(int i_)                    {return _xNumMinorTicks[i_];}
  int yNumMinorTicks(int i_)                    {return _yNumMinorTicks[i_];}
  int xMajorTickSize(int i_)                    {return _xMajorTickSize[i_];}
  int yMajorTickSize(int i_)                    {return _yMajorTickSize[i_];}
  int xMinorTickSize(int i_)                    {return _xMinorTickSize[i_];}
  int yMinorTickSize(int i_)                    {return _yMinorTickSize[i_];}
  MSIntMatrix& interactivePixel()               {return _interactivePixel;}
  unsigned long xAxisForeground(int x_)         {return _xAxisFg[x_];}
  unsigned long yAxisForeground(int x_)         {return _yAxisFg[x_];}
  MSLabelOut& xLabelOut(int i_)                 {return _xLabelOut[i_]!=0?*_xLabelOut[i_]:labelOut();}
  MSLabelOut& xSubLabelOut(int i_)              {return _xSubLabelOut[i_]!=0?*_xSubLabelOut[i_]:labelOut();}
  MSLabelOut& yLabelOut(int i_)                 {return _yLabelOut[i_]!=0?*_yLabelOut[i_]:labelOut();}
  unsigned int unsetValue(void)                 {return _UnsetValue;}
  int xStringWidth(void)                        {return _xStringWidth;}
  int yStringWidth(void)                        {return _yStringWidth;}
  int legendWidth(void)                         {return _legendWidth;}
  int legendHeight(void)                        {return _legendHeight;}

  void drawEndTicks(MSBoolean x_)               {_drawEndTicks=x_;}
  void graphDepth(int x_)                       {_graphDepth=x_;}
  void xStringWidth(int x_)                     {_xStringWidth=x_;}
  void yStringWidth(int x_)                     {_yStringWidth=x_;}
  void legendWidth(int x_)                      {_legendWidth=x_;}
  void legendHeight(int x_)                     {_legendHeight=x_;}
  void graphUIModeSet(GraphUIMode x_)           {_graphUIMode=x_;}
  virtual void updateData(void);

  virtual void update(const MSIndexVector& x_);
  virtual void update(MSTraceSet *,const MSIndexVector&);
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void updateTitle(void);
  virtual void updateSubtitle(void);
  virtual void updateFootnote(void);
  void editorActivate(void);
  void editorEscape(void);
  virtual void printDebugInfo(const char*);

  virtual void set(MSAttrValueList&);
  virtual MSAttrValueList& get(MSAttrValueList&);

private:  
  typedef void (MSGraph::*PSF)(void*,int&,int,int,int);

  GC 			_clearGC;
  GC 			_textGC;
  GC 			_axisGC;
  GC 			_gridGC;
  GC 			_zeroGC;
  GC 			_traceGC;
  GC 			_subtitleGC;
  GC 			_footnoteGC;
  GC 			_windowGC;
  GC 			_axisTitleGC;

  MSAlignment	        _titleAlign;
  MSStringVector 	_subtitle;
  unsigned long 	_subtitleForeground;
  Font  		_subtitleFont;
  MSAlignment    	_subtitleAlign;

  MSStringVector 	_footnote;
  unsigned long 	_footnoteForeground;
  Font  		_footnoteFont;
  MSAlignment   	_footnoteAlign;

  MSStringVector	_xTitle[2];
  unsigned long 	_xTitleFg[2];
  unsigned long 	_xAxisFg[2];
  MSAlignment    	_xTitleAlign[2];
  unsigned long		_xTickStyle[2];
  double		_xMargin[2][2];
  Font  		_xTitleFont[2];
  Font  		_xLabelFont[2];

  MSStringVector	_yTitle[2];
  unsigned long 	_yTitleFg[2];
  unsigned long 	_yAxisFg[2];
  unsigned long  	_yTitleAlign[2];
  unsigned long 	_yTickStyle[2];
  double		_yMargin[2][2];
  Font  		_yTitleFont[2];
  Font  		_yLabelFont[2];

  unsigned long		_axis;
  unsigned long		_axisRule;
  AxisMode		_yMode[2];
  AxisMode		_xMode[2];
  unsigned long 	_grid;
  unsigned long 	_gridForeground;
  unsigned long	 	_gridStyle;
  unsigned long		_zeroAxis;
  unsigned long 	_zeroAxisForeground;
  unsigned long	 	_zeroAxisStyle;
  int 		 	_zeroAxisWidth;
  int 		 	_zeroAxisWeight;
  double		_margin[4];

  MSGraphEditor        *_editor;
  MSGraphLegend	       *_legend;
  MSGraphStatusWin     *_dataWin;
  MSBackingStorePixmap *_graphPixmap;
  MSDisplayCursor      *_zoomCursor;
  MSDisplayCursor      *_drawCursor;
  MSDisplayCursor      *_lineCursor;
  MSTrace 	       *_selectTrace;
  
  MSRect		_plotAreaRect;
  MSRect		_updateRect;
  int 			_y_org;
  int 			_y_end;
  int 			_x_org;
  int 			_x_end;

  int                   _graphDepth;
  double 		_showSizeRatio;
  unsigned long		_orientation;
  unsigned long		_graphMode;
  GraphUIMode		_graphUIMode;

  static MSLabelFormat  _outFormat;
  static MSLabelOut     _labelOut;
  static MSLabelOut     _timeLabelOut;
  MSLabelOutPtr         _xLabelOut[2];
  MSLabelOutPtr         _xSubLabelOut[2];
  MSLabelOutPtr         _yLabelOut[2];
  MSAlignment	        _xSubLabelAlign[2];
  MSAlignment	        _xLabelAlign[2];
  MSAlignment	        _yLabelAlign[2];

  KeySym 		_arrowKeysym;
  MSGraphRepeatTimer   *_repeatTimer;
  MSBoolean             _repeatOn;

  MSSymbol              _tag;
  MSBoolean		_marketProfileStatus;
  double		_sessionPeriod;
  double		_sessionOffset;
  double		_tpoPriceInc;
  double		_tpoPeriod;
  double		_tpoOpen;
  double		_tpoClose;
  int			_tpoBreakChar;
  
  int 		        _xMinZoom;
  int 		        _xMaxZoom;
  int 		        _yMinZoom;
  int 		        _yMaxZoom;
  double 		_xScaleReal[2];
  double 		_xBaseReal[2];
  double 		_xMinReal[2];
  double 		_xMaxReal[2];
  double 		_xScale[2];
  double		_yScale[2];
  double 		_xBase[2];
  double		_yBase[2];
  double 		_xMin[2];
  double 		_xMax[2];
  double 		_yMin[2];
  double 		_yMax[2];
  double 		_xMinData[2];
  double 		_xMaxData[2];
  double		_yMinData[2];
  double 		_yMaxData[2];
  double 		_xMinSet[2];
  double 		_xMaxSet[2];
  double 		_yMinSet[2];
  double 		_yMaxSet[2];
  double 		_xLabelInc[2][2];
  double 		_yLabelInc[2];
  double 		_yInc[2];
  double 		_xIncData[2][2];
  double 		_yIncData[2];
  double 		_normalizedLabelInc;
  MSIntMatrix           _interactivePixel;
  MSFloatMatrix         _interactiveData;
  MSStringVector        _interactiveTraceTags;
  static MSString       _defaultTraceTag;
  
  int	 		_naturalWidth;
  int	 		_naturalHeight;
  int                   _newTraceCount;
  
  MSBoolean             _highlightLegendStatus;
  MSBoolean             _highlightTraceStatus;
  MSBoolean		_graphZoomStatus;
  MSBoolean		_xShowPos;
  MSBoolean		_yShowPos;
  MSBoolean		_keyPressStatus;
  MSBoolean		_updateLegendStatus;
  MSBoolean		_showYaxis[2]; 
  MSBoolean		_showXaxis[2]; 
  MSBoolean		_showYrule[2]; 
  MSBoolean		_showXrule[2];
  MSBoolean		_drawEndTicks;

  unsigned long         _style;
  unsigned long 	_legendAlign;
  unsigned long		_legendStyle;
  double		_xLegendPosition;
  double		_yLegendPosition;
  int                   _legendWidth;
  int                   _legendHeight;

  int                   _printWidth;
  int                   _pageWidth;
  int 	                _frameLineWidth;
  int	                _cursorType;
  int   		_axisRuleWidth;
  int   		_gridWidth;
  int   		_gridWeight;
  int		       *_xGrid;
  int		       *_xGridWidth;
  int		        _xGridCount;
  int		       *_yGrid;
  int		       *_yGridWidth;
  int		        _yGridCount;
  int    		_xNumMinorTicks[2];
  int    		_yNumMinorTicks[2];
  int 			_xMajorTickSize[2];
  int			_xMinorTickSize[2];
  int 			_yMajorTickSize[2];
  int			_yMinorTickSize[2];
  int                   _pieCount;
  double                _pieOffsetMargin;
  int                   _barCount;
  int                   _maxBarWidth;
  int                   _barWidth;
  int                   _xBar;
  
  MSGraphNewtrace     **_nt;
  int 	 	        _newtraceCt;
  int 	 	        _newtraceAllocCt;
  int			_newtraceIndex;
  int 			_focusLine;
  int 			_selectLine;
  int			_selectPoint;
  MSBoolean		_onLineSegment;
  MSBoolean		_onLinePoint;
  
  MSBoolean 	        _buttonTimerExpire;
  static unsigned long  _doubleClickEventTime;
  static unsigned long  _doubleClickTime;
  static unsigned long  _longPressTime;

  int	 		_titleHeight;
  int	 		_subtitleHeight;
  int 			_footnoteHeight;
  int	   	        _yLabelWidth[2];
  int 			_yRuleWidth[2];
  int	 		_xStringWidth;
  int			_yStringWidth;

  MSStringVector        _defaultLineColors;
  MSStringVector        _defaultFillColors;

  int                        _selectDistance;
  int    	             _MaxBufSize;
  static const int           _MinBufSize;
  static const int           _SHRT_MAX;
  static const int           _SHRT_MIN;
  static const unsigned int  _UnsetValue;
  static const double        _PrecisionRange;
  static MSBoolean           _debugStatus;
  
  MSPointerArray<MSTraceSet> _traceSetList;
  MSPointerArray<MSTrace>    _traceList;
  

  // pie drawing data
  int    _pieDepth;
  double _pieRx, _pieRy;
  int    _pieX, _pieY;
  int    _startAngle;
  int    _indexOfMaxAngle;
  MSBoolean _orderDependent;
  MSGraphPieData*      _pieData;
  MSGraphPieLabelData* _pieLabelData;


};

class MSGUIExport MSGraphStatusWin:public MSPrimitive
{
friend class MSGraph;

private:
  Font	      	_font;
  int 		_valueWidth;
  int 	      	_spacing;
  int           _leading;
  int 		_insideMargin;
  int 		_minWidth;
  int		_x1,_x2;
  GC	       	_gc;
  XFontStruct  *_fontInfo;
  
protected:
  MSGraphStatusWin(MSWidget *);
  ~MSGraphStatusWin(void);

  MSGraph *graph(void)                        {return (MSGraph *)owner();}
  Font font(void)                             {return _font;}
  int spacing(void)                           {return _spacing;}
  int minWidth(void)                          {return _minWidth;}
  int valueWidth(void)                        {return _valueWidth;}
  int offset(void)                            {return highlightThickness()+shadowThickness();}
  int textHeight(void)                        {return _fontInfo->ascent+_fontInfo->descent;}
  int leading(void)                           {return _leading;}
  int x1(void)                                {return _x1;}
  int x2(void)                                {return _x2;}
  int insideMargin(void)                      {return _insideMargin+offset();}
  GC gc(void)                                 {return _gc;}
  XFontStruct *fontInfo(void)                 {return _fontInfo;}

  virtual void font(Font);
  void valueWidth(int x_)                     {_valueWidth=x_==0?0:(x_>_minWidth)?x_:_minWidth;}
  void spacing(int x_)                        {_spacing=x_;}
  void minWidth(int x_)                       {_minWidth=x_;}
  void leading(int x_)                        {_leading=x_;}
  void x1(int x_)                             {_x1=x_;}
  void x2(int x_)                             {_x2=x_;}
  void insideMargin(int x_)                   {_insideMargin=x_;}
  void setDataWin(int,int);
  virtual void updateBackground(unsigned long);
  virtual void buttonPress(const XEvent *);
};

class MSGUIExport MSGraphLegend:public MSGraphStatusWin
{
friend class MSGraph;

private:
  MSBoolean		_firstDraw;
  int 	      		_columnCount;
  int 	        	_textFieldWidth;
  int 	      		_symbolWidth;
  int 	      		_symbolSize;
  Font 	      		_symbolFont;

protected:
  MSGraphLegend(MSWidget *);
  ~MSGraphLegend(void) {}

  MSGraph *graph(void)                        {return (MSGraph *)owner();}
  int columnCount(void)                       {return _columnCount;}
  int symbolWidth(void)                       {return _symbolWidth;}
  int symbolSize(void)                        {return _symbolSize;}
  Font symbolFont(void)                       {return _symbolFont;}
  int textFieldWidth(void)                    {return _textFieldWidth;}

  void columnCount(int x_)                    {_columnCount=x_;}
  void symbolWidth(int x_)                    {_symbolWidth=x_;}
  void symbolSize(int x_)                     {_symbolSize=x_;}
  void textFieldWidth(int x_)                 {_textFieldWidth=x_;}
  void legendFont(Font x_);
  void symbolFont(Font x_);
  inline int xOffset(void);
  virtual Font font(void);
  virtual void font(Font);
  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
public:
  virtual void redraw(void);
};
inline int MSGraphLegend::xOffset(void)
{return offset()+insideMargin()+symbolWidth()+2*spacing()+textFieldWidth();}

inline int MSGraph::xValueToPixel(double x_,int xs_)
{
  double x=(x_-xBase(xs_))*xScale(xs_)+x_org();
  return (int)(x>MSGraph::_SHRT_MAX?MSGraph::_SHRT_MAX:x<MSGraph::_SHRT_MIN?MSGraph::_SHRT_MIN:x);
}

inline int MSGraph::yValueToPixel(double y_,int ys_)
{
  double y=(double)y_org()-(y_-yBase(ys_))*yScale(ys_);
  return (int)(y>MSGraph::_SHRT_MAX?MSGraph::_SHRT_MAX:y<MSGraph::_SHRT_MIN?MSGraph::_SHRT_MIN:y);
}

inline double MSGraph::xPixelToValue(int x_,int xs_)
{
  return xScale(xs_)!=0?((x_<x_org()?0:x_>x_end()?x_end():x_)-x_org())/xScale(xs_)+xBase(xs_):0;
}

inline double MSGraph::normalizedPixelToValue(int x_,int xs_)
{
  if (graphMode()&Normalize)
   {
     return xScaleReal(xs_)!=0?((x_<x_org()?0:x_>x_end()?x_end():x_)-x_org())/xScaleReal(xs_)+xBaseReal(xs_):0;
   }
  else return xPixelToValue(x_,xs_);
}

inline double MSGraph::yPixelToValue(int y_,int ys_)
{
  return yScale(ys_)!=0?(y_org()-(y_<y_end()?y_end():y_>y_org()?y_org():y_))/yScale(ys_)+yBase(ys_):0;
}
  
inline unsigned long MSGraph::formatAlignment(unsigned long x_)
{
  if ((x_&MSLeft)&&(x_&MSRight)) x_-=MSRight;
  if ((x_&MSTop)&&(x_&MSBottom)) x_-=MSBottom;
  return x_;
}
inline const char *MSGraph::formatAxisLabel(MSString &buffer_,MSLabelOut& out_,double val_,MSBoolean plus_)
{
  out_.formatOutput(buffer_,val_);
  if (plus_==MSTrue&&val_>0) buffer_.insert("+");
  return buffer_;
}

inline const char *MSGraph::formatTimeLabel(MSString &buffer_,double val_)
{
  return _timeLabelOut.formatOutput(buffer_,val_);
}

// xValue, yValue - get value from trace.
// xValue normalize data if in Normalize mode
inline double MSGraph::xValue(MSTrace *trace_,int i_)
{
  return orientation()!=Horizontal?graphMode()&Normalize?(double)i_+trace_->traceSet()->normalizedOffset():trace_->traceSet()->x(i_):trace_->y(i_);
}
inline double MSGraph::yValue(MSTrace* trace_,int i_)
{ return orientation()!=Horizontal?trace_->y(i_):trace_->traceSet()->x(i_);}

inline double MSGraph::xValueMin(MSTrace* trace_)
{ return orientation() == Horizontal ? trace_->yMin() : trace_->traceSet()->xMin();  }
inline double MSGraph::xValueMax(MSTrace* trace_)
{ return orientation() == Horizontal ? trace_->yMax() : trace_->traceSet()->xMax();  }
inline double MSGraph::yValueMin(MSTrace* trace_)
{ return orientation() == Horizontal ? trace_->traceSet()->xMin() : trace_->yMin() ; }
inline double MSGraph::yValueMax(MSTrace* trace_)
{ return orientation() == Horizontal ? trace_->traceSet()->xMax() : trace_->yMax() ; }

//
inline void MSGraph::fillPolygons(Display *dpy_,Window xwin_,GC gc_,XPoint *points_,
		  int n_,int shape_,int mode_,int size_)
{int n=(n_%size_)==0?n_:n_-size_;
 for (unsigned i=0; i<n; i+=size_) PFillPolygon(dpy_,xwin_,gc_,points_+i,size_,shape_,mode_);}
inline double MSGraph::zoomMinimum(MSAlignment i_)
{return i_&MSTop|i_&MSBottom?xPixelToValue(_xMinZoom,i_&MSTop?1:0):yPixelToValue(_yMinZoom,i_&MSRight?1:0);}
inline double MSGraph::zoomMaximum(MSAlignment i_)
{return i_&MSTop|i_&MSBottom?xPixelToValue(_xMaxZoom,i_&MSTop?1:0):yPixelToValue(_yMaxZoom,i_&MSRight?1:0);}
inline double MSGraph::axisMinimum(MSAlignment i_) const
{return i_&MSLeft?_yMinData[0]:i_&MSRight?_yMinData[1]:i_&MSTop?_xMinData[1]:_xMinData[0];}
inline double MSGraph::axisMaximum(MSAlignment i_) const
{return i_&MSLeft?_yMaxData[0]:i_&MSRight?_yMaxData[1]:i_&MSTop?_xMaxData[1]:_xMaxData[0];}
inline double MSGraph::axisLabelIncrement(MSAlignment i_) const
{return i_&MSLeft?_yLabelInc[0]:i_&MSRight?_yLabelInc[1]:i_&MSTop?_xLabelInc[1][0]:_xLabelInc[0][0];}
inline double MSGraph::axisSubLabelIncrement(MSAlignment i_) const
{return _xLabelInc[MSTop&i_?1:0][1];}
inline MSG::AxisMode MSGraph::axisMode(MSAlignment i_) const
{return i_&MSLeft?_yMode[0]:i_&MSRight?_yMode[1]:i_&MSBottom?_xMode[0]:_xMode[1];}
inline const MSStringVector& MSGraph::axisTitle(MSAlignment i_) const
{return i_&MSLeft?_yTitle[0]:i_&MSRight?_yTitle[1]:i_&MSBottom?_xTitle[0]:_xTitle[1];}
inline unsigned long MSGraph::axisTitleForeground(MSAlignment i_) const
{return i_&MSLeft?_yTitleFg[0]:i_&MSRight?_yTitleFg[1]:i_&MSBottom?_xTitleFg[0]:_xTitleFg[1];}
inline Font MSGraph::axisTitleFont(MSAlignment i_) const 
{return i_&MSLeft?_yTitleFont[0]:i_&MSRight?_yTitleFont[1]:i_&MSBottom?_xTitleFont[0]:_xTitleFont[1];}
inline MSAlignment MSGraph::axisTitleAlignment(MSAlignment i_) const 
{return (MSAlignment)(i_&MSLeft?_yTitleAlign[0]:i_&MSRight?
       _yTitleAlign[1]:i_&MSBottom?_xTitleAlign[0]:_xTitleAlign[1]);}
inline MSAlignment MSGraph::axisLabelAlignment(MSAlignment i_) const
{return i_&MSLeft?_yLabelAlign[0]:i_&MSRight?_yLabelAlign[1]:i_&MSBottom?_xLabelAlign[0]:_xLabelAlign[1];}
inline MSAlignment MSGraph::axisSubLabelAlignment(MSAlignment i_) const
{return _xSubLabelAlign[MSTop&i_?1:0];}

inline MSLabelOutPtr MSGraph::axisLabelOut(MSAlignment i_) const
{ return i_&MSLeft?_yLabelOut[0]:i_&MSRight?_yLabelOut[1]:i_&MSBottom?_xLabelOut[0]:_xLabelOut[1];}
inline MSLabelOutPtr MSGraph::axisSubLabelOut(MSAlignment i_) const
{ return _xSubLabelOut[MSTop&i_?1:0]; }

inline void MSGraph::axisLabelOut(MSLabelOut *x_,unsigned long i_)
{ axisLabelOut(MSLabelOutPtr(x_,MSInit),i_); }
inline void MSGraph::axisSubLabelOut(MSLabelOut *x_,unsigned long i_)
{ axisSubLabelOut(MSLabelOutPtr(x_,MSInit),i_); }
inline void MSGraph::axisLabelOut(MSLabelOut *x_,MSAlignment i_)
{ axisLabelOut(MSLabelOutPtr(x_,MSInit),(unsigned long)i_); }
inline void MSGraph::axisSubLabelOut(MSLabelOut *x_,MSAlignment i_)
{ axisSubLabelOut(MSLabelOutPtr(x_,MSInit),(unsigned long)i_); }

inline unsigned long MSGraph::axisForeground(MSAlignment i_) const
{return i_&MSLeft?_yAxisFg[0]:i_&MSRight?_yAxisFg[1]:i_&MSBottom?_xAxisFg[0]:_xAxisFg[1];}
inline Font MSGraph::axisLabelFont(MSAlignment i_) const
{return i_&MSLeft?_yLabelFont[0]:i_&MSRight?_yLabelFont[1]:i_&MSBottom?_xLabelFont[0]:_xLabelFont[1];}
inline unsigned long MSGraph::tickStyle(MSAlignment i_) const
{return i_&MSLeft?_yTickStyle[0]:i_&MSRight?_yTickStyle[1]:i_&MSBottom?_xTickStyle[0]:_xTickStyle[1];}
inline int MSGraph::minorTicks(MSAlignment i_) const
{return i_&MSLeft?_yNumMinorTicks[0]:i_&MSRight?_yNumMinorTicks[1]:
 i_&MSBottom?_xNumMinorTicks[0]:_xNumMinorTicks[1];}
inline int MSGraph::majorTickSize(MSAlignment i_) const
{return i_&MSLeft?_yMajorTickSize[0]:i_&MSRight?_yMajorTickSize[1]:
 i_&MSBottom?_xMajorTickSize[0]:_xMajorTickSize[1];}
inline int MSGraph::minorTickSize(MSAlignment i_) const
{return i_&MSLeft?_yMinorTickSize[0]:i_&MSRight?_yMinorTickSize[1]:
 i_&MSBottom?_xMinorTickSize[0]:_xMinorTickSize[1];}

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSPointerArray<MSTrace>
#pragma do_not_instantiate MSPointerArray<MSTraceSet>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
