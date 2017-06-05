#ifndef MSRowColumnViewHEADER
#define MSRowColumnViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSCompositeText.H>
#include <MSGUI/MSPrimitive.H>
#include <MSGUI/MSLabel.H>
#include <MSGUI/MSTextField.H>
#include <MSGUI/MSDisplayCursor.H>
#include <MSGUI/MSVScrollBar.H>
#include <MSGUI/MSHScrollBar.H>
#include <MSGUI/MSColorCycle.H>
#include <MSGUI/MSDoubleClick.H>
#include <MSGUI/MSGC.H>
#include <MSGUI/MSBackingStorePixmap.H>
#include <MSTypes/MSStringVector.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSIPC/MSIntervalTimer.H>

class MSGUIExport MSRowColumnView : public MSCompositeText, public MSDoubleClick
{
public:
  class MSGUIExport Panner : public MSPrimitive
  {
  friend class MSRowColumnView;
  public:
    Panner(MSWidget *owner_);
    ~Panner(void);
    virtual void redraw(void);
  protected:
    virtual void expose(const XEvent *); 
  };

  class MSGUIExport Editor : public MSTextField
  {
  public:
    Editor(MSWidget *owner_);
    ~Editor(void);
    virtual void up(void);
    virtual void down(void);
    virtual void tab(void);
    virtual void shiftTab(void);
    virtual void activate(void);
    virtual void escape(void);
  };

  class MSGUIExport Vsb : public MSVScrollBar
  {
  protected:
    virtual void change(void);
  public:
    Vsb(MSWidget *owner_);
    ~Vsb() {}
  };

  class MSGUIExport Hsb : public MSHScrollBar
  {
  protected:
    virtual void change(void);
  public:
    Hsb(MSWidget *owner_);
    ~Hsb() {}
  };

  class MSGUIExport CycleTimer : public MSIntervalTimer
  {
  protected:
    MSRowColumnView *_array;
  public:
    CycleTimer(MSRowColumnView *,unsigned long);
    ~CycleTimer(void);
    virtual void process(void);
  };

friend class Panner;
friend class Editor;
friend class Vsb;
friend class Hsb;
friend class CycleTimer;

public:
  int rows(void) const;
  void rows(int);

  int columns(void) const;
  void columns(int);
  
  int firstRow(void) const;
  void firstRow(int);
  
  int firstColumn(void) const;
  void firstColumn(int);

  int spacing(void) const;
  void spacing(int);

  void selectionMode(MSSelectionMode);
  MSSelectionMode selectionMode(void) const;
  
  const MSUnsignedLongVector& foregroundColors(void) const;
  void foregroundColors(const MSUnsignedLongVector&);
  void foregroundColors(const MSStringVector&);

  const MSUnsignedLongVector& backgroundColors(void) const;
  void backgroundColors(const MSUnsignedLongVector&);
  void backgroundColors(const MSStringVector&);

  const MSUnsignedLongVector& cycleColors(void) const;
  void cycleColors(const MSUnsignedLongVector&);
  void cycleColors(const MSStringVector&);

  MSCycleColorMode cycleColorMode(void) const;
  void cycleColorMode(MSCycleColorMode);

  unsigned long cycleInterval(void) const;
  void cycleInterval(unsigned long);

  int scrollBarSize(void) const;
  void scrollBarSize(int);

  int vsbSize(void) const;
  void vsbSize(int);

  int hsbSize(void) const;
  void hsbSize(int);

  MSBoolean isVsbEnabled(void) const;
  void showVsb(void);
  void hideVsb(void);

  MSBoolean isHsbEnabled(void) const;
  void showHsb(void);
  void hideHsb(void);
  
  unsigned long vsbBackground(void) const;
  void vsbBackground(unsigned long pixel_);
  void vsbBackground(const char *color_);
  
  unsigned long hsbBackground(void) const;
  void hsbBackground(unsigned long pixel_);
  void hsbBackground(const char *color_);

  unsigned long editorBackground(void) const;
  void editorBackground(unsigned long pixel_);
  void editorBackground(const char *color_);  

  unsigned long editorForeground(void) const;
  void editorForeground(unsigned long pixel_);
  void editorForeground(const char *color_);

  int selectedRow(void) const;
  void selectedRow(int);

  const MSIndexVector& selectionVector(void) const;
  void selectionVector(const MSIndexVector&);

  unsigned long selectedRowBackground(void) const;
  void selectedRowBackground(unsigned long pixel_); 
  void selectedRowBackground(const char *color_); 

  void rowDragDrop(MSBoolean rowDragDrop_);
  MSBoolean rowDragDrop(void) const;

  virtual void moveRow(int from_, int to_);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

  //The following are key press handlers
  virtual void pageUp(void);
  virtual void pageDown(void);
  virtual void home(void);
  virtual void end(void);
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);
  virtual void increment(void);
  virtual void decrement(void);
  virtual void tab(void);
  virtual void shiftTab(void);
  virtual void returnKey(void);
  virtual void backspace(void);
  virtual void doubleClick(void);
  virtual void insertAbove(void);
  virtual void insertBelow(void);
  virtual void deleteKey(void);
  virtual void edit(void);

  virtual MSBoolean editorActivate(void);
  MSBoolean editing(void) const;

  virtual void mapEditor(void);
  virtual void unmapEditor(void);
  
  virtual MSString selection(void);

  // Override some methods from MSWidget class
  virtual void redraw(void);
  virtual void print(const char *file_=0);
  virtual void naturalSize(void);
  virtual void unfreeze(void);

  unsigned long addEditorKeyCallback( const char*,MSKeyCallback*);
  void removeEditorKeyCallback(unsigned long);
  void removeEditorKeyCallback(const char*);
protected:
  enum {VsbEnabled=1,HsbEnabled=2};
  enum SizeState{Valid=0,Invalid=1,RowsValid=2,ColsValid=4,AdjustSize=8};

  MSRowColumnView(MSWidget *,const char *title_=0);
  MSRowColumnView(MSWidget *,const MSStringVector&);
  ~MSRowColumnView(void);

  //Attributes
  int                    _rows;
  int                    _columns;
  int                    _firstRow;
  int                    _firstColumn;
  int                    _spacing;
  int                    _selectedRow;
  MSSelectionMode        _selectionMode;
  MSIndexVector          _selectionVector;
  unsigned long          _rowBg;      // selected row background
  MSUnsignedLongVector   _foregroundColors;
  MSUnsignedLongVector   _backgroundColors;
  MSUnsignedLongVector   _cycleColors;
  MSCycleColorMode       _cycleMode;
  unsigned long          _cycleInterval;

//Data member to support implementation  
  Vsb                   *_vsb;
  Hsb                   *_hsb;
  Panner                *_panner;
  MSLabel               *_label;
  Editor                *_editor;
  MSBackingStorePixmap  *_redrawPixmap;
  int                    _naturalRows;
  int                    _naturalCols;
  int                    _rowSpacing;
  int                    _columnSpacing;
  unsigned long          _sizeState;       // does the size need to be adjusted
  unsigned long          _scrollBarState;  // are the scrollbars enabled or disabled
  MSUnsignedLongVector   _cycleList;
  CycleTimer            *_cycleTimer;
  MSGC                   _selectionVectorMSGC;
  int                    _rowHeight; 
  int                    _headingsHeight;
  int                    _lastBlock; 

  MSDisplayCursor       *_dragRowCursor;
  Window                 _dragRowWindow;
  MSBoolean              _rowDragDrop;

//Accessors to implementation data
  MSDisplayCursor *dragRowCursor(void) { return _dragRowCursor;}
  Window dragRowWindow(void)           { return _dragRowWindow;}

  Vsb *vsb(void) const;
  Hsb *hsb(void) const;
  Panner *panner(void) const;
  MSLabel *label(void) const;
  Editor *editor(void) const;
  MSBackingStorePixmap *redrawPixmap(void) const; 
  int naturalRows(void) const;
  int naturalCols(void) const;
  int rowSpacing(void) const;
  int columnSpacing(void) const;
  unsigned long sizeState(void) const;
  unsigned long scrollBarState(void) const;
  MSUnsignedLongVector &cycleList(void);
  CycleTimer *cycleTimer(void) const; 
  MSGC& selectionVectorMSGC(void);  
  GC selectionVectorGC(void) const; 

  void headingsHeight(int);
  virtual int headingsHeight(void);

  void rowHeight(int);
  virtual int rowHeight(void);

  void lastBlock(int block_);
  int lastBlock(void) const;

  virtual int clearSelection(void);

  void init(void);
  void initKeyTranslations(void);  
  void initHsb(void);
  void initVsb(void);

  virtual void redrawImmediately(void);
  virtual void configure(void);
  virtual void computeSize(void);
  virtual void placement(void);
  virtual void updateData(void);
  
  virtual void adjustSize(void);
  virtual void adjustView(void);        // for subclass to override
  virtual void adjustNumVisible(void);  // for subclass to override
  virtual void adjustFirstRow(void);    // for subclass to override
  virtual void adjustFirstColumn(void); // for subclass to override

  virtual void defaultNumVisible(void); // for subclass to override

  virtual int yToRow(int);
  virtual int inRowRange(int);
  virtual int rowFromEvent(const XEvent *);

  virtual int xToColumn(int);
  virtual int inColRange(int);
  virtual int columnFromEvent(const XEvent *);
    
  virtual int computeYCoord(int row_);

  void removeAllCycles(void);
  void removeCycle(MSColorCycle *);
  void startCycle(int,int,const MSUnsignedLongVector&,MSCycleColorMode);

  virtual void cycleCell(MSColorCycle *); // for subclass to override
  virtual void drawRow(int);              // for subclass to override
  virtual void drawRows(int,int);         // for subclass to override
  virtual void drawSelectedRow(int,MSBoolean);
  virtual void undrawSelectedRow(int);  

  virtual void moveEditorToSelection(const MSString &); // for subclass to override

  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);

  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void defaultButtonBehavior(const XEvent *);
  virtual void defaultDoubleClickBehavior(const XEvent *);
  virtual void defaultButton1Behavior(const XEvent *);
  virtual void defaultButton2Behavior(const XEvent *);
  virtual void defaultButton3Behavior(const XEvent *);

  virtual int dragRowFromEvent(const XEvent *);
  virtual void dragRow(const XEvent *);
  virtual void shuffleRow(int from_, int to_);


  virtual void startEditing(const XEvent *); // for subclass to override
  virtual void expose(const XEvent *);
  virtual void firstMapNotify(void);

  virtual void processCycleTimer(void);
  virtual int drawWidth(void);  // for subclass to override
  virtual int drawHeight(void); // for subclass to override

  virtual unsigned numColumns(void) const; // for subclass to override;
  virtual unsigned numRows(void) const;    // for subclass to override;
  virtual int lastColumn(void);
  virtual int lastRow(void);
  MSBoolean selected(unsigned);

  virtual void scrollUp(int count_);      
  virtual void scrollUp(int,int,MSBoolean drawNewRow_=MSTrue);
  virtual void scrollDown(int count_);    
  virtual void scrollDown(int,int,MSBoolean drawNewRow_=MSTrue);
  virtual void scrollRight(int);
  virtual void scrollLeft(int);
  virtual void postVerticalScrollDraw(int,int,MSBoolean);

  virtual void drawSelected(int);   // for subclass to override
  virtual void undrawSelected(int); // for subclass to override
  virtual void fillSelection(int,int);    
  virtual void unfillSelection(int,int);  

  // Events from scrollbars
  virtual void hsbValueUpdate(void); // for subclass to override
  virtual void vsbValueUpdate(void); // for subclass to override

  // Update scrollbars' views
  virtual void updateScrollBars(void);
  virtual void updateHsb(void); // for subclass to override
  virtual void updateVsb(void); // for subclass to override

  virtual void updateInternalState(void); // for subclass to override
  virtual void calculateRowHeight(void);
  virtual void calculateHeadingsHeight(void);

  // The following methods activate callbacks
  virtual void firstColumnChangeNotify(void);
  virtual void firstRowChangeNotify(void);  
  virtual void rowSelection(void);
  virtual void rowMovedNotify(void);
  
  // The following methods are called in response to public attribute changes
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  virtual void updateTitle(void);
  virtual void updateFirstRow(int);
  virtual void updateFirstColumn(int);
  virtual void updateSelectedRow(int);
  virtual void updateSelectionVector(const MSIndexVector&);
  virtual void updateSelectionMode(MSSelectionMode);
  
};

#ifndef MS_NO_INLINES
#ifndef MSRowColumnViewINLINES
#include <MSGUI/MSRowColumnViewInlines.C>
#endif
#endif

#endif
