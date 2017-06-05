#ifndef MSTableHEADER
#define MSTableHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSArrayView.H>
#include <MSGUI/MSReportTable.H>
#include <MSGUI/MSStringPopupMenu.H>
#include <MSTypes/MSGenericVector.H>
#include <MSTypes/MSManagedPointer.H>
#include <MSGUI/MSTableColumnGroup.H>
#include <MSTypes/MSStandardOps.H>

class MSStringPopupMenu;
class MSTableColumn;


class MSGUIExport MSTable : public MSReportTable, public MSArrayView
{
public:
  class MSGUIExport ColumnPopupMenu : public MSStringPopupMenu
  {
  protected:
    MSStringVector _choices;
    MSTable       *_table;
    
    virtual void activate(void);
  public:
    ColumnPopupMenu(MSTable *pTable_) :
    _table(pTable_),MSStringPopupMenu(pTable_->server())
    { model(_choices); }
    void choices(const MSStringVector& choices_)
    { _choices=choices_; }
  };
friend class ColumnPopupMenu;  
friend class MSTableColumn;

public:
  enum ChoiceStyle {ChoicePopupMenu, ChoiceOptionMenu, ChoiceOptionMenuAlwaysDrawn };

  MSTable(MSWidget *,const char *title_=0);
  MSTable(MSWidget *,const MSStringVector&);
  ~MSTable(void);

  // return column associated with tag_
  const MSTableColumn *tableColumn(const MSSymbol& tag_) const;
  MSTableColumn *tableColumn(const MSSymbol& tag_);

  // return column at column index_
  const MSTableColumn *tableColumn(unsigned index_) const;      
  MSTableColumn *tableColumn(unsigned index_);

  MSBoolean columnDragDrop(void) const;
  void columnDragDrop(MSBoolean columnDragDrop_);

  MSBoolean columnResize(void) const;
  void columnResize(MSBoolean);

  void choiceStyle(ChoiceStyle);
  ChoiceStyle choiceStyle(void) const;

  Font headingFont(void) const;
  void headingFont(Font);
  void headingFont(const char *);

  unsigned long headingAlignment(void) const;
  void headingAlignment(unsigned long);
  
  MSBoolean showBreaks(void) const; 
  void showBreaks(MSBoolean); 

  MSBoolean dynamicRecompute(void) const; 
  void dynamicRecompute(MSBoolean); 

  const MSIndexVector& viewVector(void) const;
  void viewVector(const MSIndexVector&);

  int selectedDataRow(void) const;
  void selectedDataRow(int);
  
  MSIndexVector selectionDataVector(void) const;
  void selectionDataVector(const MSIndexVector&);

  virtual MSString selection(void);

  virtual void computeBreaks(void);

  int columnCount(void) const;
  virtual unsigned numColumns(void) const;
  virtual unsigned numRows(void) const; 
  virtual unsigned numDataRows(void) const; 
  virtual int fixedReportColumns(void) const;
  virtual void exchangeColumns(unsigned x_,unsigned y__);
  virtual void placeColumnAt(MSTableColumn *,unsigned);
  virtual void permuteColumns(const MSIndexVector& aIndexVector_);
  virtual void permuteColumns(const MSSymbolVector& aSymbolVector_);  
  virtual void permuteColumns(const MSWidgetVector &);

  virtual void increment(void);
  virtual void decrement(void);

  virtual MSWidgetVector children(void);
  virtual MSDisplayServer *displayServer(void);
  virtual const MSString& virtualHelpString(int x_,int y_);
  
  virtual MSIndexVector sortUp(const MSIndexVector &);
  virtual MSIndexVector sortUp(const MSSymbolVector &);
  virtual MSIndexVector sortDown(const MSIndexVector &);
  virtual MSIndexVector sortDown(const MSSymbolVector &);

  void columnGroups(const MSTableColumnGroup &);
  MSTableColumnGroup columnGroups(void) const;

  MSWidgetVector columnsOfGroup(const MSSymbol &); 

  void groupForeground(const MSSymbol &,const char *);
  void groupForeground(const MSSymbol &,unsigned long); 
  unsigned long groupForeground(const MSSymbol &) const;

  void groupFont(const MSSymbol &,const char *);
  void groupFont(const MSSymbol &,Font);
  Font groupFont(const MSSymbol &) const;

  void groupHeading(const MSSymbol &,const MSStringVector &);
  const MSStringVector &groupHeading(const MSSymbol &) const;

  virtual void moveRow(int from_,int to_);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);
  
  typedef MSGenericVector<MSManagedPointer<MSTableColumnGroup> > ColumnGroupList;
protected:
  Font               _headingFont;
  unsigned long      _headingAlignment;
  MSBoolean          _columnDragDrop;
  MSBoolean          _columnResize;
  Window             _dragWindow;
  MSDisplayCursor   *_dragCursor;
  MSDisplayCursor   *_resizeCursor;
  MSGC               _movegc;
  MSIndexVector      _viewVector;
  ColumnPopupMenu   *_choicesMenu;
  MSBoolean          _showBreaks;
  MSBoolean          _dynamicRecompute;
  //_headingsHeight=_groupHeadingsHeight+_columnHeadingsHeight+extra space
  int                _columnHeadingsHeight;
  int                _groupHeadingsHeight;
  MSIntVector        _groupHeadingsHeightVector;
  ColumnGroupList    _columnGroupList;
  ChoiceStyle        _choiceStyle;
  
  void init(void);
  GC moveGC(void);
  Window dragWindow(void);
  MSDisplayCursor *dragCursor(void);
  MSDisplayCursor *resizeCursor(void);

  void updateBreakStatus(unsigned,unsigned);
  virtual void updateInternalState(void);
  virtual void calculateHeadingsHeight(void);

  virtual void appendUpdate(void);
  virtual void columnUpdate(int);
  virtual void updateScreen();

  virtual void createCycle(int,int);
  virtual void drawHeadings(Window);
  virtual void drawHeadings(Window,int,int);
  virtual void drawFixedColumnHeadings(Window);

  virtual void clearColumn(int);

  virtual void dragColumn(const XEvent *,int);
  virtual void shuffleColumns(MSIndexVector &);
  virtual void resizeColumn(int,int);
  MSBoolean insideSeparator(int,int,int&);

  virtual MSBoolean editorActivate(void);
  virtual void valueChange(void);
  virtual void columnChoiceActivate(const MSString&);
  
  virtual unsigned numRows(unsigned) const; 
  virtual int columnPixelWidth(int);
  virtual unsigned columnLength(unsigned);

  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void updateTitle(void);

  virtual void returnKey(void);

  virtual void visibilityUnobscured(void);
  virtual void visibilityObscured(void);
  virtual void defaultButton3Behavior(const XEvent *);  
  virtual void motionNotify(const XEvent *);

  virtual void headingAreaSelection(const XEvent *);
  virtual void dataAreaSelection(const XEvent *);

  virtual void insertChild(MSWidget *);
  virtual void removeChild(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childRemove(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childDestroy(MSWidget *);

  virtual unsigned long convertForeground(const char *);

  // getDataRow() returns the corresponding model-specific row of the specified
  // view-specific row.  isBreakRow_ returns MSTrue if the viewRow_ is indeed a
  // break row, and MSFalse otherwise.  This method calls adjustRowForBreaks()
  // and adjustRowForViewVector().
  unsigned getDataRow(unsigned viewRow_,MSBoolean &isBreakRow_) const;

  unsigned adjustRowForViewVector(unsigned) const;
  unsigned adjustRowForBreaks(unsigned) const;
  unsigned adjustPositionForBreaks(unsigned) const;
  
  // The following methods override the default provided by MSArray because MSTable
  // supports column semantics where each column can have its own color, font, 
  // number of rows, alignment, etc.
  virtual const XFontStruct *columnFontStruct(unsigned);
  virtual unsigned long columnBackground(unsigned);
  virtual unsigned long columnForeground(unsigned);
  virtual MSClipMode columnClipMode(unsigned) const;
  virtual unsigned columnEditWidth(unsigned);
  virtual unsigned columnNumRows(unsigned) const;

  // The following methods override the default provided by MSArray because MSTable
  // supports extra view manipulation functionalities such as view vector and breaks.
  virtual const char *viewFormatOutput(MSString &,unsigned,unsigned);
  virtual MSBoolean viewValidate(const char *,unsigned,unsigned);
  virtual MSBoolean isViewProtected(unsigned,unsigned);
  virtual MSBoolean isViewValid(unsigned,unsigned);
  virtual unsigned long viewCellForeground(unsigned,unsigned);
  virtual unsigned long viewCellBackground(unsigned,unsigned);
  virtual Font viewCellFont(unsigned,unsigned);
  virtual MSAlignment viewCellAlignment(unsigned,unsigned); 

  // The following methods override the default provided by MSArray because MSTable
  // forwards them to the corresponding table column object.
  virtual const char *formatOutput(MSString &,unsigned,unsigned);
  virtual MSBoolean validate(const char *,unsigned,unsigned);
  virtual MSBoolean isCellProtected(unsigned,unsigned);
  virtual MSBoolean isValid(unsigned,unsigned);
  virtual unsigned long cellForeground(unsigned,unsigned);
  virtual unsigned long cellBackground(unsigned,unsigned);
  virtual Font cellFont(unsigned,unsigned);
  virtual MSAlignment cellAlignment(unsigned,unsigned);

  //Methods that support column grouping
  MSIntVector &groupHeadingsHeightVector(void);
  const MSIntVector &groupHeadingsHeightVector(void) const;

  ColumnGroupList &columnGroupList(void);
  const ColumnGroupList &columnGroupList(void) const;

  int columnHeadingsHeight(void) const;
  void columnHeadingsHeight(int);

  int groupHeadingsHeight(void) const;
  void groupHeadingsHeight(int);

  virtual void calculateColumnHeadingsHeight(void);
  virtual void calculateGroupHeadingsHeight(void);
  virtual void calculateGroupHeadingsHeight(MSTableColumn *,MSIntVector &);
  virtual void calculateRowHeight(void);

  virtual void drawGroupHeadings(Window);
  virtual void drawGroupHeadings(Window,int,int);
  virtual void drawGroupHeading(Window,MSTableColumnGroup *,int,int,int);
  virtual void drawGroupHeading(Window,MSTableColumnGroup *,MSRect&);

  virtual void drawCell(Window,int,int,int,int);
  virtual void drawCell(Window,int,int,int,int,unsigned long,unsigned long,Font,
                        MSBoolean,MSBoolean,MSBoolean);

  virtual void updateColumnHeadings(void);
  virtual void drawColumnHeadings(Window,int,int);
  virtual void drawColumnHeading(Window,MSTableColumn *,const MSRect&,int);
 
  virtual int computeXCoord(int column_);
  virtual int computeXCoord(MSTableColumn *);

  virtual int columnHeadingsOffset(void);
  virtual void redrawHeadings(Window,int,int);
  virtual void drawHSeparator(Window,int,int,int,int);
  virtual void drawVSeparator(Window,int,int,int,int);
  MSBoolean sanityCheck(const MSTableColumnGroup &) const;

  virtual void updateChoices(void);
  virtual void showOptions(int row_,int column_);
  virtual MSBoolean columnHasOptions(int column_);
  virtual ChoiceStyle columnChoiceStyle(int column_);
  virtual const MSStringVector &cellOptions(int row_,int column_);
  virtual void cellRootXY(int row_,int column_,int &x_,int &y_);
  
  virtual void dragRow(const XEvent *);
  void resetSortingArrows(void);

  
  friend ostream &operator<<(ostream &,MSManagedPointer<MSTableColumnGroup> const &);
};

DECLARE_ELEMENT_FOR_OPS(MSManagedPointer,MSTableColumnGroup)

#ifndef MS_NO_INLINES
#ifndef MSTableINLINES
#include <MSGUI/MSTableInlines.C>
#endif
#endif

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSGenericVector<MSManagedPointer<MSTableColumnGroup> >
#pragma do_not_instantiate MSGenericVectorOps<MSManagedPointer<MSTableColumnGroup> >
#pragma do_not_instantiate MSGenericData<MSManagedPointer<MSTableColumnGroup> >
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
