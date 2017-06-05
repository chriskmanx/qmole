#ifndef MSArrayViewHEADER
#define MSArrayViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSRowColumnView.H>

class MSGUIExport MSArrayView : public MSRowColumnView
{
public:
  MSArrayView(MSWidget *,const char *title_=0);
  MSArrayView(MSWidget *,const MSStringVector&);
  ~MSArrayView(void);

  int fixedColumns(void) const;
  void fixedColumns(int);
  
  int rowSeparator(void) const;
  void rowSeparator(int);

  int columnSeparator(void) const;
  void columnSeparator(int);

  int columnWidth(void) const;
  void columnWidth(int); 

  void selectedRowColumn(int,int);  
  
  int selectedColumn(void) const;
  void selectedColumn(int);  
  
  unsigned long selectedCellBackground(void) const;
  void selectedCellBackground(unsigned long pixel_); 
  void selectedCellBackground(const char *color_);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

  virtual void left(void);
  virtual void right(void);
  virtual void tab(void);
  virtual void shiftTab(void);
  virtual void doubleClick(void);

  virtual MSBoolean editorActivate(void);

  virtual MSString selection(void);

protected:
  MSPixmap              *_stipple;
  //Attributes
  int                    _fixedColumns;
  int                    _rowSeparator;
  int                    _columnSeparator;
  int                    _columnWidth;
  int                    _selectedColumn;
  unsigned long          _selectBg;   // selected cell background

  void init(void);

  int computeMaxTextLength(const XFontStruct *,const char *,int,int);
  
  virtual void updateView(void);
  virtual void adjustView(void);
  virtual void updateHsb(void);
  virtual void updateVsb(void);
  virtual void redrawImmediately(void);

  virtual void adjustNumVisible(void);
  virtual void adjustFirstColumn(void);
  virtual void adjustSelection(void);
  virtual void defaultNumVisible(void);
  virtual int computeNumVisibleColumns(void);
  virtual int computeNumVisibleRows(void);  
  virtual int computeXCoord(int column_);
  virtual void shapeUpdate(void);
  virtual void appendUpdate(void);

  virtual void createCycle(int,int);

  virtual void cycleRowColumn(int,int);
  virtual void cycleRow(int);
  virtual void cycleColumn(int);
  virtual void cycleCell(MSColorCycle *);

  virtual void drawCycle(MSColorCycle *);
  virtual void drawCycle(int,int,int,int,unsigned long,MSCycleColorMode);
  
  virtual void drawRowColumn(int,int);
  virtual void drawColumn(int); 
  virtual void drawRow(int);
  virtual void drawRow(Window,int);
  virtual void drawRows(int,int);
  virtual void drawRows(Window,int,int);
  virtual void drawRows(int,int,int,int);
  virtual void drawRows(Window,int,int,int,int);

  virtual void drawCell(Window,int,int,int,int);
  virtual void drawCell(Window,int,int,int,int,unsigned long,unsigned long,Font,
                        MSBoolean,MSBoolean,MSBoolean);

  virtual void clearRowColumn(int,int);
  virtual void clearRows(int,int);
  virtual void clearRows(int,int,int,int);
  virtual void clearRow(int);

  virtual void drawHeadings(Window);
  virtual void drawHeadings(Window,int,int);

  virtual void drawLabels(int,int);
  virtual void drawLabels(Window,int,int);

  virtual void drawHSeparators(Window,int,int,int,int);
  virtual void drawVSeparators(Window,int,int,int,int);
  virtual void cleanUpBottom(Window);
  virtual void cleanUpRight(Window);
  virtual void cleanUpDrawArea(Window);

  virtual void drawSelectBevel(MSRect&,MSBoolean);

  virtual void drawSelectedCell(Window,int,int,MSBoolean);
  virtual void drawSelectedCell(Window,int,int);
  virtual void undrawSelectedCell(Window,int,int);

  virtual void drawSelectedRow(Window,int,MSBoolean);
  virtual void drawSelectedRow(Window,int);
  virtual void drawSelectedRow(int);
  virtual void drawSelectedRow(int,MSBoolean);
  virtual void undrawSelectedRow(int);
  virtual void undrawSelectedRow(Window,int);

  virtual void setSelection(int,int);
  virtual void moveSelection(int,int);
  virtual void moveEditorToSelection(const MSString &);

  virtual int xToColumn(int);
  virtual int inColRange(int column_);
  virtual int columnFromEvent(const XEvent *);

  MSBoolean needRowSep(int row_); 
  MSBoolean needColSep(int column_);

  virtual void defaultButtonBehavior(const XEvent *);

  virtual void headingLabelAreaSelection(const XEvent *);
  virtual void headingAreaSelection(const XEvent *);
  virtual void labelAreaSelection(const XEvent *);
  virtual void dataAreaSelection(const XEvent *);
  
  virtual void startEditing(const XEvent *);
  virtual void focusIn(void); 
  virtual void focusOut(void);
  virtual MSBoolean loseFocus(void);

  virtual void rowColumnSelection(void);
  virtual void labelSelection(void);
  virtual void columnSelection(void);
  virtual void columnMenuButtonSelection(void);

  virtual int labelWidth(void);
  virtual int drawWidth(void);
  virtual int drawHeight(void);

  virtual int fixedColumnPixelWidth(void);
  virtual int columnPixelWidth(int);
  virtual unsigned columnLength(unsigned);
  virtual int lastColumn(void);

  virtual void scrollRight(int);
  virtual void scrollLeft(int);
  virtual void postVerticalScrollDraw(int,int,MSBoolean);

  virtual void drawSelected(int);         
  virtual void undrawSelected(int);       
  virtual void trackSelection(int,int,MSBoolean clear_,MSBoolean shifted_);
  virtual void trackUnselection(int,int);  

  virtual void hsbValueUpdate(void);
  virtual void vsbValueUpdate(void);

  virtual void update(const MSIndexVector&);

  virtual void updateInternalState(void);
  virtual void calculateRowHeight(void);
  virtual void drawFixedColumns(Window,int,int);

  // Added to support column grouping
  virtual int columnHeadingsOffset(void);
  virtual void redrawHeadings(Window,int,int);

  // The following methods are called in response to public attribute changes
  virtual void updateFirstRow(int);
  virtual void updateFirstColumn(int);
  virtual void updateSelectionVector(const MSIndexVector&);
  virtual void updateSelectedRow(int);
  virtual void updateSelectedColumn(int);
  virtual void updateSensitivity(void);

  // The following methods are meant to be overriden by subclasses with
  // non-homogeneous columns
  virtual const XFontStruct *columnFontStruct(unsigned);
  virtual unsigned long columnBackground(unsigned);
  virtual unsigned long columnForeground(unsigned);
  virtual MSClipMode columnClipMode(unsigned) const;
  virtual unsigned columnEditWidth(unsigned);
  virtual unsigned columnNumRows(unsigned) const;

  // The following methods return the appropriate values for drawing.
  // The row and column parameters passed are based on the view.
  // These methods take care of the mapping from row/column indices
  // of the view to the indices of the model.
  // Subclasses should not have to overide these methods.  
  virtual const char *viewFormatOutput(MSString &,unsigned,unsigned);
  virtual MSBoolean viewValidate(const char *,unsigned,unsigned);
  virtual MSBoolean isViewProtected(unsigned,unsigned);
  virtual MSBoolean isViewValid(unsigned,unsigned);
  virtual unsigned long viewCellForeground(unsigned,unsigned);
  virtual unsigned long viewCellBackground(unsigned,unsigned);
  virtual Font viewCellFont(unsigned,unsigned);
  virtual MSAlignment viewCellAlignment(unsigned,unsigned); 
 
  // The following methods return the appropriate values for drawing.
  // The row and column parameters passed are based on the model.
  // Subclasses may override these methods to fine tune their displays. 
  virtual const char *formatOutput(MSString &,unsigned,unsigned);
  virtual MSBoolean validate(const char *,unsigned,unsigned);

  virtual MSBoolean isValid(unsigned,unsigned);
  virtual unsigned long cellForeground(unsigned,unsigned);
  virtual unsigned long cellBackground(unsigned,unsigned);
  virtual Font cellFont(unsigned,unsigned);
  virtual MSAlignment cellAlignment(unsigned,unsigned); 

  virtual MSBoolean isCellProtected(unsigned,unsigned);

  //For backward compatibility only.
  virtual MSBoolean isProtected(unsigned,unsigned);
  virtual MSBoolean isProtected(void) const;
};

#ifndef MS_NO_INLINES
#ifndef MSArrayViewINLINES
#include <MSGUI/MSArrayViewInlines.C>
#endif
#endif

#endif
