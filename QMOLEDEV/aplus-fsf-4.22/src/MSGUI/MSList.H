#ifndef MSListHEADER
#define MSListHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSRowColumnView.H>
#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSGenericVector.H>

#ifdef MS_VC_NEED_TEMPLATE_EXPORT
class MSGUIExport MSGenericVector<MSPixmap *>;
#endif

class MSGUIExport MSList : public MSRowColumnView
{
public:
  MSList(MSWidget *,const char *title_=0);
  MSList(MSWidget *,const MSStringVector&);
  ~MSList(void);

  void registerPixmap(const MSPixmap &);
  void unregisterPixmap(const MSString&);

  const MSPixmap* pixmap(const MSString&);

  void pixmapList(const MSStringVector &);
  MSStringVector pixmapList(void) const;
  
  virtual void doubleClick(void);
  virtual void right(void);

  virtual MSBoolean editorActivate(void);

  virtual MSString selection(void);

  void selectedRowForeground(const char *);
  void selectedRowForeground(unsigned long);
  unsigned long selectedRowForeground(void) const;

  virtual void up(void);
  virtual void down(void);

  virtual void moveRow(int,int);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

  
protected:
  class PixmapRegistry : public MSHashTable
  {
  protected:
    int _maxPixmapWidth;
    int _maxPixmapHeight;
  public:
    PixmapRegistry(unsigned size_=16);
    ~PixmapRegistry(void);

    void add(const MSPixmap &);
    void remove(const MSString&);
    int maxPixmapWidth(void) const {return _maxPixmapWidth;}
    int maxPixmapHeight(void) const {return _maxPixmapHeight;}
  };
  typedef MSGenericVector<MSPixmap *> PixmapVector;
  
  PixmapRegistry   _pixmapRegistry;
  PixmapVector     _pixmapVector;
  unsigned         _maxLength;
  GC               _pixmapGC;
  unsigned long    _selectedRowForeground;
  

  MSString& isearchString(void);
  MSIndexVector& isearchVector(void);
  unsigned maxLength(void) const {return _maxLength;}
  PixmapRegistry &pixmapRegistry(void) {return _pixmapRegistry;}
  const PixmapRegistry &pixmapRegistry(void) const {return _pixmapRegistry;}
  PixmapVector &pixmapVector(void) {return _pixmapVector;}
  const PixmapVector &pixmapVector(void) const {return _pixmapVector;}
  
  void init(void);

  virtual int clearSelection(void);
  
  virtual void updateInternalState(void);
  virtual void startEditing(const XEvent *);
  virtual void moveEditorToSelection(const MSString &);
  virtual void updateHsb(void);
  virtual int computeNumVisibleColumns(void);
  virtual unsigned rowLength(unsigned) const;
  virtual int drawWidth(void);

  virtual void redrawImmediately(void);
  virtual void computeSize(void);
  virtual void placement(void);
  virtual void defaultNumVisible(void);
  virtual void adjustNumVisible(void);
  virtual void adjustFirstColumn(void);
  virtual void shapeUpdate(void);
  virtual void appendUpdate(const MSIndexVector &);

  virtual void drawRow(int);
  virtual void drawRows(int,int);
  virtual void drawRow(Window,int,const char *,int len_); 
  virtual void drawRows(Window,int,int);

  virtual void drawActualRow(Window,int row_,const char *string_,int len_,
                             const MSPixmap *,unsigned long fg_,unsigned long bg_,
                             Font font_,MSBoolean selected_,MSBoolean selectOutline_);
  virtual void drawString(Display *,Window,GC,const XFontStruct *,int,int,const char *,int);
  
  virtual void drawSelectOutline(Window,int,MSBoolean);
  virtual void drawSelectedRow(Window,int,MSBoolean);
  virtual void drawSelectedRow(Window,int);
  virtual void drawSelectedRow(int,MSBoolean);

  virtual void drawCycle(MSColorCycle *cycle_);
  virtual void drawCycle(int row_,unsigned long);

  virtual void defaultButtonBehavior(const XEvent *);
  virtual void defaultButton2Behavior(const XEvent *);
  virtual void headingAreaSelection(const XEvent *);
  virtual void dataAreaSelection(const XEvent *);
  
  virtual int dragRowFromEvent(const XEvent *);

  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void incrementalSearch(unsigned row_=0);
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual MSBoolean loseFocus(void);
  virtual void update(const MSIndexVector&);
  
  virtual void trackSelection(int,int,MSBoolean clear_,MSBoolean shifted_);
  virtual void trackUnselection(int,int);  
  virtual void drawSelected(int);    //added for Multiple Select
  virtual void undrawSelected(int);  //added for Multiple Select

  virtual void edit(void);

  virtual int computeNumVisibleRows(void);  
  virtual void calculateRowHeight(void);

  virtual void rowColumnSelection(void);
  virtual void undrawSelectedRow(int row_) { MSRowColumnView::undrawSelectedRow(row_); }
  virtual void undrawSelectedRow(Window,int);
  virtual int drawHeight(void);

  virtual void scrollRight(int);
  virtual void updateScrollBars(void);
  virtual void updateVsb(void);

  virtual void hsbValueUpdate(void);
  virtual void vsbValueUpdate(void);
  virtual void updateSelectedRow(int);
  virtual void updateSelectionVector(const MSIndexVector &);
  virtual void updateFirstRow(int);
  virtual void updateFirstColumn(int);

  virtual void cycleCell(MSColorCycle *);
  virtual void moveSelection(int);
  virtual void cycleRow(int);
  virtual void createCycle(int);

  virtual void adjustSelection(void);
  virtual void calculateMaxLength(void);

  virtual const MSPixmap *rowPixmap(unsigned);
  virtual int maxPixmapHeight(void);
  virtual int maxPixmapWidth(void);

  int actualNumColumns(void);
  virtual int numPixmapColumns(void);
  GC pixmapGC(void);
  
  virtual const char *formatOutput(MSString &,unsigned);
  virtual unsigned long rowForeground(unsigned);
  virtual unsigned long rowBackground(unsigned);
  virtual Font rowFont(unsigned);
  virtual MSBoolean isRowProtected(unsigned);
  virtual MSBoolean validate(const char *,unsigned);

  virtual unsigned long selectedRowForegroundColor(int);
  virtual unsigned long selectedRowBackgroundColor(int);
  
  virtual void toggleRowNotify(unsigned);
  virtual void toggleRow(unsigned);
  virtual void toggleModeMoveRow(int, int);

  virtual void updateSelectionMode(MSSelectionMode);
  virtual void updateBackground(unsigned long);


  //for backward compatibility only.
  virtual MSBoolean isProtected(void) const;
  virtual MSBoolean isProtected(unsigned);
private:
  MSString      _isearchString;
  MSIndexVector _isearchVector;
};

inline MSString& MSList::isearchString(void)    
{ return _isearchString; }  
inline MSIndexVector& MSList::isearchVector(void) 
{ return _isearchVector; }  
inline unsigned long MSList::selectedRowForeground(void) const
{ return _selectedRowForeground; }

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSGenericVector<MSPixmap *>
#pragma do_not_instantiate MSGenericVectorOps<MSPixmap *>
#pragma do_not_instantiate MSGenericData<MSPixmap *>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

DECLARE_ELEMENT_FOR_OPS_TYPE(MSPixmap *)

#endif
