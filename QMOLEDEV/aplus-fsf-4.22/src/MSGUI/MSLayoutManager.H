#ifndef MSLayoutManagerHEADER
#define MSLayoutManagerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSManager.H>

class MSGUIExport MSLayoutEntry 
{
private:
  MSWidget  *_widget;
  MSBoolean  _mapped;
  At         _at;

public:
  MSLayoutEntry(MSWidget *widget_);
  ~MSLayoutEntry(void);

  MSWidget *widget(void) const;
  MSBoolean mapped(void) const;
  At& at(void);
  const At& at(void) const;

  void mapped(MSBoolean b_);
};

class MSGUIExport MSLayoutVector
{
public:
  unsigned long _mask; 	// Option mask  
  int           _value;	// Size of item

  MSLayoutVector(void);
  ~MSLayoutVector(void);

  unsigned long mask(void) const;
  int value(void) const;

  void mask(unsigned long mask_);
  void value(int value_);
};

typedef void (*ValueFunc) (MSLayoutEntry *,int *,int *,MSBoolean *);
typedef int (*SizeFunc) (MSWidget *);

class MSGUIExport MSLayoutManager : public MSManager
{
public:
  enum Orientation {Vertical=0,Horizontal=1,Unspecified=2};
  enum LayoutStyle {RowMajor=0,ColumnMajor=1};

  MSLayoutManager(MSWidget *);
  ~MSLayoutManager(void);

  MSLayoutManager::Orientation orientation(void) const;
  void orientation(MSLayoutManager::Orientation);
  MSLayoutManager::LayoutStyle layoutStyle(void) const;
  void layoutStyle(MSLayoutManager::LayoutStyle);

  const MSIndexVector& geometry(void) const;
  void geometry(const MSIndexVector&);

  int rows(void) const;
  int columns(void) const;
  int rowSpacing(void) const;
  int columnSpacing(void) const;
  int margin(void) const;
  MSBoolean uniformColumns(void) const;
  MSBoolean uniformRows(void) const;
  MSBoolean lockSize(void) const;
  MSBoolean lockPositions(void) const;
  
  int row(MSWidget *) const;
  int column(MSWidget *) const;
  int columnSpan(MSWidget *) const;
  int rowSpan(MSWidget *) const;
  unsigned long options(MSWidget *) const;

  void rowSpacing(int);
  void columnSpacing(int);
  void margin(int);
  void uniformColumns(MSBoolean); // should the cols be uniform ?
  void uniformRows(MSBoolean);    // should the rows be unifowm ?
  void lockSize(MSBoolean);
  void lockPositions(MSBoolean);

  virtual void print(const char *file_=0);
  virtual void unfreeze(void);
  virtual void resize(int,int);
  virtual void map(void);
  virtual void show(void);

  virtual MSWidgetVector children(void);

  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  enum VectorState {Invalid,Minimum,Internal};

  MSIndexVector                *_geometry;
  MSLayoutManager::Orientation  _orientation;
  MSLayoutManager::LayoutStyle  _layoutStyle;
  MSNodeItem                    _mappedListHead; // list of mapped children
  int                           _innerWidth;     // inner horizontal padding        
  int	                        _innerHeight;    // inner vertical padding          
  int	                        _rowSpacing;     // space between rows              
  int	                        _columnSpacing;  // space between columns           
  int                           _margin;         // margin around layout 

  MSNodeItem *mappedListHead(void);

  virtual void computeSize(void);
  virtual void adjustSize(void);
  virtual int findDistribution(int loc_,int span_,MSLayoutVector *vec_,int *result_, MSBoolean ignoreZeros_);
  virtual void doDistribution(int n_,int *distrib_,int loc_,int span_,
                              MSLayoutVector *vec_,int size_,int spacing_);
  int computeVector(ValueFunc valueFunc,SizeFunc sizeFunc,int spacing_,MSLayoutVector *result_);
  void recomputeVectors(void);
  void setVectorOptions(ValueFunc valueFunc,MSLayoutVector *vec_);
  int vectorSize(ValueFunc valueFunc);
  virtual void extraSpace(int num_,MSLayoutVector *vec_,int size_,MSBoolean uniform_);
  virtual void extraRowSpace(int num_,MSLayoutVector *vec_,int size_);
  virtual void extraColumnSpace(int num_,MSLayoutVector *vec_,int size_);
  void placeWidget(MSWidget *,int x_,int y_,int w_,int h_,int rw_,int rh_,unsigned long opts_);
  void doPlacement(MSLayoutVector *cvec_,MSLayoutVector *rvec_,int vp_,int hp_,int rs,int cs);

  virtual void placement(void);
  virtual void insertChild(MSWidget *);
  virtual void removeChild(MSWidget *);

  MSBoolean placementFlag(void) const;
  void placementFlag(MSBoolean);

  MSLayoutEntry *getEntry(MSWidget *);
  const MSLayoutEntry *getEntry(MSWidget *) const;
  MSLayoutEntry *getEntry(int row_,int col_);

  virtual void updateOrientation(void);
  virtual MSBoolean setPositions(void);
  virtual MSBoolean setDefaultPositions(void);
  virtual void setDefaultChildPosition(MSLayoutEntry *);
  virtual void insertMappedEntry(MSLayoutEntry *);
  virtual void removeMappedEntry(MSLayoutEntry *);
  virtual MSLayoutEntry *findMappedEntry(MSWidget *);

  virtual At childPosition(MSWidget *);
  virtual void childPosition(MSWidget *widget_,const At& at_);

  virtual int realWidth(void) const;
  virtual int realHeight(void) const;
  virtual int idealWidth(void) const;
  virtual int idealHeight(void) const;
  virtual int innerHeight(void) const;
  virtual int innerWidth(void) const;
  int vectorHeight(void) const;
  int vectorWidth(void) const;

  virtual int rowHeight(unsigned row_) const;
  virtual int columnWidth(unsigned col_) const;

  virtual void printChildInfo(void); // for debugging
  virtual void configure(void);
  virtual void firstMapNotify(void);
  virtual void childConfigure(MSWidget *);
  virtual void childResizeConstraints(MSWidget *);
  virtual void childMap(MSWidget *);
  virtual void childUnmap(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childRemove(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childDestroy(MSWidget *);
  virtual void visibilityObscured(void);
  virtual void visibilityUnobscured(void);

  // For ArrowKey Traversal
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);

private:
  enum StateFlags 
   {
     UniformColumns=1,  // are the cols to be made uniform?
     UniformRows=2,     // are the rows to be made uniform?
     LockSize=4,        // do not recalc size on unfreeze
     LockPositions=8,   // do not reset row and col sizes
     PlacementFlag=16   // set when layout is configuring children
   };

  VectorState     _vectorState;    // current state of vectors         
  int		  _rows;           // number of rows                   
  int 	          _columns;        // number of columns                
  int	          _vectorHeight;   // sum of current rows              
  int		  _vectorWidth;    // sum of current columns 
  MSLayoutVector *_rowHeights;     // heights of each row              
  MSLayoutVector *_columnWidths;   // widths of each column            
  unsigned long   _stateFlag;      // holds the current state of the layout

  void init(void);
  unsigned long stateFlag(void) const; 

  // Private methods used by ArrowKey Traversal code
  MSLayoutEntry *getNextVerticalEntry(MSLayoutEntry *);
  MSLayoutEntry *getPrevVerticalEntry(MSLayoutEntry *);
  MSLayoutEntry *getNextHorizontalEntry(MSLayoutEntry *);
  MSLayoutEntry *getPrevHorizontalEntry(MSLayoutEntry *);

};

#endif

















