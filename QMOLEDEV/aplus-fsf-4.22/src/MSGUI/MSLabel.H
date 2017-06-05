#ifndef MSLabelHEADER
#define MSLabelHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSPrimitiveText.H>
#include <MSGUI/MSPixmap.H>

class MSGUIExport MSLabel : public MSPrimitiveText
{
public:
  MSLabel(MSWidget *,const char *label_=0);
  MSLabel(MSWidget *,const MSStringVector&);
  MSLabel(MSWidget *,MSStringVector&);
  MSLabel(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_);
  ~MSLabel(void);

  MSStringVector& label(void)             const {return (MSStringVector&)*_model;}
  int margin(void)                        const {return _margin;}
  int rows(void)                          const {return numRows();}
  int columns(void)                       const {return numColumns();}
  const MSPixmap *pixmap(void)            const {return _pixmap;}
  const MSPixmap *insensitivePixmap(void) const {return _insensitivePixmap;}

  void margin(int);
  void model(MSStringVector&);

  MSStringVector& viewModel(void)             { return *((MSStringVector*)_model); }
  const MSStringVector& viewModel(void) const { return *((MSStringVector*)_model); }
  void label(const MSStringVector&);
  void pixmap(const MSPixmap &);
  void insensitivePixmap(const MSPixmap&);

  void displayToolTip(MSBoolean);
  MSBoolean displayToolTip(void) const {return _displayToolTip;}

  void toolTip(const MSStringVector&);
  const MSStringVector &toolTip(void) const {return tip();}
  
  virtual void redraw(void);
  virtual void naturalSize(void);

  MSLabel& operator=(const MSStringVector& label_) {label(label_); return *this;}
  operator MSStringVector&()                       {return *((MSStringVector *)_model);}

  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);
protected:
  GC        _pixmapGC;
  int       _margin;
  MSPixmap *_pixmap;
  MSPixmap *_insensitivePixmap;
  MSBoolean _displayToolTip;
  
  void init(void);
  void createDefaultPixmap(int,int,unsigned long,unsigned long);
  void createDefaultInsensitivePixmap(int,int,unsigned long,unsigned long);
  void setClipRegion(void);
  GC pixmapGC(void);
  
  virtual void computeSize(void);
  virtual void firstMapNotify(void);
  virtual void configure(void);

  virtual void drawPixmap(void);
  virtual void drawLabel(void);
  virtual void clearRow(int);
  virtual void drawRow(int);
  virtual void drawRow(int,int,const char *,int);
  virtual void drawRows(int,int);

  virtual int computeYCoord(int);
  virtual int computeXCoord(int,int,const char *,int);
  virtual int computePixmapXCoord(const MSPixmap *);
  virtual int computePixmapYCoord(const MSPixmap *);

  // virtual methods for subclassers to override
  virtual int numRows(void) const;
  virtual int numColumns(void) const;
  virtual int numColumns(int row_) const;
  virtual const char *string(int row_) const;
  virtual const char *formatOutput(MSString &,int row_);

  int maxPixelWidth(void);
  int drawWidth(void) const;
  int drawHeight(void) const;

  virtual void update(const MSIndexVector&);
  virtual void updateData(void);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  virtual void updateSensitivity(void);
  virtual void updatePixmap(void);

  virtual void enterNotify(const XEvent *);
  virtual void leaveNotify(const XEvent *);
  virtual void toolTipXY(int &x,int &y);
  virtual const MSStringVector &tip(void) const;
private:
  MSBoolean _resetClipRegion;
  int       _numberOfRows; // this is used to determine if the number of rows has changed
                           // for any given update - i.e. it is a cache of numRows 

  int numberOfRows(void)          const {return _numberOfRows;}
  void numberOfRows(int x_)             {_numberOfRows=x_;}
  MSBoolean resetClipRegion(void) const {return _resetClipRegion;}
  void resetClipRegion(MSBoolean x_)    {_resetClipRegion=x_;}
};

#endif
