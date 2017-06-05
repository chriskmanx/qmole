#ifndef MSIconHEADER
#define MSIconHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSLabel.H>
#include <MSGUI/MSDoubleClick.H>

class MSGUIExport MSIcon : public MSLabel
{
protected:
  int           _labelSpacing;
  MSAlignment   _labelAlignment;
  MSDoubleClick _doubleClick;
  MSBoolean     _showLabel;
  MSBoolean     _showPixmap;
  
  MSDoubleClick &doubleClick(void) {return _doubleClick;}

  int labelWidth(void);
  int labelHeight(void);
  int pixmapWidth(void);
  int pixmapHeight(void);
  int computeXCoordinate(int,int,int,const char *,int);
  int computeYCoordinate(int,int);

  virtual void computeSize(void);
  virtual int computeXCoord(int,int,const char *,int);
  virtual int computeYCoord(int);
  virtual int computePixmapXCoord(const MSPixmap *);
  virtual int computePixmapYCoord(const MSPixmap *);
  virtual int computeLabelSpacing(void);

  virtual void button1Press(const XEvent *);
  virtual void buttonSelectionNotify(void);
  virtual void doubleClickNotify(void);

  void init(void);
  void init(const MSPixmap &pixmap_,const MSPixmap &nsensitivePixmap_);

public:
  MSIcon(MSWidget *,const char *label_,const MSPixmap &pixmap_,const MSPixmap &nsensitivePixmap_);
  MSIcon(MSWidget *,const MSStringVector&,const MSPixmap &pixmap_,const MSPixmap &nsensitivePixmap_);
  MSIcon(MSWidget *,MSStringVector&,const MSPixmap &pixmap_,const MSPixmap &nsensitivePixmap_);
  MSIcon(MSWidget *,const char *label_=0);
  MSIcon(MSWidget *,const MSStringVector&);
  MSIcon(MSWidget *,MSStringVector&);
  MSIcon(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_);
  ~MSIcon(void);

  int labelSpacing(void) const;
  void labelSpacing(int);
  
  MSAlignment labelAlignment(void) const;
  void labelAlignment(MSAlignment);

  void showLabel(MSBoolean);
  MSBoolean showLabel(void) const;

  void showPixmap(MSBoolean);
  MSBoolean showPixmap(void) const;

  virtual void redraw(void);
};

inline int MSIcon::labelSpacing(void) const { return _labelSpacing;}
inline MSAlignment MSIcon::labelAlignment(void) const { return _labelAlignment; }
inline MSBoolean MSIcon::showLabel(void) const { return _showLabel; }
inline MSBoolean MSIcon::showPixmap(void) const { return _showPixmap; }
inline int MSIcon::labelWidth(void) { return showLabel()==MSTrue?maxPixelWidth():0; }
inline int MSIcon::labelHeight(void) { return showLabel()==MSTrue?rows()*textHeight():0; }
inline int MSIcon::pixmapWidth(void) { return showPixmap()==MSTrue&&pixmap()!=0?pixmap()->width():0; }
inline int MSIcon::pixmapHeight(void) { return showPixmap()==MSTrue&&pixmap()!=0?pixmap()->height():0; }

#endif
