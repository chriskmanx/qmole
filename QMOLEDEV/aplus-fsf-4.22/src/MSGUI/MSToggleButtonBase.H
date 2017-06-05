#ifndef MSToggleButtonBaseHEADER
#define MSToggleButtonBaseHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSActionButton.H>
#include <MSGUI/MSGC.H>

class MSGUIExport MSToggleButtonBase : public MSActionButton
{
public:
  MSToggleButtonBase(MSWidget *,const char *label_,
		 const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSToggleButtonBase(MSWidget *,const MSStringVector& label_,
		 const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSToggleButtonBase(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		 const MSPixmap &armedPixmap_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSToggleButtonBase(MSWidget *,const char *label_,int integerTag_);
  MSToggleButtonBase(MSWidget *,const MSStringVector& label_,int  integerTag_);
  MSToggleButtonBase(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		 const MSPixmap &armedPixmap_,int integerTag_);
  ~MSToggleButtonBase(void);

  int toggleShadowThickness(void) const;
  unsigned long selectColor(void) const;

  void toggleShadowThickness(int);
  void selectColor(unsigned long);
  void selectColor(const char *);

  virtual void redraw(void);

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);
  
protected:
  int             _toggleShadowThickness;
  int             _spacing;
  unsigned long   _selectColor;
  MSGC            _selectMSGC;

  MSGC& selectMSGC(void); 
  GC selectGC(void) const;      
  int spacing(void) const;      

  void init(void);
  void createGCs(void);

  virtual int computeXCoord(int row_,int column_,const char *,int);

  virtual void arm(void);
  virtual void disarm(void);
  virtual void activate(void);
  virtual void key(KeySym,unsigned int,const char*);

  virtual int computePixmapXCoord(const MSPixmap *);
  virtual int computePixmapYCoord(const MSPixmap *);
  virtual void computeSize(void);
  virtual void configure(void);
  virtual void button1Press(const XEvent *);
  virtual void button1Release(const XEvent *);
  virtual void motionNotify(const XEvent *);
};

inline int MSToggleButtonBase::toggleShadowThickness(void) const
{ return _toggleShadowThickness; }
inline unsigned long MSToggleButtonBase::selectColor(void) const
{ return _selectColor; }
inline int MSToggleButtonBase::spacing(void) const
{ return _spacing; }
inline MSGC& MSToggleButtonBase::selectMSGC(void)
{ return _selectMSGC; }
inline GC MSToggleButtonBase::selectGC(void) const
{ return _selectMSGC.gc(); }

#endif











