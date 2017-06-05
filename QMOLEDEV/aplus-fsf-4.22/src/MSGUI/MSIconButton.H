#ifndef MSIconButtonHEADER
#define MSIconButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSIcon.H>

class MSGUIExport MSIconButton : public MSIcon
{
protected:
  MSBoolean _armed;
  MSPixmap *_armedPixmap;

  MSBoolean armed(void) const;
  void createDefaultArmedPixmap(int,int,unsigned long,unsigned long);

  virtual int computeXCoord(int,int,const char *,int);
  virtual int computeYCoord(int);

  virtual void drawBackground(void);
  virtual void drawLabel(void);
  virtual void drawPixmap(void);

  virtual void arm(void);
  virtual void disarm(void);
  virtual void activate(void);

  virtual void key(KeySym,unsigned int,const char*);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char*);
  virtual void buttonPress(const XEvent*);
  virtual void buttonRelease(const XEvent*);
  virtual void motionNotify(const XEvent*);
  virtual void button1Press(const XEvent*);
  virtual void button1Release(const XEvent*);
  virtual void updateSensitivity(void);
  virtual void updatePixmap(void);
 
  void init(void);
  void init(const MSPixmap &);

public:
  MSIconButton(MSWidget *,const char *label_,const MSPixmap &pixmap_,
	       const MSPixmap &insensitivePixmap_,const MSPixmap &armedPixmap_);
  MSIconButton(MSWidget *,const MSStringVector &label_,const MSPixmap &pixmap_,
	       const MSPixmap &insensitivePixmap_,const MSPixmap &armedPixmap_);
  MSIconButton(MSWidget *,MSStringVector &label_,const MSPixmap &pixmap_,
	       const MSPixmap &insensitivePixmap_,const MSPixmap &armedPixmap_);
  MSIconButton(MSWidget *,const char *label_=0);
  MSIconButton(MSWidget *,const MSStringVector&);
  MSIconButton(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
               const MSPixmap &armedPixmap_);
  ~MSIconButton(void);

  const MSPixmap *armedPixmap(void) const { return _armedPixmap; } 
  void armedPixmap(const MSPixmap &);
  virtual void redraw(void);
};

inline MSBoolean MSIconButton::armed(void) const
{ return _armed; }

inline void MSIconButton::updateSensitivity(void)
{
   MSIcon::updateSensitivity();
   if (sensitive()==MSTrue)
      acceptFocus(MSTrue);
   else
      acceptFocus(MSFalse);
}

#endif
