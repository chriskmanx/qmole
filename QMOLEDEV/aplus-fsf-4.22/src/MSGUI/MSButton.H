#ifndef MSButtonHEADER
#define MSButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSLabel.H>

class MSGUIExport MSButton : public MSLabel
{
public:
  MSButton(MSWidget *);
  MSButton(MSWidget *,const char *);
  MSButton(MSWidget *,const MSStringVector&);
  MSButton(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
	   const MSPixmap &armedPixmap_);
  ~MSButton(void);
  
  const MSPixmap *armedPixmap(void) const { return _armedPixmap; } 
  void armedPixmap(const MSPixmap &);
  virtual void redraw(void);

  virtual MSAttrValueList& get(MSAttrValueList&);

protected:
  MSBoolean _armed;
  MSPixmap *_armedPixmap;

  MSBoolean armed(void) const;

  void init(void);
  void createDefaultArmedPixmap(int,int,unsigned long,unsigned long);

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
  
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);

};

inline MSBoolean MSButton::armed(void) const
{ return _armed; }

#endif











