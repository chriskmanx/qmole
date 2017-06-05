#ifndef MSActionButtonHEADER
#define MSActionButtonHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSButton.H>
#include <MSGUI/MSBoolView.H>

class MSActionBox;

class MSGUIExport MSActionButton : public MSButton, public MSBoolView
{
friend class MSActionBox;
public:
  MSActionButton(MSWidget *,const char *label_,
                 const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSActionButton(MSWidget *,const MSStringVector& label_,
                 const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSActionButton(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		 const MSPixmap &armedPixmap_,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSActionButton(MSWidget *,const char *label_,int integerTag_);
  MSActionButton(MSWidget *,const MSStringVector& label_,int  integerTag_);
  MSActionButton(MSWidget *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
		 const MSPixmap &armedPixmap_,int integerTag_);
  ~MSActionButton(void);

  const MSSymbol& tag(void) const;
  void tag(const MSSymbol&);

  int integerTag(void) const;
  void integerTag(int);

  MSBoolean state(void) const;
  void state(MSBoolean);

  virtual void redraw(void);

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);
  
protected:
  MSSymbol        _tag;
  int             _integerTag;

  MSActionBox *actionBox(void) const;
  void init(void);
  
  void setArmState(void);
  void setDisarmState(void);
  void defaultActivate(void);
  void actionBoxActivate(void);

  virtual void arm(void);
  virtual void disarm(void);
  virtual void activate(void);  
  virtual void change(void);  

  virtual void drawSymbol(void);

  // Arrow Key Traversal
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);

  virtual void newBoolModelNotify(void);
  virtual void boolModelChanged(MSEvent &);
};
 
inline MSBoolean MSActionButton::state(void) const                  
{ return _armed; }
inline const MSSymbol& MSActionButton::tag(void) const                         
{ return _tag; }
inline int MSActionButton::integerTag(void) const                         
{ return _integerTag; }

#endif





