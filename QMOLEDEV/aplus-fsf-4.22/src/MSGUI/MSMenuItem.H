#ifndef MSMenuItemHEADER
#define MSMenuItemHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetOutput.H>
#include <MSGUI/MSGC.H>
#include <MSTypes/MSString.H>

class MSFontObject;
class MSArrow;
class MSPixmap;
class MSMenu;
class MSMenuBar;
class MSPulldownMenu;
class MSPopupMenu;
class MSCascadeMenu;

class MSGUIExport MSMenuItem : public MSWidgetOutput
{
friend class MSMenu;
friend class MSMenuBar;
friend class MSPulldownMenu;
friend class MSPopupMenu;
friend class MSCascadeMenu;
protected:
  enum ShowState { ShowLabel, ShowPixmap, ShowBoth };  

public:
  MSMenuItem(MSMenu *);
  MSMenuItem(MSMenu *,const char *,char mnemonic_=0,int tag_=0);
  MSMenuItem(MSMenu *,const MSString&,char mnemonic_=0,int tag_=0);
  MSMenuItem(MSMenu *,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,int tag_=0);
  MSMenuItem(MSMenu *,const MSString &,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
	     char mnemonic_=0,int tag_=0);
  MSMenuItem(MSMenu *,const char*,const MSPixmap &pixmap_,const MSPixmap &insensitivePixmap_,
	     char mnemonic_=0,int tag_=0);
  ~MSMenuItem(void);

  MSMenu *menu(void);
  const MSMenu *menu(void) const;

  int tag(void) const;
  void tag(int tag_);
  char mnemonic(void) const;
  void mnemonic(char);
  MSBoolean armed(void) const;
  void armed(MSBoolean);
  MSBoolean state(void) const;
  void state(MSBoolean);
  MSBoolean selected(void) const;
  MSBoolean cascade(void) const;

  const MSString& label(void) const;
  void label(const MSString&);
  const MSPixmap *pixmap(void) const;
  void pixmap(const MSPixmap&);
  const MSPixmap *insensitivePixmap(void) const;
  void insensitivePixmap(const MSPixmap&);

  int  marginWidth(void) const; 
  void marginWidth (int);
  int  marginHeight(void) const;
  void marginHeight (int);
  void spacing(int);
  int  spacing(void) const;
  
  virtual void select(void);
  virtual void unselect(void);
  virtual void grab(void);
  virtual void arm(void);
  virtual void disarm(void);
  virtual void activate(void);  
  virtual void redraw(void);
  virtual void naturalSize(void);
  virtual MSWidgetVector children(void);
  MSBoolean changeState(void) const;
  void changeState(MSBoolean);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);
  static MSBoolean defaultMnemonic(void);
  static void defaultMnemonic(MSBoolean);

protected:
  MSString     	    _label;
  MSPixmap     	   *_pixmap;
  MSPixmap     	   *_insensitivePixmap;
  char         	    _mnemonic;
  int          	    _item;
  int          	    _tag;
  MSBoolean    	    _armed;
  MSBoolean    	    _selected;
  MSBoolean    	    _cascade;
  MSBoolean         _changeState;
  MSGC         	    _textMSGC;
  MSFontObject 	   *_fontObject;
  int          	    _marginWidth;
  int          	    _marginHeight;
  int          	    _indent;
  int               _spacing;
  MSPixmap     	   *_stipple;
  GC                _pixmapGC;
  ShowState         _showState;
  static MSBoolean  _defaultMnemonic;

  MSGC& textMSGC(void);
  GC textGC(void) const;
  MSFontObject *fontObject(void) const;
  MSPixmap *stipple(void) const;
  GC pixmapGC(void);

  void init(void);
  void initPixmaps(const MSPixmap&,const MSPixmap&);
  void createGCs(void);
  MSPixmap *createDefaultPixmap(int,int,unsigned long,unsigned long);

  int drawWidth(void) const;
  int drawHeight(void) const;
  void indent(int);
  int indent(void) const;
  int item(void) const;  
  void item(int item_);
  ShowState showState(void) const  { return _showState; } 
  void showState(ShowState);
  int labelWidth(void);
  int labelHeight(void);
  int pixmapWidth(void);
  int pixmapHeight(void); 

  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void updateSensitivity(void);

  virtual MSMenu* cascadedMenu(void);
  virtual const MSMenu* cascadedMenu(void) const;
  virtual void radioDisarm(void);
  virtual void computeSize(void);
  virtual void configure(void);
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void drawSymbol(void);
  virtual void drawLabel(void);
  virtual void drawPixmap(void);
  virtual void computePixmapDrawingCoord(const MSPixmap *,int&,int&,int&,int&,int&,int&);
  virtual int computeYCoord(void);
  virtual int computeXCoord(void);
  virtual int computeIndentation(void);
  virtual const MSPixmap *currentPixmap(void) const;
};

inline MSFontObject *MSMenuItem::fontObject(void) const
{ return _fontObject; }  
inline MSPixmap *MSMenuItem::stipple(void) const
{ return _stipple; }
inline int MSMenuItem::item(void) const  
{ return _item; }
inline void MSMenuItem::item(int item_) 
{ _item=item_; }

inline MSMenu *MSMenuItem::menu(void)
{ return (MSMenu *)owner(); }
inline const MSMenu *MSMenuItem::menu(void) const                   
{ return (MSMenu *)owner(); }

inline int MSMenuItem::tag(void) const                         
{ return _tag; }
inline char MSMenuItem::mnemonic(void) const                    
{ return _mnemonic; }
inline MSBoolean MSMenuItem::armed(void) const              
{ return _armed; }
inline MSBoolean MSMenuItem::state(void) const            
{ return _armed; }
inline MSBoolean MSMenuItem::selected(void) const            
{ return _selected; }
inline MSBoolean MSMenuItem::cascade(void) const         
{ return _cascade; }
inline const MSString& MSMenuItem::label(void) const
{ return _label; }
inline const MSPixmap *MSMenuItem::pixmap(void) const
{ return _pixmap; }
inline const MSPixmap *MSMenuItem::insensitivePixmap(void) const
{ return _insensitivePixmap; }
inline int MSMenuItem::marginWidth(void) const
{ return _marginWidth; }
inline int MSMenuItem::marginHeight(void) const
{ return _marginHeight; }
inline int MSMenuItem::indent(void) const
{ return _indent; }

inline void MSMenuItem::tag(int tag_)   
{ _tag=tag_; }
inline void MSMenuItem::mnemonic(char c_) 
{ _mnemonic=c_; }
inline void MSMenuItem::state(MSBoolean b_) 
{ _armed=b_; }
inline void MSMenuItem::armed(MSBoolean b_) 
{ _armed=b_; }
inline  MSBoolean MSMenuItem::changeState(void) const
{ return _changeState; } 
inline  void MSMenuItem::changeState(MSBoolean changeState_)
{ _changeState = changeState_; } 
inline  int MSMenuItem::spacing(void) const
{ return _spacing; } 
#endif
