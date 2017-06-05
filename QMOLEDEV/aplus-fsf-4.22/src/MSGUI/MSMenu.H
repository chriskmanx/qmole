#ifndef MSMenuHEADER
#define MSMenuHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>
#include <MSGUI/MSMenuItem.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSNodeList.H>

class MSMenuBar;
class MSGUIExport MSMenu : public MSWidgetCommon
{
friend class MSMenuItem;
friend class MSMenuList;
public:
  MSMenu(MSDisplayServer *);
  MSMenu(MSWidget *);
  ~MSMenu(void);

  MSMenuItem *activeMenuItem(void);
  const MSMenuItem *activeMenuItem(void) const;

  MSMenuItem *taggedMenuItem(int);
  const MSMenuItem *taggedMenuItem(int) const;

  MSMenuItem *parentMenuItem(void);
  const MSMenuItem *parentMenuItem(void) const;

  int entryBorder(void) const;
  void entryBorder(int);
  
  MSBoolean radioBehavior(void) const;
  void radioBehavior(MSBoolean);  

  unsigned columns(void) const;
  void columns(unsigned);

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);
  
  virtual void left(void);
  virtual void right(void);
  virtual void up(void);
  virtual void down(void);
  virtual void returnKey(void);
  virtual void escape(void);
  virtual void done(void);
  virtual void activate(void);
  virtual void reset(void);
  virtual void enforceRadioBehavior(void);
  virtual void grabAndSelect(unsigned long time_=CurrentTime);
  virtual void grab(unsigned long time_=CurrentTime);
  virtual void ungrab(unsigned long time_=CurrentTime);
  virtual void releaseGrab(void);
  virtual void redraw(void);
  virtual void unmap(void);
  virtual void map(void);

  virtual MSWidgetVector children(void);

  void permuteMenuItems(const MSWidgetVector&);
  void permuteMenuItems(const MSIntVector&);

public:
  class MSGUIExport MSMenuList 
  {
  private:
    MSNodeItem _itemListHead;
    MSNodeItem *itemListHead(void) {return _itemListHead.address();}
  public:
    MSMenuList(void);
    ~MSMenuList(void);
    void addToList(MSMenu *);
    void removeFromList(MSMenu *);
    MSMenu *findMenu (MSDisplayServer *server_,int x_,int y_);
  };
protected:
  static MSMenuList       _menuList;
  MSWidgetVector          _itemVector;
  MSWidgetVector          _hiddenItemVector;
  int                     _selectedItem;
  MSBoolean               _radioBehavior;
  int                     _entryBorder;
  unsigned                _columns;
  MSMenuItem             *_parentMenuItem;

  void init(void);
  void initFromServer(MSDisplayServer*);

  MSMenu(MSMenuItem*);   //this constructor is to be used only by subclasses.
  
  MSWidgetVector& itemVector(void);
  const MSWidgetVector& itemVector(void) const;

  MSWidgetVector& hiddenItemVector(void);
  const MSWidgetVector& hiddenItemVector(void) const;

  MSMenuItem *menuItem(int);
  const MSMenuItem *menuItem(int) const;
  MSMenuItem *findItem(int x_,int y_);
  
  int itemCount(void) const;
  int selectedItem(void) const;
  void selectedItem(int);

  //calculateNaturalSize returns the width and height & placeMenuItems places items
  //in correct x,y coordinates. without having to be mapped first
  virtual void calculateNaturalSize(int&,int&);
  void placeMenuItems(void);

  virtual void computeSize(void);
  virtual void removeAllItems(void);
  virtual void drawItems(void);
  virtual void drawItem(int item_,MSBoolean select_);
  virtual void drawSelectedItem(void);
  virtual void undrawSelectedItem(void);
  virtual void placement(void);

  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual MSBoolean loseFocus(void);

  virtual void motionNotify(const XEvent *); 
  virtual void buttonRelease(const XEvent *);
  virtual void buttonPress(const XEvent *);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);

  virtual void childConfigure(MSWidget *);
  virtual void childCreate(MSWidget *);
  virtual void childDestroy(MSWidget *);
  virtual void childMap(MSWidget *);

  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  
  virtual void firstMapNotify(void);
  virtual void configure(void);
  virtual void accelerator(char aChar_);
  virtual void pulldownMenus(void);

  MSMenu *grabber(void);
  void grabber(MSMenu *);
  MSMenuBar *menuBar(void);
  void menuBar(MSMenuBar*);
  MSDisplayCursor *grabCursor(void);
  static MSMenuList &menuList(void)        { return _menuList; }

  virtual void pointerLeave(void);  
  virtual void reselect(void); 
  virtual void selectNone(void); 
  virtual MSMenuItem *nextRightItem(void);
  virtual MSMenuItem *nextLeftItem(void);
  virtual MSMenuItem *nextUpItem(void);
  virtual MSMenuItem *nextDownItem(void);

  MSBoolean insideColumn(unsigned column_,int item_); // Checks if item_ is in column_
  unsigned rowCount(unsigned column_); // Returns the number of rows in column_
  MSIntVector rowItems(int item_);     // Returns all the items in the row that contains item_
  void setItem(MSMenuItem *, int);     // Sets the item value of a menuItem.
};


inline MSWidgetVector& MSMenu::itemVector(void)
{ return _itemVector; }
inline const MSWidgetVector& MSMenu::itemVector(void) const
{ return _itemVector; }

inline MSWidgetVector& MSMenu::hiddenItemVector(void)
{ return _hiddenItemVector; }
inline const MSWidgetVector& MSMenu::hiddenItemVector(void) const
{ return _hiddenItemVector; }

inline int MSMenu::itemCount(void) const
{ return _itemVector.length(); }
inline int MSMenu::selectedItem(void) const      
{ return _selectedItem; }

inline int MSMenu::entryBorder(void) const
{ return _entryBorder; }
 
inline MSBoolean MSMenu::radioBehavior(void) const
{ return _radioBehavior; }

inline unsigned MSMenu::columns(void) const
{ return _columns; }

inline MSMenu *MSMenu::grabber(void)    
{ return (MSMenu*)server()->menuGrabber(); }
inline MSMenuBar *MSMenu::menuBar(void) 
{ return (MSMenuBar*)server()->menuBar(); }
inline MSDisplayCursor *MSMenu::grabCursor(void)
{ return server()->menuGrabCursor(); }

inline void MSMenu::grabber(MSMenu *menu_)    
{ server()->menuGrabber(menu_); }
inline void MSMenu::menuBar(MSMenuBar* bar_) 
{ server()->menuBar((MSWidget*)bar_); }

inline void MSMenu::selectedItem(int item_)   
{ _selectedItem=item_; }

inline unsigned MSMenu::rowCount(unsigned column_)
{ return (itemCount()/columns()+((itemCount()%columns())>column_?1:0)); }

inline MSMenuItem * MSMenu::parentMenuItem(void)
{ return _parentMenuItem; }

inline const MSMenuItem * MSMenu::parentMenuItem(void) const
{ return _parentMenuItem; }

#endif
