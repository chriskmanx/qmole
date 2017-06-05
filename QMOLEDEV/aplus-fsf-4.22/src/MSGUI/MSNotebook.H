#ifndef MSNotebookHEADER
#define MSNotebookHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////
 

#include <MSGUI/MSManager.H>
#include <MSGUI/MSIconButton.H>
#include <MSTypes/MSStringVector.H>
#include <MSGUI/MSArrowButton.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSBackingStorePixmap.H>
#include <MSGUI/MSStringPopupMenu.H>


class MSNotebook;
class MSGUIExport MSNotebookTabAttribute
{
protected:
  unsigned long  _fg;
  unsigned long  _bg;
  Font           _font;
  MSBoolean      _sensitive;
  MSStringVector _label;
  MSPixmap      *_pixmap;
  MSAlignment    _labelAlignment;
  unsigned long  _setFlag; 
  MSBoolean      _displayToolTip;
  MSStringVector _toolTip;
  MSNotebook    *_notebook;

public:
  enum SetFlag {TabBackground=0x1,TabForeground=0x2,TabFont=0x4,
                TabSensitivity=0x8,TabLabel=0x10,TabPixmap=0x20,
		TabLabelAlignment=0x40, TabDisplayToolTip=0x80, TabToolTip=0x100};

  MSNotebookTabAttribute(MSNotebook *); 
  ~MSNotebookTabAttribute(void);

  MSNotebook *notebook(void) const {return _notebook;}

  void label(const MSStringVector &label_) {_label=label_,_setFlag|=TabLabel;}
  const MSStringVector &label(void) const {return _label;}

  void pixmap(const MSPixmap &pixmap_);
  const MSPixmap *pixmap(void) const {return _pixmap;}

  void labelAlignment(MSAlignment labelAlignment_) {_labelAlignment=labelAlignment_,_setFlag|=TabLabelAlignment;}
  MSAlignment labelAlignment(void) const {return _labelAlignment;}
  
  void foreground(unsigned long fg_) {_fg=fg_,_setFlag|=TabForeground;}
  unsigned long foreground(void) const {return _fg;}

  void background(unsigned long bg_) {_bg=bg_,_setFlag|=TabBackground;}
  unsigned long background(void) const {return _bg;}
 
  void font(Font font_) {_font=font_,_setFlag|=TabFont;}
  Font font(void) const {return _font;}
  
  void sensitive(MSBoolean sensitive_) {_sensitive=sensitive_,_setFlag|=TabSensitivity;}
  MSBoolean sensitive(void) const {return _sensitive;}

  void displayToolTip(MSBoolean displayTooltip_)  
    {_displayToolTip=displayTooltip_,_setFlag|=TabDisplayToolTip;}
  MSBoolean displayToolTip(void) const {return _displayToolTip;}

  void toolTip(const MSStringVector& toolTip_) {_toolTip=toolTip_,_setFlag|=TabToolTip;}
  const MSStringVector &toolTip(void) const { return _toolTip; } 
  
  MSBoolean isSet(SetFlag flag_) const { return (_setFlag&flag_)?MSTrue:MSFalse;}
  void reset(void);
};

class MSGUIExport MSNotebook : public MSManager
{
public:
  enum Orientation {Vertical=0,Horizontal=1};
  
  MSNotebook(MSWidget *,const char *title_=0);
  MSNotebook(MSWidget *,const MSStringVector&);
  ~MSNotebook(void);

  void pageTitle(MSWidget *child_,const char *title_);  // Associate a child with a page title
  const char *pageTitle(MSWidget *child_) const;        // Same as titleFromWidget
  
  MSStringVector titles(void) const {return _titleVector;}
  MSStringVector hiddenTitles(void) const; // Returns all hidden titles 

  const char *titleFromWidget(MSWidget *widget_) const;
  MSWidget *widgetFromTitle(const char *title_) const;
  
  void currentTitle(const char *);
  const char *currentTitle(void) const;
  
  void currentWidget(MSWidget *);
  MSWidget *currentWidget(void) const;

  void tabAttribute(MSWidget *child_,const MSNotebookTabAttribute &);
  MSNotebookTabAttribute tabAttribute(MSWidget *child_) const;

  void orientation(Orientation);
  Orientation orientation(void) const {return _orientation;}

  void tabAlignment(MSAlignment);
  MSAlignment tabAlignment(void) const {return _tabAlignment;}

  void tabSpacing(unsigned);
  unsigned tabSpacing(void) const {return _tabSpacing;}

  void showBinding(MSBoolean);
  MSBoolean showBinding(void) const {return _showBinding;}

  void showTabs(MSBoolean);
  MSBoolean showTabs(void) const {return _showTabs;}

  void showPopup(MSBoolean showPopup_) {_showPopup=showPopup_;} 
  MSBoolean showPopup(void) const {return _showPopup;}

  void installPopup(MSPopupMenu *);

  void bindingWidth(unsigned);
  unsigned bindingWidth(void) const {return _bindingWidth;}

  void frameThickness(unsigned);
  unsigned frameThickness(void) const {return _frameThickness;}

  void borderWidth(unsigned);
  unsigned borderWidth(void) const {return _borderWidth;}

  void borderHeight(unsigned);
  unsigned borderHeight(void) const {return _borderHeight;}

  void marginWidth(unsigned);
  unsigned marginWidth(void) const {return _marginWidth;}

  void marginHeight(unsigned);
  unsigned marginHeight(void) const {return _marginHeight;}

  void backpages(unsigned);
  unsigned backpages(void) const {return _backpages;}

  void backpageThickness(unsigned);
  unsigned backpageThickness(void) const {return _backpageThickness;}
  
  void backpageForeground(unsigned long);   
  void backpageForeground(const char *);
  unsigned long backpageForeground(void) const {return _backpageForeground;}

  void backpageBackground(unsigned long);   
  void backpageBackground(const char *);
  unsigned long backpageBackground(void) const {return _backpageBackground;}

  void selectedPageForeground(unsigned long);   
  void selectedPageForeground(const char *);
  unsigned long selectedPageForeground(void) const {return _selectedPageForeground;}

  void selectedPageBackground(unsigned long);   
  void selectedPageBackground(const char *);
  unsigned long selectedPageBackground(void) const {return _selectedPageBackground;}
  
  void frameBackground(unsigned long);   
  void frameBackground(const char *);
  unsigned long frameBackground(void) const {return _frameBackground;}

  void lockSize(MSBoolean lockSize_) {_lockSize=lockSize_;}
  MSBoolean lockSize(void) const {return _lockSize;}
    
  void nextPage(MSBoolean = MSTrue);
  void previousPage(MSBoolean = MSTrue);

  virtual void permuteTitles(const MSStringVector &);
  virtual void permuteWidgets(const MSWidgetVector &);

  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

  virtual void naturalSize(void);
  virtual void redraw(void);
  virtual void show(void);
  virtual void unfreeze(void);
  virtual void print(const char *);

  virtual MSWidgetVector children(void);

protected:
  class NotebookTab;
  class NotebookArrow;
friend class NotebookTab;
friend class NotebookArrow;

public:
  // This class declaration should be protected, but Lucid complains...
  class NotebookEntry;
friend class NotebookEntry;
  class MSGUIExport NotebookEntry
  {
  private:
    MSWidget     *_widget;
    NotebookTab  *_tab;
    MSBoolean     _managed;
  public:
    NotebookEntry(MSNotebook *owner_,MSWidget *widget_,const char *title_="Untitled");
    ~NotebookEntry(void);
    MSWidget *widget(void) {return _widget;}
    const NotebookTab *tab(void) const {return _tab;}
    NotebookTab *tab(void) {return _tab;}
    MSBoolean managed(void) {return _managed;}
    void managed(MSBoolean managed_) {_managed=managed_;}
    const MSStringVector &title(void) const
    {return tab()->label();}
    void title(const char *title_) {tab()->label(MSStringVector(title_));}
  };

protected:

  class MSGUIExport NotebookTab : public MSIconButton
  {
  protected:
    unsigned long   _modifiedFlag;
    unsigned long   _tabBackground;
    unsigned long   _tabForeground;
    NotebookEntry  *_entry;

    unsigned long modifiedFlag(void) const {return _modifiedFlag;}
    virtual void activate(void);
    virtual void arm(void);
    virtual void disarm(void);
    virtual void buttonPress(const XEvent *);
    virtual void motionNotify(const XEvent *);
    void blendIn(void);
    
  public:
    NotebookTab(MSWidget *,NotebookEntry *,const char *);
    ~NotebookTab(void);

    const NotebookEntry *entry(void) const {return _entry;}
    NotebookEntry *entry(void) {return _entry;}

    void tabBackground(unsigned long,MSBoolean modified_=MSTrue);
    unsigned long tabBackground(void) const {return _tabBackground;}

    void tabForeground(unsigned long,MSBoolean modified_=MSTrue);
    unsigned long tabForeground(void) const {return _tabForeground;}

    void tabFont(Font,MSBoolean modified_=MSTrue);
    Font tabFont(void) const {return font();}
  
    MSBoolean isModified(MSNotebookTabAttribute::SetFlag);

    virtual void redraw(void);
  };
  
  class MSGUIExport NotebookArrow : public MSArrowButton
  {
  protected:
    enum Mode {Search,Browse};
    Mode _mode;
    virtual void activate(void);
    virtual void browse(void);
    virtual void search(void);
    virtual void buttonPress(const XEvent *);
    virtual void button1Press(const XEvent *);
  public:
    NotebookArrow(MSWidget *owner_,MSArrow::ArrowType);
    ~NotebookArrow(void);

    void mode(Mode mode_) {_mode=mode_;}
    Mode mode(void) {return _mode;}
  };
  
  MSBackingStorePixmap  *_redrawPixmap;
  MSBoolean          _lockSize;
  MSBoolean          _internalEvent;
  NotebookEntry     *_firstEntry;
  NotebookEntry     *_currentEntry;
  Orientation        _orientation;
  MSAlignment        _tabAlignment;
  MSBoolean          _showBinding;
  MSBoolean          _showTabs;
  MSBoolean          _showPopup;
  unsigned           _bindingWidth;
  unsigned           _frameThickness;
  unsigned           _borderWidth;
  unsigned           _borderHeight;
  unsigned           _marginWidth;
  unsigned           _marginHeight;
  unsigned long      _backpageForeground;
  unsigned long      _backpageBackground;
  unsigned long      _frameBackground;
  unsigned long      _selectedPageForeground;
  unsigned long      _selectedPageBackground;
  unsigned           _backpages;
  unsigned           _backpageThickness;
  unsigned           _tabSpacing;
  unsigned           _arrowOffset;
  MSLabel           *_label;
  NotebookArrow     *_upLeftArrow;
  NotebookArrow     *_downRightArrow;
  Pixmap             _bindingPixmap;
  GC                 _notebookGC;
  GC                 _bindingGC;
  GC                 _backpageGC;
  MSStringPopupMenu *_popupMenu;
  MSPopupMenu       *_applicationPopup;
  MSStringVector     _titleVector;

  void init(void);
  MSBackingStorePixmap *redrawPixmap(void) {return _redrawPixmap;}
  MSLabel *label(void) {return _label;}
  const MSLabel *label(void) const {return _label;}
  NotebookArrow *upLeftArrow(void) {return _upLeftArrow;}
  NotebookArrow *downRightArrow(void) {return _downRightArrow;}
  unsigned arrowOffset(void) {return _arrowOffset;}
  Pixmap bindingPixmap(void) {return _bindingPixmap;}
  GC notebookGC(void) {return _notebookGC;}
  GC bindingGC(void) {return _bindingGC;}
  GC backpageGC(void) {return _backpageGC;}
  MSStringVector &titleVector(void) {return _titleVector;}
  void internalEvent(MSBoolean internalEvent_) {_internalEvent=internalEvent_;}
  MSBoolean internalEvent(void) {return _internalEvent;}
  NotebookEntry *getEntry(MSWidget *) const;
  NotebookEntry *getEntry(const char *) const;
  NotebookEntry *getEntry(unsigned i) const;
  NotebookEntry *firstItem(void);
  NotebookEntry *lastItem(void);
  NotebookEntry *currentEntry(void) const {return _currentEntry;}
  void currentEntry(NotebookEntry *);
  NotebookEntry *firstEntry(void) const {return _firstEntry;}
  void firstEntry(NotebookEntry *);
  void lastEntry(NotebookEntry *);
  NotebookEntry *nextEntry(void);
  NotebookEntry *nextBrowseEntry(MSBoolean cycle_=MSFalse);
  NotebookEntry *previousEntry(void);
  NotebookEntry *previousBrowseEntry(MSBoolean cycle_=MSFalse);
  void resetFirstEntry(void);
  int numManaged(void);
  MSWidgetVector managedChildren(void);
  int visibleTabs(void);
  int totalTabWidth(void);
  int maxTabWidth(void);
  int availableWidthForTabs(void);
  int totalTabHeight(void);
  int maxTabHeight(void);
  int availableHeightForTabs(void);
  int backpageMargin(void);
  int arrowWidth(void);
  int arrowHeight(void);
  int labelWidth(void);
  int labelHeight(void);
  void positionLabel(void);
  void positionTabs(MSBoolean increment_=MSTrue);
  void insertChild(MSWidget *);
  void removeChild(MSWidget *);
  void drawBackpage(Window);
  void drawBinding(Window);
  void drawFrameShadow(Window);
  void createBindingPixmap(void);
  void drawSpiral(void);
  void computeChildSize(int &,int &);
  int computeMinimumWidth(void);
  int computeMinimumHeight(void);
  void menuSelection(void);
  MSBoolean okToSwitch(void);
  void pageSelected(NotebookEntry *);

  virtual MSPopupMenu *popupMenu(void);
  virtual void pageChanged(void);

  virtual void configure(void);
  virtual void adjustSize(void);
  virtual void computeSize(void);
  virtual void placement(void);
  virtual void firstMapNotify(void);
  virtual void childCreate(MSWidget *);
  virtual void childDestroy(MSWidget *);
  virtual void childInsert(MSWidget *);
  virtual void childRemove(MSWidget *);
  virtual void childMap(MSWidget *);
  virtual void childConfigure(MSWidget *);
  virtual void visibilityObscured(void);
  virtual void visibilityUnobscured(void);
  virtual void buttonPress(const XEvent *);
  virtual void button3Press(const XEvent *);
  
  virtual void updateTitle(void);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateFont(Font);
  virtual void updateTitleVector(void);
  virtual void updatePopupMenu(void);
};

#endif 
