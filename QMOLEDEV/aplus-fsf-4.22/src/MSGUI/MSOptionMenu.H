#ifndef MSOptionMenuHEADER
#define MSOptionMenuHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSTextField.H>
#include <MSGUI/MSTextRect.H>
#include <MSGUI/MSCompositeField.H>
#include <MSGUI/MSPopupMenu.H>
#include <MSTypes/MSString.H>

class MSOptionMenu;

class MSGUIExport MSOptionPopupMenu : public MSPopupMenu
{
friend class MSOptionMenu;
public:
  MSOptionPopupMenu(MSOptionMenu *);
  MSOptionPopupMenu(MSOptionMenu *,MSStringVector&);

  void model(MSStringVector&);
  void options(const MSStringVector&);
  
  virtual void popup(MSBoolean,unsigned long);
  virtual void activate(void);

protected:
  virtual void motionNotify(const XEvent *); 
  virtual void buttonRelease(const XEvent *);
  virtual void buttonPress(const XEvent *);

  virtual void updateData(void);
  virtual void update(const MSIndexVector&);
  int menuItemXOffset(int);
  int menuItemYOffset(int);
  
private:
  MSOptionMenu *_optionMenu;
  unsigned long  _lastShowTime;
  unsigned long  _threshold;

  MSOptionMenu *optionMenu(void)     const {return _optionMenu;}
  MSStringVector& optionsModel(void) const {return (MSStringVector&)*_model;}

  unsigned long lastShowTime(void) const { return _lastShowTime; }
  void lastShowTime(unsigned long lastShowTime_) { _lastShowTime=lastShowTime_; }

  unsigned long threshold(void) const { return _threshold; }
  void threshold(unsigned long threshold_) { _threshold=threshold_; }

  void init(void);
  void rebuildMenu(void);
};

class MSGUIExport MSOptionMenu : public MSCompositeField
{
friend class MSOptionPopupMenu;
public:
  MSOptionMenu(MSWidget *,const char *label_=0);
  MSOptionMenu(MSWidget *,const char *label_,MSStringVector& optionsModel_);
  MSOptionMenu(MSWidget *,MSStringVector& optionsModel_,const char *label_);
  MSOptionMenu(MSWidget *,MSString& model_,MSStringVector& optionsModel_,const char *label_);
  ~MSOptionMenu(void);
    
  int selectedItem(void) const;
  void selectedItem(int);

  virtual void options(const MSStringVector&);

  void optionsModel(MSStringVector&);
  MSStringVector& optionsModel(void);
  const MSStringVector& optionsModel(void) const;

  void model(MSStringVector &model_); //optionsModel
  void model(MSString &model_);

  MSString& viewModel(void);            
  const MSString& viewModel(void) const;
    
  unsigned columns(void) const;
  void columns(unsigned);

  virtual MSAttrValueList& get(MSAttrValueList&);
  virtual void set(MSAttrValueList&);

protected:
  MSOptionPopupMenu *_optionMenu;
  int                _selectedItem;
  unsigned           _columns;
  MSBoolean          _internalEvent;
  
  MSOptionPopupMenu *optionMenu(void);
  const MSOptionPopupMenu *optionMenu(void) const;

  void init(void);
  virtual void showMenu(unsigned long);

  virtual void placement(void);
  virtual void drawFieldValue(void);
  virtual void drawFieldValue(unsigned long,unsigned long);  

  virtual void updateData(void);
  virtual void update(const MSIndexVector&);

  virtual void updateFont(Font);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateSensitivity(void);
  virtual void activate(int);
  virtual void activate(void);
  virtual void escape(void);

  virtual MSBoolean loseFocus(void);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void buttonPress(const XEvent *);
  virtual void buttonRelease(const XEvent *);
  virtual void motionNotify(const XEvent *);

  // The following virtual methods can be overriden by subclassers to
  // obtain model specific behavior:
  // numberOfItems - number of options in the model
  // itemLabel     - return the label for the nth item
  // itemForeground - return the foreground for the nth item
  // formatOuput   - format the selectedItem for the entry part of the display
  // maxValueWidth - return the max pixel width over all item labels in the option Menu

  virtual const char *formatOutput(MSString &);
  virtual const char *itemLabel(unsigned);
  virtual unsigned long itemForeground(unsigned);
  virtual unsigned maxValueWidth(void);
  virtual unsigned numberOfItems(void) const;
  virtual unsigned computeValuePixelWidth(void);

  void setSelectedItem(int);
};

inline MSOptionPopupMenu *MSOptionMenu::optionMenu(void)
{ return _optionMenu; }
inline const MSOptionPopupMenu *MSOptionMenu::optionMenu(void) const
{ return _optionMenu; }

inline MSStringVector& MSOptionMenu::optionsModel(void)
{ return optionMenu()->optionsModel(); }

inline const MSStringVector& MSOptionMenu::optionsModel(void) const
{ return optionMenu()->optionsModel(); }

inline void MSOptionMenu::model(MSStringVector &model_)
{ optionsModel(model_); }

inline int MSOptionMenu::selectedItem(void) const
{ return _selectedItem; }

inline unsigned MSOptionMenu::columns(void) const
{ return _columns; }

inline MSString& MSOptionMenu::viewModel(void)
{ return (MSString&)*_model; }

inline const MSString& MSOptionMenu::viewModel(void) const
{ return (const MSString&)*_model; }

#endif








