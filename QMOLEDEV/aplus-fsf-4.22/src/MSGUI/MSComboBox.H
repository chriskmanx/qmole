#ifndef MSComboBoxHEADER
#define MSComboBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSComboField.H>
#include <MSGUI/MSStringList.H>

class MSComboBox;
class MSComboListShell;
class MSDisplayCursor;

class MSGUIExport MSComboList : public MSStringList
{
friend class MSComboBox;
public:
  MSComboList(MSComboListShell *);

  void displayAllColumns(MSBoolean);
  MSBoolean displayAllColumns(void) const;
  void setSelectedRowFrom(const char *);
  unsigned selectString(int,const char *);

  virtual void show(void);

protected:
  MSComboListShell *shell(void);
  const MSComboListShell *shell(void) const;
  MSComboBox *comboBox(void);
  const MSComboBox *comboBox(void) const;

  virtual void keyPressEvent(const XEvent *);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void buttonPress(const XEvent *);
  virtual void defaultDoubleClickBehavior(const XEvent *);
  virtual void activate(void);
  virtual void escape(void);
  virtual void doubleClick(void);
  virtual void reset(void);

  void selectEditableText(void);
  void resetVisibleColumns(void);  

  
private:
  MSBoolean _selectedText;
  MSBoolean _displayAllColumns;
  
  MSBoolean selectedText(void) const;
};

class MSGUIExport MSComboListShell : public MSWidget
{
friend class MSComboList;
public:
  MSComboListShell(MSComboBox *);    
  ~MSComboListShell(void);

  virtual void show(void);
  virtual void hide(void);
  virtual void showAt(int,int);

  MSComboList *comboList(void);
  const MSComboList *comboList(void) const;

protected:
  MSDisplayCursor *cursor(void) { return _cursor; }
  virtual void childConfigure(MSWidget *);
  virtual void configure(void);
  virtual void grab(void);
  virtual void ungrab(void);
  virtual void escape(void);
  virtual void activate(void);
    
private:
  MSComboBox      *_comboBox;
  MSComboList     *_comboList;
  MSDisplayCursor *_cursor;

  MSComboBox *comboBox(void) const;
};

inline MSComboBox *MSComboListShell::comboBox(void) const
{ return _comboBox; }  
inline MSComboList *MSComboListShell::comboList(void)
{ return _comboList; }  
inline const MSComboList *MSComboListShell::comboList(void) const
{ return _comboList; }  

inline MSComboListShell *MSComboList::shell(void)
{ return (MSComboListShell *)owner(); }
inline const MSComboListShell *MSComboList::shell(void) const 
{ return (const MSComboListShell *)owner(); }

inline MSComboBox *MSComboList::comboBox(void)
{ return shell()->comboBox(); }
inline const MSComboBox *MSComboList::comboBox(void) const 
{ return shell()->comboBox(); }

inline MSBoolean MSComboList::selectedText(void) const
{ return _selectedText; }

inline void MSComboList::displayAllColumns(MSBoolean bool_)
{ _displayAllColumns=bool_; }
inline MSBoolean MSComboList::displayAllColumns(void) const
{ return _displayAllColumns; }


class MSGUIExport MSComboBox : public MSComboField
{
friend class MSComboList;
friend class MSComboListShell;
public:
  MSComboBox(MSWidget *,const char *label_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSComboBox(MSWidget *,MSString&,const char *label_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSComboBox(void);

  MSComboList *listBox(void);
  const MSComboList *listBox(void) const;

  MSSelectionMode selectionMode(void) const;
  void selectionMode(MSSelectionMode); 

  const MSString& separator(void) const;
  void separator(const MSString&);
  
  void listModel(MSStringVector&);
  MSStringVector& listModel(void);

  MSComboBox& operator=(const char *);
  operator MSString&();

  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);
  
protected:
  MSComboListShell *_listShell;
  MSSelectionMode   _selectionMode;
  MSString          _separator;

  MSComboListShell *listShell(void) const;

  void init(void);
  virtual void createListShell(void);

  virtual void showList(void);
  virtual void listDone(void);
  virtual void listActivate(void);
  virtual void buttonActivate(void);  

  virtual void updateData(void);

  virtual void updateFont(Font);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
 
  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);

  void appendEditableText(const char *);
  void replaceEditableText(const char *);
  void selectEditableText(void);
};

inline MSComboListShell *MSComboBox::listShell(void) const
{ return _listShell; }
inline MSComboList *MSComboBox::listBox(void)
{ return listShell()->comboList(); }
inline const MSComboList *MSComboBox::listBox(void) const
{ return listShell()->comboList(); }

inline MSComboBox& MSComboBox::operator=(const char *value_) 
{ value(value_); return *this; }
inline MSComboBox::operator MSString&()                      
{ return *((MSString *)_model); } 

inline MSSelectionMode MSComboBox::selectionMode(void) const
{ return _selectionMode; }
inline void MSComboBox::selectionMode(MSSelectionMode mode_) 
{ _selectionMode=mode_; }

inline const MSString& MSComboBox::separator(void) const
{ return _separator; }

inline void MSComboBox::separator(const MSString& separator_)
{ _separator=separator_; }
  
#endif



