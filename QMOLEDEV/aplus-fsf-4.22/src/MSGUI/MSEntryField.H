#ifndef MSEntryFieldHEADER
#define MSEntryFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSCompositeField.H>
#include <MSGUI/MSTextField.H>
#include <MSGUI/MSColorCycle.H>
#include <MSIPC/MSIntervalTimer.H>
#include <MSGUI/MSValidationCallback.H>
#include <MSGUI/MSValidationInterface.H>
#include <MSTypes/MSFormat.H>

class MSEntryField;

class MSGUIExport MSEntryField : public MSCompositeField, public MSValidationInterface
{
public:
  class MSGUIExport CycleTimer : public MSIntervalTimer
  {
  protected:
    MSEntryField *_entryField;
  public:
    CycleTimer(MSEntryField *,unsigned long interval_);
    ~CycleTimer(void);
    virtual void process(void);
  };

  class MSGUIExport FieldEditor : public MSTextField
  {
  public:
    FieldEditor(MSWidget *);
    ~FieldEditor(void);
    
    virtual void returnKey(void);
    virtual void activate(void);  
    virtual void escape(void);
    virtual void up(void);
    virtual void down(void);
  };
  
friend class FieldEditor;
friend class CycleTimer;

public:
  MSEntryField(MSWidget *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSEntryField(MSWidget *,const char *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSEntryField(void);

  const MSFormat& format(void) const;
  const MSUnsignedLongVector& cycleColors(void) const;
  unsigned long cycleInterval(void) const;
  MSCycleColorMode cycleColorMode(void) const;
  MSBoolean editing(void) const;
  unsigned long editorBackground(void) const;
  unsigned long editorForeground(void) const;

  void format(const MSFormat& aFormat_);
  void cycleColors(const MSUnsignedLongVector&);
  void cycleColors(const MSStringVector&);
  void cycleInterval(unsigned long);
  void cycleColorMode(MSCycleColorMode);
  void editorBackground(unsigned long);
  void editorForeground(unsigned long);
  void editorBackground(const char *);
  void editorForeground(const char *);

  void inputMask(const MSString&);
  const MSString& inputMask(void) const;

  void inputMaskCharacter(char);
  char inputMaskCharacter(void) const;
  
  void      autoMaskInput(MSBoolean);
  MSBoolean autoMaskInput(void) const;
  
  void supportPasting(MSBoolean supportPasting_) {_supportPasting=supportPasting_;}
  int supportPasting(void) const {return _supportPasting;}

  unsigned long addEditorKeyCallback( const char*,MSKeyCallback*);
  void removeEditorKeyCallback(unsigned long);
  void removeEditorKeyCallback(const char*);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

protected:
  FieldEditor            *_editor;
  CycleTimer             *_cycleTimer;
  MSColorCycle           *_cycle;
  MSUnsignedLongVector    _cycleColors;
  unsigned long           _cycleInterval;
  MSCycleColorMode        _cycleColorMode;
  MSFormat                _format;
  MSBoolean               _autoMaskInput;

  MSColorCycle *cycle(void) const;
  CycleTimer *cycleTimer(void) const;
  FieldEditor *fieldEditor(void) const;

  void init(void);
  virtual void drawFieldValue(unsigned long,unsigned long);
  virtual void drawFieldValue(void);

  void removeCycle(void);
  void startCycle(const MSUnsignedLongVector& colors_);
  virtual void createCycle(void);
  void cycleValue(void);
  void processCycleTimer(void);

  virtual void update(const MSIndexVector&);
  virtual void updateData(void);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);

  virtual void placement(void);
  virtual void clearEditor(void);
  virtual void configureEditor(void);
  virtual void mapEditor(void);
  virtual void unmapEditor(void);
  virtual MSBoolean activateEditor(void);
  virtual void edit(void);
  virtual void editSelection(void);

  virtual void increment(void);
  virtual void decrement(void);
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);
  virtual void returnKey(void);  
  virtual void activate(void);
  virtual void escape(void);
  virtual void reference(void);
  virtual void valueChange(void);

  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual MSBoolean loseFocus(void);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void buttonPress(const XEvent *);
  virtual void button1Press(const XEvent *);
  virtual void button2Press(const XEvent *);
  virtual void button3Press(const XEvent *);
  virtual void startEditing(const XEvent *);

  virtual void generateInputMask(void);

  //The following are all newly added to support cut and paste
  int _selectionStart;
  int _selectionEnd;
  int _firstCharacter;
  int _insertCursor;
  MSBoolean _hadFocus;
  MSBoolean _supportPasting;
  MSBoolean _pasting;
  
  int selectionStart(void) const {return _selectionStart;}
  void selectionStart(int selectionStart_) {_selectionStart=selectionStart_;}

  int selectionEnd(void) const {return _selectionEnd;}
  void selectionEnd(int selectionEnd_) {_selectionEnd=selectionEnd_;}

  int firstCharacter(void) const {return _firstCharacter;}
  void firstCharacter(int firstCharacter_) {_firstCharacter=firstCharacter_;}

  MSBoolean hadFocus(void) const {return _hadFocus;}

  int insertCursor(void) const {return _insertCursor;}
  void insertCursor(int insertCursor_) {_insertCursor=insertCursor_;}

  int pasting(void) const {return _pasting;}
  void pasting(MSBoolean pasting_) {_pasting=pasting_;}

  void clearSelection(MSBoolean redrawAlways_=MSTrue);
  void setSelection(int,int);
  int locateCursorPosition(int,const MSString &);
  MSBoolean insertString(int,const MSString &);
  MSBoolean isSelected(void);
  void updateEditor(void);
  void trackSelection(const XEvent *);
  int displayableFieldWidth(const char *,int);
  MSBoolean scrollLeft(void);
  MSBoolean scrollRight(const MSString &,int &);

  virtual MSBoolean validateInput(MSString &);

  virtual void updateFont(Font);
  virtual void updateSensitivity(void);
  virtual void button1Release(const XEvent *);
  virtual void selectionClear(const XEvent *);  

  virtual const char *getPrimarySelection(MSString&, int&);
  virtual void insertPrimarySelection(void);

  virtual void currentColors(unsigned long &,unsigned long &);
};

#endif
