#ifndef AplusSlotHEADER
#define AplusSlotHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSStringEntryField.H>
#include <MSGUI/MSCompositeFieldBox.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/Macros.H>

class AplusSlot;

class AplusSlotEntryField : public MSStringEntryField
{
  friend class AplusSlot;
  
public:
  AplusSlotEntryField(MSWidget *);
  ~AplusSlotEntryField(void);

  AplusSlot *slot(void) { return (AplusSlot *)_owner; }
  const AplusSlot *slot(void) const { return (AplusSlot *)_owner; }
  
protected:
  virtual void updateForeground(unsigned long);
  virtual void keyPress(const XEvent *, KeySym, unsigned int, const char *);
  virtual MSBoolean isProtected(void) const;
  void label(const char *pString_);

  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);

  virtual void focusIn(void);

  virtual void increment(void);
  virtual void decrement(void);

  virtual MSBoolean validate(const char *);
};

//////////////////////////////////////////////////////////////////////////////

class AplusSlot : public MSCompositeFieldBox
{
  friend class AplusSlotEntryField;
  
protected:
  A                    _cycleColors;
  unsigned long        _cycleInterval;
  ACycleFunction       _cycleFunc;
  MSCycleColorMode     _cycleColorMode;
  MSUnsignedLongVector _fields;
  A                    _geometry;
  MSBoolean            _childInFocus;
  
  MSBoolean childInFocus(void)               { return _childInFocus;}
  void childInFocus(MSBoolean childInFocus_) { _childInFocus = childInFocus_;}
  MSUnsignedLongVector &fields(void)   	     { return _fields; }
  const MSUnsignedLongVector &fields(void) const  { return _fields; }
  A geometry(void)    		       	     { return _geometry; }
  void geometry(A a_) {if (_geometry!=0) dc(_geometry); _geometry=(A)ic(a_);}
  void removeAllCycles(void);
  void startCycle(int row_,A colors_);
  void createCycle(int row_);
  MSBoolean setGeometry(void);
  MSBoolean compareGeometry(A);
  MSBoolean setPositions(void);
  void rowSpan(int,A,int *,int *);
  void colSpan(int,A,int *,int *);
  virtual void firstMapNotify(void);
  unsigned long color(unsigned);
  virtual void tab(void);
  virtual void shiftTab(void);
  virtual void takeFocus(void);
  virtual MSBoolean freezeIfUnfrozen(void);
  virtual void unfreezeIfUnfrozen(MSBoolean bool_);

  void setClipMode(void);
  
public:  
  AplusSlot(MSWidget *owner_);
  ~AplusSlot(void);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);
  
  void cycleColors(A colors_);
  A cycleColors(void);
  unsigned long cycleInterval(void) { return _cycleInterval; }
  void cycleInterval(unsigned long interval_);
// cycleColorMode() method does not yet exist for MSEntryField, but will.
//  unsigned long cycleColorMode(void) { return _cycleColorMode; }
//  void cycleColorMode(unsigned long interval_);

  ACycleFunction *cycleFunc(void) { return &_cycleFunc; }
  void cycleFunc(AFunc func_,AClientData *arg_)
   { cycleFunc()->func(func_); cycleFunc()->arg(arg_); }

  int valueLength(void);
  int editLength(void);

  virtual Font valueFont(int);
  virtual A itemValue(int row_);
  virtual A itemLabel(int row_);
  virtual A cycleColor(int row_);
  unsigned long titleColor(int row_);
  Font titleFont(int row_);
  MSBoolean readOnly(int row_) const;
  
  virtual void updateFunctionalAttributes(int row_);

  virtual void update(V v_,A index_,A pick_,I ravel_); 
  virtual void update(V,int,int,UpdateType) {}
  virtual void update(const MSIndexVector &v_) { 
    MSCompositeFieldBox::update(v_); }
  virtual void updateValue(int row_);
  virtual void updateValues(void);
  virtual void updateTitle(void);
  virtual void updateFont(Font);
  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void updateSensitivity(void);
 
  virtual void updateData(void);
  virtual A defaultInFunc(const char *string_,int row_);

  void highlightThickness(int);
  void shadowThickness(int);
  void highlightColor(unsigned long);

  int highlightThickness(void);
  int shadowThickness(void);
  unsigned long highlightColor(void);

  virtual void editorBackground(unsigned long pixel_);
  virtual void editorForeground(unsigned long pixel_);
  virtual unsigned long editorBackground(void);
  virtual unsigned long editorForeground(void);
  virtual void naturalSize(void);
  MSBoolean validate(AplusSlotEntryField *, const char *);
  virtual MSBoolean verifyData(V,A);

  virtual void receiveEvent(MSEvent &);
  virtual void addSenderNotify(MSEventSender *);
  int numRows(void);

  MSBoolean editing(void);
  void startEditing(void);
  void stopEditing(void);
  int selectedItem(void) { return _selectedItem; }
  void selectedItem(int selectedItem_) { _selectedItem = selectedItem_; }

  A arrowButtons(void) { return _arrowButtons; }
  void arrowButtons(A);

  virtual void redraw(void);

protected:
  A _arrowButtons;
  int _selectedItem;

  STANDARD_WIDGET_METHODS_H
};

#endif //AplusSlotHEADER
