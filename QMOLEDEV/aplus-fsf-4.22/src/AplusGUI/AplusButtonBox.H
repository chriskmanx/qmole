#ifndef AplusButtonBoxHEADER
#define AplusButtonBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSActionBox.H>
#include <MSGUI/MSActionButton.H>
#include <MSGUI/MSToggleButton.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>

class AplusButtonBox : public MSActionBox
{
public:
  AplusButtonBox(MSWidget *);
  ~AplusButtonBox(void);
  virtual void receiveEvent(MSEvent &);
  virtual MSBoolean verifyData(V,A);
  virtual void addSenderNotify(MSEventSender *);
  const MSUnsignedLongVector& buttons(void) const { return _buttons; }
  MSUnsignedLongVector& buttons(void) { return _buttons; }
  virtual void updateData(void);
  virtual void activate(void);
  virtual void activate(MSActionButton *b_) { MSActionBox::activate(b_); }
  int callAset(V,A,A,A);
  A geometry(void)    { return _geometry; }
  void geometry(A a_) { if (_geometry!=0) dc(_geometry); _geometry=(A)ic(a_); }
  virtual void naturalSize(void);
  int selectedItem(void) { return _selectedItem; }
  void selectedItem(int selectedItem_) { _selectedItem = selectedItem_; }
  virtual void updateTitle(void);

  void buttonHighlightThickness(int);
  int buttonHighlightThickness(void);

  void buttonShadowThickness(int);
  int buttonShadowThickness(void);

  void highlightColor(unsigned long);
  unsigned long highlightColor(void);

  MSBoolean readOnly(int row_) const;

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  void updateValues(void);
  virtual void updateValue(int);
  virtual void update(V,A,A,I);
  virtual void update(const MSIndexVector &v_) { MSActionBox::update(v_); }
  int  numRows(void);
  A itemLabel(int);
  Font titleFont(int row_);
  unsigned long titleColor(int row_);
  void colSpan(int index_,A am_,int *col_,int *span_);
  void rowSpan(int index_,A am_,int *row_,int *span_);
  MSBoolean setPositions(void);
  MSBoolean compareGeometry(A ag_);
  MSBoolean setGeometry(void);
  virtual MSActionButton *newButton(void);
  virtual void valueChange(MSWidget *, MSBoolean);
  virtual void setButtonState(MSActionButton *, unsigned long) {}
  virtual unsigned long color(unsigned row_);

  MSBoolean freezeIfUnfrozen(void);
  void unfreezeIfUnfrozen(MSBoolean);
  virtual void updateSensitivity(void);

private:
  int _selectedItem;
  MSUnsignedLongVector _buttons;
  A _geometry;

  STANDARD_WIDGET_METHODS_H
};

class AplusActionButton : public MSActionButton
{
friend class AplusButtonBox;

public:
  AplusActionButton(MSWidget *, const char *);
  ~AplusActionButton(void);

protected:
  const AplusButtonBox *buttonBox(void) const { return (AplusButtonBox *) _owner; }
  AplusButtonBox *buttonBox(void) { return (AplusButtonBox *) _owner; }
  virtual void focusIn(void);
  virtual MSBoolean isProtected(void) const;
};



inline MSBoolean AplusButtonBox::freezeIfUnfrozen(void) { MSBoolean rc=frozen(); return freeze(),rc; }
inline void AplusButtonBox::unfreezeIfUnfrozen(MSBoolean bool_) { if (bool_==MSFalse) unfreeze(); }

#endif // AplusButtonBoxHEADER
