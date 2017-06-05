#ifndef MSCompositeFieldHEADER
#define MSCompositeFieldHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSTextRect.H>
#include <MSGUI/MSComposite.H>
#include <MSTypes/MSSymbol.H>

class MSCompositeFieldBox;

class MSGUIExport MSCompositeField : public MSComposite
{
friend class MSCompositeFieldBox;
public:
  class MSGUIExport FieldValue : public MSTextRect
  {
  friend class MSCompositeField;
  protected:
    MSShadow       _shadow;
    MSShadowStyle  _shadowStyle;
    int            _shadowThickness;
    
  public:
    FieldValue(MSWidget *);
    ~FieldValue(void);
  
    virtual void naturalSize(void);
    virtual void updateBackground(unsigned long);

    const MSShadow& shadow(void) const;

    MSShadowStyle shadowStyle(void) const;    
    int marginWidth(void) const;
    int highlightThickness(void) const;
    int shadowThickness(void) const;
    int offset(void) const;

    void shadowThickness(int);
    void shadowStyle(MSShadowStyle);
  };
  
  class MSGUIExport FieldLabel : public MSTextRect
  {
  friend class MSCompositeField;
  public:
    FieldLabel(MSWidget *);
    FieldLabel(MSWidget *,const char *);
    FieldLabel(MSWidget *,const MSString&);
    ~FieldLabel(void);
  
    virtual void naturalSize(void);

    int naturalWidth(void) const;
    int marginWidth(void) const;
    int highlightThickness(void) const;
    int shadowThickness(void) const;
    int offset(void) const;
  };
  
  friend class FieldLabel;
  friend class FieldValue;

public:
  MSCompositeField(MSWidget *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  MSCompositeField(MSWidget *,const char *,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~MSCompositeField(void);

  virtual void redraw(void);
  virtual void naturalSize(void);

  const MSSymbol& tag(void) const;
  void tag(const MSSymbol&);

  void clipIndicatorForeground(const char *);
  void clipIndicatorForeground(unsigned long);
  unsigned long clipIndicatorForeground(void) const;
  
  void editWidth(unsigned);
  unsigned editWidth(void) const;

  void label(const char *);
  void label(const MSString&);
  const MSString& label(void) const;

  void labelForeground(const char *);
  void labelForeground(unsigned long);
  unsigned long labelForeground(void) const;

  void labelFont(const char *);  
  void labelFont(Font);  
  Font labelFont(void) const;

  void labelAlignment(MSAlignment);
  MSAlignment labelAlignment(void) const;

  void labelJustification(MSAlignment);
  MSAlignment labelJustification(void) const;

  void labelSpacing(unsigned);
  unsigned labelSpacing(void) const;

  void marginHeight(unsigned);  
  unsigned marginHeight(void) const;  

  void valueAlignment(MSAlignment);
  MSAlignment valueAlignment(void) const;

  void valueBackground(unsigned long);
  void valueBackground(const char *);
  unsigned long valueBackground(void) const;

  void valueFont(const char *);  
  void valueFont(Font);  
  Font valueFont(void) const;

  void valueForeground(unsigned long);
  void valueForeground(const char *);
  unsigned long valueForeground(void) const;

  void valueShadowStyle(MSShadowStyle);
  MSShadowStyle valueShadowStyle(void) const;

  void valueShadowThickness(int);
  int valueShadowThickness(void) const;  

  void valueWidth(unsigned);
  unsigned valueWidth(void) const;

  void clipMode(MSClipMode);
  MSClipMode clipMode(void) const;
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);
  
protected:
  MSSymbol               _tag;
  FieldValue            *_fieldValue;
  FieldLabel            *_fieldLabel;
  MSAlignment            _labelAlignment;
  MSAlignment            _labelJustification;
  unsigned               _labelSpacing;
  MSAlignment            _valueAlignment;  
  unsigned               _valueWidth;
  unsigned               _editWidth;  
  unsigned               _marginHeight;
  unsigned long          _clipIndicatorForeground;
  MSClipMode             _clipMode;
  GC                     _drawGC;

  GC drawGC(void) {return _drawGC;}

  FieldValue *fieldValue(void) const;
  FieldLabel *fieldLabel(void) const;

  void init(void);

  virtual unsigned computeValuePixelWidth(void);
  virtual unsigned computeLabelPixelWidth(void);
  virtual void labelPixelWidth(unsigned);

  virtual void drawFieldValueShadow(void);  
  virtual void drawFieldValue(void);
  virtual void drawFieldValue(unsigned long,unsigned long);
  virtual void drawFieldLabel(void);
  virtual void computeSize(void);
  virtual void adjustSize(void);
  virtual void placement(void);
  virtual void configure(void);

  virtual MSBoolean validate(const char *);
  virtual const char *formatOutput(MSString &);

  virtual void updateFont(Font);
  virtual void updateBackground(unsigned long);
  virtual void updateForeground(unsigned long);
  virtual void updateSensitivity(void);
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual MSBoolean loseFocus(void);
  virtual void firstMapNotify(void);
};

#endif






