#ifndef AplusPasswordHEADER
#define AplusPasswordHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusEntryField.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

class PasswordEditor : public MSEntryField::FieldEditor
{
  friend class AplusPassword;
  
public:
  PasswordEditor(MSWidget *parent_);
  ~PasswordEditor(void);

  char fillChar( void ) const       { return _fillChar; }
  void fillChar( char ch_ );

  virtual const char *string( void );

protected:
  char       _fillChar;
  MSString   _stringBuffer;

};


class AplusPassword : public AplusEntryField
{
public:
  AplusPassword(MSWidget *);
  ~AplusPassword(void);

  MSBoolean valid( void ) { return _valid; }
  char fillChar( void ) { return passwordEditor()->fillChar(); }
  void fillChar( char ch_ );

  virtual const char *formatOutput(MSString& str_) { return 0; }
  virtual MSBoolean verifyData(V v_,A a_);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  MSBoolean         _valid;

  PasswordEditor *passwordEditor( void ) { return (PasswordEditor *)_editor; }
  MSBoolean checkPassword(const char *username_,const char *password_);

  virtual void button1Press(const XEvent *);
  virtual MSBoolean validate(const char *);
  //
  // need to override activate() in order to do validation properly;
  // see the comment in the method's definition
  //
  virtual void activate(void);
};

#endif // AplusPasswordHEADER
