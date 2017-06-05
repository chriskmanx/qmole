///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <pwd.h>
#include <AplusGUI/AplusPassword.H>

PasswordEditor::PasswordEditor(MSWidget *owner_) : MSEntryField::FieldEditor(owner_)
{
  _fillChar='*';
}

PasswordEditor::~PasswordEditor(void)
{}

const char *PasswordEditor::string(void)
{
  if (length()!=_stringBuffer.length())
   {
     _stringBuffer.leftJustify(length(), fillChar());
   }
  return _stringBuffer;
}

void PasswordEditor::fillChar(char ch_)
{ 
  if (fillChar()!=ch_)
   {
     _fillChar=(ch_>0)?ch_:'\0';
     _stringBuffer=MSString(0,length(),_fillChar);
     clearTextArea();
     redraw(); 
   }
}


AplusPassword::AplusPassword(MSWidget *owner_) : AplusEntryField(owner_)
{
  _valid=MSFalse;
  if (passwordEditor()!=0) delete _editor;
  _editor=new PasswordEditor(this);
}

AplusPassword::~AplusPassword(void)
{}

void AplusPassword::fillChar(char ch_)
{
  passwordEditor()->fillChar(ch_);
}

MSBoolean AplusPassword::verifyData(V v_,A a_)
{
  return (0!=v_&&0!=a_&&QA(a_)&&a_->t==Ct&&a_->r<=1)?MSTrue:MSFalse;
}


void AplusPassword::button1Press(const XEvent *pEvent_)
{
  if (activateCallback(MSWidgetCallback::reference)==MSFalse)
    {
      AplusEntryField::button1Press(pEvent_);
    }	
}


// We have to override activate() method in order to change what is passed to validateInput():
// MSEntryField::activate() uses _editor->string(), but that will return '*' characters for
// the password field; we need to use _editor->text(), which will return the actual string that
// was typed.
//
void AplusPassword::activate(void)
{
  if (_editor->mapped()==MSTrue)
    {
      if (hasModel()==MSTrue)
	{
	  MSString aString=_editor->text();
	  if (validateInput(aString)==MSTrue)
	    {
	      unmapEditor();
	      valueChange();
	    }
	}
      else
	{
	  escape();
	}
    }
}


MSBoolean AplusPassword::validate(const char *string_)
{
  AplusModel *pModel=(AplusModel *)model();
  if (pModel!=0)
    {
      A a=pModel->a();
      if (a!=0 && a->t==Ct && isNull(a)==MSFalse)
	{
	  busyOn();
	  _valid=checkPassword((const char *)a->p,string_);
	  busyOff();
	}
    }

  return MSTrue;
}


#ifdef _AIX
extern "C" const char *crypt(const char *key_,const char *salt_);
#else
// extern "C" char *crypt(const char *key_,const char *salt_);
#endif

extern "C" int kerberosCheck(const char *, const char *);

MSBoolean AplusPassword::checkPassword(const char *username_,const char *password_)
{
  struct passwd *pwd;
#if defined(APLUS_THREAD_SAFE_FUNCTIONS)
  struct passwd pwdStruct;
  char charBuf[1024];
#endif //APLUS_THREAD_SAFE_FUNCTIONS
  
  // check for missing or unknown user 
  APLUS_GETPWNAM(username_,&pwdStruct,charBuf,1024,pwd);
  if ((username_==0)||pwd==(struct passwd *)(0))
   { return MSFalse; }

  // check for trivial password protection on this user 
  if ((pwd->pw_passwd==0)||(pwd->pw_passwd[0]=='\0')) 
   { return MSTrue; }

  // check for no password provided or incorrect password provided 
  if (strcmp(pwd->pw_passwd,"*")==0) 
   {
     if (kerberosCheck(username_,password_)!=0) return MSFalse;
   }
  else 
   {
//      if (strcmp(crypt(password_,(const char *)pwd->pw_passwd),pwd->pw_passwd)!=0) return MSFalse;
   }

  return MSTrue;
}


const MSSymbol& AplusPassword::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusPassword::symbol(void)
{
  static MSSymbol sym("AplusPassword");
  return sym;
}
