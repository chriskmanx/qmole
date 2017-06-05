///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusText.H>

extern long dbg_tmstk;

AplusText::AplusText(MSWidget *owner_) : MSText(owner_)
{
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusText::~AplusText(void)
{}

MSBoolean AplusText::verifyData(V,A a_)
{
  return (0!=a_&&QA(a_)&&a_->t==Ct&&a_->r==1)?MSTrue:MSFalse;
}

void AplusText::updateData(void)
{
  AplusModel *am=(AplusModel *) model();

  if (am && am->aplusVar() && am->a())
    string((char *)am->a()->p);
  else string("");
}

void AplusText::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusText"  << endl;
     updateData();
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusText"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}

void AplusText::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusText::doClearSelection()
{
  unsigned row,col;
  positionToRowCol(selectionStart(),row,col);
  moveCursor(row,col);
  text().remove(selectionStart(),selectionLength());
  resetLinesFrom(row);
  clearSelection();
  return;
}

// keyPress() is overriden here to provide proper key translation for APL characters.
// MSText widget currently does not support that.
//
void AplusText::keyPress(const XEvent *e_,KeySym k_,unsigned int state_,const char *b_)
{
  MSKeyPress keyPress(k_,state_);
  if (isProtected()==MSFalse)
    {
      MSBoolean hasMatch = keyTranslationTable()->hasMatch(keyPress);
      if ((hasMatch == MSTrue))
	{
	  MSBoolean cleared=MSFalse;
	  if (selectionLength()>0)
            {
              doClearSelection();
              cleared=MSTrue;
            }
	  MSBoolean done = MSFalse;
	  if( cleared==MSTrue && (k_==XK_BackSpace || k_==XK_Delete) )  
	    done = MSTrue;
	  else 
	    done = keyTranslate(keyPress);
	  if ( strlen(b_)>0 && done == MSFalse ) 
	    insertString(b_);
	  return;
	}
      else if (MSTextField::keyTranslationFunction()!=0)
	{
	  const char *tString=(*MSTextField::keyTranslationFunction())(e_);
	  if (tString!=0)
	    {
	      if (selectionLength()>0)
		{
		  doClearSelection();
		}
	      insertString(tString);
	      return;
	    }
	}

      if (strlen(b_)>0)
	{
	  if (selectionLength()>0)
	    {
	      doClearSelection();
	    }
	  insertString(b_);
	}
    }
  else if(sensitive() == MSTrue) 
    {
      //We are "ReadOnly" but still sensitive, so process Up/Down,etc....
      keyTranslate(keyPress); 
    }
}


MSBoolean AplusText::isProtected(void) const
{
  if (_model==0)
    {
      return MSFalse;
    }

  AplusModel *pModel = (AplusModel *)_model;
  V v = pModel->aplusVar();

  if (v==0)
    {
      return MSFalse;
    }

  AVariableData *varData = ::pAVarDataFromV(v);
  if (varData==0)
    {
      return MSFalse;
    }

  AReadOnlyFunction *roFunc=AplusModel::getReadOnlyFunc(v);

  if (roFunc==0)	// if there is no function installed
    {
      return varData->readOnly(); // return the stored value
    }
  else	// A+ function is installed
    {
      return roFunc->invoke(v,pModel->a());
    }
}


const MSSymbol& AplusText::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusText::symbol(void)
{
  static MSSymbol sym("AplusText");
  return sym;
}
