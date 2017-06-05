///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSTime.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusEntryField.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>

extern long dbg_tmstk;

int safeAset(V,A,A,A);
A convertToPixels(const MSWidgetCommon *, A);

AplusEntryField::AplusEntryField(MSWidget *widget_) : MSEntryFieldPlus(widget_)
{
  backingStore(WhenMapped);
  AplusModel *am= new AplusModel(0);
  dynamic(MSTrue);
  supportPasting(MSFalse);
  INTERNAL_COUPLE(am);
}

AplusEntryField::~AplusEntryField(void)
{}

MSBoolean AplusEntryField::verifyData(V,A)
{ return MSTrue; }


void AplusEntryField::edit(void)
{
  MSEntryFieldPlus::edit();
}


MSBoolean AplusEntryField::activateEditor(void)
{
  return MSEntryFieldPlus::activateEditor();
}


MSBoolean AplusEntryField::validate(const char *string_)
{
  if (model() && ((AplusModel*)model())->aplusVar()!=0)
   {
     V v=((AplusModel*)model())->aplusVar();
     A r=aplus_nl;
     AInFunction *inFunc;
     if ((inFunc=((AplusModel *) model())->getInFunc(v))!=0)
       r=inFunc->invoke(v,(char *)string_,-1,-1);
     else r=defaultInFunc(v,string_);

     if (r==0 || isNull(r)==MSTrue) 
       return MSFalse;
     else 
      {
        r=(A)ic(r);
	if (safeAset(v,r,0,0)==0)
	 {
	   showError(qs);
	   dc(r);
	   return MSFalse;
	 }
	else 
         {
           ((AplusModel*)model())->doneCB(v,r,0,0);
	   dc(r);
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

void AplusEntryField::receiveEvent(MSEvent &event_)
{
  const MSSymbol& eventType = event_.type();

  if (eventType==AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusEntryField"  << endl;
     update(MSIndexVector::nullVector());
   }
  else if (eventType==AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusEntryField"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
  else if (eventType==AplusUpdateDataEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateDataEvent in AplusEntryField" << endl;
      setClipMode();
    }
  else if (eventType==AplusUpdateTitleEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateDataEvent in AplusEntryField" << endl;
      updateTitle();
    }
}


const char *AplusEntryField::formatOutput(MSString& str_)
{
  static const char blank[]={" "};
  A outStr=aplus_nl;
  if (model() && ((AplusModel *)model())->aplusVar())
  {
     V v=((AplusModel*)model())->aplusVar();
     A a=((AplusModel*)model())->a();
     ACharStrFunction *outFunc=((AplusModel *)model())->getOutFunc(v);
     if (outFunc!=0)
      {
        outStr=(A)outFunc->invoke(v,a);
        outStr=(outStr->t==Ct)?outStr:aplus_nl;
      }
  }
  if (isNull(outStr)==MSTrue) return (char *) 0;

  str_ = (Ct==outStr->t) ? (const char *)outStr->p : blank;
  dc(outStr);
  return str_;
}


void AplusEntryField::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *)m_));
  label(itemLabel());
  setClipMode();
  updateTitle();
}


A AplusEntryField::defaultInFunc(V v_,const char *string_)

{
  if (v_==0) return aplus_nl;

  char   *ptrchar=0;
  long    lnum=0;
  double  dnum=0.0;
  A r=aplus_nl;
  A av=(A)v_->a;

  switch(av->t)
   {
     case It:
       lnum=strtol((char *)string_,&ptrchar,10); // Base 10
       if (ptrchar==(char *)string_) 
        {
          r=aplus_nl;
          showError("Unknown Number: Integer Expected");
        }
       else r=gi((int)lnum);
       break;

     case Ft:
       dnum=strtod((char *)string_,&ptrchar);
       if (ptrchar==(char *)string_) 
        {
          r=aplus_nl;
          showError("Unknown Number: Float Expected");
        }
       else r=gf((double)dnum);
       break;

     case Ct:
       r=gsv(0,(char *)string_);
       break;

     case Et: // possible null object
       if (av->n==0) r=gsv(0,(char *)string_);
       break;
       
     default:
       break;
   }
  return (A) r;
}

const char *AplusEntryField::itemLabel(void)
{
  A outStr=aplus_nl;
  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v!=0)
   {
     AVariableData *varData = ::pAVarDataFromV(v);
     A str=varData->title();
     if (isNull(str)==MSFalse && str->t==Ct) outStr=(A)ic(str);
   }
  return (isNull(outStr)==MSFalse) ? (char *)outStr->p : 0;
}


void AplusEntryField::setClipMode(void)
{
  AplusModel *pModel=(AplusModel *)model();
  if (pModel!=0)
    {
      V v=pModel->aplusVar();
      if (v!=0)
	{
	  AVariableData *varData = ::pAVarDataFromV(v);
	  if (varData->stars()==MSTrue)
	    {
	      clipMode(MSClipStars);
	    }
	  else
	    {
	      clipMode(MSNoClipping);
	    }
	}
    }
}


void AplusEntryField::updateTitle(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  MSBoolean changed = MSFalse;

  if (v)
   {
     AVariableData *varData = ::pAVarDataFromV(v);
     const char *l = itemLabel();
     if (l&&label()!=MSStringVector(l))
      {
	changed = MSTrue;
	label(itemLabel());
      }
     
     // UpdateTitleFont
     Font fid = varData->titleFont();
     if (fid && fid != labelFont())
      {
	if (labelFont()!=fid)
	{
	  changed=MSTrue;
	  labelFont(fid);
	}
      }

     //UpdateTitleForeground
     unsigned long fg = titleForeground();
     labelForeground(fg);

     if (changed==MSTrue)
     {
       if (dynamic()==MSTrue) computeSize();
       else redraw();
     }
   }
}


void AplusEntryField::firstMapNotify(void)
{
  MSString str;
  const char *pString = formatOutput(str);
  // Make sure the first string fits in the field
  if (pString!=0)
    {
      AplusModel *pModel = (AplusModel *)model();
      if (pModel!=0 && pModel->aplusVar()!=0)
	{
	  AVariableData *vd = pModel->pAVarData();
	  valueWidth(vd->colWidth());
	}
    }

  MSEntryFieldPlus::firstMapNotify();
}


MSBoolean AplusEntryField::isProtected(void) const
{
  MSBoolean ro = MSFalse;
  
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  A a = (model()!=0)?((AplusModel*)model())->a():aplus_nl;

  if (v)
   {
     AVariableData *varData = ::pAVarDataFromV(v);  
     ro=varData->readOnly();
     AReadOnlyFunction *roFunc=AplusModel::getReadOnlyFunc(v);
     if (roFunc!=0) ro=(MSBoolean) roFunc->invoke(v,a);
   }
  return ro;
}


void AplusEntryField::createCycle(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  A a = (model()!=0)?((AplusModel*)model())->a():aplus_nl;
  ACycleFunction *cycleFunc = AplusModel::getCycleFunc(v);
  MSUnsignedLongVector uv;
  
  if (v!=0)
   {
     if (cycleFunc && cycleFunc->func()!=0)
      {
	A colors = cycleFunc->invoke(v,a);
	A pixelColors = (isNull(colors)==MSFalse) ? convertToPixels(this,colors) : colors;
	if (isNull(pixelColors)==MSFalse)
	 {
	   for (unsigned i = 0; i < pixelColors->n; i++) 
	   uv << (unsigned long) pixelColors->p[i];
	   dc(pixelColors);
	 }
	cycleColors(uv);
      }
   }

  MSEntryFieldPlus::createCycle();
}

void AplusEntryField::updateForeground(unsigned long oldfg_)
{
  MSComposite::updateForeground(oldfg_);
  if (oldfg_==editorBackground()) editorBackground(foreground());
  _fieldValue->foreground(foreground());
  redraw();
}

void AplusEntryField::updateFont(Font oldfid_)
{
  MSComposite::updateFont(oldfid_);
  _fieldValue->font(font());
  XSetFont(display(),drawGC(),font());
  if (dynamic()==MSTrue) computeSize();
  else redraw();
}


void AplusEntryField::increment(void)
{
  if (callback(MSWidgetCallback::increment)!=0)
    {
      activateCallback(MSWidgetCallback::increment);
    }
  else
    {
      activateCallback(MSWidgetCallback::reference);
    }
}


void AplusEntryField::decrement(void)
{
  if (callback(MSWidgetCallback::decrement)!=0)
    {
      activateCallback(MSWidgetCallback::decrement);
    }
  else
    {
      activateCallback(MSWidgetCallback::reference);
    }
}


void AplusEntryField::currentColors(unsigned long &fg_, unsigned long &bg_)
{
  if (cycle()!=0 && cycle()->count()<cycle()->numCycles())  // cycle colors
    {
      fg_=cycle()->color(cycle()->count());
      bg_=valueBackground();
    }
  else	// regular colors
    {
      fg_=foreground();
      V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
      if (v)
	{
	  AColorFunction *fgFunc=AplusModel::getFgFunc(v);
	  if (fgFunc!=0)
	    {
	      fg_=(unsigned long)fgFunc->invoke(v,(A)v->a);
	    }
	}

      bg_=valueBackground();
    }
}


const MSSymbol& AplusEntryField::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusEntryField::symbol(void)
{
  static MSSymbol sym("AplusEntryField");
  return sym;
}
