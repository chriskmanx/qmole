///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSGUI/MSApplication.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusButton.H>

extern long dbg_tmstk;

AplusButton::AplusButton(MSWidget *widget_) : MSButton(widget_)
{
  AplusModel *am = new AplusModel(0);
  INTERNAL_COUPLE(am);
}
AplusButton::~AplusButton(void)
{}

void AplusButton::receiveEvent(MSEvent& event_)
{
  const MSSymbol& eventType=event_.type();

  if (eventType==AplusEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateEvent in AplusButton"  << endl;
      MSLabel::updateData();
    }
  else if (eventType==AplusVerifyEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received VerifyEvent in AplusButton"  << endl;

      AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
      ave->result(verifyData(ave->aplusVar(), ave->a()));
    }
  else if (eventType==AplusUpdateTitleEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateTitleEvent in AplusButton" << endl;
      updateTitle();
    }
}

void AplusButton::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel*)m_));
  updateData();
  updateTitle();
}

MSBoolean AplusButton::verifyData(V,A a_)
{
  MSBoolean r=MSFalse;
  if (0!=a_&&QA(a_)&&a_->t==Et)
   {
     if (a_->n==0) r=MSTrue;
     else if (a_->n<=2)
      {
	A *ap=(A *)a_->p;
	A fun=(A)ap[0];
	if (QA(fun)&&fun->t>=Xt) r=MSTrue;	      
      }
   }
  return r;
}

void AplusButton::activate(void)
{
  V v=(model()!=0)?((AplusModel*)model())->aplusVar():0;

  if (armed()==MSTrue&&v!=0)
   {
     A av=((AplusModel *)model())->a();
     if (av->n>0)
      {     
        A cd=aplus_nl;
	A *ap=(A *)av->p;
	A fun=(A)ap[0];
	if (av->n>=2) cd=(A)ap[1];     
	callAFunc(fun,cd);  
      }
   }

  activateCallback(MSWidgetCallback::activate);
}


void AplusButton::updateTitle(void)
{
  V v=(model()!=0)?((AplusModel *)model())->aplusVar():0;
  if (v)
    {
      AVariableData *varData = pAVarDataFromV(v);
      
      if (isNull(varData->titleAFont())==MSTrue)
	{
	  font(server()->defaultFont());
	}
      else font(varData->titleFont());
      if (isNull(varData->titleAFg())==MSFalse)
	foreground(varData->titleFg());
      updateData();
   }
} 


A AplusButton::getTitle(void) const
{
  A r=aplus_nl;
  V v=(model()!=0)?((AplusModel *)model())->aplusVar():0;
  if (v)
   {
     AVariableData *varData = pAVarDataFromV(v);
     r=varData->title();
   }

  return r;
  
}

void AplusButton::callAFunc(A fun_,A cd_)
{
  typedef void *XClientArg;
  typedef void *XCallArg;
  extern void ACallback(XCallArg,XClientArg);
  AClientData *ac=new AClientData(model()->aplusVar(),(A)fun_,(A)cd_);       
  ACallback(this,ac);
  delete ac;
}


MSBoolean AplusButton::isProtected(void) const
{
  V v = model()->aplusVar();
  A a = model()->a();
  
  AVariableData *varData = pAVarDataFromV(v);  
  AReadOnlyFunction *roFunc=model()->getReadOnlyFunc(v);

//   return (roFunc!=0)?(MSBoolean)roFunc->invoke(v,a):varData->readOnly();
  if ( roFunc!=0 )
    return (MSBoolean)roFunc->invoke(v,a);
  else if ( varData->readOnly()==MSTrue || sensitive()==MSFalse )
    return MSTrue;
  else
    return MSFalse;
}


int AplusButton::numRows(void) const
{
  A a = getTitle();
  if (a!=0)
   {
     int type = a->t;
     int rank = a->r;
     int n = a->n;
  
     if (type==Et) return (int)n;
     else if (type==Ct) return (rank<=1)?1:(int)a->d[0];
   }
  return 0;
}


int AplusButton::numColumns(int row_) const
{
  A a = getTitle();

  if (a!=0&&row_<numRows())  
   {
     int type = a->t;
     int rank = a->r;
     I *data =  a->p;

     if (type==Et)
      {
        P p; p.i=data;
	A ra=p.a[row_];
	return (ra->r==0)?1:(int)ra->d[0];
      }
     else if (type==Ct)
      {
        if (rank==0) return 1;
        else if (rank==1) return (int)a->d[0];
	else return (int)a->d[1];
      }
   }
  return 0;
}

int AplusButton::numColumns(void) const
{
  A a = getTitle();
  
  if (a!=0)  
   {
     int type = a->t;
     int rank = a->r;
     int n = a->n;
     I *data = a->p;

     if (type==Et)
      {
        P p; p.i=data;
	int r=0;
        for (int i=0;i<n;i++)
	 {
           if (p.a[i]->r==0) r=(1>r)?1:r;
           else r=(p.a[i]->d[0]>r)?(int)p.a[i]->d[0]:r;
	 }
        return r;
      }
     else if (type==Ct)
      {
        if (rank==0) return 1;
        else if (rank==1) return (int)a->d[0];
	else return (int)a->d[1];
      }
   }
  return 0;
}

const char *AplusButton::formatOutput(MSString& str_, int row_)
{
  A a = getTitle();
  //  MSString *buf = (MSString *) &_buffer;

  if (a!=0&&row_<numRows())
   {
     int type = a->t;
     int rank = a->r;
     I *data = a->p;

     P p; p.i=data;
     int len=numColumns(row_);
     switch (type)
      {
	case Ct:
	  if (rank<=1) str_ = MSString((char *)p.c, len);
          else if (rank==2) str_ = MSString (((char *)p.c+(row_*len)),len);
	  break;
	case Et:
          str_ = MSString ((char *)p.a[row_]->p,len);
	  break;
	default:
	  break;
      }
   }
  return str_;
}


const MSSymbol& AplusButton::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusButton::symbol(void)
{
  static MSSymbol sym("AplusButton");
  return sym;
}
