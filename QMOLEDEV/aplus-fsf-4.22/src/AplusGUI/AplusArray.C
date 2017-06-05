///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSUtil.H>
#include <MSTypes/MSCallback.H>
#include <MSTypes/MSMethodCallback.H>
#include <MSGUI/MSFontObject.H>
#include <AplusGUI/AplusArray.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/Macros.H>

A convertToPixels(const MSWidgetCommon *, A);
A grc(A,int,int);

extern long dbg_tmstk;

AplusArray::AplusArray(MSWidget *widget_) : MSArrayView(widget_)
{
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
  selectedRowBackground(AVariableData::defaultRowColor());
  //
  // Translating MSCallbacks to AplusCallbacks.
  //
  callback(MSWidgetCallback::doubleclick, new MSMethodCallback<AplusArray>(this, &AplusArray::referenceCB));      
}

AplusArray::~AplusArray(void)
{
}


MSBoolean AplusArray::verifyData(V,A a_)
{
  MSBoolean r = MSFalse;
  if ( 0!=a_ && QA(a_) )
    if (a_->t <= Et && a_->r <= 2) r = MSTrue;
  return r;
}


MSBoolean AplusArray::validate(const char *string_, unsigned row_, unsigned col_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  
  return validate(v, string_, row_, col_);
}


MSBoolean AplusArray::validate(V v_, const char *string_, unsigned row_,unsigned col_)
{
  extern int safeAset(V,A,A,A);
  
  if (v_!=0) 
   {
     AInFunction *inFunc;
     A inData=aplus_nl;
     
     if ((inFunc=AplusModel::getInFunc(v_))!=0) inData=inFunc->invoke(v_,(char *)string_,(int)row_,(int)col_);
     else inData=defaultInFunc(v_,string_);
     if (inData!=0 && isNull(inData)==MSFalse)
      {
	A index=grc((A)v_->a,row_,col_);
        inData=(A)ic(inData);
	if (safeAset(v_,inData,index,0)==0)
	{
	  showError(qs);
	  dc(inData);
	  if (index!=0) dc(index);	
	  return MSFalse;
	}
	else 
         {
	   ((AplusModel*)model())->doneCB(v_,inData,index,0);
	   dc(inData);
	   if (index!=0) dc(index);
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}


void AplusArray::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel*)m_));
  updateData();
}


void AplusArray::receiveEvent(MSEvent& event_)
{
  const MSSymbol& eventType = event_.type();

  if (eventType==AplusEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateEvent in AplusArray"  << endl;
      AplusEvent *ave = (AplusEvent *) &event_;
      V v     = ((AplusModel *)model())->aplusVar();
      A index = ave->index();
      A pick  = ave->pick();
      I ravel = ave->ravel();;
      update(v,index, pick, ravel);
    }
  else if (eventType==AplusVerifyEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received VerifyEvent in AplusArray"  << endl;
      
      AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
      ave->result(verifyData(ave->aplusVar(), ave->a()));
    }
  else if (eventType==AplusUpdateDataEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateDataEvent in AplusArray"  << endl;
      V v = ((AplusModel *)model())->aplusVar();
      if (v)
	{
	  AVariableData *varData = ::pAVarDataFromV(v);
	  if (varData)
	    {
	      columnWidth(varData->colWidth());
	      redraw();   // cause a redraw in case `stars attribute was set
	    }
	}
    }
  else if (eventType==AplusUpdateTitleEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateTitleEvent in AplusArray"  << endl;
      updateTitle();
    }
}

A AplusArray::defaultInFunc(V v_,const char *string_)
{
  char   *ptrchar=0;
  long    lnum=0;
  double  dnum=0.0;
  int     n=0;
  A       av,r=aplus_nl;
  
  if (v_!=0) 
   {
     av=(A)v_->a;
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
	n=(av->r==1?(int)av->d[0]:(int)av->d[1]); 
	r=gv(Ct,n);
	(void) memset((char *)r->p,' ',n);  // ' ' == 0x20 == 32 --> blankspace
	strncpy((char *)r->p,(char *)string_,strlen((char *)string_));
	break;
	
	case Et: // no attempt at conversion - nesting level ????
	r=gsv(0,(char *)string_);
	break;
	
	default:
	break;
      }
   }
  return (A) r;
}


const char *AplusArray::formatOutput(MSString& str_, unsigned i, unsigned j)
{
  static const char blank[]={" "};
  V v = (model()==0)?0:((AplusModel*)model())->aplusVar();
  ACharStrFunction *outFunc = AplusModel::getOutFunc(v);
  A outStr;
  AVariableData *vd    = ::pAVarDataFromV(v);
  unsigned long   type = (model()!=0)?((AplusModel*)model())->a_type():0;
  int 	    charlength = (model()!=0)?((AplusModel*)model())->charLength():0;
  int 	    rank       = (model()!=0)?((AplusModel*)model())->rank():0;
  P p;      p.i        = (model()!=0)?((AplusModel*)model())->data():0;
  int       nCols      = numColumns();
  int       w          = (rank==2)?nCols:1;
  int       offset;
  int       n;
  
  offset=(i*w)+j;
  switch(type)
   {
   case It:
     if (outFunc!=0)  outStr=outFunc->invoke(v,(int)p.i[offset],i,j);
     break;
   case Ft:
     if (outFunc!=0)  outStr=outFunc->invoke(v,(double)p.f[offset],i,j);
     break;
   case Ct:
     n=charlength;
     offset=i*n;
     if (outFunc!=0)  outStr=outFunc->invoke(v,p.c+(offset),n,i,j);
     break;
   case Et:
     if ((model()==0)?0:((AplusModel*)model())->numElmts()>0)
      {
	A d=gs(Et);
	*d->p=ic(p.a[offset]);
	if (outFunc!=0)  outStr=outFunc->invoke(v,d,i,j);   
	dc(d);
      }
     break;
   }
  
  str_ = (Ct==outStr->t) ? (char *)outStr->p : blank;
  dc(outStr);

  return str_;
}


void AplusArray::update(V v_,int row_,int col_,UpdateType type_)
{
  unsigned a_type = ((AplusModel*)model())->a_type();
  unsigned rank = ((AplusModel*)model())->rank();
  
  if (type_==ValueUpdate&&v_!=0)
   {
     if (row_==-1&&col_==-1) redrawImmediately(); 
     else
      {
	if (row_==-1)     
	 {
	   if (a_type==Ct) col_=0;
	   cycleColumn(col_);
	 }
	else if (col_==-1)
	 {
	   if (a_type==Ct&&rank==1) row_=0;
	   (rank==1)?cycleRowColumn(row_,0):cycleRow(row_);
	 }     
	else
	 {
	   if (a_type==Ct) 
	    {
	      col_=0;
	      if (rank==1) row_=0;
	    }
	   cycleRowColumn(row_,col_);
	 }
      }
    }
   else if (type_==ShapeUpdate)  shapeUpdate();
   else if (type_==AppendUpdate) appendUpdate();
}


void AplusArray::update(V v_,A index_,A,I ravel_)
{ 
  if(!index_) shapeUpdate();
  else if(ravel_) // ravel update
    {
      A a=(A)v_->a;
      if(a->r==2&&a->n==1) 
	{ 
	  int n=(int)a->d[1];
	  int k=(int)index_->p[0];
	  int j=k/n;
	  update(v_,j,k-n*j,ValueUpdate); 
	}	
      else redrawImmediately(); 
    }
  else 
    {
      A r=index_->t==It?index_:index_->n?(A)*index_->p:aplus_nl;
      A c=index_->t==Et&&index_->n>1?(A)index_->p[1]:aplus_nl;
      int i,j;
      if(isNull(c)==MSTrue) // c is aplus_nl - all cols are updated
	{     
	  if(isNull(r)==MSTrue) redrawImmediately(); 
	  else 
	    {      
	      for(i=0;i<(int)r->n;i++) if ((int)r->p[i]>=vsb()->max()) appendUpdate();
	      for(i=0;i<(int)r->n;i++) update(v_,(int)r->p[i],-1,ValueUpdate);
	    }	
	}
      else if(isNull(r)==MSTrue) // r is aplus_nl - all rows are updated
	{ 
	  for(i=0;i<(int)c->n;i++) update(v_,-1,(int)c->p[i],ValueUpdate);
	}	
      else 
	{
	  for(i=0;i<(int)r->n;i++) if ((int)r->p[i]>=vsb()->max()) appendUpdate();
	  for(j=0;j<(int)r->n;j++)
	    {
	      for(i=0;i<(int)c->n;i++)
		{
		  update(v_,(int)r->p[j],(int)c->p[i],ValueUpdate);
		}	
	    }	
	}
    }
}


MSBoolean AplusArray::isCellProtected(unsigned row_, unsigned col_)
{
  char *buf;
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  AVariableData *varData = ::pAVarDataFromV(v);
  MSBoolean ro=(varData!=0)?varData->readOnly():MSTrue;
  AReadOnlyFunction *roFunc=AplusModel::getReadOnlyFunc(v);

  if (v && roFunc!=0&&row_<numRows())
   {
     P p; p.i 	    = ((AplusModel*)model())->data();
     int rank 	    = ((AplusModel*)model())->rank();
     int type 	    = ((AplusModel*)model())->a_type();
     int charLength = ((AplusModel*)model())->charLength();
     int n          = ((AplusModel*)model())->numElmts();
     int w=rank==2?numColumns():1;
     int offset=row_*w+col_;

     
     switch (type)
      {
        case It:
          ro=(MSBoolean) roFunc->invoke(v,(int)p.i[offset],row_,col_);   
          break;        
	case Ft:
          ro=(MSBoolean) roFunc->invoke(v,(double)p.f[offset],row_,col_);
          break;       
	case Ct:
          offset=selectedRow() * charLength;
          buf=new char[charLength+1];
          strncpy(buf,p.c+(offset),charLength);
	  buf[charLength]= '\0';
          ro=(MSBoolean) roFunc->invoke(v,(char *)buf,row_,col_);   
          delete [] buf;
          break;        
        case Et:
	  if (n>0)
           {
             A d=gs(Et);
	     *d->p=ic(p.a[offset]);
     	     ro=(MSBoolean) roFunc->invoke(v,d,row_,col_);   
             dc(d);
	   }
	  break;
      }
   }
  return ro;  
}


void AplusArray::createCycle(int row_, int col_)
{
  AplusModel *pModel=(AplusModel *)model();
  if (pModel==0)
    {
      return;
    }

  V v = pModel->aplusVar();
  if (v==0)
    {
      return;
    }

  if (pModel->numElmts()>0)
    {
      ACycleFunction *cycleFunc=AplusModel::getCycleFunc(v);
      if (cycleFunc!=0 && cycleFunc->func()!=0)
	{
	  cycleColors(getCycleColors(row_,col_)); 
	}

      MSArrayView::createCycle(row_,col_);
    }
}


MSUnsignedLongVector AplusArray::getCycleColors(int row_, int col_)
{
  char *buf;
  A r=aplus_nl;
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  ACycleFunction *cycleFunc = AplusModel::getCycleFunc(v);
  
  if (cycleFunc && cycleFunc->func()!=0&&(row_<numRows()||row_==0))
   {
     P p; p.i=((AplusModel *)model())->data();
     int rank = ((AplusModel *)model())->rank();
     int type = ((AplusModel *)model())->a_type();
     int w=rank==2?numColumns():1;
     int offset=row_*w+col_;
     int n=0;
     
     switch(type)
      {
	case It:
	  r=cycleFunc->invoke(v,(int)p.i[offset],row_,col_);
	  break;
	case Ft:
	  r=cycleFunc->invoke(v,(double)p.f[offset],row_,col_);
	  break;
	case Ct:
	  n=((AplusModel*)model())->charLength();
	  offset=row_*n;
          buf=new char[n+1];
	  strncpy(buf,p.c+(offset),n);
	  buf[n]= '\0';
	  r=cycleFunc->invoke(v,(char *)buf,row_,col_);   
          delete [] buf;
	  break;
	case Et:
	  if (((AplusModel*)model())->numElmts()>0)
           {
             A d=gs(Et);
	     *d->p=ic(p.a[offset]);
             r=cycleFunc->invoke(v,d,row_,col_);   
             dc(d);
	   }
	  break;
      }
   }
  A pixelColors = (isNull(r)==MSFalse) ? convertToPixels(this,r) : r;
  MSUnsignedLongVector uv;

  if (isNull(pixelColors)==MSFalse)
   {
     for (unsigned i = 0; i < pixelColors->n; i++) 
       uv << (unsigned long) pixelColors->p[i];
     dc(pixelColors);
   }
  return uv;
}


unsigned long AplusArray::cellForeground(unsigned row_, unsigned col_)
{
  AplusModel *m = (AplusModel*) model();
  unsigned long fgcolor=foreground();
  int n=0;
  char *buf;

  if (m)
   {
     V v = m->aplusVar();
     AColorFunction *fgFunc=AplusModel::getFgFunc(v);
     if (fgFunc==0) return foreground();
     int w = m->rank()==2?numColumns():1;
     P p; p.i = m->data();
     int offset = (row_*w)+col_;
     switch(m->a_type())
      {
      case It:
	fgcolor=fgFunc->invoke(v,(int)p.i[offset],row_,col_);
	break;
      case Ft:
	fgcolor=fgFunc->invoke(v,(double)p.f[offset],row_,col_);
	break;
      case Ct:
	n=m->charLength();
	offset=row_*n;
	buf=new char[n+1];
	strncpy(buf,p.c+(offset),n);
	buf[n]= '\0';
	fgcolor=fgFunc->invoke(v,(char *)buf,row_,col_);   
	delete [] buf;
	break;
      case Et:
	if (m->numElmts()>0)
	 {
	   A d=gs(Et);
	   *d->p=ic(p.a[offset]);
	   fgcolor=fgFunc->invoke(v,d,row_,col_);
	   dc(d);
	 }
	break;
      }
   }
  return fgcolor;
}


unsigned long AplusArray::cellBackground(unsigned row_, unsigned col_)
{
  AplusModel *m = (AplusModel*) model();
  unsigned long bgcolor=background();
  int n=0;
  char *buf;

  if (m)
   {
     V v = m->aplusVar();
     AColorFunction *bgFunc=AplusModel::getBgFunc(v);
     if (bgFunc==0) return background();
     int w = m->rank()==2?numColumns():1;
     P p; p.i = m->data();
     int offset = (row_*w)+col_;
     switch(m->a_type())
      {
      case It:
	bgcolor=bgFunc->invoke(v,(int)p.i[offset],row_,col_);
	break;
      case Ft:
	bgcolor=bgFunc->invoke(v,(double)p.f[offset],row_,col_);
	break;
      case Ct:
	n=m->charLength();
	offset=row_*n;
	buf=new char[n+1];
	strncpy(buf,p.c+(offset),n);
	buf[n]= '\0';
	bgcolor=bgFunc->invoke(v,(char *)buf,row_,col_);   
	delete [] buf;
	break;
      case Et:
	if (m->numElmts()>0)
	 {
	   A d=gs(Et);
	   *d->p=ic(p.a[offset]);
	   bgcolor=bgFunc->invoke(v,d,row_,col_);
	   dc(d);
	 }
	break;
      }
   }
  return bgcolor;
}


Font AplusArray::cellFont(unsigned row_, unsigned col_)
{
  AplusModel *m=(AplusModel *)model();
  Font fid=font();
  int n=0;
  char *buf;

  if (m)
   {
     V v = m->aplusVar();
     AFontFunction *fontFunc=AplusModel::getFontFunc(v);
     if (fontFunc==0) return fid;
     int w = m->rank()==2?numColumns():1;
     P p; p.i = m->data();
     int offset = (row_*w)+col_;
     switch(m->a_type())
      {
      case It:
	fid=fontFunc->invoke(v,(int)p.i[offset],row_,col_);
	break;
      case Ft:
	fid=fontFunc->invoke(v,(double)p.f[offset],row_,col_);
	break;
      case Ct:
	n=m->charLength();
	offset=row_*n;
	buf=new char[n+1];
	strncpy(buf,p.c+(offset),n);
	buf[n]= '\0';
	fid=fontFunc->invoke(v,(char *)buf,row_,col_);   
	delete [] buf;
	break;
      case Et:
	if (m->numElmts()>0)
	 {
	   A d=gs(Et);
	   *d->p=ic(p.a[offset]);
	   fid=fontFunc->invoke(v,d,row_,col_);
	   dc(d);
	 }
	break;
      }
   }
  return fid;
}


MSClipMode AplusArray::columnClipMode(unsigned) const
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
	      return MSClipStars;
	    }	
	}
    }

  return MSNoClipping;
}


void AplusArray::updateForeground(unsigned long oldfg_)
{
  MSCompositeText::updateForeground(oldfg_);
  redrawImmediately();
}


void AplusArray::updateFont(Font oldfid_)
{
  MSCompositeText::updateFont(oldfid_);
  _rowHeight=textHeight()+(2*rowSpacing());

  if (firstMap()==MSTrue)
   {
     if (dynamic()==MSTrue || mapped()!=MSTrue)
      {
	computeSize();
      }
     else if (mapped()==MSTrue)
      {
	adjustNumVisible();
	redrawImmediately(); 
      }
   }
}


const MSSymbol& AplusArray::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusArray::symbol(void)
{
  static MSSymbol sym("AplusArray");
  return sym;
}


void AplusArray::defaultButton3Behavior(const XEvent *pEvent_) 
{
  startEditing(pEvent_);
}


void AplusArray::referenceCB(void)
{
  activateCallback(MSWidgetCallback::reference);
}


unsigned AplusArray::numRows(void) const
{
  return (model()==0)?0:((AplusModel*) model())->numRows();
}


unsigned AplusArray::numColumns(void) const
{ 
  return (model()==0)?0:((AplusModel*) model())->numCols();
}
