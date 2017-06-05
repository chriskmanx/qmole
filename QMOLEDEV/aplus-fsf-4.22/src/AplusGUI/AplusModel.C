///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSGUI/MSWidgetView.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusCommon.H>

extern long dbg_tkerr;
extern long dbg_tmstk;

// private default constructor to make sure that AplusModel is always constructed
// with an explicit argument
AplusModel::AplusModel()  {}

AplusModel::AplusModel(V var_)
{
  _aplusVar = var_;
  // the following line creates a problem for table columns and graph trace sets;
  // see todo.txt for details
  //
  //  if (_aplusVar!=0)  _aplusVar->o=0;
  _sflags = aplus_nl;
}


AplusModel::~AplusModel( void )
{
  if (aplusVar()!=0)
    { 
      unbindWidgetView();
      aplusVar(0);
    }

  dc(_sflags);
}

MSString AplusModel::asString(void) const    { return MSString(); }
MSString AplusModel::asDebugInfo(void) const { return MSString(); }
MSString AplusModel::asMSF(void) const       { return MSString(); }
MSString AplusModel::className(void) const   { return MSString("AplusModel"); }

const MSSymbol& AplusModel::type(void) const
{	
  return symbol();
}

MSModel *AplusModel::clone(void) const
{
  return new AplusModel(aplusVar());
}

MSModel *AplusModel::create(void) const
{
  return new AplusModel();
}

void AplusModel::assign(const MSModel& aModel_)
{
  aplusVar(((AplusModel&)aModel_).aplusVar());
}

long AplusModel::compare(const MSModel&) const
{ 
  if (dbg_tmstk) cout << "Warning: AplusModel::compare called" << endl;
  return 1;
}

MSError::ErrorStatus AplusModel::set(const char *)     
{
  if (dbg_tmstk) cout << "Warning: AplusModel::set called" << endl;
  return MSError::MSSuccess;
}


MSError::ErrorStatus AplusModel::setFromMSF(const char *)
{ 
  if (dbg_tmstk) cout << "Warning: AplusModel::setFromMSF called" << endl;
  return MSError::MSSuccess;
}


const MSSymbol& AplusModel::symbol(void)
{
  static MSSymbol sym("AplusModel");
  return sym;
}


AplusModel& AplusModel::operator=(const AplusModel& aModel_)
{
  if (dbg_tmstk) cout << "WARNING:  AplusModel::operator= called" << endl;
  return *this;
}


void AplusModel::bindWidgetView(MSWidgetView *pWidgetView_)
{
  if (_aplusVar!=0)
    {
      if (pAVarData()==0)
	{
	  _aplusVar->attr = new AVariableData;
	}

      pAVarData()->pWidgetView(pWidgetView_);
      _aplusVar->o=1;
      aplusVarList().append((unsigned long)_aplusVar);
    }
}


void AplusModel::unbindWidgetView(void)
{
  if (_aplusVar!=0)
    {
      if (dbg_tmstk) cout << "Removing Object Association" << endl;
      _aplusVar->o=0;
      AVariableData *pVarData = pAVarData();
      if (pVarData!=0)
	{
	  pVarData->pWidgetView(0);
	}
      
      MSUnsignedLongVector &varList = aplusVarList();
      varList.removeAt(varList.indexOf((unsigned long)_aplusVar));
    }
}


MSBoolean AplusModel::coupleWidgetView(MSWidgetView *pWidgetView_)
{
  AplusModel *pOldModel = (AplusModel *)pWidgetView_->model();
  V oldVar = (pOldModel==0) ? 0 : pOldModel->aplusVar();
  
  if (_aplusVar==0 || _aplusVar->a==0)
    {
      if (dbg_tkerr)   showError("Null V or A in AplusModel::coupleWidgetView");
      return MSFalse;
    }

  if (evaluate()==MSFalse)
    {
      if (dbg_tkerr)   showError("Error during dependency evaluation in AplusModel::coupleWidgetView");
      return MSFalse;
    }
  
  // Verify the data to see if it is bindable
  AplusVerifyEvent ave(_aplusVar,a());
  ((MSEventReceiver *)pWidgetView_)->receiveEvent(ave);
  
  if (ave.result()==MSTrue)
    {
      if (_aplusVar!=oldVar)
	{
	  if (pOldModel!=0)
	    {
	      pOldModel->unbindWidgetView();
	    }
	  
	  bindWidgetView(pWidgetView_);
	  return addReceiver(pWidgetView_);
	}
      else
	{
	  AplusUpdateDataEvent aude;
	  ((MSEventReceiver *)pWidgetView_)->receiveEvent(aude);
	  return MSTrue;
	}
    }
  else
    {
      if (dbg_tmstk) cout << "Verify failed in AplusModel::setData"<< endl;
      // the following line may possibly be moved to constructor; look for the
      // comment in constructor
      //
      if (_aplusVar!=0)  _aplusVar->o=0;
      aplusVar(oldVar);
      return MSFalse;
    }
}


MSUnsignedLongVector & AplusModel::aplusVarList(void)
{
  static MSUnsignedLongVector list;
  return list;
}


int AplusModel::numRows(void) const
{
  int t = a_type();
  int r = rank();

  if (t==Ct && r==1)       return 1;
  else if (r==1 || r==2)   return d0();
  else if (t==Et)          return numElmts();
  else if (r==0)           return 1;
  else                     return d0();
}


int AplusModel::numCols( void ) const
{ 
  int t = a_type();
  int r = rank();

  if (t==Ct || r==0)            return 1;
  else if (r==2)                return d1();
  else if (t==Et &&
	   isNull(a())==MSTrue) return 0;
  else if (t==Et)               return 1;
  else if (r==1)                return 1;
  else                          return d1();
}


int AplusModel::charLength( void ) const
{
  if (a_type()!=Ct)       return 0;
  else if (_aplusVar==0)  return 0;
  else if (rank()==0)     return numElmts();
  else if (rank()==1)     return d0();
  else                    return d1();
}


void AplusModel::doneCB(V v_,A d_,A i_,A p_)
{
  AFunction *doneFunc=getDoneFunc(v_);
  if (doneFunc!=0)
    {
      doneFunc->execute(v_,(d_!=0)?(A)ic(d_):d_,
			(i_!=0)?(A)ic(i_):i_,(p_!=0)?(A)ic(p_):p_);
      if (d_!=0) dc(d_);
      if (i_!=0) dc(i_);
      if (p_!=0) dc(p_);
    }
}

AInFunction *AplusModel::getInFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->inFunc()->func()!=0) return pVarData->inFunc();
    }

  return 0;
}

ACharStrFunction *AplusModel::getOutFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->outFunc()->func()!=0)
	{
	  return pVarData->outFunc();
	}
    }
  
  return 0;
}

AOutFunction *AplusModel::getTitleFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->titleFunc()->func()!=0)
	{
	  return pVarData->titleFunc();
	}
    }
  
  return 0;
}


AColorFunction *AplusModel::getFgFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->fgFunc()->func()!=0)
	{
	  return pVarData->fgFunc();
	}
    }
  
  return 0;
}


AColorFunction *AplusModel::getBgFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->bgFunc()->func()!=0)
	{
	  return pVarData->bgFunc();
	}
    }
  
  return 0;
}


AColorFunction *AplusModel::getTitleColorFunc(V v_)
{
  if (v_!=0)
   {
     AVariableData *pVarData = ::pAVarDataFromV(v_);
     if (pVarData!=0 && pVarData->titleColorFunc()->func()!=0)
       {
	 return pVarData->titleColorFunc();
       }
   }
  
  return 0;
}


AFontFunction *AplusModel::getFontFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->fontFunc()->func()!=0)
	{
	  return pVarData->fontFunc();
	}
    }
  
  return 0;
}


AFontFunction *AplusModel::getTitleFontFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->titleFontFunc()->func()!=0)
	{
	  return pVarData->titleFontFunc();
	}
    }
  
  return 0;
}


AReadOnlyFunction *AplusModel::getReadOnlyFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->roFunc()->func()!=0)
	{
	  return pVarData->roFunc();
	}
    }
  
  return 0;
}


AGeometryFunction *AplusModel::getGeometryFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->geoFunc()->func()!=0)
	{
	  return pVarData->geoFunc();
	}
    }

  return 0;
}


AFunction *AplusModel::getDoneFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->doneFunc()->func()!=0)
	{
	  return pVarData->doneFunc();
	}
    }
  
  return 0;
}


ACycleFunction *AplusModel::getCycleFunc(V v_)
{
  if (v_!=0)
    {
      AVariableData *pVarData = ::pAVarDataFromV(v_);
      if (pVarData!=0 && pVarData->cycleFunc()->func()!=0)
	{
	  return pVarData->cycleFunc();
	}
    }

  return 0;
}
