///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSKeyTranslationTable.H>
#include <MSGUI/MSKeyTableData.H>
#include <MSTypes/MSHashTable.H>
#include <MSTypes/MSNodeList.H>
#include <stdio.h>

static const int KTableSize=64;

MSHashTable *MSKeyTranslationTable::_pHashTable=0; 

MSKeyTranslationTable::MSKeyTranslationTable(void) :
    _pListHead(0)
{
  if (_pHashTable==0) _pHashTable=new MSHashTable(KTableSize);
}

MSKeyTranslationTable::~MSKeyTranslationTable(void)
{
  if (_pListHead!=0)
   {
     MSNodeItem     *hp=_pListHead;
     MSNodeItem     *np;
     MSKeyTableData *tp;
     while ((np=hp->next())!=hp)
      {
	tp=(MSKeyTableData *)np->data();
	if (tp!=0) 
	 {
	   if (tp->referenceCount()==1) _pHashTable->remove((char *)tp->name());
	   tp->removeReference();
	 }
	delete np;
      }
     delete _pListHead;
   }
}

MSKeyTableData *MSKeyTranslationTable::data(const char *pName_) 
{ return (MSKeyTableData *)_pHashTable->lookup(pName_); }

MSBoolean MSKeyTranslationTable::keyTableData(const char *pName_) 
{ return (_pHashTable->lookup(pName_)==0)?MSFalse:MSTrue; }


void MSKeyTranslationTable::addKeyTableData(const char* newKeyTableDataName_)
{
  MSKeyTableData* newKeyTableData = data(newKeyTableDataName_);
  if ( newKeyTableData == 0) newKeyTableData = new MSKeyTableData(newKeyTableDataName_);
  add(newKeyTableData);
}

void MSKeyTranslationTable::removeKeyTableData(const char* keyTableDataName_)
{
  MSKeyTableData* keyTableData = data(keyTableDataName_);
  if ( keyTableData!=0)remove(keyTableData);
}

void MSKeyTranslationTable::add(MSKeyTableData *pData_)
{
  if (_pListHead==0) _pListHead=new MSNodeItem;
  MSNodeItem *hp=_pListHead;
  MSNodeItem *np=hp;
  MSNodeItem *nnp=0;
  
  while((np=np->next())!=hp)
   {
     if (pData_==(MSKeyTableData *)np->data())
      {
        nnp=np;
	np->remove();
      }
   }  
  if (nnp==0)
   {
     nnp=new MSNodeItem((void *)pData_);
     pData_->addReference();
   }
  nnp->insert(hp->next());
  if ((unsigned long)_pHashTable->lookup((char *)pData_->name())==_pHashTable->notFound())
   {
     _pHashTable->add((char *)pData_->name(),(void *)pData_);
   }
}

void MSKeyTranslationTable::remove(MSKeyTableData *pData_)
{
  if (_pListHead!=0)
   {
     MSNodeItem *hp=_pListHead;
     MSNodeItem *np=hp;
     while((np=np->next())!=hp)
      {
	if (pData_==(MSKeyTableData *)np->data())
	 {
	   np->remove();
	   pData_->removeReference();
	   delete np;
	   break;
	 }
      }  
   }
}



MSBoolean MSKeyTranslationTable::translate( const MSKeyPress &keyTranslation_,MSWidget * widget_) const
{
  if (_pListHead!=0)
   {
     MSNodeItem *hp=_pListHead;
     MSNodeItem *np=hp; 
     
     if (hp!=hp->next())
      {
	MSKeyTableData        *data;

	while ((np=np->next())!=hp)
	  {
	    data=(MSKeyTableData *) np->data();
	    if (data->process(keyTranslation_,widget_)==MSTrue)
	      return MSTrue;
	  }
      }
   }
     return MSFalse;  
}


MSBoolean MSKeyTranslationTable::hasMatch(const MSKeyPress &keyTranslation_) const
{
  if (_pListHead!=0)
   {
     MSNodeItem *hp=_pListHead;
     MSNodeItem *np=hp;
     if (hp!=hp->next())
      {
	MSKeyTableData        *data;
	
	while ((np=np->next())!=hp)
	 {
	   data=(MSKeyTableData *) np->data();
	   if (data->hasMatch(keyTranslation_) == MSTrue) return MSTrue;
	 }
      }
   }
  return MSFalse;
}

void MSKeyTranslationTable::removeAll()
{
  if (_pListHead!=0)
   {
     MSNodeItem *hp=_pListHead;
     MSNodeItem *np=hp;
     while((np=np->next())!=hp)
      {
	   np->remove();
	   ((MSKeyTableData *)np->data())->removeReference();
	   delete np;
      }  
   }
}

unsigned long  MSKeyTranslationTable::addCallback( const char* specificationString_, MSKeyCallback* keyCallback_,
                                                   const char* pKeyTableDataName_)
{
  MSKeyTableData *pKeyTableData;
  if(pKeyTableDataName_==0)
   {
     char defaultTable[32];
     sprintf(defaultTable,"%lu",(unsigned long)this);
     if ((pKeyTableData=data(defaultTable))==0)
      {
        pKeyTableData = new MSKeyTableData(defaultTable);       
        add(pKeyTableData);
      }
   }
  else if ((pKeyTableData=data(pKeyTableDataName_))==0)
   {
     pKeyTableData=new MSKeyTableData(pKeyTableDataName_);
     add(pKeyTableData);
   }
  pKeyTableData->add(specificationString_,keyCallback_);
  return (unsigned long)keyCallback_;
}


void MSKeyTranslationTable::removeAllCallbacksThat(const char*specificationString_)
{
  MSKeyTableData *pKeyTableData;
  if (_pListHead!=0)
   {
     MSNodeItem *hp=_pListHead;
     MSNodeItem *np=hp;
     while ((np=np->next())!=hp)
      {
        pKeyTableData=(MSKeyTableData *) np->data();
        // only MSKeyCallback's that are referenced by one MSKeyTranslationTable can be removed
        if (pKeyTableData->referenceCount()==1)   pKeyTableData->remove(specificationString_);
      }
   }
}  

void MSKeyTranslationTable::removeCallback(unsigned long identifier_, const char* keyTableDataName_)
{
  MSKeyTableData *pKeyTableData;
  if(keyTableDataName_!=0)
   {
     pKeyTableData = data(keyTableDataName_);
     if (pKeyTableData!=0 && pKeyTableData->referenceCount()==1)   pKeyTableData->remove(identifier_);
   }
  else
   {
     pKeyTableData = defaultKeyTableData();
     if (pKeyTableData!=0 )   pKeyTableData->remove(identifier_);
   }  
}

void MSKeyTranslationTable::removeCallback(const char* specificationString_, const char* keyTableDataName_)
{
  MSKeyTableData *pKeyTableData;
  if(keyTableDataName_!=0)
   {
     pKeyTableData = data(keyTableDataName_);
     if (pKeyTableData!=0 && pKeyTableData->referenceCount()==1)   pKeyTableData->remove(specificationString_);
   }
  else
   {
     pKeyTableData = defaultKeyTableData();
     if (pKeyTableData!=0 )   pKeyTableData->remove(specificationString_);
   }  
}

MSKeyTableData*  MSKeyTranslationTable::defaultKeyTableData(void) const
{
  char defaultTable[32];
  sprintf(defaultTable,"%lu",(unsigned long)this);
  return data(defaultTable);
}
