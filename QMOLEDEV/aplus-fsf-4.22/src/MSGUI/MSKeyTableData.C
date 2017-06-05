///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1998-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <memory.h>
#include <string.h>
#include <MSGUI/MSKeyCallback.H>
#include <MSGUI/MSKeyTableData.H>


static const int MSKeyTranslationListSize=128;

//#############################################################################
// MSKeyCallbackNode
//#############################################################################
class MSKeyCallbackNode 
{
protected:
  MSKeyCallback *_keyCallback;
  KeySym                 _keysym;
  unsigned int           _mask;
  unsigned int           _flag;

public:
  MSKeyCallbackNode(const char*, MSKeyCallback *);
  ~MSKeyCallbackNode(void);

  KeySym keysym()                const {return _keysym;}
  unsigned int mask()            const {return _mask;}
  unsigned int flag()            const {return _flag;}
  MSKeyCallback *callback() const {return _keyCallback;}
  
  MSBoolean isMatch(const MSKeyPress&) const;
  MSBoolean isExactMatch(KeySym, unsigned int, unsigned int ) const;  
};



//#############################################################################
// MSKeyCallbackNode
//#############################################################################
MSKeyCallbackNode::MSKeyCallbackNode(const char* pTranslationString_, MSKeyCallback *keyCallback_):
  _keyCallback(keyCallback_)
{ MSKeyPress::translate(pTranslationString_,_keysym,_mask,_flag); }

MSKeyCallbackNode::~MSKeyCallbackNode(void)
{ if (callback()!=0) delete _keyCallback; }

MSBoolean MSKeyCallbackNode::isMatch(const MSKeyPress& keyTranslation_) const
{
  return MSKeyPress::isMatch(keyTranslation_.keysym(),keyTranslation_.state()
				   ,keysym(),mask(),flag());
}

MSBoolean MSKeyCallbackNode::isExactMatch(KeySym keysym_,unsigned int mask_,
					unsigned int flag_) const
{  return ( keysym_ == keysym() && mask_ == mask() && flag_ == flag() ) ? MSTrue : MSFalse ; }






//#############################################################################
// MSKeyTableData
//#############################################################################

MSKeyTableData::MSKeyTableData(const char *pName_) :
_referenceCount(0)    
{
  if (pName_!=0)
   {
     int n=strlen(pName_);
     _pName=new char[n+1];
     memcpy(_pName,pName_,n);
     _pName[n]='\0';
   }
  else _pName=0;
}

MSKeyTableData::~MSKeyTableData(void)
{ if (_pName!=0) delete [] _pName; }

void MSKeyTableData::addReference(void)
{ _referenceCount++; }
void MSKeyTableData::removeReference(void)
{ if (--_referenceCount==0) delete this; }

void MSKeyTableData::add(const char* pTranslationString_,MSKeyCallback *keyCallback_)
{ _ktList.add(new MSKeyCallbackNode(pTranslationString_,keyCallback_)); }

void MSKeyTableData::remove( unsigned long identifier_)
{
  MSKeyCallbackNode *tp;
  for (int i=0;i<_ktList._count;i++)
    {
     tp=_ktList._array[i];
     if (tp != 0 && (unsigned long)tp->callback()==identifier_) 
      { 
        _ktList.remove(tp);
	 delete tp;
	 break;
      }     
    }
}


void MSKeyTableData::remove( KeySym keysym_,unsigned int mask_, unsigned int flag_)
{
  MSKeyCallbackNode *tp;
  for (int i=0;i<_ktList._count;i++)
    {
     tp=_ktList._array[i];
     if (tp != 0 && tp->isExactMatch(keysym_,mask_,flag_) == MSTrue) 
       { 
	 _ktList.remove(tp);
	 delete tp;
       }     
    }
}

void  MSKeyTableData::remove( const char * pString_)
{
  KeySym keysym;
  unsigned int mask,flag;
  MSKeyPress::translate(pString_,keysym,mask,flag);
  remove(keysym,mask,flag);
} 

MSBoolean MSKeyTableData::process(const MSKeyPress& keyTranslation_,MSWidget* widget_) const
{
  MSKeyCallbackNode *tp;
  for (int i=0;i<_ktList._count;i++)
   {
     tp=_ktList._array[i];
     if (tp->isMatch(keyTranslation_)==MSTrue && tp->callback()->process(widget_,keyTranslation_) == MSTrue)  return MSTrue;     
   }
  return MSFalse;
}

MSBoolean MSKeyTableData::hasMatch(const MSKeyPress& keyTranslation_) const
{
  MSKeyCallbackNode *tp;
  for (int i=0;i<_ktList._count;i++)
   {
     tp=_ktList._array[i];
     if (tp->isMatch(keyTranslation_)==MSTrue )  return MSTrue;     
   }
  return MSFalse;
}

//#############################################################################
// MSKeyTableData::List
//#############################################################################
MSKeyTableData::List::List(int size_)
{
  _size=(size_==0)?MSKeyTranslationListSize:size_;
  _count=0;
  _array=new MSKeyCallbackNode*[_size];
  for (int i=0;i<_size;i++) _array[i]=0;
}

MSKeyTableData::List::~List(void)
{
  for (int i=0;i<_count;i++) 
   {
     delete _array[i];
     _array[i]=0;
   }
  delete [] _array;
  _array=0; 
  _size=0;
  _count=0;
}

void MSKeyTableData::List::reserve(int size_)
{
  if (_size<size_)
   {
     int newSize=(_size==0)?size_<<1:_size<<1;
     int i;
     MSKeyCallbackNode **array=new MSKeyCallbackNode*[newSize];
     for (i=0;i<_size;i++) 
      {
        array[i]=_array[i];
        _array[i]=0;
      }
     for (i=_size;i<newSize;i++) array[i]=0;
     delete [] _array;
     _array=array;
     _size=newSize;
   }
}

void MSKeyTableData::List::add(MSKeyCallbackNode *data_)
{
  reserve(_count+1);
  _array[_count]=data_;
  _count++;
}

void MSKeyTableData::List::remove(MSKeyCallbackNode *data_)
{
  for (int i=0;i<_count;i++)
   {
     if (_array[i]==data_) 
      {
        for (int j=i;j<_count-1;j++) _array[j]=_array[j+1];
        _array[_count-1]=0;
        _count--;
	break;
      }
   }
}
