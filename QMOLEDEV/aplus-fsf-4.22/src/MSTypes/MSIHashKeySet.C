#ifndef MSIHashKeySetIMPLEMENTATION
#define MSIHashKeySetIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997 -2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

// helps us guarantee that the hash table size is a power of 2
// thus,we can value&(size-1) for the hash value,which is
// faster than value%size

#include <MSTypes/MSIHashKeySet.H>
inline unsigned computeHashSize(unsigned requestedSize_)
{
  unsigned newSize=1;
  while (newSize<requestedSize_) newSize<<=1;
  return newSize;
}

template<class Element,class Key>
MSIHashKeySet<Element,Key>::
MSIHashKeySet(unsigned long n)
{
  createHashtable(::computeHashSize(n));
}

template<class Element,class Key>
MSIHashKeySet<Element,Key>::
MSIHashKeySet(MSIHashKeySet<Element,Key> const& h)
{
  createHashtable(h.ivNoEntries);
  copyHashtable(h);
}

template<class Element,class Key>
MSIHashKeySet<Element,Key>::
~MSIHashKeySet()
{
  removeAll();
  ivOps.removeBlock(ivTable,ivNoEntries*sizeof(Node*));
  ivOps.removeBlock(ivCollList,ivNoEntries*sizeof(unsigned long));
}

template<class Element,class Key>
MSIHashKeySet<Element,Key>&
MSIHashKeySet<Element,Key>::
operator=(MSIHashKeySet<Element,Key> const& h)
{
  if (this!=&h)
   {
     removeAll();
     copyHashtable(h);
   }
  return *this;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
add(Element const& element,unsigned long hashvalue,MSIHashKeySet<Element,Key>::Cursor& cursor)
{
  Node* node=ivOps.newNode(element);
  if (ivTable[hashvalue]!=0)
   {
     ivCollList[hashvalue]++;
   }

  //
  // link element in bucket
  //
  node->ivNext=ivTable[hashvalue];
  ivTable[hashvalue]=node;
  ivNoElements++;

  //
  // Let cursor point to newly added element
  //
  cursor.ivEntryNumber=hashvalue;
  cursor.ivNode=node;

  if (ivNoEntries*2<ivNoElements) resize(node,cursor.ivEntryNumber);
  return MSTrue;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
add(Element const& element,unsigned long hashvalue)
{
  Node* node=ivOps.newNode(element);
  if (ivTable[hashvalue]!=0)
   {
     ivCollList[hashvalue]++;
   }

  //
  // link element in bucket
  //
  node->ivNext=ivTable[hashvalue];
  ivTable[hashvalue]=node;
  ivNoElements++;

  if (ivNoEntries*2<ivNoElements)
   {
    unsigned long dummy;
    resize(node,dummy);
  }
  return MSTrue;
}

template <class Element,class Key>
void MSIHashKeySet<Element,Key>::
addAllFrom(MSIHashKeySet<Element,Key> const& hashtable)
{
  MSICollectionCheck( (this!=&hashtable) ,"identical collection")
  Cursor c(*this);
  for (unsigned long i=0;i<hashtable.ivNoEntries;i++)
   {
     Node* tmp=hashtable.ivTable[i];
     while (tmp!=0)
      {
        //
        // New element is added at the start of the bucket.
        //
        add(hashtable.ivOps.elementAt(tmp),ivOps.getHashvalue(ivOps.keyAt(tmp),ivNoEntries),c);
        tmp=tmp->ivNext;
      }
   }
  return;
}

template <class Element,class Key>
Element const& MSIHashKeySet<Element,Key>::
anyElement() const
{
  Cursor cursor(*this);
  setToFirst(cursor);
  return ivOps.elementAt(cursor.ivNode);
}

template <class Element,class Key>
void MSIHashKeySet<Element,Key>::
removeAt(MSIHashKeySet<Element,Key>::Cursor const& cursor)
{
  if (cursor.ivNode==ivTable[cursor.ivEntryNumber])
   {
     //
     // remove first element in a bucket.
     //
     ivTable[cursor.ivEntryNumber]=cursor.ivNode->ivNext;
   }
  else
   {
     //
     // remove element somewhere in bucket,find previous
     //

     Node* prev=ivTable[cursor.ivEntryNumber];
     while (prev->ivNext!=cursor.ivNode)
      {
        //
        // find previous of element
        //
        prev=prev->ivNext;
      }

     //
     // link next of previous to next of element to remove
     //
     prev->ivNext=cursor.ivNode->ivNext;
   }
  ivOps.deleteNode(cursor.ivNode);
  ivNoElements--;
  if (ivCollList[cursor.ivEntryNumber] > 0)
   {
     ivCollList[cursor.ivEntryNumber]--;
   }
  return;
}

template <class Element,class Key>
unsigned long MSIHashKeySet<Element,Key>::
removeAll(void* predicate,void* env)
{
  Cursor c,next;
  unsigned long removed=0;
  for (setToFirst(c),setToFirst(next);c.ivNode!=0;c=next)
   {
     setToNext(next);
     if (ivOps.constantFunctionIteration(predicate,env,c.ivNode))
      {
        removeAt(c);
        removed++;
      }
   }
  return removed;
}

template <class Element,class Key>
void MSIHashKeySet<Element,Key>::
removeAll()
{
  for (unsigned long i=0;i<ivNoEntries;i++)
   {
     Node* tmp1=ivTable[i];
     while (tmp1!=0)
      {
        Node* tmp2=tmp1->ivNext;
        ivOps.deleteNode(tmp1);
        tmp1=tmp2;
      }
     ivTable[i]=0;
     ivCollList[i]=0;
   }
  ivNoElements=0;
  return;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
setToFirst(MSIHashKeySet<Element,Key>::Cursor& cursor) const
{
  cursor.ivEntryNumber=0;
  if (isEmpty())
   {
     //
     // setToFirst will return very fast on an empty hashtable
     //
     cursor.ivNode=0;
   }
  else
   {
     //
     // find first non-empty bucket
     //
     while (ivTable[cursor.ivEntryNumber]==0 &&
            cursor.ivEntryNumber<ivNoEntries-1)
      {
        cursor.ivEntryNumber++;
      }
     cursor.ivNode=ivTable[cursor.ivEntryNumber];
   }

  return cursor.ivNode!=0?MSTrue:MSFalse;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
setToNext(MSIHashKeySet<Element,Key>::Cursor& cursor) const
{
  cursor.ivNode=cursor.ivNode->ivNext;
  if (cursor.ivNode==0) {
    //
    // end of the bucket,
    // try to find new non-empty bucket
    //
    while (cursor.ivEntryNumber<ivNoEntries-1)
     {
       cursor.ivEntryNumber++;
       if (ivTable[cursor.ivEntryNumber]!=0)
        {
          cursor.ivNode=ivTable[cursor.ivEntryNumber];
          break;
        }
    }
  }
  return cursor.ivNode!=0?MSTrue:MSFalse;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
containsElementWithKey(Key const& key,unsigned long hashvalue) const
{
  Cursor cursor(*this);
  return locateElementWithKey(key,hashvalue,cursor);
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
containsAllKeysFrom(MSIHashKeySet<Element,Key> const& hashtable) const
{
  Cursor cursor(*this);
  if (hashtable.setToFirst(cursor))
   {
     Key const* key;
     do
      {
        key=&ivOps.keyAt(cursor.ivNode);
      }
     while (containsElementWithKey(*key,ivOps.getHashvalue(*key,ivNoEntries))
            && hashtable.setToNext(cursor));
   }
  return cursor.ivNode==0?MSTrue:MSFalse;
}

template <class Element,class Key>
unsigned long MSIHashKeySet<Element,Key>::
numberOfElementsWithKey(Key const& key,unsigned long hashvalue) const
{
  unsigned long n;
  Cursor cursor(*this);
  if (locateElementWithKey(key,hashvalue,cursor))
   {
     n=1;
     while (locateNextElementWithKey(key,hashvalue,cursor))
      {
        n++;
      }
   }
  else
   {
     n=0;
   }
  return n;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
locateElementWithKey(Key const& key,unsigned long hashvalue,Node *& pNode) const
{
  pNode=ivTable[hashvalue];
  // while element available and key not equal to argument key try next one
  while (!(pNode==0 || ivOps.isKeyEqualToKey(pNode,key))) pNode=pNode->ivNext;
  return pNode!=0?MSTrue:MSFalse;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
replaceElementWithKey(Element const& element,unsigned long hashvalue,MSIHashKeySet<Element,Key>::Cursor& cursor)
{
  MSBoolean rv;
  if (locateElementWithKeyOfElement(element,hashvalue,cursor))
   {
     replaceAt(cursor,element);
     rv=MSTrue;
   }
  else
   {
     rv=MSFalse;
   }
  return rv;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
replaceElementWithKey(Element const& element,unsigned long hashvalue)
{
  MSBoolean rv;
  Cursor cursor(*this);
  if (locateElementWithKeyOfElement(element,hashvalue,cursor))
   {
     replaceAt(cursor,element);
     rv=MSTrue;
   }
  else
   {
     rv=MSFalse;
   }
  return rv;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
locateOrAddElementWithKey(Element const& element,unsigned long hashvalue,MSIHashKeySet<Element,Key>::Cursor& cursor)
{
  MSBoolean rv=locateElementWithKeyOfElement(element,hashvalue,cursor);
  if (!rv) add(element,hashvalue,cursor);
  return rv;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
locateOrAddElementWithKey(Element const& element,unsigned long hashvalue)
{
  Cursor cursor(*this);
  MSBoolean rv=locateElementWithKeyOfElement(element,hashvalue,cursor);
  if (!rv) add(element,hashvalue);
  return rv;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
addOrReplaceElementWithKey(Element const& element,unsigned long hashvalue,MSIHashKeySet<Element,Key>::Cursor& cursor)
{
  MSBoolean rv;
  if (locateElementWithKeyOfElement(element,hashvalue,cursor))
   {
     replaceAt(cursor,element);
     rv=MSFalse;
   }
  else
   {
     rv=add(element,hashvalue,cursor);
     rv=MSTrue;
   }
  return rv;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
addOrReplaceElementWithKey(Element const& element,unsigned long hashvalue)
{
  MSBoolean rv;
  Cursor cursor(*this);
  if (locateElementWithKeyOfElement(element,hashvalue,cursor))
   {
     replaceAt(cursor,element);
     rv=MSFalse;
   }
  else
   {
     rv=add(element,hashvalue);
     rv=MSTrue;
   }
  return rv;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
removeElementWithKey(Key const& key,unsigned long hashvalue)
{
  MSBoolean rv;
  Cursor cursor(*this);
  if (locateElementWithKey(key,hashvalue,cursor))
   {
     removeAt(cursor);
     rv=MSTrue;
   }
  else
   {
     rv=MSFalse;
   }
  return rv;
}

template <class Element,class Key>
Element const& MSIHashKeySet<Element,Key>::
elementWithKey(Key const& key,unsigned long hashvalue) const
{
  Node *pNode=ivTable[hashvalue];
  // while element available and key not equal to argument key,
  // try next one
  while (!(pNode==0||ivOps.isKeyEqualToKey(pNode,key))) pNode=pNode->ivNext;
  MSICollectionCheck(pNode,"key not contained");
  return ivOps.elementAt(pNode);
}

template <class Element,class Key>
Element& MSIHashKeySet<Element,Key>::
elementWithKey(Key const& key,unsigned long hashvalue)
{
  Node *pNode=ivTable[hashvalue];
  // while element available and key not equal to argument key,
  // try next one
  while (!(pNode==0||ivOps.isKeyEqualToKey(pNode,key))) pNode=pNode->ivNext;
  MSICollectionCheck(pNode,"key not contained");
  return ivOps.elementAt(pNode);
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
locateNextElementWithKey(Key const& key,unsigned long,MSIHashKeySet<Element,Key>::Cursor& cursor) const
{
  do
   {
     cursor.ivNode=cursor.ivNode->ivNext;
   }
  while (!(cursor.ivNode==0 || ivOps.isKeyEqualToKey(cursor.ivNode,key)));
  return cursor.ivNode!=0?MSTrue:MSFalse;
}

template <class Element,class Key>
unsigned long MSIHashKeySet<Element,Key>::
removeAllElementsWithKey(Key const& key,unsigned long hashvalue)
{
  Cursor cursor,next;
  unsigned long count=0;

  cursor.ivEntryNumber=hashvalue;
  cursor.ivNode=ivTable[cursor.ivEntryNumber];

  if (cursor.ivNode!=0)
   {
     next=cursor;
     do
      {
        setToNext(next);
        if (ivOps.isKeyEqualToKey(cursor.ivNode,key))
         {
           removeAt(cursor);
           count++;
         }
        cursor=next;
      }
     while (cursor.ivNode!=0 && cursor.ivEntryNumber==hashvalue);
   }
  return count;
}

template <class Element,class Key>
unsigned long MSIHashKeySet<Element,Key>::
numberOfDifferentKeys() const
{
  Cursor c(*this);
  unsigned long result=0;
  for (setToFirst(c);c.ivNode!=0;setToNextWithDifferentKey(c)) result++;
  return result;
}

template <class Element,class Key>
MSBoolean MSIHashKeySet<Element,Key>::
setToNextWithDifferentKey(MSIHashKeySet<Element,Key>::Cursor& cursor) const
{
  Key const& key=ivOps.keyAt(cursor.ivNode);
  do
   {
     setToNext(cursor);
   }
  while (cursor.ivNode!=0 && ivOps.isKeyEqualToKey(cursor.ivNode,key));
  return cursor.ivNode!=0?MSTrue:MSFalse;
}

//
// PRIVATE methods
//

template <class Element,class Key>
void MSIHashKeySet<Element,Key>::
createHashtable(unsigned long noEntries)
{
  if (noEntries==0) noEntries=1;
  ivNoElements=0;
  ivNoEntries=0;

  // in case newBlock() fails
  ivTable =(Node**) 0;
  ivTable =(Node**) ivOps.newBlock(noEntries*sizeof(Node*));
  ivNoEntries=noEntries;

  // in case newBlock() fails
  ivCollList =(unsigned long*) 0;
  ivCollList =(unsigned long*) ivOps.newBlock(ivNoEntries*sizeof(unsigned long));

  for (unsigned long i=0;i<ivNoEntries;i++)
   {
     ivTable[i]=0;
     ivCollList[i]=0;
   }
  return;
}

template <class Element,class Key>
void MSIHashKeySet<Element,Key>::
copyHashtable(MSIHashKeySet<Element,Key> const& hashtable)
{
  if (ivNoEntries==hashtable.ivNoEntries)
   {
     //
     // copy contents of bucket table and collision list
     //
     for (unsigned long i=0;i<ivNoEntries;i++)
      {
        Node* node;
        Node* tmpNode;

        ivCollList[i]=hashtable.ivCollList[i];
        ivTable[i]=0;

        tmpNode=hashtable.ivTable[i];
        while (tmpNode!=0)
         {
           node=ivOps.copyNode(tmpNode);
           node->ivNext=ivTable[i];
           ivTable[i]=node;
           tmpNode=tmpNode->ivNext;
         }
      }
     ivNoElements=hashtable.ivNoElements;
   }
  else
   {
     //
     // add per element,
     // hashvalue has to be computed for every element
     //
     addAllFrom(hashtable);
   }
  return;
}

template <class Element,class Key>
void MSIHashKeySet<Element,Key>::
resize(Node *newNode,unsigned long& newHashvalue)
{
  MSIHashKeySet<Element,Key> old(0,0); // call to private constructor
  old.ivOps=ivOps;
  old.ivNoEntries=ivNoEntries;
  old.ivNoElements=ivNoElements;
  old.ivTable=ivTable;
  old.ivCollList=ivCollList;
  createHashtable(ivNoEntries*2); 

  for (unsigned long i=0;i<old.ivNoEntries;i++)
   {
     while (old.ivTable[i]!=0)
      {
        Node* node=old.ivTable[i];
        old.ivTable[i]=node->ivNext;
        unsigned long hashvalue=ivOps.getHashvalue(ivOps.keyAt(node),ivNoEntries);
        if (node==newNode) newHashvalue=hashvalue;
        if (ivTable[hashvalue]!=0) ivCollList[hashvalue]++;
        node->ivNext=ivTable[hashvalue];
        ivTable[hashvalue]=node;
        ivNoElements++;
      }
   }
}

#endif
