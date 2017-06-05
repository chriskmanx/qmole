// StringList class : implements a doubly linked list of FXString

#include <stdio.h>

#include <fx.h>

#include "StringList.h"


// Insert an item before the given item
void StringList::insertBeforeItem(FXString str, StringItem *item)
{
    StringItem *newItem;
	
    newItem=new StringItem();
	
    newItem->prev=item->prev;
    newItem->next=item;
    newItem->str=str;
	
    if(item->prev==NULL)
    {
        this->first=newItem;
    }
	if (item->next==NULL)
	{
		this->last=item;
	}
	
    item->prev=newItem;

}

// Insert an item before the first item
void StringList::insertFirstItem(FXString str)
{
    StringItem *newItem;
	
    if(this->first==NULL)
    {
        newItem=new StringItem();
        this->first=newItem;
        this->last=newItem;
        newItem->prev=NULL;
        newItem->next=NULL;
        newItem->str=str;
    }
    else
    {
       insertBeforeItem(str,this->first);
    }
}


// Insert an item after the given item
void StringList::insertAfterItem(FXString str, StringItem *item)
{
    StringItem *newItem;
	
    newItem=new StringItem();
    newItem->next=item->next;
    newItem->prev =item;
    newItem->str=str;

    if(item->next==NULL)
    {
        this->last=newItem;
    }
    item->next=newItem;
}


// Insert an item after the last item
void StringList::insertLastItem(FXString str)
{
    if(this->last==NULL)
        insertFirstItem(str);
    else
        insertAfterItem(str,this->last);
}


// Remove the first item
void StringList::removeFirstItem(void)
{
    removeItem(this->first);
}


// Remove the last item
void StringList::removeLastItem(void)
{
    removeItem(this->last);
}


// Remove the item before the given item
void StringList::removeBeforeItem(StringItem *item)
{

    if(item->prev==this->first)
    {
        this->first=item;
        this->first->prev=NULL;
    }
    else
    {
        removeItem(item->prev);
    }
}


// Remove the item after the given item
void StringList::removeAfterItem(StringItem *item)
{
    if(item->next==this->last)
    {
        this->last=item;
        this->last->next=NULL;
    }
    else
    {
        removeItem(item->next);
    }
}


// Number of items in the list
int StringList::getNumItems(void)
{
    StringItem* item;
	int num=0;
	
    item=this->first;
    
	while(item!=NULL)
    {
        item=item->next;
		num++;
    }
	return num;
}
	
	
// Remove a particular item
void StringList::removeItem(StringItem *item)
{
    if(item==this->first && item==this->last)
    {
		this->first=NULL;
		this->last=NULL;
	}
    else if(item==this->first)
    {
        this->first=this->first->next;
		this->first->prev=NULL;
    }
    else if (item==this->last)
    {
        this->last=this->last->prev;
        this->last->next=NULL;
    }
    else
    {
        item->prev->next=item->next;
        item->next->prev=item->prev;
    }
}


// Remove all items before a given item
void StringList::removeAllItemsBefore(StringItem *item)
{
    StringItem *previtem;
	
	previtem=item->prev;
	if (previtem!=NULL)
	{
		while (previtem!=NULL)
		{
			removeItem(previtem);
			previtem=item->prev;
		}
	}
}


// Remove all items after a given item
void StringList::removeAllItemsAfter(StringItem *item)
{
    StringItem *nextitem;
	
	nextitem=item->next;
	if (nextitem!=NULL)
	{
		while (nextitem!=NULL)
		{
			removeItem(nextitem);
			nextitem=item->next;
		}
	}
}


// Remove all items
void StringList::removeAllItems(void)
{
    StringItem *item, *previtem;

    item=this->last;
    
	while(item!=NULL)
    {
		previtem=item->prev;
		removeItem(item);
		if (previtem==NULL)
		{
			break;
		}
		item=previtem;
   }
}


// Get item based on its position (first position is 0)
StringItem* StringList::getItemAtPos(const int pos)
{
    int num=getNumItems();
	if (num==0 || pos<0 || pos>num-1)
		return NULL;
	
	StringItem* item;
	
    item=this->first;
	if (pos==0)
		return item;
    
	num=0;
	while(item!=NULL)
    {
        item=item->next;
		num++;
		if (pos==num)
			break;
    }
	return item;
}



// Print the list from the first item
void StringList::printFromFirst(void)
{
    StringItem* item;
	
    item=this->first;
    
	fprintf(stdout,"\n=> printFromFirst\n");
	while(item!=NULL)
    {
        fprintf(stdout,"str=%s\n",item->str.text());
        item=item->next;
    }
	fprintf(stdout,"<= printFromFirst\n\n");
}

// Print the list from endwards
void StringList::printFromLast(void)
{
    StringItem* item;
	
    item=this->last;

	fprintf(stdout,"\n=> printFromLast\n");
    while(item!=NULL)
    {
        fprintf(stdout,"str=%s\n",item->str.text());
        item=item->prev;
    }
	fprintf(stdout,"<= printFromLast\n\n");
}

/*

// To test this class
int main(void)
{
    StringList *strlist ;
	StringItem *item;
	FXString str;

    strlist= new StringList();

    // Insert at the end
	strlist->insertLastItem("test2");
    strlist->printFromFirst();
    strlist->insertLastItem("test3");
    strlist->printFromFirst();
    strlist->insertLastItem("test4");
	strlist->printFromFirst();
	
    // Insert at the beginning
    strlist->insertFirstItem("test1");
    strlist->printFromFirst();
    strlist->insertFirstItem("test0");
    strlist->printFromFirst();
	
	// Insert at the end
    strlist->insertLastItem("test5");
    strlist->printFromFirst();
    strlist->insertLastItem("test6");
    strlist->printFromFirst();
	
	// Remove the first item
    strlist->removeFirstItem();
    strlist->printFromFirst();
	
    // Remove the last item
	strlist->removeLastItem();
    strlist->printFromFirst();
	
	// Number of items
	fprintf(stdout,"Number of items = %d\n\n",strlist->getNumItems());
	
	// Get first item
	item=strlist->getFirst();
	if (item)
	{
		str=strlist->getString(item);
		fprintf(stdout,"str=%s\n",str.text());
	}

	// Get last item
	item=strlist->getLast();
	if (item)
	{
		str=strlist->getString(item);
		fprintf(stdout,"str=%s\n",str.text());
	}

	// Get next item
	item=strlist->getNext(item);
	if (item)
	{
		str=strlist->getString(item);
		fprintf(stdout,"str=%s\n",str.text());
	}

	// Get item at some positions
	item=strlist->getItemAtPos(0);
	if (item)
	{
		str=strlist->getString(item);
		fprintf(stdout,"item at position 0 : str=%s\n",str.text());
	}
	item=strlist->getItemAtPos(3);
	if (item)
	{
		str=strlist->getString(item);
		fprintf(stdout,"item at position 3 : str=%s\n",str.text());
	}

	// Remove all items before position 3
	strlist->removeAllItemsBefore(item);
	strlist->printFromFirst();

    // Insert at the end
	strlist->insertLastItem("test6");
    strlist->insertLastItem("test7");
    strlist->insertLastItem("test8");
	strlist->printFromFirst();

	// Remove all items after position 3
	item=strlist->getItemAtPos(3);
	strlist->removeAllItemsAfter(item);
	strlist->printFromFirst();
	
	// Remove all items in the list
	//strlist->printFromFirst();
	//strlist->removeAllItems();
	//strlist->printFromFirst();
	
	// Delete the list
	delete strlist;
}
*/
