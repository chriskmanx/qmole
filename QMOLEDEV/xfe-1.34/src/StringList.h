#ifndef STRINGLIST_H
#define STRINGLIST_H


// StringList class : implements a doubly linked list of FXString


class StringItem
{
    friend class StringList;
protected:
    FXString str;              // FXString stored in the item
    StringItem *next;          // Pointer to next item
    StringItem *prev;          // Pointer to previous item
};

class StringList
{
public:
    StringItem *first;         // Pointer to begin of list
    StringItem *last;          // Pointer to end of list

    StringList()
    {
        first=NULL;
        last=NULL;
    }

	// Get first item
	StringItem* getFirst(void)
	{
		return this->first;
	}

	// Get last item
	StringItem* getLast(void)
	{
		return this->last;
	}

	// Get previous item
	StringItem* getPrev(StringItem *item)
	{
		return item->prev;
	}

	// Get next item
	StringItem* getNext(StringItem *item)
	{
		return item->next;
	}
	
	// Get string from item
	FXString getString(StringItem *item)
	{
		return item->str;
	}
	
    void insertFirstItem(FXString);
    void insertLastItem(FXString);
    void removeFirstItem();
    void removeLastItem();
    void insertBeforeItem(FXString, StringItem*);
    void insertAfterItem(FXString, StringItem*);
    void removeBeforeItem(StringItem*);
    void removeAfterItem(StringItem*);
    void removeItem(StringItem*);
	int getNumItems(void);
	void removeAllItemsBefore(StringItem*);
	void removeAllItemsAfter(StringItem*);
	void removeAllItems(void); 
	StringItem* getItemAtPos(const int);
	void printFromFirst();
    void printFromLast();
};

#endif
