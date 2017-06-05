/**
 *
 * $Id: Hash.c,v 1.3 2005/01/04 17:07:05 dannybackx Exp $
 *
 * Copyright (C) 1995-1998 Free Software Foundation, Inc.
 * Copyright (C) 1998-2001, 2004, 2005 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#include <LTconfig.h>
#include <string.h>
#include <XmI/XmI.h>
#include <XmI/HashI.h>
#include <XmI/DebugUtil.h>

#include <XmI/ImageCacheI.h>

/*
 * This is a general-purpose hash table module. It can be used to implement
 * caches and all other kinds of associative arrays -- like the upcoming
 * trait mechanism.
 *
 * Written by Harald Albrecht (albrecht@igpm.rwth-aachen.de)
 */

/*
 * To improve allocation performance, we don't give buckets back to the
 * malloc() memory allocator. Instead we put buckets that aren't needed
 * any longer in our own free list.
 */
static LTBucket BucketFreeStore = NULL;

/*
 * Allocate a bucket. Instead of really allocating only one of these tiny
 * suckers we allocate a whole bunch of them at once. This should improve
 * performance. The buckets that are not needed at this time are linked
 * into the free list.
 */
static LTBucket 
LTCreateBucket(void)
{
    LTBucket Bucket;
    int i;

    if (BucketFreeStore == NULL)
    {

	BucketFreeStore = (LTBucket) XtMalloc(sizeof(LTBucketRec) *
					      LTHASH_BUCKET_CHUNK_SIZE);

	Bucket = BucketFreeStore;
	for (i = 1; i < LTHASH_BUCKET_CHUNK_SIZE; i++)
	{
	    Bucket->Next = Bucket + 1;
	    Bucket++;
	}
	Bucket->Next = NULL;
    }
    Bucket = BucketFreeStore;
    BucketFreeStore = Bucket->Next;

    return Bucket;
}

/*
 * Free a bucket. As I mentioned above, the bucket isn't really freed
 * (from malloc()'s point of view) but instead linked into a free list.
 * Unfortunately ANSI C doesn't know of "inline", so I'm doing it the
 * traditional way using #define. Ouch.
 */
#define LTDestroyBucket(Bucket, IDSize) { \
    if ( IDSize ) {                       \
        XtFree((char *) Bucket->ItemID);  \
    }                                     \
    Bucket->Next    = BucketFreeStore;    \
    BucketFreeStore = Bucket;             \
}


/*
 * This one frees a whole bucket chain. We do this by just linking
 * the whole chain into the free list at once and skip calling LTDeleteBucket
 * for every single bucket.
 */
static void 
LTDestroyBucketChain(LTBucket FirstBucket, unsigned int IDSize)
{
    LTBucket Bucket, NextBucket;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "LTDestroyBucketChain(%p,%d)\n", FirstBucket, IDSize));

    if (FirstBucket == NULL)
    {
	return;
    }

    Bucket = FirstBucket;
    if (IDSize)
    {
	while ((NextBucket = Bucket->Next) != NULL)
	{
	    XtFree((char *)Bucket->ItemID);
	    Bucket = NextBucket;
	}
    }
    else
    {
	while ((NextBucket = Bucket->Next) != NULL)
	{
	    Bucket = NextBucket;
	}
    }
    Bucket->Next = BucketFreeStore;
    BucketFreeStore = FirstBucket;
}


/*
 * Here we provide default hash and compare functions just for those who
 * are lazy enough not to invent their own ones.
 */
static unsigned long
LTDefaultGetHash(LTHashItemID item_id)
{
    return (unsigned long)item_id;
}


static unsigned long
LTDefaultStringGetHash(LTHashItemID item_id)
{
    unsigned long HashValue;
    char *p, ch;

    p = (char *)item_id;
    HashValue = 0L;

    while ((ch = *p++))
    {
	HashValue += (HashValue << 3) + ch;
    }

    return HashValue;
}


static Boolean 
LTDefaultCompare(LTHashItemID item1_id, LTHashItemID item2_id)
{
    return item1_id == item2_id ? True : False;
}


static Boolean 
LTDefaultStringCompare(LTHashItemID item1_id, LTHashItemID item2_id)
{
    return strcmp((const char *)item1_id, (const char *)item2_id) == 0 ?
	True : False;
}


/*
 * Create a new hash table. The programmer can supply her/his own functions
 * for calculating the hash value and for comparing items. There is no need
 * to specify a hash table size as the hash table will adjust itself
 * dynamically.
 */
extern LTHashTable 
_LTHashTableCreate(LTHashGetHashFunction GetHash,
		  LTHashCompareFunction Compare,
		  unsigned int IDSize)
{
    LTHashTable ht;

    ht = (LTHashTable) XtMalloc(sizeof(LTHashTableRec));

    /* If we're called with NULL, NULL -> ImageCache */
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_LTHashTableCreate%s(idsize %d) -> %p\n",
			    (GetHash == NULL) ? " ImageCache " : "",
			    IDSize, ht));

    ht->Mask = LTHASH_DEFAULT_TABLE_SIZE;

    ht->InUse = 0;

    ht->HighWaterMark = (ht->Mask +1)*LTHASH_AVERAGE_WATERMARK;

    ht->BucketChainList = (LTBucketList) XtCalloc(ht->Mask +1,
						  sizeof(LTBucket));
    ht->GetHash = GetHash ? GetHash :
	(IDSize == LTHASH_ID_STRING ?
	 LTDefaultStringGetHash : LTDefaultGetHash);

    ht->Compare = Compare ? Compare :
	(IDSize == LTHASH_ID_STRING ?
	 LTDefaultStringCompare : LTDefaultCompare);

    ht->IDSize = IDSize;

    return ht;
}


/*
 * Deletes a hash table: frees the memory used by the hash table and puts
 * the buckets back into the free list.
 */
extern void 
_LTHashTableDelete(LTHashTable ht)
{
    LTBucketList BucketChain;
    int i;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_LTHashTableDelete(%p)\n", ht));

    if (ht == NULL)
    {
	_XmError(NULL, "LTHashTableDelete: attempt to delete non-existing "
		 "hash table.");
    }

    /*
     * First free all buckets we've allocated for our hash table.
     */
    BucketChain = ht->BucketChainList;
    for (i = 0; i <= ht->Mask; i++)
    {
	LTDestroyBucketChain(*BucketChain++, ht->IDSize);
    }

    /*
     * Finally give back the memory occupied the the hash table description
     * and the bucket chain list to the free store (heap,...).
     */
    XtFree((char *)ht->BucketChainList);
    XtFree((char *)ht);
}


/*
 * Increases the size of a hash table when the average filling level has
 * been reached.
 */
static void 
LTHashTableGrow(LTHashTable ht)
{
    LTBucketList BucketChain, OldBucketChain;
    LTBucket Bucket, NextBucket;
    int i, OldMask, Index;

    if (ht->InUse < ht->HighWaterMark)
    {
	return;
    }

    /*
     * First, save the old bucket chain table and the old table size...
     */
    OldBucketChain = ht->BucketChainList;
    OldMask = ht->Mask;

    /*
     * ...then allocate a new, larger table...
     */
    ht->Mask = (ht->Mask <<2)|0x3;
    ht->HighWaterMark = (ht->Mask +1)*LTHASH_AVERAGE_WATERMARK;
    BucketChain = (LTBucketList) XtCalloc(ht->Mask +1,
					  sizeof(LTBucket));
    ht->BucketChainList = BucketChain;

    /*
     * ...and finally move the buckets from the old hash table to their
     * new places in the new hash table.
     */
    for (i = 0; i <= OldMask; i++)
    {

	Bucket = OldBucketChain[i];

	while (Bucket)
	{
	    NextBucket = Bucket->Next;
	    Index = ht->GetHash(Bucket->ItemID) & ht->Mask;
	    Bucket->Next = BucketChain[Index];
	    BucketChain[Index] = Bucket;
	    Bucket = NextBucket;
	}
    }

    /*
     * That's it! Now clean up the leftovers.
     */
    XtFree((char *)OldBucketChain);
}


/*
 * Returns how many items are currently stored in the hash table. This is
 * just for the curious.
 */
extern int 
_LTHashTableGetNumItems(LTHashTable ht)
{
    if (ht == NULL)
    {
	_XmError(NULL, "LTHashTableGetNumItems: NULL hash table specified.");
    }

    return ht->InUse;
}


/*
 * Adds an item to the hash table. If the item already exists in the hash
 * table then the function will return False immediatly.
 */
extern Boolean 
_LTHashTableAddItem(LTHashTable ht, LTHashItemID id, LTHashItemValue value)
{
    int Index;
    LTBucket Bucket;

    {
    LTPixmapDescRec *pPixmapDesc = (LTPixmapDescRec*)id;
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_LTHashTableAddItem(%p, %p.%p, %p)",	/* no CRLF */
			    ht, pPixmapDesc->screen, pPixmapDesc->pixmap, value));
    }

    if (ht == NULL)
    {
	_XmError(NULL, "LTHashTableAddItem: NULL hash table specified.");
    }

    Index = ht->GetHash(id) & ht->Mask;
    Bucket = ht->BucketChainList[Index];
    while (Bucket &&
	   !ht->Compare(Bucket->ItemID, id))
    {
	Bucket = Bucket->Next;
    }

    if (Bucket == NULL)
    {
	/*
	 * There's no such item in the hash table. Thus allocate a new
	 * bucket and fill it with the item and the value. That's it.
	 */
	Bucket = LTCreateBucket();

	if (ht->IDSize)
	{
	    if (ht->IDSize == LTHASH_ID_STRING)
	    {
		Bucket->ItemID = (LTHashItemID) XtNewString((String)id);
	    }
	    else
	    {
		Bucket->ItemID = (LTHashItemID) XtMalloc(ht->IDSize);
		memcpy(Bucket->ItemID, id, ht->IDSize);
	    }
	}
	else
	{
	    Bucket->ItemID = id;
	}

	Bucket->Value = value;
	Bucket->Next = ht->BucketChainList[Index];
	ht->BucketChainList[Index] = Bucket;
	ht->InUse++;
	LTHashTableGrow(ht);

	DEBUGOUT(_LtDebug0(__FILE__, NULL, " -> InUse %d\n", ht->InUse));
	return True;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, " -> InUse %d\n", ht->InUse));
    return False;
}


/*
 * Adds an item to or replaces an item in a hash table. If the item already
 * exists then old value is returned in "value_ret". The function then
 * returns True. If such an item does not yet exists it is created and
 * the function returns False.
 */
extern Boolean 
_LTHashTableReplaceItem(LTHashTable ht,
		       LTHashItemID id, LTHashItemValue value,
		       LTHashItemValue * value_ret)
{
    int Index;
    LTBucket Bucket;

    {
    LTPixmapDescRec *pPixmapDesc = (LTPixmapDescRec*)id;
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_LTHashTableReplaceItem(%p, %p.%p, %p)",	/* No CRLF */
			    ht, pPixmapDesc->screen, pPixmapDesc->pixmap, value));
    }

    if (ht == NULL)
    {
	_XmError(NULL, "LTHashTableReplaceItem: NULL hash table specified.");
    }

    Index = ht->GetHash(id) & ht->Mask;
    Bucket = ht->BucketChainList[Index];
    while (Bucket &&
	   !ht->Compare(Bucket->ItemID, id))
    {
	Bucket = Bucket->Next;
    }

    if (Bucket == NULL)
    {
	/*
	 * There's no such item in the hash table. Thus allocate a new
	 * bucket and fill it with the item and the value. That's it.
	 */
	Bucket = LTCreateBucket();
	if (ht->IDSize)
	{
	    if (ht->IDSize == LTHASH_ID_STRING)
	    {
		Bucket->ItemID = (LTHashItemID) XtNewString((String)id);
	    }
	    else
	    {
		Bucket->ItemID = (LTHashItemID) XtMalloc(ht->IDSize);
		memcpy(Bucket->ItemID, id, ht->IDSize);
	    }
	}
	else
	{
	    Bucket->ItemID = id;
	}

	Bucket->Value = value;
	Bucket->Next = ht->BucketChainList[Index];
	ht->BucketChainList[Index] = Bucket;
	ht->InUse++;
	LTHashTableGrow(ht);

	DEBUGOUT(_LtDebug0(__FILE__, NULL, " -> InUse %d\n", ht->InUse));
	return False;
    }

    /*
     * There's already such an item in the list, so we're going to over-
     * write the item's value. And we return the old value.
     */
    if (value_ret)
    {
	*value_ret = Bucket->Value;
    }

    Bucket->Value = value;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, " -> InUse %d\n", ht->InUse));
    return True;
}


extern Boolean 
_LTHashTableReplaceItemAndID(LTHashTable ht,
			    LTHashItemID id, LTHashItemValue value,
			    LTHashItemID * id_ret,
			    LTHashItemValue * value_ret)
{
    int Index;
    LTBucket Bucket;

    {
    LTPixmapDescRec *pPixmapDesc = (LTPixmapDescRec*)id;
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_LTHashTableReplaceItemAndID(%p, %p.%p, %p)", /* No CRLF */
			    ht, pPixmapDesc->screen, pPixmapDesc->pixmap, value));
    }

    if (ht == NULL)
    {
	_XmError(NULL, "LTHashTableReplaceItem: NULL hash table specified.");
    }

    Index = ht->GetHash(id) & ht->Mask;
    Bucket = ht->BucketChainList[Index];
    while (Bucket &&
	   !ht->Compare(Bucket->ItemID, id))
    {
	Bucket = Bucket->Next;
    }

    if (Bucket == NULL)
    {
	/*
	 * There's no such item in the hash table. Thus allocate a new
	 * bucket and fill it with the item and the value. That's it.
	 */
	Bucket = LTCreateBucket();
	if (ht->IDSize)
	{
	    if (ht->IDSize == LTHASH_ID_STRING)
	    {
		Bucket->ItemID = (LTHashItemID) XtNewString((String)id);
	    }
	    else
	    {
		Bucket->ItemID = (LTHashItemID) XtMalloc(ht->IDSize);
		memcpy(Bucket->ItemID, id, ht->IDSize);
	    }
	}
	else
	{
	    Bucket->ItemID = id;
	}

	Bucket->Value = value;
	Bucket->Next = ht->BucketChainList[Index];
	ht->BucketChainList[Index] = Bucket;
	ht->InUse++;
	LTHashTableGrow(ht);

	DEBUGOUT(_LtDebug0(__FILE__, NULL, " -> InUse %d\n", ht->InUse));
	return False;
    }

    /*
     * There's already such an item in the list, so we're going to over-
     * write the item's value. And we return the old value. And we over-
     * write the item's id, too.
     */
    if (id_ret)
    {
	*id_ret = Bucket->ItemID;
    }

    if (ht->IDSize)
    {
	if (ht->IDSize == LTHASH_ID_STRING)
	{
	    Bucket->ItemID = (LTHashItemID) XtNewString((String)id);
	}
	else
	{
	    Bucket->ItemID = (LTHashItemID) XtMalloc(ht->IDSize);
	    memcpy(Bucket->ItemID, id, ht->IDSize);
	}
    }
    else
    {
	Bucket->ItemID = id;
    }

    if (value_ret)
    {
	*value_ret = Bucket->Value;
    }
    Bucket->Value = value;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, " -> InUse %d\n", ht->InUse));
    return True;
}


/*
 * Removes an item "item" from the hashtable "ht". If the function was
 * successfully and found the item and delted it then it returns True.
 * If there is no such "id" then the function returns False. If "id"
 * was found and "value_ret" is a non-NULL pointer, then the value of the
 * item is returned through the "value_ret" pointer. This is a shortcut for
 * many cleanup tasks which occur in conjunction with hash tables: you
 * have to delete an item and free the memory associated with it. Using
 * the "value" parameter you don't have first to lookup the item, then
 * free the associated memory and finally delete the item in the hash
 * table. Instead you just delete the item and use *value to free the
 * associated memory.
 */
extern Boolean 
_LTHashTableRemoveItem(LTHashTable ht, LTHashItemID id,
		      LTHashItemValue * value_ret)
{
    int Index;
    LTBucket Bucket, PrevBucket;

    {
    LTPixmapDescRec *pPixmapDesc = (LTPixmapDescRec*)id;
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_LTHashTableRemoveItem(%p, %p.%p)", ht,	/* No CRLF */
			    pPixmapDesc->screen, pPixmapDesc->pixmap));
    }

    if (ht == NULL)
    {
	_XmError(NULL, "LTHashTableRemoveItem: NULL hash table specified.");
    }
    Index = ht->GetHash(id) & ht->Mask;
    Bucket = ht->BucketChainList[Index];

    PrevBucket = NULL;
    while (Bucket)
    {
	if (ht->Compare(Bucket->ItemID, id))
	{
	    if (PrevBucket == NULL)
	    {
		ht->BucketChainList[Index] = Bucket->Next;
	    }
	    else
	    {
		PrevBucket->Next = Bucket->Next;
	    }

	    if (value_ret)
	    {
		*value_ret = Bucket->Value;
	    }
	    LTDestroyBucket(Bucket, ht->IDSize);
	    ht->InUse--;
	    DEBUGOUT(_LtDebug0(__FILE__, NULL, " -> InUse %d\n", ht->InUse));
	    return True;
	}
	PrevBucket = Bucket;
	Bucket = Bucket->Next;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, " -> InUse %d\n", ht->InUse));
    return False;
}


/*
 * Lookups the value of an item within the hash table. The parameters and
 * the return value are the same as in LTHashTableRemoveItem(). But this
 * time the item isn't freed. This function is used to either get the value
 * assigned to an item or just to test the existence of the item.
 */
extern Boolean 
_LTHashTableLookupItem(LTHashTable ht, LTHashItemID id,
		      LTHashItemValue * value)
{
    int Index;
    LTBucket Bucket;

    if (ht == NULL)
    {
	_XmError(NULL, "LTHashTableLookupItem: NULL hash table specified.");
    }

    Index = ht->GetHash(id) & ht->Mask;
    Bucket = ht->BucketChainList[Index];
    while (Bucket &&
	   !ht->Compare(Bucket->ItemID, id))
    {
	Bucket = Bucket->Next;
    }

    if (Bucket)
    {
	if (value)
	{
	    *value = Bucket->Value;
	}
	return True;
    }

    return False;
}


static void DebugHashTable(LTHashTable ht)
{
	int	i;

	if (! _LtDebugInDebug(__FILE__, NULL))
		return;
	_LtDebug0(__FILE__, NULL, "DebugHashTable(%p) Mask %d InUse %d HWM %d IDSize %d\n",
			ht, ht->Mask, ht->InUse, ht->HighWaterMark, ht->IDSize);
	for (i=0; i<ht->Mask; i++) {
		LTBucket Bucket = ht->BucketChainList[i];
		_LtDebug0(__FILE__, NULL, "\t[%d] bucket %p\n", i, Bucket);
		while (Bucket) {
			Bucket = Bucket->Next;
			_LtDebug0(__FILE__, NULL, "\t\tbucket %p\n", Bucket);
		}
	}
}

/*
 * Sad, sad, very sad. Sometimes we need an iterator to iterate over all
 * entries within the hash table.
 */
extern int 
_LTHashTableForEachItem(LTHashTable ht, LTHashForEachFunction iter,
		       XtPointer ClientData)
{
    int Counter, i;
    LTBucket Bucket;
    LTHashForEachIteratorResult ItResult;

    if (ht == NULL)
    {
	_XmError(NULL, "LTHashTableForEachItem: NULL hash table specified.");
    }

    DebugHashTable(ht);

    Counter = 0;
    for (i = 0; i <= ht->Mask; i++)
    {
	Bucket = ht->BucketChainList[i];

	while (Bucket)
	{
	    if (Bucket->ItemID) {
		    ItResult = iter(ht, Bucket->ItemID, Bucket->Value, ClientData);
		    if (ItResult == LTHASH_BREAK) {
			return Counter;
		    } else if (ItResult == LTHASH_COUNTANDBREAK) {
			Counter++;
			return Counter;
		    } else if (ItResult == LTHASH_COUNT) {
			Counter++;
		    }
	    }

	    Bucket = Bucket->Next;
	}
    }
    return Counter;
}
