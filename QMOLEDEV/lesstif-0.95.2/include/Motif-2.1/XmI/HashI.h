/**
 *
 * $Id: HashI.h,v 1.1 2004/08/28 19:23:29 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#ifndef _XMI_HASHI_H
#define _XMI_HASHI_H

#include <X11/Intrinsic.h>

/*
 * Determines how many buckets are allocated in a single call to malloc()
 * at once. This will help us to improve allocation performance.
 */
#define LTHASH_BUCKET_CHUNK_SIZE 256
/*
 * Controls how many table entries -- that is: number of bucket chains --
 * are in a freshly created hash table.
 * The number specified below must be an odd number with all low bits set,
 * that is: 001111 is valid, but 001101 is not.
 */
#define LTHASH_DEFAULT_TABLE_SIZE 15
/*
 * When there are this many buckets per bucket chain (on average), then
 * the hash table is increased in size.
 */
#define LTHASH_AVERAGE_WATERMARK 4

typedef XtPointer LTHashItemID;
typedef XtPointer LTHashItemValue;

typedef unsigned long (*LTHashGetHashFunction)(LTHashItemID);
typedef Boolean (*LTHashCompareFunction)(LTHashItemID, LTHashItemID);

/*
 * The next two symbols can be used as the "IDSize" parameter in a call
 * to LTHashTableCreate().
 */
#define LTHASH_ID_NOCOPY 0
#define LTHASH_ID_STRING 1

/*
 * The hash table implemented here is of the "closed" kind. That is, it
 * chains up all items in a bucket list which share the same hash key.
 */
typedef struct _LTBucket {
    struct _LTBucket *Next;
    LTHashItemID      ItemID;
    LTHashItemValue   Value;
} LTBucketRec, *LTBucket, **LTBucketList;

typedef struct _LTHashTableRec {
    int                   Mask;
    int                   InUse;
    int                   HighWaterMark;
    LTBucketList          BucketChainList;
    LTHashGetHashFunction GetHash;
    LTHashCompareFunction Compare;
    int                   IDSize;
} LTHashTableRec, *LTHashTable;

typedef enum {
    LTHASH_BREAK,        /* exit the foreach iteratur loop        */
    LTHASH_CONT,         /* continue iteration                    */
    LTHASH_COUNT,        /* continue iteration, increment counter */
    LTHASH_COUNTANDBREAK /* like COUNT and BREAK in a row         */
} LTHashForEachIteratorResult;
typedef LTHashForEachIteratorResult
    (*LTHashForEachFunction)(LTHashTable ht,
                             LTHashItemID, LTHashItemValue, XtPointer);

/*
 * The hash table API.
 */
LTHashTable _LTHashTableCreate(LTHashGetHashFunction GetHash,
                                     LTHashCompareFunction Compare,
				     unsigned int IDSize);
void _LTHashTableDelete(LTHashTable ht);
int _LTHashTableGetNumItems(LTHashTable ht);

Boolean _LTHashTableAddItem(LTHashTable ht,
				  LTHashItemID id, LTHashItemValue value);
Boolean _LTHashTableReplaceItem(LTHashTable ht,
				      LTHashItemID id, LTHashItemValue value,
				      LTHashItemValue *value_ret);
Boolean _LTHashTableReplaceItemAndID(LTHashTable ht,
				           LTHashItemID id, LTHashItemValue value,
				           LTHashItemID *id_ret,
				           LTHashItemValue *value_ret);

Boolean _LTHashTableRemoveItem(LTHashTable ht, LTHashItemID id,
				     LTHashItemValue *value_ret);

Boolean _LTHashTableLookupItem(LTHashTable ht, LTHashItemID id,
				     LTHashItemValue *value);

int _LTHashTableForEachItem(LTHashTable ht, LTHashForEachFunction iter,
                                  XtPointer ClientData);

#endif /* _XMI_HASHI_H */

/* End of HashI.h */
