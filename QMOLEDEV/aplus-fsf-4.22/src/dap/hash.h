#ifndef included_dap_hash_h
#define included_dap_hash_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* header file inclusions */
#include <dap/node.h>

/* external macro declarations */
#define HASH_TBLSZ		(5021)	/* default hash table size */

/* external struct, union, typedef and enum declarations */
struct hash
{
	struct node *tbl;		/* hash table */
	int tblsz;				/* hash table size */
	int (*hashfunc)();
	void *(*value)();
	struct node *(*find)();
	struct node *(*insert)();
	void (*replace)();
	void (*remove)();
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct hash *hashalloc(
				int,int (*)(),void *(*)(),
				struct node *(*)(),
				struct node *(*)(),void (*)(),
				void (*)()
				);
  extern void *hashfind(struct hash *,void *);
  extern void hashfree(struct hash *);
  extern void *hashinsert(struct hash *,void *,void *);
  extern struct node *hashnode(struct hash *,void *);
  extern void *hashremove(struct hash *,void *);
  extern void *hashreplace(struct hash *,void *,void *);
  extern void *hashwalk(struct hash *,void*(*)(),void *);
# ifdef __cplusplus
}
# endif
#else
  extern struct hash *hashalloc();
  extern void *hashfind();
  extern void hashfree();
  extern void *hashinsert();
  extern struct node *hashnode();
  extern void *hashremove();
  extern void *hashreplace();
  extern void *hashwalk();
#endif

#endif

