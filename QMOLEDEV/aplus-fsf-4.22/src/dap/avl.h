#ifndef included_dap_avl_h
#define included_dap_avl_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* external macro declarations */

/* avlwalk() type codes */
#define AVLWALK_PREORDER	(0)
#define AVLWALK_INORDER		(2)
#define AVLWALK_POSTORDER	(4)
#define AVLWALK_LEAF		(6)
#define AVLWALK_ALL			(8)
#define AVLWALK_REVERSE		(1)

/* external struct, union, typedef and enum declarations */

/* avl tree node */
struct avln
{
	struct avln *l;		/* left pointer */
	struct avln *r;		/* right pointer */
	int bal;			/* balancing: Height(left) - Height(right) */
	void *d;			/* user data (including key and value) */
};

/* avl tree search structure */
struct avl
{
	struct avln *root;		/* pointer to root node of avl tree */
	void *(*key)();			/* extract key portion */
	void *(*value)();		/* extract value portion */
	int (*compare)();		/* key comparison function */
	void *(*insert)();		/* insertion notification */
	void *(*replace)();		/* replacement notification */
	void (*remove)();		/* removal notification */
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct avl *avlalloc(void *(*)(),
			      void *(*)(),
			      int (*)(),
			      void *(*)(),
			      void *(*)(),
			      void (*)()
			      );
  extern void *avlfind(struct avl *,void *);
  extern void avlfree(struct avl *);
  extern void *avlinsert(struct avl *,void *,void *);
  extern void *avlremove(struct avl *,void *);
  extern void *avlreplace(struct avl *,void *,void *);
  extern void *avlwalk(struct avl *,int,void *(*)(),void *);
# ifdef __cplusplus
}
# endif
#else
  extern struct avl *avlalloc();
  extern void *avlfind();
  extern void avlfree();
  extern void *avlinsert();
  extern void *avlremove();
  extern void *avlreplace();
  extern void *avlwalk();
#endif
#endif





