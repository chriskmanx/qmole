#ifndef included_dap_node_h
#define included_dap_node_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/


/* external macro declarations */
#define NODEAT(np)		((struct node *)((np)->d))

/* external struct, union, typedef and enum declarations */
struct node
{
	struct node *f;
	struct node *b;
	void *d;
};

/* external function declarations */
#if defined(__STDC__) || defined(__cplusplus) || defined(_AIX)
# ifdef __cplusplus
extern "C" {
# endif
  extern struct node *nodealloc(void);
  extern struct node *nodealloc_r(void);
  extern void nodefree(struct node *);
  extern void nodeinsert(struct node *,struct node *);
  extern void noderemove(struct node *);
  extern void nodesort(struct node *,int (*)());
  extern void nodetoad(struct node *);
# ifdef __cplusplus
}
# endif
#else
  extern struct node *nodealloc();
  extern struct node *nodealloc_r();
  extern void nodefree();
  extern void nodeinsert();
  extern void noderemove();
  extern void nodesort();
  extern void nodetoad();
#endif

#endif

