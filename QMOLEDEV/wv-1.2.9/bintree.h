#ifndef BinTree
#define BinTree
/* Caolan.McNamara@ul.ie */

/* modify these lines to establish data type */

typedef struct Node_ {
    struct Node_ *Left;		/* left child */
    struct Node_ *Right;	/* right child */
    struct Node_ *Parent;	/* parent */
    void *Data;			/* data stored in node */
} Node;



typedef struct BintreeInfo_ {
    Node *Root;
    int (*CompLT) (void *, void *);
    int (*CompEQ) (void *, void *);
    int no_in_tree;
} BintreeInfo;


void InitBintree (BintreeInfo *, int (*)(void *, void *),
		  int (*)(void *, void *));
Node *InsertNode (BintreeInfo *, void *);
void wvDeleteNode (BintreeInfo *, Node *);
Node *FindNode (BintreeInfo *, void *);
Node *NextNode (BintreeInfo *, Node *);
#endif
