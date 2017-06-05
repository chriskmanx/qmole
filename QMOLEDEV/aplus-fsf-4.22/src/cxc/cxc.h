#ifndef included_cxc_cxc_h
#define included_cxc_cxc_h        

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/* "eh?" Error Codes */
#define STOP_ERROR 0
#define INTERRUPT_ERROR 1
#define WSFULL_ERROR 2
#define STACK_ERROR 3
#define VALUE_ERROR 4
#define VALENCE_ERROR 5
#define TYPE_ERROR 6
#define RANK_ERROR 7
#define LENGTH_ERROR 8
#define DOMAIN_ERROR 9
#define INDEX_ERROR 10
#define MISMATCH_ERROR 11
#define NONCE_ERROR 12
#define MAXRANK_ERROR 13
#define NONFUNCTION_ERROR 14
#define PARSE_ERROR 15
#define MAXITEMS_ERROR 16

typedef struct _PointerTable {
  long		length;		/* number of pointers allocated */
  void		**ptr;
} PointerTable;

typedef struct _MaskTable {
  unsigned long	mask;
  char		*string;
  I		msymbol;
  char		found;
} MaskTable;

typedef struct _EnumTable {
  unsigned long	value;
  char		*string;
  I		msymbol;
} EnumTable;

#define ST_BOOL		0
#define ST_CHAR		1
#define ST_INT		2
#define ST_INTENUM	3
#define ST_LONGMASK	4
#define ST_ULONG	5
#define ST_MULTIINT	6
#define ST_VOID		7

typedef struct _StructureTable {
  unsigned long	mask;
  char		*string;
  unsigned int	offset;
  int		type;
  void		*table;
  I		msymbol;
} StructureTable;

extern PointerTable *AllocPointerTable();
extern void FreePointerTable();
extern char *AToString();
extern void *FetchPointer();
extern int FetchIndex();
extern void RemovePointer();
extern I InternPointer();
extern void InitStructureTable();
extern void InitEnumTable();
extern void InitMaskTable();
extern A EnumToSymbol();
extern A MaskToSymbols();
extern int SymbolToEnum();
extern int SymbolsToMask();
extern int AToStructure();

#endif /* included_cxc_cxc_h  */
