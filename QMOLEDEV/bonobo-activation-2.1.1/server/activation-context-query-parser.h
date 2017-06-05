#ifndef BISON_Y_TAB_H
# define BISON_Y_TAB_H

#ifndef YYSTYPE
typedef union
{
  char *val_string;
  char **val_stringv;
  gdouble val_number;
  gboolean val_boolean;
  QueryExpr *qexp;
  GSList *elist;
  int val_enum;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	P_CONST_STRING	257
# define	P_CONST_NUMBER	258
# define	P_CONST_BOOLEAN	259
# define	P_CONST_ID	260
# define	LPAREN	261
# define	RPAREN	262
# define	LBRACKET	263
# define	RBRACKET	264
# define	P_NOT	265
# define	P_NEGATE	266
# define	P_DOLLAR	267
# define	P_MULTIPLY	268
# define	P_DIVIDE	269
# define	P_ADD	270
# define	P_SUBTRACT	271
# define	P_EQ	272
# define	P_NEQ	273
# define	P_LEQ	274
# define	P_GEQ	275
# define	P_LT	276
# define	P_GT	277
# define	P_OR	278
# define	P_AND	279
# define	P_XOR	280
# define	COMMA	281
# define	PERIOD	282
# define	PARSE_ERROR	283


extern YYSTYPE yylval;

#endif /* not BISON_Y_TAB_H */
