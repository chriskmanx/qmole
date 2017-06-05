/* The following defines shamelessly stolen from GDB sources... */

/* Remap normal yacc parser interface names (yyparse, yylex, yyerror, etc),
   as well as gratuitiously global symbol names, so we can have multiple
   yacc generated parsers in gdb.  Note that these are only the variables
   produced by yacc.  If other parser generators (bison, byacc, etc) produce
   additional global names that conflict at link time, then those parser
   generators need to be fixed instead of adding those names to this list. */

#define	yymaxdepth matcher_parsermaxdepth
#define	yyparse	matcher_parserparse
#define	yylex	matcher_parserlex
#define	yyerror	matcher_parsererror
#define	yylval	matcher_parserlval
#define	yychar	matcher_parserchar
#define	yydebug	matcher_parserdebug
#define	yypact	matcher_parserpact	
#define	yyr1	matcher_parserr1			
#define	yyr2	matcher_parserr2			
#define	yydef	matcher_parserdef		
#define	yychk	matcher_parserchk		
#define	yypgo	matcher_parserpgo		
#define	yyact	matcher_parseract		
#define	yyexca	matcher_parserexca
#define yyerrflag matcher_parsererrflag
#define yynerrs	matcher_parsernerrs
#define	yyps	matcher_parserps
#define	yypv	matcher_parserpv
#define	yys	matcher_parsers
#define	yy_yys	matcher_parseryys
#define	yystate	matcher_parserstate
#define	yytmp	matcher_parsertmp
#define	yyv	matcher_parserv
#define	yy_yyv	matcher_parseryyv
#define	yyval	matcher_parserval
#define	yylloc	matcher_parserlloc
#define yyreds	matcher_parserreds		/* With YYDEBUG defined */
#define yytoks	matcher_parsertoks		/* With YYDEBUG defined */
#define yylhs	matcher_parseryylhs
#define yylen	matcher_parseryylen
#define yydefred matcher_parseryydefred
#define yydgoto	matcher_parseryydgoto
#define yysindex matcher_parseryysindex
#define yyrindex matcher_parseryyrindex
#define yygindex matcher_parseryygindex
#define yytable	 matcher_parseryytable
#define yycheck	 matcher_parseryycheck
#define yyrestart matcher_parserrestart
