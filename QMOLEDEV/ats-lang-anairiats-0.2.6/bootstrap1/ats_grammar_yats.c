
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 17 "ats_grammar.yats"

//
#include <stdio.h> // for [fprintf]
#include "ats_memory.h" // HX: loading [ats_types.h] as well
/*
// HX: this is okay
#define malloc ats_malloc_gc
#define realloc ats_realloc_gc
#define free ats_free_gc
*/
//
// HX-2011-09-06:
//
#define malloc ats_malloc_ngc
#define realloc ats_realloc_ngc
#define free ats_free_ngc

extern void yyerror (char *s) ;

/* ****** ****** */

typedef ats_ptr_type c0har_t ;
typedef ats_ptr_type e0xtcode_t ;
typedef ats_ptr_type f0loat_t ;
typedef ats_ptr_type f0loatsp_t ;
typedef ats_ptr_type i0nt_t ;
typedef ats_ptr_type i0ntsp_t ;
typedef ats_ptr_type s0tring_t ;
typedef ats_ptr_type t0kn_t ;

typedef ats_ptr_type abskind_t ;
typedef ats_ptr_type dcstkind_t ;
typedef ats_ptr_type datakind_t ;
typedef ats_ptr_type stadefkind_t ;
typedef ats_ptr_type valkind_t ;
typedef ats_ptr_type funkind_t ;
typedef ats_ptr_type lamkind_t ;
typedef lamkind_t fixkind_t ;
typedef ats_ptr_type srpifkindtok_t ;

typedef ats_ptr_type i0de_t ;
typedef ats_ptr_type i0delst_t ;
typedef ats_ptr_type i0delstlst_t ;
typedef ats_ptr_type i0dext_t ;
typedef ats_ptr_type l0ab_t ;

typedef ats_ptr_type p0rec_t ;

typedef ats_ptr_type e0xp_t ;
typedef ats_ptr_type e0xplst_t ;
typedef ats_ptr_type e0xpopt_t ;

typedef ats_ptr_type e0fftag_t ;
typedef ats_ptr_type e0fftaglst_t ;
typedef ats_ptr_type e0fftaglstopt_t ;

typedef ats_ptr_type s0rtq_t ;
typedef ats_ptr_type s0rt_t ;
typedef ats_ptr_type s0rtlst_t ;
typedef ats_ptr_type s0rtopt_t ;
typedef ats_ptr_type s0rtpol_t ;

typedef ats_ptr_type d0atsrtcon_t ;
typedef ats_ptr_type d0atsrtconlst_t ;
typedef ats_ptr_type d0atsrtdec_t ;
typedef ats_ptr_type d0atsrtdeclst_t ;

typedef ats_ptr_type s0taq_t ;
typedef ats_ptr_type d0ynq_t ;
typedef ats_ptr_type sqi0de_t ;
typedef ats_ptr_type dqi0de_t ;
typedef ats_ptr_type arrqi0de_t ;
typedef ats_ptr_type tmpqi0de_t ;
typedef ats_ptr_type s0arg_t ;
typedef ats_ptr_type s0arglst_t ;
typedef ats_ptr_type s0arglstlst_t ;
typedef ats_ptr_type s0exp_t ;
typedef ats_ptr_type s0expext_t ; // for external types
typedef ats_ptr_type s0explst_t ;
typedef ats_ptr_type s0expopt_t ;
typedef ats_ptr_type s0explstlst_t ;
typedef ats_ptr_type s0explstopt_t ;
typedef ats_ptr_type labs0explst_t ;
typedef ats_ptr_type s0arrind_t ;
typedef ats_ptr_type t1mps0explstlst_t ; // with location information
typedef ats_ptr_type s0rtext_t ;
typedef ats_ptr_type s0qua_t ;
typedef ats_ptr_type s0qualst_t ;
typedef ats_ptr_type s0qualstlst_t ;
typedef ats_ptr_type s0qualstopt_t ;
typedef ats_ptr_type impqi0de_t ;

typedef ats_ptr_type sp0at_t ;

typedef ats_ptr_type d0atarg_t ;
typedef ats_ptr_type d0atarglst_t ;
typedef ats_ptr_type s0rtdef_t ;
typedef ats_ptr_type s0rtdeflst_t ;
typedef ats_ptr_type s0tacon_t ;
typedef ats_ptr_type s0taconlst_t ;
typedef ats_ptr_type s0tacst_t ;
typedef ats_ptr_type s0tacstlst_t ;
typedef ats_ptr_type s0tavar_t ;
typedef ats_ptr_type s0tavarlst_t ;
typedef ats_ptr_type s0expdef_t ;
typedef ats_ptr_type s0expdeflst_t ;
typedef ats_ptr_type s0aspdec_t ;
typedef ats_ptr_type d0atcon_t ;
typedef ats_ptr_type d0atconlst_t ;
typedef ats_ptr_type d0atdec_t ;
typedef ats_ptr_type d0atdeclst_t ;
typedef ats_ptr_type e0xndec_t ;
typedef ats_ptr_type e0xndeclst_t ;

typedef ats_ptr_type p0arg_t ;
typedef ats_ptr_type p0arglst_t ;
typedef ats_ptr_type d0arg_t ;
typedef ats_ptr_type d0arglst_t ;
typedef ats_ptr_type m0acarg_t ;
typedef ats_ptr_type m0acarglst_t ;
typedef ats_ptr_type extnamopt_t ;
typedef ats_ptr_type d0cstdec_t ;
typedef ats_ptr_type d0cstdeclst_t ;
typedef ats_ptr_type p0at_t ;
typedef ats_ptr_type p0atlst_t ;
typedef ats_ptr_type labp0atlst_t ;
typedef ats_ptr_type s0vararg_t ;
typedef ats_ptr_type s0exparg_t ;
typedef ats_ptr_type f0arg_t ;
typedef ats_ptr_type f0arglst_t ;
typedef ats_ptr_type s0elop_t ;
typedef ats_ptr_type witht0ype_t ;
typedef ats_ptr_type d0exp_t ;
typedef ats_ptr_type d0explst_t ;
typedef ats_ptr_type d0expopt_t ;
typedef ats_ptr_type labd0explst_t ;
typedef ats_ptr_type d0arrind_t ;
typedef ats_ptr_type ifhead_t ;
typedef ats_ptr_type casehead_t ;
typedef ats_ptr_type loophead_t ;
typedef ats_ptr_type tryhead_t ;
typedef ats_ptr_type m0atch_t ;
typedef ats_ptr_type m0atchlst_t ;
typedef ats_ptr_type guap0at_t ;
typedef ats_ptr_type c0lau_t ;
typedef ats_ptr_type c0laulst_t ;
typedef ats_ptr_type sc0lau_t ;
typedef ats_ptr_type sc0laulst_t ;
typedef ats_ptr_type i0nvarg_t ;
typedef ats_ptr_type i0nvarglst_t ;
typedef ats_ptr_type i0nvresstate_t ;
typedef ats_ptr_type loopi0nv_t ;
typedef ats_ptr_type initestpost_t ;
typedef ats_ptr_type v0aldec_t ;
typedef ats_ptr_type v0aldeclst_t ;
typedef ats_ptr_type f0undec_t ;
typedef ats_ptr_type f0undeclst_t ;
typedef ats_ptr_type v0arwth_t ;
typedef ats_ptr_type v0ardec_t ;
typedef ats_ptr_type v0ardeclst_t ;
typedef ats_ptr_type m0acdef_t ;
typedef ats_ptr_type m0acdeflst_t ;
typedef ats_ptr_type i0mpdec_t ;
typedef ats_ptr_type d0ec_t ;
typedef ats_ptr_type d0eclst_t ;
typedef ats_ptr_type d0ecllst_t ;
typedef ats_ptr_type guad0ec_t ;

/* ****** ****** */

typedef ats_ptr_type yyres_t ;

/* ****** ****** */

yyres_t theYYVALyyres ; /* the result of parsing */

/* ****** ****** */

extern abskind_t abskind_prop (void) ;
extern abskind_t abskind_type (void) ;
extern abskind_t abskind_t0ype (void) ;
extern abskind_t abskind_view (void) ;
extern abskind_t abskind_viewtype (void) ;
extern abskind_t abskind_viewt0ype (void) ;

extern dcstkind_t dcstkind_fun (void) ;
extern dcstkind_t dcstkind_val (void) ;
extern dcstkind_t dcstkind_castfn (void) ;
extern dcstkind_t dcstkind_praxi (void) ;
extern dcstkind_t dcstkind_prfun (void) ;
extern dcstkind_t dcstkind_prval (void) ;

extern datakind_t datakind_prop (void) ;
extern datakind_t datakind_type (void) ;
extern datakind_t datakind_view (void) ;
extern datakind_t datakind_viewtype (void) ;

extern stadefkind_t stadefkind_generic (void) ;
extern stadefkind_t stadefkind_prop (t0kn_t) ;
extern stadefkind_t stadefkind_type (t0kn_t) ;
extern stadefkind_t stadefkind_view (t0kn_t) ;
extern stadefkind_t stadefkind_viewtype (t0kn_t) ;

extern valkind_t valkind_val (void) ;
extern valkind_t valkind_valminus (void) ;
extern valkind_t valkind_valplus (void) ;
extern valkind_t valkind_prval (void) ;

extern funkind_t funkind_fn (void) ;
extern funkind_t funkind_fnstar (void) ;
extern funkind_t funkind_fun (void) ;
extern funkind_t funkind_castfn (void) ;
extern funkind_t funkind_prfn (void) ;
extern funkind_t funkind_prfun (void) ;

extern lamkind_t lamkind_lam (t0kn_t) ;
extern lamkind_t lamkind_atlam (t0kn_t) ;
extern lamkind_t lamkind_llam (t0kn_t) ;
extern lamkind_t lamkind_atllam (t0kn_t) ;
extern fixkind_t fixkind_fix (t0kn_t) ;
extern fixkind_t fixkind_atfix (t0kn_t) ;

extern srpifkindtok_t srpifkindtok_if (t0kn_t) ;
extern srpifkindtok_t srpifkindtok_ifdef (t0kn_t) ;
extern srpifkindtok_t srpifkindtok_ifndef (t0kn_t) ;

/* ****** ****** */

extern i0de_t i0de_make_ampersand (t0kn_t) ;
extern i0de_t i0de_make_backslash (t0kn_t) ;
extern i0de_t i0de_make_bang (t0kn_t) ;
extern i0de_t i0de_make_eq (t0kn_t) ;
extern i0de_t i0de_make_gt (t0kn_t) ;
extern i0de_t i0de_make_gtlt (t0kn_t) ;
extern i0de_t i0de_make_lt (t0kn_t) ;
extern i0de_t i0de_make_minusgt (t0kn_t) ;
extern i0de_t i0de_make_minuslt (t0kn_t) ;
extern i0de_t i0de_make_minusltgt (t0kn_t) ;
extern i0de_t i0de_make_module (t0kn_t) ;
extern i0de_t i0de_make_r0ead (t0kn_t) ;
extern i0de_t i0de_make_tilde (t0kn_t) ;
extern i0de_t i0de_make_t0ype (t0kn_t) ;
extern i0de_t i0de_make_viewt0ype (t0kn_t) ;

extern i0de_t i0de_make_DO (t0kn_t) ;
extern i0de_t i0de_make_IN (t0kn_t) ;
extern i0de_t i0de_make_WHILE (t0kn_t) ;

extern i0delst_t i0delst_nil (void) ;
extern i0delst_t i0delst_sing (i0de_t) ;
extern i0delst_t i0delst_cons (i0de_t, i0delst_t) ;

extern i0delstlst_t i0delstlst_nil (void) ;
extern i0delstlst_t i0delstlst_cons (i0delst_t, i0delstlst_t) ;

extern l0ab_t l0ab_ide (i0de_t) ;
extern l0ab_t l0ab_int (i0nt_t) ;

extern i0de_t stai0de_make (i0de_t) ;

/* ****** ****** */

extern p0rec_t p0rec_emp (void) ;
extern p0rec_t p0rec_ide (i0de_t) ;
extern p0rec_t p0rec_int (i0nt_t) ;
extern p0rec_t p0rec_opr (i0de_t, i0de_t/*opr*/, i0nt_t) ;

/* ****** ****** */

extern e0xp_t e0xp_app (e0xp_t, e0xp_t) ;
extern e0xp_t e0xp_char (c0har_t) ;
extern e0xp_t e0xp_eval (t0kn_t, e0xp_t, t0kn_t) ;
extern e0xp_t e0xp_float (f0loat_t) ;
extern e0xp_t e0xp_ide (i0de_t) ;
extern e0xp_t e0xp_int (i0nt_t) ;
extern e0xp_t e0xp_list (t0kn_t, e0xplst_t, t0kn_t) ;
extern e0xp_t e0xp_string (s0tring_t) ;

extern e0xp_t e0xp_FILENAME (t0kn_t) ; // a special string constant
extern e0xp_t e0xp_LOCATION (t0kn_t) ; // a special string constant

extern e0xplst_t e0xplst_nil (void) ;
extern e0xplst_t e0xplst_cons (e0xp_t, e0xplst_t) ;
extern e0xpopt_t e0xpopt_none (void) ;
extern e0xpopt_t e0xpopt_some (e0xp_t) ;

/* ****** ****** */

extern e0fftag_t e0fftag_cst (int, i0de_t) ;
extern e0fftag_t e0fftag_var (i0de_t) ;
extern e0fftag_t e0fftag_var_fun (t0kn_t) ;
extern e0fftag_t e0fftag_int (i0nt_t) ;
extern e0fftaglst_t e0fftaglst_nil (void) ;
extern e0fftaglst_t e0fftaglst_cons (e0fftag_t, e0fftaglst_t) ;
extern e0fftaglstopt_t e0fftaglstopt_none (void) ;
extern e0fftaglstopt_t e0fftaglstopt_some (e0fftaglst_t) ;

/* ****** ****** */

extern s0rtq_t s0rtq_str (s0tring_t) ;
extern s0rtq_t s0rtq_sym (i0de_t) ;

extern s0rt_t s0rt_prop (t0kn_t) ;
extern s0rt_t s0rt_type (t0kn_t) ;
extern s0rt_t s0rt_t0ype (t0kn_t) ;
extern s0rt_t s0rt_view (t0kn_t) ;
extern s0rt_t s0rt_viewtype (t0kn_t) ;
extern s0rt_t s0rt_viewt0ype (t0kn_t) ;

extern s0rt_t s0rt_app (s0rt_t, s0rt_t) ;
extern s0rt_t s0rt_ide (i0de_t) ;
extern s0rt_t s0rt_qid (s0rtq_t, i0de_t) ;
extern s0rt_t s0rt_list (t0kn_t, s0rtlst_t, t0kn_t) ;
extern s0rt_t s0rt_tup (t0kn_t, s0rtlst_t, t0kn_t) ;

extern s0rtlst_t s0rtlst_nil (void) ;
extern s0rtlst_t s0rtlst_cons (s0rt_t, s0rtlst_t) ;

extern s0rtopt_t s0rtopt_none (void) ;
extern s0rtopt_t s0rtopt_some (s0rt_t) ;

extern s0rtpol_t s0rtpol_make (s0rt_t, int) ;

/* ****** ****** */

/*
** datasort declaration
*/
extern d0atsrtcon_t d0atsrtcon_make_none (i0de_t) ;
extern d0atsrtcon_t d0atsrtcon_make_some (i0de_t, s0rt_t) ;
extern d0atsrtconlst_t d0atsrtconlst_nil (void) ;
extern d0atsrtconlst_t d0atsrtconlst_cons (d0atsrtcon_t, d0atsrtconlst_t) ;
extern d0atsrtdec_t d0atsrtdec_make (i0de_t, d0atsrtconlst_t) ;
extern d0atsrtdeclst_t d0atsrtdeclst_nil (void) ;
extern d0atsrtdeclst_t d0atsrtdeclst_cons (d0atsrtdec_t, d0atsrtdeclst_t) ;

/* ****** ****** */

/*
** static qualifiers
*/
extern s0taq_t s0taq_symdot (i0de_t) ;
extern s0taq_t s0taq_symcolon (i0de_t) ;
extern s0taq_t s0taq_fildot (s0tring_t) ;

/*
** dynamic qualifiers
*/
extern d0ynq_t d0ynq_symcolon(i0de_t) ;
extern d0ynq_t d0ynq_symdot(i0de_t) ;
extern d0ynq_t d0ynq_symdot_symcolon(i0de_t, i0de_t) ;
extern d0ynq_t d0ynq_fildot(s0tring_t) ;
extern d0ynq_t d0ynq_fildot_symcolon(s0tring_t, i0de_t) ;

/* ****** ****** */

/*
** (qualified) static identifiers
*/
extern sqi0de_t sqi0de_make_none (i0de_t) ;
extern sqi0de_t sqi0de_make_some (s0taq_t, i0de_t) ;

/*
** (qualified) dynamic identifiers
*/
extern dqi0de_t dqi0de_make_none (i0de_t) ;
extern dqi0de_t dqi0de_make_some (d0ynq_t, i0de_t) ;

/*
** (qualified) array identifiers
*/
extern arrqi0de_t arrqi0de_make_none (i0de_t) ;
extern arrqi0de_t arrqi0de_make_some (d0ynq_t, i0de_t) ;

/*
** (qualified) template identifiers
*/
extern tmpqi0de_t tmpqi0de_make_none (i0de_t) ;
extern tmpqi0de_t tmpqi0de_make_some (d0ynq_t, i0de_t) ;

/* ****** ****** */

/*
** static arguments
*/
extern s0arg_t s0arg_make (i0de_t, s0rtopt_t) ;
extern s0arg_t s0arg_make_none (i0de_t) ;
extern s0arglst_t s0arglst_nil (void) ;
extern s0arglst_t s0arglst_cons (s0arg_t, s0arglst_t) ;
extern s0arglstlst_t s0arglstlst_nil (void) ;
extern s0arglstlst_t s0arglstlst_cons (s0arglst_t, s0arglstlst_t) ;
extern s0arglstlst_t s0arglstlst_cons_ide (i0de_t, s0arglstlst_t) ;

/* ****** ****** */

extern impqi0de_t impqi0de_make_none (dqi0de_t) ;
extern impqi0de_t impqi0de_make_some
  (tmpqi0de_t, s0explst_t, t1mps0explstlst_t, t0kn_t) ;

/* ****** ****** */

extern sp0at_t sp0at_con (sqi0de_t, s0arglst_t, t0kn_t) ;

/* ****** ****** */

/*
** static expressions
*/
extern s0exp_t s0exp_ann (s0exp_t, s0rt_t) ;
extern s0exp_t s0exp_app (s0exp_t, s0exp_t) ;
extern s0exp_t s0exp_char (c0har_t) ;
extern s0exp_t s0exp_exi (t0kn_t, int/*funres*/, s0qualst_t, t0kn_t) ;
extern s0expext_t s0expext_nam (t0kn_t, s0tring_t/*name*/) ;
extern s0expext_t s0expext_app (s0expext_t/*fun*/, s0exp_t/*arg*/) ;
extern s0exp_t s0exp_extern (s0expext_t) ;
extern s0exp_t s0exp_ide (i0de_t) ;
extern s0exp_t s0exp_imp (t0kn_t, e0fftaglst_t, t0kn_t) ;
extern s0exp_t s0exp_imp_emp (t0kn_t) ;
extern s0exp_t s0exp_int (i0nt_t) ;
extern s0exp_t s0exp_intsp_err (i0nt_t) ; /* error handling */
extern s0exp_t s0exp_lams (t0kn_t, s0arglstlst_t, s0rtopt_t, s0exp_t) ;
extern s0exp_t s0exp_list (t0kn_t, s0explst_t, t0kn_t) ;
extern s0exp_t s0exp_list2 (t0kn_t, s0explst_t, s0explst_t, t0kn_t) ;
/*
// HX-2010-12-04: inadequate design
extern s0exp_t s0exp_named (i0de_t, s0exp_t) ;
*/
extern s0exp_t s0exp_opide (t0kn_t, i0de_t) ;
extern s0exp_t s0exp_qid (s0taq_t, i0de_t) ;
extern s0exp_t s0exp_struct (t0kn_t, labs0explst_t, t0kn_t) ;
extern s0exp_t s0exp_tyarr (t0kn_t, s0exp_t, s0arrind_t) ;
extern s0exp_t s0exp_tyrec (int, t0kn_t, labs0explst_t, t0kn_t) ;
extern s0exp_t s0exp_tyrec_ext (t0kn_t, s0tring_t, labs0explst_t, t0kn_t) ;
extern s0exp_t s0exp_tytup (int, t0kn_t, s0explst_t, t0kn_t) ;
extern s0exp_t s0exp_tytup2 (int, t0kn_t, s0explst_t, s0explst_t, t0kn_t) ;
extern s0exp_t s0exp_uni (t0kn_t, s0qualst_t, t0kn_t) ;
extern s0exp_t s0exp_union (t0kn_t, s0exp_t, labs0explst_t, t0kn_t) ;

extern s0explst_t s0explst_nil (void) ;
extern s0explst_t s0explst_cons (s0exp_t, s0explst_t) ;

extern s0expopt_t s0expopt_none (void) ;
extern s0expopt_t s0expopt_some (s0exp_t) ;

extern s0explstlst_t s0explstlst_nil (void) ;
extern s0explstlst_t s0explstlst_cons (s0explst_t, s0explstlst_t) ;

extern s0explstopt_t s0explstopt_none (void) ;
extern s0explstopt_t s0explstopt_some (s0explst_t) ;

extern labs0explst_t labs0explst_nil (void) ;
extern labs0explst_t labs0explst_cons (l0ab_t, s0exp_t, labs0explst_t) ;

extern s0arrind_t s0arrind_make_sing (s0explst_t, t0kn_t) ;
extern s0arrind_t s0arrind_make_cons (s0explst_t, s0arrind_t) ;

extern t1mps0explstlst_t gtlt_t1mps0expseqseq_nil (void) ;
extern t1mps0explstlst_t
gtlt_t1mps0expseqseq_cons_tok (t0kn_t, s0explst_t, t1mps0explstlst_t) ;
// end of [extern]

/* ****** ****** */

extern s0rtext_t s0rtext_srt (s0rt_t) ;
extern s0rtext_t s0rtext_sub
  (t0kn_t, i0de_t, s0rtext_t, s0exp_t, s0explst_t, t0kn_t) ;

extern s0qua_t s0qua_prop(s0exp_t) ;
extern s0qua_t s0qua_vars(i0de_t, i0delst_t, s0rtext_t) ;
extern s0qualst_t s0qualst_nil (void) ;
extern s0qualst_t s0qualst_cons (s0qua_t, s0qualst_t) ;
extern s0qualstlst_t s0qualstlst_nil (void) ;
extern s0qualstlst_t s0qualstlst_cons (s0qualst_t, s0qualstlst_t) ;
extern s0qualstopt_t s0qualstopt_none (void) ;
extern s0qualstopt_t s0qualstopt_some (s0qualst_t) ;

/* ****** ****** */

extern d0atarg_t d0atarg_srt (s0rtpol_t) ;
extern d0atarg_t d0atarg_id_srt (i0de_t, s0rtpol_t) ;
extern d0atarglst_t d0atarglst_nil (void) ;
extern d0atarglst_t d0atarglst_cons (d0atarg_t, d0atarglst_t) ;

/* ****** ****** */

extern s0rtdef_t s0rtdef_make (i0de_t, s0rtext_t) ;
extern s0rtdeflst_t s0rtdeflst_nil (void) ;
extern s0rtdeflst_t s0rtdeflst_cons (s0rtdef_t, s0rtdeflst_t) ;

/* ****** ****** */

extern s0tacon_t s0tacon_make_none_none (i0de_t) ;
extern s0tacon_t s0tacon_make_some_none (i0de_t, d0atarglst_t, t0kn_t) ;
extern s0tacon_t s0tacon_make_none_some (i0de_t, s0exp_t) ;
extern s0tacon_t s0tacon_make_some_some (i0de_t, d0atarglst_t, s0exp_t) ;
extern s0taconlst_t s0taconlst_nil (void) ;
extern s0taconlst_t s0taconlst_cons (s0tacon_t, s0taconlst_t) ;

extern s0tacst_t s0tacst_make_none (i0de_t, s0rt_t) ;
extern s0tacst_t s0tacst_make_some (i0de_t, d0atarglst_t, s0rt_t) ;
extern s0tacstlst_t s0tacstlst_nil (void) ;
extern s0tacstlst_t s0tacstlst_cons (s0tacst_t, s0tacstlst_t) ;

extern s0tavar_t s0tavar_make (i0de_t, s0rt_t) ;
extern s0tavarlst_t s0tavarlst_nil (void) ;
extern s0tavarlst_t s0tavarlst_cons (s0tavar_t, s0tavarlst_t) ;

/* ****** ****** */

extern s0expdef_t s0expdef_make (i0de_t, s0arglstlst_t, s0rtopt_t, s0exp_t) ;
extern s0expdeflst_t s0expdeflst_nil (void) ;
extern s0expdeflst_t s0expdeflst_cons (s0expdef_t, s0expdeflst_t) ;
//
extern s0aspdec_t s0aspdec_make (i0de_t, s0arglstlst_t, s0rtopt_t, s0exp_t) ;
//
extern d0atcon_t
d0atcon_make (s0qualstlst_t, i0de_t, s0explstopt_t, s0expopt_t) ;
extern d0atconlst_t d0atconlst_nil (void) ;
extern d0atconlst_t d0atconlst_cons (d0atcon_t, d0atconlst_t) ;
//
extern d0atdec_t d0atdec_make_none (i0de_t, d0atconlst_t) ;
extern d0atdec_t
d0atdec_make_some (i0de_t, d0atarglst_t, t0kn_t, d0atconlst_t) ;
extern d0atdeclst_t d0atdeclst_nil (void) ;
extern d0atdeclst_t d0atdeclst_cons (d0atdec_t, d0atdeclst_t) ;
//
extern e0xndec_t e0xndec_make (s0qualstlst_t, i0de_t, s0expopt_t) ;
extern e0xndeclst_t e0xndeclst_nil (void) ;
extern e0xndeclst_t e0xndeclst_cons (e0xndec_t, e0xndeclst_t) ;

/* ****** ****** */

extern p0arg_t p0arg_make_none (i0de_t) ;
extern p0arg_t p0arg_make_some (i0de_t, s0exp_t) ;
extern p0arglst_t p0arglst_nil (void) ;
extern p0arglst_t p0arglst_cons (p0arg_t, p0arglst_t) ;
//
extern d0arg_t d0arg_var (i0de_t) ;
extern d0arg_t d0arg_dyn (t0kn_t, p0arglst_t, t0kn_t) ;
extern d0arg_t d0arg_dyn2 (t0kn_t, p0arglst_t, p0arglst_t, t0kn_t) ;
extern d0arg_t d0arg_sta (t0kn_t, s0qualst_t, t0kn_t) ;
extern d0arglst_t d0arglst_nil (void) ;
extern d0arglst_t d0arglst_cons (d0arg_t, d0arglst_t) ;
//
extern m0acarg_t m0acarg_one (i0de_t) ;
extern m0acarg_t m0acarg_lst (t0kn_t, i0delst_t, t0kn_t) ;
extern m0acarglst_t m0acarglst_nil () ;
extern m0acarglst_t m0acarglst_cons (m0acarg_t, m0acarglst_t) ;

/* ****** ****** */

extern extnamopt_t extnamopt_none (void) ;
extern extnamopt_t extnamopt_some (s0tring_t) ;

extern d0cstdec_t
d0cstdec_make (i0de_t, d0arglst_t, e0fftaglstopt_t, s0exp_t, extnamopt_t) ;
extern d0cstdeclst_t d0cstdeclst_nil (void) ;
extern d0cstdeclst_t d0cstdeclst_cons (d0cstdec_t, d0cstdeclst_t) ;

/* ****** ****** */

extern p0at_t p0at_ann (p0at_t, s0exp_t) ;
extern p0at_t p0at_apps (p0at_t, p0atlst_t) ;
extern p0at_t p0at_as (i0de_t, p0at_t) ;
extern p0at_t p0at_char (c0har_t) ;
extern p0at_t p0at_exist (t0kn_t, s0arglst_t, t0kn_t) ;
extern p0at_t p0at_float (f0loat_t); 
extern p0at_t p0at_free (t0kn_t, p0at_t); 
extern p0at_t p0at_ide (i0de_t) ;
extern p0at_t p0at_int (i0nt_t) ;
extern p0at_t p0at_list (t0kn_t, p0atlst_t, t0kn_t) ;
extern p0at_t p0at_list2 (t0kn_t, p0atlst_t, p0atlst_t, t0kn_t) ;
extern p0at_t p0at_lst (t0kn_t, p0atlst_t, t0kn_t) ;
extern p0at_t p0at_qid (d0ynq_t, i0de_t) ;
extern p0at_t p0at_opide (t0kn_t, i0de_t) ;
extern p0at_t p0at_rec (int, t0kn_t, labp0atlst_t, t0kn_t) ;
extern p0at_t p0at_ref (t0kn_t, i0de_t); 
extern p0at_t p0at_refas (t0kn_t, i0de_t, p0at_t); 
extern p0at_t p0at_svararg (t0kn_t, s0vararg_t, t0kn_t) ;
extern p0at_t p0at_string (s0tring_t) ;
extern p0at_t p0at_tup (int, t0kn_t, p0atlst_t, t0kn_t) ;
extern p0at_t p0at_tup2 (int, t0kn_t, p0atlst_t, p0atlst_t, t0kn_t) ;
//
extern p0atlst_t p0atlst_nil (void) ;
extern p0atlst_t p0atlst_cons (p0at_t, p0atlst_t) ;
//
extern labp0atlst_t labp0atlst_nil (void) ;
extern labp0atlst_t labp0atlst_dot (void) ;
extern labp0atlst_t labp0atlst_cons (l0ab_t, p0at_t, labp0atlst_t) ;
//
extern s0vararg_t s0vararg_one (void) ;
extern s0vararg_t s0vararg_all (void) ;
extern s0vararg_t s0vararg_seq (s0arglst_t) ;

/* ****** ****** */

extern s0exparg_t s0exparg_one (void) ;
extern s0exparg_t s0exparg_all (void) ;
extern s0exparg_t s0exparg_seq (s0explst_t) ;

extern f0arg_t f0arg_sta1 (t0kn_t, s0qualst_t, t0kn_t) ;
extern f0arg_t f0arg_sta2 (t0kn_t, s0arglst_t, t0kn_t) ;
extern f0arg_t f0arg_dyn (p0at_t) ;
extern f0arg_t f0arg_met_none (t0kn_t) ;
extern f0arg_t f0arg_met_some (t0kn_t, s0explst_t, t0kn_t) ;
extern f0arglst_t f0arglst_nil (void) ;
extern f0arglst_t f0arglst_cons (f0arg_t, f0arglst_t) ;

extern s0elop_t s0elop_make (int, t0kn_t) ;

extern witht0ype_t witht0ype_none (void) ;
extern witht0ype_t witht0ype_prop (s0exp_t) ;
extern witht0ype_t witht0ype_type (s0exp_t) ;
extern witht0ype_t witht0ype_view (s0exp_t) ;
extern witht0ype_t witht0ype_viewtype (s0exp_t) ;

/* ****** ****** */

/*
** dynamic expressions
*/
extern d0exp_t d0exp_ann (d0exp_t, s0exp_t) ;
//
extern d0exp_t d0exp_apps (d0exp_t, d0explst_t) ;
//
extern d0exp_t d0exp_arrinit_none
  (t0kn_t, s0exp_t, d0explst_t /*elt*/, t0kn_t) ;
extern d0exp_t d0exp_arrinit_some
  (t0kn_t, s0exp_t, d0exp_t /*asz*/, d0explst_t /*elt*/, t0kn_t) ;
//
extern d0exp_t d0exp_arrsize
  (t0kn_t, s0exp_t, t0kn_t/*lparen*/, d0explst_t, t0kn_t/*rparen*/) ;
//
extern d0exp_t d0exp_arrsub (arrqi0de_t, d0arrind_t) ;
//
extern d0exp_t d0exp_char (t0kn_t) ;
//
extern d0exp_t d0exp_caseof (casehead_t, d0exp_t, t0kn_t, c0laulst_t) ;
//
extern d0exp_t d0exp_crypt (int, t0kn_t) ;
//
extern d0exp_t d0exp_decseq (t0kn_t, d0eclst_t, t0kn_t) ;
//
extern d0exp_t d0exp_delay (int/*lin*/, t0kn_t) ;
//
extern d0exp_t d0exp_dynload (t0kn_t) ;
//
// HX: [d0exp_effmask_*] are implemented in [ats_effect.dats]
//
extern d0exp_t d0exp_effmask_all (t0kn_t) ;
extern d0exp_t d0exp_effmask_exn (t0kn_t) ;
extern d0exp_t d0exp_effmask_ntm (t0kn_t) ;
extern d0exp_t d0exp_effmask_ref (t0kn_t) ;
//
extern d0exp_t d0exp_exist (t0kn_t, s0exparg_t, t0kn_t, d0exp_t, t0kn_t) ;
//
extern d0exp_t d0exp_extval (t0kn_t, s0exp_t, s0tring_t, t0kn_t) ;
//
extern d0exp_t d0exp_fix
  (fixkind_t, i0de_t, f0arglst_t, s0expopt_t, e0fftaglstopt_t, d0exp_t) ;
//
extern d0exp_t d0exp_float (f0loat_t) ;
extern d0exp_t d0exp_floatsp (f0loatsp_t) ;
//
extern d0exp_t d0exp_foldat (t0kn_t, d0explst_t) ;
//
extern d0exp_t d0exp_for_itp (loophead_t, initestpost_t, d0exp_t) ;
//
extern d0exp_t d0exp_freeat (t0kn_t, d0explst_t) ;
//
extern d0exp_t d0exp_ide (i0de_t) ;
extern d0exp_t d0exp_idext (i0de_t) ;
//
extern d0exp_t d0exp_if_none (ifhead_t, d0exp_t, d0exp_t) ;
extern d0exp_t d0exp_if_some (ifhead_t, d0exp_t, d0exp_t, d0exp_t) ;
//
extern d0exp_t d0exp_int (i0nt_t) ;
extern d0exp_t d0exp_intsp (i0ntsp_t) ;
//
extern
d0exp_t d0exp_lam
  (lamkind_t, f0arglst_t, s0expopt_t, e0fftaglstopt_t, d0exp_t) ;
//
extern
d0exp_t d0exp_let_seq (t0kn_t, d0eclst_t, t0kn_t, d0explst_t, t0kn_t) ;
//
extern
d0exp_t d0exp_list (t0kn_t, d0explst_t, t0kn_t) ;
extern
d0exp_t d0exp_list2 (t0kn_t, d0explst_t, d0explst_t, t0kn_t) ;
//
extern
d0exp_t d0exp_lst (
  int, t0kn_t, s0expopt_t, t0kn_t/*lparen*/, d0explst_t, t0kn_t/*rparen*/
) ; // end of [d0exp_lst]
extern d0exp_t d0exp_lst_quote (t0kn_t, d0explst_t, t0kn_t) ;
//
extern d0exp_t d0exp_loopexn (int, t0kn_t) ;
//
extern d0exp_t d0exp_macsyn_cross (t0kn_t, d0explst_t, t0kn_t) ;
extern d0exp_t d0exp_macsyn_decode (t0kn_t, d0explst_t, t0kn_t) ;
extern d0exp_t d0exp_macsyn_encode_seq (t0kn_t, d0explst_t, t0kn_t) ;
//
extern d0exp_t d0exp_opide (t0kn_t, i0de_t) ;
extern d0exp_t d0exp_ptrof (t0kn_t) ;
extern d0exp_t d0exp_qid (d0ynq_t, i0de_t) ;
extern d0exp_t d0exp_raise (t0kn_t, d0exp_t) ;
extern d0exp_t d0exp_rec (int, t0kn_t, labd0explst_t, t0kn_t) ;
extern d0exp_t d0exp_scaseof (casehead_t, s0exp_t, t0kn_t, sc0laulst_t) ;
extern d0exp_t d0exp_sel_lab (t0kn_t, l0ab_t) ;
extern d0exp_t d0exp_sel_ind (t0kn_t, d0arrind_t) ;
extern d0exp_t d0exp_seq (t0kn_t, d0explst_t, t0kn_t) ;
extern d0exp_t d0exp_sexparg (t0kn_t, s0exparg_t, t0kn_t) ;
extern d0exp_t d0exp_sif (ifhead_t, s0exp_t, d0exp_t, d0exp_t) ;
extern d0exp_t d0exp_string (s0tring_t) ;
extern d0exp_t d0exp_tmpid (tmpqi0de_t, s0explst_t, t1mps0explstlst_t, t0kn_t) ;
extern d0exp_t d0exp_trywith_seq (tryhead_t, d0explst_t, t0kn_t, c0laulst_t) ;
extern d0exp_t d0exp_tup (int, t0kn_t, d0explst_t, t0kn_t) ;
extern d0exp_t d0exp_tup2 (int, t0kn_t, d0explst_t, d0explst_t, t0kn_t) ;
extern d0exp_t d0exp_viewat (t0kn_t) ;
extern d0exp_t d0exp_where (d0exp_t, d0eclst_t, t0kn_t) ;
extern d0exp_t d0exp_while (loophead_t, d0exp_t, d0exp_t) ;
//
extern d0exp_t d0exp_FILENAME (t0kn_t) ; // a special string constant
extern d0exp_t d0exp_LOCATION (t0kn_t) ; // a special string constant
//
extern d0explst_t d0explst_nil (void) ;
extern d0explst_t d0explst_cons (d0exp_t, d0explst_t) ;
extern d0explst_t d0explst_sing (d0exp_t) ;
//
extern d0expopt_t d0expopt_none (void) ;
extern d0expopt_t d0expopt_some (d0exp_t) ;
//
extern labd0explst_t labd0explst_nil (void) ;
extern labd0explst_t labd0explst_cons (l0ab_t, d0exp_t, labd0explst_t) ;
//
extern d0arrind_t d0arrind_make_sing (d0explst_t, t0kn_t) ;
extern d0arrind_t d0arrind_make_cons (d0explst_t, d0arrind_t) ;

/* ****** ****** */

extern ifhead_t ifhead_make (t0kn_t, i0nvresstate_t) ;
extern casehead_t casehead_make (int, t0kn_t, i0nvresstate_t) ;
extern loophead_t loophead_make_none (t0kn_t) ;
extern loophead_t loophead_make_some (t0kn_t, loopi0nv_t, t0kn_t) ;
extern tryhead_t tryhead_make (t0kn_t) ;

/* ****** ****** */

/*
** pattern matching
*/
extern m0atch_t m0atch_make_none (d0exp_t) ;
extern m0atch_t m0atch_make_some (d0exp_t, p0at_t) ;
extern m0atchlst_t m0atchlst_nil (void) ;
extern m0atchlst_t m0atchlst_cons (m0atch_t, m0atchlst_t) ;

extern guap0at_t guap0at_make_none (p0at_t) ;
extern guap0at_t guap0at_make_some (p0at_t, d0exp_t) ;

extern c0lau_t c0lau_make (guap0at_t, int, int, d0exp_t) ;
extern c0laulst_t c0laulst_nil (void) ;
extern c0laulst_t c0laulst_cons (c0lau_t, c0laulst_t) ;

extern sc0lau_t sc0lau_make (sp0at_t, d0exp_t) ;
extern sc0laulst_t sc0laulst_nil (void) ;
extern sc0laulst_t sc0laulst_cons (sc0lau_t, sc0laulst_t) ;

/* ****** ****** */

extern i0nvarg_t i0nvarg_make_none (i0de_t) ;
extern i0nvarg_t i0nvarg_make_some (i0de_t, s0exp_t) ;

extern i0nvarglst_t i0nvarglst_nil (void) ;
extern i0nvarglst_t i0nvarglst_cons (i0nvarg_t, i0nvarglst_t) ;

extern i0nvresstate_t i0nvresstate_none (void) ;
extern i0nvresstate_t i0nvresstate_some (s0qualstopt_t, i0nvarglst_t) ;

extern loopi0nv_t loopi0nv_make
  (s0qualstopt_t, s0explstopt_t, i0nvarglst_t, i0nvresstate_t) ;

extern initestpost_t initestpost_make
  (t0kn_t, d0explst_t, t0kn_t, d0explst_t, t0kn_t, d0explst_t, t0kn_t) ;
// end of [extern]

/* ****** ****** */

extern v0aldec_t v0aldec_make (p0at_t, d0exp_t, witht0ype_t) ;
extern v0aldeclst_t v0aldeclst_nil (void) ;
extern v0aldeclst_t v0aldeclst_cons (v0aldec_t, v0aldeclst_t) ;

extern f0undec_t f0undec_make_none
  (i0de_t, f0arglst_t, d0exp_t, witht0ype_t) ;
extern f0undec_t f0undec_make_some
  (i0de_t, f0arglst_t, e0fftaglstopt_t, s0exp_t, d0exp_t, witht0ype_t) ;
extern f0undeclst_t f0undeclst_nil (void) ;
extern f0undeclst_t f0undeclst_cons (f0undec_t, f0undeclst_t) ;

extern v0arwth_t v0arwth_none () ;
extern v0arwth_t v0arwth_some (i0de_t) ;

extern v0ardec_t v0ardec_make_some_none
  (int /*stadyn*/, i0de_t, v0arwth_t, s0exp_t) ;
extern v0ardec_t v0ardec_make_none_some
  (int /*stadyn*/, i0de_t, v0arwth_t, d0exp_t) ;
extern v0ardec_t v0ardec_make_some_some
  (int /*stadyn*/, i0de_t, s0exp_t, v0arwth_t, d0exp_t) ;
extern v0ardeclst_t v0ardeclst_nil (void) ;
extern v0ardeclst_t v0ardeclst_cons (v0ardec_t, v0ardeclst_t) ;

/* ****** ****** */

extern m0acdef_t m0acdef_make (i0de_t, m0acarglst_t, d0exp_t) ;
extern m0acdeflst_t m0acdeflst_nil (void) ;
extern m0acdeflst_t m0acdeflst_cons (m0acdef_t, m0acdeflst_t) ;

/* ****** ****** */

extern
i0mpdec_t i0mpdec_make (impqi0de_t, f0arglst_t, s0expopt_t, d0exp_t) ;
// end of [i0mpdec_make]
 
/* ****** ****** */

/*
** static and dynamic declarations
*/
extern d0ec_t d0ec_infix (t0kn_t, p0rec_t, int, i0delst_t) ;
extern d0ec_t d0ec_prefix (t0kn_t, p0rec_t, i0delst_t) ;
extern d0ec_t d0ec_postfix (t0kn_t, p0rec_t, i0delst_t) ;
extern d0ec_t d0ec_nonfix (t0kn_t, i0delst_t) ;
extern d0ec_t d0ec_symintr (t0kn_t, i0delst_t) ;
extern d0ec_t d0ec_include (int/*0:sta/1:dyn*/, s0tring_t) ;
extern d0ec_t d0ec_e0xpundef (i0de_t) ;
extern d0ec_t d0ec_e0xpdef (i0de_t, e0xpopt_t) ;
extern d0ec_t d0ec_e0xpact_assert (e0xp_t) ;
extern d0ec_t d0ec_e0xpact_error (e0xp_t) ;
extern d0ec_t d0ec_e0xpact_print (e0xp_t) ;
extern d0ec_t d0ec_srtdefs (s0rtdef_t, s0rtdeflst_t) ;
extern d0ec_t d0ec_datsrts (int/*para*/, d0atsrtdec_t, d0atsrtdeclst_t) ;
extern d0ec_t d0ec_stacons (abskind_t, s0tacon_t, s0taconlst_t) ;
extern d0ec_t d0ec_stacsts (s0tacst_t, s0tacstlst_t) ;
extern d0ec_t d0ec_stavars (s0tavar_t, s0tavarlst_t) ;
extern d0ec_t d0ec_sexpdefs (stadefkind_t, s0expdef_t, s0expdeflst_t) ;
extern d0ec_t d0ec_propdefs (t0kn_t, s0expdef_t, s0expdeflst_t) ;
extern d0ec_t d0ec_typedefs (t0kn_t, s0expdef_t, s0expdeflst_t) ;
extern d0ec_t d0ec_viewdefs (t0kn_t, s0expdef_t, s0expdeflst_t) ;
extern d0ec_t d0ec_viewtypedefs (t0kn_t, s0expdef_t, s0expdeflst_t) ;
extern d0ec_t d0ec_saspdec (s0aspdec_t) ;
extern d0ec_t d0ec_dcstdecs
  (dcstkind_t, s0qualstlst_t, d0cstdec_t, d0cstdeclst_t) ;
extern d0ec_t d0ec_datdecs
  (datakind_t, d0atdec_t, d0atdeclst_t, s0explstopt_t) ;
extern d0ec_t d0ec_exndecs (t0kn_t, e0xndec_t, e0xndeclst_t) ;
//
extern d0ec_t d0ec_classdec_none (t0kn_t, i0de_t) ;
extern d0ec_t d0ec_classdec_some (t0kn_t, i0de_t, s0exp_t) ;
//
extern d0ec_t d0ec_overload (t0kn_t, i0de_t, dqi0de_t) ;
extern d0ec_t d0ec_overload_lrbrackets (t0kn_t, t0kn_t, t0kn_t, dqi0de_t) ;
//
extern d0ec_t d0ec_dynload (s0tring_t) ;
extern d0ec_t d0ec_staload_none (s0tring_t) ;
extern d0ec_t d0ec_staload_some (i0de_t, s0tring_t) ;

extern d0ec_t d0ec_extype (s0tring_t, s0exp_t) ;
extern d0ec_t d0ec_extval (s0tring_t, d0exp_t) ;
extern d0ec_t d0ec_extcode_dyn (e0xtcode_t) ;
extern d0ec_t d0ec_extcode_sta (e0xtcode_t) ;
extern d0ec_t d0ec_valdecs (valkind_t, v0aldec_t, v0aldeclst_t) ;
extern d0ec_t d0ec_valdecs_par (v0aldec_t, v0aldeclst_t) ;
extern d0ec_t d0ec_valdecs_rec (v0aldec_t, v0aldeclst_t) ;
extern d0ec_t d0ec_fundecs (funkind_t, s0qualstlst_t, f0undec_t, f0undeclst_t) ;
extern d0ec_t d0ec_vardecs (v0ardec_t, v0ardeclst_t) ;
extern d0ec_t d0ec_macdefs (int, m0acdef_t, m0acdeflst_t) ;
extern d0ec_t d0ec_impdec (t0kn_t, s0arglstlst_t, i0mpdec_t) ;

extern d0ec_t d0ec_local (t0kn_t, d0eclst_t, d0eclst_t, t0kn_t) ;
extern d0ec_t d0ec_guadec (srpifkindtok_t, guad0ec_t) ;

extern guad0ec_t guad0ec_one (e0xp_t, d0eclst_t, t0kn_t) ;
extern guad0ec_t guad0ec_two (e0xp_t, d0eclst_t, d0eclst_t, t0kn_t) ;
extern guad0ec_t guad0ec_cons (e0xp_t, d0eclst_t, srpifkindtok_t, guad0ec_t) ;

extern d0eclst_t d0eclst_nil (void) ;
extern d0eclst_t d0eclst_cons (d0ec_t, d0eclst_t) ;
extern d0ecllst_t d0ecllst_nil (void) ;
extern d0ecllst_t d0ecllst_cons (d0ecllst_t, d0ec_t) ;
extern d0eclst_t d0ecllst_reverse (d0ecllst_t) ;

/* ****** ****** */

/*
** HX: implemented in [ats_parser.dats]
*/
extern yyres_t atsopt_yyres_i0de (i0de_t) ;
extern yyres_t atsopt_yyres_s0exp (s0exp_t) ;
extern yyres_t atsopt_yyres_d0exp (d0exp_t) ;
extern yyres_t atsopt_yyres_d0eclst (d0eclst_t) ;

/* ****** ****** */

typedef union {
t0kn_t t0kn ;
c0har_t c0har ;
e0xtcode_t e0xtcode ;
f0loat_t f0loat ;
f0loatsp_t f0loatsp ;
i0nt_t i0nt ;
i0ntsp_t i0ntsp ;
s0tring_t s0tring ;
i0de_t i0de ;
i0delst_t i0delst ;
l0ab_t l0ab ;
p0rec_t p0rec ;
abskind_t abskind ;
dcstkind_t dcstkind ;
datakind_t datakind ;
stadefkind_t stadefkind ;
valkind_t valkind ;
funkind_t funkind ;
lamkind_t lamkind ;
fixkind_t fixkind ;
srpifkindtok_t srpifkindtok ;
e0xp_t e0xp ;
e0xplst_t e0xplst ;
e0xpopt_t e0xpopt ;
e0fftag_t e0fftag ;
e0fftaglst_t e0fftaglst ;
e0fftaglstopt_t e0fftaglstopt ;
s0rt_t s0rt ;
s0rtq_t s0rtq ;
s0rtlst_t s0rtlst ;
s0rtopt_t s0rtopt ;
s0rtpol_t s0rtpol ;
d0atsrtcon_t d0atsrtcon ;
d0atsrtconlst_t d0atsrtconlst ;
d0atsrtdec_t d0atsrtdec ;
d0atsrtdeclst_t d0atsrtdeclst ;
s0taq_t s0taq ;
d0ynq_t d0ynq ;
sqi0de_t sqi0de ;
dqi0de_t dqi0de ;
arrqi0de_t arrqi0de ;
tmpqi0de_t tmpqi0de ;
s0arg_t s0arg ;
s0arglst_t s0arglst ;
s0arglstlst_t s0arglstlst ;
sp0at_t sp0at ;
s0exp_t s0exp ;
s0expext_t s0expext ;
s0explst_t s0explst ;
s0expopt_t s0expopt ;
s0explstlst_t s0explstlst ;
s0explstopt_t s0explstopt ;
labs0explst_t labs0explst ;
s0arrind_t s0arrind ;
t1mps0explstlst_t t1mps0explstlst ;
s0qua_t s0qua ;
s0qualst_t s0qualst ;
s0qualstlst_t s0qualstlst ;
s0qualstopt_t s0qualstopt ;
s0rtext_t s0rtext ;
impqi0de_t impqi0de ;
s0rtdef_t s0rtdef ;
s0rtdeflst_t s0rtdeflst ;
d0atarg_t d0atarg ;
d0atarglst_t d0atarglst ;
s0tacon_t s0tacon ;
s0taconlst_t s0taconlst ;
s0tacst_t s0tacst ;
s0tacstlst_t s0tacstlst ;
s0tavar_t s0tavar ;
s0tavarlst_t s0tavarlst ;
s0expdef_t s0expdef ;
s0expdeflst_t s0expdeflst ;
s0aspdec_t s0aspdec ;
d0atcon_t d0atcon ;
d0atconlst_t d0atconlst ;
d0atdec_t d0atdec ;
d0atdeclst_t d0atdeclst ;
e0xndec_t e0xndec ;
e0xndeclst_t e0xndeclst ;
p0arg_t p0arg ;
p0arglst_t p0arglst ;
d0arg_t d0arg ;
d0arglst_t d0arglst ;
extnamopt_t extnamopt ;
d0cstdec_t d0cstdec ;
d0cstdeclst_t d0cstdeclst ;
s0vararg_t s0vararg ;
s0exparg_t s0exparg ;
s0elop_t s0elop ;
witht0ype_t witht0ype ;
p0at_t p0at ;
p0atlst_t p0atlst ;
labp0atlst_t labp0atlst ;
f0arg_t f0arg ;
f0arglst_t f0arglst ;
d0exp_t d0exp ;
d0explst_t d0explst ;
d0expopt_t d0expopt ;
labd0explst_t labd0explst ;
d0arrind_t d0arrind ;
ifhead_t ifhead ;
casehead_t casehead ;
loophead_t loophead ;
tryhead_t tryhead ;
m0atch_t m0atch ;
m0atchlst_t m0atchlst ;
guap0at_t guap0at ;
c0lau_t c0lau ;
c0laulst_t c0laulst ;
sc0lau_t sc0lau ;
sc0laulst_t sc0laulst ;
i0nvarg_t i0nvarg ;
i0nvarglst_t i0nvarglst ;
i0nvresstate_t i0nvresstate ;
loopi0nv_t loopi0nv ;
initestpost_t initestpost ;
m0acarg_t m0acarg ;
m0acarglst_t m0acarglst ;
m0acdef_t m0acdef ;
m0acdeflst_t m0acdeflst ;
v0aldec_t v0aldec ;
v0aldeclst_t v0aldeclst ;
f0undec_t f0undec ;
f0undeclst_t f0undeclst ;
v0arwth_t v0arwth ;
v0ardec_t v0ardec ;
v0ardeclst_t v0ardeclst ;
i0mpdec_t i0mpdec ;
d0ec_t d0ec ;
d0eclst_t d0eclst ;
} YYSTYPE_union ;
#define YYSTYPE YYSTYPE_union



/* Line 189 of yacc.c  */
#line 1114 "ats_grammar_yats.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     YYBEG_i0de = 258,
     YYBEG_s0rtid = 259,
     YYBEG_si0de = 260,
     YYBEG_di0de = 261,
     YYBEG_s0exp = 262,
     YYBEG_d0exp = 263,
     YYBEG_d0ecseq_sta = 264,
     YYBEG_d0ecseq_dyn = 265,
     TOKEN_eof = 266,
     LITERAL_char = 267,
     LITERAL_extcode = 268,
     LITERAL_float = 269,
     LITERAL_floatsp = 270,
     LITERAL_int = 271,
     LITERAL_intsp = 272,
     LITERAL_string = 273,
     IDENTIFIER_alp = 274,
     IDENTIFIER_sym = 275,
     IDENTIFIER_arr = 276,
     IDENTIFIER_tmp = 277,
     IDENTIFIER_ext = 278,
     IDENTIFIER_dlr = 279,
     IDENTIFIER_srp = 280,
     ABSPROP = 281,
     ABSTYPE = 282,
     ABST0YPE = 283,
     ABSVIEW = 284,
     ABSVIEWTYPE = 285,
     ABSVIEWT0YPE = 286,
     AND = 287,
     AS = 288,
     ASSUME = 289,
     ATLAM = 290,
     ATLLAM = 291,
     ATFIX = 292,
     BEGIN = 293,
     BREAK = 294,
     CASE = 295,
     CASEMINUS = 296,
     CASEPLUS = 297,
     CASTFN = 298,
     CLASSDEC = 299,
     CONTINUE = 300,
     DATASORT = 301,
     DATAPARASORT = 302,
     DATAPROP = 303,
     DATATYPE = 304,
     DATAVIEW = 305,
     DATAVIEWTYPE = 306,
     DO = 307,
     DYN = 308,
     DYNLOAD = 309,
     ELSE = 310,
     END = 311,
     EXCEPTION = 312,
     EXTERN = 313,
     FIX = 314,
     FN = 315,
     FNSTAR = 316,
     FOR = 317,
     FORSTAR = 318,
     FUN = 319,
     IF = 320,
     IMPLEMENT = 321,
     IN = 322,
     INFIX = 323,
     INFIXL = 324,
     INFIXR = 325,
     LAM = 326,
     LET = 327,
     LLAM = 328,
     LOCAL = 329,
     MACDEF = 330,
     MACRODEF = 331,
     NONFIX = 332,
     OF = 333,
     OP = 334,
     OVERLOAD = 335,
     PAR = 336,
     POSTFIX = 337,
     PRAXI = 338,
     PREFIX = 339,
     PRFN = 340,
     PRFUN = 341,
     PROPDEF = 342,
     PROPMINUS = 343,
     PROPPLUS = 344,
     PRVAL = 345,
     REC = 346,
     R0EAD = 347,
     SCASE = 348,
     SIF = 349,
     SORTDEF = 350,
     STACST = 351,
     STADEF = 352,
     STAIF = 353,
     STALOAD = 354,
     STAVAR = 355,
     SYMELIM = 356,
     SYMINTR = 357,
     THEN = 358,
     TRY = 359,
     TYPEDEF = 360,
     TYPEMINUS = 361,
     TYPEPLUS = 362,
     T0YPE = 363,
     T0YPEMINUS = 364,
     T0YPEPLUS = 365,
     VAL = 366,
     VALMINUS = 367,
     VALPLUS = 368,
     VAR = 369,
     VIEWDEF = 370,
     VIEWMINUS = 371,
     VIEWPLUS = 372,
     VIEWTYPEDEF = 373,
     VIEWTYPEMINUS = 374,
     VIEWTYPEPLUS = 375,
     VIEWT0YPE = 376,
     VIEWT0YPEMINUS = 377,
     VIEWT0YPEPLUS = 378,
     WHEN = 379,
     WHERE = 380,
     WHILE = 381,
     WHILESTAR = 382,
     WITH = 383,
     WITHPROP = 384,
     WITHTYPE = 385,
     WITHVIEW = 386,
     WITHVIEWTYPE = 387,
     AMPERSAND = 388,
     BACKQUOTE = 389,
     BACKSLASH = 390,
     BANG = 391,
     BAR = 392,
     COMMA = 393,
     COLON = 394,
     SEMICOLON = 395,
     DOT = 396,
     EQ = 397,
     LT = 398,
     GT = 399,
     DOLLAR = 400,
     HASH = 401,
     TILDE = 402,
     DOTDOT = 403,
     DOTDOTDOT = 404,
     EQLT = 405,
     EQGT = 406,
     EQLTGT = 407,
     EQGTGT = 408,
     EQSLASHEQGT = 409,
     EQSLASHEQGTGT = 410,
     GTLT = 411,
     DOTLT = 412,
     GTDOT = 413,
     DOTLTGTDOT = 414,
     MINUSLT = 415,
     MINUSGT = 416,
     MINUSLTGT = 417,
     COLONLT = 418,
     COLONLTGT = 419,
     BACKQUOTELPAREN = 420,
     COMMALPAREN = 421,
     PERCENTLPAREN = 422,
     DLRARRSZ = 423,
     DLRLST_T = 424,
     DLRLST_VT = 425,
     DLRREC_T = 426,
     DLRREC_VT = 427,
     DLRTUP_T = 428,
     DLRTUP_VT = 429,
     DLRDELAY = 430,
     DLRLDELAY = 431,
     DLRDYNLOAD = 432,
     DLREFFMASK_ALL = 433,
     DLREFFMASK_EXN = 434,
     DLREFFMASK_NTM = 435,
     DLREFFMASK_REF = 436,
     DLRDECRYPT = 437,
     DLRENCRYPT = 438,
     DLREXTERN = 439,
     DLREXTVAL = 440,
     DLREXTYPE = 441,
     DLREXTYPE_STRUCT = 442,
     DLRFOLD = 443,
     DLRUNFOLD = 444,
     DLRRAISE = 445,
     DLRTYPEOF = 446,
     SRPFILENAME = 447,
     SRPLOCATION = 448,
     SRPCHARCOUNT = 449,
     SRPLINECOUNT = 450,
     SRPASSERT = 451,
     SRPDEFINE = 452,
     SRPELSE = 453,
     SRPELIF = 454,
     SRPELIFDEF = 455,
     SRPELIFNDEF = 456,
     SRPENDIF = 457,
     SRPERROR = 458,
     SRPIF = 459,
     SRPIFDEF = 460,
     SRPIFNDEF = 461,
     SRPINCLUDE = 462,
     SRPPRINT = 463,
     SRPTHEN = 464,
     SRPUNDEF = 465,
     FOLDAT = 466,
     FREEAT = 467,
     VIEWAT = 468,
     LPAREN = 469,
     RPAREN = 470,
     LBRACKET = 471,
     RBRACKET = 472,
     LBRACE = 473,
     RBRACE = 474,
     ATLPAREN = 475,
     ATLBRACKET = 476,
     ATLBRACE = 477,
     QUOTELPAREN = 478,
     QUOTELBRACKET = 479,
     QUOTELBRACE = 480,
     HASHLPAREN = 481,
     HASHLBRACKET = 482,
     HASHLBRACE = 483,
     PATAS = 484,
     PATFREE = 485,
     SEXPLAM = 486,
     DEXPLAM = 487,
     DEXPFIX = 488,
     CLAUS = 489,
     DEXPCASE = 490,
     DEXPIF = 491,
     DEXPRAISE = 492,
     DEXPTRY = 493,
     DEXPFOR = 494,
     DEXPWHILE = 495,
     BARCLAUSSEQNONE = 496,
     TMPSEXP = 497,
     TMPSARG = 498,
     SARRIND = 499,
     SEXPDARGSEQEMPTY = 500
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 1400 "ats_grammar_yats.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  178
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3319

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  246
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  205
/* YYNRULES -- Number of rules.  */
#define YYNRULES  651
/* YYNRULES -- Number of states.  */
#define YYNSTATES  1252

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   500

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     7,    11,    15,    19,    23,    27,    31,
      35,    37,    39,    41,    43,    45,    47,    49,    51,    53,
      55,    57,    59,    61,    63,    65,    67,    69,    71,    73,
      75,    77,    79,    81,    83,    85,    87,    89,    91,    93,
      95,    97,    99,   101,   103,   105,   107,   109,   111,   113,
     115,   117,   119,   121,   122,   124,   126,   128,   130,   132,
     134,   136,   138,   140,   142,   144,   146,   148,   150,   151,
     154,   156,   158,   160,   162,   164,   168,   170,   171,   173,
     177,   183,   186,   188,   190,   192,   194,   196,   198,   200,
     202,   206,   210,   211,   214,   215,   219,   220,   222,   224,
     227,   230,   232,   234,   236,   237,   240,   241,   245,   247,
     249,   253,   256,   258,   261,   265,   267,   269,   271,   273,
     275,   277,   279,   281,   284,   288,   292,   293,   296,   297,
     301,   303,   305,   307,   309,   311,   313,   315,   317,   319,
     321,   323,   325,   327,   329,   333,   335,   338,   339,   343,
     347,   348,   352,   355,   358,   362,   365,   368,   372,   376,
     381,   383,   385,   387,   389,   391,   393,   395,   397,   399,
     401,   403,   406,   407,   411,   413,   415,   417,   419,   421,
     423,   425,   427,   429,   431,   434,   436,   438,   440,   443,
     445,   447,   450,   452,   454,   457,   458,   461,   464,   465,
     468,   469,   473,   474,   477,   482,   483,   486,   487,   491,
     492,   497,   502,   504,   506,   510,   516,   518,   520,   522,
     524,   527,   530,   534,   540,   544,   548,   553,   558,   564,
     570,   577,   584,   588,   592,   597,   602,   609,   615,   619,
     621,   625,   629,   633,   636,   638,   641,   644,   645,   649,
     653,   656,   661,   663,   668,   669,   672,   673,   677,   681,
     683,   692,   693,   695,   696,   700,   704,   705,   709,   712,
     713,   718,   719,   725,   727,   730,   732,   733,   736,   737,
     741,   742,   746,   748,   753,   757,   758,   762,   764,   768,
     769,   772,   773,   777,   779,   784,   788,   795,   796,   800,
     804,   811,   812,   816,   820,   821,   825,   831,   832,   836,
     842,   843,   848,   849,   853,   854,   857,   862,   864,   867,
     868,   872,   876,   883,   884,   888,   889,   893,   897,   898,
     902,   904,   908,   909,   912,   913,   917,   919,   923,   929,
     933,   934,   937,   938,   941,   947,   948,   952,   954,   956,
     958,   960,   962,   964,   966,   968,   969,   972,   975,   978,
     981,   984,   988,   992,   997,  1000,  1002,  1004,  1006,  1008,
    1010,  1013,  1016,  1019,  1023,  1029,  1033,  1037,  1041,  1047,
    1053,  1057,  1061,  1065,  1067,  1071,  1072,  1075,  1076,  1079,
    1080,  1084,  1086,  1091,  1092,  1095,  1101,  1105,  1107,  1111,
    1113,  1114,  1117,  1121,  1123,  1124,  1127,  1130,  1134,  1139,
    1146,  1153,  1158,  1163,  1169,  1176,  1180,  1184,  1187,  1192,
    1198,  1200,  1202,  1204,  1206,  1208,  1210,  1212,  1214,  1216,
    1219,  1222,  1224,  1226,  1228,  1230,  1233,  1236,  1238,  1240,
    1242,  1244,  1246,  1248,  1250,  1252,  1254,  1256,  1263,  1273,
    1279,  1282,  1285,  1289,  1294,  1300,  1304,  1310,  1316,  1322,
    1326,  1330,  1334,  1338,  1342,  1347,  1352,  1358,  1364,  1368,
    1372,  1377,  1382,  1389,  1393,  1397,  1401,  1407,  1411,  1415,
    1416,  1419,  1421,  1423,  1424,  1427,  1430,  1435,  1436,  1439,
    1441,  1443,  1447,  1448,  1451,  1454,  1457,  1460,  1463,  1466,
    1469,  1471,  1475,  1479,  1481,  1482,  1485,  1486,  1490,  1491,
    1493,  1497,  1501,  1502,  1507,  1508,  1514,  1516,  1520,  1523,
    1524,  1528,  1530,  1534,  1538,  1542,  1546,  1550,  1552,  1555,
    1556,  1560,  1564,  1566,  1569,  1570,  1574,  1575,  1579,  1580,
    1584,  1586,  1589,  1593,  1594,  1597,  1598,  1602,  1606,  1607,
    1611,  1612,  1618,  1623,  1631,  1633,  1634,  1637,  1638,  1642,
    1644,  1648,  1649,  1652,  1657,  1658,  1662,  1667,  1668,  1672,
    1678,  1686,  1687,  1691,  1692,  1695,  1700,  1706,  1711,  1718,
    1719,  1723,  1729,  1733,  1737,  1741,  1745,  1749,  1752,  1755,
    1758,  1762,  1765,  1768,  1771,  1775,  1779,  1783,  1787,  1791,
    1795,  1799,  1802,  1807,  1811,  1814,  1819,  1824,  1830,  1834,
    1839,  1843,  1848,  1851,  1856,  1860,  1861,  1864,  1865,  1868,
    1870,  1875,  1877,  1880,  1883,  1889,  1894,  1901,  1907,  1909,
    1910,  1914,  1916,  1922,  1928,  1934,  1938,  1943,  1948,  1953,
    1957,  1961,  1967,  1969,  1972,  1975,  1978,  1983,  1990,  1996,
    1998,  1999
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     247,     0,    -1,     9,   445,    11,    -1,    10,   449,    11,
      -1,     3,   259,    11,    -1,     4,   278,    11,    -1,     5,
     290,    11,    -1,     6,   293,    11,    -1,     7,   310,    11,
      -1,     8,   381,    11,    -1,    26,    -1,    27,    -1,    28,
      -1,    29,    -1,    30,    -1,    31,    -1,    64,    -1,   111,
      -1,    43,    -1,    83,    -1,    86,    -1,    90,    -1,    48,
      -1,    49,    -1,    50,    -1,    51,    -1,    97,    -1,    87,
      -1,   105,    -1,   115,    -1,   118,    -1,   111,    -1,   112,
      -1,   113,    -1,    90,    -1,    60,    -1,    61,    -1,    64,
      -1,    43,    -1,    85,    -1,    86,    -1,    71,    -1,    35,
      -1,    73,    -1,    36,    -1,    59,    -1,    37,    -1,   204,
      -1,   205,    -1,   206,    -1,   199,    -1,   200,    -1,   201,
      -1,    -1,   209,    -1,    19,    -1,    20,    -1,   133,    -1,
     135,    -1,   136,    -1,   142,    -1,   144,    -1,   156,    -1,
     143,    -1,   161,    -1,   162,    -1,   147,    -1,    24,    -1,
      -1,   259,   261,    -1,    23,    -1,    52,    -1,   126,    -1,
     259,    -1,    16,    -1,   214,   263,   215,    -1,    19,    -1,
      -1,    16,    -1,   214,   259,   215,    -1,   214,   259,    20,
      16,   215,    -1,   266,   267,    -1,   267,    -1,    12,    -1,
      14,    -1,    16,    -1,    18,    -1,   192,    -1,   193,    -1,
     259,    -1,   214,   268,   215,    -1,   167,   266,   215,    -1,
      -1,   266,   269,    -1,    -1,   138,   266,   269,    -1,    -1,
     266,    -1,    19,    -1,   136,   271,    -1,   147,   271,    -1,
     271,    -1,    64,    -1,    16,    -1,    -1,   272,   274,    -1,
      -1,   138,   272,   274,    -1,   139,    -1,   164,    -1,   163,
     273,   144,    -1,   276,   279,    -1,   279,    -1,   260,   141,
      -1,   145,    18,   141,    -1,    19,    -1,    20,    -1,   108,
      -1,   121,    -1,   135,    -1,   161,    -1,   162,    -1,   278,
      -1,   277,   278,    -1,   214,   280,   215,    -1,   220,   280,
     215,    -1,    -1,   276,   281,    -1,    -1,   138,   276,   281,
      -1,   276,    -1,    88,    -1,    89,    -1,   106,    -1,   107,
      -1,   109,    -1,   110,    -1,   116,    -1,   117,    -1,   119,
      -1,   120,    -1,   122,    -1,   123,    -1,   259,    -1,   259,
      78,   276,    -1,   285,    -1,   283,   285,    -1,    -1,   137,
     283,   285,    -1,   259,   142,   284,    -1,    -1,    32,   286,
     287,    -1,   260,   141,    -1,   260,   139,    -1,   145,    18,
     141,    -1,   260,   141,    -1,   260,   139,    -1,   260,   260,
     139,    -1,   145,    18,   141,    -1,   145,    18,   260,   139,
      -1,    19,    -1,    20,    -1,    92,    -1,   133,    -1,   135,
      -1,   136,    -1,   144,    -1,   143,    -1,   161,    -1,   147,
      -1,   290,    -1,   288,   290,    -1,    -1,   138,   290,   292,
      -1,    19,    -1,    20,    -1,   135,    -1,   136,    -1,   142,
      -1,   144,    -1,   156,    -1,   143,    -1,   147,    -1,   293,
      -1,   289,   293,    -1,    19,    -1,    20,    -1,   293,    -1,
      79,   293,    -1,    21,    -1,   297,    -1,   289,   297,    -1,
      22,    -1,   299,    -1,   289,   299,    -1,    -1,   139,   276,
      -1,   290,   301,    -1,    -1,   302,   304,    -1,    -1,   138,
     302,   304,    -1,    -1,   290,   305,    -1,   214,   303,   215,
     305,    -1,    -1,   302,   307,    -1,    -1,   138,   302,   307,
      -1,    -1,   218,   306,   219,   308,    -1,   291,   214,   303,
     215,    -1,   312,    -1,   313,    -1,   310,   139,   276,    -1,
      71,   305,   301,   151,   310,    -1,    12,    -1,    16,    -1,
      17,    -1,   290,    -1,    79,   290,    -1,   288,   290,    -1,
     214,   320,   215,    -1,   214,   320,   137,   320,   215,    -1,
     220,   320,   215,    -1,   223,   320,   215,    -1,   173,   214,
     320,   215,    -1,   174,   214,   320,   215,    -1,   220,   320,
     137,   320,   215,    -1,   223,   320,   137,   320,   215,    -1,
     173,   214,   320,   137,   320,   215,    -1,   174,   214,   320,
     137,   320,   215,    -1,   222,   324,   219,    -1,   225,   324,
     219,    -1,   171,   218,   324,   219,    -1,   172,   218,   324,
     219,    -1,   187,    18,    78,   218,   324,   219,    -1,   221,
     310,   217,   216,   315,    -1,   160,   273,   144,    -1,   162,
      -1,   218,   317,   219,    -1,   216,   317,   217,    -1,   227,
     317,   217,    -1,   312,   311,    -1,   311,    -1,   186,    18,
      -1,   313,   311,    -1,    -1,   218,   310,   219,    -1,   216,
     310,   217,    -1,   320,   217,    -1,   320,   217,   216,   315,
      -1,   312,    -1,   290,   292,   139,   319,    -1,    -1,   316,
     318,    -1,    -1,   137,   316,   318,    -1,   140,   316,   318,
      -1,   276,    -1,   218,   290,   139,   319,   137,   310,   321,
     219,    -1,    -1,   323,    -1,    -1,   137,   310,   321,    -1,
     140,   310,   321,    -1,    -1,   138,   310,   322,    -1,   310,
     322,    -1,    -1,   263,   142,   310,   325,    -1,    -1,   138,
     263,   142,   310,   325,    -1,   311,    -1,   326,   311,    -1,
     326,    -1,    -1,   327,   329,    -1,    -1,   138,   327,   329,
      -1,    -1,   156,   328,   330,    -1,   294,    -1,   300,   328,
     330,   144,    -1,   278,   142,   319,    -1,    -1,    32,   332,
     333,    -1,   282,    -1,   259,   139,   282,    -1,    -1,   334,
     336,    -1,    -1,   138,   334,   336,    -1,   290,    -1,   290,
     214,   335,   215,    -1,   290,   142,   310,    -1,   290,   214,
     335,   215,   142,   310,    -1,    -1,    32,   337,   338,    -1,
     290,   139,   276,    -1,   290,   214,   335,   215,   139,   276,
      -1,    -1,    32,   339,   340,    -1,   290,   139,   276,    -1,
      -1,    32,   341,   342,    -1,   290,   305,   301,   142,   310,
      -1,    -1,    32,   343,   344,    -1,   291,   305,   301,   142,
     310,    -1,    -1,   218,   317,   219,   346,    -1,    -1,   214,
     320,   215,    -1,    -1,    78,   310,    -1,   346,   293,   347,
     348,    -1,   351,    -1,   349,   351,    -1,    -1,   137,   349,
     351,    -1,   290,   142,   350,    -1,   290,   214,   335,   215,
     142,   350,    -1,    -1,    32,   352,   353,    -1,    -1,   125,
     343,   344,    -1,   346,   293,   348,    -1,    -1,    32,   355,
     356,    -1,   295,    -1,   295,   139,   310,    -1,    -1,   357,
     359,    -1,    -1,   138,   357,   359,    -1,   295,    -1,   214,
     358,   215,    -1,   214,   358,   137,   358,   215,    -1,   218,
     317,   219,    -1,    -1,   360,   361,    -1,    -1,   142,    18,
      -1,   293,   361,   275,   310,   362,    -1,    -1,    32,   363,
     364,    -1,   148,    -1,   149,    -1,   303,    -1,   148,    -1,
     149,    -1,   323,    -1,   141,    -1,   161,    -1,    -1,   129,
     310,    -1,   130,   310,    -1,   131,   310,    -1,   132,   310,
      -1,   370,   372,    -1,   369,   139,   310,    -1,   295,    33,
     369,    -1,   136,   295,    33,   369,    -1,   147,   369,    -1,
      12,    -1,    16,    -1,    14,    -1,    18,    -1,   295,    -1,
     136,   295,    -1,    79,   295,    -1,   289,   295,    -1,   214,
     373,   215,    -1,   214,   373,   137,   373,   215,    -1,   224,
     373,   217,    -1,   220,   373,   215,    -1,   223,   373,   215,
      -1,   220,   373,   137,   373,   215,    -1,   223,   373,   137,
     373,   215,    -1,   222,   375,   219,    -1,   225,   375,   219,
      -1,   216,   303,   217,    -1,   370,    -1,   218,   365,   219,
      -1,    -1,   371,   372,    -1,    -1,   369,   374,    -1,    -1,
     138,   369,   374,    -1,   149,    -1,   263,   142,   369,   376,
      -1,    -1,   138,   149,    -1,   138,   263,   142,   369,   376,
      -1,   218,   317,   219,    -1,   370,    -1,   157,   320,   158,
      -1,   159,    -1,    -1,   377,   378,    -1,   218,   303,   219,
      -1,   370,    -1,    -1,   379,   380,    -1,   382,   386,    -1,
     381,   139,   310,    -1,   391,   381,   103,   381,    -1,   391,
     381,   103,   381,    55,   381,    -1,   392,   310,   103,   381,
      55,   381,    -1,   393,   381,    78,   409,    -1,   394,   310,
      78,   412,    -1,   254,   378,   388,   389,   381,    -1,   255,
     293,   378,   388,   389,   381,    -1,   395,   423,   381,    -1,
     396,   382,   381,    -1,   190,   381,    -1,   397,   400,   128,
     409,    -1,   381,   125,   218,   449,   219,    -1,    12,    -1,
      14,    -1,    15,    -1,    16,    -1,    17,    -1,    18,    -1,
     192,    -1,   193,    -1,   293,    -1,    79,   293,    -1,   289,
     259,    -1,   262,    -1,   133,    -1,    39,    -1,    45,    -1,
     211,   384,    -1,   212,   384,    -1,   213,    -1,   182,    -1,
     183,    -1,   175,    -1,   176,    -1,   177,    -1,   178,    -1,
     179,    -1,   180,    -1,   181,    -1,   221,   310,   217,   214,
     398,   215,    -1,   221,   310,   217,   216,   381,   217,   214,
     398,   215,    -1,   168,   314,   214,   398,   215,    -1,   298,
     387,    -1,   367,   263,    -1,   367,   216,   387,    -1,   300,
     328,   330,   144,    -1,   227,   366,   137,   381,   217,    -1,
     214,   398,   215,    -1,   214,   398,   137,   398,   215,    -1,
     169,   314,   214,   398,   215,    -1,   170,   314,   214,   398,
     215,    -1,   224,   398,   217,    -1,    38,   400,    56,    -1,
     214,   401,   215,    -1,   220,   398,   215,    -1,   223,   398,
     215,    -1,   173,   214,   398,   215,    -1,   174,   214,   398,
     215,    -1,   220,   398,   137,   398,   215,    -1,   223,   398,
     137,   398,   215,    -1,   222,   402,   219,    -1,   225,   402,
     219,    -1,   171,   218,   402,   219,    -1,   172,   218,   402,
     219,    -1,   185,   214,   310,   138,    18,   215,    -1,   167,
     381,   215,    -1,   166,   381,   215,    -1,   165,   400,   215,
      -1,    72,   449,    67,   400,    56,    -1,   218,   449,   219,
      -1,   218,   366,   219,    -1,    -1,   383,   384,    -1,   382,
      -1,   383,    -1,    -1,   385,   386,    -1,   398,   217,    -1,
     398,   217,   216,   387,    -1,    -1,   139,   310,    -1,   151,
      -1,   152,    -1,   150,   273,   144,    -1,    -1,   421,   151,
      -1,    65,   390,    -1,    94,   390,    -1,    40,   390,    -1,
      41,   390,    -1,    42,   390,    -1,    93,   390,    -1,    62,
      -1,    63,   422,   151,    -1,   127,   422,   151,    -1,   104,
      -1,    -1,   381,   399,    -1,    -1,   138,   381,   399,    -1,
      -1,   381,    -1,   381,   140,   400,    -1,   381,   140,   400,
      -1,    -1,   263,   142,   381,   403,    -1,    -1,   138,   263,
     142,   381,   403,    -1,   381,    -1,   381,    33,   369,    -1,
     404,   406,    -1,    -1,    32,   404,   406,    -1,   369,    -1,
     369,   124,   405,    -1,   407,   151,   381,    -1,   407,   153,
     381,    -1,   407,   154,   381,    -1,   407,   155,   381,    -1,
     410,    -1,   408,   410,    -1,    -1,   137,   408,   410,    -1,
     309,   151,   381,    -1,   413,    -1,   411,   413,    -1,    -1,
     137,   411,   413,    -1,    -1,   218,   317,   219,    -1,    -1,
     157,   320,   158,    -1,   159,    -1,   293,   139,    -1,   293,
     139,   310,    -1,    -1,   416,   418,    -1,    -1,   138,   416,
     418,    -1,   214,   417,   215,    -1,    -1,   216,   317,   217,
      -1,    -1,   139,   420,   214,   417,   215,    -1,   414,   415,
     419,   421,    -1,   214,   398,   140,   398,   140,   398,   215,
      -1,   295,    -1,    -1,   424,   426,    -1,    -1,   138,   424,
     426,    -1,   424,    -1,   214,   425,   215,    -1,    -1,   427,
     428,    -1,   293,   428,   142,   381,    -1,    -1,    32,   429,
     430,    -1,   369,   142,   381,   368,    -1,    -1,    32,   431,
     432,    -1,   296,   378,   142,   381,   368,    -1,   296,   378,
     275,   310,   142,   381,   368,    -1,    -1,    32,   433,   434,
      -1,    -1,   128,   295,    -1,   295,   435,   142,   381,    -1,
     136,   295,   435,   142,   381,    -1,   295,   139,   310,   435,
      -1,   295,   139,   310,   435,   142,   381,    -1,    -1,    32,
     436,   437,    -1,   331,   380,   388,   142,   381,    -1,    68,
     265,   261,    -1,    69,   265,   261,    -1,    70,   265,   261,
      -1,    84,   265,   261,    -1,    82,   265,   261,    -1,    77,
     261,    -1,   102,   261,    -1,   210,   259,    -1,   197,   259,
     270,    -1,   196,   266,    -1,   203,   266,    -1,   208,   266,
      -1,    95,   332,   333,    -1,    46,   286,   287,    -1,    47,
     286,   287,    -1,   248,   337,   338,    -1,    96,   339,   340,
      -1,   100,   341,   342,    -1,   251,   343,   344,    -1,    34,
     345,    -1,   250,   352,   353,   354,    -1,    57,   355,   356,
      -1,    44,   290,    -1,    44,   290,   139,   310,    -1,    80,
     293,   128,   294,    -1,    80,   216,   217,   128,   294,    -1,
      75,   429,   430,    -1,    75,    91,   429,   430,    -1,    76,
     429,   430,    -1,    76,    91,   429,   430,    -1,    99,    18,
      -1,    99,   264,   142,    18,    -1,   218,   317,   219,    -1,
      -1,   440,   441,    -1,    -1,   442,   140,    -1,   439,    -1,
     249,   441,   363,   364,    -1,    13,    -1,   256,   444,    -1,
     207,    18,    -1,    74,   445,    67,   445,    56,    -1,   266,
     258,   445,   202,    -1,   266,   258,   445,   198,   445,   202,
      -1,   266,   258,   445,   257,   444,    -1,   446,    -1,    -1,
     446,   443,   442,    -1,   439,    -1,    58,   249,   441,   363,
     364,    -1,    58,   105,    18,   142,   310,    -1,    58,   111,
      18,   142,   381,    -1,   252,   431,   432,    -1,   111,    81,
     431,   432,    -1,   111,    91,   431,   432,    -1,   253,   441,
     433,   434,    -1,   114,   436,   437,    -1,    66,   308,   438,
      -1,    74,   449,    67,   449,    56,    -1,    13,    -1,   256,
     448,    -1,   207,    18,    -1,    54,    18,    -1,   266,   258,
     449,   202,    -1,   266,   258,   449,   198,   449,   202,    -1,
     266,   258,   449,   257,   448,    -1,   450,    -1,    -1,   450,
     447,   442,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1557,  1557,  1558,  1559,  1560,  1561,  1562,  1563,  1564,
    1568,  1569,  1570,  1571,  1572,  1573,  1577,  1578,  1579,  1580,
    1581,  1582,  1586,  1587,  1588,  1589,  1593,  1594,  1595,  1596,
    1597,  1601,  1602,  1603,  1604,  1608,  1609,  1610,  1611,  1612,
    1613,  1617,  1618,  1619,  1620,  1624,  1625,  1629,  1630,  1631,
    1635,  1636,  1637,  1641,  1642,  1646,  1647,  1648,  1649,  1650,
    1651,  1652,  1653,  1654,  1655,  1656,  1657,  1661,  1665,  1666,
    1670,  1671,  1672,  1676,  1677,  1678,  1682,  1686,  1687,  1688,
    1689,  1693,  1694,  1698,  1699,  1700,  1701,  1702,  1703,  1704,
    1705,  1706,  1710,  1711,  1715,  1716,  1720,  1721,  1725,  1729,
    1730,  1731,  1732,  1733,  1737,  1738,  1742,  1743,  1747,  1748,
    1749,  1753,  1754,  1758,  1759,  1763,  1764,  1765,  1766,  1767,
    1768,  1769,  1773,  1774,  1775,  1776,  1780,  1781,  1785,  1786,
    1790,  1791,  1792,  1793,  1794,  1795,  1796,  1797,  1798,  1799,
    1800,  1801,  1802,  1806,  1807,  1811,  1812,  1816,  1817,  1821,
    1825,  1826,  1830,  1831,  1832,  1836,  1837,  1838,  1839,  1840,
    1844,  1845,  1846,  1847,  1848,  1849,  1850,  1851,  1852,  1853,
    1857,  1858,  1862,  1863,  1867,  1868,  1869,  1870,  1871,  1872,
    1873,  1874,  1875,  1879,  1880,  1884,  1885,  1889,  1890,  1894,
    1898,  1899,  1903,  1907,  1908,  1912,  1913,  1917,  1921,  1922,
    1926,  1927,  1931,  1932,  1933,  1937,  1938,  1942,  1943,  1947,
    1948,  1952,  1956,  1957,  1958,  1959,  1963,  1964,  1965,  1966,
    1967,  1968,  1969,  1970,  1971,  1972,  1973,  1974,  1975,  1976,
    1977,  1978,  1979,  1980,  1981,  1982,  1983,  1984,  1985,  1986,
    1987,  1988,  1989,  1993,  1994,  1998,  1999,  2003,  2004,  2005,
    2009,  2010,  2014,  2015,  2019,  2020,  2024,  2025,  2026,  2030,
    2031,  2035,  2036,  2040,  2041,  2042,  2046,  2047,  2051,  2055,
    2056,  2060,  2061,  2065,  2066,  2070,  2074,  2075,  2079,  2080,
    2084,  2085,  2089,  2090,  2094,  2098,  2099,  2103,  2104,  2108,
    2109,  2113,  2114,  2118,  2119,  2120,  2121,  2125,  2126,  2130,
    2131,  2135,  2136,  2140,  2144,  2145,  2149,  2153,  2154,  2158,
    2162,  2163,  2167,  2168,  2172,  2173,  2177,  2181,  2182,  2186,
    2187,  2191,  2192,  2196,  2197,  2201,  2202,  2206,  2210,  2211,
    2215,  2216,  2220,  2221,  2225,  2226,  2230,  2231,  2232,  2233,
    2237,  2238,  2242,  2243,  2247,  2251,  2252,  2256,  2257,  2258,
    2262,  2263,  2264,  2268,  2269,  2273,  2274,  2275,  2276,  2277,
    2281,  2282,  2283,  2284,  2285,  2289,  2290,  2291,  2292,  2293,
    2294,  2295,  2296,  2297,  2298,  2299,  2300,  2301,  2302,  2303,
    2304,  2305,  2306,  2310,  2311,  2315,  2316,  2320,  2321,  2325,
    2326,  2330,  2331,  2335,  2336,  2337,  2341,  2342,  2343,  2344,
    2348,  2349,  2353,  2354,  2358,  2359,  2363,  2364,  2365,  2366,
    2367,  2368,  2369,  2370,  2371,  2372,  2373,  2374,  2375,  2376,
    2380,  2381,  2382,  2383,  2384,  2385,  2386,  2387,  2388,  2389,
    2390,  2391,  2392,  2393,  2394,  2395,  2396,  2397,  2398,  2399,
    2400,  2401,  2402,  2403,  2404,  2405,  2406,  2407,  2408,  2409,
    2410,  2411,  2412,  2413,  2414,  2415,  2416,  2417,  2418,  2419,
    2420,  2421,  2422,  2423,  2424,  2425,  2426,  2427,  2428,  2429,
    2430,  2431,  2432,  2433,  2434,  2435,  2436,  2437,  2441,  2445,
    2446,  2450,  2451,  2455,  2456,  2460,  2461,  2465,  2466,  2470,
    2471,  2472,  2476,  2477,  2481,  2485,  2489,  2490,  2491,  2495,
    2499,  2500,  2504,  2508,  2512,  2513,  2517,  2518,  2522,  2523,
    2524,  2528,  2532,  2533,  2537,  2538,  2542,  2543,  2547,  2551,
    2552,  2556,  2557,  2561,  2562,  2563,  2564,  2568,  2569,  2573,
    2574,  2578,  2582,  2583,  2587,  2588,  2592,  2593,  2597,  2598,
    2599,  2603,  2604,  2608,  2609,  2613,  2614,  2618,  2622,  2623,
    2627,  2628,  2632,  2636,  2640,  2644,  2645,  2649,  2650,  2654,
    2655,  2659,  2660,  2664,  2668,  2669,  2673,  2677,  2678,  2682,
    2683,  2687,  2688,  2692,  2693,  2697,  2698,  2699,  2700,  2704,
    2705,  2709,  2713,  2714,  2715,  2716,  2717,  2718,  2719,  2720,
    2721,  2722,  2723,  2724,  2725,  2726,  2727,  2728,  2729,  2730,
    2731,  2732,  2733,  2734,  2735,  2736,  2737,  2738,  2739,  2740,
    2741,  2742,  2743,  2744,  2748,  2752,  2753,  2757,  2758,  2762,
    2763,  2764,  2765,  2766,  2767,  2771,  2772,  2773,  2777,  2781,
    2782,  2786,  2787,  2788,  2789,  2790,  2791,  2792,  2793,  2794,
    2795,  2796,  2797,  2798,  2799,  2800,  2804,  2805,  2806,  2810,
    2814,  2815
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "YYBEG_i0de", "YYBEG_s0rtid",
  "YYBEG_si0de", "YYBEG_di0de", "YYBEG_s0exp", "YYBEG_d0exp",
  "YYBEG_d0ecseq_sta", "YYBEG_d0ecseq_dyn", "TOKEN_eof", "LITERAL_char",
  "LITERAL_extcode", "LITERAL_float", "LITERAL_floatsp", "LITERAL_int",
  "LITERAL_intsp", "LITERAL_string", "IDENTIFIER_alp", "IDENTIFIER_sym",
  "IDENTIFIER_arr", "IDENTIFIER_tmp", "IDENTIFIER_ext", "IDENTIFIER_dlr",
  "IDENTIFIER_srp", "ABSPROP", "ABSTYPE", "ABST0YPE", "ABSVIEW",
  "ABSVIEWTYPE", "ABSVIEWT0YPE", "AND", "AS", "ASSUME", "ATLAM", "ATLLAM",
  "ATFIX", "BEGIN", "BREAK", "CASE", "CASEMINUS", "CASEPLUS", "CASTFN",
  "CLASSDEC", "CONTINUE", "DATASORT", "DATAPARASORT", "DATAPROP",
  "DATATYPE", "DATAVIEW", "DATAVIEWTYPE", "DO", "DYN", "DYNLOAD", "ELSE",
  "END", "EXCEPTION", "EXTERN", "FIX", "FN", "FNSTAR", "FOR", "FORSTAR",
  "FUN", "IF", "IMPLEMENT", "IN", "INFIX", "INFIXL", "INFIXR", "LAM",
  "LET", "LLAM", "LOCAL", "MACDEF", "MACRODEF", "NONFIX", "OF", "OP",
  "OVERLOAD", "PAR", "POSTFIX", "PRAXI", "PREFIX", "PRFN", "PRFUN",
  "PROPDEF", "PROPMINUS", "PROPPLUS", "PRVAL", "REC", "R0EAD", "SCASE",
  "SIF", "SORTDEF", "STACST", "STADEF", "STAIF", "STALOAD", "STAVAR",
  "SYMELIM", "SYMINTR", "THEN", "TRY", "TYPEDEF", "TYPEMINUS", "TYPEPLUS",
  "T0YPE", "T0YPEMINUS", "T0YPEPLUS", "VAL", "VALMINUS", "VALPLUS", "VAR",
  "VIEWDEF", "VIEWMINUS", "VIEWPLUS", "VIEWTYPEDEF", "VIEWTYPEMINUS",
  "VIEWTYPEPLUS", "VIEWT0YPE", "VIEWT0YPEMINUS", "VIEWT0YPEPLUS", "WHEN",
  "WHERE", "WHILE", "WHILESTAR", "WITH", "WITHPROP", "WITHTYPE",
  "WITHVIEW", "WITHVIEWTYPE", "AMPERSAND", "BACKQUOTE", "BACKSLASH",
  "BANG", "BAR", "COMMA", "COLON", "SEMICOLON", "DOT", "EQ", "LT", "GT",
  "DOLLAR", "HASH", "TILDE", "DOTDOT", "DOTDOTDOT", "EQLT", "EQGT",
  "EQLTGT", "EQGTGT", "EQSLASHEQGT", "EQSLASHEQGTGT", "GTLT", "DOTLT",
  "GTDOT", "DOTLTGTDOT", "MINUSLT", "MINUSGT", "MINUSLTGT", "COLONLT",
  "COLONLTGT", "BACKQUOTELPAREN", "COMMALPAREN", "PERCENTLPAREN",
  "DLRARRSZ", "DLRLST_T", "DLRLST_VT", "DLRREC_T", "DLRREC_VT", "DLRTUP_T",
  "DLRTUP_VT", "DLRDELAY", "DLRLDELAY", "DLRDYNLOAD", "DLREFFMASK_ALL",
  "DLREFFMASK_EXN", "DLREFFMASK_NTM", "DLREFFMASK_REF", "DLRDECRYPT",
  "DLRENCRYPT", "DLREXTERN", "DLREXTVAL", "DLREXTYPE", "DLREXTYPE_STRUCT",
  "DLRFOLD", "DLRUNFOLD", "DLRRAISE", "DLRTYPEOF", "SRPFILENAME",
  "SRPLOCATION", "SRPCHARCOUNT", "SRPLINECOUNT", "SRPASSERT", "SRPDEFINE",
  "SRPELSE", "SRPELIF", "SRPELIFDEF", "SRPELIFNDEF", "SRPENDIF",
  "SRPERROR", "SRPIF", "SRPIFDEF", "SRPIFNDEF", "SRPINCLUDE", "SRPPRINT",
  "SRPTHEN", "SRPUNDEF", "FOLDAT", "FREEAT", "VIEWAT", "LPAREN", "RPAREN",
  "LBRACKET", "RBRACKET", "LBRACE", "RBRACE", "ATLPAREN", "ATLBRACKET",
  "ATLBRACE", "QUOTELPAREN", "QUOTELBRACKET", "QUOTELBRACE", "HASHLPAREN",
  "HASHLBRACKET", "HASHLBRACE", "PATAS", "PATFREE", "SEXPLAM", "DEXPLAM",
  "DEXPFIX", "CLAUS", "DEXPCASE", "DEXPIF", "DEXPRAISE", "DEXPTRY",
  "DEXPFOR", "DEXPWHILE", "BARCLAUSSEQNONE", "TMPSEXP", "TMPSARG",
  "SARRIND", "SEXPDARGSEQEMPTY", "$accept", "theStartEntry", "abskind",
  "dcstkind", "datakind", "stadefkind", "valkind", "funkind", "lamkind",
  "fixkind", "srpifkind", "srpelifkind", "srpthenopt", "i0de", "i0de_dlr",
  "i0deseq", "i0dext", "l0ab", "stai0de", "p0rec", "e0xp", "atme0xp",
  "e0xpseq", "commae0xpseq", "e0xpopt", "e0ffid", "e0fftag", "e0fftagseq",
  "commae0fftagseq", "colonwith", "s0rt", "s0rtq", "s0rtid", "atms0rt",
  "s0rtseq", "commas0rtseq", "s0rtpol", "d0atsrtcon", "d0atsrtconseq",
  "bard0atsrtconseq", "d0atsrtdec", "andd0atsrtdecseq", "s0taq", "d0ynq",
  "si0de", "sqi0de", "commasi0deseq", "di0de", "dqi0de", "pi0de", "fi0de",
  "arri0de", "arrqi0de", "tmpi0de", "tmpqi0de", "colons0rtopt", "s0arg",
  "s0argseq", "commas0argseq", "s0argseqseq", "decs0argseq",
  "commadecs0argseq", "decs0argseqseq", "sp0at", "s0exp", "atms0exp",
  "apps0exp", "exts0exp", "s0expelt", "s0arrind", "s0qua", "s0quaseq",
  "barsemis0quaseq", "s0rtext", "s0expseq", "barsemis0expseq",
  "commas0expseq", "s0expseq1", "labs0expseq", "commalabs0expseq",
  "t0mps0exp", "t1mps0exp", "t1mps0expseq", "commat1mps0expseq",
  "gtlt_t1mps0expseqseq", "impqi0de", "s0rtdef", "ands0rtdefseq",
  "d0atarg", "d0atargseq", "commad0atargseq", "s0tacon", "ands0taconseq",
  "s0tacst", "ands0tacstseq", "s0tavar", "ands0tavarseq", "s0expdef",
  "ands0expdefseq", "s0aspdec", "conq0uaseq", "coni0ndopt", "cona0rgopt",
  "d0atcon", "d0atconseq", "bard0atconseq", "d0atdec", "andd0atdecseq",
  "s0expdefseqopt", "e0xndec", "ande0xndecseq", "p0arg", "p0argseq",
  "commap0argseq", "d0arg", "d0argseq", "extnamopt", "d0cstdec",
  "andd0cstdecseq", "s0vararg", "s0exparg", "s0elop", "witht0ype", "p0at",
  "atmp0at", "argp0at", "argp0atseq", "p0atseq", "commap0atseq",
  "labp0atseq", "commalabp0atseq", "f0arg1", "f0arg1seq", "f0arg2",
  "f0arg2seq", "d0exp", "atmd0exp", "s0expdarg", "s0expdargseq",
  "argd0exp", "argd0expseq", "d0arrind", "colons0expopt", "funarrow",
  "caseinv", "ifhead", "sifhead", "casehead", "scasehead", "forhead",
  "whilehead", "tryhead", "d0expcommaseq", "commad0expseq",
  "d0expsemiseq0", "d0expsemiseq1", "labd0expseq", "commalabd0expseq",
  "m0atch", "m0atchseq", "andm0atchseq", "guap0at", "c0lau", "c0lauseq",
  "barc0lauseq", "sc0lau", "sc0lauseq", "barsc0lauseq", "i0nvqua",
  "i0nvmet", "i0nvarg", "i0nvargseq", "commai0nvargseq", "i0nvargstate",
  "i0nvresqua", "i0nvresstate", "loopi0nv", "initestpost", "m0arg",
  "m0argseq", "commam0argseq", "m0acarg", "m0acargseq", "m0acdef",
  "andm0acdefseq", "v0aldec", "andv0aldecseq", "f0undec", "andf0undecseq",
  "v0arwth", "v0ardec", "andv0ardecseq", "i0mpdec", "d0ec", "d0ecarg",
  "d0ecargseq", "semicolonseq", "d0ec_sta", "guad0ec_sta", "d0ecseq_sta",
  "d0ecseq_sta_rev", "d0ec_dyn", "guad0ec_dyn", "d0ecseq_dyn",
  "d0ecseq_dyn_rev", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   246,   247,   247,   247,   247,   247,   247,   247,   247,
     248,   248,   248,   248,   248,   248,   249,   249,   249,   249,
     249,   249,   250,   250,   250,   250,   251,   251,   251,   251,
     251,   252,   252,   252,   252,   253,   253,   253,   253,   253,
     253,   254,   254,   254,   254,   255,   255,   256,   256,   256,
     257,   257,   257,   258,   258,   259,   259,   259,   259,   259,
     259,   259,   259,   259,   259,   259,   259,   260,   261,   261,
     262,   262,   262,   263,   263,   263,   264,   265,   265,   265,
     265,   266,   266,   267,   267,   267,   267,   267,   267,   267,
     267,   267,   268,   268,   269,   269,   270,   270,   271,   272,
     272,   272,   272,   272,   273,   273,   274,   274,   275,   275,
     275,   276,   276,   277,   277,   278,   278,   278,   278,   278,
     278,   278,   279,   279,   279,   279,   280,   280,   281,   281,
     282,   282,   282,   282,   282,   282,   282,   282,   282,   282,
     282,   282,   282,   283,   283,   284,   284,   285,   285,   286,
     287,   287,   288,   288,   288,   289,   289,   289,   289,   289,
     290,   290,   290,   290,   290,   290,   290,   290,   290,   290,
     291,   291,   292,   292,   293,   293,   293,   293,   293,   293,
     293,   293,   293,   294,   294,   295,   295,   296,   296,   297,
     298,   298,   299,   300,   300,   301,   301,   302,   303,   303,
     304,   304,   305,   305,   305,   306,   306,   307,   307,   308,
     308,   309,   310,   310,   310,   310,   311,   311,   311,   311,
     311,   311,   311,   311,   311,   311,   311,   311,   311,   311,
     311,   311,   311,   311,   311,   311,   311,   311,   311,   311,
     311,   311,   311,   312,   312,   313,   313,   314,   314,   314,
     315,   315,   316,   316,   317,   317,   318,   318,   318,   319,
     319,   320,   320,   321,   321,   321,   322,   322,   323,   324,
     324,   325,   325,   326,   326,   327,   328,   328,   329,   329,
     330,   330,   331,   331,   332,   333,   333,   334,   334,   335,
     335,   336,   336,   337,   337,   337,   337,   338,   338,   339,
     339,   340,   340,   341,   342,   342,   343,   344,   344,   345,
     346,   346,   347,   347,   348,   348,   349,   350,   350,   351,
     351,   352,   352,   353,   353,   354,   354,   355,   356,   356,
     357,   357,   358,   358,   359,   359,   360,   360,   360,   360,
     361,   361,   362,   362,   363,   364,   364,   365,   365,   365,
     366,   366,   366,   367,   367,   368,   368,   368,   368,   368,
     369,   369,   369,   369,   369,   370,   370,   370,   370,   370,
     370,   370,   370,   370,   370,   370,   370,   370,   370,   370,
     370,   370,   370,   371,   371,   372,   372,   373,   373,   374,
     374,   375,   375,   376,   376,   376,   377,   377,   377,   377,
     378,   378,   379,   379,   380,   380,   381,   381,   381,   381,
     381,   381,   381,   381,   381,   381,   381,   381,   381,   381,
     382,   382,   382,   382,   382,   382,   382,   382,   382,   382,
     382,   382,   382,   382,   382,   382,   382,   382,   382,   382,
     382,   382,   382,   382,   382,   382,   382,   382,   382,   382,
     382,   382,   382,   382,   382,   382,   382,   382,   382,   382,
     382,   382,   382,   382,   382,   382,   382,   382,   382,   382,
     382,   382,   382,   382,   382,   382,   382,   382,   383,   384,
     384,   385,   385,   386,   386,   387,   387,   388,   388,   389,
     389,   389,   390,   390,   391,   392,   393,   393,   393,   394,
     395,   395,   396,   397,   398,   398,   399,   399,   400,   400,
     400,   401,   402,   402,   403,   403,   404,   404,   405,   406,
     406,   407,   407,   408,   408,   408,   408,   409,   409,   410,
     410,   411,   412,   412,   413,   413,   414,   414,   415,   415,
     415,   416,   416,   417,   417,   418,   418,   419,   420,   420,
     421,   421,   422,   423,   424,   425,   425,   426,   426,   427,
     427,   428,   428,   429,   430,   430,   431,   432,   432,   433,
     433,   434,   434,   435,   435,   436,   436,   436,   436,   437,
     437,   438,   439,   439,   439,   439,   439,   439,   439,   439,
     439,   439,   439,   439,   439,   439,   439,   439,   439,   439,
     439,   439,   439,   439,   439,   439,   439,   439,   439,   439,
     439,   439,   439,   439,   440,   441,   441,   442,   442,   443,
     443,   443,   443,   443,   443,   444,   444,   444,   445,   446,
     446,   447,   447,   447,   447,   447,   447,   447,   447,   447,
     447,   447,   447,   447,   447,   447,   448,   448,   448,   449,
     450,   450
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     3,     3,     3,     3,     3,     3,     3,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     2,
       1,     1,     1,     1,     1,     3,     1,     0,     1,     3,
       5,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     0,     2,     0,     3,     0,     1,     1,     2,
       2,     1,     1,     1,     0,     2,     0,     3,     1,     1,
       3,     2,     1,     2,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     3,     3,     0,     2,     0,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     2,     0,     3,     3,
       0,     3,     2,     2,     3,     2,     2,     3,     3,     4,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     0,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     1,     2,     1,
       1,     2,     1,     1,     2,     0,     2,     2,     0,     2,
       0,     3,     0,     2,     4,     0,     2,     0,     3,     0,
       4,     4,     1,     1,     3,     5,     1,     1,     1,     1,
       2,     2,     3,     5,     3,     3,     4,     4,     5,     5,
       6,     6,     3,     3,     4,     4,     6,     5,     3,     1,
       3,     3,     3,     2,     1,     2,     2,     0,     3,     3,
       2,     4,     1,     4,     0,     2,     0,     3,     3,     1,
       8,     0,     1,     0,     3,     3,     0,     3,     2,     0,
       4,     0,     5,     1,     2,     1,     0,     2,     0,     3,
       0,     3,     1,     4,     3,     0,     3,     1,     3,     0,
       2,     0,     3,     1,     4,     3,     6,     0,     3,     3,
       6,     0,     3,     3,     0,     3,     5,     0,     3,     5,
       0,     4,     0,     3,     0,     2,     4,     1,     2,     0,
       3,     3,     6,     0,     3,     0,     3,     3,     0,     3,
       1,     3,     0,     2,     0,     3,     1,     3,     5,     3,
       0,     2,     0,     2,     5,     0,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     2,     2,     2,     2,
       2,     3,     3,     4,     2,     1,     1,     1,     1,     1,
       2,     2,     2,     3,     5,     3,     3,     3,     5,     5,
       3,     3,     3,     1,     3,     0,     2,     0,     2,     0,
       3,     1,     4,     0,     2,     5,     3,     1,     3,     1,
       0,     2,     3,     1,     0,     2,     2,     3,     4,     6,
       6,     4,     4,     5,     6,     3,     3,     2,     4,     5,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     1,     1,     1,     1,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     6,     9,     5,
       2,     2,     3,     4,     5,     3,     5,     5,     5,     3,
       3,     3,     3,     3,     4,     4,     5,     5,     3,     3,
       4,     4,     6,     3,     3,     3,     5,     3,     3,     0,
       2,     1,     1,     0,     2,     2,     4,     0,     2,     1,
       1,     3,     0,     2,     2,     2,     2,     2,     2,     2,
       1,     3,     3,     1,     0,     2,     0,     3,     0,     1,
       3,     3,     0,     4,     0,     5,     1,     3,     2,     0,
       3,     1,     3,     3,     3,     3,     3,     1,     2,     0,
       3,     3,     1,     2,     0,     3,     0,     3,     0,     3,
       1,     2,     3,     0,     2,     0,     3,     3,     0,     3,
       0,     5,     4,     7,     1,     0,     2,     0,     3,     1,
       3,     0,     2,     4,     0,     3,     4,     0,     3,     5,
       7,     0,     3,     0,     2,     4,     5,     4,     6,     0,
       3,     5,     3,     3,     3,     3,     3,     2,     2,     2,
       3,     2,     2,     2,     3,     3,     3,     3,     3,     3,
       3,     2,     4,     3,     2,     4,     4,     5,     3,     4,
       3,     4,     2,     4,     3,     0,     2,     0,     2,     1,
       4,     1,     2,     2,     5,     4,     6,     5,     1,     0,
       3,     1,     5,     5,     5,     3,     4,     4,     4,     3,
       3,     5,     1,     2,     2,     2,     4,     6,     5,     1,
       0,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     0,     0,     0,     0,     0,     0,   629,   650,     0,
      55,    56,    57,    58,    59,    60,    63,    61,    66,    62,
      64,    65,     0,   115,   116,   117,   118,   119,   120,   121,
       0,   160,   161,   162,   163,   164,   165,   167,   166,   169,
     168,     0,   174,   175,   176,   177,   178,   181,   179,   182,
     180,     0,   216,   217,   218,    67,   202,     0,     0,   104,
     239,     0,     0,     0,     0,     0,     0,   261,   254,   254,
     261,     0,   269,   261,   269,   254,     0,     0,   219,     0,
     244,   212,   213,   420,   421,   422,   423,   424,   425,   189,
     192,    70,    42,    44,    46,   508,   433,   492,   492,   492,
     434,    71,    45,   500,   536,   492,    41,   650,    43,     0,
     492,   492,   503,    72,   536,   432,   353,     0,   354,   508,
       0,     0,   247,   247,   247,     0,     0,     0,     0,   440,
     441,   442,   443,   444,   445,   446,   438,   439,     0,     0,
     426,   427,   479,   479,   437,   504,   650,   504,     0,   512,
     504,   504,   512,     0,   400,     0,     0,   431,     0,   428,
     190,   504,   193,   276,     0,     0,   483,     0,     0,     0,
       0,     0,     0,   508,     0,   628,     0,   649,     1,     4,
       5,     6,     7,   198,   202,   195,   220,     0,   103,    98,
     102,     0,     0,   101,   106,     0,   269,   269,   261,   261,
     245,     0,   266,     0,   262,   219,   252,   256,     0,     0,
       0,     0,    74,     0,    73,     0,     0,     0,     0,     0,
     153,   152,   221,     8,     0,   243,   246,   509,     0,   548,
     496,     0,   497,   498,   254,   538,     0,   494,     0,   429,
     499,   495,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   512,   512,   504,   504,     0,   417,     0,   479,
     435,   436,   506,     0,     0,     0,   506,     0,     0,     0,
       0,     0,     0,     0,   350,   351,   352,     0,   365,   367,
     366,   368,   185,   186,     0,     0,   261,   399,   387,   198,
     254,   387,     0,   387,   387,     0,     0,   369,   397,   400,
     487,   400,   156,   155,     0,   430,   191,   194,   450,     0,
     273,   275,   278,   280,   504,   451,     9,     0,     0,   650,
     481,   482,   483,   406,     0,     0,     0,     0,   504,     0,
       0,     0,     2,   621,    10,    11,    12,    13,    14,    15,
       0,    18,     0,     0,     0,    22,    23,    24,    25,   310,
      16,    77,    77,    77,   629,     0,     0,    68,     0,    77,
      19,    77,    20,    27,    21,     0,     0,    26,     0,     0,
      68,    28,    17,    29,    30,     0,     0,     0,    47,    48,
      49,     0,     0,     0,     0,   615,     0,     0,     0,   619,
     617,     3,   642,    38,     0,     0,    35,    36,    37,   209,
     650,    39,    40,    34,    31,    32,    33,     0,     0,     0,
     615,     0,   631,   617,   195,   200,     0,   203,     0,     0,
     154,    99,   100,     0,   105,   238,     0,     0,     0,     0,
       0,     0,   268,   261,   222,     0,     0,     0,     0,   255,
     241,   240,   261,   224,     0,     0,     0,   232,   261,   225,
     233,   242,     0,   126,   126,     0,   214,     0,   122,   112,
     508,   460,   254,     0,   493,     0,   261,   540,     0,   501,
     508,   502,   158,     0,   475,   474,   473,     0,     0,   504,
     504,   504,     0,     0,     0,     0,     0,     0,   480,     0,
     508,   505,   504,   455,   461,   477,   504,   462,     0,     0,
     468,   504,   463,   459,   469,     0,   371,   370,     0,     0,
       0,   369,   389,   385,     0,     0,     0,     0,   391,     0,
       0,     0,     0,     0,   372,   401,     0,     0,   487,   157,
     485,   274,     0,   277,   276,     0,   452,   650,   407,   484,
       0,     0,   529,   534,     0,   415,   416,   529,     0,   170,
     202,   601,   604,     0,   150,   150,   254,     0,   328,    78,
       0,    68,    68,    68,     0,     0,   561,   564,     0,   564,
      68,   587,     0,     0,    68,    68,     0,   285,     0,   301,
     612,    76,     0,     0,   304,   588,    83,    84,    85,    86,
       0,    87,    88,    92,    89,   591,    82,    96,   592,   623,
     593,   589,   293,   297,   254,   615,     0,     0,   323,   202,
     307,    53,   622,   630,   645,     0,    17,   615,   205,     0,
       0,     0,     0,     0,   573,   579,   644,     0,   567,     0,
      53,   643,   651,   197,     0,   199,   202,   196,     0,   106,
     234,   235,   261,   226,   261,   227,   269,   266,     0,   172,
       0,   256,   256,     0,   261,    75,   271,     0,     0,   128,
       0,     0,   113,   111,   123,   510,     0,   543,   537,     0,
     543,   550,     0,   159,   249,   248,     0,     0,     0,   470,
     471,   464,   465,     0,   478,   506,   511,     0,     0,   504,
       0,   514,     0,     0,   398,   370,   364,     0,     0,     0,
     388,   198,   383,   385,   360,   387,   373,   382,   396,   387,
     376,     0,   380,   387,   377,   375,   381,   488,   104,   489,
     490,     0,     0,   504,   278,   280,   453,     0,   408,     0,
       0,   521,     0,   529,   411,   527,     0,     0,     0,   534,
     412,   532,   504,   418,   171,   195,     0,   147,     0,   595,
     596,     0,   314,   310,   603,     0,   582,   583,   584,   629,
     564,   555,   554,   559,   561,     0,     0,   608,   564,   610,
      69,     0,     0,   586,   585,     0,     0,   594,     0,   289,
       0,   598,     0,     0,     0,   599,     0,    94,     0,    81,
      97,   590,     0,   289,     0,   597,     0,   616,   340,   345,
     319,   289,     0,   325,   195,     0,   600,    54,   629,   618,
       0,     0,     0,   207,     0,     0,   183,   282,   276,   404,
     640,   650,   567,   567,   573,     0,     0,     0,     0,   639,
       0,     0,   635,     0,   187,   400,   571,   650,   200,   204,
     215,   107,     0,     0,     0,   267,   223,   173,     0,   259,
     253,   257,   258,   228,   237,     0,     0,   270,   229,   114,
       0,   127,   124,   125,   549,     0,   545,     0,   539,     0,
     552,   476,   449,   457,   458,     0,   507,   456,   466,     0,
       0,     0,   513,   467,   454,     0,   362,   389,   361,   347,
     348,   349,     0,   386,     0,     0,   393,     0,     0,   413,
       0,   486,   279,   281,   419,     0,     0,   529,     0,     0,
       0,     0,     0,   528,   534,   198,     0,   533,     0,     0,
     605,     0,   143,   147,   149,   145,   150,   310,     0,   327,
     328,     0,    79,     0,   609,   557,     0,   562,     0,   564,
     611,     0,     0,   606,   284,   285,   299,   115,   116,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   119,   120,   121,     0,   130,   287,   291,     0,   301,
     613,   303,   304,    91,     0,    93,    90,   295,     0,   297,
     614,   332,   254,   336,   340,     0,     0,   620,   310,     0,
     319,   321,   317,     0,   323,     0,   602,     0,   307,     0,
       0,     0,   345,     0,   206,   209,   184,   280,   198,   403,
     404,   487,     0,   636,   637,     0,   574,   573,     0,   579,
     355,   567,   188,     0,     0,   638,     0,   201,   230,   231,
     236,     0,   250,     0,   128,   541,     0,   544,   551,   547,
     472,   447,     0,     0,   363,   390,   384,   374,   378,     0,
     392,   379,   491,   414,   409,   410,   530,   516,   519,   522,
     523,   524,   525,   526,   535,     0,   531,   504,     0,   147,
       0,   146,   151,   311,   315,   329,     0,   624,     0,   556,
     560,   563,   565,   607,   286,     0,     0,   290,     0,   302,
     305,    94,   294,   298,   330,   334,     0,     0,   341,   108,
     104,   109,     0,   345,   319,   312,   318,     0,   324,   307,
       0,   308,   629,    50,    51,    52,   625,     0,   633,   634,
     632,   207,   210,     0,     0,   405,     0,   641,     0,   577,
     575,   580,     0,     0,     0,     0,   566,   568,     0,     0,
     571,   650,   646,     0,     0,   261,     0,   129,   542,   545,
     504,     0,   394,     0,     0,     0,   518,   211,     0,   309,
     148,   144,    80,   557,   288,   291,     0,    95,     0,     0,
       0,   333,   332,   337,   339,     0,   342,   346,   320,   261,
     314,   319,   326,   306,     0,   627,   208,   283,   402,     0,
     576,     0,   356,   357,   358,   359,   355,     0,   572,     0,
     648,     0,   251,   271,   546,     0,   514,     0,   517,   519,
     553,   558,   292,   300,   296,   331,   334,     0,   110,     0,
     344,     0,   316,   322,   626,   581,   578,   569,     0,   647,
       0,   272,   448,   515,   393,   520,   335,   338,   343,   313,
     355,   263,   395,   570,     0,     0,     0,   263,   263,   260,
     264,   265
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     9,   384,   385,   386,   387,   409,   410,   154,   155,
     388,  1117,   808,   594,   156,   571,   157,   215,   582,   561,
     611,   596,   788,   975,   791,   193,   194,   195,   424,  1102,
     965,   457,   458,   459,   660,   861,   966,   923,   924,   925,
     554,   749,    77,   158,    78,   737,   436,   159,   817,   511,
     835,   160,   161,   162,   163,   419,   415,   416,   635,   185,
     814,  1004,   619,   738,   202,    80,    81,    82,   249,   854,
     207,   208,   439,   850,   855,  1246,   432,   204,   216,   857,
     311,   312,   313,   533,   535,   819,   577,   777,   967,   968,
    1087,   603,   795,   579,   781,   584,   785,   610,   806,   551,
     989,  1180,   929,   990,   991,   992,   608,   803,   996,   558,
     754,  1095,  1096,  1171,   984,   985,  1220,   799,   987,   892,
     487,   164,  1136,   512,   513,   703,   704,   514,   700,   520,
    1050,   299,   300,  1010,  1011,   266,   166,   259,   260,   322,
     323,   308,   527,   721,   230,   167,   168,   169,   170,   171,
     172,   173,   309,   491,   228,   264,   270,   882,  1058,  1059,
    1156,   732,   733,   734,   735,   739,   740,   741,   235,   468,
     866,   867,  1037,   671,   463,   231,   236,   329,   763,   936,
    1079,   764,   765,   567,   767,   628,   832,   836,  1025,   827,
     625,   829,   820,   389,   605,   606,   613,   390,   612,   174,
     175,   413,   631,   265,   177
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -1085
static const yytype_int16 yypact[] =
{
    1015,  1722,   204,  1860,  1669,  1601,  1884, -1085, -1085,   210,
   -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085,
   -1085, -1085,   263, -1085, -1085, -1085, -1085, -1085, -1085, -1085,
     278, -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085,
   -1085,   306, -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085,
   -1085,   316, -1085, -1085, -1085, -1085,   248,  1860,   326,    71,
   -1085,   113,   143,   176,   226,   385,   396,  1601,  2386,  2386,
    1601,  1601,  2769,  1601,  2769,  2386,   324,  1860, -1085,    86,
   -1085,  2386,  2386, -1085, -1085, -1085, -1085, -1085, -1085, -1085,
   -1085, -1085, -1085, -1085, -1085,  1884, -1085,    18,    18,    18,
   -1085, -1085, -1085, -1085,   228,    18, -1085, -1085, -1085,  1669,
      18,    18, -1085, -1085,   228, -1085, -1085,   406, -1085,  1884,
    1884,  1884,    80,    80,    80,   232,   237,   266,   277, -1085,
   -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085,   291,  1884,
   -1085, -1085,   300,   300, -1085,  1884, -1085,  1884,  1601,  2769,
    1884,  1884,  2769,  1429,   602,  1669,   149, -1085,  1548, -1085,
   -1085,  1884, -1085,  2492,  1204,    87,  2100,  1884,  1601,  1884,
    1601,   318,  2314,  1884,   509,  3109,   517,  2924, -1085, -1085,
   -1085, -1085, -1085,  1860,   248,   391, -1085,   398, -1085, -1085,
   -1085,   518,   518, -1085,   417,   413,  2769,  2769,  1601,  1601,
   -1085,   485,    38,   -29, -1085,   260,  2386,    35,   348,   356,
      84,   102, -1085,  2769, -1085,   435,   362,   147,   383,   366,
   -1085, -1085, -1085, -1085,  1208, -1085, -1085,   313,   535,   389,
   -1085,   457, -1085, -1085,  2386,   314,   466, -1085,   552, -1085,
   -1085, -1085,   474,    68,   415,   -19,    -2,  1601,  1601,   420,
     421,   424,  2769,  2769,  1884,  1884,  1601,    41,  1429,   300,
   -1085, -1085,   360,   157,   425,   422,    15,   158,   172,   500,
     428,   160,   431,   436, -1085, -1085, -1085,   513, -1085, -1085,
   -1085, -1085, -1085, -1085,   526,   526,  1601, -1085,  1178,  1860,
    2386,  1178,  1993,  1178,  1178,  1993,   526, -1085, -1085,   602,
     512,   602, -1085, -1085,   520, -1085, -1085, -1085, -1085,   440,
   -1085,  2492,   522,   508,  1884, -1085, -1085,   447,  1601,  1429,
   -1085, -1085,  2100, -1085,    22,    16,    39,    40,  1884,  1884,
    1884,   538, -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085,
    1358, -1085,  1860,  1722,  1722, -1085, -1085, -1085, -1085,   449,
   -1085,    51,    51,    51, -1085,   655,  1270,  1722,   334,    51,
   -1085,    51, -1085, -1085, -1085,   204,  1860, -1085,   368,  1860,
    1722, -1085, -1085, -1085, -1085,  2598,  1722,  2598, -1085, -1085,
   -1085,   651,  2598,  1722,  1860,   453,  1860,  1860,  2598, -1085,
   -1085, -1085, -1085, -1085,   658,   411, -1085, -1085, -1085,   460,
   -1085, -1085, -1085, -1085,   219, -1085, -1085,    95,   661,  1178,
     453,  2598, -1085, -1085,   391,   542,   472, -1085,  1208,   532,
   -1085, -1085, -1085,    71, -1085, -1085,   481,   482,   178,   181,
     486,  1601, -1085,  1601, -1085,  1860,   564,  2386,  2386, -1085,
   -1085, -1085,  1601, -1085,   490,   492,  1601, -1085,  1601, -1085,
   -1085, -1085,   691,  1208,  1208,   570,  1208,   204, -1085, -1085,
    1884, -1085,  2386,   503, -1085,   499,  1601, -1085,   505, -1085,
    1884, -1085, -1085,   582, -1085, -1085, -1085,   184,   -34,  1884,
    1884,  1884,   506,   507,   514,   515,   414,   516, -1085,  1884,
    1884, -1085,  1884, -1085, -1085, -1085,  1884, -1085,   301,  1884,
   -1085,  1884, -1085, -1085, -1085,  1884, -1085, -1085,   569,   526,
    1178,   689,   432,   696,   189,   511,   521,   193, -1085,   591,
     525,   197,   519,   533, -1085, -1085,  1601,   384,   512, -1085,
     529, -1085,  2386, -1085,  2492,   590, -1085, -1085,   600, -1085,
    1884,  1884,   852,  2051,   611,    41,    41,   852,  1860, -1085,
     248, -1085,   603,   613,   726,   726,  2386,  1669,   730, -1085,
    1722,  1722,  1722,  1722,   697,  1669,    52,   731,  1669,   731,
    1722, -1085,   548,   638,  1722,  1722,   626,   737,   124,   740,
   -1085, -1085,   634,   643,   745, -1085, -1085, -1085, -1085, -1085,
    2598, -1085, -1085,  2598, -1085,  2598, -1085,  2598,  2598, -1085,
    2598, -1085,   -33,   751,  2386,   453,  1669,   -31,   752,   248,
     762,  2234, -1085,   663, -1085,   777,   782,   453,  1860,  1410,
     742,  1178,  1178,   526,    21,   772, -1085,   218,   775,  1531,
    2234, -1085,   663, -1085,  1860, -1085,   248,  1208,  1601,   417,
   -1085, -1085,  1601, -1085,  1601, -1085,  2769,    38,   595,   674,
     830,    35,    35,   604,  1601, -1085,   434,   608,   676,   523,
     614,   618, -1085, -1085, -1085, -1085,   617,  1669, -1085,   670,
    1669,   698,   779, -1085, -1085, -1085,   621,   623,   624, -1085,
   -1085, -1085, -1085,   824, -1085,    15, -1085,   630,   631,  1884,
    1884,   364,   632,   -12, -1085,   815,   712,  1178,  1178,  1601,
   -1085,  1826, -1085,   696, -1085,  1178, -1085, -1085, -1085,  1178,
   -1085,  1178, -1085,  1178, -1085, -1085, -1085,   600,    71, -1085,
   -1085,  1884,   384,  1884,   522,   508, -1085,   633,   216,   246,
    1178,     7,   268,   720, -1085, -1085,  1358,   644,   708,   723,
   -1085, -1085,  1884, -1085, -1085,   391,  1601,  1300,  1722, -1085,
   -1085,   646,   784,   449, -1085,    49, -1085, -1085, -1085, -1085,
     731,   526, -1085, -1085,    52,   721,  1669, -1085,   731, -1085,
   -1085,   739,   468, -1085, -1085,   830,   204, -1085,  1208,  2661,
    1860, -1085,   851,  1208,  1860, -1085,  1700,  2560,   659, -1085,
    2598, -1085,  1601,  2661,  1860, -1085,   656, -1085,    62,   845,
      55,  2661,  1860,   753,   391,  1860, -1085, -1085, -1085, -1085,
     738,   741,  1669,   743,   666,  2015, -1085, -1085,  2492,  1251,
   -1085, -1085,   775,   775,   754,   526,  1601,   744,    95, -1085,
    1884,  1178, -1085,  1669, -1085,   602,   847, -1085,   542, -1085,
     600, -1085,   673,   677,   679, -1085, -1085, -1085,  1860,  1208,
   -1085, -1085, -1085, -1085, -1085,   682,  2769, -1085, -1085, -1085,
    1208, -1085, -1085, -1085, -1085,   757,   755,   685, -1085,   686,
   -1085, -1085, -1085, -1085, -1085,   687, -1085, -1085, -1085,   688,
       5,  2769, -1085, -1085, -1085,  1178,   712,   432,   600, -1085,
   -1085, -1085,   690, -1085,   692,   693,   451,   700,   761,    41,
    1884, -1085, -1085, -1085, -1085,  1884,  1884,   720,  1884,  1884,
    1884,  1884,  1884, -1085,   723,  1860,  1884, -1085,   766,   769,
     600,  1722,   835,   780, -1085, -1085,   726,   449,  1601, -1085,
     730,   908, -1085,   870, -1085,   789,   713, -1085,  1884,   731,
   -1085,   468,  1669, -1085, -1085,   737,  1208,   790,   793, -1085,
   -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085, -1085,
   -1085,   794,   796,   798,   800,  1208, -1085,   802,   728,   740,
   -1085,  1208,   745, -1085,  2598, -1085, -1085,   600,   732,   751,
   -1085,   526,  2386, -1085,    62,   206,  1669, -1085,   449,  1669,
     809, -1085, -1085,   733,   752,  1860, -1085,   807,   762,   491,
    1601,  1884,   845,  1860, -1085,   460, -1085,   508,  1860, -1085,
    1251,   512,   894, -1085, -1085,   811, -1085,    31,  1884,   772,
     394,   775, -1085,   344,  1531, -1085,   497, -1085, -1085, -1085,
   -1085,   816,   746,   812,   523,  1601,  1669, -1085, -1085, -1085,
   -1085, -1085,   749,   822,   712, -1085, -1085, -1085, -1085,  2154,
   -1085, -1085, -1085,    41,    41,    41, -1085,   154,   925, -1085,
      41,    41,    41,    41, -1085,   759,    41,  1884,  1601,   780,
    1208, -1085, -1085, -1085,   600, -1085,   763, -1085,   526, -1085,
   -1085,    41, -1085, -1085, -1085,  2727,  2661, -1085,   819, -1085,
   -1085,  2560,   825, -1085,   829,   831,   200,   760, -1085, -1085,
      71, -1085,  1601,   845,   809,   767, -1085,   834, -1085,   762,
    1601, -1085, -1085, -1085, -1085, -1085, -1085,  2598,   600,    41,
   -1085,   743, -1085,   838,   764, -1085,   844, -1085,  1884,   856,
      41, -1085,  1601,  1601,  1601,  1601, -1085, -1085,  1884,  1601,
     847, -1085, -1085,  2598,   830,  1601,  1601, -1085,   600,   755,
    1884,  1884, -1085,   858,  1178,  1884, -1085, -1085,   778,   600,
   -1085,  1208, -1085,   789, -1085,   802,  1208, -1085,  1601,  1601,
     526, -1085,   526, -1085, -1085,   843,   367, -1085, -1085,  1601,
     784,    55, -1085,   600,   792, -1085, -1085, -1085, -1085,  1884,
      41,  1884,   600,   600,   600,   600,   394,   372, -1085,   799,
   -1085,   865, -1085,   434, -1085,   788,   364,  1178,   712,   925,
   -1085, -1085, -1085,  1208,   600,   600,   831,   791, -1085,   986,
   -1085,   795, -1085, -1085, -1085,    41,    41, -1085,  1884, -1085,
    1601, -1085, -1085, -1085,   451, -1085, -1085, -1085, -1085, -1085,
     394,   319, -1085, -1085,  1601,  1601,   797,   319,   319, -1085,
   -1085, -1085
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1085, -1085, -1085,   612, -1085, -1085, -1085, -1085, -1085, -1085,
     832,   -18,   381,   410,    -5,  -254, -1085,  -124, -1085,   -10,
    -338,  -504, -1085,   -79, -1085,   357,   592,  -680,   387,     4,
    -214, -1085,     9,  -405,   574,    -3,   -53,   112, -1085,  -835,
    -317,  -520,  -306,   596,  1174,   694,   386,   224,  -714,   660,
   -1085,   879, -1085,  -138,   419,  -365,  -586,  -259,   202,  -162,
   -1085,   -80,    37, -1085,    14,   -39,   -60, -1085,   470,  -102,
     163,   -46,   -56,  -739,   -66,  -620,   399,   -50,   -58,  -158,
   -1085,   527,  -489,   323,  -671, -1085,   275,   107,   -32,  -659,
    -112,   261,    79,   280,    92,   285,    90,  -748,  -903, -1085,
    -343, -1085,  -116,    82,  -110,  -890,   271,    85, -1085,   325,
     155,   -86,   -85,  -130, -1085,   106, -1085,  -729,  -900, -1085,
     943, -1085, -1084,  -406,  -149, -1085,   395,  -241,   213,   806,
    -132, -1085,  -268, -1085,    93,   183,   -73,  -105,   -59, -1085,
     785,  -288,  -507,   382,   487, -1085, -1085, -1085, -1085, -1085,
   -1085, -1085,  -133,   423,   -63, -1085,    -7,   -96,   -44, -1085,
     -97, -1085,   388,   567,  -647,   379, -1085,  -679, -1085, -1085,
      81,   450,   -30, -1085, -1085,   452,  1008, -1085,  -720, -1085,
     -38, -1085,   369,  -332,  -510,  -575,  -744,   108,   -14,  -769,
     303,   117, -1085,   960, -1085,  -330,   729, -1085,    27,  -341,
   -1085, -1085,     2,    -6, -1085
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -551
static const yytype_int16 yytable[] =
{
      76,   203,   176,   627,   210,   298,   557,   217,   206,   206,
     456,    30,   263,   564,   267,   206,   218,   271,   272,    79,
     307,   722,   417,   209,   569,   269,   536,   555,   269,   219,
     515,   525,   813,   528,   548,   750,   944,   595,   898,   598,
     315,   935,   225,   226,   600,   725,   822,   823,   838,   633,
     517,   663,   521,   522,   903,  1015,   244,   998,   943,   769,
     917,   321,    76,    76,    76,    76,    76,   559,    76,   931,
      76,   282,   283,   630,  -310,  -310,    76,    76,  1013,  1014,
     629,   282,   283,  1002,   261,   211,   913,   188,  1071,   445,
     189,   789,    55,   320,   789,  1111,   789,   223,   316,   330,
    1106,   238,  1120,   276,   696,   224,   317,   789,   433,   792,
     331,   800,  1227,   317,   282,   283,   585,   542,   543,   541,
     318,   484,   485,   317,   310,   540,   789,   318,   269,   269,
     317,   908,   428,   429,   978,   190,   731,   318,   426,   427,
     317,   731,   993,    76,   318,   273,   699,   317,    76,   825,
     298,   304,   298,   489,   318,   224,  1243,   229,    76,   825,
     826,   318,   268,    76,   317,    76,   317,   225,   519,  -550,
     224,   519,   437,    55,   206,   438,   431,   224,   318,   224,
     318,   793,   325,   801,   327,   675,   434,  1154,   465,   165,
    -310,  -310,   988,    76,    76,   544,   475,  -310,  -310,  -310,
     488,    76,  -310,  1177,   637,   884,  1182,   191,   276,   472,
     178,  -310,   317,   476,  1178,   627,   627,   321,   192,   455,
     508,   442,  1042,    23,    24,   224,   318,  1083,    51,    76,
     206,   623,   663,   760,  1160,  1064,   768,   548,   473,   659,
     659,   224,    76,    76,   516,   482,   483,  1109,  1129,   320,
     934,    76,   786,    76,   663,   787,  1021,  1103,   940,   790,
    1056,   477,   478,   778,   932,   560,   761,    31,    32,   276,
     486,   905,   531,   556,   179,   797,   981,  1137,   227,   317,
     982,    76,   789,   789,   448,    76,   789,   812,   302,   180,
     303,   886,   887,   318,   492,   496,   247,   501,   248,   443,
     621,   906,   227,   245,   246,   896,    76,   756,   757,   758,
     622,   224,    25,    76,    76,   642,   770,   181,   644,   444,
     773,   774,   257,   224,   731,    26,   705,   182,   262,  1007,
     709,   196,   538,   239,   713,    76,  1123,  1172,   779,    27,
      33,   317,   562,   563,   187,  1099,   676,   677,   678,   574,
     324,   575,   326,    42,    43,   318,   227,   699,  1163,   687,
     830,   197,   449,   688,   702,    28,    29,   648,   692,  1100,
    1101,   317,   493,   497,   576,   502,   653,   206,   206,   301,
     919,    34,   657,    35,    36,   318,   580,   581,   745,   498,
     198,    37,    38,   643,   620,    39,   645,   665,   435,  -172,
     669,   674,   206,   200,   706,  1201,  1072,   672,   710,    40,
     557,    22,   714,   455,   201,  1173,   666,  1121,   933,   909,
    1175,   910,   911,   912,   243,   627,    76,   686,    76,  1082,
     548,   926,    76,    76,   939,   901,   849,    76,   317,   997,
     199,    76,   891,    76,   663,   647,   234,   804,   455,   455,
     252,   455,   318,   460,   341,   253,  1244,    76,   224,  1245,
     656,    76,   183,   220,   894,   221,   664,   999,   895,    44,
      45,   466,   897,   467,   839,   350,    46,    47,    48,  1044,
     254,    49,   214,  1099,   214,   317,  1138,    42,    43,   317,
      50,   255,    55,   310,   360,   310,   206,   362,   489,   318,
     490,   364,   881,   318,  1126,   256,   224,  1100,  1101,  1219,
     751,   224,   545,   546,  1228,   689,   615,   690,   258,   317,
     332,    76,   616,  1132,  1133,  1134,  1135,    76,   391,    76,
     418,   727,   328,   318,   718,   719,   720,   189,    76,   420,
     717,   663,    23,    24,   206,   282,   283,    55,   421,   422,
     572,    76,   683,   224,   702,   423,   879,   425,   796,   214,
     663,   849,   214,   430,   946,   440,   663,  1023,   305,   971,
     698,   699,   856,   224,   214,   441,   842,   446,   843,   566,
     566,   447,   573,   451,  1073,   232,   233,   789,   844,  1049,
     699,   461,   237,   250,   251,   851,   852,   240,   241,    76,
     651,   652,   450,    44,    45,   462,   214,   214,   464,   918,
      46,    47,    48,   117,   278,    49,   279,   469,   280,   470,
     281,   282,   283,   214,    50,   471,    55,  1250,  1251,   663,
     474,    25,   455,    76,   479,   480,  1091,    76,   481,    76,
     494,   495,   499,   227,    26,   455,  1034,   500,   503,    76,
     505,   526,   840,   227,   455,   504,  1065,   530,    27,   529,
     532,   860,   214,   214,   534,   537,   547,   556,   452,   599,
    1009,   604,   685,   227,    42,    43,   614,   307,   618,   626,
     634,   284,   691,   638,    28,    29,   298,   636,   693,  1112,
    1113,  1114,  1115,  1116,    76,  1141,  1113,  1114,  1115,  1142,
     640,   641,   214,   650,   646,   214,   654,   655,   278,   658,
     279,   662,   280,   888,   281,   282,   283,   667,   668,   670,
      55,   673,   697,   728,   729,   679,   680,   694,   707,   681,
     682,    76,  1033,   711,   726,   684,   715,   453,   285,   224,
     708,    76,   746,   454,   712,   723,   565,   117,  1208,  1124,
     296,   742,   716,   553,   553,   747,   663,  1043,   748,   286,
     920,   287,   753,   766,   759,   771,   772,   570,   775,   776,
     455,  1184,   780,   455,   455,   284,   782,   784,   455,   310,
     570,   752,   783,   794,   802,   576,   597,    76,   455,   566,
      44,    45,   566,   601,   805,   810,   455,    46,    47,    48,
     811,  1234,    49,   809,   828,   630,   977,   831,   663,   821,
     846,    50,   435,    76,   297,  1012,   288,   859,   289,   853,
     290,    76,   291,   858,   292,   293,   294,   295,   868,   862,
     798,  1026,   285,   863,   864,   871,   872,   229,   873,   874,
    1017,   117,   875,   816,   455,   877,   878,   883,   885,    23,
      24,   699,   904,   834,    55,   455,  1161,   730,   915,   916,
     736,  1009,   928,   938,   278,   927,   279,   941,   280,   970,
     281,   282,   283,   880,   976,   980,    55,   986,   995,  1024,
    1000,  1003,   825,  1001,   296,  1005,  1018,   296,  1028,   296,
     296,   865,  1029,  1036,   865,   296,  1035,   296,  1030,  1032,
    1038,  1039,  1040,  1041,   899,  1052,  1067,  1047,  1048,  1046,
     288,  1068,   289,  1070,   701,  1051,   291,   921,   292,   293,
     294,   295,   206,    76,  1076,  1153,  1077,  1078,  1080,   -55,
     849,   284,   -56,   -58,  1158,   -64,  1097,   -65,    25,  1085,
    1086,   455,  1074,  1088,   506,   507,   988,  1092,  1107,  1110,
    1127,    26,  1213,  1128,  1146,  1144,   524,  1155,  1166,   297,
     455,   297,  1145,  1150,  1151,    27,   455,  1168,  1169,  1170,
     755,   570,   570,   570,  1157,   452,  1181,    76,  1162,  1174,
     570,  1179,  1187,  1188,   570,   570,  1189,  1218,   509,   730,
     566,    28,    29,  1210,  1224,    76,   816,   117,  1191,   510,
    1207,  1229,  1230,  1232,  1238,   296,  1237,   617,  1143,   411,
    1239,   837,  1167,  1020,  1118,   639,  1249,  1205,     1,     2,
       3,     4,     5,     6,     7,     8,   841,  1139,   661,   455,
      76,  1147,  1164,  1069,   550,   847,   798,   306,   818,  1006,
    1027,  1186,  1122,  1202,   453,  1231,   845,   902,   848,  1148,
     454,   945,  1084,  1212,  1165,   979,   214,  1022,  1093,   724,
     969,  1089,  1090,    76,  1222,   455,   288,   624,   289,   972,
    1104,  1223,   291,   994,   292,   293,   294,   295,   930,  1108,
     455,   455,  1159,  1053,  1216,  1075,  1236,  1217,  1054,  1055,
    1098,  1057,  1060,  1061,  1062,  1063,   277,    76,   893,  1066,
    1045,   523,  1242,  1125,   900,    76,   296,   539,   876,   296,
    1233,  1209,  1235,  1221,   743,   914,  1176,  1149,   907,  1204,
     869,  1081,   242,   870,  1183,  1211,  1198,    76,    76,    76,
      76,  1019,  1140,   937,    76,  1199,  1131,   412,   296,   455,
      76,    76,   632,   296,  1185,  1200,  1192,  1193,  1194,  1195,
       0,     0,     0,  1197,     0,     0,   455,   922,   553,     0,
    1203,   455,     0,    76,    76,   816,  1006,     0,     0,   695,
       0,     0,     0,   297,    76,     0,     0,    41,     0,     0,
       0,     0,  1214,  1215,  1119,     0,     0,     0,     0,   964,
     278,     0,   279,     0,   280,     0,   281,   282,   283,     0,
       0,  1130,    55,   964,     0,     0,     0,     0,   455,     0,
     798,   964,     0,  1105,     0,   815,     0,   296,   296,     0,
     212,     0,     0,    10,    11,    76,   762,    23,    24,     0,
     184,   186,    55,     0,     0,     0,     0,     0,     0,    76,
      76,     0,   205,   205,  1241,     0,     0,     0,   834,   205,
       0,   222,     0,     0,     0,     0,     0,   284,  1247,  1248,
     865,     0,     0,   278,     0,   279,   214,   280,     0,   281,
     282,   283,     0,     0,     0,    55,     0,     0,     0,     0,
       0,     0,     0,   824,     0,     0,     0,     0,     0,    42,
      43,   214,     0,   296,   296,     0,     0,     0,     0,   296,
       0,   296,     0,     0,     0,   296,     0,   296,     0,   296,
       0,  1190,     0,     0,   509,     0,    25,     0,     0,    10,
      11,  1196,     0,   117,     0,   510,   296,     0,     0,    26,
     284,   922,     0,     0,  1206,     0,     0,    12,  1057,    13,
      14,     0,     0,    27,     0,     0,    15,    16,    17,     0,
       0,    18,     0,   452,     0,     0,     0,   414,   184,     0,
      19,   568,     0,   297,     0,    20,    21,     0,   942,    28,
      29,     0,  1225,     0,  1226,     0,     0,    31,    32,     0,
       0,     0,    55,     0,     0,     0,     0,   285,     0,     0,
       0,     0,   288,     0,   289,     0,   117,     0,   291,     0,
     292,   293,   294,   295,     0,    44,    45,     0,   205,     0,
       0,  1240,    46,    47,    48,   296,     0,    49,   213,     0,
     314,   762,   453,     0,   762,     0,    50,   296,   454,    42,
      43,   296,    90,    12,    55,    13,    14,   921,     0,     0,
       0,    52,    15,    16,    17,    53,    54,    18,    31,    32,
      33,     0,     0,    55,     0,     0,    19,     0,   983,   214,
       0,    20,    21,   414,   205,   288,     0,   289,     0,  1008,
       0,   291,     0,   292,   293,   294,   295,     0,     0,   297,
       0,   296,     0,     0,     0,  1016,     0,     0,   624,     0,
       0,    34,     0,    35,    36,   297,   964,     0,     0,     0,
      56,    37,    38,    58,     0,    39,     0,     0,    57,     0,
       0,     0,     0,     0,   549,     0,   552,     0,     0,    40,
       0,    33,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   942,     0,     0,
     578,     0,     0,   583,     0,    44,    45,     0,     0,     0,
      42,    43,    46,    47,    48,   117,     0,    49,   602,     0,
     607,   609,    34,     0,    35,    36,    50,    10,    11,    89,
      90,     0,    37,    38,    58,     0,    39,   274,   275,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    59,
      40,    60,     0,     0,     0,     0,     0,     0,     0,     0,
      61,    62,    63,    64,     0,     0,   296,     0,     0,   649,
     833,   205,   205,    52,     0,    65,    66,    53,    54,     0,
      31,    32,     0,     0,     0,    55,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   205,     0,     0,     0,
       0,  1094,     0,    67,   983,    68,     0,    69,     0,    70,
      71,    72,    73,     0,    74,     0,    75,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    44,    45,     0,     0,
     297,     0,    56,    46,    47,    48,     0,     0,    49,     0,
      57,    12,     0,    13,    14,     0,     0,    50,    42,    43,
      15,    16,    17,    33,     0,    18,     0,     0,     0,     0,
       0,     0,     0,     0,    19,     0,     0,     0,     0,    20,
      21,     0,   586,     0,   587,     0,   588,   549,   589,    10,
      11,     0,   744,     0,   184,     0,     0,     0,     0,     0,
     205,     0,     0,     0,    34,     0,    35,    36,   762,     0,
       0,    10,    11,     0,    37,    38,    58,     0,    39,     0,
     296,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    59,    40,    60,     0,     0,     0,     0,     0,     0,
       0,     0,    61,    62,    63,    64,     0,     0,   205,     0,
       0,     0,     0,   184,     0,     0,     0,    65,    66,     0,
       0,     0,   414,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   296,    44,    45,     0,     0,   414,     0,
     184,    46,    47,    48,     0,    67,    49,    68,     0,    69,
       0,    70,    71,    72,    73,    50,    74,     0,    75,     0,
    1094,     0,  1094,    12,     0,    13,    14,     0,     0,     0,
       0,     0,    15,    16,    17,    31,    32,    18,     0,     0,
       0,     0,     0,     0,     0,    12,    19,    13,    14,     0,
       0,    20,    21,     0,    15,    16,    17,   590,     0,    18,
       0,     0,     0,     0,     0,   414,     0,     0,    19,    31,
      32,     0,     0,    20,    21,     0,     0,     0,     0,     0,
       0,     0,   591,   592,     0,     0,    83,     0,    84,    85,
      86,    87,    88,    42,    43,    89,    90,    91,    55,     0,
     549,     0,     0,     0,   593,   973,     0,     0,    33,    92,
      93,    94,    95,    96,    97,    98,    99,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,     0,     0,   102,     0,     0,   103,   104,     0,   105,
       0,     0,    33,     0,   578,   106,   107,   108,   583,    34,
       0,    35,    36,   109,     0,     0,     0,     0,   602,    37,
      38,     0,     0,    39,   889,   890,   607,   110,   111,   609,
       0,     0,     0,     0,     0,     0,     0,    40,   112,     0,
       0,     0,     0,    34,     0,    35,    36,     0,     0,     0,
       0,     0,     0,    37,    38,     0,     0,    39,     0,   212,
     113,   114,    10,    11,     0,     0,     0,   115,     0,    44,
      45,    40,  1031,     0,     0,   116,    46,    47,    48,   117,
       0,    49,     0,     0,    42,    43,     0,    90,     0,     0,
      50,     0,     0,     0,     0,   118,     0,     0,     0,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,     0,   138,
      31,    32,     0,     0,   139,    55,   140,   141,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
       0,     0,     0,     0,     0,   142,   143,   144,   145,     0,
       0,     0,   146,     0,   147,   148,   149,   150,   151,   152,
       0,   153,    83,     0,    84,    85,    86,    87,    88,    42,
      43,    89,    90,    91,    55,     0,    12,     0,    13,    14,
       0,     0,     0,     0,     0,    15,    16,    17,    95,    96,
      18,     0,   518,    33,     0,   100,     0,     0,     0,    19,
      44,    45,   101,     0,    20,    21,   205,    46,    47,    48,
       0,     0,    49,     0,     0,     0,     0,     0,     0,   609,
     212,    50,   107,    10,    11,     0,     0,   414,     0,   109,
       0,     0,   414,     0,    34,     0,    35,    36,   736,     0,
       0,     0,     0,     0,    37,    38,    58,     0,    39,     0,
       0,     0,     0,     0,     0,     0,     0,   213,     0,     0,
       0,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,     0,     0,   115,     0,    44,    45,     0,     0,     0,
       0,   116,    46,    47,    48,   117,   586,    49,   587,     0,
     588,     0,   589,    10,    11,     0,    50,     0,     0,     0,
       0,   118,     0,     0,     0,   119,   120,   121,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,     0,   138,     0,    12,     0,    13,
      14,     0,   140,   141,     0,     0,    15,    16,    17,     0,
       0,    18,     0,  1152,     0,     0,     0,     0,     0,     0,
      19,   142,   143,   144,   145,    20,    21,     0,   319,     0,
     147,   148,   149,   150,   151,   152,    83,   153,    84,    85,
      86,    87,    88,    42,    43,    89,    90,    91,    55,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    95,    96,     0,     0,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   101,    12,   213,    13,
      14,     0,     0,     0,     0,     0,    15,    16,    17,     0,
       0,    18,     0,     0,     0,     0,   107,     0,     0,     0,
      19,     0,     0,   109,     0,    20,    21,     0,    52,     0,
       0,   590,    53,    54,     0,    31,    32,     0,     0,     0,
      55,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   591,   592,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,     0,     0,   807,     0,     0,     0,   115,   593,    44,
      45,     0,     0,     0,     0,   116,    46,    47,    48,   117,
       0,    49,     0,     0,     0,    57,     0,     0,     0,     0,
      50,     0,     0,     0,     0,   118,     0,     0,    33,   119,
     120,   121,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,     0,   138,
       0,     0,     0,     0,    52,     0,   140,   141,    53,    54,
       0,    31,    32,     0,     0,     0,    55,     0,     0,    34,
       0,    35,    36,     0,     0,   142,   143,   144,   145,    37,
      38,    58,   146,    39,   147,   148,   149,   150,   151,   152,
       0,   153,     0,     0,     0,     0,    59,    40,    60,     0,
       0,     0,     0,     0,     0,     0,     0,    61,    62,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,   586,    66,   587,     0,   588,     0,   589,    10,
      11,     0,     0,     0,    33,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      67,     0,    68,     0,    69,     0,    70,    71,    72,    73,
     586,    74,   587,    75,   588,     0,   589,    10,    11,     0,
       0,     0,     0,     0,     0,    34,     0,    35,    36,     0,
       0,     0,     0,     0,     0,    37,     0,    58,     0,    39,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    59,    40,    60,     0,     0,     0,     0,     0,
       0,     0,     0,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    66,
     947,   948,     0,     0,     0,    55,     0,     0,     0,     0,
       0,     0,     0,    12,     0,    13,    14,     0,   974,     0,
       0,     0,    15,    16,    17,     0,    67,    18,    68,     0,
      69,     0,    70,    71,    72,    73,    19,    74,     0,    75,
       0,    20,    21,     0,     0,     0,     0,   590,     0,     0,
       0,    12,     0,    13,    14,     0,     0,     0,     0,     0,
      15,    16,    17,     0,     0,    18,    23,    24,     0,   949,
     950,    55,   591,   592,    19,     0,     0,     0,     0,    20,
      21,     0,     0,     0,     0,   590,     0,   951,   952,    25,
     953,   954,     0,     0,   593,     0,     0,   955,   956,     0,
     957,   958,    26,   959,   960,   212,     0,     0,    10,    11,
     591,   592,     0,     0,    12,     0,   961,    14,     0,     0,
       0,     0,     0,    15,    16,    17,   452,     0,    18,     0,
       0,     0,   593,     0,     0,   949,   950,    19,     0,     0,
       0,     0,   962,   963,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   951,   952,    25,   953,   954,     0,     0,
       0,     0,     0,   955,   956,     0,   957,   958,    26,   959,
     960,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    27,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   452,     0,     0,   453,     0,     0,     0,     0,
       0,   454,     0,     0,     0,     0,     0,     0,    28,    29,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    12,     0,    13,    14,     0,     0,     0,     0,
       0,    15,    16,    17,     0,     0,    18,     0,     0,     0,
       0,     0,     0,     0,     0,    19,     0,     0,     0,     0,
      20,    21,     0,     0,     0,     0,     0,   392,     0,     0,
       0,   453,     0,     0,     0,     0,     0,   454,     0,     0,
     334,   335,   336,   337,   338,   339,     0,     0,   340,     0,
       0,     0,     0,     0,     0,     0,     0,   393,   342,     0,
     343,   344,   345,   346,   347,   348,     0,     0,   394,     0,
       0,   349,   395,   213,   396,   397,     0,     0,   398,     0,
     399,     0,   351,   352,   353,     0,     0,     0,   400,   355,
     356,   357,     0,     0,   358,     0,   359,     0,   361,   401,
     402,   363,     0,     0,   403,     0,     0,     0,     0,   365,
     366,   367,     0,   368,   369,     0,   370,     0,     0,   371,
       0,     0,     0,     0,     0,   404,   405,   406,   407,   373,
       0,     0,   374,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     375,   376,   333,     0,     0,     0,     0,   377,   378,   379,
     380,   408,   382,     0,   383,   334,   335,   336,   337,   338,
     339,     0,     0,   340,     0,     0,     0,     0,     0,     0,
       0,     0,   341,   342,     0,   343,   344,   345,   346,   347,
     348,     0,     0,     0,     0,     0,   349,     0,     0,     0,
       0,     0,     0,   350,     0,     0,     0,   351,   352,   353,
       0,     0,     0,   354,   355,   356,   357,     0,     0,   358,
       0,   359,   360,   361,     0,   362,   363,     0,     0,   364,
       0,     0,     0,     0,   365,   366,   367,     0,   368,   369,
       0,   370,     0,     0,   371,     0,     0,     0,     0,     0,
     372,     0,     0,     0,   373,     0,     0,   374,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,   376,     0,     0,     0,
       0,     0,   377,   378,   379,   380,   381,   382,     0,   383
};

static const yytype_int16 yycheck[] =
{
       5,    67,     8,   409,    70,   154,   349,    73,    68,    69,
     224,     2,   145,   354,   147,    75,    74,   150,   151,     5,
     158,   528,   184,    69,   356,   149,   314,   344,   152,    75,
     289,   299,   618,   301,   340,   555,   775,   375,   718,   377,
     164,   761,    81,    82,   382,   534,   621,   622,   634,   414,
     291,   456,   293,   294,   725,   824,   119,   805,   772,   569,
     739,   166,    67,    68,    69,    70,    71,    16,    73,    20,
      75,    19,    20,   411,    19,    20,    81,    82,   822,   823,
     410,    19,    20,   812,   143,    71,   733,    16,   923,   213,
      19,   595,    24,   166,   598,   998,   600,    11,    11,   172,
     990,   107,  1002,   153,   510,   139,   125,   611,   137,   142,
     173,   142,  1196,   125,    19,    20,   370,    78,    78,   103,
     139,   254,   255,   125,   163,   103,   630,   139,   252,   253,
     125,   124,   198,   199,   793,    64,   542,   139,   196,   197,
     125,   547,   801,   148,   139,   152,   139,   125,   153,   128,
     299,   156,   301,   138,   139,   139,  1240,   139,   163,   128,
     139,   139,   148,   168,   125,   170,   125,   206,   292,   151,
     139,   295,   137,    24,   234,   140,   138,   139,   139,   139,
     139,   214,   168,   214,   170,   219,   215,    33,   234,     6,
     135,   136,   137,   198,   199,   328,   215,   142,   143,   144,
     259,   206,   147,  1103,   418,   217,  1109,   136,   258,   141,
       0,   156,   125,   215,  1104,   621,   622,   322,   147,   224,
     286,   137,   217,    19,    20,   139,   139,   941,     4,   234,
     290,   136,   637,   565,  1069,   914,   568,   543,   243,   453,
     454,   139,   247,   248,   290,   252,   253,   995,  1017,   322,
     760,   256,   590,   258,   659,   593,   831,   986,   768,   597,
     907,   247,   248,   139,   215,   214,   214,    19,    20,   319,
     256,    55,   311,   218,    11,   605,   214,  1021,    95,   125,
     218,   286,   786,   787,   137,   290,   790,   617,   139,    11,
     141,   697,   698,   139,   137,   137,   216,   137,   218,   215,
      81,    55,   119,   120,   121,   711,   311,   561,   562,   563,
      91,   139,   108,   318,   319,   137,   570,    11,   137,   217,
     574,   575,   139,   139,   730,   121,   137,    11,   145,   818,
     137,   218,   318,   109,   137,   340,  1007,   137,   214,   135,
      92,   125,   352,   353,    18,   139,   479,   480,   481,   359,
     167,   361,   169,    19,    20,   139,   173,   139,  1078,   492,
     142,   218,   215,   496,   513,   161,   162,   433,   501,   163,
     164,   125,   215,   215,   365,   215,   442,   437,   438,   155,
     745,   133,   448,   135,   136,   139,    18,    19,   550,   217,
     214,   143,   144,   215,   400,   147,   215,   460,   138,   139,
     466,   217,   462,    18,   215,  1144,   926,   470,   215,   161,
     753,     1,   215,   418,    18,   215,   462,  1003,   759,   151,
    1100,   153,   154,   155,    18,   831,   431,   490,   433,   939,
     736,   748,   437,   438,   766,   723,   650,   442,   125,   804,
     214,   446,   701,   448,   849,   431,   218,   609,   453,   454,
     218,   456,   139,   140,    43,   218,   137,   462,   139,   140,
     446,   466,   214,   139,   705,   141,   457,   808,   709,   135,
     136,   157,   713,   159,   636,    64,   142,   143,   144,   885,
     214,   147,    72,   139,    74,   125,   142,    19,    20,   125,
     156,   214,    24,   532,    83,   534,   556,    86,   138,   139,
     140,    90,   138,   139,  1011,   214,   139,   163,   164,   142,
     556,   139,   329,   330,   142,   214,   105,   216,   218,   125,
      11,   526,   111,   129,   130,   131,   132,   532,    11,   534,
     139,   537,   214,   139,   150,   151,   152,    19,   543,   141,
     526,   946,    19,    20,   604,    19,    20,    24,   191,   192,
     216,   556,   138,   139,   703,   138,   689,   144,   604,   149,
     965,   775,   152,    78,   778,   217,   971,   835,   158,   783,
     138,   139,   138,   139,   164,   219,   642,   142,   644,   355,
     356,   219,   358,   217,   927,    98,    99,  1091,   646,   138,
     139,    56,   105,   123,   124,   651,   652,   110,   111,   604,
     437,   438,   219,   135,   136,   216,   196,   197,   151,   742,
     142,   143,   144,   145,    12,   147,    14,   151,    16,    67,
      18,    19,    20,   213,   156,   151,    24,  1247,  1248,  1034,
     215,   108,   637,   638,   214,   214,   974,   642,   214,   644,
     215,   219,   142,   460,   121,   650,   860,   219,   217,   654,
     137,   139,   638,   470,   659,   219,   915,   217,   135,   139,
     138,   138,   252,   253,   156,   218,   128,   218,   145,    18,
     819,   218,   489,   490,    19,    20,    18,   815,   218,    18,
     138,    79,   499,   151,   161,   162,   835,   215,   505,   198,
     199,   200,   201,   202,   699,   198,   199,   200,   201,   202,
     219,   219,   292,   139,   218,   295,   216,   215,    12,    18,
      14,   141,    16,   699,    18,    19,    20,   214,   219,   214,
      24,   139,    33,   540,   541,   219,   219,   158,   217,   215,
     215,   736,   856,   142,   144,   219,   217,   214,   136,   139,
     219,   746,   139,   220,   219,   216,    91,   145,  1154,  1008,
     154,   140,   219,   343,   344,   142,  1161,   881,    32,   157,
     746,   159,    32,    32,    67,   217,   128,   357,   142,    32,
     775,  1112,    32,   778,   779,    79,   142,    32,   783,   818,
     370,   557,   139,    32,    32,   776,   376,   792,   793,   565,
     135,   136,   568,   383,    32,    18,   801,   142,   143,   144,
      18,  1207,   147,   140,    32,  1143,   792,    32,  1213,    67,
     215,   156,   138,   818,   154,   821,   214,   141,   216,   215,
     218,   826,   220,   215,   222,   223,   224,   225,   158,   215,
     606,   837,   136,   215,   217,    56,   215,   139,   215,   215,
     826,   145,    18,   619,   849,   215,   215,   215,    33,    19,
      20,   139,   219,   629,    24,   860,  1070,   137,   214,   151,
     137,  1010,    78,   142,    12,   219,    14,   128,    16,    18,
      18,    19,    20,   690,   215,   219,    24,    32,   125,    32,
     142,   138,   128,   142,   288,   219,   142,   291,   215,   293,
     294,   667,   215,   138,   670,   299,   139,   301,   219,   217,
     215,   215,   215,   215,   721,   144,   140,   215,   215,   219,
     214,   142,   216,    78,   218,   215,   220,   137,   222,   223,
     224,   225,   982,   928,    16,  1049,    56,   138,   215,   139,
    1144,    79,   139,   139,  1067,   139,   982,   139,   108,   139,
     138,   946,   928,   215,   284,   285,   137,   215,   215,   142,
      56,   121,  1166,   142,   142,   139,   296,    32,   139,   299,
     965,   301,   216,   214,   142,   135,   971,   142,   139,   138,
     560,   561,   562,   563,   215,   145,   142,   982,   215,   219,
     570,   214,   144,   219,   574,   575,   142,   144,   136,   137,
     766,   161,   162,   215,   202,  1000,   772,   145,   142,   147,
     142,   202,   137,   215,    18,   409,   215,   395,  1026,   177,
     215,   630,  1091,   830,  1000,   423,   219,  1150,     3,     4,
       5,     6,     7,     8,     9,    10,   639,  1023,   454,  1034,
    1035,  1034,  1085,   921,   340,   649,   812,   158,   619,   815,
     838,  1121,  1005,  1145,   214,  1203,   647,   724,   218,  1035,
     220,   776,   945,  1165,  1086,   794,   646,   833,   979,   532,
     780,   969,   972,  1068,  1180,  1070,   214,   407,   216,   784,
     988,  1181,   220,   802,   222,   223,   224,   225,   753,   994,
    1085,  1086,  1068,   900,  1170,   930,  1216,  1172,   905,   906,
     984,   908,   909,   910,   911,   912,   153,  1102,   703,   916,
     887,   295,  1234,  1010,   722,  1110,   510,   322,   685,   513,
    1206,  1155,  1209,  1179,   547,   736,  1102,  1036,   730,  1149,
     670,   938,   114,   671,  1110,  1163,  1140,  1132,  1133,  1134,
    1135,   828,  1024,   764,  1139,  1141,  1019,   177,   542,  1144,
    1145,  1146,   413,   547,  1117,  1143,  1132,  1133,  1134,  1135,
      -1,    -1,    -1,  1139,    -1,    -1,  1161,   747,   748,    -1,
    1146,  1166,    -1,  1168,  1169,   941,   942,    -1,    -1,   509,
      -1,    -1,    -1,   513,  1179,    -1,    -1,     3,    -1,    -1,
      -1,    -1,  1168,  1169,  1001,    -1,    -1,    -1,    -1,   779,
      12,    -1,    14,    -1,    16,    -1,    18,    19,    20,    -1,
      -1,  1018,    24,   793,    -1,    -1,    -1,    -1,  1213,    -1,
     986,   801,    -1,   989,    -1,   619,    -1,   621,   622,    -1,
      16,    -1,    -1,    19,    20,  1230,   566,    19,    20,    -1,
      56,    57,    24,    -1,    -1,    -1,    -1,    -1,    -1,  1244,
    1245,    -1,    68,    69,  1230,    -1,    -1,    -1,  1024,    75,
      -1,    77,    -1,    -1,    -1,    -1,    -1,    79,  1244,  1245,
    1036,    -1,    -1,    12,    -1,    14,   856,    16,    -1,    18,
      19,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   623,    -1,    -1,    -1,    -1,    -1,    19,
      20,   881,    -1,   697,   698,    -1,    -1,    -1,    -1,   703,
      -1,   705,    -1,    -1,    -1,   709,    -1,   711,    -1,   713,
      -1,  1128,    -1,    -1,   136,    -1,   108,    -1,    -1,    19,
      20,  1138,    -1,   145,    -1,   147,   730,    -1,    -1,   121,
      79,   921,    -1,    -1,  1151,    -1,    -1,   133,  1155,   135,
     136,    -1,    -1,   135,    -1,    -1,   142,   143,   144,    -1,
      -1,   147,    -1,   145,    -1,    -1,    -1,   183,   184,    -1,
     156,    91,    -1,   703,    -1,   161,   162,    -1,   772,   161,
     162,    -1,  1189,    -1,  1191,    -1,    -1,    19,    20,    -1,
      -1,    -1,    24,    -1,    -1,    -1,    -1,   136,    -1,    -1,
      -1,    -1,   214,    -1,   216,    -1,   145,    -1,   220,    -1,
     222,   223,   224,   225,    -1,   135,   136,    -1,   234,    -1,
      -1,  1228,   142,   143,   144,   819,    -1,   147,   214,    -1,
     216,   761,   214,    -1,   764,    -1,   156,   831,   220,    19,
      20,   835,    22,   133,    24,   135,   136,   137,    -1,    -1,
      -1,    12,   142,   143,   144,    16,    17,   147,    19,    20,
      92,    -1,    -1,    24,    -1,    -1,   156,    -1,   798,  1049,
      -1,   161,   162,   289,   290,   214,    -1,   216,    -1,   218,
      -1,   220,    -1,   222,   223,   224,   225,    -1,    -1,   819,
      -1,   885,    -1,    -1,    -1,   825,    -1,    -1,   828,    -1,
      -1,   133,    -1,   135,   136,   835,  1086,    -1,    -1,    -1,
      71,   143,   144,   145,    -1,   147,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,   340,    -1,   342,    -1,    -1,   161,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   941,    -1,    -1,
     366,    -1,    -1,   369,    -1,   135,   136,    -1,    -1,    -1,
      19,    20,   142,   143,   144,   145,    -1,   147,   384,    -1,
     386,   387,   133,    -1,   135,   136,   156,    19,    20,    21,
      22,    -1,   143,   144,   145,    -1,   147,   148,   149,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     171,   172,   173,   174,    -1,    -1,  1010,    -1,    -1,   435,
      79,   437,   438,    12,    -1,   186,   187,    16,    17,    -1,
      19,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   462,    -1,    -1,    -1,
      -1,   981,    -1,   214,   984,   216,    -1,   218,    -1,   220,
     221,   222,   223,    -1,   225,    -1,   227,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   135,   136,    -1,    -1,
    1010,    -1,    71,   142,   143,   144,    -1,    -1,   147,    -1,
      79,   133,    -1,   135,   136,    -1,    -1,   156,    19,    20,
     142,   143,   144,    92,    -1,   147,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,    -1,   161,
     162,    -1,    12,    -1,    14,    -1,    16,   543,    18,    19,
      20,    -1,   548,    -1,   550,    -1,    -1,    -1,    -1,    -1,
     556,    -1,    -1,    -1,   133,    -1,   135,   136,  1078,    -1,
      -1,    19,    20,    -1,   143,   144,   145,    -1,   147,    -1,
    1154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   171,   172,   173,   174,    -1,    -1,   604,    -1,
      -1,    -1,    -1,   609,    -1,    -1,    -1,   186,   187,    -1,
      -1,    -1,   618,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1207,   135,   136,    -1,    -1,   634,    -1,
     636,   142,   143,   144,    -1,   214,   147,   216,    -1,   218,
      -1,   220,   221,   222,   223,   156,   225,    -1,   227,    -1,
    1170,    -1,  1172,   133,    -1,   135,   136,    -1,    -1,    -1,
      -1,    -1,   142,   143,   144,    19,    20,   147,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   133,   156,   135,   136,    -1,
      -1,   161,   162,    -1,   142,   143,   144,   167,    -1,   147,
      -1,    -1,    -1,    -1,    -1,   701,    -1,    -1,   156,    19,
      20,    -1,    -1,   161,   162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   192,   193,    -1,    -1,    12,    -1,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
     736,    -1,    -1,    -1,   214,   215,    -1,    -1,    92,    35,
      36,    37,    38,    39,    40,    41,    42,    -1,    -1,    45,
      -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,    -1,    -1,
      -1,    -1,    -1,    59,    -1,    -1,    62,    63,    -1,    65,
      -1,    -1,    92,    -1,   780,    71,    72,    73,   784,   133,
      -1,   135,   136,    79,    -1,    -1,    -1,    -1,   794,   143,
     144,    -1,    -1,   147,   148,   149,   802,    93,    94,   805,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,   104,    -1,
      -1,    -1,    -1,   133,    -1,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,   143,   144,    -1,    -1,   147,    -1,    16,
     126,   127,    19,    20,    -1,    -1,    -1,   133,    -1,   135,
     136,   161,   848,    -1,    -1,   141,   142,   143,   144,   145,
      -1,   147,    -1,    -1,    19,    20,    -1,    22,    -1,    -1,
     156,    -1,    -1,    -1,    -1,   161,    -1,    -1,    -1,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   183,    -1,   185,
      19,    20,    -1,    -1,   190,    24,   192,   193,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   915,
      -1,    -1,    -1,    -1,    -1,   211,   212,   213,   214,    -1,
      -1,    -1,   218,    -1,   220,   221,   222,   223,   224,   225,
      -1,   227,    12,    -1,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,   133,    -1,   135,   136,
      -1,    -1,    -1,    -1,    -1,   142,   143,   144,    38,    39,
     147,    -1,   149,    92,    -1,    45,    -1,    -1,    -1,   156,
     135,   136,    52,    -1,   161,   162,   982,   142,   143,   144,
      -1,    -1,   147,    -1,    -1,    -1,    -1,    -1,    -1,   995,
      16,   156,    72,    19,    20,    -1,    -1,  1003,    -1,    79,
      -1,    -1,  1008,    -1,   133,    -1,   135,   136,   137,    -1,
      -1,    -1,    -1,    -1,   143,   144,   145,    -1,   147,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   214,    -1,    -1,
      -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,
      -1,    -1,    -1,   133,    -1,   135,   136,    -1,    -1,    -1,
      -1,   141,   142,   143,   144,   145,    12,   147,    14,    -1,
      16,    -1,    18,    19,    20,    -1,   156,    -1,    -1,    -1,
      -1,   161,    -1,    -1,    -1,   165,   166,   167,   168,   169,
     170,   171,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,   183,    -1,   185,    -1,   133,    -1,   135,
     136,    -1,   192,   193,    -1,    -1,   142,   143,   144,    -1,
      -1,   147,    -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,
     156,   211,   212,   213,   214,   161,   162,    -1,   218,    -1,
     220,   221,   222,   223,   224,   225,    12,   227,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    -1,    -1,    -1,    -1,    -1,    52,   133,   214,   135,
     136,    -1,    -1,    -1,    -1,    -1,   142,   143,   144,    -1,
      -1,   147,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
     156,    -1,    -1,    79,    -1,   161,   162,    -1,    12,    -1,
      -1,   167,    16,    17,    -1,    19,    20,    -1,    -1,    -1,
      24,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   192,   193,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,   209,    -1,    -1,    -1,   133,   214,   135,
     136,    -1,    -1,    -1,    -1,   141,   142,   143,   144,   145,
      -1,   147,    -1,    -1,    -1,    79,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,    -1,   161,    -1,    -1,    92,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   183,    -1,   185,
      -1,    -1,    -1,    -1,    12,    -1,   192,   193,    16,    17,
      -1,    19,    20,    -1,    -1,    -1,    24,    -1,    -1,   133,
      -1,   135,   136,    -1,    -1,   211,   212,   213,   214,   143,
     144,   145,   218,   147,   220,   221,   222,   223,   224,   225,
      -1,   227,    -1,    -1,    -1,    -1,   160,   161,   162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,   172,   173,
     174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    79,    12,   187,    14,    -1,    16,    -1,    18,    19,
      20,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     214,    -1,   216,    -1,   218,    -1,   220,   221,   222,   223,
      12,   225,    14,   227,    16,    -1,    18,    19,    20,    -1,
      -1,    -1,    -1,    -1,    -1,   133,    -1,   135,   136,    -1,
      -1,    -1,    -1,    -1,    -1,   143,    -1,   145,    -1,   147,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   171,   172,   173,   174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   187,
      19,    20,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   133,    -1,   135,   136,    -1,   138,    -1,
      -1,    -1,   142,   143,   144,    -1,   214,   147,   216,    -1,
     218,    -1,   220,   221,   222,   223,   156,   225,    -1,   227,
      -1,   161,   162,    -1,    -1,    -1,    -1,   167,    -1,    -1,
      -1,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,
     142,   143,   144,    -1,    -1,   147,    19,    20,    -1,    88,
      89,    24,   192,   193,   156,    -1,    -1,    -1,    -1,   161,
     162,    -1,    -1,    -1,    -1,   167,    -1,   106,   107,   108,
     109,   110,    -1,    -1,   214,    -1,    -1,   116,   117,    -1,
     119,   120,   121,   122,   123,    16,    -1,    -1,    19,    20,
     192,   193,    -1,    -1,   133,    -1,   135,   136,    -1,    -1,
      -1,    -1,    -1,   142,   143,   144,   145,    -1,   147,    -1,
      -1,    -1,   214,    -1,    -1,    88,    89,   156,    -1,    -1,
      -1,    -1,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,   108,   109,   110,    -1,    -1,
      -1,    -1,    -1,   116,   117,    -1,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   145,    -1,    -1,   214,    -1,    -1,    -1,    -1,
      -1,   220,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,
      -1,   142,   143,   144,    -1,    -1,   147,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,    -1,
     161,   162,    -1,    -1,    -1,    -1,    -1,    13,    -1,    -1,
      -1,   214,    -1,    -1,    -1,    -1,    -1,   220,    -1,    -1,
      26,    27,    28,    29,    30,    31,    -1,    -1,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    43,    44,    -1,
      46,    47,    48,    49,    50,    51,    -1,    -1,    54,    -1,
      -1,    57,    58,   214,    60,    61,    -1,    -1,    64,    -1,
      66,    -1,    68,    69,    70,    -1,    -1,    -1,    74,    75,
      76,    77,    -1,    -1,    80,    -1,    82,    -1,    84,    85,
      86,    87,    -1,    -1,    90,    -1,    -1,    -1,    -1,    95,
      96,    97,    -1,    99,   100,    -1,   102,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,   111,   112,   113,   114,   115,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     196,   197,    13,    -1,    -1,    -1,    -1,   203,   204,   205,
     206,   207,   208,    -1,   210,    26,    27,    28,    29,    30,
      31,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    43,    44,    -1,    46,    47,    48,    49,    50,
      51,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,    70,
      -1,    -1,    -1,    74,    75,    76,    77,    -1,    -1,    80,
      -1,    82,    83,    84,    -1,    86,    87,    -1,    -1,    90,
      -1,    -1,    -1,    -1,    95,    96,    97,    -1,    99,   100,
      -1,   102,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,
     111,    -1,    -1,    -1,   115,    -1,    -1,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   196,   197,    -1,    -1,    -1,
      -1,    -1,   203,   204,   205,   206,   207,   208,    -1,   210
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,     3,     4,     5,     6,     7,     8,     9,    10,   247,
      19,    20,   133,   135,   136,   142,   143,   144,   147,   156,
     161,   162,   259,    19,    20,   108,   121,   135,   161,   162,
     278,    19,    20,    92,   133,   135,   136,   143,   144,   147,
     161,   290,    19,    20,   135,   136,   142,   143,   144,   147,
     156,   293,    12,    16,    17,    24,    71,    79,   145,   160,
     162,   171,   172,   173,   174,   186,   187,   214,   216,   218,
     220,   221,   222,   223,   225,   227,   260,   288,   290,   310,
     311,   312,   313,    12,    14,    15,    16,    17,    18,    21,
      22,    23,    35,    36,    37,    38,    39,    40,    41,    42,
      45,    52,    59,    62,    63,    65,    71,    72,    73,    79,
      93,    94,   104,   126,   127,   133,   141,   145,   161,   165,
     166,   167,   168,   169,   170,   171,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   183,   185,   190,
     192,   193,   211,   212,   213,   214,   218,   220,   221,   222,
     223,   224,   225,   227,   254,   255,   260,   262,   289,   293,
     297,   298,   299,   300,   367,   381,   382,   391,   392,   393,
     394,   395,   396,   397,   445,   446,   449,   450,     0,    11,
      11,    11,    11,   214,   290,   305,   290,    18,    16,    19,
      64,   136,   147,   271,   272,   273,   218,   218,   214,   214,
      18,    18,   310,   320,   323,   290,   312,   316,   317,   317,
     320,   310,    16,   214,   259,   263,   324,   320,   324,   317,
     139,   141,   290,    11,   139,   311,   311,   381,   400,   139,
     390,   421,   390,   390,   218,   414,   422,   390,   449,   293,
     390,   390,   422,    18,   400,   381,   381,   216,   218,   314,
     314,   314,   218,   218,   214,   214,   214,   381,   218,   383,
     384,   384,   381,   398,   401,   449,   381,   398,   310,   263,
     402,   398,   398,   402,   148,   149,   323,   366,    12,    14,
      16,    18,    19,    20,    79,   136,   157,   159,   214,   216,
     218,   220,   222,   223,   224,   225,   289,   295,   370,   377,
     378,   293,   139,   141,   260,   259,   297,   299,   387,   398,
     311,   326,   327,   328,   216,   263,    11,   125,   139,   218,
     382,   383,   385,   386,   381,   310,   381,   310,   214,   423,
     382,   400,    11,    13,    26,    27,    28,    29,    30,    31,
      34,    43,    44,    46,    47,    48,    49,    50,    51,    57,
      64,    68,    69,    70,    74,    75,    76,    77,    80,    82,
      83,    84,    86,    87,    90,    95,    96,    97,    99,   100,
     102,   105,   111,   115,   118,   196,   197,   203,   204,   205,
     206,   207,   208,   210,   248,   249,   250,   251,   256,   439,
     443,    11,    13,    43,    54,    58,    60,    61,    64,    66,
      74,    85,    86,    90,   111,   112,   113,   114,   207,   252,
     253,   256,   439,   447,   290,   302,   303,   305,   139,   301,
     141,   271,   271,   138,   274,   144,   324,   324,   320,   320,
      78,   138,   322,   137,   215,   138,   292,   137,   140,   318,
     217,   219,   137,   215,   217,   263,   142,   219,   137,   215,
     219,   217,   145,   214,   220,   260,   276,   277,   278,   279,
     140,    56,   216,   420,   151,   317,   157,   159,   415,   151,
      67,   151,   141,   260,   215,   215,   215,   310,   310,   214,
     214,   214,   402,   402,   398,   398,   310,   366,   384,   138,
     140,   399,   137,   215,   215,   219,   137,   215,   217,   142,
     219,   137,   215,   217,   219,   137,   295,   295,   320,   136,
     147,   295,   369,   370,   373,   303,   317,   373,   149,   263,
     375,   373,   373,   375,   295,   378,   139,   388,   378,   139,
     217,   311,   138,   329,   156,   330,   387,   218,   310,   386,
     103,   103,    78,    78,   398,   381,   381,   128,   288,   290,
     291,   345,   290,   259,   286,   286,   218,   346,   355,    16,
     214,   265,   265,   265,   445,    91,   293,   429,    91,   429,
     259,   261,   216,   293,   265,   265,   278,   332,   290,   339,
      18,    19,   264,   290,   341,   261,    12,    14,    16,    18,
     167,   192,   193,   214,   259,   266,   267,   259,   266,    18,
     266,   259,   290,   337,   218,   440,   441,   290,   352,   290,
     343,   266,   444,   442,    18,   105,   111,   249,   218,   308,
     449,    81,    91,   136,   295,   436,    18,   369,   431,   441,
     266,   448,   442,   301,   138,   304,   215,   276,   151,   272,
     219,   219,   137,   215,   137,   215,   218,   310,   320,   290,
     139,   316,   316,   320,   216,   215,   310,   320,    18,   276,
     280,   280,   141,   279,   278,   400,   317,   214,   219,   320,
     214,   419,   400,   139,   217,   219,   398,   398,   398,   219,
     219,   215,   215,   138,   219,   381,   400,   398,   398,   214,
     216,   381,   398,   381,   158,   295,   369,    33,   138,   139,
     374,   218,   370,   371,   372,   137,   215,   217,   219,   137,
     215,   142,   219,   137,   215,   217,   219,   310,   150,   151,
     152,   389,   388,   216,   327,   328,   144,   449,   381,   381,
     137,   369,   407,   408,   409,   410,   137,   291,   309,   411,
     412,   413,   140,   409,   290,   305,   139,   142,    32,   287,
     287,   317,   293,    32,   356,   259,   261,   261,   261,    67,
     429,   214,   295,   424,   427,   428,    32,   430,   429,   430,
     261,   217,   128,   261,   261,   142,    32,   333,   139,   214,
      32,   340,   142,   139,    32,   342,   266,   266,   268,   267,
     266,   270,   142,   214,    32,   338,   317,   441,   293,   363,
     142,   214,    32,   353,   305,    32,   344,   209,   258,   140,
      18,    18,   441,   302,   306,   289,   293,   294,   300,   331,
     438,    67,   431,   431,   295,   128,   139,   435,    32,   437,
     142,    32,   432,    79,   293,   296,   433,   258,   302,   305,
     310,   274,   320,   320,   324,   322,   215,   292,   218,   276,
     319,   318,   318,   215,   315,   320,   138,   325,   215,   141,
     138,   281,   215,   215,   217,   293,   416,   417,   158,   417,
     421,    56,   215,   215,   215,    18,   399,   215,   215,   398,
     381,   138,   403,   215,   217,    33,   369,   369,   310,   148,
     149,   303,   365,   372,   373,   373,   369,   373,   273,   381,
     389,   387,   329,   330,   219,    55,    55,   408,   124,   151,
     153,   154,   155,   410,   411,   214,   151,   413,   398,   301,
     310,   137,   259,   283,   284,   285,   286,   219,    78,   348,
     355,    20,   215,   445,   430,   424,   425,   428,   142,   429,
     430,   128,   289,   294,   319,   332,   276,    19,    20,    88,
      89,   106,   107,   109,   110,   116,   117,   119,   120,   122,
     123,   135,   161,   162,   259,   276,   282,   334,   335,   339,
      18,   276,   341,   215,   138,   269,   215,   310,   335,   337,
     219,   214,   218,   295,   360,   361,    32,   364,   137,   346,
     349,   350,   351,   335,   352,   125,   354,   301,   343,   445,
     142,   142,   363,   138,   307,   219,   293,   328,   218,   370,
     379,   380,   449,   432,   432,   435,   295,   310,   142,   436,
     381,   431,   293,   378,    32,   434,   449,   304,   215,   215,
     219,   290,   217,   263,   276,   139,   138,   418,   215,   215,
     215,   215,   217,   263,   369,   374,   219,   215,   215,   138,
     376,   215,   144,   381,   381,   381,   410,   381,   404,   405,
     381,   381,   381,   381,   413,   303,   381,   140,   142,   283,
      78,   285,   287,   346,   310,   356,    16,    56,   138,   426,
     215,   381,   430,   294,   333,   139,   138,   336,   215,   340,
     342,   266,   215,   338,   295,   357,   358,   317,   361,   139,
     163,   164,   275,   363,   349,   293,   351,   215,   353,   343,
     142,   344,   198,   199,   200,   201,   202,   257,   310,   381,
     364,   302,   308,   330,   303,   380,   388,    56,   142,   435,
     381,   437,   129,   130,   131,   132,   368,   432,   142,   275,
     433,   198,   202,   257,   139,   216,   142,   281,   310,   416,
     214,   142,   149,   263,    33,    32,   406,   215,   398,   310,
     285,   276,   215,   424,   282,   334,   139,   269,   142,   139,
     138,   359,   137,   215,   219,   273,   310,   364,   351,   214,
     347,   142,   344,   310,   445,   444,   307,   144,   219,   142,
     381,   142,   310,   310,   310,   310,   381,   310,   434,   449,
     448,   319,   315,   310,   418,   398,   381,   142,   369,   404,
     215,   426,   336,   276,   310,   310,   357,   358,   144,   142,
     362,   320,   348,   350,   202,   381,   381,   368,   142,   202,
     137,   325,   215,   403,   369,   406,   359,   215,    18,   215,
     381,   310,   376,   368,   137,   140,   321,   310,   310,   219,
     321,   321
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 1557 "ats_grammar.yats"
    { theYYVALyyres = atsopt_yyres_d0eclst((yyvsp[(2) - (3)].d0eclst)) ; return 0 ; ;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 1558 "ats_grammar.yats"
    { theYYVALyyres = atsopt_yyres_d0eclst((yyvsp[(2) - (3)].d0eclst)) ; return 0 ; ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 1559 "ats_grammar.yats"
    { theYYVALyyres = atsopt_yyres_i0de((yyvsp[(2) - (3)].i0de)) ; return 0 ; ;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 1560 "ats_grammar.yats"
    { theYYVALyyres = atsopt_yyres_i0de((yyvsp[(2) - (3)].i0de)) ; return 0 ; ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 1561 "ats_grammar.yats"
    { theYYVALyyres = atsopt_yyres_i0de((yyvsp[(2) - (3)].i0de)) ; return 0 ; ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 1562 "ats_grammar.yats"
    { theYYVALyyres = atsopt_yyres_i0de((yyvsp[(2) - (3)].i0de)) ; return 0 ; ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 1563 "ats_grammar.yats"
    { theYYVALyyres = atsopt_yyres_s0exp((yyvsp[(2) - (3)].s0exp)) ; return 0 ; ;}
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 1564 "ats_grammar.yats"
    { theYYVALyyres = atsopt_yyres_d0exp((yyvsp[(2) - (3)].d0exp)) ; return 0 ; ;}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 1568 "ats_grammar.yats"
    { (yyval.abskind) = abskind_prop () ; ;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 1569 "ats_grammar.yats"
    { (yyval.abskind) = abskind_type () ; ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 1570 "ats_grammar.yats"
    { (yyval.abskind) = abskind_t0ype () ; ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 1571 "ats_grammar.yats"
    { (yyval.abskind) = abskind_view () ; ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 1572 "ats_grammar.yats"
    { (yyval.abskind) = abskind_viewtype () ; ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 1573 "ats_grammar.yats"
    { (yyval.abskind) = abskind_viewt0ype () ; ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 1577 "ats_grammar.yats"
    { (yyval.dcstkind) = dcstkind_fun () ; ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 1578 "ats_grammar.yats"
    { (yyval.dcstkind) = dcstkind_val () ; ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 1579 "ats_grammar.yats"
    { (yyval.dcstkind) = dcstkind_castfn () ; ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 1580 "ats_grammar.yats"
    { (yyval.dcstkind) = dcstkind_praxi () ; ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 1581 "ats_grammar.yats"
    { (yyval.dcstkind) = dcstkind_prfun () ; ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 1582 "ats_grammar.yats"
    { (yyval.dcstkind) = dcstkind_prval () ; ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 1586 "ats_grammar.yats"
    { (yyval.datakind) = datakind_prop () ; ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 1587 "ats_grammar.yats"
    { (yyval.datakind) = datakind_type () ; ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 1588 "ats_grammar.yats"
    { (yyval.datakind) = datakind_view () ; ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 1589 "ats_grammar.yats"
    { (yyval.datakind) = datakind_viewtype () ; ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 1593 "ats_grammar.yats"
    { (yyval.stadefkind) = stadefkind_generic () ; ;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 1594 "ats_grammar.yats"
    { (yyval.stadefkind) = stadefkind_prop ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 1595 "ats_grammar.yats"
    { (yyval.stadefkind) = stadefkind_type ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 1596 "ats_grammar.yats"
    { (yyval.stadefkind) = stadefkind_view ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 1597 "ats_grammar.yats"
    { (yyval.stadefkind) = stadefkind_viewtype ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 1601 "ats_grammar.yats"
    { (yyval.valkind) = valkind_val () ; ;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 1602 "ats_grammar.yats"
    { (yyval.valkind) = valkind_valminus () ; ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 1603 "ats_grammar.yats"
    { (yyval.valkind) = valkind_valplus () ; ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 1604 "ats_grammar.yats"
    { (yyval.valkind) = valkind_prval () ; ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 1608 "ats_grammar.yats"
    { (yyval.funkind) = funkind_fn () ; ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 1609 "ats_grammar.yats"
    { (yyval.funkind) = funkind_fnstar () ; ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 1610 "ats_grammar.yats"
    { (yyval.funkind) = funkind_fun () ; ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 1611 "ats_grammar.yats"
    { (yyval.funkind) = funkind_castfn () ; ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 1612 "ats_grammar.yats"
    { (yyval.funkind) = funkind_prfn () ; ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 1613 "ats_grammar.yats"
    { (yyval.funkind) = funkind_prfun () ; ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 1617 "ats_grammar.yats"
    { (yyval.lamkind) = lamkind_lam ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 1618 "ats_grammar.yats"
    { (yyval.lamkind) = lamkind_atlam ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 1619 "ats_grammar.yats"
    { (yyval.lamkind) = lamkind_llam ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 1620 "ats_grammar.yats"
    { (yyval.lamkind) = lamkind_atllam ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 1624 "ats_grammar.yats"
    { (yyval.fixkind) = fixkind_fix ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 1625 "ats_grammar.yats"
    { (yyval.fixkind) = fixkind_atfix ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 1629 "ats_grammar.yats"
    { (yyval.srpifkindtok) = srpifkindtok_if ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 1630 "ats_grammar.yats"
    { (yyval.srpifkindtok) = srpifkindtok_ifdef ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 1631 "ats_grammar.yats"
    { (yyval.srpifkindtok) = srpifkindtok_ifndef ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 1635 "ats_grammar.yats"
    { (yyval.srpifkindtok) = srpifkindtok_if ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 1636 "ats_grammar.yats"
    { (yyval.srpifkindtok) = srpifkindtok_ifdef ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 1637 "ats_grammar.yats"
    { (yyval.srpifkindtok) = srpifkindtok_ifndef ((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 1641 "ats_grammar.yats"
    { ; ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 1642 "ats_grammar.yats"
    { ; ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 1646 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 1647 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 1648 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_ampersand((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 1649 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_backslash((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 1650 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_bang((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 1651 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_eq((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 1652 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_gt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 1653 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_gtlt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 1654 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_lt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 1655 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_minusgt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 1656 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_minusltgt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 1657 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_tilde((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 1661 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 1665 "ats_grammar.yats"
    { (yyval.i0delst) = i0delst_nil() ; ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 1666 "ats_grammar.yats"
    { (yyval.i0delst) = i0delst_cons((yyvsp[(1) - (2)].i0de), (yyvsp[(2) - (2)].i0delst)) ; ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 1670 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 1671 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_DO((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 1672 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_WHILE((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 1676 "ats_grammar.yats"
    { (yyval.l0ab) = l0ab_ide((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 1677 "ats_grammar.yats"
    { (yyval.l0ab) = l0ab_int((yyvsp[(1) - (1)].i0nt)) ; ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 1678 "ats_grammar.yats"
    { (yyval.l0ab) = (yyvsp[(2) - (3)].l0ab) ; ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 1682 "ats_grammar.yats"
    { (yyval.i0de) = stai0de_make((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 1686 "ats_grammar.yats"
    { (yyval.p0rec) = p0rec_emp() ; ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 1687 "ats_grammar.yats"
    { (yyval.p0rec) = p0rec_int((yyvsp[(1) - (1)].i0nt)) ; ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 1688 "ats_grammar.yats"
    { (yyval.p0rec) = p0rec_ide((yyvsp[(2) - (3)].i0de)) ; ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 1689 "ats_grammar.yats"
    { (yyval.p0rec) = p0rec_opr((yyvsp[(2) - (5)].i0de), (yyvsp[(3) - (5)].i0de), (yyvsp[(4) - (5)].i0nt)) ; ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 1693 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_app((yyvsp[(1) - (2)].e0xp), (yyvsp[(2) - (2)].e0xp)) ; ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 1694 "ats_grammar.yats"
    { (yyval.e0xp) = (yyvsp[(1) - (1)].e0xp) ; ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 1698 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_char((yyvsp[(1) - (1)].c0har)) ; ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 1699 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_float((yyvsp[(1) - (1)].f0loat)) ; ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 1700 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_int((yyvsp[(1) - (1)].i0nt)) ; ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 1701 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_string((yyvsp[(1) - (1)].s0tring)) ; ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 1702 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_FILENAME((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 1703 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_LOCATION((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 1704 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_ide((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 1705 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_list((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].e0xplst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 1706 "ats_grammar.yats"
    { (yyval.e0xp) = e0xp_eval((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].e0xp), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 1710 "ats_grammar.yats"
    { (yyval.e0xplst) = e0xplst_nil() ; ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1711 "ats_grammar.yats"
    { (yyval.e0xplst) = e0xplst_cons((yyvsp[(1) - (2)].e0xp), (yyvsp[(2) - (2)].e0xplst)) ; ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 1715 "ats_grammar.yats"
    { (yyval.e0xplst) = e0xplst_nil() ; ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 1716 "ats_grammar.yats"
    { (yyval.e0xplst) = e0xplst_cons((yyvsp[(2) - (3)].e0xp), (yyvsp[(3) - (3)].e0xplst)) ; ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1720 "ats_grammar.yats"
    { (yyval.e0xpopt) = e0xpopt_none() ; ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 1721 "ats_grammar.yats"
    { (yyval.e0xpopt) = e0xpopt_some((yyvsp[(1) - (1)].e0xp)) ; ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 1725 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 1729 "ats_grammar.yats"
    { (yyval.e0fftag) = e0fftag_cst (0, (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 1730 "ats_grammar.yats"
    { (yyval.e0fftag) = e0fftag_cst (1, (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 1731 "ats_grammar.yats"
    { (yyval.e0fftag) = e0fftag_var((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 1732 "ats_grammar.yats"
    { (yyval.e0fftag) = e0fftag_var_fun((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1733 "ats_grammar.yats"
    { (yyval.e0fftag) = e0fftag_int((yyvsp[(1) - (1)].i0nt)) ; ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 1737 "ats_grammar.yats"
    { (yyval.e0fftaglst) = e0fftaglst_nil() ; ;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 1738 "ats_grammar.yats"
    { (yyval.e0fftaglst) = e0fftaglst_cons((yyvsp[(1) - (2)].e0fftag), (yyvsp[(2) - (2)].e0fftaglst)) ; ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 1742 "ats_grammar.yats"
    { (yyval.e0fftaglst) = e0fftaglst_nil() ; ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 1743 "ats_grammar.yats"
    { (yyval.e0fftaglst) = e0fftaglst_cons((yyvsp[(2) - (3)].e0fftag), (yyvsp[(3) - (3)].e0fftaglst)) ; ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 1747 "ats_grammar.yats"
    { (yyval.e0fftaglstopt) = e0fftaglstopt_none() ; ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 1748 "ats_grammar.yats"
    { (yyval.e0fftaglstopt) = e0fftaglstopt_some(e0fftaglst_nil()) ; ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 1749 "ats_grammar.yats"
    { (yyval.e0fftaglstopt) = e0fftaglstopt_some((yyvsp[(2) - (3)].e0fftaglst)) ; ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 1753 "ats_grammar.yats"
    { (yyval.s0rt) = s0rt_app((yyvsp[(1) - (2)].s0rt), (yyvsp[(2) - (2)].s0rt)) ; ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 1754 "ats_grammar.yats"
    { (yyval.s0rt) = (yyvsp[(1) - (1)].s0rt) ; ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 1758 "ats_grammar.yats"
    { (yyval.s0rtq) = s0rtq_sym((yyvsp[(1) - (2)].i0de)) ; ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 1759 "ats_grammar.yats"
    { (yyval.s0rtq) = s0rtq_str((yyvsp[(2) - (3)].s0tring)) ; ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 1763 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 1764 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 1765 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_t0ype((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 1766 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_viewt0ype((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 1767 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_backslash((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 1768 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_minusgt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 1769 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_minusltgt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 1773 "ats_grammar.yats"
    { (yyval.s0rt) = s0rt_ide((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 1774 "ats_grammar.yats"
    { (yyval.s0rt) = s0rt_qid((yyvsp[(1) - (2)].s0rtq), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 1775 "ats_grammar.yats"
    { (yyval.s0rt) = s0rt_list((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0rtlst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 1776 "ats_grammar.yats"
    { (yyval.s0rt) = s0rt_tup((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0rtlst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 1780 "ats_grammar.yats"
    { (yyval.s0rtlst) = s0rtlst_nil() ; ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 1781 "ats_grammar.yats"
    { (yyval.s0rtlst) = s0rtlst_cons((yyvsp[(1) - (2)].s0rt), (yyvsp[(2) - (2)].s0rtlst)) ; ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 1785 "ats_grammar.yats"
    { (yyval.s0rtlst) = s0rtlst_nil() ; ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 1786 "ats_grammar.yats"
    { (yyval.s0rtlst) = s0rtlst_cons((yyvsp[(2) - (3)].s0rt), (yyvsp[(3) - (3)].s0rtlst)) ; ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 1790 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make((yyvsp[(1) - (1)].s0rt), 0) ; ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 1791 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_prop((yyvsp[(1) - (1)].t0kn)), -1) ; ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 1792 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_prop((yyvsp[(1) - (1)].t0kn)),  1) ; ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 1793 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_type((yyvsp[(1) - (1)].t0kn)), -1) ; ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 1794 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_type((yyvsp[(1) - (1)].t0kn)),  1) ; ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 1795 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_t0ype((yyvsp[(1) - (1)].t0kn)), -1) ; ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 1796 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_t0ype((yyvsp[(1) - (1)].t0kn)),  1) ; ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 1797 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_view((yyvsp[(1) - (1)].t0kn)), -1) ; ;}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 1798 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_view((yyvsp[(1) - (1)].t0kn)),  1) ; ;}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 1799 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_viewtype((yyvsp[(1) - (1)].t0kn)), -1) ; ;}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 1800 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_viewtype((yyvsp[(1) - (1)].t0kn)),  1) ; ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 1801 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_viewt0ype((yyvsp[(1) - (1)].t0kn)), -1) ; ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 1802 "ats_grammar.yats"
    { (yyval.s0rtpol) = s0rtpol_make(s0rt_viewt0ype((yyvsp[(1) - (1)].t0kn)),  1) ; ;}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 1806 "ats_grammar.yats"
    { (yyval.d0atsrtcon) = d0atsrtcon_make_none((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 1807 "ats_grammar.yats"
    { (yyval.d0atsrtcon) = d0atsrtcon_make_some((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].s0rt)) ; ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 1811 "ats_grammar.yats"
    { (yyval.d0atsrtconlst) = (yyvsp[(1) - (1)].d0atsrtconlst) ; ;}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 1812 "ats_grammar.yats"
    { (yyval.d0atsrtconlst) = d0atsrtconlst_cons((yyvsp[(1) - (2)].d0atsrtcon), (yyvsp[(2) - (2)].d0atsrtconlst)) ; ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 1816 "ats_grammar.yats"
    { (yyval.d0atsrtconlst) = d0atsrtconlst_nil() ; ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 1817 "ats_grammar.yats"
    { (yyval.d0atsrtconlst) = d0atsrtconlst_cons((yyvsp[(2) - (3)].d0atsrtcon), (yyvsp[(3) - (3)].d0atsrtconlst)) ; ;}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 1821 "ats_grammar.yats"
    { (yyval.d0atsrtdec) = d0atsrtdec_make((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].d0atsrtconlst)) ; ;}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 1825 "ats_grammar.yats"
    { (yyval.d0atsrtdeclst) = d0atsrtdeclst_nil() ; ;}
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 1826 "ats_grammar.yats"
    { (yyval.d0atsrtdeclst) = d0atsrtdeclst_cons((yyvsp[(2) - (3)].d0atsrtdec), (yyvsp[(3) - (3)].d0atsrtdeclst)) ; ;}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 1830 "ats_grammar.yats"
    { (yyval.s0taq) = s0taq_symdot((yyvsp[(1) - (2)].i0de)) ; ;}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 1831 "ats_grammar.yats"
    { (yyval.s0taq) = s0taq_symcolon((yyvsp[(1) - (2)].i0de)) ; ;}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 1832 "ats_grammar.yats"
    { (yyval.s0taq) = s0taq_fildot((yyvsp[(2) - (3)].s0tring)) ; ;}
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 1836 "ats_grammar.yats"
    { (yyval.d0ynq) = d0ynq_symdot((yyvsp[(1) - (2)].i0de)) ; ;}
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 1837 "ats_grammar.yats"
    { (yyval.d0ynq) = d0ynq_symcolon((yyvsp[(1) - (2)].i0de)) ; ;}
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 1838 "ats_grammar.yats"
    { (yyval.d0ynq) = d0ynq_symdot_symcolon ((yyvsp[(1) - (3)].i0de), (yyvsp[(2) - (3)].i0de)) ; ;}
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 1839 "ats_grammar.yats"
    { (yyval.d0ynq) = d0ynq_fildot((yyvsp[(2) - (3)].s0tring)) ; ;}
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 1840 "ats_grammar.yats"
    { (yyval.d0ynq) = d0ynq_fildot_symcolon((yyvsp[(2) - (4)].s0tring), (yyvsp[(3) - (4)].i0de)) ; ;}
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 1844 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 1845 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 1846 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_r0ead((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 1847 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_ampersand((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 1848 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_backslash((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 1849 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_bang((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 1850 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_gt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 1851 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_lt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 1852 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_minusgt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 169:

/* Line 1455 of yacc.c  */
#line 1853 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_tilde((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 1857 "ats_grammar.yats"
    { (yyval.sqi0de) = sqi0de_make_none((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 1858 "ats_grammar.yats"
    { (yyval.sqi0de) = sqi0de_make_some((yyvsp[(1) - (2)].s0taq), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 172:

/* Line 1455 of yacc.c  */
#line 1862 "ats_grammar.yats"
    { (yyval.i0delst) = i0delst_nil() ; ;}
    break;

  case 173:

/* Line 1455 of yacc.c  */
#line 1863 "ats_grammar.yats"
    { (yyval.i0delst) = i0delst_cons((yyvsp[(2) - (3)].i0de), (yyvsp[(3) - (3)].i0delst)) ; ;}
    break;

  case 174:

/* Line 1455 of yacc.c  */
#line 1867 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 1868 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 176:

/* Line 1455 of yacc.c  */
#line 1869 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_backslash((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 1870 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_bang((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 1871 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_eq((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 1872 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_gt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 1873 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_gtlt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 1874 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_lt((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 1875 "ats_grammar.yats"
    { (yyval.i0de) = i0de_make_tilde((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 1879 "ats_grammar.yats"
    { (yyval.dqi0de) = dqi0de_make_none((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 184:

/* Line 1455 of yacc.c  */
#line 1880 "ats_grammar.yats"
    { (yyval.dqi0de) = dqi0de_make_some((yyvsp[(1) - (2)].d0ynq), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 1884 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 1885 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 1889 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 188:

/* Line 1455 of yacc.c  */
#line 1890 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(2) - (2)].i0de) ; ;}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 1894 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 1898 "ats_grammar.yats"
    { (yyval.arrqi0de) = arrqi0de_make_none((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 191:

/* Line 1455 of yacc.c  */
#line 1899 "ats_grammar.yats"
    { (yyval.arrqi0de) = arrqi0de_make_some((yyvsp[(1) - (2)].d0ynq), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 1903 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 1907 "ats_grammar.yats"
    { (yyval.tmpqi0de) = tmpqi0de_make_none((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 1908 "ats_grammar.yats"
    { (yyval.tmpqi0de) = tmpqi0de_make_some((yyvsp[(1) - (2)].d0ynq), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 1912 "ats_grammar.yats"
    { (yyval.s0rtopt) = s0rtopt_none() ; ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 1913 "ats_grammar.yats"
    { (yyval.s0rtopt) = s0rtopt_some((yyvsp[(2) - (2)].s0rt)) ; ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 1917 "ats_grammar.yats"
    { (yyval.s0arg) = s0arg_make((yyvsp[(1) - (2)].i0de), (yyvsp[(2) - (2)].s0rtopt)) ; ;}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 1921 "ats_grammar.yats"
    { (yyval.s0arglst) = s0arglst_nil() ; ;}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 1922 "ats_grammar.yats"
    { (yyval.s0arglst) = s0arglst_cons((yyvsp[(1) - (2)].s0arg), (yyvsp[(2) - (2)].s0arglst)) ; ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 1926 "ats_grammar.yats"
    { (yyval.s0arglst) = s0arglst_nil() ; ;}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 1927 "ats_grammar.yats"
    { (yyval.s0arglst) = s0arglst_cons((yyvsp[(2) - (3)].s0arg), (yyvsp[(3) - (3)].s0arglst)) ; ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 1931 "ats_grammar.yats"
    { (yyval.s0arglstlst) = s0arglstlst_nil() ; ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 1932 "ats_grammar.yats"
    { (yyval.s0arglstlst) = s0arglstlst_cons_ide((yyvsp[(1) - (2)].i0de), (yyvsp[(2) - (2)].s0arglstlst)) ; ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 1933 "ats_grammar.yats"
    { (yyval.s0arglstlst) = s0arglstlst_cons((yyvsp[(2) - (4)].s0arglst), (yyvsp[(4) - (4)].s0arglstlst)); ;}
    break;

  case 205:

/* Line 1455 of yacc.c  */
#line 1937 "ats_grammar.yats"
    { (yyval.s0arglst) = s0arglst_nil() ; ;}
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 1938 "ats_grammar.yats"
    { (yyval.s0arglst) = s0arglst_cons((yyvsp[(1) - (2)].s0arg), (yyvsp[(2) - (2)].s0arglst)) ; ;}
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 1942 "ats_grammar.yats"
    { (yyval.s0arglst) = s0arglst_nil() ; ;}
    break;

  case 208:

/* Line 1455 of yacc.c  */
#line 1943 "ats_grammar.yats"
    { (yyval.s0arglst) = s0arglst_cons((yyvsp[(2) - (3)].s0arg), (yyvsp[(3) - (3)].s0arglst)) ; ;}
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 1947 "ats_grammar.yats"
    { (yyval.s0arglstlst) = s0arglstlst_nil() ; ;}
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 1948 "ats_grammar.yats"
    { (yyval.s0arglstlst) = s0arglstlst_cons((yyvsp[(2) - (4)].s0arglst), (yyvsp[(4) - (4)].s0arglstlst)) ; ;}
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 1952 "ats_grammar.yats"
    { (yyval.sp0at) = sp0at_con((yyvsp[(1) - (4)].sqi0de), (yyvsp[(3) - (4)].s0arglst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 1956 "ats_grammar.yats"
    { (yyval.s0exp) = (yyvsp[(1) - (1)].s0exp) ; ;}
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 1957 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_extern((yyvsp[(1) - (1)].s0expext)) ; ;}
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 1958 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_ann((yyvsp[(1) - (3)].s0exp), (yyvsp[(3) - (3)].s0rt)) ; ;}
    break;

  case 215:

/* Line 1455 of yacc.c  */
#line 1959 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_lams((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0arglstlst), (yyvsp[(3) - (5)].s0rtopt), (yyvsp[(5) - (5)].s0exp)) ; ;}
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 1963 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_char((yyvsp[(1) - (1)].c0har)) ; ;}
    break;

  case 217:

/* Line 1455 of yacc.c  */
#line 1964 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_int((yyvsp[(1) - (1)].i0nt)) ; ;}
    break;

  case 218:

/* Line 1455 of yacc.c  */
#line 1965 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_intsp_err((yyvsp[(1) - (1)].i0ntsp)) ; ;}
    break;

  case 219:

/* Line 1455 of yacc.c  */
#line 1966 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_ide((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 220:

/* Line 1455 of yacc.c  */
#line 1967 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_opide((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 221:

/* Line 1455 of yacc.c  */
#line 1968 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_qid((yyvsp[(1) - (2)].s0taq), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 222:

/* Line 1455 of yacc.c  */
#line 1969 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_list((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 223:

/* Line 1455 of yacc.c  */
#line 1970 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_list2((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0explst), (yyvsp[(4) - (5)].s0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 224:

/* Line 1455 of yacc.c  */
#line 1971 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tytup(0, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 225:

/* Line 1455 of yacc.c  */
#line 1972 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tytup(1, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 226:

/* Line 1455 of yacc.c  */
#line 1973 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tytup(2, (yyvsp[(1) - (4)].t0kn), (yyvsp[(3) - (4)].s0explst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 227:

/* Line 1455 of yacc.c  */
#line 1974 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tytup(3, (yyvsp[(1) - (4)].t0kn), (yyvsp[(3) - (4)].s0explst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 228:

/* Line 1455 of yacc.c  */
#line 1975 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tytup2(0, (yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0explst), (yyvsp[(4) - (5)].s0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 229:

/* Line 1455 of yacc.c  */
#line 1976 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tytup2(1, (yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0explst), (yyvsp[(4) - (5)].s0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 230:

/* Line 1455 of yacc.c  */
#line 1977 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tytup2(2, (yyvsp[(1) - (6)].t0kn), (yyvsp[(3) - (6)].s0explst), (yyvsp[(5) - (6)].s0explst), (yyvsp[(6) - (6)].t0kn)) ; ;}
    break;

  case 231:

/* Line 1455 of yacc.c  */
#line 1978 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tytup2(3, (yyvsp[(1) - (6)].t0kn), (yyvsp[(3) - (6)].s0explst), (yyvsp[(5) - (6)].s0explst), (yyvsp[(6) - (6)].t0kn)) ; ;}
    break;

  case 232:

/* Line 1455 of yacc.c  */
#line 1979 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tyrec(0, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].labs0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 233:

/* Line 1455 of yacc.c  */
#line 1980 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tyrec(1, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].labs0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 234:

/* Line 1455 of yacc.c  */
#line 1981 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tyrec(2, (yyvsp[(1) - (4)].t0kn), (yyvsp[(3) - (4)].labs0explst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 235:

/* Line 1455 of yacc.c  */
#line 1982 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tyrec(3, (yyvsp[(1) - (4)].t0kn), (yyvsp[(3) - (4)].labs0explst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 236:

/* Line 1455 of yacc.c  */
#line 1983 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tyrec_ext((yyvsp[(1) - (6)].t0kn), (yyvsp[(2) - (6)].s0tring), (yyvsp[(5) - (6)].labs0explst), (yyvsp[(6) - (6)].t0kn)) ; ;}
    break;

  case 237:

/* Line 1455 of yacc.c  */
#line 1984 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_tyarr((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0exp), (yyvsp[(5) - (5)].s0arrind)) ; ;}
    break;

  case 238:

/* Line 1455 of yacc.c  */
#line 1985 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_imp((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].e0fftaglst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 239:

/* Line 1455 of yacc.c  */
#line 1986 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_imp_emp((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 240:

/* Line 1455 of yacc.c  */
#line 1987 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_uni((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0qualst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 241:

/* Line 1455 of yacc.c  */
#line 1988 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_exi((yyvsp[(1) - (3)].t0kn), 0/*funres*/, (yyvsp[(2) - (3)].s0qualst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 242:

/* Line 1455 of yacc.c  */
#line 1989 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_exi((yyvsp[(1) - (3)].t0kn), 1/*funres*/, (yyvsp[(2) - (3)].s0qualst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 243:

/* Line 1455 of yacc.c  */
#line 1993 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_app((yyvsp[(1) - (2)].s0exp), (yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 244:

/* Line 1455 of yacc.c  */
#line 1994 "ats_grammar.yats"
    { (yyval.s0exp) = (yyvsp[(1) - (1)].s0exp) ; ;}
    break;

  case 245:

/* Line 1455 of yacc.c  */
#line 1998 "ats_grammar.yats"
    { (yyval.s0expext) = s0expext_nam((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].s0tring)) ; ;}
    break;

  case 246:

/* Line 1455 of yacc.c  */
#line 1999 "ats_grammar.yats"
    { (yyval.s0expext) = s0expext_app((yyvsp[(1) - (2)].s0expext), (yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 247:

/* Line 1455 of yacc.c  */
#line 2003 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_none () ; ;}
    break;

  case 248:

/* Line 1455 of yacc.c  */
#line 2004 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_some ((yyvsp[(2) - (3)].s0exp)) ; ;}
    break;

  case 249:

/* Line 1455 of yacc.c  */
#line 2005 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_some ((yyvsp[(2) - (3)].s0exp)) ; ;}
    break;

  case 250:

/* Line 1455 of yacc.c  */
#line 2009 "ats_grammar.yats"
    { (yyval.s0arrind) = s0arrind_make_sing((yyvsp[(1) - (2)].s0explst), (yyvsp[(2) - (2)].t0kn)) ; ;}
    break;

  case 251:

/* Line 1455 of yacc.c  */
#line 2010 "ats_grammar.yats"
    { (yyval.s0arrind) = s0arrind_make_cons((yyvsp[(1) - (4)].s0explst), (yyvsp[(4) - (4)].s0arrind)) ; ;}
    break;

  case 252:

/* Line 1455 of yacc.c  */
#line 2014 "ats_grammar.yats"
    { (yyval.s0qua) = s0qua_prop((yyvsp[(1) - (1)].s0exp)) ; ;}
    break;

  case 253:

/* Line 1455 of yacc.c  */
#line 2015 "ats_grammar.yats"
    { (yyval.s0qua) = s0qua_vars((yyvsp[(1) - (4)].i0de), (yyvsp[(2) - (4)].i0delst), (yyvsp[(4) - (4)].s0rtext)) ; ;}
    break;

  case 254:

/* Line 1455 of yacc.c  */
#line 2019 "ats_grammar.yats"
    { (yyval.s0qualst) = s0qualst_nil() ; ;}
    break;

  case 255:

/* Line 1455 of yacc.c  */
#line 2020 "ats_grammar.yats"
    { (yyval.s0qualst) = s0qualst_cons((yyvsp[(1) - (2)].s0qua), (yyvsp[(2) - (2)].s0qualst)) ; ;}
    break;

  case 256:

/* Line 1455 of yacc.c  */
#line 2024 "ats_grammar.yats"
    { (yyval.s0qualst) = s0qualst_nil() ; ;}
    break;

  case 257:

/* Line 1455 of yacc.c  */
#line 2025 "ats_grammar.yats"
    { (yyval.s0qualst) = s0qualst_cons((yyvsp[(2) - (3)].s0qua), (yyvsp[(3) - (3)].s0qualst)) ; ;}
    break;

  case 258:

/* Line 1455 of yacc.c  */
#line 2026 "ats_grammar.yats"
    { (yyval.s0qualst) = s0qualst_cons((yyvsp[(2) - (3)].s0qua), (yyvsp[(3) - (3)].s0qualst)) ; ;}
    break;

  case 259:

/* Line 1455 of yacc.c  */
#line 2030 "ats_grammar.yats"
    { (yyval.s0rtext) = s0rtext_srt((yyvsp[(1) - (1)].s0rt)) ; ;}
    break;

  case 260:

/* Line 1455 of yacc.c  */
#line 2031 "ats_grammar.yats"
    { (yyval.s0rtext) = s0rtext_sub((yyvsp[(1) - (8)].t0kn), (yyvsp[(2) - (8)].i0de), (yyvsp[(4) - (8)].s0rtext), (yyvsp[(6) - (8)].s0exp), (yyvsp[(7) - (8)].s0explst), (yyvsp[(8) - (8)].t0kn)) ; ;}
    break;

  case 261:

/* Line 1455 of yacc.c  */
#line 2035 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_nil() ; ;}
    break;

  case 262:

/* Line 1455 of yacc.c  */
#line 2036 "ats_grammar.yats"
    { (yyval.s0explst) = (yyvsp[(1) - (1)].s0explst) ; ;}
    break;

  case 263:

/* Line 1455 of yacc.c  */
#line 2040 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_nil() ; ;}
    break;

  case 264:

/* Line 1455 of yacc.c  */
#line 2041 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_cons((yyvsp[(2) - (3)].s0exp), (yyvsp[(3) - (3)].s0explst)) ; ;}
    break;

  case 265:

/* Line 1455 of yacc.c  */
#line 2042 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_cons((yyvsp[(2) - (3)].s0exp), (yyvsp[(3) - (3)].s0explst)) ; ;}
    break;

  case 266:

/* Line 1455 of yacc.c  */
#line 2046 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_nil() ; ;}
    break;

  case 267:

/* Line 1455 of yacc.c  */
#line 2047 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_cons((yyvsp[(2) - (3)].s0exp), (yyvsp[(3) - (3)].s0explst)) ; ;}
    break;

  case 268:

/* Line 1455 of yacc.c  */
#line 2051 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_cons((yyvsp[(1) - (2)].s0exp), (yyvsp[(2) - (2)].s0explst)) ; ;}
    break;

  case 269:

/* Line 1455 of yacc.c  */
#line 2055 "ats_grammar.yats"
    { (yyval.labs0explst) = labs0explst_nil() ; ;}
    break;

  case 270:

/* Line 1455 of yacc.c  */
#line 2056 "ats_grammar.yats"
    { (yyval.labs0explst) = labs0explst_cons((yyvsp[(1) - (4)].l0ab), (yyvsp[(3) - (4)].s0exp), (yyvsp[(4) - (4)].labs0explst)) ; ;}
    break;

  case 271:

/* Line 1455 of yacc.c  */
#line 2060 "ats_grammar.yats"
    { (yyval.labs0explst) = labs0explst_nil() ; ;}
    break;

  case 272:

/* Line 1455 of yacc.c  */
#line 2061 "ats_grammar.yats"
    { (yyval.labs0explst) = labs0explst_cons((yyvsp[(2) - (5)].l0ab), (yyvsp[(4) - (5)].s0exp), (yyvsp[(5) - (5)].labs0explst)) ; ;}
    break;

  case 273:

/* Line 1455 of yacc.c  */
#line 2065 "ats_grammar.yats"
    { (yyval.s0exp) = (yyvsp[(1) - (1)].s0exp) ; ;}
    break;

  case 274:

/* Line 1455 of yacc.c  */
#line 2066 "ats_grammar.yats"
    { (yyval.s0exp) = s0exp_app((yyvsp[(1) - (2)].s0exp), (yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 275:

/* Line 1455 of yacc.c  */
#line 2070 "ats_grammar.yats"
    { (yyval.s0exp) = (yyvsp[(1) - (1)].s0exp) ; ;}
    break;

  case 276:

/* Line 1455 of yacc.c  */
#line 2074 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_nil() ; ;}
    break;

  case 277:

/* Line 1455 of yacc.c  */
#line 2075 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_cons((yyvsp[(1) - (2)].s0exp), (yyvsp[(2) - (2)].s0explst)) ; ;}
    break;

  case 278:

/* Line 1455 of yacc.c  */
#line 2079 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_nil() ; ;}
    break;

  case 279:

/* Line 1455 of yacc.c  */
#line 2080 "ats_grammar.yats"
    { (yyval.s0explst) = s0explst_cons((yyvsp[(2) - (3)].s0exp), (yyvsp[(3) - (3)].s0explst)) ; ;}
    break;

  case 280:

/* Line 1455 of yacc.c  */
#line 2084 "ats_grammar.yats"
    { (yyval.t1mps0explstlst) = gtlt_t1mps0expseqseq_nil() ; ;}
    break;

  case 281:

/* Line 1455 of yacc.c  */
#line 2085 "ats_grammar.yats"
    { (yyval.t1mps0explstlst) = gtlt_t1mps0expseqseq_cons_tok((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0explst), (yyvsp[(3) - (3)].t1mps0explstlst)) ; ;}
    break;

  case 282:

/* Line 1455 of yacc.c  */
#line 2089 "ats_grammar.yats"
    { (yyval.impqi0de) = impqi0de_make_none((yyvsp[(1) - (1)].dqi0de)) ; ;}
    break;

  case 283:

/* Line 1455 of yacc.c  */
#line 2090 "ats_grammar.yats"
    { (yyval.impqi0de) = impqi0de_make_some((yyvsp[(1) - (4)].tmpqi0de), (yyvsp[(2) - (4)].s0explst), (yyvsp[(3) - (4)].t1mps0explstlst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 284:

/* Line 1455 of yacc.c  */
#line 2094 "ats_grammar.yats"
    { (yyval.s0rtdef) = s0rtdef_make((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].s0rtext)) ; ;}
    break;

  case 285:

/* Line 1455 of yacc.c  */
#line 2098 "ats_grammar.yats"
    { (yyval.s0rtdeflst) = s0rtdeflst_nil() ; ;}
    break;

  case 286:

/* Line 1455 of yacc.c  */
#line 2099 "ats_grammar.yats"
    { (yyval.s0rtdeflst) = s0rtdeflst_cons((yyvsp[(2) - (3)].s0rtdef), (yyvsp[(3) - (3)].s0rtdeflst)) ; ;}
    break;

  case 287:

/* Line 1455 of yacc.c  */
#line 2103 "ats_grammar.yats"
    { (yyval.d0atarg) = d0atarg_srt((yyvsp[(1) - (1)].s0rtpol)) ; ;}
    break;

  case 288:

/* Line 1455 of yacc.c  */
#line 2104 "ats_grammar.yats"
    { (yyval.d0atarg) = d0atarg_id_srt((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].s0rtpol)) ; ;}
    break;

  case 289:

/* Line 1455 of yacc.c  */
#line 2108 "ats_grammar.yats"
    { (yyval.d0atarglst) = d0atarglst_nil() ; ;}
    break;

  case 290:

/* Line 1455 of yacc.c  */
#line 2109 "ats_grammar.yats"
    { (yyval.d0atarglst) = d0atarglst_cons((yyvsp[(1) - (2)].d0atarg), (yyvsp[(2) - (2)].d0atarglst)) ; ;}
    break;

  case 291:

/* Line 1455 of yacc.c  */
#line 2113 "ats_grammar.yats"
    { (yyval.d0atarglst) = d0atarglst_nil() ; ;}
    break;

  case 292:

/* Line 1455 of yacc.c  */
#line 2114 "ats_grammar.yats"
    { (yyval.d0atarglst) = d0atarglst_cons((yyvsp[(2) - (3)].d0atarg), (yyvsp[(3) - (3)].d0atarglst)) ; ;}
    break;

  case 293:

/* Line 1455 of yacc.c  */
#line 2118 "ats_grammar.yats"
    { (yyval.s0tacon) = s0tacon_make_none_none((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 294:

/* Line 1455 of yacc.c  */
#line 2119 "ats_grammar.yats"
    { (yyval.s0tacon) = s0tacon_make_some_none((yyvsp[(1) - (4)].i0de), (yyvsp[(3) - (4)].d0atarglst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 295:

/* Line 1455 of yacc.c  */
#line 2120 "ats_grammar.yats"
    { (yyval.s0tacon) = s0tacon_make_none_some((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].s0exp)) ; ;}
    break;

  case 296:

/* Line 1455 of yacc.c  */
#line 2121 "ats_grammar.yats"
    { (yyval.s0tacon) = s0tacon_make_some_some((yyvsp[(1) - (6)].i0de), (yyvsp[(3) - (6)].d0atarglst), (yyvsp[(6) - (6)].s0exp)) ; ;}
    break;

  case 297:

/* Line 1455 of yacc.c  */
#line 2125 "ats_grammar.yats"
    { (yyval.s0taconlst) = s0taconlst_nil() ; ;}
    break;

  case 298:

/* Line 1455 of yacc.c  */
#line 2126 "ats_grammar.yats"
    { (yyval.s0taconlst) = s0taconlst_cons((yyvsp[(2) - (3)].s0tacon), (yyvsp[(3) - (3)].s0taconlst)) ; ;}
    break;

  case 299:

/* Line 1455 of yacc.c  */
#line 2130 "ats_grammar.yats"
    { (yyval.s0tacst) = s0tacst_make_none((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].s0rt)) ; ;}
    break;

  case 300:

/* Line 1455 of yacc.c  */
#line 2131 "ats_grammar.yats"
    { (yyval.s0tacst) = s0tacst_make_some((yyvsp[(1) - (6)].i0de), (yyvsp[(3) - (6)].d0atarglst), (yyvsp[(6) - (6)].s0rt)) ; ;}
    break;

  case 301:

/* Line 1455 of yacc.c  */
#line 2135 "ats_grammar.yats"
    { (yyval.s0tacstlst) = s0tacstlst_nil() ; ;}
    break;

  case 302:

/* Line 1455 of yacc.c  */
#line 2136 "ats_grammar.yats"
    { (yyval.s0tacstlst) = s0tacstlst_cons((yyvsp[(2) - (3)].s0tacst), (yyvsp[(3) - (3)].s0tacstlst)) ; ;}
    break;

  case 303:

/* Line 1455 of yacc.c  */
#line 2140 "ats_grammar.yats"
    { (yyval.s0tavar) = s0tavar_make((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].s0rt)) ; ;}
    break;

  case 304:

/* Line 1455 of yacc.c  */
#line 2144 "ats_grammar.yats"
    { (yyval.s0tavarlst) = s0tavarlst_nil() ; ;}
    break;

  case 305:

/* Line 1455 of yacc.c  */
#line 2145 "ats_grammar.yats"
    { (yyval.s0tavarlst) = s0tavarlst_cons((yyvsp[(2) - (3)].s0tavar), (yyvsp[(3) - (3)].s0tavarlst)) ; ;}
    break;

  case 306:

/* Line 1455 of yacc.c  */
#line 2149 "ats_grammar.yats"
    { (yyval.s0expdef) = s0expdef_make ((yyvsp[(1) - (5)].i0de), (yyvsp[(2) - (5)].s0arglstlst), (yyvsp[(3) - (5)].s0rtopt), (yyvsp[(5) - (5)].s0exp)) ; ;}
    break;

  case 307:

/* Line 1455 of yacc.c  */
#line 2153 "ats_grammar.yats"
    { (yyval.s0expdeflst) = s0expdeflst_nil() ; ;}
    break;

  case 308:

/* Line 1455 of yacc.c  */
#line 2154 "ats_grammar.yats"
    { (yyval.s0expdeflst) = s0expdeflst_cons((yyvsp[(2) - (3)].s0expdef), (yyvsp[(3) - (3)].s0expdeflst)) ; ;}
    break;

  case 309:

/* Line 1455 of yacc.c  */
#line 2158 "ats_grammar.yats"
    { (yyval.s0aspdec) = s0aspdec_make((yyvsp[(1) - (5)].sqi0de), (yyvsp[(2) - (5)].s0arglstlst), (yyvsp[(3) - (5)].s0rtopt), (yyvsp[(5) - (5)].s0exp)) ; ;}
    break;

  case 310:

/* Line 1455 of yacc.c  */
#line 2162 "ats_grammar.yats"
    { (yyval.s0qualstlst) = s0qualstlst_nil() ; ;}
    break;

  case 311:

/* Line 1455 of yacc.c  */
#line 2163 "ats_grammar.yats"
    { (yyval.s0qualstlst) = s0qualstlst_cons((yyvsp[(2) - (4)].s0qualst), (yyvsp[(4) - (4)].s0qualstlst)) ; ;}
    break;

  case 312:

/* Line 1455 of yacc.c  */
#line 2167 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_none() ; ;}
    break;

  case 313:

/* Line 1455 of yacc.c  */
#line 2168 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_some(s0exp_list((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0explst), (yyvsp[(3) - (3)].t0kn))) ; ;}
    break;

  case 314:

/* Line 1455 of yacc.c  */
#line 2172 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_none() ; ;}
    break;

  case 315:

/* Line 1455 of yacc.c  */
#line 2173 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_some((yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 316:

/* Line 1455 of yacc.c  */
#line 2177 "ats_grammar.yats"
    { (yyval.d0atcon) = d0atcon_make((yyvsp[(1) - (4)].s0qualstlst), (yyvsp[(2) - (4)].i0de), (yyvsp[(3) - (4)].s0expopt), (yyvsp[(4) - (4)].s0expopt)) ; ;}
    break;

  case 317:

/* Line 1455 of yacc.c  */
#line 2181 "ats_grammar.yats"
    { (yyval.d0atconlst) = (yyvsp[(1) - (1)].d0atconlst) ; ;}
    break;

  case 318:

/* Line 1455 of yacc.c  */
#line 2182 "ats_grammar.yats"
    { (yyval.d0atconlst) = d0atconlst_cons((yyvsp[(1) - (2)].d0atcon), (yyvsp[(2) - (2)].d0atconlst)) ; ;}
    break;

  case 319:

/* Line 1455 of yacc.c  */
#line 2186 "ats_grammar.yats"
    { (yyval.d0atconlst) = d0atconlst_nil() ; ;}
    break;

  case 320:

/* Line 1455 of yacc.c  */
#line 2187 "ats_grammar.yats"
    { (yyval.d0atconlst) = d0atconlst_cons((yyvsp[(2) - (3)].d0atcon), (yyvsp[(3) - (3)].d0atconlst)) ; ;}
    break;

  case 321:

/* Line 1455 of yacc.c  */
#line 2191 "ats_grammar.yats"
    { (yyval.d0atdec) = d0atdec_make_none((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].d0atconlst)) ; ;}
    break;

  case 322:

/* Line 1455 of yacc.c  */
#line 2192 "ats_grammar.yats"
    { (yyval.d0atdec) = d0atdec_make_some((yyvsp[(1) - (6)].i0de), (yyvsp[(3) - (6)].d0atarglst), (yyvsp[(4) - (6)].t0kn), (yyvsp[(6) - (6)].d0atconlst)) ; ;}
    break;

  case 323:

/* Line 1455 of yacc.c  */
#line 2196 "ats_grammar.yats"
    { (yyval.d0atdeclst) = d0atdeclst_nil() ; ;}
    break;

  case 324:

/* Line 1455 of yacc.c  */
#line 2197 "ats_grammar.yats"
    { (yyval.d0atdeclst) = d0atdeclst_cons((yyvsp[(2) - (3)].d0atdec), (yyvsp[(3) - (3)].d0atdeclst)) ; ;}
    break;

  case 325:

/* Line 1455 of yacc.c  */
#line 2201 "ats_grammar.yats"
    { (yyval.s0expdeflst) = s0expdeflst_nil() ; ;}
    break;

  case 326:

/* Line 1455 of yacc.c  */
#line 2202 "ats_grammar.yats"
    { (yyval.s0expdeflst) = s0expdeflst_cons((yyvsp[(2) - (3)].s0expdef), (yyvsp[(3) - (3)].s0expdeflst)) ; ;}
    break;

  case 327:

/* Line 1455 of yacc.c  */
#line 2206 "ats_grammar.yats"
    { (yyval.e0xndec) = e0xndec_make((yyvsp[(1) - (3)].s0qualstlst), (yyvsp[(2) - (3)].i0de), (yyvsp[(3) - (3)].s0expopt)) ; ;}
    break;

  case 328:

/* Line 1455 of yacc.c  */
#line 2210 "ats_grammar.yats"
    { (yyval.e0xndeclst) = e0xndeclst_nil() ; ;}
    break;

  case 329:

/* Line 1455 of yacc.c  */
#line 2211 "ats_grammar.yats"
    { (yyval.e0xndeclst) = e0xndeclst_cons((yyvsp[(2) - (3)].e0xndec), (yyvsp[(3) - (3)].e0xndeclst)) ; ;}
    break;

  case 330:

/* Line 1455 of yacc.c  */
#line 2215 "ats_grammar.yats"
    { (yyval.p0arg) = p0arg_make_none((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 331:

/* Line 1455 of yacc.c  */
#line 2216 "ats_grammar.yats"
    { (yyval.p0arg) = p0arg_make_some((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].s0exp)) ; ;}
    break;

  case 332:

/* Line 1455 of yacc.c  */
#line 2220 "ats_grammar.yats"
    { (yyval.p0arglst) = p0arglst_nil() ; ;}
    break;

  case 333:

/* Line 1455 of yacc.c  */
#line 2221 "ats_grammar.yats"
    { (yyval.p0arglst) = p0arglst_cons((yyvsp[(1) - (2)].p0arg), (yyvsp[(2) - (2)].p0arglst)) ; ;}
    break;

  case 334:

/* Line 1455 of yacc.c  */
#line 2225 "ats_grammar.yats"
    { (yyval.p0arglst) = p0arglst_nil() ; ;}
    break;

  case 335:

/* Line 1455 of yacc.c  */
#line 2226 "ats_grammar.yats"
    { (yyval.p0arglst) = p0arglst_cons((yyvsp[(2) - (3)].p0arg), (yyvsp[(3) - (3)].p0arglst)) ; ;}
    break;

  case 336:

/* Line 1455 of yacc.c  */
#line 2230 "ats_grammar.yats"
    { (yyval.d0arg) = d0arg_var((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 337:

/* Line 1455 of yacc.c  */
#line 2231 "ats_grammar.yats"
    { (yyval.d0arg) = d0arg_dyn((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0arglst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 338:

/* Line 1455 of yacc.c  */
#line 2232 "ats_grammar.yats"
    { (yyval.d0arg) = d0arg_dyn2((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].p0arglst), (yyvsp[(4) - (5)].p0arglst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 339:

/* Line 1455 of yacc.c  */
#line 2233 "ats_grammar.yats"
    { (yyval.d0arg) = d0arg_sta((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0qualst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 340:

/* Line 1455 of yacc.c  */
#line 2237 "ats_grammar.yats"
    { (yyval.d0arglst) = d0arglst_nil() ; ;}
    break;

  case 341:

/* Line 1455 of yacc.c  */
#line 2238 "ats_grammar.yats"
    { (yyval.d0arglst) = d0arglst_cons((yyvsp[(1) - (2)].d0arg), (yyvsp[(2) - (2)].d0arglst)) ; ;}
    break;

  case 342:

/* Line 1455 of yacc.c  */
#line 2242 "ats_grammar.yats"
    { (yyval.extnamopt) = extnamopt_none() ; ;}
    break;

  case 343:

/* Line 1455 of yacc.c  */
#line 2243 "ats_grammar.yats"
    { (yyval.extnamopt) = extnamopt_some((yyvsp[(2) - (2)].s0tring)) ; ;}
    break;

  case 344:

/* Line 1455 of yacc.c  */
#line 2247 "ats_grammar.yats"
    { (yyval.d0cstdec) = d0cstdec_make((yyvsp[(1) - (5)].i0de), (yyvsp[(2) - (5)].d0arglst), (yyvsp[(3) - (5)].e0fftaglstopt), (yyvsp[(4) - (5)].s0exp), (yyvsp[(5) - (5)].extnamopt)) ; ;}
    break;

  case 345:

/* Line 1455 of yacc.c  */
#line 2251 "ats_grammar.yats"
    { (yyval.d0cstdeclst) = d0cstdeclst_nil() ; ;}
    break;

  case 346:

/* Line 1455 of yacc.c  */
#line 2252 "ats_grammar.yats"
    { (yyval.d0cstdeclst) = d0cstdeclst_cons((yyvsp[(2) - (3)].d0cstdec), (yyvsp[(3) - (3)].d0cstdeclst)) ; ;}
    break;

  case 347:

/* Line 1455 of yacc.c  */
#line 2256 "ats_grammar.yats"
    { (yyval.s0vararg) = s0vararg_one() ; ;}
    break;

  case 348:

/* Line 1455 of yacc.c  */
#line 2257 "ats_grammar.yats"
    { (yyval.s0vararg) = s0vararg_all() ; ;}
    break;

  case 349:

/* Line 1455 of yacc.c  */
#line 2258 "ats_grammar.yats"
    { (yyval.s0vararg) = s0vararg_seq((yyvsp[(1) - (1)].s0arglst)) ; ;}
    break;

  case 350:

/* Line 1455 of yacc.c  */
#line 2262 "ats_grammar.yats"
    { (yyval.s0exparg) = s0exparg_one() ; ;}
    break;

  case 351:

/* Line 1455 of yacc.c  */
#line 2263 "ats_grammar.yats"
    { (yyval.s0exparg) = s0exparg_all() ; ;}
    break;

  case 352:

/* Line 1455 of yacc.c  */
#line 2264 "ats_grammar.yats"
    { (yyval.s0exparg) = s0exparg_seq((yyvsp[(1) - (1)].s0explst)) ; ;}
    break;

  case 353:

/* Line 1455 of yacc.c  */
#line 2268 "ats_grammar.yats"
    { (yyval.s0elop) = s0elop_make (0, (yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 354:

/* Line 1455 of yacc.c  */
#line 2269 "ats_grammar.yats"
    { (yyval.s0elop) = s0elop_make (1, (yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 355:

/* Line 1455 of yacc.c  */
#line 2273 "ats_grammar.yats"
    { (yyval.witht0ype) = witht0ype_none() ; ;}
    break;

  case 356:

/* Line 1455 of yacc.c  */
#line 2274 "ats_grammar.yats"
    { (yyval.witht0ype) = witht0ype_prop((yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 357:

/* Line 1455 of yacc.c  */
#line 2275 "ats_grammar.yats"
    { (yyval.witht0ype) = witht0ype_type((yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 358:

/* Line 1455 of yacc.c  */
#line 2276 "ats_grammar.yats"
    { (yyval.witht0ype) = witht0ype_view((yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 359:

/* Line 1455 of yacc.c  */
#line 2277 "ats_grammar.yats"
    { (yyval.witht0ype) = witht0ype_viewtype((yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 360:

/* Line 1455 of yacc.c  */
#line 2281 "ats_grammar.yats"
    { (yyval.p0at) = p0at_apps((yyvsp[(1) - (2)].p0at), (yyvsp[(2) - (2)].p0atlst)) ; ;}
    break;

  case 361:

/* Line 1455 of yacc.c  */
#line 2282 "ats_grammar.yats"
    { (yyval.p0at) = p0at_ann((yyvsp[(1) - (3)].p0at), (yyvsp[(3) - (3)].s0exp)) ; ;}
    break;

  case 362:

/* Line 1455 of yacc.c  */
#line 2283 "ats_grammar.yats"
    { (yyval.p0at) = p0at_as((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].p0at)) ; ;}
    break;

  case 363:

/* Line 1455 of yacc.c  */
#line 2284 "ats_grammar.yats"
    { (yyval.p0at) = p0at_refas((yyvsp[(1) - (4)].t0kn), (yyvsp[(2) - (4)].i0de), (yyvsp[(4) - (4)].p0at)) ; ;}
    break;

  case 364:

/* Line 1455 of yacc.c  */
#line 2285 "ats_grammar.yats"
    { (yyval.p0at) = p0at_free((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].p0at)) ; ;}
    break;

  case 365:

/* Line 1455 of yacc.c  */
#line 2289 "ats_grammar.yats"
    { (yyval.p0at) = p0at_char((yyvsp[(1) - (1)].c0har)) ; ;}
    break;

  case 366:

/* Line 1455 of yacc.c  */
#line 2290 "ats_grammar.yats"
    { (yyval.p0at) = p0at_int((yyvsp[(1) - (1)].i0nt)) ; ;}
    break;

  case 367:

/* Line 1455 of yacc.c  */
#line 2291 "ats_grammar.yats"
    { (yyval.p0at) = p0at_float((yyvsp[(1) - (1)].f0loat)) ; ;}
    break;

  case 368:

/* Line 1455 of yacc.c  */
#line 2292 "ats_grammar.yats"
    { (yyval.p0at) = p0at_string((yyvsp[(1) - (1)].s0tring)) ; ;}
    break;

  case 369:

/* Line 1455 of yacc.c  */
#line 2293 "ats_grammar.yats"
    { (yyval.p0at) = p0at_ide((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 370:

/* Line 1455 of yacc.c  */
#line 2294 "ats_grammar.yats"
    { (yyval.p0at) = p0at_ref((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 371:

/* Line 1455 of yacc.c  */
#line 2295 "ats_grammar.yats"
    { (yyval.p0at) = p0at_opide((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 372:

/* Line 1455 of yacc.c  */
#line 2296 "ats_grammar.yats"
    { (yyval.p0at) = p0at_qid((yyvsp[(1) - (2)].d0ynq), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 373:

/* Line 1455 of yacc.c  */
#line 2297 "ats_grammar.yats"
    { (yyval.p0at) = p0at_list((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0atlst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 374:

/* Line 1455 of yacc.c  */
#line 2298 "ats_grammar.yats"
    { (yyval.p0at) = p0at_list2((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].p0atlst), (yyvsp[(4) - (5)].p0atlst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 375:

/* Line 1455 of yacc.c  */
#line 2299 "ats_grammar.yats"
    { (yyval.p0at) = p0at_lst((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0atlst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 376:

/* Line 1455 of yacc.c  */
#line 2300 "ats_grammar.yats"
    { (yyval.p0at) = p0at_tup(0, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0atlst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 377:

/* Line 1455 of yacc.c  */
#line 2301 "ats_grammar.yats"
    { (yyval.p0at) = p0at_tup(1, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0atlst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 378:

/* Line 1455 of yacc.c  */
#line 2302 "ats_grammar.yats"
    { (yyval.p0at) = p0at_tup2(0, (yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].p0atlst), (yyvsp[(4) - (5)].p0atlst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 379:

/* Line 1455 of yacc.c  */
#line 2303 "ats_grammar.yats"
    { (yyval.p0at) = p0at_tup2(1, (yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].p0atlst), (yyvsp[(4) - (5)].p0atlst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 380:

/* Line 1455 of yacc.c  */
#line 2304 "ats_grammar.yats"
    { (yyval.p0at) = p0at_rec(0, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].labp0atlst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 381:

/* Line 1455 of yacc.c  */
#line 2305 "ats_grammar.yats"
    { (yyval.p0at) = p0at_rec(1, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].labp0atlst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 382:

/* Line 1455 of yacc.c  */
#line 2306 "ats_grammar.yats"
    { (yyval.p0at) = p0at_exist((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0arglst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 383:

/* Line 1455 of yacc.c  */
#line 2310 "ats_grammar.yats"
    { (yyval.p0at) = (yyvsp[(1) - (1)].p0at) ; ;}
    break;

  case 384:

/* Line 1455 of yacc.c  */
#line 2311 "ats_grammar.yats"
    { (yyval.p0at) = p0at_svararg((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0vararg), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 385:

/* Line 1455 of yacc.c  */
#line 2315 "ats_grammar.yats"
    { (yyval.p0atlst) = p0atlst_nil() ; ;}
    break;

  case 386:

/* Line 1455 of yacc.c  */
#line 2316 "ats_grammar.yats"
    { (yyval.p0atlst) = p0atlst_cons((yyvsp[(1) - (2)].p0at), (yyvsp[(2) - (2)].p0atlst)) ; ;}
    break;

  case 387:

/* Line 1455 of yacc.c  */
#line 2320 "ats_grammar.yats"
    { (yyval.p0atlst) = p0atlst_nil() ; ;}
    break;

  case 388:

/* Line 1455 of yacc.c  */
#line 2321 "ats_grammar.yats"
    { (yyval.p0atlst) = p0atlst_cons((yyvsp[(1) - (2)].p0at), (yyvsp[(2) - (2)].p0atlst)) ; ;}
    break;

  case 389:

/* Line 1455 of yacc.c  */
#line 2325 "ats_grammar.yats"
    { (yyval.p0atlst) = p0atlst_nil() ; ;}
    break;

  case 390:

/* Line 1455 of yacc.c  */
#line 2326 "ats_grammar.yats"
    { (yyval.p0atlst) = p0atlst_cons((yyvsp[(2) - (3)].p0at), (yyvsp[(3) - (3)].p0atlst)) ; ;}
    break;

  case 391:

/* Line 1455 of yacc.c  */
#line 2330 "ats_grammar.yats"
    { (yyval.labp0atlst) = labp0atlst_dot() ; ;}
    break;

  case 392:

/* Line 1455 of yacc.c  */
#line 2331 "ats_grammar.yats"
    { (yyval.labp0atlst) = labp0atlst_cons((yyvsp[(1) - (4)].l0ab), (yyvsp[(3) - (4)].p0at), (yyvsp[(4) - (4)].labp0atlst)) ; ;}
    break;

  case 393:

/* Line 1455 of yacc.c  */
#line 2335 "ats_grammar.yats"
    { (yyval.labp0atlst) = labp0atlst_nil() ; ;}
    break;

  case 394:

/* Line 1455 of yacc.c  */
#line 2336 "ats_grammar.yats"
    { (yyval.labp0atlst) = labp0atlst_dot() ; ;}
    break;

  case 395:

/* Line 1455 of yacc.c  */
#line 2337 "ats_grammar.yats"
    { (yyval.labp0atlst) = labp0atlst_cons((yyvsp[(2) - (5)].l0ab), (yyvsp[(4) - (5)].p0at), (yyvsp[(5) - (5)].labp0atlst)) ; ;}
    break;

  case 396:

/* Line 1455 of yacc.c  */
#line 2341 "ats_grammar.yats"
    { (yyval.f0arg) = f0arg_sta1((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0qualst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 397:

/* Line 1455 of yacc.c  */
#line 2342 "ats_grammar.yats"
    { (yyval.f0arg) = f0arg_dyn((yyvsp[(1) - (1)].p0at)) ; ;}
    break;

  case 398:

/* Line 1455 of yacc.c  */
#line 2343 "ats_grammar.yats"
    { (yyval.f0arg) = f0arg_met_some((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 399:

/* Line 1455 of yacc.c  */
#line 2344 "ats_grammar.yats"
    { (yyval.f0arg) = f0arg_met_none((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 400:

/* Line 1455 of yacc.c  */
#line 2348 "ats_grammar.yats"
    { (yyval.f0arglst) = f0arglst_nil() ; ;}
    break;

  case 401:

/* Line 1455 of yacc.c  */
#line 2349 "ats_grammar.yats"
    { (yyval.f0arglst) = f0arglst_cons((yyvsp[(1) - (2)].f0arg), (yyvsp[(2) - (2)].f0arglst)) ; ;}
    break;

  case 402:

/* Line 1455 of yacc.c  */
#line 2353 "ats_grammar.yats"
    { (yyval.f0arg) = f0arg_sta2((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0arglst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 403:

/* Line 1455 of yacc.c  */
#line 2354 "ats_grammar.yats"
    { (yyval.f0arg) = f0arg_dyn((yyvsp[(1) - (1)].p0at)) ; ;}
    break;

  case 404:

/* Line 1455 of yacc.c  */
#line 2358 "ats_grammar.yats"
    { (yyval.f0arglst) = f0arglst_nil() ; ;}
    break;

  case 405:

/* Line 1455 of yacc.c  */
#line 2359 "ats_grammar.yats"
    { (yyval.f0arglst) = f0arglst_cons((yyvsp[(1) - (2)].f0arg), (yyvsp[(2) - (2)].f0arglst)) ; ;}
    break;

  case 406:

/* Line 1455 of yacc.c  */
#line 2363 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_apps((yyvsp[(1) - (2)].d0exp), (yyvsp[(2) - (2)].d0explst)) ; ;}
    break;

  case 407:

/* Line 1455 of yacc.c  */
#line 2364 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_ann((yyvsp[(1) - (3)].d0exp), (yyvsp[(3) - (3)].s0exp)) ; ;}
    break;

  case 408:

/* Line 1455 of yacc.c  */
#line 2365 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_if_none((yyvsp[(1) - (4)].ifhead), (yyvsp[(2) - (4)].d0exp), (yyvsp[(4) - (4)].d0exp)) ; ;}
    break;

  case 409:

/* Line 1455 of yacc.c  */
#line 2366 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_if_some((yyvsp[(1) - (6)].ifhead), (yyvsp[(2) - (6)].d0exp), (yyvsp[(4) - (6)].d0exp), (yyvsp[(6) - (6)].d0exp)) ; ;}
    break;

  case 410:

/* Line 1455 of yacc.c  */
#line 2367 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_sif((yyvsp[(1) - (6)].ifhead), (yyvsp[(2) - (6)].s0exp), (yyvsp[(4) - (6)].d0exp), (yyvsp[(6) - (6)].d0exp)) ; ;}
    break;

  case 411:

/* Line 1455 of yacc.c  */
#line 2368 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_caseof((yyvsp[(1) - (4)].casehead), (yyvsp[(2) - (4)].d0exp), (yyvsp[(3) - (4)].t0kn), (yyvsp[(4) - (4)].c0laulst)) ; ;}
    break;

  case 412:

/* Line 1455 of yacc.c  */
#line 2369 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_scaseof((yyvsp[(1) - (4)].casehead), (yyvsp[(2) - (4)].s0exp), (yyvsp[(3) - (4)].t0kn), (yyvsp[(4) - (4)].sc0laulst)) ; ;}
    break;

  case 413:

/* Line 1455 of yacc.c  */
#line 2370 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_lam((yyvsp[(1) - (5)].lamkind), (yyvsp[(2) - (5)].f0arglst), (yyvsp[(3) - (5)].s0expopt), (yyvsp[(4) - (5)].e0fftaglstopt), (yyvsp[(5) - (5)].d0exp) ) ; ;}
    break;

  case 414:

/* Line 1455 of yacc.c  */
#line 2371 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_fix((yyvsp[(1) - (6)].fixkind), (yyvsp[(2) - (6)].i0de), (yyvsp[(3) - (6)].f0arglst), (yyvsp[(4) - (6)].s0expopt), (yyvsp[(5) - (6)].e0fftaglstopt), (yyvsp[(6) - (6)].d0exp)) ; ;}
    break;

  case 415:

/* Line 1455 of yacc.c  */
#line 2372 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_for_itp ((yyvsp[(1) - (3)].loophead), (yyvsp[(2) - (3)].initestpost), (yyvsp[(3) - (3)].d0exp)) ; ;}
    break;

  case 416:

/* Line 1455 of yacc.c  */
#line 2373 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_while ((yyvsp[(1) - (3)].loophead), (yyvsp[(2) - (3)].d0exp), (yyvsp[(3) - (3)].d0exp)) ; ;}
    break;

  case 417:

/* Line 1455 of yacc.c  */
#line 2374 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_raise((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].d0exp)) ; ;}
    break;

  case 418:

/* Line 1455 of yacc.c  */
#line 2375 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_trywith_seq((yyvsp[(1) - (4)].tryhead), (yyvsp[(2) - (4)].d0explst), (yyvsp[(3) - (4)].t0kn), (yyvsp[(4) - (4)].c0laulst)) ; ;}
    break;

  case 419:

/* Line 1455 of yacc.c  */
#line 2376 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_where ((yyvsp[(1) - (5)].d0exp), (yyvsp[(4) - (5)].d0eclst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 420:

/* Line 1455 of yacc.c  */
#line 2380 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_char((yyvsp[(1) - (1)].c0har)) ; ;}
    break;

  case 421:

/* Line 1455 of yacc.c  */
#line 2381 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_float((yyvsp[(1) - (1)].f0loat)) ; ;}
    break;

  case 422:

/* Line 1455 of yacc.c  */
#line 2382 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_floatsp((yyvsp[(1) - (1)].f0loatsp)) ; ;}
    break;

  case 423:

/* Line 1455 of yacc.c  */
#line 2383 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_int((yyvsp[(1) - (1)].i0nt)) ; ;}
    break;

  case 424:

/* Line 1455 of yacc.c  */
#line 2384 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_intsp((yyvsp[(1) - (1)].i0ntsp)) ; ;}
    break;

  case 425:

/* Line 1455 of yacc.c  */
#line 2385 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_string((yyvsp[(1) - (1)].s0tring)) ; ;}
    break;

  case 426:

/* Line 1455 of yacc.c  */
#line 2386 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_FILENAME((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 427:

/* Line 1455 of yacc.c  */
#line 2387 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_LOCATION((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 428:

/* Line 1455 of yacc.c  */
#line 2388 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_ide((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 429:

/* Line 1455 of yacc.c  */
#line 2389 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_opide((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 430:

/* Line 1455 of yacc.c  */
#line 2390 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_qid((yyvsp[(1) - (2)].d0ynq), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 431:

/* Line 1455 of yacc.c  */
#line 2391 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_idext((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 432:

/* Line 1455 of yacc.c  */
#line 2392 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_ptrof((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 433:

/* Line 1455 of yacc.c  */
#line 2393 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_loopexn(0, (yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 434:

/* Line 1455 of yacc.c  */
#line 2394 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_loopexn(1, (yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 435:

/* Line 1455 of yacc.c  */
#line 2395 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_foldat((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].d0explst)) ; ;}
    break;

  case 436:

/* Line 1455 of yacc.c  */
#line 2396 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_freeat((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].d0explst)) ; ;}
    break;

  case 437:

/* Line 1455 of yacc.c  */
#line 2397 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_viewat((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 438:

/* Line 1455 of yacc.c  */
#line 2398 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_crypt (-1, (yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 439:

/* Line 1455 of yacc.c  */
#line 2399 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_crypt ( 1, (yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 440:

/* Line 1455 of yacc.c  */
#line 2400 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_delay(0, (yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 441:

/* Line 1455 of yacc.c  */
#line 2401 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_delay(1, (yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 442:

/* Line 1455 of yacc.c  */
#line 2402 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_dynload((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 443:

/* Line 1455 of yacc.c  */
#line 2403 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_effmask_all((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 444:

/* Line 1455 of yacc.c  */
#line 2404 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_effmask_exn((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 445:

/* Line 1455 of yacc.c  */
#line 2405 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_effmask_ntm((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 446:

/* Line 1455 of yacc.c  */
#line 2406 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_effmask_ref((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 447:

/* Line 1455 of yacc.c  */
#line 2407 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_arrinit_none ((yyvsp[(1) - (6)].t0kn), (yyvsp[(2) - (6)].s0exp), (yyvsp[(5) - (6)].d0explst), (yyvsp[(6) - (6)].t0kn)) ; ;}
    break;

  case 448:

/* Line 1455 of yacc.c  */
#line 2408 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_arrinit_some ((yyvsp[(1) - (9)].t0kn), (yyvsp[(2) - (9)].s0exp), (yyvsp[(5) - (9)].d0exp), (yyvsp[(8) - (9)].d0explst), (yyvsp[(9) - (9)].t0kn)) ; ;}
    break;

  case 449:

/* Line 1455 of yacc.c  */
#line 2409 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_arrsize ((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0expopt), (yyvsp[(3) - (5)].t0kn), (yyvsp[(4) - (5)].d0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 450:

/* Line 1455 of yacc.c  */
#line 2410 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_arrsub ((yyvsp[(1) - (2)].arrqi0de), (yyvsp[(2) - (2)].d0arrind)) ; ;}
    break;

  case 451:

/* Line 1455 of yacc.c  */
#line 2411 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_sel_lab ((yyvsp[(1) - (2)].s0elop), (yyvsp[(2) - (2)].l0ab)) ; ;}
    break;

  case 452:

/* Line 1455 of yacc.c  */
#line 2412 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_sel_ind ((yyvsp[(1) - (3)].s0elop), (yyvsp[(3) - (3)].d0arrind)) ; ;}
    break;

  case 453:

/* Line 1455 of yacc.c  */
#line 2413 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_tmpid ((yyvsp[(1) - (4)].tmpqi0de), (yyvsp[(2) - (4)].s0explst), (yyvsp[(3) - (4)].t1mps0explstlst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 454:

/* Line 1455 of yacc.c  */
#line 2414 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_exist ((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0exparg), (yyvsp[(3) - (5)].t0kn), (yyvsp[(4) - (5)].d0exp), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 455:

/* Line 1455 of yacc.c  */
#line 2415 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_list ((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 456:

/* Line 1455 of yacc.c  */
#line 2416 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_list2 ((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].d0explst), (yyvsp[(4) - (5)].d0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 457:

/* Line 1455 of yacc.c  */
#line 2417 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_lst (0, (yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0expopt), (yyvsp[(3) - (5)].t0kn), (yyvsp[(4) - (5)].d0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 458:

/* Line 1455 of yacc.c  */
#line 2418 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_lst (1, (yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].s0expopt), (yyvsp[(3) - (5)].t0kn), (yyvsp[(4) - (5)].d0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 459:

/* Line 1455 of yacc.c  */
#line 2419 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_lst_quote ((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 460:

/* Line 1455 of yacc.c  */
#line 2420 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_seq ((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 461:

/* Line 1455 of yacc.c  */
#line 2421 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_seq ((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 462:

/* Line 1455 of yacc.c  */
#line 2422 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_tup (0, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 463:

/* Line 1455 of yacc.c  */
#line 2423 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_tup (1, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 464:

/* Line 1455 of yacc.c  */
#line 2424 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_tup (2, (yyvsp[(1) - (4)].t0kn), (yyvsp[(3) - (4)].d0explst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 465:

/* Line 1455 of yacc.c  */
#line 2425 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_tup (3, (yyvsp[(1) - (4)].t0kn), (yyvsp[(3) - (4)].d0explst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 466:

/* Line 1455 of yacc.c  */
#line 2426 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_tup2 (0, (yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].d0explst), (yyvsp[(4) - (5)].d0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 467:

/* Line 1455 of yacc.c  */
#line 2427 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_tup2 (1, (yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].d0explst), (yyvsp[(4) - (5)].d0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 468:

/* Line 1455 of yacc.c  */
#line 2428 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_rec (0, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].labd0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 469:

/* Line 1455 of yacc.c  */
#line 2429 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_rec (1, (yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].labd0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 470:

/* Line 1455 of yacc.c  */
#line 2430 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_rec (2, (yyvsp[(1) - (4)].t0kn), (yyvsp[(3) - (4)].labd0explst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 471:

/* Line 1455 of yacc.c  */
#line 2431 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_rec (3, (yyvsp[(1) - (4)].t0kn), (yyvsp[(3) - (4)].labd0explst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 472:

/* Line 1455 of yacc.c  */
#line 2432 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_extval((yyvsp[(1) - (6)].t0kn), (yyvsp[(3) - (6)].s0exp), (yyvsp[(5) - (6)].s0tring), (yyvsp[(6) - (6)].t0kn)) ; ;}
    break;

  case 473:

/* Line 1455 of yacc.c  */
#line 2433 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_macsyn_cross((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0exp), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 474:

/* Line 1455 of yacc.c  */
#line 2434 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_macsyn_decode((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0exp), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 475:

/* Line 1455 of yacc.c  */
#line 2435 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_macsyn_encode_seq((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0explst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 476:

/* Line 1455 of yacc.c  */
#line 2436 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_let_seq((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].d0eclst), (yyvsp[(3) - (5)].t0kn), (yyvsp[(4) - (5)].d0explst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 477:

/* Line 1455 of yacc.c  */
#line 2437 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_decseq((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].d0eclst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 478:

/* Line 1455 of yacc.c  */
#line 2441 "ats_grammar.yats"
    { (yyval.d0exp) = d0exp_sexparg((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0exparg), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 479:

/* Line 1455 of yacc.c  */
#line 2445 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_nil() ; ;}
    break;

  case 480:

/* Line 1455 of yacc.c  */
#line 2446 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_cons((yyvsp[(1) - (2)].d0exp), (yyvsp[(2) - (2)].d0explst)) ;  ;}
    break;

  case 481:

/* Line 1455 of yacc.c  */
#line 2450 "ats_grammar.yats"
    { (yyval.d0exp) = (yyvsp[(1) - (1)].d0exp) ; ;}
    break;

  case 482:

/* Line 1455 of yacc.c  */
#line 2451 "ats_grammar.yats"
    { (yyval.d0exp) = (yyvsp[(1) - (1)].d0exp) ; ;}
    break;

  case 483:

/* Line 1455 of yacc.c  */
#line 2455 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_nil() ; ;}
    break;

  case 484:

/* Line 1455 of yacc.c  */
#line 2456 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_cons((yyvsp[(1) - (2)].d0exp), (yyvsp[(2) - (2)].d0explst)) ; ;}
    break;

  case 485:

/* Line 1455 of yacc.c  */
#line 2460 "ats_grammar.yats"
    { (yyval.d0arrind) = d0arrind_make_sing((yyvsp[(1) - (2)].d0explst), (yyvsp[(2) - (2)].t0kn)) ; ;}
    break;

  case 486:

/* Line 1455 of yacc.c  */
#line 2461 "ats_grammar.yats"
    { (yyval.d0arrind) = d0arrind_make_cons((yyvsp[(1) - (4)].d0explst), (yyvsp[(4) - (4)].d0arrind)) ; ;}
    break;

  case 487:

/* Line 1455 of yacc.c  */
#line 2465 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_none() ; ;}
    break;

  case 488:

/* Line 1455 of yacc.c  */
#line 2466 "ats_grammar.yats"
    { (yyval.s0expopt) = s0expopt_some((yyvsp[(2) - (2)].s0exp)) ; ;}
    break;

  case 489:

/* Line 1455 of yacc.c  */
#line 2470 "ats_grammar.yats"
    { (yyval.e0fftaglstopt) = e0fftaglstopt_none() ; ;}
    break;

  case 490:

/* Line 1455 of yacc.c  */
#line 2471 "ats_grammar.yats"
    { (yyval.e0fftaglstopt) = e0fftaglstopt_some(e0fftaglst_nil()) ; ;}
    break;

  case 491:

/* Line 1455 of yacc.c  */
#line 2472 "ats_grammar.yats"
    { (yyval.e0fftaglstopt) = e0fftaglstopt_some((yyvsp[(2) - (3)].e0fftaglst)) ; ;}
    break;

  case 492:

/* Line 1455 of yacc.c  */
#line 2476 "ats_grammar.yats"
    { (yyval.i0nvresstate) = i0nvresstate_none() ; ;}
    break;

  case 493:

/* Line 1455 of yacc.c  */
#line 2477 "ats_grammar.yats"
    { (yyval.i0nvresstate) = (yyvsp[(1) - (2)].i0nvresstate) ; ;}
    break;

  case 494:

/* Line 1455 of yacc.c  */
#line 2481 "ats_grammar.yats"
    { (yyval.ifhead) = ifhead_make((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0nvresstate)) ; ;}
    break;

  case 495:

/* Line 1455 of yacc.c  */
#line 2485 "ats_grammar.yats"
    { (yyval.ifhead) = ifhead_make((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0nvresstate)) ; ;}
    break;

  case 496:

/* Line 1455 of yacc.c  */
#line 2489 "ats_grammar.yats"
    { (yyval.casehead) = casehead_make(0, (yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0nvresstate)) ; ;}
    break;

  case 497:

/* Line 1455 of yacc.c  */
#line 2490 "ats_grammar.yats"
    { (yyval.casehead) = casehead_make(-1, (yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0nvresstate)) ; ;}
    break;

  case 498:

/* Line 1455 of yacc.c  */
#line 2491 "ats_grammar.yats"
    { (yyval.casehead) = casehead_make(1, (yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0nvresstate)) ; ;}
    break;

  case 499:

/* Line 1455 of yacc.c  */
#line 2495 "ats_grammar.yats"
    { (yyval.casehead) = casehead_make(0, (yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0nvresstate)) ; ;}
    break;

  case 500:

/* Line 1455 of yacc.c  */
#line 2499 "ats_grammar.yats"
    { (yyval.loophead) = loophead_make_none((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 501:

/* Line 1455 of yacc.c  */
#line 2500 "ats_grammar.yats"
    { (yyval.loophead) = loophead_make_some((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].loopi0nv), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 502:

/* Line 1455 of yacc.c  */
#line 2504 "ats_grammar.yats"
    { (yyval.loophead) = loophead_make_some((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].loopi0nv), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 503:

/* Line 1455 of yacc.c  */
#line 2508 "ats_grammar.yats"
    { (yyval.tryhead) = tryhead_make((yyvsp[(1) - (1)].t0kn)) ; ;}
    break;

  case 504:

/* Line 1455 of yacc.c  */
#line 2512 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_nil() ; ;}
    break;

  case 505:

/* Line 1455 of yacc.c  */
#line 2513 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_cons((yyvsp[(1) - (2)].d0exp), (yyvsp[(2) - (2)].d0explst)) ; ;}
    break;

  case 506:

/* Line 1455 of yacc.c  */
#line 2517 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_nil() ; ;}
    break;

  case 507:

/* Line 1455 of yacc.c  */
#line 2518 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_cons((yyvsp[(2) - (3)].d0exp), (yyvsp[(3) - (3)].d0explst)) ; ;}
    break;

  case 508:

/* Line 1455 of yacc.c  */
#line 2522 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_nil() ; ;}
    break;

  case 509:

/* Line 1455 of yacc.c  */
#line 2523 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_sing((yyvsp[(1) - (1)].d0exp)) ; ;}
    break;

  case 510:

/* Line 1455 of yacc.c  */
#line 2524 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_cons((yyvsp[(1) - (3)].d0exp), (yyvsp[(3) - (3)].d0explst)) ; ;}
    break;

  case 511:

/* Line 1455 of yacc.c  */
#line 2528 "ats_grammar.yats"
    { (yyval.d0explst) = d0explst_cons((yyvsp[(1) - (3)].d0exp), (yyvsp[(3) - (3)].d0explst)) ; ;}
    break;

  case 512:

/* Line 1455 of yacc.c  */
#line 2532 "ats_grammar.yats"
    { (yyval.labd0explst) = labd0explst_nil() ; ;}
    break;

  case 513:

/* Line 1455 of yacc.c  */
#line 2533 "ats_grammar.yats"
    { (yyval.labd0explst) = labd0explst_cons((yyvsp[(1) - (4)].l0ab), (yyvsp[(3) - (4)].d0exp), (yyvsp[(4) - (4)].labd0explst)) ; ;}
    break;

  case 514:

/* Line 1455 of yacc.c  */
#line 2537 "ats_grammar.yats"
    { (yyval.labd0explst) = labd0explst_nil() ; ;}
    break;

  case 515:

/* Line 1455 of yacc.c  */
#line 2538 "ats_grammar.yats"
    { (yyval.labd0explst) = labd0explst_cons((yyvsp[(2) - (5)].l0ab), (yyvsp[(4) - (5)].d0exp), (yyvsp[(5) - (5)].labd0explst)) ; ;}
    break;

  case 516:

/* Line 1455 of yacc.c  */
#line 2542 "ats_grammar.yats"
    { (yyval.m0atch) = m0atch_make_none ((yyvsp[(1) - (1)].d0exp)) ; ;}
    break;

  case 517:

/* Line 1455 of yacc.c  */
#line 2543 "ats_grammar.yats"
    { (yyval.m0atch) = m0atch_make_some ((yyvsp[(1) - (3)].d0exp), (yyvsp[(3) - (3)].p0at)) ; ;}
    break;

  case 518:

/* Line 1455 of yacc.c  */
#line 2547 "ats_grammar.yats"
    { (yyval.m0atchlst) = m0atchlst_cons ((yyvsp[(1) - (2)].m0atch), (yyvsp[(2) - (2)].m0atchlst) ) ; ;}
    break;

  case 519:

/* Line 1455 of yacc.c  */
#line 2551 "ats_grammar.yats"
    { (yyval.m0atchlst) = m0atchlst_nil () ; ;}
    break;

  case 520:

/* Line 1455 of yacc.c  */
#line 2552 "ats_grammar.yats"
    { (yyval.m0atchlst) = m0atchlst_cons ((yyvsp[(2) - (3)].m0atch), (yyvsp[(3) - (3)].m0atchlst) ) ; ;}
    break;

  case 521:

/* Line 1455 of yacc.c  */
#line 2556 "ats_grammar.yats"
    { (yyval.guap0at) = guap0at_make_none((yyvsp[(1) - (1)].p0at)) ; ;}
    break;

  case 522:

/* Line 1455 of yacc.c  */
#line 2557 "ats_grammar.yats"
    { (yyval.guap0at) = guap0at_make_some((yyvsp[(1) - (3)].p0at), (yyvsp[(3) - (3)].m0atchlst)) ; ;}
    break;

  case 523:

/* Line 1455 of yacc.c  */
#line 2561 "ats_grammar.yats"
    { (yyval.c0lau) = c0lau_make ((yyvsp[(1) - (3)].guap0at), 0, 0, (yyvsp[(3) - (3)].d0exp)) ; ;}
    break;

  case 524:

/* Line 1455 of yacc.c  */
#line 2562 "ats_grammar.yats"
    { (yyval.c0lau) = c0lau_make ((yyvsp[(1) - (3)].guap0at), 1, 0, (yyvsp[(3) - (3)].d0exp)) ; ;}
    break;

  case 525:

/* Line 1455 of yacc.c  */
#line 2563 "ats_grammar.yats"
    { (yyval.c0lau) = c0lau_make ((yyvsp[(1) - (3)].guap0at), 0, 1, (yyvsp[(3) - (3)].d0exp)) ; ;}
    break;

  case 526:

/* Line 1455 of yacc.c  */
#line 2564 "ats_grammar.yats"
    { (yyval.c0lau) = c0lau_make ((yyvsp[(1) - (3)].guap0at), 1, 1, (yyvsp[(3) - (3)].d0exp)) ; ;}
    break;

  case 527:

/* Line 1455 of yacc.c  */
#line 2568 "ats_grammar.yats"
    { (yyval.c0laulst) = (yyvsp[(1) - (1)].c0laulst) ; ;}
    break;

  case 528:

/* Line 1455 of yacc.c  */
#line 2569 "ats_grammar.yats"
    { (yyval.c0laulst) = c0laulst_cons((yyvsp[(1) - (2)].c0lau), (yyvsp[(2) - (2)].c0laulst)) ; ;}
    break;

  case 529:

/* Line 1455 of yacc.c  */
#line 2573 "ats_grammar.yats"
    { (yyval.c0laulst) = c0laulst_nil() ; ;}
    break;

  case 530:

/* Line 1455 of yacc.c  */
#line 2574 "ats_grammar.yats"
    { (yyval.c0laulst) = c0laulst_cons((yyvsp[(2) - (3)].c0lau), (yyvsp[(3) - (3)].c0laulst)) ; ;}
    break;

  case 531:

/* Line 1455 of yacc.c  */
#line 2578 "ats_grammar.yats"
    { (yyval.sc0lau) = sc0lau_make((yyvsp[(1) - (3)].sp0at), (yyvsp[(3) - (3)].d0exp)) ; ;}
    break;

  case 532:

/* Line 1455 of yacc.c  */
#line 2582 "ats_grammar.yats"
    { (yyval.sc0laulst) = (yyvsp[(1) - (1)].sc0laulst) ; ;}
    break;

  case 533:

/* Line 1455 of yacc.c  */
#line 2583 "ats_grammar.yats"
    { (yyval.sc0laulst) = sc0laulst_cons((yyvsp[(1) - (2)].sc0lau), (yyvsp[(2) - (2)].sc0laulst)) ; ;}
    break;

  case 534:

/* Line 1455 of yacc.c  */
#line 2587 "ats_grammar.yats"
    { (yyval.sc0laulst) = sc0laulst_nil() ; ;}
    break;

  case 535:

/* Line 1455 of yacc.c  */
#line 2588 "ats_grammar.yats"
    { (yyval.sc0laulst) = sc0laulst_cons((yyvsp[(2) - (3)].sc0lau), (yyvsp[(3) - (3)].sc0laulst)) ; ;}
    break;

  case 536:

/* Line 1455 of yacc.c  */
#line 2592 "ats_grammar.yats"
    { (yyval.s0qualstopt) = s0qualstopt_none() ; ;}
    break;

  case 537:

/* Line 1455 of yacc.c  */
#line 2593 "ats_grammar.yats"
    { (yyval.s0qualstopt) = s0qualstopt_some((yyvsp[(2) - (3)].s0qualst)) ; ;}
    break;

  case 538:

/* Line 1455 of yacc.c  */
#line 2597 "ats_grammar.yats"
    { (yyval.s0qualstopt) = s0explstopt_none() ; ;}
    break;

  case 539:

/* Line 1455 of yacc.c  */
#line 2598 "ats_grammar.yats"
    { (yyval.s0qualstopt) = s0explstopt_some((yyvsp[(2) - (3)].s0explst)) ; ;}
    break;

  case 540:

/* Line 1455 of yacc.c  */
#line 2599 "ats_grammar.yats"
    { (yyval.s0qualstopt) = s0explstopt_some(s0explst_nil()) ; ;}
    break;

  case 541:

/* Line 1455 of yacc.c  */
#line 2603 "ats_grammar.yats"
    { (yyval.i0nvarg) = i0nvarg_make_none((yyvsp[(1) - (2)].i0de)) ; ;}
    break;

  case 542:

/* Line 1455 of yacc.c  */
#line 2604 "ats_grammar.yats"
    { (yyval.i0nvarg) = i0nvarg_make_some((yyvsp[(1) - (3)].i0de), (yyvsp[(3) - (3)].s0exp)) ; ;}
    break;

  case 543:

/* Line 1455 of yacc.c  */
#line 2608 "ats_grammar.yats"
    { (yyval.i0nvarglst) = i0nvarglst_nil() ; ;}
    break;

  case 544:

/* Line 1455 of yacc.c  */
#line 2609 "ats_grammar.yats"
    { (yyval.i0nvarglst) = i0nvarglst_cons((yyvsp[(1) - (2)].i0nvarg), (yyvsp[(2) - (2)].i0nvarglst)) ; ;}
    break;

  case 545:

/* Line 1455 of yacc.c  */
#line 2613 "ats_grammar.yats"
    { (yyval.i0nvarglst) = i0nvarglst_nil() ; ;}
    break;

  case 546:

/* Line 1455 of yacc.c  */
#line 2614 "ats_grammar.yats"
    { (yyval.i0nvarglst) = i0nvarglst_cons((yyvsp[(2) - (3)].i0nvarg), (yyvsp[(3) - (3)].i0nvarglst)) ; ;}
    break;

  case 547:

/* Line 1455 of yacc.c  */
#line 2618 "ats_grammar.yats"
    { (yyval.i0nvarglst) = (yyvsp[(2) - (3)].i0nvarglst) ; ;}
    break;

  case 548:

/* Line 1455 of yacc.c  */
#line 2622 "ats_grammar.yats"
    { (yyval.s0qualstopt) = s0qualstopt_none() ; ;}
    break;

  case 549:

/* Line 1455 of yacc.c  */
#line 2623 "ats_grammar.yats"
    { (yyval.s0qualstopt) = s0qualstopt_some((yyvsp[(2) - (3)].s0qualst)) ; ;}
    break;

  case 550:

/* Line 1455 of yacc.c  */
#line 2627 "ats_grammar.yats"
    { (yyval.i0nvresstate) = i0nvresstate_none() ; ;}
    break;

  case 551:

/* Line 1455 of yacc.c  */
#line 2628 "ats_grammar.yats"
    { (yyval.i0nvresstate) = i0nvresstate_some((yyvsp[(2) - (5)].s0qualstopt), (yyvsp[(4) - (5)].i0nvarglst)) ; ;}
    break;

  case 552:

/* Line 1455 of yacc.c  */
#line 2632 "ats_grammar.yats"
    { (yyval.loopi0nv) = loopi0nv_make((yyvsp[(1) - (4)].s0qualstopt), (yyvsp[(2) - (4)].s0qualstopt), (yyvsp[(3) - (4)].i0nvarglst), (yyvsp[(4) - (4)].i0nvresstate)) ; ;}
    break;

  case 553:

/* Line 1455 of yacc.c  */
#line 2636 "ats_grammar.yats"
    { (yyval.initestpost) = initestpost_make ((yyvsp[(1) - (7)].t0kn),(yyvsp[(2) - (7)].d0explst),(yyvsp[(3) - (7)].t0kn),(yyvsp[(4) - (7)].d0explst),(yyvsp[(5) - (7)].t0kn),(yyvsp[(6) - (7)].d0explst),(yyvsp[(7) - (7)].t0kn)) ; ;}
    break;

  case 554:

/* Line 1455 of yacc.c  */
#line 2640 "ats_grammar.yats"
    { (yyval.i0de) = (yyvsp[(1) - (1)].i0de) ; ;}
    break;

  case 555:

/* Line 1455 of yacc.c  */
#line 2644 "ats_grammar.yats"
    { (yyval.i0delst) = i0delst_nil() ; ;}
    break;

  case 556:

/* Line 1455 of yacc.c  */
#line 2645 "ats_grammar.yats"
    { (yyval.i0delst) = i0delst_cons((yyvsp[(1) - (2)].i0de), (yyvsp[(2) - (2)].i0delst)) ; ;}
    break;

  case 557:

/* Line 1455 of yacc.c  */
#line 2649 "ats_grammar.yats"
    { (yyval.i0delst) = i0delst_nil() ; ;}
    break;

  case 558:

/* Line 1455 of yacc.c  */
#line 2650 "ats_grammar.yats"
    { (yyval.i0delst) = i0delst_cons((yyvsp[(2) - (3)].i0de), (yyvsp[(3) - (3)].i0delst)) ; ;}
    break;

  case 559:

/* Line 1455 of yacc.c  */
#line 2654 "ats_grammar.yats"
    { (yyval.m0acarg) = m0acarg_one ((yyvsp[(1) - (1)].i0de)) ; ;}
    break;

  case 560:

/* Line 1455 of yacc.c  */
#line 2655 "ats_grammar.yats"
    { (yyval.m0acarg) = m0acarg_lst ((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].i0delst), (yyvsp[(3) - (3)].t0kn)) ; ;}
    break;

  case 561:

/* Line 1455 of yacc.c  */
#line 2659 "ats_grammar.yats"
    { (yyval.m0acarglst) = m0acarglst_nil () ; ;}
    break;

  case 562:

/* Line 1455 of yacc.c  */
#line 2660 "ats_grammar.yats"
    { (yyval.m0acarglst) = m0acarglst_cons ((yyvsp[(1) - (2)].m0acarg), (yyvsp[(2) - (2)].m0acarglst)) ; ;}
    break;

  case 563:

/* Line 1455 of yacc.c  */
#line 2664 "ats_grammar.yats"
    { (yyval.m0acdef) = m0acdef_make((yyvsp[(1) - (4)].i0de), (yyvsp[(2) - (4)].m0acarglst), (yyvsp[(4) - (4)].d0exp)) ; ;}
    break;

  case 564:

/* Line 1455 of yacc.c  */
#line 2668 "ats_grammar.yats"
    { (yyval.m0acdeflst) = m0acdeflst_nil() ; ;}
    break;

  case 565:

/* Line 1455 of yacc.c  */
#line 2669 "ats_grammar.yats"
    { (yyval.m0acdeflst) = m0acdeflst_cons((yyvsp[(2) - (3)].m0acdef), (yyvsp[(3) - (3)].m0acdeflst)) ; ;}
    break;

  case 566:

/* Line 1455 of yacc.c  */
#line 2673 "ats_grammar.yats"
    { (yyval.v0aldec) = v0aldec_make ((yyvsp[(1) - (4)].p0at), (yyvsp[(3) - (4)].d0exp), (yyvsp[(4) - (4)].witht0ype)) ; ;}
    break;

  case 567:

/* Line 1455 of yacc.c  */
#line 2677 "ats_grammar.yats"
    { (yyval.v0aldeclst) = v0aldeclst_nil() ; ;}
    break;

  case 568:

/* Line 1455 of yacc.c  */
#line 2678 "ats_grammar.yats"
    { (yyval.v0aldeclst) = v0aldeclst_cons((yyvsp[(2) - (3)].v0aldec), (yyvsp[(3) - (3)].v0aldeclst)) ; ;}
    break;

  case 569:

/* Line 1455 of yacc.c  */
#line 2682 "ats_grammar.yats"
    { (yyval.f0undec) = f0undec_make_none((yyvsp[(1) - (5)].i0de), (yyvsp[(2) - (5)].f0arglst), (yyvsp[(4) - (5)].d0exp), (yyvsp[(5) - (5)].witht0ype)) ; ;}
    break;

  case 570:

/* Line 1455 of yacc.c  */
#line 2683 "ats_grammar.yats"
    { (yyval.f0undec) = f0undec_make_some((yyvsp[(1) - (7)].i0de), (yyvsp[(2) - (7)].f0arglst), (yyvsp[(3) - (7)].e0fftaglstopt), (yyvsp[(4) - (7)].s0exp), (yyvsp[(6) - (7)].d0exp), (yyvsp[(7) - (7)].witht0ype)) ; ;}
    break;

  case 571:

/* Line 1455 of yacc.c  */
#line 2687 "ats_grammar.yats"
    { (yyval.f0undeclst) = f0undeclst_nil() ; ;}
    break;

  case 572:

/* Line 1455 of yacc.c  */
#line 2688 "ats_grammar.yats"
    { (yyval.f0undeclst) = f0undeclst_cons((yyvsp[(2) - (3)].f0undec), (yyvsp[(3) - (3)].f0undeclst)) ; ;}
    break;

  case 573:

/* Line 1455 of yacc.c  */
#line 2692 "ats_grammar.yats"
    { (yyval.v0arwth) = v0arwth_none () ; ;}
    break;

  case 574:

/* Line 1455 of yacc.c  */
#line 2693 "ats_grammar.yats"
    { (yyval.v0arwth) = v0arwth_some ((yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 575:

/* Line 1455 of yacc.c  */
#line 2697 "ats_grammar.yats"
    { (yyval.v0ardec) = v0ardec_make_none_some(0, (yyvsp[(1) - (4)].i0de), (yyvsp[(2) - (4)].v0arwth), (yyvsp[(4) - (4)].d0exp)) ; ;}
    break;

  case 576:

/* Line 1455 of yacc.c  */
#line 2698 "ats_grammar.yats"
    { (yyval.v0ardec) = v0ardec_make_none_some(1, (yyvsp[(2) - (5)].i0de), (yyvsp[(3) - (5)].v0arwth), (yyvsp[(5) - (5)].d0exp)) ; ;}
    break;

  case 577:

/* Line 1455 of yacc.c  */
#line 2699 "ats_grammar.yats"
    { (yyval.v0ardec) = v0ardec_make_some_none(0, (yyvsp[(1) - (4)].i0de), (yyvsp[(3) - (4)].s0exp), (yyvsp[(4) - (4)].v0arwth)) ; ;}
    break;

  case 578:

/* Line 1455 of yacc.c  */
#line 2700 "ats_grammar.yats"
    { (yyval.v0ardec) = v0ardec_make_some_some(0, (yyvsp[(1) - (6)].i0de), (yyvsp[(3) - (6)].s0exp), (yyvsp[(4) - (6)].v0arwth), (yyvsp[(6) - (6)].d0exp)) ; ;}
    break;

  case 579:

/* Line 1455 of yacc.c  */
#line 2704 "ats_grammar.yats"
    { (yyval.v0ardeclst) = v0ardeclst_nil() ; ;}
    break;

  case 580:

/* Line 1455 of yacc.c  */
#line 2705 "ats_grammar.yats"
    { (yyval.v0ardeclst) = v0ardeclst_cons((yyvsp[(2) - (3)].v0ardec), (yyvsp[(3) - (3)].v0ardeclst)) ; ;}
    break;

  case 581:

/* Line 1455 of yacc.c  */
#line 2709 "ats_grammar.yats"
    { (yyval.i0mpdec) = i0mpdec_make((yyvsp[(1) - (5)].impqi0de), (yyvsp[(2) - (5)].f0arglst), (yyvsp[(3) - (5)].s0expopt), (yyvsp[(5) - (5)].d0exp)) ; ;}
    break;

  case 582:

/* Line 1455 of yacc.c  */
#line 2713 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_infix((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0rec),  0, (yyvsp[(3) - (3)].i0delst)) ; ;}
    break;

  case 583:

/* Line 1455 of yacc.c  */
#line 2714 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_infix((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0rec), -1, (yyvsp[(3) - (3)].i0delst)) ; ;}
    break;

  case 584:

/* Line 1455 of yacc.c  */
#line 2715 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_infix((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0rec),  1, (yyvsp[(3) - (3)].i0delst)) ; ;}
    break;

  case 585:

/* Line 1455 of yacc.c  */
#line 2716 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_prefix((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0rec), (yyvsp[(3) - (3)].i0delst)) ; ;}
    break;

  case 586:

/* Line 1455 of yacc.c  */
#line 2717 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_postfix((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].p0rec), (yyvsp[(3) - (3)].i0delst)) ; ;}
    break;

  case 587:

/* Line 1455 of yacc.c  */
#line 2718 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_nonfix((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0delst)) ; ;}
    break;

  case 588:

/* Line 1455 of yacc.c  */
#line 2719 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_symintr((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0delst)) ; ;}
    break;

  case 589:

/* Line 1455 of yacc.c  */
#line 2720 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_e0xpundef((yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 590:

/* Line 1455 of yacc.c  */
#line 2721 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_e0xpdef((yyvsp[(2) - (3)].i0de), (yyvsp[(3) - (3)].e0xpopt)) ; ;}
    break;

  case 591:

/* Line 1455 of yacc.c  */
#line 2722 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_e0xpact_assert((yyvsp[(2) - (2)].e0xp)) ; ;}
    break;

  case 592:

/* Line 1455 of yacc.c  */
#line 2723 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_e0xpact_error((yyvsp[(2) - (2)].e0xp)) ; ;}
    break;

  case 593:

/* Line 1455 of yacc.c  */
#line 2724 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_e0xpact_print((yyvsp[(2) - (2)].e0xp)) ; ;}
    break;

  case 594:

/* Line 1455 of yacc.c  */
#line 2725 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_srtdefs((yyvsp[(2) - (3)].s0rtdef), (yyvsp[(3) - (3)].s0rtdeflst)) ; ;}
    break;

  case 595:

/* Line 1455 of yacc.c  */
#line 2726 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_datsrts(0, (yyvsp[(2) - (3)].d0atsrtdec), (yyvsp[(3) - (3)].d0atsrtdeclst)) ; ;}
    break;

  case 596:

/* Line 1455 of yacc.c  */
#line 2727 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_datsrts(1, (yyvsp[(2) - (3)].d0atsrtdec), (yyvsp[(3) - (3)].d0atsrtdeclst)) ; ;}
    break;

  case 597:

/* Line 1455 of yacc.c  */
#line 2728 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_stacons((yyvsp[(1) - (3)].abskind), (yyvsp[(2) - (3)].s0tacon), (yyvsp[(3) - (3)].s0taconlst)) ; ;}
    break;

  case 598:

/* Line 1455 of yacc.c  */
#line 2729 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_stacsts((yyvsp[(2) - (3)].s0tacst), (yyvsp[(3) - (3)].s0tacstlst)) ; ;}
    break;

  case 599:

/* Line 1455 of yacc.c  */
#line 2730 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_stavars((yyvsp[(2) - (3)].s0tavar), (yyvsp[(3) - (3)].s0tavarlst)) ; ;}
    break;

  case 600:

/* Line 1455 of yacc.c  */
#line 2731 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_sexpdefs((yyvsp[(1) - (3)].stadefkind), (yyvsp[(2) - (3)].s0expdef), (yyvsp[(3) - (3)].s0expdeflst)) ; ;}
    break;

  case 601:

/* Line 1455 of yacc.c  */
#line 2732 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_saspdec((yyvsp[(2) - (2)].s0aspdec)) ; ;}
    break;

  case 602:

/* Line 1455 of yacc.c  */
#line 2733 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_datdecs((yyvsp[(1) - (4)].datakind), (yyvsp[(2) - (4)].d0atdec), (yyvsp[(3) - (4)].d0atdeclst), (yyvsp[(4) - (4)].s0expdeflst)) ; ;}
    break;

  case 603:

/* Line 1455 of yacc.c  */
#line 2734 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_exndecs((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].e0xndec), (yyvsp[(3) - (3)].e0xndeclst)) ; ;}
    break;

  case 604:

/* Line 1455 of yacc.c  */
#line 2735 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_classdec_none ((yyvsp[(1) - (2)].t0kn), (yyvsp[(2) - (2)].i0de)) ; ;}
    break;

  case 605:

/* Line 1455 of yacc.c  */
#line 2736 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_classdec_some ((yyvsp[(1) - (4)].t0kn), (yyvsp[(2) - (4)].i0de), (yyvsp[(4) - (4)].s0exp)) ; ;}
    break;

  case 606:

/* Line 1455 of yacc.c  */
#line 2737 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_overload((yyvsp[(1) - (4)].t0kn), (yyvsp[(2) - (4)].i0de), (yyvsp[(4) - (4)].dqi0de)) ; ;}
    break;

  case 607:

/* Line 1455 of yacc.c  */
#line 2738 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_overload_lrbrackets((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].t0kn), (yyvsp[(3) - (5)].t0kn), (yyvsp[(5) - (5)].dqi0de)) ; ;}
    break;

  case 608:

/* Line 1455 of yacc.c  */
#line 2739 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_macdefs(0, (yyvsp[(2) - (3)].m0acdef), (yyvsp[(3) - (3)].m0acdeflst)) ; ;}
    break;

  case 609:

/* Line 1455 of yacc.c  */
#line 2740 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_macdefs(-1/*error*/, (yyvsp[(3) - (4)].m0acdef), (yyvsp[(4) - (4)].m0acdeflst)) ; ;}
    break;

  case 610:

/* Line 1455 of yacc.c  */
#line 2741 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_macdefs(1, (yyvsp[(2) - (3)].m0acdef), (yyvsp[(3) - (3)].m0acdeflst)) ; ;}
    break;

  case 611:

/* Line 1455 of yacc.c  */
#line 2742 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_macdefs(2, (yyvsp[(3) - (4)].m0acdef), (yyvsp[(4) - (4)].m0acdeflst)) ; ;}
    break;

  case 612:

/* Line 1455 of yacc.c  */
#line 2743 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_staload_none((yyvsp[(2) - (2)].s0tring)) ; ;}
    break;

  case 613:

/* Line 1455 of yacc.c  */
#line 2744 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_staload_some((yyvsp[(2) - (4)].i0de), (yyvsp[(4) - (4)].s0tring)) ; ;}
    break;

  case 614:

/* Line 1455 of yacc.c  */
#line 2748 "ats_grammar.yats"
    { (yyval.s0qualst) = (yyvsp[(2) - (3)].s0qualst) ; ;}
    break;

  case 615:

/* Line 1455 of yacc.c  */
#line 2752 "ats_grammar.yats"
    { (yyval.s0qualstlst) = s0qualstlst_nil() ; ;}
    break;

  case 616:

/* Line 1455 of yacc.c  */
#line 2753 "ats_grammar.yats"
    { (yyval.s0qualstlst) = s0qualstlst_cons((yyvsp[(1) - (2)].s0qualst), (yyvsp[(2) - (2)].s0qualstlst)) ; ;}
    break;

  case 617:

/* Line 1455 of yacc.c  */
#line 2757 "ats_grammar.yats"
    { ; ;}
    break;

  case 618:

/* Line 1455 of yacc.c  */
#line 2758 "ats_grammar.yats"
    { ; ;}
    break;

  case 619:

/* Line 1455 of yacc.c  */
#line 2762 "ats_grammar.yats"
    { (yyval.d0ec) = (yyvsp[(1) - (1)].d0ec) ; ;}
    break;

  case 620:

/* Line 1455 of yacc.c  */
#line 2763 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_dcstdecs((yyvsp[(1) - (4)].dcstkind), (yyvsp[(2) - (4)].s0qualstlst), (yyvsp[(3) - (4)].d0cstdec), (yyvsp[(4) - (4)].d0cstdeclst)) ; ;}
    break;

  case 621:

/* Line 1455 of yacc.c  */
#line 2764 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_extcode_sta((yyvsp[(1) - (1)].e0xtcode)) ; ;}
    break;

  case 622:

/* Line 1455 of yacc.c  */
#line 2765 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_guadec((yyvsp[(1) - (2)].srpifkindtok), (yyvsp[(2) - (2)].d0eclst)) ; ;}
    break;

  case 623:

/* Line 1455 of yacc.c  */
#line 2766 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_include(0/*sta*/, (yyvsp[(2) - (2)].s0tring)) ; ;}
    break;

  case 624:

/* Line 1455 of yacc.c  */
#line 2767 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_local((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].d0eclst), (yyvsp[(4) - (5)].d0eclst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 625:

/* Line 1455 of yacc.c  */
#line 2771 "ats_grammar.yats"
    { (yyval.d0eclst) = guad0ec_one((yyvsp[(1) - (4)].e0xp), (yyvsp[(3) - (4)].d0eclst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 626:

/* Line 1455 of yacc.c  */
#line 2772 "ats_grammar.yats"
    { (yyval.d0eclst) = guad0ec_two((yyvsp[(1) - (6)].e0xp), (yyvsp[(3) - (6)].d0eclst), (yyvsp[(5) - (6)].d0eclst), (yyvsp[(6) - (6)].t0kn)) ; ;}
    break;

  case 627:

/* Line 1455 of yacc.c  */
#line 2773 "ats_grammar.yats"
    { (yyval.d0eclst) = guad0ec_cons((yyvsp[(1) - (5)].e0xp), (yyvsp[(3) - (5)].d0eclst), (yyvsp[(4) - (5)].srpifkindtok), (yyvsp[(5) - (5)].d0eclst)) ; ;}
    break;

  case 628:

/* Line 1455 of yacc.c  */
#line 2777 "ats_grammar.yats"
    { (yyval.d0eclst) = d0ecllst_reverse((yyvsp[(1) - (1)].d0eclst)) ; ;}
    break;

  case 629:

/* Line 1455 of yacc.c  */
#line 2781 "ats_grammar.yats"
    { (yyval.d0eclst) = d0ecllst_nil() ; ;}
    break;

  case 630:

/* Line 1455 of yacc.c  */
#line 2782 "ats_grammar.yats"
    { (yyval.d0eclst) = d0ecllst_cons((yyvsp[(1) - (3)].d0eclst), (yyvsp[(2) - (3)].d0ec)) ; ;}
    break;

  case 631:

/* Line 1455 of yacc.c  */
#line 2786 "ats_grammar.yats"
    { (yyval.d0ec) = (yyvsp[(1) - (1)].d0ec) ; ;}
    break;

  case 632:

/* Line 1455 of yacc.c  */
#line 2787 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_dcstdecs((yyvsp[(2) - (5)].dcstkind), (yyvsp[(3) - (5)].s0qualstlst), (yyvsp[(4) - (5)].d0cstdec), (yyvsp[(5) - (5)].d0cstdeclst)) ; ;}
    break;

  case 633:

/* Line 1455 of yacc.c  */
#line 2788 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_extype((yyvsp[(3) - (5)].s0tring), (yyvsp[(5) - (5)].s0exp)) ; ;}
    break;

  case 634:

/* Line 1455 of yacc.c  */
#line 2789 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_extval((yyvsp[(3) - (5)].s0tring), (yyvsp[(5) - (5)].d0exp)) ; ;}
    break;

  case 635:

/* Line 1455 of yacc.c  */
#line 2790 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_valdecs((yyvsp[(1) - (3)].valkind), (yyvsp[(2) - (3)].v0aldec), (yyvsp[(3) - (3)].v0aldeclst)) ; ;}
    break;

  case 636:

/* Line 1455 of yacc.c  */
#line 2791 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_valdecs_par((yyvsp[(3) - (4)].v0aldec), (yyvsp[(4) - (4)].v0aldeclst)) ; ;}
    break;

  case 637:

/* Line 1455 of yacc.c  */
#line 2792 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_valdecs_rec((yyvsp[(3) - (4)].v0aldec), (yyvsp[(4) - (4)].v0aldeclst)) ; ;}
    break;

  case 638:

/* Line 1455 of yacc.c  */
#line 2793 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_fundecs((yyvsp[(1) - (4)].funkind), (yyvsp[(2) - (4)].s0qualstlst), (yyvsp[(3) - (4)].f0undec), (yyvsp[(4) - (4)].f0undeclst)) ; ;}
    break;

  case 639:

/* Line 1455 of yacc.c  */
#line 2794 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_vardecs((yyvsp[(2) - (3)].v0ardec), (yyvsp[(3) - (3)].v0ardeclst)) ; ;}
    break;

  case 640:

/* Line 1455 of yacc.c  */
#line 2795 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_impdec((yyvsp[(1) - (3)].t0kn), (yyvsp[(2) - (3)].s0arglstlst), (yyvsp[(3) - (3)].i0mpdec)) ; ;}
    break;

  case 641:

/* Line 1455 of yacc.c  */
#line 2796 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_local((yyvsp[(1) - (5)].t0kn), (yyvsp[(2) - (5)].d0eclst), (yyvsp[(4) - (5)].d0eclst), (yyvsp[(5) - (5)].t0kn)) ; ;}
    break;

  case 642:

/* Line 1455 of yacc.c  */
#line 2797 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_extcode_dyn((yyvsp[(1) - (1)].e0xtcode)) ; ;}
    break;

  case 643:

/* Line 1455 of yacc.c  */
#line 2798 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_guadec((yyvsp[(1) - (2)].srpifkindtok), (yyvsp[(2) - (2)].d0eclst)) ; ;}
    break;

  case 644:

/* Line 1455 of yacc.c  */
#line 2799 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_include(1/*dyn*/, (yyvsp[(2) - (2)].s0tring)) ; ;}
    break;

  case 645:

/* Line 1455 of yacc.c  */
#line 2800 "ats_grammar.yats"
    { (yyval.d0ec) = d0ec_dynload((yyvsp[(2) - (2)].s0tring)) ; ;}
    break;

  case 646:

/* Line 1455 of yacc.c  */
#line 2804 "ats_grammar.yats"
    { (yyval.d0eclst) = guad0ec_one((yyvsp[(1) - (4)].e0xp), (yyvsp[(3) - (4)].d0eclst), (yyvsp[(4) - (4)].t0kn)) ; ;}
    break;

  case 647:

/* Line 1455 of yacc.c  */
#line 2805 "ats_grammar.yats"
    { (yyval.d0eclst) = guad0ec_two((yyvsp[(1) - (6)].e0xp), (yyvsp[(3) - (6)].d0eclst), (yyvsp[(5) - (6)].d0eclst), (yyvsp[(6) - (6)].t0kn)) ; ;}
    break;

  case 648:

/* Line 1455 of yacc.c  */
#line 2806 "ats_grammar.yats"
    { (yyval.d0eclst) = guad0ec_cons((yyvsp[(1) - (5)].e0xp), (yyvsp[(3) - (5)].d0eclst), (yyvsp[(4) - (5)].srpifkindtok), (yyvsp[(5) - (5)].d0eclst)) ; ;}
    break;

  case 649:

/* Line 1455 of yacc.c  */
#line 2810 "ats_grammar.yats"
    { (yyval.d0eclst) = d0ecllst_reverse((yyvsp[(1) - (1)].d0eclst)) ; ;}
    break;

  case 650:

/* Line 1455 of yacc.c  */
#line 2814 "ats_grammar.yats"
    { (yyval.d0eclst) = d0ecllst_nil() ; ;}
    break;

  case 651:

/* Line 1455 of yacc.c  */
#line 2815 "ats_grammar.yats"
    { (yyval.d0eclst) = d0ecllst_cons((yyvsp[(1) - (3)].d0eclst), (yyvsp[(2) - (3)].d0ec)) ; ;}
    break;



/* Line 1455 of yacc.c  */
#line 8794 "ats_grammar_yats.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 2821 "ats_grammar.yats"


/* ****** ****** */

int
yylex_tok0 = -1 ;

int
yylex() {
//
  int tok ;
//
  if (yylex_tok0 >= 0) {
    tok = yylex_tok0 ; yylex_tok0 = -1 ;
  } else {
    tok = atsopt_lexer_token_get () ;
  } // end of [if]
/*
** fprintf (stdout, "tok = %i\n", tok) ;
*/
  return tok ;
//
} /* end of [yylex_tok0] */

//
// HX: needed in [ats_lexer.lats]
//
ats_void_type
yylval_char_set(c0har_t val)
  { yylval.c0har = val ; return ; }

ats_void_type
yylval_extcode_set(e0xtcode_t val)
  { yylval.e0xtcode = val ; return ; }

ats_void_type
yylval_float_set(f0loat_t val)
  { yylval.f0loat = val ; return ; }

ats_void_type
yylval_floatsp_set(f0loatsp_t val)
  { yylval.f0loatsp = val ; return ; }

ats_void_type
yylval_ide_set(i0de_t val)
  { yylval.i0de = val ; return ; }

ats_void_type
yylval_int_set(i0nt_t val)
  { yylval.i0nt = val ; return ; }

ats_void_type
yylval_intsp_set(i0ntsp_t val)
  { yylval.i0ntsp = val ; return ; }

ats_void_type
yylval_string_set(s0tring_t val)
  { yylval.s0tring = val ; return ; }

ats_void_type
yylval_token_set(t0kn_t val)
  { yylval.t0kn = val ; return ; }

// HX: see [stdlib.h]
extern void exit (int) ;
//
// HX: implemented in [ats_filename.dats]
extern ats_void_type atsopt_filename_prerr () ;
//
extern ats_ptr_type lexing_fstpos_get () ;
extern ats_void_type lexing_prerr_position (ats_ptr_type) ;
//
void
yyerror (char *s) {
  fprintf (stderr, "%s: ", s) ;
  atsopt_filename_prerr () ;
  fprintf (stderr, ": [") ;
  lexing_prerr_position (lexing_fstpos_get ()) ;
  fprintf (stderr, "]\n") ;
  exit (1) ; // HX: no error recovery yet; maybe in future
  return ;
} /* end of [yyerror] */

yyres_t
yyparse_main (
  ats_int_type tok0
) {
/*
** HX: we must take care of garbage collection!
*/
  // fprintf (stderr, "yyparse_main: &yyss = %p\n", &yyss) ;
  // ATS_GC_MARKROOT (&yyss, sizeof(short*)) ; // [ats_malloc_ngc] is used
  // fprintf (stderr, "yyparse_main: &yyvs = %p\n", &yyvs) ;
  // ATS_GC_MARKROOT (&yyvs, sizeof(YYSTYPE*)) ;  // [ats_malloc_ngc] is used
/*
** HX-2010-02-25:
** if BISON is used then [yyval] is a stack variable and
** thus there is no need to treat it as a GC root explicitly
*/
//
#ifndef _ATS_YYVALISLOCAL
  extern YYSTYPE yyval;
  // fprintf (stderr, "yyparse_main: &yyval = %p\n", &yyval) ;
  ATS_GC_MARKROOT (&yyval, sizeof(YYSTYPE)) ;
#endif // end of [_ATS_YYVALISLOCAL]
//
  extern YYSTYPE yylval;
  // fprintf (stderr, "yyparse_main: &yylval = %p\n", &yylval) ;
  ATS_GC_MARKROOT (&yylval, sizeof(YYSTYPE)) ;
//
#ifdef YYPATCH
#if (YYPATCH >= 20101229)
  // fprintf (stderr, "yyparse_main: &yystack = %p\n", &yystack) ;
  ATS_GC_MARKROOT (&yystack, sizeof(YYSTACKDATA)) ;
#endif
#endif
//
  yylex_tok0 = tok0 ;
//
  yyparse () ;
//
  return theYYVALyyres ;
} /* end of [yyparse_main] */

/* ****** ****** */

// end of [ats_grammar.yats]

