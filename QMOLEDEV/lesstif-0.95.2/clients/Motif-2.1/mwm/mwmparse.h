/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ALT_TOK = 258,
     APP_TOK = 259,
     BACK_TOK = 260,
     BORDER_TOK = 261,
     BTN1_CLICK2_TOK = 262,
     BTN1_CLICK_TOK = 263,
     BTN1_DOWN_TOK = 264,
     BTN1_UP_TOK = 265,
     BTN2_CLICK2_TOK = 266,
     BTN2_CLICK_TOK = 267,
     BTN2_DOWN_TOK = 268,
     BTN2_UP_TOK = 269,
     BTN3_CLICK2_TOK = 270,
     BTN3_CLICK_TOK = 271,
     BTN3_DOWN_TOK = 272,
     BTN3_UP_TOK = 273,
     BTN4_CLICK2_TOK = 274,
     BTN4_CLICK_TOK = 275,
     BTN4_DOWN_TOK = 276,
     BTN4_UP_TOK = 277,
     BTN5_CLICK2_TOK = 278,
     BTN5_CLICK_TOK = 279,
     BTN5_DOWN_TOK = 280,
     BTN5_UP_TOK = 281,
     BUTTONS_TOK = 282,
     CTRL_TOK = 283,
     FBEEP_TOK = 284,
     FCIRCLE_DOWN_TOK = 285,
     FCIRCLE_UP_TOK = 286,
     FEXEC_TOK = 287,
     FFOCUS_COLOR_TOK = 288,
     FFOCUS_KEY_TOK = 289,
     FKILL_TOK = 290,
     FLOWER_TOK = 291,
     FMAXIMIZE_TOK = 292,
     FMENU_TOK = 293,
     FMINIMIZE_TOK = 294,
     FMOVE_TOK = 295,
     FNEXT_CMAP_TOK = 296,
     FNEXT_KEY_TOK = 297,
     FNOP_TOK = 298,
     FNORMALIZE_TOK = 299,
     FNORMANDRAISE_TOK = 300,
     FPACK_ICONS_TOK = 301,
     FPASS_KEYS_TOK = 302,
     FPOST_WMENU_TOK = 303,
     FPREV_CMAP_TOK = 304,
     FPREV_KEY_TOK = 305,
     FQUIT_MWM_TOK = 306,
     FRAISE_LOWER_TOK = 307,
     FRAISE_TOK = 308,
     FRAME_TOK = 309,
     FREE_FAMILY_TOK = 310,
     FREFRESH_TOK = 311,
     FREFRESH_WIN_TOK = 312,
     FRESIZE_TOK = 313,
     FRESTART_TOK = 314,
     FRESTOREANDRAISE_TOK = 315,
     FRESTORE_TOK = 316,
     FSCREEN_TOK = 317,
     FSEND_MSG_TOK = 318,
     FSEPARATOR_TOK = 319,
     FSET_BEHAVIOR_TOK = 320,
     FTITLE_TOK = 321,
     FWINDOWLIST_TOK = 322,
     FDESK_TOK = 323,
     FTOGGLE_PAGE_TOK = 324,
     FGOTO_PAGE_TOK = 325,
     ICON_TOK = 326,
     KEY_TOK = 327,
     KEYS_TOK = 328,
     LOCK_TOK = 329,
     MENU_TOK = 330,
     MENUB_TOK = 331,
     MINIMIZEB_TOK = 332,
     MAXIMIZEB_TOK = 333,
     MOD1_TOK = 334,
     MOD2_TOK = 335,
     MOD3_TOK = 336,
     MOD4_TOK = 337,
     MOD5_TOK = 338,
     NEXT_TOK = 339,
     PREV_TOK = 340,
     ROOT_TOK = 341,
     SHIFT_TOK = 342,
     TITLE_TOK = 343,
     TRANSIENT_TOK = 344,
     WINDOW_TOK = 345,
     WITHIN_TOK = 346,
     STRING_TOK = 347
   };
#endif
/* Tokens.  */
#define ALT_TOK 258
#define APP_TOK 259
#define BACK_TOK 260
#define BORDER_TOK 261
#define BTN1_CLICK2_TOK 262
#define BTN1_CLICK_TOK 263
#define BTN1_DOWN_TOK 264
#define BTN1_UP_TOK 265
#define BTN2_CLICK2_TOK 266
#define BTN2_CLICK_TOK 267
#define BTN2_DOWN_TOK 268
#define BTN2_UP_TOK 269
#define BTN3_CLICK2_TOK 270
#define BTN3_CLICK_TOK 271
#define BTN3_DOWN_TOK 272
#define BTN3_UP_TOK 273
#define BTN4_CLICK2_TOK 274
#define BTN4_CLICK_TOK 275
#define BTN4_DOWN_TOK 276
#define BTN4_UP_TOK 277
#define BTN5_CLICK2_TOK 278
#define BTN5_CLICK_TOK 279
#define BTN5_DOWN_TOK 280
#define BTN5_UP_TOK 281
#define BUTTONS_TOK 282
#define CTRL_TOK 283
#define FBEEP_TOK 284
#define FCIRCLE_DOWN_TOK 285
#define FCIRCLE_UP_TOK 286
#define FEXEC_TOK 287
#define FFOCUS_COLOR_TOK 288
#define FFOCUS_KEY_TOK 289
#define FKILL_TOK 290
#define FLOWER_TOK 291
#define FMAXIMIZE_TOK 292
#define FMENU_TOK 293
#define FMINIMIZE_TOK 294
#define FMOVE_TOK 295
#define FNEXT_CMAP_TOK 296
#define FNEXT_KEY_TOK 297
#define FNOP_TOK 298
#define FNORMALIZE_TOK 299
#define FNORMANDRAISE_TOK 300
#define FPACK_ICONS_TOK 301
#define FPASS_KEYS_TOK 302
#define FPOST_WMENU_TOK 303
#define FPREV_CMAP_TOK 304
#define FPREV_KEY_TOK 305
#define FQUIT_MWM_TOK 306
#define FRAISE_LOWER_TOK 307
#define FRAISE_TOK 308
#define FRAME_TOK 309
#define FREE_FAMILY_TOK 310
#define FREFRESH_TOK 311
#define FREFRESH_WIN_TOK 312
#define FRESIZE_TOK 313
#define FRESTART_TOK 314
#define FRESTOREANDRAISE_TOK 315
#define FRESTORE_TOK 316
#define FSCREEN_TOK 317
#define FSEND_MSG_TOK 318
#define FSEPARATOR_TOK 319
#define FSET_BEHAVIOR_TOK 320
#define FTITLE_TOK 321
#define FWINDOWLIST_TOK 322
#define FDESK_TOK 323
#define FTOGGLE_PAGE_TOK 324
#define FGOTO_PAGE_TOK 325
#define ICON_TOK 326
#define KEY_TOK 327
#define KEYS_TOK 328
#define LOCK_TOK 329
#define MENU_TOK 330
#define MENUB_TOK 331
#define MINIMIZEB_TOK 332
#define MAXIMIZEB_TOK 333
#define MOD1_TOK 334
#define MOD2_TOK 335
#define MOD3_TOK 336
#define MOD4_TOK 337
#define MOD5_TOK 338
#define NEXT_TOK 339
#define PREV_TOK 340
#define ROOT_TOK 341
#define SHIFT_TOK 342
#define TITLE_TOK 343
#define TRANSIENT_TOK 344
#define WINDOW_TOK 345
#define WITHIN_TOK 346
#define STRING_TOK 347




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 228 "mwmparse.y"
{
    char	*string;
    int		number;
    KeySym	key;
    struct {
	int func;
	char *arg;
    } function;
    struct {
	int type;
	char *string;
    } label;
    long	modifiers;
    struct {
	int button;
	int event;
	int count;
	int modifiers;
    } button;
    struct {
	int modifiers;
	KeySym key;
    } hotkey;
}
/* Line 1489 of yacc.c.  */
#line 258 "mwmparse.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

