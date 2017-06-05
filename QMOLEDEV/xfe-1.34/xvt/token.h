/*  Copyright 1992 John Bovey, University of Kent at Canterbury.
 *
 *  Redistribution and use in source code and/or executable forms, with
 *  or without modification, are permitted provided that the following
 *  condition is met:
 *
 *  Any redistribution must retain the above copyright notice, this
 *  condition and the following disclaimer, either as part of the
 *  program source code included in the redistribution or in human-
 *  readable materials provided with the redistribution.
 *
 *  THIS SOFTWARE IS PROVIDED "AS IS".  Any express or implied
 *  warranties concerning this software are disclaimed by the copyright
 *  holder to the fullest extent permitted by applicable law.  In no
 *  event shall the copyright-holder be liable for any damages of any
 *  kind, however caused and on any theory of liability, arising in any
 *  way out of the use of, or inability to use, this software.
 *
 *  -------------------------------------------------------------------
 *
 *  In other words, do not misrepresent my work as your own work, and
 *  do not sue me if it causes problems.  Feel free to do anything else
 *  you wish with it.
 */

/* Guard C code in headers, while including them from C++ */
#ifdef  __cplusplus
extern "C" {
#endif


/*  Values of tk_region for Xevent generated tokens.
 */
#define MAINWIN		0
#define SCREEN		1
/*#define SCROLLBAR	2*/

/*  Token types
 */
#define TK_NULL		0	/* null token to be ignored */
#define TK_STRING	1	/* string of printable characters */
#define TK_CHAR		2	/* single character */
#define TK_EOF		3	/* read end of file */

#define TK_ENTRY	4	/* cursor crossed window boundery */
#define TK_EXPOSE	5	/* window has been exposed */
#define TK_RESIZE	6	/* main window has been resized */

/*#define TK_SBSWITCH	7*/	/* switch scrollbar in or out */
/*#define TK_SBGOTO	8*/	/* scrollbar goto */
/*#define TK_SBUP		9*/	/* scrollbar move up */
/*#define TK_SBDOWN	10*/	/* scrollbar move down */

#define TK_SELSTART	11	/* start the selection */
#define TK_SELEXTND	12	/* extend the selection */
#define TK_SELDRAG	13	/* drag the selection */
#define TK_SELINSRT	14	/* insert the selection */
#define TK_SELWORD	15	/* select a word */
#define TK_SELLINE	16	/* select a line */
#define TK_SELECT	17	/* confirm the selection */

#define TK_SELCLEAR	18	/* selection clear request */
#define TK_SELNOTIFY	19	/* selection notify request */
#define TK_SELREQUEST	20	/* selection request */

#define TK_TXTPAR	21	/* seq with text parameter */

#define TK_FOCUS	22	/* keyboard focus event */

/*  DEC VT100 control sequence token types
 */
#define TK_CUU		'A'	/* Cursor up */
#define TK_CUD		'B'	/* cursor down */
#define TK_CUF		'C'	/* cursor back */
#define TK_CUB		'D'	/* cursor back */
#define TK_CUP		'H'	/* position cursor */
#define TK_ED		'J'	/* erase to start or end of screen */
#define TK_EL		'K'	/* erase to start or end of line */
#define TK_IL		'L'	/* insert lines */
#define TK_DL		'M'	/* delete lines */
#define TK_DCH		'P'	/* Delete characters */
#define TK_ICH		'@'	/* insert characters */
#define TK_DA		'c'	/* device attributes request */
#define TK_HVP		'f'	/* horizontal and vertical position */
#define TK_TBC		'g'	/* tab clear */
#define TK_SET		'h'	/* set mode */
#define TK_RESET	'l'	/* reset mode */
#define TK_SGR		'm'	/* set graphics rendition */
#define TK_DSR		'n'	/* report status or position */
#define TK_DECSTBM	'r'	/* set top and bottom margins */

#define TK_DECSWH	'#'	/* set character width or height */
#define TK_SCS0		'('	/* set character set G0 */
#define TK_SCS1		')'	/* set character set G1 */
#define TK_DECSC	'7'	/* save cursor position */
#define TK_DECRC	'8'	/* restore cursor position */
#define TK_DECPAM	'='	/* keypad to applications mode */
#define TK_DECPNM	'>'	/* keypad to numeric mode */
#define TK_IND		0x100	/* index downward */
#define TK_NEL		0x101	/* beginning of next line */
#define TK_HTS		0x102	/* horizontal tab set */
#define TK_RI		0x103	/* reverse index */
#define TK_SS2		0x104	/* single shift 2 */
#define TK_SS3		0x105	/* single shift 3 */
#define TK_DECID	0x106	/* request terminal ID */

#ifdef  __cplusplus
}
#endif
