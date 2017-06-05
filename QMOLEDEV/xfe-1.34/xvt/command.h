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


#define TKS_MAX		5000	/* max length of a string token */
#define TK_MAX_ARGS	200	/* max number of numerical arguments */

/*  Structure used to represent a piece of input from the program
 *  or an interesting X event.
 */
struct tokenst
{
    int tk_type;		/* the token type */
    int tk_private;		/* non zero for private control sequences */
    int tk_char;		/* single (unprintable) character */
    unsigned char tk_string[TKS_MAX + 1];/* the text for string tokens */
    int tk_nlcount;		/* number of newlines in the string */
    int tk_length;		/* length of string */
    int tk_arg[TK_MAX_ARGS];/* first two numerical arguments */
    int tk_nargs;		/* number of numerical arguments */
    int tk_region;		/* terminal or scrollbar */
};

void init_command(char *,char **);
void set_cur_keys(int);
void set_kp_keys(int);
void set_sun_function_keys(int);
void send_string(unsigned char *,int);
void cprintf(char *,...);
void get_token(struct tokenst *);
void show_token(struct tokenst *);

#ifdef  __cplusplus
}
#endif
