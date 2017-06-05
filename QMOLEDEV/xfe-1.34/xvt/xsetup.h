/*  Copyright 1992, 1994 John Bovey, University of Kent at Canterbury.
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

void fix_environment(void);
void init_display(int,char **,int,char **,char *);
int resize_window(void);
/*void switch_scrollbar(void);*/
void change_window_name(unsigned char *);
void change_icon_name(unsigned char *);
void error(char *,...);
void send_auth(void);
void map_window(void);
int is_logshell(void);
int is_eightbit(void);
int is_console(void);
/*void usage(int);*/

#ifdef  __cplusplus
}
#endif
