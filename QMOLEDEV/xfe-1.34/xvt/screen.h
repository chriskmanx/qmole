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


/*  flags for scr_move()
 */
#define COL_RELATIVE	1	/* column movement is relative */
#define ROW_RELATIVE	2	/* row movement is relative */

#define MAX_SCROLL	50	/* max number of lines that can scroll at once */

/*  arguments to the screen delete functions
 */
#define END	0
#define START	1
#define ENTIRE	2

/*  rendition style flags.
 */
#define RS_NONE		0x00	/* Normal */
#define RS_BOLD		0x01	/* Bold face */
#define RS_ULINE	0x02	/* underline */
#define RS_BLINK	0x04	/* blinking */
#define RS_RVID		0x08	/* reverse video */
#define RS_STYLE	0x0f	/* style mask */

/* character set flags.
 */
#define CS_USASCII	0x00
#define CS_UKASCII	0x10
#define CS_SPECIAL	0x20
#define CS_STYLE	0x30

/*  The current selection unit
 */
enum selunit
{
    CHAR,
    WORD,
    LINE
};

int is_string_char(int);
void scr_backspace(void);
void scr_bell(void);
void scr_change_rendition(int);
void scr_change_screen(int);
void scr_char_class(unsigned char *);
void scr_clear_selection(void);
void scr_delete_characters(int);
void scr_delete_lines(int);
void scr_efill(void);
void scr_erase_line(int);
void scr_erase_screen(int);
void scr_extend_selection(int,int,int);
void scr_focus(int,int);
void scr_get_size(int *,int *);
void scr_index(void);
void scr_init(int);
void scr_insert_characters(int);
void scr_insert_lines(int);
void scr_make_selection(int);
void scr_move(int,int,int);
void scr_move_by(int);
void scr_move_to(int);
void scr_paste_primary(int,int,int);
void scr_refresh(int,int,int,int);
void scr_report_display(void);
void scr_report_position(void);
void scr_request_selection(int,int,int);
void scr_reset(void);
void scr_restore_cursor(void);
void scr_rindex(void);
void scr_save_cursor(void);
void scr_send_selection(int,int,int,int);
void scr_set_char_set(int,int);
void scr_set_decom(int);
void scr_set_insert(int);
void scr_set_margins(int,int);
void scr_set_wrap(int);
void scr_shift(int);
void scr_start_selection(int,int,enum selunit);
void scr_string(unsigned char *,int,int);
void scr_tab(void);

#ifdef  __cplusplus
}
#endif
