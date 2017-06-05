/*--------------------------------*-C-*---------------------------------*
 * File:	command.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1992        John Bovey <jdb@ukc.ac.uk>
 * Copyright (c) 1994        Robert Nation <nation@rocket.sanders.lockheed.com>
 * Copyright (c) 1995        Garrett D'Amore <garrett@netcom.com>
 * Copyright (c) 1995        Steven Hirsch <hirsch@emba.uvm.edu>
 * Copyright (c) 1995        Jakub Jelinek <jj@gnu.ai.mit.edu>
 * Copyright (c) 1997        MJ Olesen <olesen@me.queensu.ca>
 * Copyright (c) 1997        Raul Garcia Garcia <rgg@tid.es>
 * Copyright (c) 1997,1998   Oezguer Kesim <kesim@math.fu-berlin.de>
 * Copyright (c) 1998-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 1998        Alfredo K. Kojima <kojima@windowmaker.org>
 * Copyright (c) 2001        Marius Gedminas <marius.gedminas@uosis.mif.vu.lt>
 * Copyright (c) 2003        Rob McMullen <robm@flipturn.org>
 * Copyright (c) 2004        Terry Griffin <griffint@pobox.com>
 * Copyright (c) 2005        Johann 'Mykraverk' Oskarsson <johann@myrkraverk.com>
 * Copyright (c) 2004-2005   Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------*/
/*
** $Id: command.c,v 1.219 2005/06/24 22:16:26 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
#define DEBUG_LEVEL 1
#else 
#define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
#define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
#define DBG_MSG(d,x)
#endif


#ifdef USE_DEADKEY
/*
** Deadkey sequences table.
*/
typedef struct _DeadKeyChar	{
	KeySym		ks;		/* keysym */
	KeySym		dk;		/* accent */
	KeySym		ach;	/* accent keysym */
} DeadKeyChar;

static DeadKeyChar	dkc_tab[] = {
	{XK_A, XK_grave,		XK_Agrave},
	{XK_A, XK_acute,		XK_Agrave},
	{XK_A, XK_apostrophe,	XK_Aacute},
	{XK_A, XK_asciicircum,	XK_Acircumflex},
	{XK_A, XK_asciitilde,	XK_Atilde},
	{XK_A, XK_quotedbl,		XK_Adiaeresis},

	{XK_a, XK_grave,		XK_agrave},
	{XK_a, XK_acute,		XK_agrave},
	{XK_a, XK_apostrophe,	XK_aacute},
	{XK_a, XK_asciicircum,	XK_acircumflex},
	{XK_a, XK_asciitilde,	XK_atilde},
	{XK_a, XK_quotedbl,		XK_adiaeresis},

	{XK_C, XK_grave,		XK_Ccedilla},
	{XK_C, XK_acute,		XK_Ccedilla},

	{XK_c, XK_grave,		XK_ccedilla},
	{XK_c, XK_acute,		XK_ccedilla},

	{XK_E, XK_grave,		XK_Egrave},
	{XK_E, XK_acute,		XK_Eacute},
	{XK_E, XK_apostrophe,	XK_Eacute},
	{XK_E, XK_asciicircum,	XK_Ecircumflex},
	{XK_E, XK_quotedbl,		XK_Ediaeresis},

	{XK_e, XK_grave,		XK_egrave},
	{XK_e, XK_acute,		XK_eacute},
	{XK_e, XK_apostrophe,	XK_eacute},
	{XK_e, XK_asciicircum,	XK_ecircumflex},
	{XK_e, XK_quotedbl,		XK_ediaeresis},

	{XK_I, XK_grave,		XK_Igrave},
	{XK_I, XK_acute,		XK_Iacute},
	{XK_I, XK_apostrophe,	XK_Iacute},
	{XK_I, XK_asciicircum,	XK_Icircumflex},
	{XK_I, XK_quotedbl,		XK_Idiaeresis},

	{XK_i, XK_grave,		XK_igrave},
	{XK_i, XK_acute,		XK_iacute},
	{XK_i, XK_apostrophe,	XK_iacute},
	{XK_i, XK_asciicircum,	XK_icircumflex},
	{XK_i, XK_quotedbl,		XK_idiaeresis},

	{XK_N, XK_asciitilde,	XK_Ntilde},
	{XK_n, XK_asciitilde,	XK_ntilde},

	{XK_O, XK_grave,		XK_Ograve},
	{XK_O, XK_acute,		XK_Oacute},
	{XK_O, XK_apostrophe,	XK_Oacute},
	{XK_O, XK_asciicircum,	XK_Ocircumflex},
	{XK_O, XK_asciitilde,	XK_Otilde},
	{XK_O, XK_quotedbl,		XK_Odiaeresis},

	{XK_o, XK_grave,		XK_ograve},
	{XK_o, XK_acute,		XK_oacute},
	{XK_o, XK_apostrophe,	XK_oacute},
	{XK_o, XK_asciicircum,	XK_ocircumflex},
	{XK_o, XK_asciitilde,	XK_otilde},
	{XK_o, XK_quotedbl,		XK_odiaeresis},

	{XK_U, XK_grave,		XK_Ugrave},
	{XK_U, XK_acute,		XK_Uacute},
	{XK_U, XK_apostrophe,	XK_Uacute},
	{XK_U, XK_asciicircum,	XK_Ucircumflex},
	{XK_U, XK_quotedbl,		XK_Udiaeresis},

	{XK_u, XK_grave,		XK_ugrave},
	{XK_u, XK_acute,		XK_uacute},
	{XK_u, XK_apostrophe,	XK_uacute},
	{XK_u, XK_asciicircum,	XK_ucircumflex},
	{XK_u, XK_quotedbl,		XK_udiaeresis},

	{XK_Y, XK_acute,		XK_Yacute},
	{XK_Y, XK_apostrophe,	XK_Yacute},

	{XK_y, XK_acute,		XK_yacute},
	{XK_y, XK_apostrophe,	XK_yacute},
	{XK_y, XK_quotedbl,		XK_ydiaeresis},

	{0, 0, 0},
};

# define DEADKEY_CHAR_NUMBER		(int)((sizeof(dkc_tab) / sizeof(DeadKeyChar)))

#endif	/* USE_DEADKEY */



/*--------------------------------------------------------------------*
 *         BEGIN `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/
#ifdef TRANSPARENT
void           rxvt_toggle_transparency      (rxvt_t*);
#endif
int            rxvt_hotkey_dummy             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_change_title      (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_new_tab           (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_kill_tab          (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_prev_tab          (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_next_tab          (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_prev_atab         (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_1             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_2             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_3             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_4             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_5             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_6             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_7             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_8             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_9             (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_10            (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_11            (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_tab_12            (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_lmove_tab         (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_rmove_tab         (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_dump_screen       (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_inc_opacity       (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_dec_opacity       (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_transparency      (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_hide_tabbar       (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_hide_scrollbar    (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_hide_menubar      (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_hide_button       (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_verybold          (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_hold_exit         (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_broadcast         (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_small_font        (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_large_font        (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_scroll_up         (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_scroll_down       (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_scroll_pgup       (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_scroll_pgdown     (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_save_config       (rxvt_t*, XKeyEvent*);
int            rxvt_hotkey_paste             (rxvt_t*, XKeyEvent*);
int            rxvt_process_hotkeys          (rxvt_t*, KeySym, XKeyEvent*);
void           rxvt_process_keypress         (rxvt_t*, XKeyEvent*);
void           rxvt_clean_cmd_page           (rxvt_t*);
int            rxvt_find_cmd_child           (rxvt_t*, int*);
void           rxvt_check_cmdbuf             (rxvt_t*, int);
unsigned char  rxvt_cmd_getc                 (rxvt_t*, int* page);
#ifdef POINTER_BLANK
void           rxvt_pointer_blank            (rxvt_t*, int);
#endif
void           rxvt_mouse_report             (rxvt_t*, const XButtonEvent*);
void           rxvt_set_bg_focused           (rxvt_t*, int, Bool);
#if defined(MOUSE_WHEEL) && defined(MOUSE_SLIP_WHEELING)
void           rxvt_process_keyrelease       (rxvt_t*, XKeyEvent*);
#endif
void           rxvt_scrollbar_dispatcher     (rxvt_t*, int, XButtonEvent*);
void           rxvt_process_buttonpress      (rxvt_t*, int, XButtonEvent*);
#ifdef MOUSE_WHEEL
void           rxvt_process_wheel_button     (rxvt_t*, int, XButtonEvent*);
#endif
void           rxvt_process_buttonrelease    (rxvt_t*, int, XButtonEvent*);
void           rxvt_process_clientmessage    (rxvt_t*, XClientMessageEvent*);
void           rxvt_process_visibilitynotify (rxvt_t*, XVisibilityEvent*);
#ifdef MONITOR_ENTER_LEAVE
void           rxvt_process_enter            (rxvt_t*, XCrossingEvent*);
void           rxvt_process_leave            (rxvt_t*, XCrossingEvent*);
#endif
void           rxvt_process_focusin          (rxvt_t*, XFocusChangeEvent*);
void           rxvt_process_focusout         (rxvt_t*, XFocusChangeEvent*);
void           rxvt_recalc_szhint            (rxvt_t* r, resize_reason_t reason, unsigned int* p_w, unsigned int* p_h);
void           rxvt_resize_on_font           (rxvt_t* r, char* fontname);
int            rxvt_calc_colrow              (rxvt_t* r, unsigned int width, unsigned int height);
void           rxvt_resize_sub_windows       (rxvt_t* r);
void           rxvt_resize_on_configure      (rxvt_t* r, unsigned int width, unsigned int height);
void           rxvt_process_configurenotify  (rxvt_t*, XConfigureEvent*);
void           rxvt_process_selectionnotify  (rxvt_t*, XSelectionEvent*);
void           rxvt_process_propertynotify   (rxvt_t*, XPropertyEvent*);
int            rxvt_set_opacity              (rxvt_t*);
void           rxvt_process_reparentnotify   (rxvt_t*, XEvent*);
void           rxvt_process_expose           (rxvt_t*, XExposeEvent*);
void           rxvt_process_motionnotify     (rxvt_t*, XEvent*);
void           rxvt_process_x_event          (rxvt_t*, XEvent*);
#ifdef PRINTPIPE
void           rxvt_process_print_pipe       (rxvt_t*, int);
#endif
void           rxvt_process_nonprinting      (rxvt_t*, int, unsigned char);
void           rxvt_process_escape_vt52      (rxvt_t*, int, unsigned char);
void           rxvt_process_escape_seq       (rxvt_t*, int);
void           rxvt_process_csi_seq          (rxvt_t*, int);
#ifndef NO_FRILLS
void           rxvt_process_window_ops       (rxvt_t*, int, const int*, unsigned int);
#endif
unsigned char* rxvt_get_to_st                (rxvt_t*, int, unsigned char*);
void           rxvt_process_dcs_seq          (rxvt_t*, int);
void           rxvt_process_osc_seq          (rxvt_t*, int);
void           rxvt_xwsh_seq                 (rxvt_t*, int, const char*);
void           rxvt_process_xwsh_seq         (rxvt_t*, int);
#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
void           rxvt_refresh_bg_image         (rxvt_t* r, int page);
#endif	/* TRANSPARENT || BACKGROUND_IMAGE */
int            rxvt_privcases                (rxvt_t*, int, int, unsigned long);
void           rxvt_process_terminal_mode    (rxvt_t*, int, int, int, unsigned int, const int*);
void           rxvt_process_sgr_mode         (rxvt_t*, int, unsigned int, const int*);
void           rxvt_process_graphics         (rxvt_t*, int);
/*--------------------------------------------------------------------*
 *         END   `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/



/*----------------------------------------------------------------------*/

#ifdef TRANSPARENT
/* INTPROTO */
void
rxvt_toggle_transparency (rxvt_t* r)
{
	register int	i;


	if (r->Options & Opt_transparent)	{
		DBG_MSG(1, (stderr, "unset background transparency\n"));
		r->Options &= ~Opt_transparent;
		XSetWindowBackground (r->Xdisplay, r->TermWin.parent,
			r->h->global_bg);

		for (i = 0; i <= LTAB(r); i ++)	{
# ifdef BACKGROUND_IMAGE
			if (None != PVTS(r, i)->pixmap)
				XSetWindowBackgroundPixmap (r->Xdisplay,
					PVTS(r, i)->vt, PVTS(r, i)->pixmap);
			else
# endif	/* BACKGROUND_IMAGE */
			XSetWindowBackground (r->Xdisplay, PVTS(r, i)->vt,
				*(PVTS(r, i)->p_bg));
		}

# ifdef HAVE_SCROLLBARS
		if (None != r->scrollBar.win &&
			r->Options & Opt_transparent_scrollbar) {
#  ifdef BACKGROUND_IMAGE
			if (None != r->scrollBar.pixmap)
				XSetWindowBackgroundPixmap (r->Xdisplay,
					r->scrollBar.win, r->scrollBar.pixmap);
			else
#  endif	/* BACKGROUND_IMAGE */
			{
				unsigned long	bg = 0;

				switch (r->scrollBar.style)	{
#  ifdef PLAIN_SCROLLBAR
				case R_SB_PLAIN: bg = r->scrollBar.plain_bg; break;
#  endif
#  ifdef XTERM_SCROLLBAR
				case R_SB_XTERM: bg = r->scrollBar.xterm_bg; break;
#  endif
#  ifdef RXVT_SCROLLBAR
				case R_SB_RXVT:  bg = r->scrollBar.rxvt_bg; break;
#  endif
#  ifdef NEXT_SCROLLBAR
				case R_SB_NEXT:  bg = r->scrollBar.next_bg; break;
#  endif
#  ifdef SGI_SCROLLBAR
				case R_SB_SGI:   bg = r->scrollBar.sgi_bg; break;
#  endif
				default: assert (0);
				}	/* switch */
				XSetWindowBackground (r->Xdisplay, r->scrollBar.win,bg);
			}
		}
# endif	/* HAVE_SCROLLBARS */

# ifdef HAVE_MENUBAR
		if (None != r->menuBar.win &&
			r->Options & Opt_transparent_menubar)	{
#  ifdef BACKGROUND_IMAGE
			if (None != r->menuBar.pixmap)
				XSetWindowBackgroundPixmap (r->Xdisplay,
					r->menuBar.win, r->menuBar.pixmap);
			else
#  endif	/* BACKGROUND_IMAGE */
			XSetWindowBackground (r->Xdisplay, r->menuBar.win,
				r->menuBar.bg);
		}
# endif	/* HAVE_MENUBAR */

		if (r->Options & Opt_transparent_tabbar)	{
# ifdef BACKGROUND_IMAGE
			if (None != r->tabBar.pixmap)
				XSetWindowBackgroundPixmap (r->Xdisplay,
					r->tabBar.win, r->tabBar.pixmap);
			else
# endif	/* BACKGROUND_IMAGE */
			XSetWindowBackground (r->Xdisplay, r->tabBar.win,
				r->tabBar.bg);
		}
	}
	else	{
		DBG_MSG(1, (stderr, "set background transparency\n"));
		r->Options |= Opt_transparent;
		XSetWindowBackgroundPixmap (r->Xdisplay, r->TermWin.parent,
			ParentRelative);
		for (i = 0; i <= LTAB(r); i ++)	{
			XSetWindowBackgroundPixmap (r->Xdisplay,
				PVTS(r, i)->vt, ParentRelative);
		}
# ifdef HAVE_SCROLLBARS
		if (None != r->scrollBar.win &&
			r->Options & Opt_transparent_scrollbar)
			XSetWindowBackgroundPixmap (r->Xdisplay,
				r->scrollBar.win, ParentRelative);
# endif
# ifdef HAVE_MENUBAR
		if (None != r->menuBar.win &&
			r->Options & Opt_transparent_menubar)
			XSetWindowBackgroundPixmap (r->Xdisplay,
				r->menuBar.win, ParentRelative);
# endif
		if (r->Options & Opt_transparent_tabbar)
			XSetWindowBackgroundPixmap (r->Xdisplay,
				r->tabBar.win, ParentRelative);

		XSelectInput(r->Xdisplay, XROOT, PropertyChangeMask);
		rxvt_check_our_parents (r);
	}

	/* Clear all windows */
	XClearWindow (r->Xdisplay, r->TermWin.parent);
# ifdef HAVE_SCROLLBARS
	rxvt_scrollbar_update (r, 0);
# endif
# ifdef HAVE_MENUBAR
	rxvt_menubar_expose (r);
# endif
	rxvt_tabbar_expose (r);
	rxvt_scr_clear (r, ATAB(r));
	rxvt_scr_touch (r, ATAB(r), True);
}
#endif	/* TRANSPARENT */



/* INTPROTO */
int
rxvt_hotkey_dummy (rxvt_t* r, XKeyEvent* ev)
{
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_change_title (rxvt_t* r, XKeyEvent* ev)
{
	if (NULL != r->selection.text)	{
		rxvt_tabbar_set_title (r, ATAB(r), (const unsigned char TAINTED*) r->selection.text);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_new_tab (rxvt_t* r, XKeyEvent* ev)
{
	rxvt_append_page (r, NULL);
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_kill_tab (rxvt_t* r, XKeyEvent* ev)
{
	if (!(r->Options2 & Opt2_protectSecondary) ||
		((r->Options2 & Opt2_protectSecondary) &&
		 (PRIMARY == AVTS(r)->current_screen)))	{
		rxvt_kill_page (r, ATAB(r));
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_prev_tab (rxvt_t* r, XKeyEvent* ev)
{
	if (0 != ATAB(r))	{
		rxvt_activate_page (r, ATAB(r)-1);
		return 1;
	}
	else if (0 != LTAB(r))	{
		rxvt_activate_page (r, LTAB(r));
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_next_tab (rxvt_t* r, XKeyEvent* ev)
{
	if (ATAB(r) != LTAB(r))	{
		rxvt_activate_page (r, ATAB(r)+1);
		return 1;
	}
	else if (0 != LTAB(r))	{
		rxvt_activate_page (r, 0);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_prev_atab (rxvt_t* r, XKeyEvent* ev)
{
	if (PTAB(r) != ATAB(r))	{
		rxvt_activate_page (r, PTAB(r));
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_1 (rxvt_t* r, XKeyEvent* ev)
{
	rxvt_activate_page (r, 0);
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_tab_2 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 1)	{
		rxvt_activate_page (r, 1);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_3 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 2)	{
		rxvt_activate_page (r, 2);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_4 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 3)	{
		rxvt_activate_page (r, 3);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_5 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 4)	{
		rxvt_activate_page (r, 4);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_6 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 5)	{
		rxvt_activate_page (r, 5);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_7 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 6)	{
		rxvt_activate_page (r, 6);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_8 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 7)	{
		rxvt_activate_page (r, 7);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_9 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 8)	{
		rxvt_activate_page (r, 8);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_10 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 9)	{
		rxvt_activate_page (r, 9);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_11 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 10)	{
		rxvt_activate_page (r, 10);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_tab_12 (rxvt_t* r, XKeyEvent* ev)
{
	if (LTAB(r) >= 11)	{
		rxvt_activate_page (r, 11);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_lmove_tab (rxvt_t* r, XKeyEvent* ev)
{
	rxvt_tabbar_move_tab (r, 0);
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_rmove_tab (rxvt_t* r, XKeyEvent* ev)
{
	rxvt_tabbar_move_tab (r, 1);
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_dump_screen (rxvt_t* r, XKeyEvent* ev)
{
#ifdef PRINTPIPE
	int		shft = (ev->state & ShiftMask);
	int		ctrl = (ev->state & ControlMask);
	rxvt_scr_printscreen (r, ATAB(r), ctrl | shft);
	return 1;
#endif
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_inc_opacity (rxvt_t* r, XKeyEvent* ev)
{
	if (None != r->h->xa[XA_NET_WM_WINDOW_OPACITY] &&
		r->TermWin.opacity > 0)	{
		if (r->h->rs[Rs_opacityDegree])	{
			r->TermWin.opacity -= r->TermWin.opacity_degree;
			if (r->TermWin.opacity < 0)
				r->TermWin.opacity = 0;
		}
		else
			r->TermWin.opacity --;
		rxvt_set_opacity (r);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_dec_opacity (rxvt_t* r, XKeyEvent* ev)
{
	if (None != r->h->xa[XA_NET_WM_WINDOW_OPACITY] &&
		r->TermWin.opacity < 100) {
		if (r->h->rs[Rs_opacityDegree])	{
			r->TermWin.opacity += r->TermWin.opacity_degree;
			if (r->TermWin.opacity > 100)
				r->TermWin.opacity = 100;
		}
		else
			r->TermWin.opacity ++;
		rxvt_set_opacity (r);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_transparency (rxvt_t* r, XKeyEvent* ev)
{
#ifdef TRANSPARENT
	rxvt_toggle_transparency (r);
	return 1;
#endif
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_hide_tabbar (rxvt_t* r, XKeyEvent* ev)
{
	if (rxvt_tabbar_visible (r))	{
		if (rxvt_tabbar_hide (r))	{
			rxvt_resize_on_subwin (r, HIDE_TABBAR);
			return 1;
		}
	}
	else	{
		if (rxvt_tabbar_show (r))	{
			rxvt_resize_on_subwin (r, SHOW_TABBAR);
			return 1;
		}
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_hide_scrollbar (rxvt_t* r, XKeyEvent* ev)
{
#ifdef HAVE_SCROLLBARS
	if (rxvt_scrollbar_visible (r))	{
		if (rxvt_scrollbar_hide (r))	{
			rxvt_resize_on_subwin (r, HIDE_SCROLLBAR);
			return 1;
		}
	}
	else	{
		if (rxvt_scrollbar_show (r))	{
			rxvt_resize_on_subwin (r, SHOW_SCROLLBAR);
			return 1;
		}
	}
#endif
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_hide_menubar (rxvt_t* r, XKeyEvent* ev)
{
#ifdef HAVE_MENUBAR
	if (rxvt_menubar_visible (r))	{
		if (rxvt_menubar_hide (r))	{
			rxvt_resize_on_subwin (r, HIDE_MENUBAR);
			return 1;
		}
	}
	else	{
		if (rxvt_menubar_show (r))	{
			rxvt_resize_on_subwin (r, SHOW_MENUBAR);
			return 1;
		}
	}
#endif
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_hide_button (rxvt_t* r, XKeyEvent* ev)
{
	if (r->Options2 & Opt2_hideButtons)
		r->Options2 &= ~Opt2_hideButtons;
	else
		r->Options2 |= Opt2_hideButtons;
	rxvt_tabbar_set_visible_tabs (r);
	if (rxvt_tabbar_visible (r))
		rxvt_tabbar_expose (r);
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_verybold (rxvt_t* r, XKeyEvent* ev)
{
	if (r->Options2 & Opt2_veryBold)
		r->Options2 &= ~Opt2_veryBold;
	else
		r->Options2 |= Opt2_veryBold;
	rxvt_scr_touch (r, ATAB(r), True);
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_hold_exit (rxvt_t* r, XKeyEvent* ev)
{
	if (r->Options2 & Opt2_holdExit)	{
		register int	k;
		for (k = LTAB(r); k>= 0; k --)
			if (PVTS(r, k)->dead && PVTS(r, k)->hold > 1)
				rxvt_remove_page (r, k);
		r->Options2 &= ~Opt2_holdExit;
	}
	else
		r->Options2 |= Opt2_holdExit;
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_broadcast (rxvt_t* r, XKeyEvent* ev)
{
	if (r->Options2 & Opt2_broadcast)
		r->Options2 &= ~Opt2_broadcast;
	else
		r->Options2 |= Opt2_broadcast;
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_small_font (rxvt_t* r, XKeyEvent* ev)
{
	rxvt_resize_on_font (r, FONT_DN);
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_large_font (rxvt_t* r, XKeyEvent* ev)
{
	rxvt_resize_on_font (r, FONT_UP);
	return 1;
}

/* INTPROTO */
int
rxvt_hotkey_scroll_up (rxvt_t* r, XKeyEvent* ev)
{
	if (AVTS(r)->saveLines)	{
		rxvt_scr_page (r, ATAB(r), UP, 1);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_scroll_down (rxvt_t* r, XKeyEvent* ev)
{
	if (AVTS(r)->saveLines)	{
		rxvt_scr_page (r, ATAB(r), DN, 1);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_scroll_pgup (rxvt_t* r, XKeyEvent* ev)
{
	if (AVTS(r)->saveLines)	{
#ifdef PAGING_CONTEXT_LINES
		int		lnsppg = r->TermWin.nrow - PAGING_CONTEXT_LINES;
#else
		int		lnsppg = r->TermWin.nrow * 4 / 5;
#endif

		rxvt_scr_page (r, ATAB(r), UP, lnsppg);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_scroll_pgdown (rxvt_t* r, XKeyEvent* ev)
{
	if (AVTS(r)->saveLines)	{
#ifdef PAGING_CONTEXT_LINES
		int		lnsppg = r->TermWin.nrow - PAGING_CONTEXT_LINES;
#else
		int		lnsppg = r->TermWin.nrow * 4 / 5;
#endif

		rxvt_scr_page (r, ATAB(r), DN, lnsppg);
		return 1;
	}
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_save_config (rxvt_t* r, XKeyEvent* ev)
{
	char	cfile[PATH_MAX] = "";

	if (NULL != r->h->rs[Rs_confFileSave])	{
		STRNCPY (cfile, r->h->rs[Rs_confFileSave], PATH_MAX-1);
		cfile[PATH_MAX-1] = (char) 0;
	}
	else	{
		char*	home = getenv ("HOME");

		if (NULL == home)
			return 0;

		snprintf (cfile, PATH_MAX-1, "%s/%s", home, ".mrxvtrc.save");
		cfile[PATH_MAX-1] = (char) 0;
	}

	return rxvt_save_options (r, cfile);
}

/* INTPROTO */
int
rxvt_hotkey_copy_sel (rxvt_t* r, XKeyEvent* ev)
{
	return 0;
}

/* INTPROTO */
int
rxvt_hotkey_paste_sel (rxvt_t* r, XKeyEvent* ev)
{
	rxvt_selection_request (r, ATAB(r), ev->time, 0, 0);
	return 1;
}


hotkeys_handler_t	hk_handlers[NUM_HKFUNCS];

/* EXTPROTO */
void
rxvt_init_hotkey_handlers (rxvt_t* r)
{
	hk_handlers[HKF_DUMMY].handler			= rxvt_hotkey_dummy;
	hk_handlers[HKF_CHANGE_TITLE].handler	= rxvt_hotkey_change_title;
	hk_handlers[HKF_NEW_TAB	].handler		= rxvt_hotkey_new_tab;
	hk_handlers[HKF_KILL_TAB].handler		= rxvt_hotkey_kill_tab;
	hk_handlers[HKF_PREV_TAB].handler		= rxvt_hotkey_prev_tab;
	hk_handlers[HKF_NEXT_TAB].handler		= rxvt_hotkey_next_tab;
	hk_handlers[HKF_PREV_ATAB].handler		= rxvt_hotkey_prev_atab;
	hk_handlers[HKF_TAB_1].handler			= rxvt_hotkey_tab_1;
	hk_handlers[HKF_TAB_2].handler			= rxvt_hotkey_tab_2;
	hk_handlers[HKF_TAB_3].handler			= rxvt_hotkey_tab_3;
	hk_handlers[HKF_TAB_4].handler			= rxvt_hotkey_tab_4;
	hk_handlers[HKF_TAB_5].handler			= rxvt_hotkey_tab_5;
	hk_handlers[HKF_TAB_6].handler			= rxvt_hotkey_tab_6;
	hk_handlers[HKF_TAB_7].handler			= rxvt_hotkey_tab_7;
	hk_handlers[HKF_TAB_8].handler			= rxvt_hotkey_tab_8;
	hk_handlers[HKF_TAB_9].handler			= rxvt_hotkey_tab_9;
	hk_handlers[HKF_TAB_10].handler			= rxvt_hotkey_tab_10;
	hk_handlers[HKF_TAB_11].handler			= rxvt_hotkey_tab_11;
	hk_handlers[HKF_TAB_12].handler			= rxvt_hotkey_tab_12;
	hk_handlers[HKF_LMOVE_TAB].handler		= rxvt_hotkey_lmove_tab;
	hk_handlers[HKF_RMOVE_TAB].handler		= rxvt_hotkey_rmove_tab;
	hk_handlers[HKF_DUMP_SCREEN].handler	= rxvt_hotkey_dump_screen;
	hk_handlers[HKF_INC_OPACITY].handler	= rxvt_hotkey_inc_opacity;
	hk_handlers[HKF_DEC_OPACITY].handler	= rxvt_hotkey_dec_opacity;
	hk_handlers[HKF_TRANSPARENCY].handler	= rxvt_hotkey_transparency;
	hk_handlers[HKF_HIDE_TABBAR].handler	= rxvt_hotkey_hide_tabbar;
	hk_handlers[HKF_HIDE_SCROLLBAR].handler	= rxvt_hotkey_hide_scrollbar;
	hk_handlers[HKF_HIDE_MENUBAR].handler	= rxvt_hotkey_hide_menubar;
	hk_handlers[HKF_HIDE_BUTTON].handler	= rxvt_hotkey_hide_button;
	hk_handlers[HKF_VERYBOLD].handler		= rxvt_hotkey_verybold;
	hk_handlers[HKF_HOLD_EXIT].handler		= rxvt_hotkey_hold_exit;
	hk_handlers[HKF_BROADCAST].handler		= rxvt_hotkey_broadcast;
	hk_handlers[HKF_SMALL_FONT].handler		= rxvt_hotkey_small_font;
	hk_handlers[HKF_LARGE_FONT].handler		= rxvt_hotkey_large_font;
	hk_handlers[HKF_SCROLL_UP].handler		= rxvt_hotkey_scroll_up;
	hk_handlers[HKF_SCROLL_DOWN].handler	= rxvt_hotkey_scroll_down;
	hk_handlers[HKF_SCROLL_PGUP].handler	= rxvt_hotkey_scroll_pgup;
	hk_handlers[HKF_SCROLL_PGDOWN].handler	= rxvt_hotkey_scroll_pgdown;
	hk_handlers[HKF_SAVE_CONFIG].handler	= rxvt_hotkey_save_config;
	hk_handlers[HKF_COPY_SEL].handler		= rxvt_hotkey_copy_sel;
	hk_handlers[HKF_PASTE_SEL].handler		= rxvt_hotkey_paste_sel;

	hk_handlers[HKF_DUMMY].res			= "hotkey*Dummy";
	hk_handlers[HKF_CHANGE_TITLE].res	= "hotkey*ChangeTitle";
	hk_handlers[HKF_NEW_TAB	].res		= "hotkey*NewTab";
	hk_handlers[HKF_KILL_TAB].res		= "hotkey*KillTab";
	hk_handlers[HKF_PREV_TAB].res		= "hotkey*PrevTab";
	hk_handlers[HKF_NEXT_TAB].res		= "hotkey*NextTab";
	hk_handlers[HKF_PREV_ATAB].res		= "hotkey*PrevActiveTab";
	hk_handlers[HKF_TAB_1].res			= "hotkey*Tab1";
	hk_handlers[HKF_TAB_2].res			= "hotkey*Tab2";
	hk_handlers[HKF_TAB_3].res			= "hotkey*Tab3";
	hk_handlers[HKF_TAB_4].res			= "hotkey*Tab4";
	hk_handlers[HKF_TAB_5].res			= "hotkey*Tab5";
	hk_handlers[HKF_TAB_6].res			= "hotkey*Tab6";
	hk_handlers[HKF_TAB_7].res			= "hotkey*Tab7";
	hk_handlers[HKF_TAB_8].res			= "hotkey*Tab8";
	hk_handlers[HKF_TAB_9].res			= "hotkey*Tab9";
	hk_handlers[HKF_TAB_10].res			= "hotkey*Tab10";
	hk_handlers[HKF_TAB_11].res			= "hotkey*Tab11";
	hk_handlers[HKF_TAB_12].res			= "hotkey*Tab12";
	hk_handlers[HKF_LMOVE_TAB].res		= "hotkey*LeftMoveTab";
	hk_handlers[HKF_RMOVE_TAB].res		= "hotkey*RightMoveTab";
	hk_handlers[HKF_DUMP_SCREEN].res	= "hotkey*DumpScreen";
	hk_handlers[HKF_INC_OPACITY].res	= "hotkey*IncOpacity";
	hk_handlers[HKF_DEC_OPACITY].res	= "hotkey*DecOpacity";
	hk_handlers[HKF_TRANSPARENCY].res	= "hotkey*Transparency";
	hk_handlers[HKF_HIDE_TABBAR].res	= "hotkey*HideTabbar";
	hk_handlers[HKF_HIDE_SCROLLBAR].res	= "hotkey*HideScrollbar";
	hk_handlers[HKF_HIDE_MENUBAR].res	= "hotkey*HideMenubar";
	hk_handlers[HKF_HIDE_BUTTON].res	= "hotkey*HideButton";
	hk_handlers[HKF_VERYBOLD].res		= "hotkey*VeryBold";
	hk_handlers[HKF_HOLD_EXIT].res		= "hotkey*HoldExit";
	hk_handlers[HKF_BROADCAST].res		= "hotkey*Broadcast";
	hk_handlers[HKF_SMALL_FONT].res		= "hotkey*SmallFont";
	hk_handlers[HKF_LARGE_FONT].res		= "hotkey*LargeFont";
	hk_handlers[HKF_SCROLL_UP].res		= "hotkey*ScrollUp";
	hk_handlers[HKF_SCROLL_DOWN].res	= "hotkey*ScrollDown";
	hk_handlers[HKF_SCROLL_PGUP].res	= "hotkey*ScrollPageUp";
	hk_handlers[HKF_SCROLL_PGDOWN].res	= "hotkey*ScrollPageDown";
	hk_handlers[HKF_SAVE_CONFIG].res	= "hotkey*SaveConfig";
	hk_handlers[HKF_COPY_SEL].res		= "hotkey*CopySel";
	hk_handlers[HKF_PASTE_SEL].res		= "hotkey*PasteSel";
}


/*
** If it is a valid hotkey, and we have consumed, return 1;
** otherwise return 0 so that the caller can process it.
*/
/* INTPROTO */
int
rxvt_process_hotkeys (rxvt_t* r, KeySym keysym, XKeyEvent* ev)
{
	register int	i;
	unsigned short	flag = 0;
	int				ctrl = 0, meta = 0, shft = 0;


	/* get keyboard masks */
	shft = (ev->state & ShiftMask);
	ctrl = (ev->state & ControlMask);
	meta = (ev->state & r->h->ModMetaMask);

	/* optimization, quick bypass normal characters */
	if (!ctrl && !meta && shft)	{
		/* ignore Shift+ASCII printable non-space characters */
		if (keysym < 128 && isgraph (keysym))
			return 0;
	}

	if (ctrl != 0)
		HK_SET_CTRL(flag);
	if (meta != 0)
		HK_SET_META(flag);
	if (shft != 0)
		HK_SET_SHFT(flag);

	for (i = 0; i < MAX_HOTKEYS; i ++)	{
		if (HKF_DUMMY == r->hotkeys[i].func)
			break;		/* meet last valid hotkey */
		if (keysym != r->hotkeys[i].keysym)
			continue;	/* keysym does not match */
		if (flag != (r->hotkeys[i].flag & HK_MASK))
			continue;	/* ctrl/meta/shft status does not match */

		/* if hotkey is only effective on primary screen */
		if (HK_IS_PRIMARY(r->hotkeys[i].flag) &&
			PRIMARY != AVTS(r)->current_screen)
			return 0;

		/*
		** Now we have found a hotkey and try to process it. Notice
		** that if we process the hotkey, we must return 1.
		** TODO: what if we do not process the hotkey?
		*/
		return (hk_handlers[r->hotkeys[i].func].handler) (r, ev);
	}
	return 0;	/* not hotkey, or not processed */
}



/* INTPRO */
int
rxvt_0xffxx_keypress (rxvt_t* r, KeySym keysym, int ctrl, int meta, int shft, unsigned char* kbuf)
{
	/*
	** B: beginning of a {} body
	*/
	unsigned int		newlen = 1;

	switch (keysym) {

#ifndef NO_BACKSPACE_KEY
	case XK_BackSpace:
		if (AVTS(r)->PrivateModes & PrivMode_HaveBackSpace) {
			kbuf[0] = (!!(AVTS(r)->PrivateModes & PrivMode_BackSpace) ^ !!ctrl) ? '\b' : '\177';
			kbuf[1] = '\0';
		}
		else
			STRCPY(kbuf, r->h->key_backspace);
# ifdef MULTICHAR_SET
		if ((r->Options & Opt_mc_hack) &&
			AVTS(r)->screen.cur.col > 0) {
			int			 col, row;

			newlen = STRLEN(kbuf);
			col = AVTS(r)->screen.cur.col - 1;
			row = AVTS(r)->screen.cur.row + AVTS(r)->saveLines;
			if (IS_MULTI2(AVTS(r)->screen.rend[row][col]))
				MEMMOVE(kbuf + newlen, kbuf, newlen + 1);
		}
# endif	/* MULTICHAR_SET */
		break;
#endif	/* !NO_BACKSPACE_KEY */


#ifndef NO_DELETE_KEY
	case XK_Delete:
		STRCPY(kbuf, r->h->key_delete);
# ifdef MULTICHAR_SET
		if (r->Options & Opt_mc_hack) {
			int			 col, row;

			newlen = STRLEN(kbuf);
			col = AVTS(r)->screen.cur.col;
			row = AVTS(r)->screen.cur.row + AVTS(r)->saveLines;
			if (IS_MULTI1(AVTS(r)->screen.rend[row][col]))
				MEMMOVE(kbuf + newlen, kbuf, newlen + 1);
			}
# endif	/* MULTICHAR_SET */
		break;
#endif	/* !NO_DELETE_KEY */


	case XK_Tab:
		if (shft)
			STRCPY(kbuf, "\033[Z");
		else {
#ifdef CTRL_TAB_MAKES_META
			if (ctrl)
				meta = 1;
#endif	/* CTRL_TAB_MAKES_META */
#ifdef MOD4_TAB_MAKES_META
			if (ev->state & Mod4Mask)
				meta = 1;
#endif	/* MOD4_TAB_MAKES_META */
			newlen = 0;
		}
		break;


#ifdef XK_KP_Left
	case XK_KP_Up:		/* \033Ox or standard */
	case XK_KP_Down:	/* \033Or or standard */
	case XK_KP_Right:	/* \033Ov or standard */
	case XK_KP_Left:	/* \033Ot or standard */
		if ((AVTS(r)->PrivateModes & PrivMode_aplKP) ?
			!shft : shft) {
			STRCPY(kbuf, "\033OZ");
			kbuf[2] = ("txvr"[keysym - XK_KP_Left]);
			break;
		}
		else
			/* translate to std. cursor key */
			keysym = XK_Left + (keysym - XK_KP_Left);
		/* FALLTHROUGH */
#endif	/* XK_KP_Left */
	case XK_Up:	/* "\033[A" */
	case XK_Down:	/* "\033[B" */
	case XK_Right:	/* "\033[C" */
	case XK_Left:	/* "\033[D" */
		STRCPY(kbuf, "\033[Z");
		kbuf[2] = ("DACB"[keysym - XK_Left]);
		/* do Shift first */
		if (shft)
			kbuf[2] = ("dacb"[keysym - XK_Left]);
		else if (ctrl) {
			kbuf[1] = 'O';
			kbuf[2] = ("dacb"[keysym - XK_Left]);
		}
		else if (AVTS(r)->PrivateModes & PrivMode_aplCUR)
			kbuf[1] = 'O';
#ifdef MULTICHAR_SET
		if (r->Options & Opt_mc_hack) {
			int			 col, row, m;

			col = AVTS(r)->screen.cur.col;
			row = AVTS(r)->screen.cur.row + AVTS(r)->saveLines;
			m = 0;
			if (keysym == XK_Right &&
				IS_MULTI1(AVTS(r)->screen.rend[row][col]))
				m = 1;
			else if (keysym == XK_Left) {
				if (col > 0) {
					if (IS_MULTI2(AVTS(r)->screen.rend[row][col - 1]))
						m = 1;
				}
				else if (AVTS(r)->screen.cur.row > 0) {
					col = AVTS(r)->screen.tlen[--row];
					if (col == -1)
						col = r->TermWin.ncol - 1;
					else
						col--;
					if (col > 0 &&
						IS_MULTI2(AVTS(r)->screen.rend[row][col]))
						m = 1;
				}
			}
			if (m)
				MEMMOVE(kbuf + 3, kbuf, 3 + 1);
		}
#endif	/* MULTICHAR_SET */
		break;


#ifndef UNSHIFTED_SCROLLKEYS
# ifdef XK_KP_Prior
	case XK_KP_Prior:
		/* allow shift to override */
		if ((AVTS(r)->PrivateModes & PrivMode_aplKP) ?
			!shft : shft) {
			STRCPY(kbuf, "\033Oy");
			break;
		}
		/* FALLTHROUGH */
# endif	/* XK_KP_Prior */
	case XK_Prior:
		STRCPY(kbuf, "\033[5~");
		break;
# ifdef XK_KP_Next
	case XK_KP_Next:
		/* allow shift to override */
		if ((AVTS(r)->PrivateModes & PrivMode_aplKP) ?
			!shft : shft) {
			STRCPY(kbuf, "\033Os");
			break;
		}
		/* FALLTHROUGH */
# endif	/* XK_KP_Next */
	case XK_Next:
		STRCPY(kbuf, "\033[6~");
		break;
#endif	/* !UNSHIFTED_SCROLLKEYS */
	case XK_KP_Enter:
		/* allow shift to override */
		if ((AVTS(r)->PrivateModes & PrivMode_aplKP) ?
			!shft : shft) {
			STRCPY(kbuf, "\033OM");
		}
		else {
			kbuf[0] = '\r';
			kbuf[1] = '\0';
		}
		break;

#ifdef XK_KP_Begin
	case XK_KP_Begin:
		STRCPY(kbuf, "\033Ou");
		break;

	case XK_KP_Insert:
		STRCPY(kbuf, "\033Op");
		break;

	case XK_KP_Delete:
		STRCPY(kbuf, "\033On");
		break;
#endif	/* XK_KP_Begin */

	case XK_KP_F1:	/* "\033OP" */
	case XK_KP_F2:	/* "\033OQ" */
	case XK_KP_F3:	/* "\033OR" */
	case XK_KP_F4:	/* "\033OS" */
		STRCPY(kbuf, "\033OP");
		kbuf[2] += (keysym - XK_KP_F1);
		break;

	case XK_KP_Multiply:	/* "\033Oj" : "*" */
	case XK_KP_Add:		/* "\033Ok" : "+" */
	case XK_KP_Separator:	/* "\033Ol" : "," */
	case XK_KP_Subtract:	/* "\033Om" : "-" */
	case XK_KP_Decimal:	/* "\033On" : "." */
	case XK_KP_Divide:	/* "\033Oo" : "/" */
	case XK_KP_0:		/* "\033Op" : "0" */
	case XK_KP_1:		/* "\033Oq" : "1" */
	case XK_KP_2:		/* "\033Or" : "2" */
	case XK_KP_3:		/* "\033Os" : "3" */
	case XK_KP_4:		/* "\033Ot" : "4" */
	case XK_KP_5:		/* "\033Ou" : "5" */
	case XK_KP_6:		/* "\033Ov" : "6" */
	case XK_KP_7:		/* "\033Ow" : "7" */
	case XK_KP_8:		/* "\033Ox" : "8" */
	case XK_KP_9:		/* "\033Oy" : "9" */
		/* allow shift to override */
		if ((AVTS(r)->PrivateModes & PrivMode_aplKP) ? !shft : shft) {
			STRCPY(kbuf, "\033Oj");
			kbuf[2] += (keysym - XK_KP_Multiply);
		}
		else {
			kbuf[0] = ('*' + (keysym - XK_KP_Multiply));
			kbuf[1] = '\0';
		}
		break;

	case XK_Find:
		STRCPY(kbuf, "\033[1~");
		break;
	case XK_Insert:
		STRCPY(kbuf, "\033[2~");
		break;
#ifdef DXK_Remove		/* support for DEC remove like key */
	case DXK_Remove:
		/* FALLTHROUGH */
#endif	/* DXK_Remove */
	case XK_Execute:
		STRCPY(kbuf, "\033[3~");
		break;
	case XK_Select:
		STRCPY(kbuf, "\033[4~");
		break;
#ifdef XK_KP_End
	case XK_KP_End:
		/* allow shift to override */
		if ((AVTS(r)->PrivateModes & PrivMode_aplKP) ?
			!shft : shft) {
			STRCPY(kbuf, "\033Oq");
			break;
		}
		/* FALLTHROUGH */
#endif	/* XK_KP_End */
	case XK_End:
		if (r->Options2 & Opt2_linuxHomeEndKey)
			STRCPY(kbuf, KS_END_LINUX);
		else
			STRCPY(kbuf, KS_END);
		break;
#ifdef XK_KP_Home
	case XK_KP_Home:
		/* allow shift to override */
		if ((AVTS(r)->PrivateModes & PrivMode_aplKP) ?
			!shft : shft) {
			STRCPY(kbuf, "\033Ow");
			break;
		}
		/* FALLTHROUGH */
#endif	/* XK_KP_Home */
	case XK_Home:
		if (r->Options2 & Opt2_linuxHomeEndKey)
			STRCPY(kbuf, KS_HOME_LINUX);
		else
			STRCPY(kbuf, KS_HOME);
		break;

#define FKEY(n, fkey)							\
	sprintf((char *)kbuf,"\033[%2d~", (int)((n) + (keysym - fkey)))

	case XK_F1:	/* "\033[11~" */
	case XK_F2:	/* "\033[12~" */
	case XK_F3:	/* "\033[13~" */
	case XK_F4:	/* "\033[14~" */
		if (TERMENV_XTERM == AVTS(r)->termenv)	{
			STRCPY(kbuf, "\033OP");
			kbuf[2] += (keysym - XK_F1);
			break;
		}
		/* FALL THROUGH */
	case XK_F5:	/* "\033[15~" */
		FKEY(11, XK_F1);
		break;
	case XK_F6:	/* "\033[17~" */
	case XK_F7:	/* "\033[18~" */
	case XK_F8:	/* "\033[19~" */
	case XK_F9:	/* "\033[20~" */
	case XK_F10:	/* "\033[21~" */
		FKEY(17, XK_F6);
		break;
	case XK_F11:	/* "\033[23~" */
	case XK_F12:	/* "\033[24~" */
	case XK_F13:	/* "\033[25~" */
	case XK_F14:	/* "\033[26~" */
		FKEY(23, XK_F11);
		break;
	case XK_F15:	/* "\033[28~" */
	case XK_F16:	/* "\033[29~" */
		FKEY(28, XK_F15);
		break;
	case XK_Help:	/* "\033[28~" */
		FKEY(28, XK_Help);
		break;
	case XK_Menu:	/* "\033[29~" */
		FKEY(29, XK_Menu);
		break;
	case XK_F17:	/* "\033[31~" */
	case XK_F18:	/* "\033[32~" */
	case XK_F19:	/* "\033[33~" */
	case XK_F20:	/* "\033[34~" */
	case XK_F21:	/* "\033[35~" */
	case XK_F22:	/* "\033[36~" */
	case XK_F23:	/* "\033[37~" */
	case XK_F24:	/* "\033[38~" */
	case XK_F25:	/* "\033[39~" */
	case XK_F26:	/* "\033[40~" */
	case XK_F27:	/* "\033[41~" */
	case XK_F28:	/* "\033[42~" */
	case XK_F29:	/* "\033[43~" */
	case XK_F30:	/* "\033[44~" */
	case XK_F31:	/* "\033[45~" */
	case XK_F32:	/* "\033[46~" */
	case XK_F33:	/* "\033[47~" */
	case XK_F34:	/* "\033[48~" */
	case XK_F35:	/* "\033[49~" */
		FKEY(31, XK_F17);
		break;
#undef FKEY
	default:
		newlen = 0;
		break;
	}	/* switch (keysym) */


	if (newlen)
		return (STRLEN(kbuf));
	else
		return (-1);
	/*
	** B: end of a {} body
	*/
}


/*{{{ Convert the keypress event into a string */
/* INTPROTO */
void
rxvt_process_keypress (rxvt_t* r, XKeyEvent *ev)
{
	int				ctrl, meta, shft, len;
	KeySym			keysym;
#ifdef USE_DEADKEY
	static KeySym	accent = 0;
#endif	/* USE_DEADKEY */
#ifdef DEBUG_CMD
	static int		debug_key = 1;	/* accessible by a debugger only */
#endif	/* DEBUG_CMD */
#ifdef USE_XIM
	int				valid_keysym = 0;
#endif	/* USE_XIM */
	unsigned char*	kbuf = r->h->kbuf;


	DBG_MSG(2, (stderr, "KeyPress event\n"));
	/*
	** use Num_Lock to toggle Keypad on/off.  If Num_Lock is off,
	** allow an escape sequence to toggle the Keypad. Always permit
	** `shift' to override the current setting
	*/
	shft = (ev->state & ShiftMask);
	ctrl = (ev->state & ControlMask);
	meta = (ev->state & r->h->ModMetaMask);
	if (r->numlock_state || (ev->state & r->h->ModNumLockMask)) {
		r->numlock_state = (ev->state & r->h->ModNumLockMask);
		PrivMode((!r->numlock_state), PrivMode_aplKP, ATAB(r));
	}
#ifdef USE_XIM
	if (r->h->Input_Context != NULL) {
		Status		  status_return;

		kbuf[0] = '\0';
		len = XmbLookupString(r->h->Input_Context, ev, (char *)kbuf,
				  KBUFSZ, &keysym, &status_return);
		valid_keysym = ((status_return == XLookupKeySym) ||
						(status_return == XLookupBoth));
	}
	else {
		valid_keysym = 1;
#endif	/* USE_XIM */

		/*
		******************************************************
		** Begin of part that is used when XIM is disabled
		*/
		len = XLookupString(ev, (char *)kbuf, KBUFSZ, &keysym,
			&r->h->compose);
		/*
		** map unmapped Latin[2-4]/Katakana/Arabic/Cyrillic/Greek
		** entries -> Latin1. good for installations with correct
		** fonts, but without XLOCALE
		*/
		if (!len) {
			if ((keysym >= 0x0100) && (keysym < 0x0800)) {
				kbuf[0] = (keysym & 0xFF);
				kbuf[1] = '\0';
				len = 1;
			}
			else
				kbuf[0] = '\0';
		}
		/*
		** End of part that is used when XIM is disabled
		******************************************************
		*/

#ifdef USE_XIM
	}
#endif	/* USE_XIM */


#ifdef USE_DEADKEY
	if (0 != accent)	{
		if (!ctrl && !meta /* NO ctrl or meta */ && (
			XK_A == keysym || XK_a == keysym ||
			XK_C == keysym || XK_c == keysym ||
			XK_E == keysym || XK_e == keysym ||
			XK_I == keysym || XK_i == keysym ||
			XK_N == keysym || XK_n == keysym ||
			XK_O == keysym || XK_o == keysym ||
			XK_U == keysym || XK_u == keysym ||
			XK_Y == keysym || XK_y == keysym) )	{
			register int	idx;
			KeySym	dk;

			/* dead key + space -> dead key itself */
			switch (accent)	{
			case XK_dead_grave:			/* ` */
				dk = XK_grave;		break;
			case XK_dead_acute:			/* ' */
				dk = XK_acute;		break;
			case XK_dead_circumflex:	/* ^ */
				dk = XK_asciicircum;break;
			case XK_dead_diaeresis:		/* " */
				dk = XK_quotedbl;	break;
			case XK_dead_tilde:			/* ~ */
				dk = XK_asciitilde; break;
			}	/* switch */

			for (idx = 0; idx < DEADKEY_CHAR_NUMBER; idx++)	{
				if (keysym == dkc_tab[idx].ks &&
					dk == dkc_tab[idx].dk)	{
					kbuf[0] = (unsigned char) dkc_tab[idx].ach;
					break;
				}
			}
			assert (0 != kbuf[0]);	/* impossible */

			len = 1;
			accent = 0;	/* clear accent anyway */
		}	/* if */
		else if (!ctrl && !meta &&	/* NO ctrl or meta */
			(XK_space == keysym || accent == keysym))	{
			KeySym	dk;

			/*
			** dead key + space -> dead key itself
			** dead key ^ 2 -> dead key itself
			** change the keysym so as to print out the dead key
			*/
			switch (accent)	{
			case XK_dead_grave:			/* ` */
				keysym = dk = XK_grave;			break;
			case XK_dead_acute:			/* ' */
				keysym = dk = XK_apostrophe;	break;
			case XK_dead_circumflex:	/* ^ */
				keysym = dk = XK_asciicircum;	break;
			case XK_dead_diaeresis:		/* " */
				keysym = dk = XK_quotedbl;		break;
			case XK_dead_tilde:			/* ~ */
				keysym = dk = XK_asciitilde;	break;
			}	/* switch */
			kbuf[0] = (unsigned char) dk;

			len = 1;
			accent = 0;	/* clear accent anyway */
		}
		else if (!ctrl && !meta && 0 == len &&
			(XK_Shift_L == keysym || XK_Shift_R == keysym))	{
			;	/* do NOT clear accent when only shft is pressed */
		}
		else	{
			accent = 0;	/* clear accent anyway */
		}
	}	/* 0 != accent */
#endif	/* USE_DEADKEY */


	/*
	** V: beginning of valid_keysym (1)
	*/
#ifdef USE_XIM
	if (valid_keysym)
#endif	/* USE_XIM */
	{
#ifdef DEBUG_VERBOSE
		DBG_MSG(2, (stderr, "ctrl-meta-shft-keysym: %d-%d-%d-%x\n", 
			ctrl, meta, shft, (int) keysym));
#endif	/* DEBUG_VERBOSE */

		/* for some backwards compatibility */
#if defined(HOTKEY_CTRL) || defined(HOTKEY_META)
# ifdef HOTKEY_CTRL
		if (ctrl) {
# else
		if (meta) {
# endif	/* HOTKEY_CTRL */
			if (keysym == r->h->ks_bigfont) {
				rxvt_resize_on_font (r, FONT_UP);
				return;
			}
			else if (keysym == r->h->ks_smallfont) {
				rxvt_resize_on_font (r, FONT_DN);
				return;
			}
		}
#endif	/* HOTKEY_CTRL || HOTKEY_META */


		/* process hotkeys */
		if (ctrl || meta || shft)	/* optimization */
			if (rxvt_process_hotkeys (r, keysym, ev))
				return;

		if (AVTS(r)->saveLines) {
#ifdef UNSHIFTED_SCROLLKEYS
			if (!ctrl && !meta) {
#else
			if (IS_SCROLL_MOD) {
#endif	/* UNSHIFTED_SCROLLKEYS */
				int			 lnsppg;

#ifdef PAGING_CONTEXT_LINES
				lnsppg = r->TermWin.nrow - PAGING_CONTEXT_LINES;
#else
				lnsppg = r->TermWin.nrow * 4 / 5;
#endif	/* PAGING_CONTEXT_LINES */
				if (keysym == XK_Prior) {
					rxvt_scr_page(r, ATAB(r), UP, lnsppg);
					return;
				}
				else if (keysym == XK_Next) {
					rxvt_scr_page(r, ATAB(r), DN, lnsppg);
					return;
				}
			}

#ifdef SCROLL_ON_UPDOWN_KEYS
			if (IS_SCROLL_MOD) {
				if (keysym == XK_Up) {
					rxvt_scr_page(r, ATAB(r), UP, 1);
					return;
				}
				else if (keysym == XK_Down) {
					rxvt_scr_page(r, ATAB(r), DN, 1);
					return;
				}
			}
#endif	/* SCROLL_ON_UPDOWN_KEYS */

#ifdef SCROLL_ON_HOMEEND_KEYS
			if (IS_SCROLL_MOD) {
				if (keysym == XK_Home) {
					rxvt_scr_move_to(r, ATAB(r), 0, 1);
					return;
				}
				else if (keysym == XK_End) {
					rxvt_scr_move_to(r, ATAB(r), 1, 0);
					return;
				}
			}
#endif	/* SCROLL_ON_HOMEEND_KEYS */
		}	/* if (AVTS(r)->saveLines) */


		if (shft) {
			/* Shift + F1 - F10 generates F11 - F20 */
			if (keysym >= XK_F1 && keysym <= XK_F10) {
				keysym += (XK_F11 - XK_F1);
				shft = 0;	/* turn off Shift */
			}
			else if (!ctrl && !meta &&
				(AVTS(r)->PrivateModes & PrivMode_ShiftKeys)) {
				switch (keysym) {
					/* normal XTerm key bindings */
					case XK_Insert:
						/* Shift+Insert = paste mouse selection */
						rxvt_selection_request(r, ATAB(r), ev->time, 0, 0);
						return;
	
					/* rxvt extras */
					case XK_KP_Add:
						/* Shift+KP_Add = bigger font */
						rxvt_resize_on_font (r, FONT_UP);
						return;
					case XK_KP_Subtract:
						/* Shift+KP_Subtract = smaller font */
						rxvt_resize_on_font (r, FONT_DN);
						return;
				}
			}
		}
	

#ifdef PRINTPIPE
		if (keysym == XK_Print) {
			rxvt_scr_printscreen(r, ATAB(r), ctrl | shft);
			return;
		}
#endif	/* PRINTPIPE */

	
#ifdef GREEK_SUPPORT
		if (keysym == r->h->ks_greekmodeswith) {
			r->h->greek_mode = !r->h->greek_mode;
			if (r->h->greek_mode) {
				rxvt_xterm_seq(r, ATAB(r), XTerm_title,
					(greek_getmode() == GREEK_ELOT928 ?
					"[Greek: iso]" : "[Greek: ibm]"), CHAR_ST);
				greek_reset();
			}
			else
				rxvt_xterm_seq(r, ATAB(r), XTerm_title,
					APL_NAME "-" VERSION, CHAR_ST);
			return;
		}
#endif	/* GREEK_SUPPORT */

	
		/*
		** At this point, all the keystrokes that have special meaning
		** to us have been handled. If we are in the hold mode, this is
		** the keystroke to exit. Otherwise, return here.
		*/
		if (AVTS(r)->hold > 1)	{
			DBG_MSG(1, (stderr, "exit after hold\n"));
			if (keysym && len)
				rxvt_remove_page (r, ATAB(r));
			return;
		}
	
	
		/*
		** A: begin 0xFFxx keysym
		*/
		if (keysym >= 0xFF00 && keysym <= 0xFFFF) {
#ifdef KEYSYM_RESOURCE
			if (!(shft | ctrl) &&
				r->h->Keysym_map[keysym & 0xFF] != NULL) {
				unsigned int	l;
				const unsigned char *kbuf0;
				const unsigned char ch = C0_ESC;
	
				kbuf0 = (r->h->Keysym_map[keysym & 0xFF]);
				l = (unsigned int)*kbuf0++;
	
				/* escape prefix */
				if (meta)
# ifdef META8_OPTION
					if (r->h->meta_char == C0_ESC)
# endif	/* META8_OPTION */
						rxvt_tt_write(r, ATAB(r), &ch, 1);
				rxvt_tt_write(r, ATAB(r), kbuf0, l);
				return;
			}
			else
#endif	/* KEYSYM_RESOURCE */
			{
				int		newlen = rxvt_0xffxx_keypress (r, keysym,
									ctrl, meta, shft, kbuf);
				if (-1 != newlen)
					len = newlen;
			}
	
#ifdef META8_OPTION
			/*
			** Pass meta for all function keys, if 'meta' option set
			*/
			if (meta && (r->h->meta_char == 0x80) && len > 0)
				kbuf[len - 1] |= 0x80;
#endif	/* META8_OPTION */

		}
		/*
		** A: end of 0xFFxx keysym
		*/


		else if (ctrl && keysym == XK_minus) {
			len = 1;
			kbuf[0] = '\037';	/* Ctrl-Minus generates ^_ (31) */
		}


#if defined(XK_dead_grave) && defined(XK_dead_horn)
		/*
		** ========================================================
		** C: beginning of 0xFE50 - 0xFE62, dead keys
		*/
		else if (!ctrl && !meta &&	/* NO ctrl or meta */
			keysym >= XK_dead_grave && keysym <= XK_dead_horn)	{
# ifdef USE_DEADKEY
			if (XK_dead_grave == keysym ||		/* ` */
				XK_dead_acute == keysym ||		/* ' */
				XK_dead_circumflex == keysym ||	/* ^ */
				XK_dead_diaeresis == keysym ||	/* " */
				XK_dead_tilde == keysym)	{	/* ~ */
				len = 0;
				accent = keysym;
			}
# endif	/* USE_DEADKEY */
		}
		/*
		** C: end of 0xFE50 - 0xFE62, dead keys
		** ========================================================
		*/
#endif /* XK_dead_grave || XK_dead_horn */


		else {
#ifdef META8_OPTION
			/* set 8-bit on */
			if (meta && (r->h->meta_char == 0x80)) {
				unsigned char  *ch;
	
				for (ch = kbuf; ch < kbuf + len; ch++)
					*ch |= 0x80;
				meta = 0;
			}
#endif	/* META8_OPTION */
#ifdef GREEK_SUPPORT
			if (r->h->greek_mode)
				len = greek_xlat(kbuf, len);
#endif	/* GREEK_SUPPORT */
				; /* nil */
		}	/* else */


	}
	/*
	** V: if (valid_keysym)
	*/



	if (len <= 0)
		return;			/* not mapped */

	if (r->Options & Opt_scrollTtyKeypress)	{
		if (AVTS(r)->view_start) {
			AVTS(r)->view_start = 0;
			r->h->want_refresh = 1;
		}
	}


	/*
	** these modifications only affect the static keybuffer
	** pass Shift/Control indicators for function keys ending with `~'
	**
	** eg,
	**   Prior = "ESC[5~"
	**   Shift+Prior = "ESC[5$"
	**   Ctrl+Prior = "ESC[5^"
	**   Ctrl+Shift+Prior = "ESC[5@"
	** Meta adds an Escape prefix (with META8_OPTION, if
	** meta == <escape>).
	*/
	if (kbuf[0] == C0_ESC &&
		kbuf[1] == '[' &&
		kbuf[len - 1] == '~')
		kbuf[len - 1] = (shft ? (ctrl ? '@' : '$') : (ctrl ? '^' : '~'));

	/* escape prefix */
	if (meta
#ifdef META8_OPTION
	&& (r->h->meta_char == C0_ESC)
#endif	/* META8_OPTION */
	) {
		const unsigned char ch = C0_ESC;

		rxvt_tt_write(r, ATAB(r), &ch, 1);
	}


#ifdef DEBUG_CMD
	if (debug_key) {		/* Display keyboard buffer contents */
		char		   *p;
		int			 i;

		fprintf(stderr, "key 0x%04X [%d]: `", (unsigned int) keysym,
			len);
		for (i = 0, p = kbuf; i < len; i++, p++)
			fprintf(stderr, (*p >= ' ' && *p < '\177' ? "%c" :
				"\\%03o"), *p);
		fprintf(stderr, "'\n");
	}
#endif	/* DEBUG_CMD */


	rxvt_tt_write(r, ATAB(r), kbuf, (unsigned int)len);
}
/*}}} */



/*{{{ rxvt_cmd_write(), rxvt_cmd_getc() */
/* attempt to `write' count to the input buffer */
/* EXTPROTO */
unsigned int
rxvt_cmd_write(rxvt_t* r, int page, const unsigned char *str, unsigned int count)
{
	unsigned int	n, s;
	unsigned char*	cmdbuf_base = PVTS(r, page)->cmdbuf_base;
	unsigned char*	cmdbuf_endp = PVTS(r, page)->cmdbuf_endp;
	unsigned char*	cmdbuf_ptr = PVTS(r, page)->cmdbuf_ptr;

	n = cmdbuf_ptr - cmdbuf_base;
	s = cmdbuf_base + (BUFSIZ - 1) - cmdbuf_endp;
	if (n > 0 && s < count) {
		MEMMOVE(cmdbuf_base, cmdbuf_ptr,
			(unsigned int)(cmdbuf_endp - cmdbuf_ptr));
		cmdbuf_ptr = cmdbuf_base;
		cmdbuf_endp -= n;
		s += n;
	}
	if (count > s) {
		rxvt_print_error("data loss: cmd_write too large");
		count = s;
	}
	for (; count--;)
		*cmdbuf_endp++ = *str++;
	PVTS(r, page)->cmdbuf_ptr = cmdbuf_ptr;
	PVTS(r, page)->cmdbuf_endp = cmdbuf_endp;
	assert (PVTS(r, page)->cmdbuf_base <= PVTS(r, page)->cmdbuf_endp);
	return 0;
}



/*
** Be careful the time to call this function. Do NOT call it in
** rxvt_cmd_getc because this can redirect command buffer input
** of one terminal to the other terminals!!!
*/
/* INTPROTO */
void
rxvt_clean_cmd_page (rxvt_t* r)
{
	int			hold_msg = 0;	/* whether title has been changed */
	char*		msg;	/* hold message */


	msg = (char*) r->h->rs[Rs_holdExitText];
	if (NULL == msg)
		msg = " -- Terminal finished, ESC to exit";


	DBG_MSG(1, (stderr, "%d children have died, clean cmd fd\n", r->vt_died));
	while (r->vt_died > 0)	{
		int			dead;	/* child that has died */
		/*
		** We start from the last child because we need to move
		** ahead after removing dead child. This makes it much
		** safer.
		**
		** Why do we need to restart dead value from LTAB(r) again?
		** Because a child may have died when we do something
		** following and changed the value of r->vt_died! This
		** child may be later than any dead children we have
		** examined.
		*/
		for (dead = LTAB(r); dead >= 0; dead--)
			/* only dead children that are not held */
			if (PVTS(r, dead)->dead &&
				(!(r->Options2 & Opt2_holdExit) ||
				 (r->Options2 & Opt2_holdExit &&
				  1 == PVTS(r, dead)->hold)))
				break;
		assert (dead <= LTAB(r));	/* in case */


		if (1 == PVTS(r, dead)->hold)	{
			DBG_MSG(1, (stderr, "hold child %d after it died\n", dead));

			if (!hold_msg)	{
				rxvt_set_term_title (r, (const unsigned char*) msg);
				rxvt_set_icon_name (r, (const unsigned char*) msg);
				hold_msg = 1;	/* do not change title anymore */
			}

			if (r->h->rs[Rs_holdExitText])	{
				/* print the holding exit text on screen */
				rxvt_scr_add_lines(r, dead,
					(const unsigned char*) r->h->rs[Rs_holdExitText],
					1, STRLEN(r->h->rs[Rs_holdExitText]));
				rxvt_scr_refresh (r, dead, SMOOTH_REFRESH);
			}

			/* increase hold number, so next iteration will skip it */
			PVTS(r, dead)->hold ++;
		}
		else
			rxvt_remove_page (r, dead);

		/* reduce number of dead children by -1 */
		r->vt_died --;
	}	/* while loop */
}


/* INTPROTO */
int
rxvt_find_cmd_child (rxvt_t* r, int* p_page)
{
	register int	k;

	assert (NULL != p_page);
	assert (-1 == *p_page);	/* in case */

	if (r->vt_died > 0)	{
		/* if a child has died, try to find and return it */
		for (k = 0; k <= LTAB(r); k ++)	{
			assert (PVTS(r, k)->cmdbuf_base <= PVTS(r, k)->cmdbuf_endp);

			if (PVTS(r, k)->dead &&
				!((r->Options2 & Opt2_holdExit) &&
				  (PVTS(r, k)->hold > 1)) )	{

				*PVTS(r, k)->cmdbuf_endp = (char) 0;
				*p_page = k;
				return 1;
			}

			/* output any pending chars of page's v_buffer */
			if (PVTS(r, k)->v_bufstr < PVTS(r, k)->v_bufptr)
				rxvt_tt_write(r, k, NULL, 0);
		}	/* for loop */
	}
	else	{
		/* Reverse loop direction on each entry. It is used to avoid
		** poor performance on one side of a particularly busy tab if
		** there are a lot of activities in it. This problem is noticed
		** by Carsten Menke (sourceforge bug id 1102791) */
		static int	direction = 1;

		/* reverse the loop direction */
		direction = !direction;

		if (0 == direction)	{
			/* try to find a child with input,
			** from the left to the right */
			for (k = 0; k <= LTAB(r); k ++)	{
				assert (PVTS(r, k)->cmdbuf_base <=
					PVTS(r, k)->cmdbuf_endp);

				/* already have something in some page's buffer */
				if (PVTS(r, k)->cmdbuf_ptr <
					PVTS(r, k)->cmdbuf_endp)	{
					*p_page = k;
					return 1;
				}

				/* output any pending chars of page's v_buffer */
				if (PVTS(r, k)->v_bufstr < PVTS(r, k)->v_bufptr)
					rxvt_tt_write(r, k, NULL, 0);
			}	/* for loop */
		}
		else	{
			/* try to find a child with input,
			** from the right to the left */
			for (k = LTAB(r); k >= 0; k --)	{
				assert (PVTS(r, k)->cmdbuf_base <=
					PVTS(r, k)->cmdbuf_endp);

				/* already have something in some page's buffer */
				if (PVTS(r, k)->cmdbuf_ptr <
					PVTS(r, k)->cmdbuf_endp)	{
					*p_page = k;
					return 1;
				}

				/* output any pending chars of page's v_buffer */
				if (PVTS(r, k)->v_bufstr < PVTS(r, k)->v_bufptr)
					rxvt_tt_write(r, k, NULL, 0);
			}	/* for loop */
		}
	}

	return 0; /* not found */
}


/* INTPROTO */
void
rxvt_check_cmdbuf (rxvt_t* r, int page)
{
	if (PVTS(r, page)->cmdbuf_ptr == PVTS(r, page)->cmdbuf_endp) {
		/* If there is no data in the buffer, reset it to
		** the beginning of the buffer */
		assert (PVTS(r, page)->cmdbuf_base <=
			PVTS(r, page)->cmdbuf_endp);
		PVTS(r, page)->cmdbuf_ptr =
		PVTS(r, page)->cmdbuf_endp =
		PVTS(r, page)->cmdbuf_base;
		assert (PVTS(r, page)->cmdbuf_base <=
			PVTS(r, page)->cmdbuf_endp);
	}
	else
	if ((BUFSIZ - 1) ==
		(PVTS(r, page)->cmdbuf_endp - PVTS(r, page)->cmdbuf_base) &&
		(PVTS(r, page)->cmdbuf_ptr > PVTS(r, page)->cmdbuf_base))	{
		/* If there is space at beginning of the buffer, but
		** not space at the end of the buffer, move the
		** content of buffer forward to free space */
		unsigned int	n =
			PVTS(r, page)->cmdbuf_ptr - PVTS(r, page)->cmdbuf_base;
		unsigned int	len =
			PVTS(r, page)->cmdbuf_endp - PVTS(r, page)->cmdbuf_ptr;

		assert (n == BUFSIZ - 1);
		assert (PVTS(r, page)->cmdbuf_ptr <
				PVTS(r, page)->cmdbuf_endp);
		MEMMOVE(PVTS(r, page)->cmdbuf_base, PVTS(r, page)->cmdbuf_ptr,
			len);
		PVTS(r, page)->cmdbuf_ptr = PVTS(r, page)->cmdbuf_base;
		PVTS(r, page)->cmdbuf_endp -= n;
		assert (PVTS(r, page)->cmdbuf_base <=
			PVTS(r, page)->cmdbuf_endp);
	}
}


/*
** rxvt_cmd_getc() - Return next input character
** Return the next input character after first passing any keyboard
** input to the command.
*/
/* INTPROTO */
unsigned char
rxvt_cmd_getc(rxvt_t *r, int* p_page)
{
	int				page = *p_page;
#define TIMEOUT_USEC	5000
	fd_set			readfds;
	int				quick_timeout, select_res;
#ifdef POINTER_BLANK
	int				want_motion_time = 0;
#endif
#ifdef CURSOR_BLINK
	int				want_keypress_time = 0;
#endif
	struct timeval	value;
#if defined(POINTER_BLANK) || defined(CURSOR_BLINK)
	struct timeval	tp;
#endif
	struct rxvt_hidden *h = r->h;
	register int	i;


	while (1) {
		/* loop until we can return something */
	
		if (-1 != page)	{
			assert (PVTS(r, page)->cmdbuf_base <=
				PVTS(r, page)->cmdbuf_endp);
			/* already have something in the buffer */
			if (PVTS(r, page)->cmdbuf_ptr < PVTS(r, page)->cmdbuf_endp)
				return *(PVTS(r, page)->cmdbuf_ptr)++;

			/* output any pending chars of page's v_buffer */
			if (PVTS(r, page)->v_bufstr < PVTS(r, page)->v_bufptr)
				rxvt_tt_write(r, page, NULL, 0);
		}
		else	{
			assert (AVTS(r)->cmdbuf_base <= AVTS(r)->cmdbuf_endp);
			/* if -1 == page, we only process the active tab here */
			if (AVTS(r)->cmdbuf_ptr < AVTS(r)->cmdbuf_endp)	{
				*p_page = ATAB(r);
				return *(AVTS(r)->cmdbuf_ptr)++;
			}

			/* output any pending chars of page's v_buffer */
			if (AVTS(r)->v_bufstr < AVTS(r)->v_bufptr)
				rxvt_tt_write(r, ATAB(r), NULL, 0);

			/*
			** if there is no data in active tab, we go to process
			** the X events before trying to find a tab that has
			** some input/output. this should improve the response
			** performance of the active tab.
			*/
		}
	
#if defined(POINTER_BLANK) || defined(CURSOR_BLINK)
		/* presume == 0 implies time not yet retrieved */
		tp.tv_sec = tp.tv_usec = 0;	
#endif	/* POINTER_BLANK || CURSOR_BLINK */
#ifdef CURSOR_BLINK
		want_keypress_time = 0;
#endif	/* CURSOR_BLINK */
#ifdef POINTER_BLANK
		if (r->Options & Opt_pointerBlank)
			want_motion_time = 0;
#endif	/* POINTER_BLANK */
	

		/* process all pending X events */
		while (XPending(r->Xdisplay)) {
			XEvent		  xev;
	
			XNextEvent(r->Xdisplay, &xev);


#ifdef CURSOR_BLINK
			if ((r->Options & Opt_cursorBlink) &&
				xev.type == KeyPress) {
				if (h->hidden_cursor) {
					DBG_MSG(1, (stderr,"** hide cursor on keypress\n"));
					h->hidden_cursor = 0;
					h->want_refresh = 1;
				}
				want_keypress_time = 1;
			}
#endif	/* CURSOR_BLINK */
	
#ifdef POINTER_BLANK
			if ((r->Options & Opt_pointerBlank) &&
				(h->pointerBlankDelay > 0)) {
				if (xev.type == MotionNotify ||
					xev.type == ButtonPress ||
					xev.type == ButtonRelease) {
					/* only work for current active tab */
					if (AVTS(r)->hidden_pointer)
						rxvt_pointer_unblank(r, ATAB(r));
					want_motion_time = 1;
				}
				/* only work for current active tab */
				if (xev.type == KeyPress &&
					!AVTS(r)->hidden_pointer)
					rxvt_pointer_blank(r, ATAB(r));
			}
#endif	/* POINTER_BLANK */


#ifdef USE_XIM
			if (r->h->Input_Context != NULL)	{
				if (!XFilterEvent(&xev, xev.xany.window))
					rxvt_process_x_event(r, &xev);
				h->event_type = xev.type;
			}
			else
#endif	/* USE_XIM */
			rxvt_process_x_event(r, &xev);


			/* In case button actions pushed chars to cmdbuf. */
			if (-1 != page)	{
				assert (PVTS(r, page)->cmdbuf_base <=
					PVTS(r, page)->cmdbuf_endp);

				if (PVTS(r, page)->cmdbuf_ptr <
					PVTS(r, page)->cmdbuf_endp)
					return *(PVTS(r, page)->cmdbuf_ptr)++;
			}
			else	{
				assert (AVTS(r)->cmdbuf_base <= AVTS(r)->cmdbuf_endp);
				/*
				** -1 == page, only try the active tab here. we will
				** handle inactive tabs after processed all X events
				*/
				if (AVTS(r)->cmdbuf_ptr < AVTS(r)->cmdbuf_endp)	{
					*p_page = ATAB(r);
					return *(AVTS(r)->cmdbuf_ptr)++;
				}
				/*
				** Notice that there might be something BAD here: the
				** active tab is changed by user interaction and data
				** are pushed into previous active tab (now inactive).
				** Need to study on this in the future.
				*/
			}
		}	/* while ((XPending(r->Xdisplay)) */


		if (-1 == page)	{
			/* in case -1 == page, and there's no X events to process.
			** we will not go to the select call if there's already
			** input/output in some tabs. to reach here, we have tried
			** active tab but with no luck. */
			if (rxvt_find_cmd_child (r, p_page))
				return *(PVTS(r, *p_page)->cmdbuf_ptr)++;
		}

#ifdef CURSOR_BLINK
		if (want_keypress_time) {
			/* reset last cursor change time on keypress event */
			(void) gettimeofday (&tp, NULL);
			DBG_MSG(3, (stderr,"** init cursor time on keypress\n"));
			h->lastcursorchange.tv_sec = tp.tv_sec;
			h->lastcursorchange.tv_usec = tp.tv_usec;
		}
#endif	/* CURSOR_BLINK */
	
#ifdef POINTER_BLANK
		if ((r->Options & Opt_pointerBlank) && want_motion_time) {
			(void) gettimeofday (&tp, NULL);
			h->lastmotion.tv_sec = tp.tv_sec;
			h->lastmotion.tv_usec = tp.tv_usec;
		}
#endif	/* POINTER_BLANK */
	
		/*
		** the command input buffer is empty and we have no pending
		** X events
		*/
		quick_timeout = 0;
	
#if defined(MOUSE_WHEEL) && defined(MOUSE_SLIP_WHEELING)
		if (h->mouse_slip_wheel_speed) {
			quick_timeout = 1;
			/* only work for current active tab */
			if (!h->mouse_slip_wheel_delay-- &&
				rxvt_scr_page(r, ATAB(r),
					h->mouse_slip_wheel_speed >0 ? UP : DN,
					abs(h->mouse_slip_wheel_speed))) {
				h->mouse_slip_wheel_delay = SCROLLBAR_CONTINUOUS_DELAY;
				h->refresh_type |= SMOOTH_REFRESH;
				h->want_refresh = 1;
			}
		}
#endif /* MOUSE_WHEEL && MOUSE_SLIP_WHEELING */
	
#ifdef SELECTION_SCROLLING
		if (h->pending_scroll_selection) {
			quick_timeout = 1;
			/* only work for current active tab */
			if (!h->scroll_selection_delay-- &&
				rxvt_scr_page(r, ATAB(r), h->scroll_selection_dir,
					h->scroll_selection_lines)) {
				rxvt_selection_extend(r, ATAB(r), h->selection_save_x,
					h->selection_save_y, h->selection_save_state);
				h->scroll_selection_delay = SCROLLBAR_CONTINUOUS_DELAY;
				h->refresh_type |= SMOOTH_REFRESH;
				h->want_refresh = 1;
			}
		}
#endif	/* SELECTION_SCROLLING */

#ifdef HAVE_SCROLLBARS
# ifndef NO_SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
		if (scrollbar_isUp() || scrollbar_isDn()) {
			quick_timeout = 1;
			/* only work for current active tab */
			if (!h->scroll_arrow_delay-- &&
				rxvt_scr_page(r, ATAB(r), scrollbar_isUp()?UP:DN, 1)) {
				h->scroll_arrow_delay = SCROLLBAR_CONTINUOUS_DELAY;
				h->refresh_type |= SMOOTH_REFRESH;
				h->want_refresh = 1;
			}
		}
# endif	/* NO_SCROLLBAR_BUTTON_CONTINUAL_SCROLLING */
#endif


		/* Prepare to read in from children's file descriptors */
		FD_ZERO(&readfds);
		for (i = 0; i <= LTAB(r); i ++)	{
			/* remember to skip held childrens */
			if ((r->Options2 & Opt2_holdExit) &&
				(PVTS(r, i)->hold > 1))	{
				DBG_MSG(2,(stderr," not listen on vt[%d].cmd_fd\n",i));
				continue;
			}

			FD_SET(PVTS(r, i)->cmd_fd, &readfds);
			DBG_MSG(2, (stderr, " listen on vt[%d].cmd_fd = %d\n", i,
				PVTS(r, i)->cmd_fd));
		}
		FD_SET(r->Xfd, &readfds);
#ifdef HAVE_X11_SM_SMLIB_H
		if (-1 != r->TermWin.ice_fd)
			FD_SET(r->TermWin.ice_fd, &readfds);
#endif
		value.tv_usec = TIMEOUT_USEC;
		value.tv_sec = 0;
	

		if (!r->TermWin.mapped)
			quick_timeout = 0;
		else {
			quick_timeout |= h->want_refresh;
#ifdef TRANSPARENT
			quick_timeout |= h->want_full_refresh;
#endif	/* TRANSPARENT */
		}
	
#if defined(POINTER_BLANK) || defined(CURSOR_BLINK)
		{
			int			set_quick_timeout = 0;
			long		csdiff, psdiff;
	
			csdiff = psdiff = 60000000L;	/* or, say, LONG_MAX */
# if defined(CURSOR_BLINK)
			if (r->Options & Opt_cursorBlink) {
				DBG_MSG(3, (stderr,"** get cursor time on select\n"));
				(void)gettimeofday(&tp, NULL);
	
				csdiff = (tp.tv_sec - h->lastcursorchange.tv_sec) * 1000000L
					 + tp.tv_usec - h->lastcursorchange.tv_usec;
				if (csdiff > h->blinkInterval) {
					/* XXX: settable blink times */
					h->lastcursorchange.tv_sec = tp.tv_sec;
					h->lastcursorchange.tv_usec = tp.tv_usec;
					h->hidden_cursor = !h->hidden_cursor;
					DBG_MSG(3, (stderr, "%s\n", h->hidden_cursor ?
						"** hide cursor" : "** show cursor"));
					csdiff = 0;
				}
				else
					csdiff = h->blinkInterval - csdiff;
				set_quick_timeout = 1;
			}
# endif	/* CURSOR_BLINK */
	
# if defined(POINTER_BLANK)
			/*
			** If we haven't moved the pointer for a while
			*/
			if ((r->Options & Opt_pointerBlank) &&
				(h->pointerBlankDelay > 0) &&
				(AVTS(r)->hidden_pointer == 0)) {
				long			pdelay;
	
				DBG_MSG(3, (stderr,"** get pointer time on select\n"));
				(void) gettimeofday(&tp, NULL);
				psdiff = (tp.tv_sec - h->lastmotion.tv_sec) * 1000000L
					 + tp.tv_usec - h->lastmotion.tv_usec;
				pdelay = h->pointerBlankDelay * 1000000L;
				/* only work for current active tab */
				if (psdiff >= pdelay)
					rxvt_pointer_blank(r, ATAB(r));
				else {
					set_quick_timeout = 1;
					psdiff = pdelay - psdiff;
				}
			}
# endif	/* POINTER_BLANK */
			if (!quick_timeout && set_quick_timeout) {
				MIN_IT(csdiff, psdiff);
				value.tv_sec =  csdiff / 1000000L;
				value.tv_usec = csdiff % 1000000L;
				quick_timeout = 1;
			}
		}
#endif	/* POINTER_BLANK || CURSOR_BLINK */
	
	
		/* Now begin to read in from children's file descriptors */
		if ((select_res = select(r->num_fds, &readfds, NULL, NULL,
			(quick_timeout ? &value : NULL))) == 0) {
			/* select statement timed out - we're not hard and fast
			** scrolling */
			h->refresh_limit = 1;
		}
#ifdef CURSOR_BLINK
		if (r->Options & Opt_cursorBlink)
			h->want_refresh = 1;
#endif	/* CURSOR_BLINK */


		/*
		** Handle the children that have generate input. Notice in
		** this loop we only process input, but do NOT determine
		** the child we want to return.
		*/
		for (i = 0; i <= LTAB(r); i++)	{
			int				n = 0;
			unsigned int	count, bufsiz;


			/* check next file descriptor if this one has nothing to
			** read in */
			if (!FD_ISSET(PVTS(r, i)->cmd_fd, &readfds))
				continue;

			DBG_MSG(1, (stderr, "reading from shell %d\n", i));

			/* check our command buffer before reading data */
			rxvt_check_cmdbuf (r, i);

			assert (PVTS(r, i)->cmdbuf_base <= PVTS(r, i)->cmdbuf_endp);
			/* The buffer size is the buffer length - used length */
			count = bufsiz = (BUFSIZ - 1) -
				(PVTS(r, i)->cmdbuf_endp - PVTS(r, i)->cmdbuf_base);

			while (count)	{
				DBG_MSG(1, (stderr, "read maximal %d bytes\n", count));
				errno = 0;	/* clear errno */
				n = read (PVTS(r, i)->cmd_fd, PVTS(r, i)->cmdbuf_endp,
						count);
				DBG_MSG(1, (stderr, "read %d bytes\n", n));

				if (n > 0)	{
					/* Update count and buffer pointer */
					count -= n;
					PVTS(r, i)->cmdbuf_endp += n;

					/*
					** check the file descriptor to see if there are
					** further input, this is to avoid blocking on
					** read(), which seems to be an issue when running
					** mc in bash. this will waste several CPU cycles,
					** but it's safer than blocking.
					*/
					FD_ZERO(&readfds);
					FD_SET(PVTS(r, i)->cmd_fd, &readfds);
					value.tv_sec = 0;
					value.tv_usec = 5;	/* time out, 5us */
					select_res = select(r->num_fds, &readfds, NULL,
						NULL, &value);
					if (0 == select_res)	{
						/* time-out, no further data to read */
						DBG_MSG(1, (stderr, "no further data\n"));
						break;
					}
					if (-1 == select_res)	{
						/* error, stop reading */
						DBG_MSG(1, (stderr, "select error\n"));
						break;
					}
					/* continue the next loop iteration */
					DBG_MSG(1, (stderr, "more data to read\n"));
				}
				else if (0 == n)	{
					DBG_MSG(1, (stderr, "Should not happen?\n"));
					break;
				}
				else if (n < 0)	{
					if (errno == EAGAIN ||
						errno == EIO ||		/* cygwin */
						errno == EINVAL)	/* solaris */
						break;
					/*
					** We do not update count and buffer pointer and
					** continue trying read more data in the next
					** loop iteration.
					*/
					DBG_MSG(1, (stderr, "%s\n", strerror(errno)));
				}
			}	/* while (count) */

			assert (PVTS(r, i)->cmdbuf_base <= PVTS(r, i)->cmdbuf_endp);
			/* check if a child died */
			if (PVTS(r, i)->dead /* && errno == EIO*/)
				*PVTS(r, i)->cmdbuf_endp = (char) 0;
			else
			if (n < 0 && errno != EAGAIN)	{
				assert (!PVTS(r, i)->dead);	/* in case */

				/*
				** It seems there is a signal loss if more than one
				** children die at almost the same moment. The result
				** is that rxvt_clean_cmd_page does not flag the dead
				** value for some dead children. Thus, the call of
				** rxvt_cmd_getc (-1) in rxvt_mail_loop will fall
				** into an infinite loop on the path to check the
				** input from dead children since their cmd_fd are
				** not excluded in the select system call.
				** 
				** So we introduce the following hack to fix it. If
				** it finds certain dead children are processed by
				** the select system call, it flags it as dead.
				*/
				if (PVTS(r, i)->cmd_pid == 
					waitpid (PVTS(r, i)->cmd_pid, NULL, WNOHANG))	{
					DBG_MSG(1, (stderr,"signal lost on child %d\n",i));
					PVTS(r, i)->dead = 1;
					if (r->Options2 & Opt2_holdExit)
						PVTS(r, i)->hold = 1;
					*PVTS(r, i)->cmdbuf_endp = (char) 0;

					/* increase vt_died number, there is a possible
					** race condition here (a signal comes in) */
					r->vt_died ++;
				}
			}

			/* highlight inactive tab if there is some input */
			if (bufsiz != count && i != ATAB(r))	{
				rxvt_tabbar_highlight_tab (r, i);
			}
		}	/* for loop */


		if (-1 != page)	{
			assert (PVTS(r, page)->cmdbuf_base <=
				PVTS(r, page)->cmdbuf_endp);
			/* Handle the cases that the child has died */
			if (r->vt_died > 0 && PVTS(r, page)->dead)
				return *(PVTS(r, page)->cmdbuf_ptr)++;

			/* Handle the cases that the child has some input */
			if (PVTS(r, page)->cmdbuf_ptr < PVTS(r, page)->cmdbuf_endp)
				return *(PVTS(r, page)->cmdbuf_ptr)++;
		}
		else	{
			assert (AVTS(r)->cmdbuf_base <= AVTS(r)->cmdbuf_endp);
			/* -1 == page, try the active tab first */
			if (AVTS(r)->cmdbuf_ptr < AVTS(r)->cmdbuf_endp)	{
				*p_page = ATAB(r);
				return *(AVTS(r)->cmdbuf_ptr)++;
			}

			if (rxvt_find_cmd_child (r, p_page))
				return *(PVTS(r, *p_page)->cmdbuf_ptr)++;
		}


#ifdef HAVE_X11_SM_SMLIB_H
		/*
		** ICE file descriptor must be processed after we process
		** all file descriptors of children. Otherwise, if a child
		** exit, IceProcessMessages may hang and make the entire
		** terminal unresponsive.
		*/
		if (-1 != r->TermWin.ice_fd &&
			FD_ISSET (r->TermWin.ice_fd, &readfds))	{
			rxvt_process_ice_msgs (r);
		}
#endif


#ifdef TRANSPARENT
		if (h->want_full_refresh) {
			h->want_full_refresh = 0;
			/* only work for active tab */
			rxvt_scr_clear(r, ATAB(r));
			rxvt_scr_touch(r, ATAB(r), False);
			h->want_refresh = 1;
		}
#endif	/* TRANSPARENT */

		if (h->want_refresh) {
			rxvt_scr_refresh(r, ATAB(r), h->refresh_type);
#ifdef HAVE_SCROLLBARS
			rxvt_scrollbar_update(r, 1);
#endif
#ifdef USE_XIM
			rxvt_IM_send_spot (r);
#endif	/* USE_XIM */
		}	/* if (h->want_refresh) */
	}	/* while (1)	*/
	/* NOTREACHED */
}
/*}}} */


/* EXTPROTO */
void
rxvt_pointer_unblank(rxvt_t* r, int page)
{
	XDefineCursor(r->Xdisplay, PVTS(r, page)->vt, r->term_pointer);
	rxvt_recolour_cursor(r);
#ifdef POINTER_BLANK
	if (!(r->Options & Opt_pointerBlank))
		return;	/* no need to do anything */

	PVTS(r, page)->hidden_pointer = 0;

	if (r->h->pointerBlankDelay > 0) {
		struct timeval  tp;

		(void)gettimeofday(&tp, NULL);
		r->h->lastmotion.tv_sec = tp.tv_sec;
		r->h->lastmotion.tv_usec = tp.tv_usec;
	}
#endif
}


#ifdef POINTER_BLANK
/* INTPROTO */
void
rxvt_pointer_blank(rxvt_t* r, int page)
{
	if ((r->Options & Opt_pointerBlank) &&
		(None != r->h->blank_pointer))	{
		XDefineCursor(r->Xdisplay, PVTS(r, page)->vt,
			r->h->blank_pointer);
		XFlush(r->Xdisplay);
		PVTS(r, page)->hidden_pointer = 1;
	}
}
#endif


/* INTPROTO */
void
rxvt_mouse_report(rxvt_t* r, const XButtonEvent *ev)
{
	int			 button_number, key_state = 0;
	int			 x, y;

	x = ev->x;
	y = ev->y;
	rxvt_pixel_position(r, &x, &y);

	if (r->h->MEvent.button == AnyButton) {
		button_number = 3;
	}
	else {
		button_number = r->h->MEvent.button - Button1;
		/* add 0x3D for wheel events, like xterm does */
		if (button_number >= 3)
			button_number += (64 - 3);
	}

	if (AVTS(r)->PrivateModes & PrivMode_MouseX10) {
		/*
		 * do not report ButtonRelease
		 * no state info allowed
		 */
		key_state = 0;
		if (button_number == 3)
			return;
	}
	else {
		/* XTerm mouse reporting needs these values:
		 *   4 = Shift
		 *   8 = Meta
		 *  16 = Control
		 * plus will add in our own Double-Click reporting
		 *  32 = Double Click
		 */
		key_state = ((r->h->MEvent.state & ShiftMask) ? 4 : 0)
			 + ((r->h->MEvent.state & r->h->ModMetaMask) ? 8 : 0)
			 + ((r->h->MEvent.state & ControlMask) ? 16 : 0);
#ifdef MOUSE_REPORT_DOUBLECLICK
		key_state += ((r->h->MEvent.clicks > 1) ? 32 : 0);
#endif
	}

#ifdef DEBUG_MOUSEREPORT
	fprintf(stderr, "Mouse [");
	if (key_state & 16)
		fputc('C', stderr);
	if (key_state & 4)
		fputc('S', stderr);
	if (key_state & 8)
		fputc('A', stderr);
	if (key_state & 32)
		fputc('2', stderr);
	fprintf(stderr, "]: <%d>, %d/%d\n",
		button_number,
		x + 1,
		y + 1);
#else
	rxvt_tt_printf(r, ATAB(r), "\033[M%c%c%c",
		  (32 + button_number + key_state),
		  (32 + x + 1),
		  (32 + y + 1));
#endif
}


/*
** Before calling rxvt_set_bg_focused, bg and ufbg are already
** restored to correct state
*/
/* INTPROTO */
void
rxvt_set_bg_focused(rxvt_t* r, int page, Bool focus)
{
	XGCValues		gcvalue;


	/* Make sure bg and ufbg are in correct state */
	assert (0 == r->ufbg_switched);
	if (focus)
		rxvt_restore_ufbg_color (r);
	else
		rxvt_switch_ufbg_color (r);
	gcvalue.background = r->PixColors[Color_bg];

# ifdef TRANSPARENT
	if (!(r->Options & Opt_transparent))
# endif	/* TRANSPARENT */
#ifdef BACKGROUND_IMAGE
	if (None == PVTS(r, page)->pixmap)
#endif	/* BACKGROUND_IMAGE */
	{
		XSetBackground(r->Xdisplay, r->TermWin.gc,
			r->PixColors[Color_bg]);
		XSetWindowBackground(r->Xdisplay, PVTS(r, page)->vt,
			r->PixColors[Color_bg]);
	}

#ifdef TRANSPARENT
	if (r->Options & Opt_transparent)
		rxvt_check_our_parents(r);
	else
#endif	/* TRANSPARENT */
#ifdef BACKGROUND_IMAGE
	if (None != PVTS(r, page)->pixmap)	{
		DBG_MSG (1, (stderr, "reset pixmap bg of vt %d\n", page));
		XSetWindowBackgroundPixmap(r->Xdisplay, PVTS(r, page)->vt,
			PVTS(r, page)->pixmap);
	}
#endif	/* BACKGROUND_IMAGE */
	{
		/* Nothing to do, avoid compile error when defined
		** TRANSPARENT but not BACKGROUND_IMAGE
		*/
	}

	/*
	** Set foreground/background color for GC. This is necessary.
	** Otherwise, the old color will be used for drawing the 
	** following text before a color change.
	*/
	XSetForeground (r->Xdisplay, r->TermWin.gc, r->PixColors[Color_fg]);
	XSetBackground (r->Xdisplay, r->TermWin.gc, r->PixColors[Color_bg]);

	rxvt_scr_clear(r, page);
	rxvt_scr_touch(r, page, True);
}



/*
** Individual X Event handlers
*/
#if defined(MOUSE_WHEEL) && defined(MOUSE_SLIP_WHEELING)
/* INTPROTO */
void
rxvt_process_keyrelease(rxvt_t* r, XKeyEvent *ev)
{
	DBG_MSG(2, (stderr, "KeyRelease event\n"));
	if (!(ev->state & ControlMask))
		r->h->mouse_slip_wheel_speed = 0;
	else {
		KeySym		  ks;

		ks = XKeycodeToKeysym(r->Xdisplay, ev->keycode, 0);
		if (ks == XK_Control_L || ks == XK_Control_R)
			r->h->mouse_slip_wheel_speed = 0;
	}
}
#endif



#ifdef HAVE_SCROLLBARS
/* INTPROTO */
void
rxvt_scrollbar_dispatcher (rxvt_t* r, int page, XButtonEvent* ev)
{
	int			reportmode = 0;
	struct rxvt_hidden*	h = r->h;


	if (!h->bypass_keystate)
		reportmode = !!(PVTS(r, page)->PrivateModes & PrivMode_mouse_report);

	scrollbar_setIdle();
	/*
	 * Rxvt-style scrollbar:
	 * move up if mouse is above slider
	 * move dn if mouse is below slider
	 *
	 * XTerm-style scrollbar:
	 * Move display proportional to pointer location
	 * pointer near top -> scroll one line
	 * pointer near bot -> scroll full page
	 */
# ifndef NO_SCROLLBAR_REPORT
	if (reportmode) {
		/*
		 * Mouse report disabled scrollbar:
		 * arrow buttons - send up/down
		 * click on scrollbar - send pageup/down
		 */
		if ((r->scrollBar.style == R_SB_NEXT &&
				scrollbarnext_upButton(ev->y)) ||
			(r->scrollBar.style == R_SB_RXVT &&
				scrollbarrxvt_upButton(ev->y)) ||
			(r->scrollBar.style == R_SB_SGI &&
				scrollbarsgi_upButton(ev->y)))
			rxvt_tt_printf(r, page, "\033[A");
		else
		if ((r->scrollBar.style == R_SB_NEXT &&
					scrollbarnext_dnButton(ev->y)) ||
			(r->scrollBar.style == R_SB_RXVT &&
					scrollbarrxvt_dnButton(ev->y)) ||
			(r->scrollBar.style == R_SB_SGI &&
					scrollbarsgi_dnButton(ev->y)))
			rxvt_tt_printf(r, page, "\033[B");
		else
		{
			switch (ev->button) {
			case Button2:
				rxvt_tt_printf(r, page, "\014");
				break;
			case Button1:
				rxvt_tt_printf(r, page, "\033[6~");
				break;
			case Button3:
				rxvt_tt_printf(r, page, "\033[5~");
				break;
			}
		}
	}
	else
# endif	/* NO_SCROLLBAR_REPORT */
	{
		int		upordown = 0;

		if (r->scrollBar.style == R_SB_NEXT) {
			if (scrollbarnext_upButton(ev->y))
				upordown = -1;	/* up */
			else if (scrollbarnext_dnButton(ev->y))
				upordown = 1;	/* down */
		}
		else
		if (r->scrollBar.style == R_SB_RXVT) {
			if (scrollbarrxvt_upButton(ev->y))
				upordown = -1;	/* up */
			else if (scrollbarrxvt_dnButton(ev->y))
				upordown = 1;	/* down */
		}
		else
		if (r->scrollBar.style == R_SB_SGI) {
			if (scrollbarsgi_upButton(ev->y))
				upordown = -1;	/* up */
			else if (scrollbarsgi_dnButton(ev->y))
				upordown = 1;	/* down */
		}

		if (upordown) { 
#ifndef NO_SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
			h->scroll_arrow_delay = SCROLLBAR_INITIAL_DELAY;
#endif
			if (rxvt_scr_page(r, ATAB(r), upordown < 0?UP:DN,1)) {
				if (upordown < 0)
					scrollbar_setUp();
				else
					scrollbar_setDn();
			}
		}
		else
			switch (ev->button) {
			case Button2:
				switch (h->scrollbar_align) {
				case R_SB_ALIGN_TOP:
					h->csrO = 0;
					break;
				case R_SB_ALIGN_CENTRE:
					h->csrO = (r->scrollBar.bot-r->scrollBar.top)/2;
					break;
				case R_SB_ALIGN_BOTTOM:
					h->csrO = r->scrollBar.bot - r->scrollBar.top;
					break;
				}

				if (r->scrollBar.style == R_SB_XTERM ||
					scrollbar_above_slider(ev->y) ||
					scrollbar_below_slider(ev->y))
					rxvt_scr_move_to(r, page,
						scrollbar_position(ev->y) - h->csrO,
						scrollbar_size());
				scrollbar_setMotion();
			break;

		case Button1:
			if (h->scrollbar_align == R_SB_ALIGN_CENTRE)
				h->csrO = ev->y - r->scrollBar.top;
				/* FALLTHROUGH */

		case Button3:
			if (r->scrollBar.style != R_SB_XTERM) {
				if (scrollbar_above_slider(ev->y))
# ifdef RXVT_SCROLL_FULL
					rxvt_scr_page(r, ATAB(r), UP, r->TermWin.nrow-1);
# else
					rxvt_scr_page(r, ATAB(r), UP, r->TermWin.nrow/4);
# endif
				else if (scrollbar_below_slider(ev->y))
# ifdef RXVT_SCROLL_FULL
					rxvt_scr_page(r, ATAB(r), DN, r->TermWin.nrow - 1);
# else
					rxvt_scr_page(r, ATAB(r), DN, r->TermWin.nrow / 4);
# endif
				else
					scrollbar_setMotion();
			}
			else {
				rxvt_scr_page(r, ATAB(r),
					(ev->button == Button1 ? DN : UP),
					(r->TermWin.nrow * scrollbar_position(ev->y)
						/ scrollbar_size()));
			}
			break;
		}
	}
	return;
}
#endif	/* HAVE_SCROLLBARS */


/* INTPROTO */
void
rxvt_process_buttonpress(rxvt_t* r, int page, XButtonEvent *ev)
{
	int					reportmode = 0, clickintime;
	struct rxvt_hidden*	h = r->h;


	DBG_MSG(2, (stderr, "ButtonPress event\n"));
	h->bypass_keystate = ev->state & (h->ModMetaMask | ShiftMask);
	if (!h->bypass_keystate)
		reportmode = !!(PVTS(r, page)->PrivateModes & PrivMode_mouse_report);

	/*
	** VT window processing of button press
	*/
	if (ev->window == PVTS(r, page)->vt) {
		clickintime = ev->time - h->MEvent.time < MULTICLICK_TIME;
		if (reportmode) {
			/* mouse report from vt window */
			/* save the xbutton state (for ButtonRelease) */
			h->MEvent.state = ev->state;
#ifdef MOUSE_REPORT_DOUBLECLICK
			if (ev->button == h->MEvent.button && clickintime) {
				/* same button, within alloted time */
				h->MEvent.clicks++;
				if (h->MEvent.clicks > 1) {
					/* only report double clicks */
					h->MEvent.clicks = 2;
					rxvt_mouse_report(r, ev);

					/* don't report the release */
					h->MEvent.clicks = 0;
					h->MEvent.button = AnyButton;
				}
			}
			else {
				/* different button, or time expired */
				h->MEvent.clicks = 1;
				h->MEvent.button = ev->button;
				rxvt_mouse_report(r, ev);
			}
#else
			h->MEvent.button = ev->button;
			rxvt_mouse_report(r, ev);
#endif				/* MOUSE_REPORT_DOUBLECLICK */
		}
		else {
			if (ev->button != h->MEvent.button)
				h->MEvent.clicks = 0;
			switch (ev->button) {
			case Button1:
				if (h->MEvent.button == Button1 && clickintime)
					h->MEvent.clicks++;
				else
					h->MEvent.clicks = 1;
				rxvt_selection_click(r, page, h->MEvent.clicks,
					ev->x, ev->y);
				h->MEvent.button = Button1;
				break;

			case Button3:
				if (h->MEvent.button == Button3 && clickintime)
					rxvt_selection_rotate(r, page, ev->x, ev->y);
				else
					rxvt_selection_extend(r, page, ev->x, ev->y, 1);
				h->MEvent.button = Button3;
				break;
			}
		}
		h->MEvent.time = ev->time;
		return;
	}


	/*
	** Tabbar window processing of button press
	*/
	if (rxvt_is_tabbar_win (r, ev->window))
		rxvt_tabbar_dispatcher (r, ev);


#ifdef HAVE_SCROLLBARS
	/*
	** Scrollbar window processing of button press
	*/
	if (rxvt_is_scrollbar_win (r, ev->window))
		rxvt_scrollbar_dispatcher (r, page, ev);
#endif


#ifdef HAVE_MENUBAR
	/*
	** Menubar window processing of button press
	*/
	if (rxvt_is_menubar_win(r, ev->window))
		rxvt_menubar_control(r, ev);
#endif
}



#ifdef MOUSE_WHEEL
/* INTPROTO */
void
rxvt_process_wheel_button(rxvt_t* r, int page, XButtonEvent *ev)
{
	int			 i, v;

	v = (ev->button == Button4) ? UP : DN;
	if (ev->state & ShiftMask)
		i = 1;
	else if ((r->Options & Opt_mouseWheelScrollPage))
		i = r->TermWin.nrow - 1;
	else
		i = 5;

# ifdef MOUSE_SLIP_WHEELING
	if (ev->state & ControlMask) {
		r->h->mouse_slip_wheel_speed += (v ? -1 : 1);
		r->h->mouse_slip_wheel_delay = SCROLLBAR_CONTINUOUS_DELAY;
	}
# endif

# ifdef JUMP_MOUSE_WHEEL
	rxvt_scr_page(r, ATAB(r), v, i);

#  ifdef XFT_SUPPORT
	/* disable screen refresh if XFT antialias is
	** used to improve performance */
	if (!((r->Options & Opt_xft) &&
		  (r->Options2 & Opt2_xftAntialias)))
#  endif	/* XFT_SUPPORT */
	rxvt_scr_refresh(r, page, SMOOTH_REFRESH);
#  ifdef HAVE_SCROLLBARS
	rxvt_scrollbar_update(r, 1);
#  endif	/* HAVE_SCROLLBARS */


# else	/* !JUMP_MOUSE_WHEEL */
	for (; i--;) {
		rxvt_scr_page(r, ATAB(r), v, 1);

#  ifdef XFT_SUPPORT
		/* disable screen refresh if XFT antialias is
		** used to improve performance */
		if (!((r->Options & Opt_xft) &&
			  (r->Options2 & Opt2_xftAntialias)))
#  endif	/* XFT_SUPPORT */
		rxvt_scr_refresh(r, page, SMOOTH_REFRESH);
#  ifdef HAVE_SCROLLBARS
		rxvt_scrollbar_update(r, 1);
#  endif	/* HAVE_SCROLLBARS */
	}
# endif	/* JUMP_MOUSE_WHEEL */
}
#endif	/* MOUSE_WHEEL */


/* INTPROTO */
void
rxvt_process_buttonrelease(rxvt_t* r, int page, XButtonEvent *ev)
{
	int			 reportmode = 0;


	DBG_MSG(2, (stderr, "ButtonRelease event\n"));
	r->h->csrO = 0;		/* reset csr Offset */
	if (!r->h->bypass_keystate)
		reportmode = !!(PVTS(r, page)->PrivateModes & PrivMode_mouse_report);

#ifdef HAVE_SCROLLBARS
	if (scrollbar_isUpDn()) {
		scrollbar_setIdle();
		rxvt_scrollbar_update(r, 0);
# ifndef NO_SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
		r->h->refresh_type &= ~SMOOTH_REFRESH;
# endif
	}
#endif	/* HAVE_SCROLLBARS */

#ifdef SELECTION_SCROLLING
	r->h->pending_scroll_selection=0;
#endif	/* SELECTION_SCROLLING */

	if (ev->window == PVTS(r, page)->vt) {
		if (ev->subwindow == None)	{
			if (reportmode) {
				/* mouse report from vt window */
				/* don't report release of wheel "buttons" */
				if (ev->button >= 4)
					return;
#ifdef MOUSE_REPORT_DOUBLECLICK
				/* only report the release of 'slow' single clicks */
				if (r->h->MEvent.button != AnyButton
					&& (ev->button != r->h->MEvent.button
					|| (ev->time - r->h->MEvent.time
					> MULTICLICK_TIME / 2))) {
					r->h->MEvent.clicks = 0;
					r->h->MEvent.button = AnyButton;
					rxvt_mouse_report(r, ev);
				}
#else	/* MOUSE_REPORT_DOUBLECLICK */
				r->h->MEvent.button = AnyButton;
				rxvt_mouse_report(r, ev);
#endif	/* MOUSE_REPORT_DOUBLECLICK */
				return;
			}
			/*
			 * dumb hack to compensate for the failure of click-and-drag
			 * when overriding mouse reporting
			 */
			if (PVTS(r, page)->PrivateModes & PrivMode_mouse_report
				&& r->h->bypass_keystate
				&& ev->button == Button1 && r->h->MEvent.clicks <= 1)
				rxvt_selection_extend(r, page, ev->x, ev->y, 0);

			switch (ev->button) {
			case Button1:
			case Button3:
				rxvt_selection_make(r, page, ev->time);
				break;
			case Button2:
				rxvt_selection_request(r, page, ev->time, ev->x, ev->y);
				break;
#ifdef MOUSE_WHEEL
			case Button4:
			case Button5:
				rxvt_process_wheel_button (r, page, ev);
				break;
#endif	/* MOUSE_WHEEL */
			}
		}
	}
#ifdef HAVE_MENUBAR
	else if (rxvt_is_menubar_win(r, ev->window))
		rxvt_menubar_control(r, ev);
#endif	/* HAVE_MENUBAR */
}



/* INTPROTO */
void
rxvt_process_clientmessage(rxvt_t* r, XClientMessageEvent* ev)
{
	DBG_MSG(2, (stderr, "ClientMessage event\n"));
	if (ev->format == 32 &&
		(Atom)ev->data.l[0] == r->h->xa[XA_WMDELETEWINDOW])
		rxvt_clean_exit (r);

#ifdef OFFIX_DND
	/* OffiX Dnd (drag 'n' drop) protocol */
	if (ev->xclient.message_type == h->xa[XA_DNDPROTOCOL] &&
		(ev->xclient.data.l[0] == DndFile ||
		ev->xclient.data.l[0] == DndDir ||
		ev->xclient.data.l[0] == DndLink)) {
		/* Get Dnd data */
		Atom			ActualType;
		int				ActualFormat;
		unsigned char*	data;
		unsigned long	Size, RemainingBytes;

		XGetWindowProperty(r->Xdisplay, XROOT,
		   r->h->xa[XA_DNDSELECTION], 0L, 1000000L,
		   False, AnyPropertyType, &ActualType, &ActualFormat,
		   &Size, &RemainingBytes, &data);
		XChangeProperty(r->Xdisplay, XROOT, XA_CUT_BUFFER0,
			XA_STRING, 8, PropModeReplace, data, STRLEN(data));
		rxvt_selection_paste(r, ATAB(r), XROOT, XA_CUT_BUFFER0, True);
		XSetInputFocus(r->Xdisplay, XROOT, RevertToNone, CurrentTime);
	}
#endif		/* OFFIX_DND */
}



/* INTPROTO */
void
rxvt_process_visibilitynotify(rxvt_t* r, XVisibilityEvent* ev)
{
	DBG_MSG(2, (stderr, "VisibilityNotify event\n"));
	switch (ev->state) {
	case VisibilityUnobscured:
		r->h->refresh_type = FAST_REFRESH;
		break;
	case VisibilityPartiallyObscured:
		r->h->refresh_type = SLOW_REFRESH;
		break;
	default:
		r->h->refresh_type = NO_REFRESH;
		break;
	}
}



#ifdef MONITOR_ENTER_LEAVE
/* INTPROTO */
void
rxvt_process_enter (rxvt_t* r, XCrossingEvent* ev)
{
	if (ev->window == r->TermWin.parent)	{
		DBG_MSG(1, (stderr, "Enter event\n"));
		r->TermWin.enter = 1;
	}
}


/* INTPROTO */
void
rxvt_process_leave (rxvt_t* r, XCrossingEvent* ev)
{
	if (ev->window == r->TermWin.parent)	{
		DBG_MSG(1, (stderr, "Leave event\n"));
		r->TermWin.enter = 0;
	}
}
#endif	/* MONITOR_ENTER_LEAVE */


/* INTPROTO */
void
rxvt_process_focusin (rxvt_t* r, XFocusChangeEvent* ev)
{
	if (ev->window == r->TermWin.parent)	{
#ifdef OFF_FOCUS_FADING
		register int	changed = 0;
#endif

		DBG_MSG(2, (stderr, "FocusIn event\n"));
		r->TermWin.focus = 1;

#ifdef USE_XIM
		if (r->h->Input_Context != NULL)
			XSetICFocus(r->h->Input_Context);
#endif

		/* if we have switched bg/ufbg color, restore it */
		rxvt_restore_ufbg_color (r);
#ifdef OFF_FOCUS_FADING
		/* if we have switched to off-focus color, restore it */
		changed = rxvt_restore_pix_color (r);
#endif

		if (ISSET_PIXCOLOR(r->h, Color_ufbg)
#ifdef OFF_FOCUS_FADING
			|| (r->h->rs[Rs_fade] && changed)
#endif
			)	{
			r->h->want_refresh = 1;
			/* before calling rxvt_set_bg_focused, bg and ufbg are
			** already restored to correct state */
			rxvt_set_bg_focused(r, ATAB(r), True);
		}
	}

}



/* INTPROTO */
void
rxvt_process_focusout (rxvt_t* r, XFocusChangeEvent* ev)
{
	if (ev->window == r->TermWin.parent)	{
#ifdef OFF_FOCUS_FADING
		register int	changed = 0;
#endif

		DBG_MSG(2, (stderr, "FocusOut event\n"));
		r->TermWin.focus = 0;

#ifdef USE_XIM
		if (r->h->Input_Context != NULL)
			XUnsetICFocus(r->h->Input_Context);
#endif

		/* if we have switched bg/ufbg color, restore it */
		rxvt_restore_ufbg_color (r);
#ifdef OFF_FOCUS_FADING
		/* if we are using on-focus color, switch it */
		changed = rxvt_switch_pix_color (r);
#endif

		if (ISSET_PIXCOLOR(r->h, Color_ufbg)
#ifdef OFF_FOCUS_FADING
			|| (r->h->rs[Rs_fade] && changed)
#endif
			)	{
			r->h->want_refresh = 1;
			/* before calling rxvt_set_bg_focused, bg and ufbg are
			** already restored to correct state */
			rxvt_set_bg_focused(r, ATAB(r), False);
		}
	}
}



/* Resize windows on showing/hiding sub windows */
/* EXTPROTO */
void
rxvt_resize_on_subwin (rxvt_t* r, resize_reason_t reason)
{
#ifdef SMART_RESIZE
	unsigned int	old_width = r->szHint.width,
					old_height = r->szHint.height;
#endif
	unsigned int	w = r->szHint.width, h = r->szHint.height;

	DBG_MSG(1, (stderr, "rxvt_resize_on_subwin\n"));
	rxvt_recalc_szhint (r, reason, &w, &h);

#ifdef SMART_RESIZE	
	{
		/*
		** resize by Marius Gedminas <marius.gedminas@uosis.mif.vu.lt>
		** reposition window on resize depending on placement on screen
		*/
		int				x, y, x1, y1;
		int				dx, dy;
		unsigned int	unused_w1, unused_h1, unused_b1, unused_d1;
		Window			unused_cr;

		XTranslateCoordinates(r->Xdisplay, r->TermWin.parent, XROOT,
			0, 0, &x, &y, &unused_cr);
		XGetGeometry(r->Xdisplay, r->TermWin.parent, &unused_cr,
			&x1, &y1, &unused_w1, &unused_h1, &unused_b1, &unused_d1);
		/*
		** if XROOT isn't the parent window, a WM will probably have
		** offset our position for handles and decorations. Counter
		** it.
		*/
		if (x1 != x || y1 != y) {
			x -= x1;
			y -= y1;
		}

		x1 = (DisplayWidth(r->Xdisplay, XSCREEN) - old_width) / 2;
		y1 = (DisplayHeight(r->Xdisplay, XSCREEN) - old_height) / 2;
		dx = old_width - w;
		dy = old_height - h;

		/* Check position of the center of the window */
		if (x < x1)		/* left half */
			dx = 0;
		else if (x == x1)	/* exact center */
			dx /= 2;
		if (y < y1)		/* top half */
			dy = 0;
		else if (y == y1)	/* exact center */
			dy /= 2;

		XMoveWindow(r->Xdisplay, r->TermWin.parent, x+dx, y+dy);
	}
#endif	/* SMART_RESIZE */
}



/*
** Recalculate the szHint upon changes of menubar/scrollbar/tabbar
** or font
*/
/* INTPROTO */
void
rxvt_recalc_szhint (rxvt_t* r, resize_reason_t reason, unsigned int* p_w, unsigned int* p_h)
{
	unsigned int	old_width = r->szHint.width;
	unsigned int	old_height = r->szHint.height;


	assert (NULL != p_w);
	assert (NULL != p_h);

	switch (reason)	{
#ifdef HAVE_MENUBAR
	case HIDE_MENUBAR:
		r->szHint.base_height -= rxvt_menubar_rheight (r);
		r->szHint.min_height -= rxvt_menubar_rheight (r);
		r->szHint.flags = PMinSize | PBaseSize | PSize;
		break;
	case SHOW_MENUBAR:
		r->szHint.base_height += rxvt_menubar_rheight (r);
		r->szHint.min_height += rxvt_menubar_rheight (r);
		r->szHint.flags = PMinSize | PBaseSize | PSize;
		break;
#endif	/* HAVE_MENUBAR */
	case HIDE_TABBAR:
		r->szHint.base_height -= rxvt_tabbar_rheight (r);
		r->szHint.min_height -= rxvt_tabbar_rheight (r);
		r->szHint.flags = PMinSize | PBaseSize | PSize;
		break;
	case SHOW_TABBAR:
		r->szHint.base_height += rxvt_tabbar_rheight (r);
		r->szHint.min_height += rxvt_tabbar_rheight (r);
		r->szHint.flags = PMinSize | PBaseSize | PSize;
		break;
#ifdef HAVE_SCROLLBARS
	case HIDE_SCROLLBAR:
		r->szHint.base_width -= rxvt_scrollbar_rwidth (r);
		r->szHint.min_width -= rxvt_scrollbar_rwidth (r);
		r->szHint.flags = PMinSize | PBaseSize | PSize;
		break;
	case SHOW_SCROLLBAR:
		r->szHint.base_width += rxvt_scrollbar_rwidth (r);
		r->szHint.min_width += rxvt_scrollbar_rwidth (r);
		r->szHint.flags = PMinSize | PBaseSize | PSize;
		break;
#endif	/* HAVE_SCROLLBARS */
	case RESIZE_FONT:
		/* Calculate the base width and height */
		r->szHint.base_width = 2 * r->TermWin.int_bwidth;
		r->szHint.base_height = 2 * r->TermWin.int_bwidth;
#ifdef HAVE_SCROLLBARS
		r->szHint.base_width += rxvt_scrollbar_width (r);
#endif
#if defined(HAVE_MENUBAR) && (MENUBAR_MAX > 1)
		r->szHint.base_height += rxvt_menubar_height (r);
#endif
		r->szHint.base_height += rxvt_tabbar_height (r);

		/* Set the terminal minimal width and height */
		r->szHint.min_width = r->szHint.base_width + r->szHint.width_inc;
		r->szHint.min_height = r->szHint.base_height + r->szHint.height_inc;
		/* Set the terminal incremental width and height */
		r->szHint.width_inc = r->TermWin.fwidth;
		r->szHint.height_inc = r->TermWin.fheight;
		r->szHint.flags = PMinSize | PBaseSize | PResizeInc | PSize;
		break;
	default:
		assert (0);	/* should not reach here */
		return ;
	}

	/* Compute the new width/height */
	r->szHint.width = r->szHint.base_width + Width2Pixel(r->TermWin.ncol);
	r->szHint.height = r->szHint.base_height + Height2Pixel(r->TermWin.nrow);

	/* Reset WMNormal Hints */
	XSetWMNormalHints (r->Xdisplay, r->TermWin.parent, &(r->szHint));

	/* Save desired width/height to return values */
	*p_w = r->szHint.width;
	*p_h = r->szHint.height;

	/* 
	** According to X Reference, we should only trust the size of
	** a Window through ConfigureNotify event, or through XGet...
	** functions. So here we must not change the window size till
	** we receive a ConfigureNotify event.
	*/
	r->szHint.width = old_width;
	r->szHint.height = old_height;

	/* Set the terminal window starting position */
	if (!(r->Options & Opt_scrollBar_right))
		r->h->window_vt_x = (r->Options & Opt_scrollBar_right) ? 
			0 : r->szHint.base_width - 2*r->TermWin.int_bwidth;

	r->h->window_vt_y = r->szHint.base_height - 2*r->TermWin.int_bwidth;
	if (r->Options2 & Opt2_bottomTabbar)
		r->h->window_vt_y -= rxvt_tabbar_height (r);

	/* Now we can resize the window */
	XResizeWindow(r->Xdisplay, r->TermWin.parent, *p_w, *p_h);

}


/* Resize windows on changing fonts */
/* INTPROTO */
void
rxvt_resize_on_font (rxvt_t* r, char* fontname)
{
	unsigned int	w = r->szHint.width, h = r->szHint.height;


	DBG_MSG(1, (stderr, "rxvt_resize_on_font\n"));

#ifdef XFT_SUPPORT
	/* if use freetype font, disallow resize by now ;-) */
	if ((r->Options & Opt_xft) && r->TermWin.xftfont)
		if (!rxvt_change_font_xft (r, fontname))
			return ;
	if (!(r->Options & Opt_xft))
#endif	/* XFT_SUPPORT */
	/* X11 font resize */
	if (!rxvt_change_font_x11 (r, fontname))
		return ;

	rxvt_recalc_szhint (r, RESIZE_FONT, &w, &h);
}



/*
** Recalculate the window col/row upon window resizing
*/
/* INTPROTO */
int
rxvt_calc_colrow (rxvt_t* r, unsigned int width, unsigned int height)
{
	unsigned int	ncol, nrow;


	assert (0 != r->TermWin.fwidth);
	assert (0 != r->TermWin.fheight);

	/* do nothing if size does not change */
	if (r->szHint.width == width && r->szHint.height == height)
		return 0;

	DBG_MSG(1, (stderr, "Recalc row/col of (%d, %d)\n", width, height));
	ncol = Pixel2Width (width - r->szHint.base_width);
	nrow = Pixel2Height (height - r->szHint.base_height);
	MAX_IT(ncol, 1);
	MAX_IT(nrow, 1);

	r->h->prev_ncol = r->TermWin.ncol;
	r->h->prev_nrow = r->TermWin.nrow;
	r->TermWin.ncol = ncol;
	r->TermWin.nrow = nrow;

	/* 
	** According to X Reference, we should only trust the size of
	** a Window through ConfigureNotify event, or through XGet...
	** functions. This function should only be called from 
	** ConfigureNotify event handler
	*/
	r->szHint.width = width;
	r->szHint.height = height;

	return ((r->h->prev_ncol != r->TermWin.ncol) ||
			(r->h->prev_nrow != r->TermWin.nrow));
}


/* INTPROTO */
void
rxvt_resize_sub_windows (rxvt_t* r)
{
	register int	i;


	DBG_MSG(1, (stderr, "rxvt_resize_sub_windows\n"));

#ifdef HAVE_SCROLLBARS
	rxvt_scrollbar_resize(r);
#endif
#ifdef HAVE_MENUBAR
	rxvt_menubar_resize(r);
#endif
	rxvt_tabbar_resize (r);

	for (i = 0; i <= LTAB(r); i ++)	{
		XMoveResizeWindow (r->Xdisplay, PVTS(r, i)->vt,
			r->h->window_vt_x, r->h->window_vt_y,
			VT_WIDTH(r), VT_HEIGHT(r));
	}
}


/* Resize windows on configurenotify event */
/* INTPROTO */
void
rxvt_resize_on_configure (rxvt_t* r, unsigned int width, unsigned int height)
{
	register int	i = 0;
	unsigned int	old_width = r->szHint.width,
					old_height = r->szHint.height;
	int				fix_screen;


	DBG_MSG(1, (stderr, "rxvt_resize_on_configure\n"));
	/* update ncol/nrow of new window */
	fix_screen = rxvt_calc_colrow (r, width, height);

	if (old_width != r->szHint.width || old_height != r->szHint.height)
		rxvt_resize_sub_windows (r);

	if (fix_screen) {
		for (i = 0; i <= LTAB(r); i ++)	{
			int			curr_screen = -1;
			RUINT16T	old_ncol;

			/*
			** Update previous columns and rows for each VT. We need
			** these copies so that rxvt_scr_reset can work for each
			** VT. If all VT only share one copy, the first VT that
			** calls rxvt_scr_reset will update it, thus other VT
			** will not correctly adjust their column/row.
			*/
			PVTS(r, i)->prev_nrow = r->h->prev_nrow;
			PVTS(r, i)->prev_ncol = r->h->prev_ncol;
			old_ncol = PVTS(r, i)->prev_ncol;

			rxvt_scr_clear(r, i);
#ifdef BACKGROUND_IMAGE
			rxvt_resize_pixmap(r, i);
#endif

			curr_screen = rxvt_scr_change_screen(r, i, PRIMARY);
			/* scr_reset only works on the primary screen */
			rxvt_scr_reset(r, i);
			if (curr_screen >= 0) {
				/* this is not the first time through */
				rxvt_scr_change_screen(r, i, curr_screen);
				rxvt_selection_check(r, i, (old_ncol != r->TermWin.ncol ? 4 : 0));
			}
		}
	}

#ifdef USE_XIM
	rxvt_IM_set_status_pos (r);
#endif
}


/* INTPROTO */
void
rxvt_process_configurenotify (rxvt_t* r, XConfigureEvent* ev)
{
	unsigned int	height, width;


	if (ev->window != r->TermWin.parent)
		return;

	DBG_MSG(1, (stderr, "ConfigureNotify event\n"));
	do {	/* Wrap lots of configures into one */
		width = ev->width;
		height = ev->height;
	} while (XCheckTypedWindowEvent(r->Xdisplay, ev->window,
			ConfigureNotify, (XEvent*) ev));

	/*
	** Remember the position of the window. Notice this position may
	** not be correct if Window Manager exists.
	*/
	r->szHint.x = ev->x;
	r->szHint.y = ev->y;
	DBG_MSG(1,(stderr, "Position: (%d, %d)\n", ev->x, ev->y));

	if (r->szHint.width != width || r->szHint.height != height) {
		rxvt_resize_on_configure (r, width, height);
	}
#ifdef DEBUG_VERBOSE
	else {
		DBG_MSG(1,(stderr, "Size: Not resizing\n"));
	}
#endif

#ifdef TRANSPARENT		/* XXX: maybe not needed - leave in for now */
	if (r->Options & Opt_transparent)	{
		rxvt_check_our_parents(r);
		r->h->want_full_refresh = 1;
	}
#endif
}



/* INTPROTO */
void
rxvt_process_selectionnotify (rxvt_t* r, XSelectionEvent* ev)
{
	DBG_MSG(2, (stderr, "SelectionNotify event\n"));
	if (r->h->selection_wait == Sel_normal)
		rxvt_selection_paste(r, ev->requestor, ev->property, True);
}



/* INTPROTO */
void
rxvt_process_propertynotify (rxvt_t* r, XPropertyEvent* ev)
{
	DBG_MSG(2, (stderr, "PropertyNotify event\n"));
	if (ev->atom == r->h->xa[XA_VT_SELECTION]) {
		if (ev->state == PropertyNewValue)
			rxvt_selection_property(r, ev->window, ev->atom);
		return;
	}

#ifdef TRANSPARENT
	/*
	 * if user used some Esetroot compatible prog to set the root bg,
	 * use the property to determine the pixmap.  We use it later on.
	 */
	if (None == r->h->xa[XA_XROOTPMAPID])
		r->h->xa[XA_XROOTPMAPID] = XInternAtom(r->Xdisplay, "_XROOTPMAP_ID", False);
	if (ev->atom != r->h->xa[XA_XROOTPMAPID])
		return ;

	if ((r->Options & Opt_transparent) && rxvt_check_our_parents(r)) {
		r->h->want_full_refresh = 1;
	}
#endif		/* TRANSPARENT */
}



/* INTPROTO */
int
rxvt_set_opacity (rxvt_t* r)
{
	int				k;
	unsigned int	n;
	Window			wintree[PARENT_NUMBER];
	Window			root;
	Window*			list;
	CARD32			opacity;


	/* do not set opacity */
	if (NULL == r->h->rs[Rs_opacity] ||
		None == r->h->xa[XA_NET_WM_WINDOW_OPACITY])
		return 0;

#ifdef TRANSPARENT
	/* Override pseudo-transparent in case */
	if (r->Options & Opt_transparent)
		r->Options &= ~Opt_transparent;
	XSetWindowBackground(r->Xdisplay, r->TermWin.parent,
		r->h->global_bg);
#endif

	opacity = (CARD32) (r->TermWin.opacity * (0xffffffff / 100));

	/*
	** look for parent tree of top level window because the window
	** manager may embed our window into some frame windows
	*/
	wintree[0] = r->TermWin.parent;
	for (k = 1; k < PARENT_NUMBER; k++)	{
		XQueryTree (r->Xdisplay, wintree[k-1], &root,
			&(wintree[k]), &list, &n);
		XFree (list);
		if (wintree[k] == XROOT)
			break;
	}

	/* Set opacity for all windows */
	if (k != PARENT_NUMBER)	{
		while (k-- > 0)
			XChangeProperty (r->Xdisplay, wintree[k],
				r->h->xa[XA_NET_WM_WINDOW_OPACITY], XA_CARDINAL, 32,
				PropModeReplace, (unsigned char*) &opacity, 1L);
	}

	XSync(r->Xdisplay, False);

	return 1;
}


/* INTPROTO */
void
rxvt_process_reparentnotify (rxvt_t* r, XEvent* ev)
{
	DBG_MSG(2, (stderr, "ReparentNotify event\n"));
	
	rxvt_set_opacity (r);

#ifdef TRANSPARENT
	if ((r->Options & Opt_transparent) && rxvt_check_our_parents(r)) {
		r->h->want_full_refresh = 1;
	}
#endif
}



/* INTPROTO */
void
rxvt_process_expose (rxvt_t* r, XExposeEvent* ev)
{
	int			page = ATAB(r);


	DBG_MSG(2, (stderr, "Expose event\n"));
	if (ev->window == PVTS(r, page)->vt) {
		XEvent			uev;
		register int	lx, ly, rx, ry;

		lx = ev->x;				/* top left point */
		ly = ev->y;
		rx = ev->x + ev->width;	/* bottom right point */
		ry = ev->y + ev->height;
		/* process other expose event in a batch */
		while (XCheckTypedWindowEvent(r->Xdisplay, ev->window,
			Expose, &uev))	{
			if (uev.xexpose.x + uev.xexpose.width > rx)
				rx = uev.xexpose.x + uev.xexpose.width;
			if (uev.xexpose.y + uev.xexpose.height > ry)
				ry = uev.xexpose.y + uev.xexpose.height;
			if (uev.xexpose.x < lx)
				lx = uev.xexpose.x;
			if (uev.xexpose.y < ly)
				ly = uev.xexpose.y;
		}
		/* process other xexpose event in a batch */
		while (XCheckTypedWindowEvent(r->Xdisplay, ev->window,
			GraphicsExpose, &uev))	{
			if (uev.xexpose.x + uev.xexpose.width > rx)
				rx = uev.xexpose.x + uev.xexpose.width;
			if (uev.xexpose.y + uev.xexpose.height > ry)
				ry = uev.xexpose.y + uev.xexpose.height;
			if (uev.xexpose.x < lx)
				lx = uev.xexpose.x;
			if (uev.xexpose.y < ly)
				ly = uev.xexpose.y;
		}

#ifdef NO_SLOW_LINK_SUPPORT
		DBG_MSG(2, (stderr, "Expose event on quick link\n"));
		rxvt_scr_expose(r, page, lx, ly, rx-lx, ry-ly, False);
#else
		DBG_MSG(2, (stderr, "Expose event on slow link\n"));
		rxvt_scr_expose(r, page, lx, 0, rx-lx, VT_HEIGHT(r), False);
#endif
		r->h->want_refresh = 1;
	}
	else {
		XEvent		unused_xevent;

#ifdef DEBUG_VERBOSE
		/* Debug message */
		if (rxvt_is_tabbar_win (r, ev->window))	{
			DBG_MSG(2, (stderr, "Expose event on tabbar\n"));
		}
# ifdef HAVE_SCROLLBARS
		else if (rxvt_is_scrollbar_win (r, ev->window))	{
			DBG_MSG(2, (stderr, "Expose event on scrollbar\n"));
		}
# endif
# ifdef HAVE_MENUBAR
		else if (rxvt_is_menubar_win (r, ev->window))	{
			DBG_MSG(2, (stderr, "Expose event on menubar\n"));
		}
# endif
#endif	/* DEBUG */

		/* fetch all expose events if possible */
		while (XCheckTypedWindowEvent(r->Xdisplay, ev->window,
			Expose, &unused_xevent))
			;
		while (XCheckTypedWindowEvent(r->Xdisplay, ev->window,
			GraphicsExpose, &unused_xevent))
			;

		if (rxvt_is_tabbar_win(r, ev->window) &&
			rxvt_tabbar_visible (r))	{
			rxvt_tabbar_expose(r);
		}
#ifdef HAVE_SCROLLBARS
		else if (rxvt_is_scrollbar_win (r, ev->window) &&
			rxvt_scrollbar_visible (r)) {
			scrollbar_setIdle();
			rxvt_scrollbar_update(r, 0);
		}
#endif
#ifdef HAVE_MENUBAR
		else if (rxvt_is_menubar_win(r, ev->window) &&
			rxvt_menubar_visible (r))	{
			rxvt_menubar_expose(r);
		}
#endif
	}
}



/* INTPROTO */
void
rxvt_process_motionnotify (rxvt_t* r, XEvent* ev)
{
	int				page = ATAB(r);
	Window			unused_root, unused_child;
	int				unused_root_x, unused_root_y;
	unsigned int	unused_mask;


	DBG_MSG(2, (stderr, "MotionNotify event\n"));
#ifdef POINTER_BLANK
	if ((r->Options & Opt_pointerBlank) && 
		PVTS(r, page)->hidden_pointer)
		rxvt_pointer_unblank (r, page);
#endif
#ifdef HAVE_MENUBAR
	if (rxvt_is_menubar_win(r, ev->xmotion.window)) {
		rxvt_menubar_control(r, &(ev->xbutton));
		return;
	}
#endif
	if ((PVTS(r, page)->PrivateModes & PrivMode_mouse_report) &&
		!(r->h->bypass_keystate))
		return;

	if (ev->xany.window == PVTS(r, page)->vt) {
		if ((ev->xbutton.state & (Button1Mask | Button3Mask))) {
			while (XCheckTypedWindowEvent(r->Xdisplay, PVTS(r, page)->vt,
				MotionNotify, ev))
				;
			XQueryPointer(r->Xdisplay, PVTS(r, page)->vt,
				&unused_root, &unused_child,
				&unused_root_x, &unused_root_y,
				&(ev->xbutton.x), &(ev->xbutton.y),
				&unused_mask);
#ifdef MOUSE_THRESHOLD
			/* deal with a `jumpy' mouse */
			if ((ev->xmotion.time - r->h->MEvent.time) >
				MOUSE_THRESHOLD) {
#endif
				rxvt_selection_extend(r, page,
					(ev->xbutton.x), (ev->xbutton.y),
					(ev->xbutton.state & Button3Mask) ? 2 : 0);
#ifdef SELECTION_SCROLLING
				if (ev->xbutton.y<r->TermWin.int_bwidth ||
					Pixel2Row(ev->xbutton.y)>(r->TermWin.nrow-1)) {
					int dist;
			
					r->h->pending_scroll_selection=1;
			
					/*
					** don't clobber the current delay if we are
					** already in the middle of scrolling.
					*/
				if (r->h->scroll_selection_delay<=0)
					r->h->scroll_selection_delay = SCROLLBAR_CONTINUOUS_DELAY;

					/*
					** save the event params so we can highlight
					** the selection in the pending-scroll loop
					*/
				r->h->selection_save_x = ev->xbutton.x;
				r->h->selection_save_y = ev->xbutton.y;
				r->h->selection_save_state = (ev->xbutton.state & Button3Mask) ? 2 : 0;

					/* calc number of lines to scroll */
					if (ev->xbutton.y<r->TermWin.int_bwidth) {
						r->h->scroll_selection_dir = UP;
						dist = r->TermWin.int_bwidth - ev->xbutton.y;
					}
					else {
						r->h->scroll_selection_dir = DN;
						dist = ev->xbutton.y -
						(r->TermWin.int_bwidth + VT_HEIGHT(r));
					}
					r->h->scroll_selection_lines=(Pixel2Height(dist)/
						SELECTION_SCROLL_LINE_SPEEDUP)+1;
					MIN_IT(r->h->scroll_selection_lines,
						SELECTION_SCROLL_MAX_LINES);
				}
				else {
					/*
					** we are within the text window, so we
					** shouldn't be scrolling
					*/
					r->h->pending_scroll_selection = 0;
				}
#endif
#ifdef MOUSE_THRESHOLD
			}
#endif
		}
	}
#ifdef HAVE_SCROLLBARS
	else if (rxvt_is_scrollbar_win (r, ev->xany.window) &&
		scrollbar_isMotion()) {
		while (XCheckTypedWindowEvent(r->Xdisplay, r->scrollBar.win,
			MotionNotify, ev))
			;
		XQueryPointer(r->Xdisplay, r->scrollBar.win,
			&unused_root, &unused_child,
			&unused_root_x, &unused_root_y,
			&(ev->xbutton.x), &(ev->xbutton.y),
			&unused_mask);
		rxvt_scr_move_to(r, page,
			scrollbar_position(ev->xbutton.y) - r->h->csrO,
			scrollbar_size());
		rxvt_scr_refresh(r, page, r->h->refresh_type);
		r->h->refresh_limit = 0;
		rxvt_scrollbar_update(r, 1);
	}
#endif
}



/*{{{ process an X event */
/* INTPROTO */
void
rxvt_process_x_event(rxvt_t* r, XEvent *ev)
{
	int				page = ATAB(r);
	int				i, want_timeout = 0;
	struct timeval	tp;
	struct rxvt_hidden *h = r->h;
#ifdef DEBUG_X
	const char *const eventnames[] =
	{				/* mason - this matches my system */
	"",
	"",
	"KeyPress",
	"KeyRelease",
	"ButtonPress",
	"ButtonRelease",
	"MotionNotify",
	"EnterNotify",
	"LeaveNotify",
	"FocusIn",
	"FocusOut",
	"KeymapNotify",
	"Expose",
	"GraphicsExpose",
	"NoExpose",
	"VisibilityNotify",
	"CreateNotify",
	"DestroyNotify",
	"UnmapNotify",
	"MapNotify",
	"MapRequest",
	"ReparentNotify",
	"ConfigureNotify",
	"ConfigureRequest",
	"GravityNotify",
	"ResizeRequest",
	"CirculateNotify",
	"CirculateRequest",
	"PropertyNotify",
	"SelectionClear",
	"SelectionRequest",
	"SelectionNotify",
	"ColormapNotify",
	"ClientMessage",
	"MappingNotify"
	};
	struct tm	  *ltt;
#endif

	/*
	 * check if we need to get the time for any timeouts
	 */

	for (i = NUM_TIMEOUTS; i--; )
	if (h->timeout[i].tv_sec) {
		want_timeout = 1;
		break;
	}

#ifndef DEBUG_X
	if (want_timeout)
#endif
		(void)gettimeofday(&tp, NULL);

#ifdef DEBUG_X
	ltt = localtime(&(tp.tv_sec));
	DBG_MSG(2,(stderr, "Event: %-16s %-7s %08lx (%4d-%02d-%02d %02d:%02d:%02d.%.6ld) %s %lu\n", eventnames[ev->type], (ev->xany.window == r->TermWin.parent ? "parent" : (ev->xany.window == PVTS(r, page)->vt ? "vt" : (ev->xany.window == r->scrollBar.win ? "scroll" : (ev->xany.window == r->menuBar.win ? "menubar" : "UNKNOWN")))), (ev->xany.window == r->TermWin.parent ? r->TermWin.parent : (ev->xany.window == PVTS(r, page)->vt ? PVTS(r, page)->vt : (ev->xany.window == r->scrollBar.win ? r->scrollBar.win : (ev->xany.window == r->menuBar.win ? r->menuBar.win : 0)))), ltt->tm_year + 1900, ltt->tm_mon + 1, ltt->tm_mday, ltt->tm_hour, ltt->tm_min, ltt->tm_sec, tp.tv_usec, ev->xany.send_event ? "S" : " ", ev->xany.serial));
#endif

	/* X event timeouts */
	if (want_timeout)	{
		for (i = NUM_TIMEOUTS; i--; ) {
			if (h->timeout[i].tv_sec == 0)
				continue;
			if ((tp.tv_sec < h->timeout[i].tv_sec) ||
				(tp.tv_sec == h->timeout[i].tv_sec &&
				 tp.tv_usec < h->timeout[i].tv_usec))
				continue;
			h->timeout[i].tv_sec = 0;
			switch(i) {
			case TIMEOUT_INCR:
				rxvt_print_error("data loss: timeout on INCR selection paste");
				h->selection_wait = Sel_none;
				break;
			default:
				break;
			}
		}
	}


	switch (ev->type) {
	case KeyPress:
		rxvt_process_keypress (r, (XKeyEvent *) ev);
		break;

#if defined(MOUSE_WHEEL) && defined(MOUSE_SLIP_WHEELING)
	case KeyRelease:
		rxvt_process_keyrelease (r, (XKeyEvent*) ev);
		break;
#endif

	case ButtonPress:
		rxvt_process_buttonpress(r, page, (XButtonEvent *) ev);
		break;

	case ButtonRelease:
		rxvt_process_buttonrelease(r, page, (XButtonEvent *) ev);
		break;

	case ClientMessage:
		rxvt_process_clientmessage(r, (XClientMessageEvent*) ev);
		break;

	case MappingNotify:
		XRefreshKeyboardMapping(&(ev->xmapping));
		break;

	/*
	 * XXX: this is not the _current_ arrangement
	 * Here's my conclusion:
	 * If the window is completely unobscured, use bitblt's
	 * to scroll. Even then, they're only used when doing partial
	 * screen scrolling. When partially obscured, we have to fill
	 * in the GraphicsExpose parts, which means that after each refresh,
	 * we need to wait for the graphics expose or Noexpose events,
	 * which ought to make things real slow!
	 */
	case VisibilityNotify:
		rxvt_process_visibilitynotify (r, (XVisibilityEvent*) ev);
		break;

#ifdef MONITOR_ENTER_LEAVE
	case EnterNotify:
		rxvt_process_enter (r, (XCrossingEvent*) ev);
		break;

	case LeaveNotify:
		rxvt_process_leave (r, (XCrossingEvent*) ev);
		break;
#endif	/* MONITOR_ENTER_LEAVE */

	case FocusIn:
		rxvt_process_focusin (r, (XFocusChangeEvent*) ev);
		break;

	case FocusOut:
		rxvt_process_focusout (r, (XFocusChangeEvent*) ev);
		break;

	case ConfigureNotify:
		rxvt_process_configurenotify (r, (XConfigureEvent*) ev);
		break;

	case SelectionClear:
		rxvt_process_selectionclear(r, page);
		break;

	case SelectionNotify:
		rxvt_process_selectionnotify (r, (XSelectionEvent*) ev);
		break;

	case SelectionRequest:
		rxvt_process_selectionrequest (r, page, (XSelectionRequestEvent*) ev);
		break;

	case UnmapNotify:
		r->TermWin.mapped = 0;
		break;

	case MapNotify:
		r->TermWin.mapped = 1;
		break;

	case PropertyNotify:
		rxvt_process_propertynotify (r, (XPropertyEvent*) ev);
		break;

	case ReparentNotify:
		rxvt_process_reparentnotify (r, ev);
		break;

	case GraphicsExpose:
	case Expose:
		rxvt_process_expose (r, (XExposeEvent*) ev);
		break;

	case MotionNotify:
		rxvt_process_motionnotify (r, ev);
		break;
	}
}


#if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
static void    shade_ximage (rxvt_t*, XImage*, int, int, int, int);

/* EXTPROTO */
void
rxvt_shade_pixmap (rxvt_t* r, Drawable src, GC gc, int sx, int sy, unsigned nw, unsigned nh, int shade, unsigned long pixel)
{
	XImage*		ximage;
	XColor		xcol;


	if (100 == shade)		/* no need to shade */
		return;

	xcol.pixel = pixel;
	XQueryColor (r->Xdisplay, XCMAP, &xcol);

	ximage = XGetImage(r->Xdisplay, src, sx, sy, nw, nh,
				AllPlanes, ZPixmap);
	if (NULL != ximage)	{
		shade_ximage (r, ximage, shade, xcol.red, xcol.green, xcol.blue);
		XPutImage(r->Xdisplay, src, gc, ximage, 0, 0, sx, sy, nw, nh);
		XDestroyImage (ximage);
	}
}


/* INTPROTO */
static void
shade_ximage (rxvt_t* r, XImage* srcImage, int shade, int rm, int gm, int bm)
{
	int				sh_r, sh_g, sh_b;
	RUINT32T		mask_r, mask_g, mask_b;
	RUINT32T		*lookup, *lookup_r, *lookup_g, *lookup_b;
	unsigned int	lower_lim_r, lower_lim_g, lower_lim_b;
	unsigned int	upper_lim_r, upper_lim_g, upper_lim_b;
	int i;
	Visual*			visual = XVISUAL;


	if (100 == shade)		/* no need to shade */
		return;

	if( visual->class != TrueColor || srcImage->format != ZPixmap )
		return ;

	DBG_MSG(2, (stderr, "shade background image\n"));

	/* for convenience */
	mask_r = visual->red_mask;
	mask_g = visual->green_mask;
	mask_b = visual->blue_mask;

	/* boring lookup table pre-initialization */
	switch (srcImage->bits_per_pixel) {
	case 15:
		if ((mask_r != 0x7c00) ||
			(mask_g != 0x03e0) ||
			(mask_b != 0x001f))
			return;
		lookup = (RUINT32T *) rxvt_malloc (sizeof (RUINT32T)*(32+32+32));
		lookup_r = lookup;
		lookup_g = lookup+32;
		lookup_b = lookup+32+32;
		sh_r = 10;
		sh_g = 5;
		sh_b = 0;
		break;
	case 16:
		if ((mask_r != 0xf800) ||
			(mask_g != 0x07e0) ||
			(mask_b != 0x001f))
			return;
		lookup = (RUINT32T *) rxvt_malloc (sizeof (RUINT32T)*(32+64+32));
		lookup_r = lookup;
		lookup_g = lookup+32;
		lookup_b = lookup+32+64;
		sh_r = 11;
		sh_g = 5;
		sh_b = 0;
		break;
    case 24:
		if ((mask_r != 0xff0000) ||
			(mask_g != 0x00ff00) ||
			(mask_b != 0x0000ff))
			return;
		lookup = (RUINT32T *) rxvt_malloc (sizeof (RUINT32T)*(256+256+256));
		lookup_r = lookup;
		lookup_g = lookup+256;
		lookup_b = lookup+256+256;
		sh_r = 16;
		sh_g = 8;
		sh_b = 0;
		break;
	case 32:
		if ((mask_r != 0xff0000) ||
			(mask_g != 0x00ff00) ||
			(mask_b != 0x0000ff))
        return;
		lookup = (RUINT32T *) rxvt_malloc (sizeof (RUINT32T)*(256+256+256));
		lookup_r = lookup;
		lookup_g = lookup+256;
		lookup_b = lookup+256+256;
		sh_r = 16;
		sh_g = 8;
		sh_b = 0;
		break;
	default:
		return; /* we do not support this color depth */
	}

	/* prepare limits for color transformation (each channel is
	** handled separately) */
	if (shade < 0) {
		shade = -shade;
		if (shade < 0)
			shade = 0;
		if (shade > 100)
			shade = 100;

		lower_lim_r = 65535-rm;
		lower_lim_g = 65535-gm;
		lower_lim_b = 65535-bm;

		lower_lim_r = 65535-(unsigned int)(((RUINT32T)lower_lim_r)*((RUINT32T)shade)/100);
		lower_lim_g = 65535-(unsigned int)(((RUINT32T)lower_lim_g)*((RUINT32T)shade)/100);
		lower_lim_b = 65535-(unsigned int)(((RUINT32T)lower_lim_b)*((RUINT32T)shade)/100);

		upper_lim_r = upper_lim_g = upper_lim_b = 65535;
	}
	else {
		if (shade < 0)
			shade = 0;
		if (shade > 100)
			shade = 100;

		lower_lim_r = lower_lim_g = lower_lim_b = 0;

		upper_lim_r = (unsigned int)((((RUINT32T)rm)*((RUINT32T)shade))/100);
		upper_lim_g = (unsigned int)((((RUINT32T)gm)*((RUINT32T)shade))/100);
		upper_lim_b = (unsigned int)((((RUINT32T)bm)*((RUINT32T)shade))/100);
	}

	/* switch red and blue bytes if necessary, we need it for some
	** weird XServers like XFree86 3.3.3.1 */
	if ((srcImage->bits_per_pixel == 24) && (mask_r >= 0xFF0000 )) {
		unsigned int tmp;

		tmp = lower_lim_r;
		lower_lim_r = lower_lim_b;
		lower_lim_b = tmp;

		tmp = upper_lim_r;
		upper_lim_r = upper_lim_b;
		upper_lim_b = tmp;
	}

	/* fill our lookup tables */
	for (i = 0; i <= mask_r>>sh_r; i++) {
		RUINT32T tmp;
		tmp = ((RUINT32T)i)*((RUINT32T)(upper_lim_r-lower_lim_r));
		tmp += ((RUINT32T)(mask_r>>sh_r))*((RUINT32T)lower_lim_r);
		lookup_r[i] = (tmp/65535)<<sh_r;
	}
	for (i = 0; i <= mask_g>>sh_g; i++) {
		RUINT32T tmp;
		tmp = ((RUINT32T)i)*((RUINT32T)(upper_lim_g-lower_lim_g));
		tmp += ((RUINT32T)(mask_g>>sh_g))*((RUINT32T)lower_lim_g);
		lookup_g[i] = (tmp/65535)<<sh_g;
	}
	for (i = 0; i <= mask_b>>sh_b; i++) {
		RUINT32T tmp;
		tmp = ((RUINT32T)i)*((RUINT32T)(upper_lim_b-lower_lim_b));
		tmp += ((RUINT32T)(mask_b>>sh_b))*((RUINT32T)lower_lim_b);
		lookup_b[i] = (tmp/65535)<<sh_b;
	}

	/* apply table to input image (replacing colors by newly
	** calculated ones) */
	switch (srcImage->bits_per_pixel) {
	case 15:
		{
			unsigned short *p1, *pf, *p, *pl;
			p1 = (unsigned short *) srcImage->data;
			pf = (unsigned short *) (srcImage->data + srcImage->height * srcImage->bytes_per_line);
			while (p1 < pf) {
				p = p1;
				pl = p1 + srcImage->width;
				for (; p < pl; p++) {
					*p = lookup_r[(*p & 0x7c00)>>10] |
						lookup_g[(*p & 0x03e0)>> 5] |
						lookup_b[(*p & 0x001f)];
				}
				p1 = (unsigned short *) ((char *) p1 + srcImage->bytes_per_line);
			}
			break;
		}
	case 16:
		{
			unsigned short *p1, *pf, *p, *pl;
			p1 = (unsigned short *) srcImage->data;
			pf = (unsigned short *) (srcImage->data + srcImage->height * srcImage->bytes_per_line);
			while (p1 < pf) {
				p = p1;
				pl = p1 + srcImage->width;
				for (; p < pl; p++) {
					*p = lookup_r[(*p & 0xf800)>>11] |
						lookup_g[(*p & 0x07e0)>> 5] |
						lookup_b[(*p & 0x001f)];
				}
				p1 = (unsigned short *) ((char *) p1 + srcImage->bytes_per_line);
			}
			break;
		}
	case 24:
		{
			unsigned char *p1, *pf, *p, *pl;
			p1 = (unsigned char *) srcImage->data;
			pf = (unsigned char *) (srcImage->data + srcImage->height * srcImage->bytes_per_line);
			while (p1 < pf) {
				p = p1;
				pl = p1 + srcImage->width * 3;
				for (; p < pl; p += 3) {
					p[0] = lookup_r[(p[0] & 0xff0000)>>16];
					p[1] = lookup_r[(p[1] & 0x00ff00)>> 8];
					p[2] = lookup_r[(p[2] & 0x0000ff)];
				}
				p1 = (unsigned char *) ((char *) p1 + srcImage->bytes_per_line);
			}
			break;
		}
	case 32:
		{
			RUINT32T *p1, *pf, *p, *pl;
			p1 = (RUINT32T *) srcImage->data;
			pf = (RUINT32T *) (srcImage->data + srcImage->height * srcImage->bytes_per_line);

			while (p1 < pf) {
				p = p1;
				pl = p1 + srcImage->width;
				for (; p < pl; p++) {
					*p = lookup_r[(*p & 0xff0000)>>16] |
						lookup_g[(*p & 0x00ff00)>> 8] |
						lookup_b[(*p & 0x0000ff)] |
						(*p & ~0xffffff);
				}
				p1 = (RUINT32T *) ((char *) p1 + srcImage->bytes_per_line);
			}
			break;
		}
	}

	free (lookup);
}
#endif	/* BACKGROUND_IMAGE || TRANSPARENT */



#ifdef TRANSPARENT
static XImage* get_parent_ximage (rxvt_t*, Pixmap, unsigned int, unsigned int, int*, int*);
static int     reset_parent_pixmap (rxvt_t*, XImage*, int, int);

/* INTPROTO */
static XImage*
get_parent_ximage (rxvt_t* r, Pixmap rootpixmap, unsigned int rootw, unsigned int rooth, int* nx, int* ny)
{
	int				sx, sy;
	unsigned int	nw, nh;
	Window			cr;


	XTranslateCoordinates(r->Xdisplay, r->TermWin.parent, XROOT,
		0, 0, &sx, &sy, &cr);
	nw = r->szHint.width;
	nh = r->szHint.height;
	*nx = *ny = 0;
	if (sx < 0) {
		nw += sx;
		*nx = -sx;
		sx = 0;
	}
	if (sy < 0) {
		nh += sy;
		*ny = -sy;
		sy = 0;
	}
	MIN_IT(nw, (unsigned int) (rootw - sx));
	MIN_IT(nh, (unsigned int) (rooth - sy));

	DBG_MSG(1, (stderr, "XGetImage (%d, %d, %d, %d)\n", sx, sy, nw, nh));
	if (None != rootpixmap)	{
		return (XGetImage(r->Xdisplay, rootpixmap, sx, sy, nw, nh,
				AllPlanes, ZPixmap));
	}
	else	{
		return NULL;
	}
}


/* INTPROTO */
static int
reset_parent_pixmap (rxvt_t* r, XImage* image, int nx, int ny)
{
	Pixmap		pixmap;
	GC			gc;
	XGCValues	gcvalue;


	if (r->h->rs[Rs_backgroundFade])	{
		XColor		xcol;
		xcol.pixel = r->PixColors[Color_White];
		XQueryColor (r->Xdisplay, XCMAP, &xcol);
		shade_ximage (r, image, r->TermWin.bgfade,
			xcol.red, xcol.green, xcol.blue);
	}

#ifdef TINTING_SUPPORT
	if (ISSET_PIXCOLOR (r->h, Color_tint) &&
		r->h->rs[Rs_shade])	{
		XColor		xcol;

		xcol.pixel = r->PixColors[Color_tint];
		XQueryColor (r->Xdisplay, XCMAP, &xcol);
		shade_ximage (r, image, r->TermWin.shade,
			xcol.red, xcol.green, xcol.blue);
	}
#endif

	pixmap = XCreatePixmap(r->Xdisplay, r->TermWin.parent,
				r->szHint.width, r->szHint.height,
				(unsigned int) image->depth);
	if (None == pixmap)
		return 0;

	/* Transparent VTs share the same copy of bg pixmap!!! */
	if (r->TermWin.pixmap != None)
		XFreePixmap(r->Xdisplay, r->TermWin.pixmap);
	r->TermWin.pixmap = pixmap;

	gc = XCreateGC (r->Xdisplay, r->TermWin.parent, 0UL, &gcvalue);
	if (None != gc)	{
		XPutImage(r->Xdisplay, pixmap, gc, image,
			0, 0,		/* src x and y */
			nx, ny,		/* dst x and y */
			(unsigned int) image->width, (unsigned int) image->height);
		XFreeGC(r->Xdisplay, gc);
	}

	DBG_MSG(2, (stderr, "reset background image for parent\n"));
	/* Reset background of TermWin!!! */
	XSetWindowBackgroundPixmap (r->Xdisplay, r->TermWin.parent, pixmap);

	/* Do not handle the subwindows if VT windows have not been
	** created. Otherwise, the following will cause crash. */
	if (-1 == LTAB(r))
		return 1;

# ifdef HAVE_SCROLLBARS
	if (r->Options & Opt_transparent_scrollbar)	{
		DBG_MSG(2, (stderr, "reset background image for scrollbar\n"));
		XClearWindow (r->Xdisplay, r->scrollBar.win);
		r->scrollBar.update (r, 1, r->scrollBar.top, r->scrollBar.bot,
			r->h->scroller_len);
	}
# endif
# ifdef HAVE_MENUBAR
	if (r->Options & Opt_transparent_menubar)	{
		DBG_MSG(2, (stderr, "reset background image for menubar\n"));
		rxvt_menubar_expose (r);
	}
# endif
	if (r->Options & Opt_transparent_tabbar)	{
		DBG_MSG(2, (stderr, "reset background image for tabbar\n"));
		rxvt_tabbar_expose (r);
	}

	return 1;
}


/*
 * Check our parents are still who we think they are.
 * Do transparency updates if required
 */
/* EXTPROTO */
int
rxvt_check_our_parents(rxvt_t *r)
{
	int					have_prop = 0,
						have_changed = 0,
						have_pixmap = 0,
						aformat, rootdepth;
	unsigned long		nitems, bytes_after;
	Atom				atype;
	unsigned char*		prop = NULL;
	Window				root, oldp, *list;
	Pixmap				rootpixmap = None;
	XWindowAttributes	wattr, wrootattr;


	if (!(r->Options & Opt_transparent))
		return have_changed;

	DBG_MSG(2, (stderr, "rxvt_check_our_parent ()\n"));

	XGetWindowAttributes(r->Xdisplay, XROOT, &wrootattr);
	rootdepth = wrootattr.depth;
	XGetWindowAttributes(r->Xdisplay, r->TermWin.parent, &wattr);
	if (rootdepth != wattr.depth) {
		if (r->h->am_transparent) {
			have_changed = 1;
			r->h->am_transparent = r->h->am_pixmap_trans = 0;
			assert (-1 != LTAB(r)); /* should't happen */
			XSetWindowBackground(r->Xdisplay, AVTS(r)->vt,
				 r->h->global_bg);
		}
		return have_changed;
	}

	XSync(r->Xdisplay, False);

	have_prop = (None != r->h->xa[XA_XROOTPMAPID] &&
		(Success == XGetWindowProperty(r->Xdisplay, XROOT,
			r->h->xa[XA_XROOTPMAPID], 0L, 1L, False, XA_PIXMAP,
			&atype, &aformat, &nitems, &bytes_after, &prop)));

	if (have_prop && NULL != prop)	{
		/* Free this _XROOTPMAPID */
		have_pixmap = 1;
		rootpixmap = *((Pixmap*) prop);
		XFree(prop);
	}

	if (have_pixmap) {
		/*
		** Copy XROOT pixmap transparency
		*/
		int				nx, ny;
		XImage*			imagetw;

		/*
		** handle BadMatch - usually because we're outside the
		** pixmap. may need a delay here?
		*/
		r->h->allowedxerror = -1;
		imagetw = get_parent_ximage (r, rootpixmap,
					wrootattr.width, wrootattr.height, &nx, &ny);
		r->h->allowedxerror = 0;

		if (NULL == imagetw) {
			if (r->h->am_transparent && r->h->am_pixmap_trans) {
				have_changed = 1;
				/* free old termwin pixmaps */
				if (None != r->TermWin.pixmap)	{
					XFreePixmap(r->Xdisplay, r->TermWin.pixmap);
					r->TermWin.pixmap = None;
				}
			}
			r->h->am_pixmap_trans = 0;
		}
		else {
			int		retvt = 0;
			/* Transparent VTs share the same copy of bg pixmap!!! */
			if (imagetw)	{
				retvt = reset_parent_pixmap (r, imagetw, nx, ny);
				XDestroyImage (imagetw);
			}
			/*
			if ((!r->h->am_transparent || !r->h->am_pixmap_trans) &&
				retvt)
			*/
			if (retvt)
				have_changed = 1;
			r->h->am_transparent = r->h->am_pixmap_trans = 1;
		}
	}

	/*
	** Make the frame window set by the window manager that have the
	** root background. Some window managers put multiple nested frame
	** windows for each client, so we have to take care about that.
	*/
	if (!r->h->am_pixmap_trans) {
		unsigned int	i, n;
		/*
		 * Transparent transparency
		 */
		DBG_MSG(1,(stderr, "Transparent Seeking to  %08lx\n", XROOT));
		for (i = 1; i < PARENT_NUMBER; i++) {
			oldp = r->TermWin.parenttree[i];
			XQueryTree(r->Xdisplay, r->TermWin.parenttree[i - 1],
				&root, &r->TermWin.parenttree[i], &list, &n);
			XFree(list);
			DBG_MSG(1,(stderr, "Transparent Parent[%d] = %08lx\n", i, r->TermWin.parenttree[i]));
			if (r->TermWin.parenttree[i] == XROOT) {
				if (oldp != None)
					have_changed = 1;
				break;
			}
			if (oldp != r->TermWin.parenttree[i])
				have_changed = 1;
		}
		n = 0;
		if (have_changed) {
			for (; n < (unsigned int)i; n++) {
				XGetWindowAttributes(r->Xdisplay,
					r->TermWin.parenttree[n], &wattr);
				DBG_MSG(1,(stderr, "Transparent Checking Parent[%d]: %s\n", n, (wattr.depth == rootdepth && wattr.class != InputOnly) ? "OK" : "FAIL"));
				if (wattr.depth != rootdepth ||
					wattr.class == InputOnly) {
					n = PARENT_NUMBER + 1;
					break;
				}
			}
		}

		if (n > PARENT_NUMBER) {
			DBG_MSG(1,(stderr, "Transparent Turning off\n"));
			XSetWindowBackground(r->Xdisplay, r->TermWin.parent,
				r->PixColors[Color_fg]);
			r->h->am_transparent = 0;
		}
		else {
			/* wait (an arbitrary period) for the WM to do its thing
			 * needed for fvwm2.2.2 (and before?) */
			 /*
# ifdef HAVE_NANOSLEEP
			struct timespec rqt;

			rqt.tv_sec = 1;
			rqt.tv_nsec = 0;
			nanosleep(&rqt, NULL);
# else
			sleep(1);	
# endif
			*/
			DBG_MSG(1,(stderr, "Transparent Turning on (%d parents)\n", i - 1));
			for (n = 0; n < i; n ++)
				XSetWindowBackgroundPixmap(r->Xdisplay,
					r->TermWin.parenttree[n], ParentRelative);

			r->h->am_transparent = 1;
		}

		for (; i < PARENT_NUMBER; i++)
			r->TermWin.parenttree[i] = None;
	}

	return have_changed;
}
#endif

/*}}} */



/*{{{ print pipe */
/*----------------------------------------------------------------------*/
#ifdef PRINTPIPE
/* EXTPROTO */
FILE		   *
rxvt_popen_printer(rxvt_t *r)
{
	FILE		   *stream = popen(r->h->rs[Rs_print_pipe], "w");

	if (stream == NULL)
	rxvt_print_error("can't open printer pipe");
	return stream;
}


/* EXTPROTO */
int
rxvt_pclose_printer(FILE *stream)
{
	fflush(stream);
/* pclose() reported not to work on SunOS 4.1.3 */
/* # if defined (__sun__) */
# ifdef OS_SUNOS
	/* pclose works provided SIGCHLD handler uses waitpid */
	return pclose(stream);	/* return fclose (stream); */
# else
	return pclose(stream);
# endif
}


/*
 * simulate attached vt100 printer
 */
/* INTPROTO */
void
rxvt_process_print_pipe(rxvt_t* r, int page)
{
	int			readpage = page;
#ifdef DEBUG
	clock_t		checksum = PVTS(r, page)->checksum;
#endif
	int			done;
	FILE*		fd;


	if ((fd = rxvt_popen_printer(r)) == NULL)
		return;

	/*
	** Send all input to the printer until either ESC[4i or ESC[?4i
	** is received.
	*/
	for (done = 0; !done;) {
		unsigned char   buf[8];
		unsigned char   ch;
		unsigned int	i, len;

		if ((ch = rxvt_cmd_getc(r, &readpage)) != C0_ESC) {
			assert (readpage == page);
			assert (checksum == PVTS(r, page)->checksum);
			if (putc(ch, fd) == EOF)
				break;		/* done = 1 */
		}
		else {
			len = 0;
			buf[len++] = ch;

			if ((buf[len++] = rxvt_cmd_getc(r, &readpage)) == '[') {
				assert (page == readpage);
				assert (checksum == PVTS(r, page)->checksum);
				if ((ch = rxvt_cmd_getc(r, &readpage)) == '?') {
					assert (page == readpage);
					assert (checksum == PVTS(r, page)->checksum);
					buf[len++] = '?';
					ch = rxvt_cmd_getc(r, &readpage);
					assert (page == readpage);
					assert (checksum == PVTS(r, page)->checksum);
				}
				if ((buf[len++] = ch) == '4') {
					if ((buf[len++]=rxvt_cmd_getc(r, &readpage))=='i') {
						assert (page == readpage);
						assert (checksum == PVTS(r, page)->checksum);
						break;	/* done = 1 */
					}
				}
			}

			for (i = 0; i < len; i++)	{
				if (putc(buf[i], fd) == EOF) {
					done = 1;
					break;
				}
			}
		}
	}

	rxvt_pclose_printer(fd);
}
#endif				/* PRINTPIPE */
/*}}} */


/* *INDENT-OFF* */
enum {
	C1_40 = 0x40,
		C1_41 , C1_BPH, C1_NBH, C1_44 , C1_NEL, C1_SSA, C1_ESA,
	C1_HTS, C1_HTJ, C1_VTS, C1_PLD, C1_PLU, C1_RI , C1_SS2, C1_SS3,
	C1_DCS, C1_PU1, C1_PU2, C1_STS, C1_CCH, C1_MW , C1_SPA, C1_EPA,
	C1_SOS, C1_59 , C1_SCI, C1_CSI, CS_ST , C1_OSC, C1_PM , C1_APC
};
/* *INDENT-ON* */


/*{{{ process non-printing single characters */
/* INTPROTO */
void
rxvt_process_nonprinting(rxvt_t* r, int page, unsigned char ch)
{
	switch (ch) {
	case C0_ENQ:	/* terminal Status */
		if (r->h->rs[Rs_answerbackstring])
			rxvt_tt_write(r, page,
				(const unsigned char *)r->h->rs[Rs_answerbackstring],
				(unsigned int)STRLEN(r->h->rs[Rs_answerbackstring]));
		else
			rxvt_tt_write(r, page, (unsigned char *)VT100_ANS,
				(unsigned int)STRLEN(VT100_ANS));
		break;
	case C0_BEL:	/* bell */
		rxvt_scr_bell(r);
		break;
	case C0_BS:		/* backspace */
		rxvt_scr_backspace(r, page);
		break;
	case C0_HT:		/* tab */
		rxvt_scr_tab(r, page, 1);
		break;
	case C0_CR:		/* carriage return */
		rxvt_scr_gotorc(r, page, 0, 0, R_RELATIVE);
		break;
	case C0_VT:		/* vertical tab, form feed */
	case C0_FF:
	case C0_LF:		/* line feed */
		rxvt_scr_index(r, page, UP);
		break;
	case C0_SO:		/* shift out - acs */
		rxvt_scr_charset_choose(r, page, 1);
		break;
	case C0_SI:		/* shift in - acs */
		rxvt_scr_charset_choose(r, page, 0);
		break;
	}
}
/*}}} */



/*{{{ process VT52 escape sequences */
/* INTPROTO */
void
rxvt_process_escape_vt52(rxvt_t* r, int page, unsigned char ch)
{
	int		row, col;
	int		readpage = page;
#ifdef DEBUG
	clock_t	checksum = PVTS(r, page)->checksum;
#endif


	switch (ch) {
	case 'A':		/* cursor up */
		rxvt_scr_gotorc(r, page, -1, 0, R_RELATIVE | C_RELATIVE);	
		break;
	case 'B':		/* cursor down */
		rxvt_scr_gotorc(r, page, 1, 0, R_RELATIVE | C_RELATIVE);	
		break;
	case 'C':		/* cursor right */
		rxvt_scr_gotorc(r, page, 0, 1, R_RELATIVE | C_RELATIVE);	
		break;
	case 'D':		/* cursor left */
		rxvt_scr_gotorc(r, page, 0, -1, R_RELATIVE | C_RELATIVE);	
		break;
	case 'H':		/* cursor home */
		rxvt_scr_gotorc(r, page, 0, 0, 0);	
		break;
	case 'I':		/* cursor up and scroll down if needed */
		rxvt_scr_index(r, page, DN);
		break;
	case 'J':		/* erase to end of screen */
		rxvt_scr_erase_screen(r, page, 0);
		break;
	case 'K':		/* erase to end of line */
		rxvt_scr_erase_line(r, page, 0);
		break;
	case 'Y':		 	/* move to specified row and col */
		/* full command is 'ESC Y row col' where row and col
		** are encoded by adding 32 and sending the ascii
		** character.  eg. SPACE = 0, '+' = 13, '0' = 18,
		** etc.
		*/
		row = rxvt_cmd_getc(r, &readpage) - ' ';
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		col = rxvt_cmd_getc(r, &readpage) - ' ';
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		rxvt_scr_gotorc(r, page, row, col, 0);
		break;
	case 'Z':		/* identify the terminal type */
		/* I am a VT100 emulating a VT52 */
		rxvt_tt_printf(r, page, "\033/Z");
		break;
	case '<':		/* turn off VT52 mode */
		PrivMode(0, PrivMode_vt52, page);
		break;
	case 'F':	 	/* use special graphics character set */
	case 'G':		   /* use regular character set */
		/* unimplemented */
		break;
	case '=':	 	/* use alternate keypad mode */
	case '>':		   /* use regular keypad mode */
		/* unimplemented */
		break;
	}
}
/*}}} */



/*{{{ process escape sequences */
/* INTPROTO */
void
rxvt_process_escape_seq(rxvt_t* r, int page)
{
	int			readpage = page;
#ifdef DEBUG
	clock_t		checksum = PVTS(r, page)->checksum;
#endif


	unsigned char   ch = rxvt_cmd_getc(r, &readpage);
	assert (page == readpage);	/* in case */
	assert (checksum == PVTS(r, page)->checksum);
	if (PVTS(r, page)->PrivateModes & PrivMode_vt52) {
		rxvt_process_escape_vt52(r, page, ch);
		return;
	}
	
	assert (page == readpage);	/* possible race condition? */
	switch (ch) {
	/* case 1:		do_tek_mode (); break; */
	case '#':
		if (rxvt_cmd_getc(r, &readpage) == '8')	{
			assert (page == readpage);
			assert (checksum == PVTS(r, page)->checksum);
			rxvt_scr_E(r, readpage);
		}
		break;
	case '(':
		rxvt_scr_charset_set(r, page, 0,
			(unsigned int)rxvt_cmd_getc(r, &readpage));
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		break;
	case ')':
		rxvt_scr_charset_set(r, page, 1,
			(unsigned int)rxvt_cmd_getc(r, &readpage));
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		break;
	case '*':
		rxvt_scr_charset_set(r, page, 2,
			(unsigned int)rxvt_cmd_getc(r, &readpage));
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		break;
	case '+':
		rxvt_scr_charset_set(r, page, 3,
			(unsigned int)rxvt_cmd_getc(r, &readpage));
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		break;
#ifdef MULTICHAR_SET
	case '$':
		rxvt_scr_charset_set(r, page, -2,
			(unsigned int)rxvt_cmd_getc(r, &readpage));
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		break;
#endif
#ifndef NO_FRILLS
	case '6':
		rxvt_scr_backindex(r, page);
		break;
#endif
	case '7':
		rxvt_scr_cursor(r, page, SAVE);
		break;
	case '8':
		rxvt_scr_cursor(r, page, RESTORE);
		break;
#ifndef NO_FRILLS
	case '9':
		rxvt_scr_forwardindex(r, page);
		break;
#endif
	case '=':
	case '>':
		PrivMode((ch == '='), PrivMode_aplKP, page);
		break;

	case C1_40:
		rxvt_cmd_getc(r, &readpage);
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		break;
	case C1_44:
		rxvt_scr_index(r, page, UP);
		break;

	/* 8.3.87: NEXT LINE */
	case C1_NEL:		/* ESC E */
		rxvt_scr_add_lines(r, page, (const unsigned char *)"\n\r", 1, 2);
		break;

	/* kidnapped escape sequence: Should be 8.3.48 */
	case C1_ESA:		/* ESC G */
		rxvt_process_graphics(r, page);
		break;

	/* 8.3.63: CHARACTER TABULATION SET */
	case C1_HTS:		/* ESC H */
		rxvt_scr_set_tab(r, page, 1);
		break;

	/* 8.3.105: REVERSE LINE FEED */
	case C1_RI:			/* ESC M */
		rxvt_scr_index(r, page, DN);
		break;

	/* 8.3.142: SINGLE-SHIFT TWO */
	/*case C1_SS2: scr_single_shift (2);   break; */

	/* 8.3.143: SINGLE-SHIFT THREE */
	/*case C1_SS3: scr_single_shift (3);   break; */

	/* 8.3.27: DEVICE CONTROL STRING */
	case C1_DCS:		/* ESC P */
		/* rxvt_process_dcs_seq(r, page); */
		rxvt_process_xwsh_seq (r, page);
		break;

	/* 8.3.110: SINGLE CHARACTER INTRODUCER */
	case C1_SCI:		/* ESC Z */
		rxvt_tt_write(r, page, (const unsigned char *)ESCZ_ANSWER,
			  (unsigned int)(sizeof(ESCZ_ANSWER) - 1));
		break;			/* steal obsolete ESC [ c */

	/* 8.3.16: CONTROL SEQUENCE INTRODUCER */
	case C1_CSI:		/* ESC [ */
		rxvt_process_csi_seq(r, page);
		break;

	/* 8.3.90: OPERATING SYSTEM COMMAND */
	case C1_OSC:		/* ESC ] */
		rxvt_process_osc_seq(r, page);
		break;

	/* 8.3.106: RESET TO INITIAL STATE */
	case 'c':
		rxvt_scr_poweron(r, page);
#ifdef HAVE_SCROLLBARS
		rxvt_scrollbar_update(r, 1);
#endif
		break;

	/* 8.3.79: LOCKING-SHIFT TWO (see ISO2022) */
	case 'n':
		rxvt_scr_charset_choose(r, page, 2);
		break;

	/* 8.3.81: LOCKING-SHIFT THREE (see ISO2022) */
	case 'o':
		rxvt_scr_charset_choose(r, page, 3);
		break;
	}
}
/*}}} */


/*{{{ process CONTROL SEQUENCE INTRODUCER (CSI) sequences `ESC[' */
/* *INDENT-OFF* */
enum {
	CSI_ICH = 0x40,
	CSI_CUU, CSI_CUD, CSI_CUF, CSI_CUB, CSI_CNL, CSI_CPL, CSI_CHA,
	CSI_CUP, CSI_CHT, CSI_ED , CSI_EL , CSI_IL , CSI_DL , CSI_EF , CSI_EA ,
	CSI_DCH, CSI_SEE, CSI_CPR, CSI_SU , CSI_SD , CSI_NP , CSI_PP , CSI_CTC,
	CSI_ECH, CSI_CVT, CSI_CBT, CSI_SRS, CSI_PTX, CSI_SDS, CSI_SIMD, CSI_5F,
	CSI_HPA, CSI_HPR, CSI_REP, CSI_DA , CSI_VPA, CSI_VPR, CSI_HVP, CSI_TBC,
	CSI_SM , CSI_MC , CSI_HPB, CSI_VPB, CSI_RM , CSI_SGR, CSI_DSR, CSI_DAQ,
	CSI_70 , CSI_71 , CSI_72 , CSI_73 , CSI_74 , CSI_75 , CSI_76 , CSI_77 ,
	CSI_78 , CSI_79 , CSI_7A , CSI_7B , CSI_7C , CSI_7D , CSI_7E , CSI_7F 
};

#define make_byte(b7,b6,b5,b4,b3,b2,b1,b0)			\
	(((b7) << 7) | ((b6) << 6) | ((b5) << 5) | ((b4) << 4)	\
	 | ((b3) << 3) | ((b2) << 2) | ((b1) << 1) | (b0))
#define get_byte_array_bit(array, bit)				\
	(!!((array)[(bit) / 8] & (128 >> ((bit) & 7))))

const unsigned char csi_defaults[] = {
	make_byte(1,1,1,1,1,1,1,1),	/* @, A, B, C, D, E, F, G, */
	make_byte(1,1,0,0,1,1,0,0),	/* H, I, J, K, L, M, N, O, */
	make_byte(1,0,1,1,1,1,1,0),	/* P, Q, R, S, T, U, V, W, */
	make_byte(1,1,1,0,0,0,1,0),	/* X, Y, Z, [, \, ], ^, _, */
	make_byte(1,1,1,0,1,1,1,0),	/* `, a, b, c, d, e, f, g, */
	make_byte(0,0,1,1,0,0,0,0),	/* h, i, j, k, l, m, n, o, */
	make_byte(0,0,0,0,0,0,0,0),	/* p, q, r, s, t, u, v, w, */
	make_byte(0,0,0,0,0,0,0,0)	/* x, y, z, {, |, }, ~,	*/
};
/* *INDENT-ON* */


/* INTPROTO */
void
rxvt_process_csi_seq(rxvt_t* r, int page)
{
	int				readpage = page;
#ifdef DEBUG
	clock_t			checksum = PVTS(r, page)->checksum;
#endif
	unsigned char   ch, priv, i;
	unsigned int	nargs, p;
	int				n, ndef;
	int				arg[ESC_ARGS];


	for (nargs = ESC_ARGS; nargs > 0;)
		arg[--nargs] = 0;

	priv = 0;
	ch = rxvt_cmd_getc(r, &readpage);
	assert (page == readpage);
	assert (checksum == PVTS(r, page)->checksum);
	if (ch >= '<' && ch <= '?') {	/* '<' '=' '>' '?' */
		priv = ch;
		ch = rxvt_cmd_getc(r, &readpage);
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
	}

	/* read any numerical arguments */
	for (n = -1; ch < CSI_ICH; ) {
		if (isdigit(ch)) {
			if (n < 0)
				n = ch - '0';
			else
				n = n * 10 + ch - '0';
		}
		else if (ch == ';') {
			if (nargs < ESC_ARGS)
				arg[nargs++] = n;
			n = -1;
		}
		else if (ch == '\b') {
			rxvt_scr_backspace(r, page);
		}
		else if (ch == C0_ESC) {
			rxvt_process_escape_seq(r, page);
			return;
		}
		else if (ch < ' ') {
			rxvt_process_nonprinting(r, page, ch);
		}

		ch = rxvt_cmd_getc(r, &readpage);
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
	}

	if (ch > CSI_7F)
		return;

	if (nargs < ESC_ARGS)
	arg[nargs++] = n;

	i = ch - CSI_ICH;
	ndef = get_byte_array_bit(csi_defaults, i);
	for (p = 0; p < nargs; p++)
		if (arg[p] == -1)
			arg[p] = ndef;

#ifdef DEBUG_CMD
	fprintf(stderr, "CSI ");
	for (p = 0; p < nargs; p++)
		fprintf(stderr, "%d%s", arg[p], p < nargs - 1 ? ";" : "");
	fprintf(stderr, "%c\n", ch);
#endif


	/* private mode handling */
	if (priv) {
		switch (priv) {
		case '>':
			if (ch == CSI_DA)	/* secondary device attributes */
			rxvt_tt_printf(r, page, "\033[>%d;%-.8s;0c", 'R', VERSION);
			break;
		case '?':
			if (ch == 'h' || ch == 'l' || ch == 'r' ||
				ch == 's' || ch == 't')	{
				rxvt_process_terminal_mode(r, page, ch, priv,
					nargs, arg);
			}
			break;
		}
		return;
	}


	switch (ch) {
	/* ISO/IEC 6429:1992(E) CSI sequences (defaults in parentheses) */
#ifdef PRINTPIPE
	case CSI_MC:		/* 8.3.83: (0) MEDIA COPY */
		switch (arg[0]) {
		case 0:			/* initiate transfer to primary aux device */
			rxvt_scr_printscreen(r, page, 0);
			break;
		case 5:			/* start relay to primary aux device */
			rxvt_process_print_pipe(r, page);
			break;
		}
		break;
#endif

	case CSI_CUU:		/* 8.3.22: (1) CURSOR UP */
	case CSI_VPR:		/* 8.3.161: (1) LINE POSITION FORWARD */
		arg[0] = -arg[0];
		/* FALLTHROUGH */
	case CSI_CUD:		/* 8.3.19: (1) CURSOR DOWN */
	case CSI_VPB:		/* 8.3.160: (1) LINE POSITION BACKWARD */
		rxvt_scr_gotorc(r, page, arg[0], 0, RELATIVE);
		break;

	case CSI_CUB:		/* 8.3.18: (1) CURSOR LEFT */
	case CSI_HPB: 		/* 8.3.59: (1) CHARACTER POSITION BACKWARD */
#ifdef ISO6429
		arg[0] = -arg[0];
#else				/* emulate common DEC VTs */
		arg[0] = arg[0] ? -arg[0] : -1;
#endif
		/* FALLTHROUGH */
	case CSI_CUF:		/* 8.3.20: (1) CURSOR RIGHT */
	case CSI_HPR:		/* 8.3.60: (1) CHARACTER POSITION FORWARD */
#ifdef ISO6429
		rxvt_scr_gotorc(r, page, 0, arg[0], RELATIVE);
#else				/* emulate common DEC VTs */
		rxvt_scr_gotorc(r, page, 0, arg[0] ? arg[0] : 1, RELATIVE);
#endif
		break;

	case CSI_CPL:		/* 8.3.13: (1) CURSOR PRECEDING LINE */
		arg[0] = -arg[0];
		/* FALLTHROUGH */
	case CSI_CNL:		/* 8.3.12: (1) CURSOR NEXT LINE */
		rxvt_scr_gotorc(r, page, arg[0], 0, R_RELATIVE);
		break;

	case CSI_CHA:		/* 8.3.9: (1) CURSOR CHARACTER ABSOLUTE */
	case CSI_HPA:		/* 8.3.58: (1) CURSOR POSITION ABSOLUTE */
		rxvt_scr_gotorc(r, page, 0, arg[0] - 1, R_RELATIVE);
		break;

	case CSI_VPA:		/* 8.3.159: (1) LINE POSITION ABSOLUTE */
		rxvt_scr_gotorc(r, page, arg[0] - 1, 0, C_RELATIVE);
		break;

	case CSI_CUP:		/* 8.3.21: (1,1) CURSOR POSITION */
	case CSI_HVP:		/* 8.3.64: (1,1) CHARACTER AND LINE POSITION */
		rxvt_scr_gotorc(r, page, arg[0] - 1, nargs < 2 ? 0 : (arg[1] - 1), 0);
		break;

	case CSI_CBT:		/* 8.3.7: (1) CURSOR BACKWARD TABULATION */
		arg[0] = -arg[0];
		/* FALLTHROUGH */
	case CSI_CHT:		/* 8.3.10: (1) CURSOR FORWARD TABULATION */
		rxvt_scr_tab(r, page, arg[0]);
		break;

	case CSI_ED:		/* 8.3.40: (0) ERASE IN PAGE */
		rxvt_scr_erase_screen(r, page, arg[0]);
		break;

	case CSI_EL:		/* 8.3.42: (0) ERASE IN LINE */
		rxvt_scr_erase_line(r, page, arg[0]);
		break;

	case CSI_ICH:		/* 8.3.65: (1) INSERT CHARACTER */
		rxvt_scr_insdel_chars(r, page, arg[0], INSERT);
		break;

	case CSI_IL:		/* 8.3.68: (1) INSERT LINE */
		rxvt_scr_insdel_lines(r, page, arg[0], INSERT);
		break;

	case CSI_DL:		/* 8.3.33: (1) DELETE LINE */
		rxvt_scr_insdel_lines(r, page, arg[0], DELETE);
		break;

	case CSI_ECH:		/* 8.3.39: (1) ERASE CHARACTER */
		rxvt_scr_insdel_chars(r, page, arg[0], ERASE);
		break;

	case CSI_DCH:		/* 8.3.26: (1) DELETE CHARACTER */
		rxvt_scr_insdel_chars(r, page, arg[0], DELETE);
		break;

	case CSI_SD:		/* 8.3.114: (1) SCROLL DOWN */
		arg[0] = -arg[0];
		/* FALLTHROUGH */
	case CSI_SU:		/* 8.3.148: (1) SCROLL UP */
		rxvt_scroll_text(r, page, PVTS(r, page)->screen.tscroll,
			PVTS(r, page)->screen.bscroll, arg[0], 0);
		break;

	case CSI_DA:		/* 8.3.24: (0) DEVICE ATTRIBUTES */
		rxvt_tt_write(r, page, (const unsigned char *)VT100_ANS,
			(unsigned int)(sizeof(VT100_ANS) - 1));
		break;

	case CSI_SGR:		/* 8.3.118: (0) SELECT GRAPHIC RENDITION */
		rxvt_process_sgr_mode(r, page, nargs, arg);
		break;

	case CSI_DSR:		/* 8.3.36: (0) DEVICE STATUS REPORT */
		switch (arg[0]) {
		case 5:			/* DSR requested */
			rxvt_tt_printf(r, page, "\033[0n");
			break;
		case 6:			/* CPR requested */
			rxvt_scr_report_position(r, page);
			break;
#if defined (ENABLE_DISPLAY_ANSWER)
		case 7:			/* unofficial extension */
			rxvt_tt_printf(r, page, "%-.250s\n",
				r->h->rs[Rs_display_name]);
			break;
#endif
		case 8:			/* unofficial extension */
			rxvt_xterm_seq(r, page, XTerm_title, APL_NAME "-" VERSION,
				CHAR_ST);
			break;
		}	/* switch (arg[0]) */
		break;

	case CSI_TBC:		/* 8.3.155: (0) TABULATION CLEAR */
		switch (arg[0]) {
		case 0:			/* char tab stop cleared at active position */
			rxvt_scr_set_tab(r, page, 0);
			break;
		/* case 1: */		/* line tab stop cleared in active line */
		/* case 2: */		/* char tab stops cleared in active line */
		case 3:			/* all char tab stops are cleared */
		/* case 4: */		/* all line tab stops are cleared */
		case 5:			/* all tab stops are cleared */
			rxvt_scr_set_tab(r, page, -1);
			break;
		}	/* switch (arg[0]) */
		break;

	case CSI_CTC:		/* 8.3.17: (0) CURSOR TABULATION CONTROL */
		switch (arg[0]) {
		case 0:			/* char tab stop set at active position */
			rxvt_scr_set_tab(r, page, 1);
			break;		/* = ESC H */
		/* case 1: */		/* line tab stop set at active line */
		case 2:			/* char tab stop cleared at active position */
			rxvt_scr_set_tab(r, page, 0);
			break;		/* = ESC [ 0 g */
		/* case 3: */		/* line tab stop cleared at active line */
		/* case 4: */		/* char tab stops cleared at active line */
		case 5:			/* all char tab stops are cleared */
			rxvt_scr_set_tab(r, page, -1);
			break;		/* = ESC [ 3 g */
		/* case 6: */		/* all line tab stops are cleared */
		}	/* switch (arg[0]) */
		break;

	case CSI_RM:		/* 8.3.107: RESET MODE */
		if (arg[0] == 4)
			rxvt_scr_insert_mode(r, page, 0);
		break;

	case CSI_SM:		/* 8.3.126: SET MODE */
		if (arg[0] == 4)
			rxvt_scr_insert_mode(r, page, 1);
		break;

	/* PRIVATE USE beyond this point.  All CSI_7? seqeunces here */ 
	case CSI_72:		/* DECSTBM: set top and bottom margins */
		if (nargs == 1)
			rxvt_scr_scroll_region(r, page, arg[0] - 1, MAX_ROWS - 1);
		else if (nargs == 0 || arg[0] >= arg[1])
			rxvt_scr_scroll_region(r, page, 0, MAX_ROWS - 1);
		else 
			rxvt_scr_scroll_region(r, page, arg[0] - 1, arg[1] - 1);
		break;

	case CSI_73:
		rxvt_scr_cursor(r, page, SAVE);
		break;
	case CSI_75:
		rxvt_scr_cursor(r, page, RESTORE);
		break;

#ifndef NO_FRILLS
	case CSI_74:
		rxvt_process_window_ops(r, page, arg, nargs);
		break;
#endif

	case CSI_78:		/* DECREQTPARM */
		if (arg[0] == 0 || arg[0] == 1)
			rxvt_tt_printf(r, page, "\033[%d;1;1;112;112;1;0x",
				arg[0] + 2);
		/* FALLTHROUGH */
	default:
		break;
	}
}
/*}}} */



#ifndef NO_FRILLS
/* ARGSUSED */
/* INTPROTO */
void
rxvt_process_window_ops(rxvt_t* r, int page, const int *args, unsigned int nargs)
{
	int					x, y;
	unsigned int		w, h;
	XWindowAttributes	wattr;
	Window				wdummy;


	if (nargs == 0)
		return;
	switch (args[0]) {
	/* commands */
	case 1:			/* deiconify window */
		XMapWindow (r->Xdisplay, r->TermWin.parent);
		break;
	case 2:			/* iconify window */
		XIconifyWindow (r->Xdisplay, r->TermWin.parent,
			DefaultScreen(r->Xdisplay));
		break;
	case 3:			/* set position (pixels) */
		XMoveWindow (r->Xdisplay, r->TermWin.parent, args[1], args[2]);
		break;
	case 4:			/* set size (pixels) */
		w = (unsigned int) args[2];
		h = (unsigned int) args[1];
		XResizeWindow (r->Xdisplay, r->TermWin.parent, w, h);
		break;
	case 5:			/* raise window */
		XRaiseWindow (r->Xdisplay, r->TermWin.parent);
		break;
	case 6:			/* lower window */
		XLowerWindow (r->Xdisplay, r->TermWin.parent);
		break;
	case 7:			/* refresh window */
		rxvt_scr_touch (r, page, True);
		break;
	case 8:			/* set size (chars) */
		w = (unsigned int) (Width2Pixel(args[2]) + r->szHint.base_width);
		h = (unsigned int) (Height2Pixel(args[1]) + r->szHint.base_height);
		XResizeWindow (r->Xdisplay, r->TermWin.parent, w, h);
		break;
	default:
		if (args[0] >= 24)	{
			/* set height (chars) */
			w = (unsigned int) r->szHint.width;
			h = (unsigned int) (args[1] * r->TermWin.fheight + r->szHint.base_height);
			XResizeWindow (r->Xdisplay, r->TermWin.parent, w, h);
		}
		break;

	/* reports - some output format copied from XTerm */
	case 11:			/* report window state */
		XGetWindowAttributes(r->Xdisplay, r->TermWin.parent, &wattr);
		rxvt_tt_printf(r, page, "\033[%dt",
			wattr.map_state == IsViewable ? 1 : 2);
		break;
	case 13:			/* report window position */
		XGetWindowAttributes(r->Xdisplay, r->TermWin.parent, &wattr);
		XTranslateCoordinates(r->Xdisplay, r->TermWin.parent,
			wattr.root, -wattr.border_width, -wattr.border_width,
			&x, &y, &wdummy);
		rxvt_tt_printf(r, page, "\033[3;%d;%dt", x, y);
		break;
	case 14:			/* report window size (pixels) */
		XGetWindowAttributes(r->Xdisplay, r->TermWin.parent, &wattr);
		rxvt_tt_printf(r, page, "\033[4;%d;%dt", wattr.height,
			wattr.width);
		break;
	case 18:			/* report window size (chars) */
		rxvt_tt_printf(r, page, "\033[8;%d;%dt", r->TermWin.nrow,
			r->TermWin.ncol);
		break;
	}
}
#endif	/* NO_FRILLS */


/*----------------------------------------------------------------------*/
/*
 * get input up until STRING TERMINATOR (or BEL)
 * ends_how is terminator used.  returned input must be free()d
 */
/* INTPROTO */
unsigned char  *
rxvt_get_to_st(rxvt_t* r, int page, unsigned char *ends_how)
{
	int				readpage = page;
#ifdef DEBUG
	clock_t			checksum = PVTS(r, page)->checksum;
#endif
	int				seen_esc = 0;	/* seen escape? */
	unsigned int	n = 0;
	unsigned char*	s;
	unsigned char   ch, string[STRING_MAX];


	for (; (ch = rxvt_cmd_getc(r, &readpage));) {
		assert (readpage == page);
		assert (checksum == PVTS(r, page)->checksum);
		if (ch == C0_BEL ||
			ch == CHAR_ST ||
			(ch == 0x5c && seen_esc))	/* 7bit ST */
			break;
		if (ch == C0_ESC) {
			seen_esc = 1;
			continue;
		}
		else if (ch == '\t')
			ch = ' ';	/* translate '\t' to space */
		else if (ch < 0x08 || (ch > 0x0d && ch < 0x20))	{
			/* return NULL;	*/ /* other control character - exit */

			/*
			** NO! See http://vt100.net/emu/dec_ansi_parser for reason.
			** I am not sure what to do with CAN or SUB, but the rest
			** should be *ignored* - I will try to find out what to do
			** with CAN and SUB.
			**            - Johann 'Mykraverk' Oskarsson
			**              <johann@myrkraverk.com>
			*/
			continue; /* do nothing */
		}

		if (n < sizeof(string) - 1)
			string[n++] = ch;

		seen_esc = 0;
	}

	string[n++] = '\0';
	if ((s = (unsigned char UNTAINTED *) STRNDUP (string, n)) == NULL)
		return NULL;
	*ends_how = (ch == 0x5c ? C0_ESC : ch);

	return s;
}


/*----------------------------------------------------------------------*/
/*
 * process DEVICE CONTROL STRING `ESC P ... (ST|BEL)' or `0x90 ... (ST|BEL)'
 */
/* INTPROTO */
void
rxvt_process_dcs_seq(rxvt_t* r, int page)
{
	unsigned char	eh, *s;

	/* Not handled yet */
	s = rxvt_get_to_st (r, page, &eh);
	if (s)
		free(s);
	return;
}

/*----------------------------------------------------------------------*/
/*
 * process OPERATING SYSTEM COMMAND sequence `ESC ] Ps ; Pt (ST|BEL)'
 */
/* INTPROTO */
void
rxvt_process_osc_seq (rxvt_t* r, int page)
{
	int				readpage = page;
#ifdef DEBUG
	clock_t			checksum = PVTS(r, page)->checksum;
#endif
	unsigned char	ch, eh, *s;
	int				arg;

	ch = rxvt_cmd_getc(r, &readpage);
	assert (page == readpage);
	assert (checksum == PVTS(r, page)->checksum);
	for (arg = 0; isdigit(ch);
		ch = rxvt_cmd_getc(r, &readpage))	{
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		arg = arg * 10 + (ch - '0');
	}

	if (ch == ';') {
		s = rxvt_get_to_st(r, page, &eh);
		if (s) {
			/*
			** rxvt_menubar_dispatch() violates the constness of the
			** string, so do it here
			*/
			/* XXX: currently disabled due to security concerns */
			if (arg == XTerm_Menu)
				/* rxvt_menubar_dispatch(r, (char *)s) */;
			else
				rxvt_xterm_seq(r, page, arg, (char *)s, eh);
			free(s);
		}
	}
}


/*
 * Xwsh escape sequences: ESC P Ps.y;Pt ESC \
 *       1 = change title
 *       3 = change iconName
 *       4 = set text color by string
 *       5 = set page color by string
 *       6 = set selection text color by string
 *       7 = set selection page color by string
 *       8 = set cursor text color by string
 *       9 = set cursor page color by string
 *      10 = set half intensity by string
 *      11 = set bold intensity by string
 *     101 = bind string to key Ps+1 and pass as value
 *     103 = bind string to key Ps+1 and pass s function
 */
/* INTPROTO */
void
rxvt_xwsh_seq(rxvt_t* r, int op, const char *str)
{
	assert(str != NULL);
	switch (op) {
	case Xwsh_title:
		rxvt_set_term_title (r, (const unsigned char*) str);
		break;
	case Xwsh_iconName:
		rxvt_set_icon_name (r, (const unsigned char*) str);
		break;
	case Xwsh_textColor:
		break;
	case Xwsh_pageColor:
		break;
	case Xwsh_selTextColor:
		break;
	case Xwsh_selPageColor:
		break;
	case Xwsh_cursorTextColor:
		break;
	case Xwsh_cursorPageColor:
		break;
	case Xwsh_halfIntColor:
		break;
	case Xwsh_boldIntColor:
		break;
	case Xwsh_bindStrKeyVal:
		break;
	case Xwsh_bindStrKeyFunc:
		break;
	}
}


/* INTPROTO */
void
rxvt_process_xwsh_seq (rxvt_t* r, int page)
{
	int				readpage = page;
#ifdef DEBUG
	clock_t			checksum = PVTS(r, page)->checksum;
#endif
	unsigned char   ch, string[STRING_MAX];
	int             arg;


	ch = rxvt_cmd_getc (r, &readpage);
	assert (page == readpage);
	assert (checksum == PVTS(r, page)->checksum);
	for (arg = 0; isdigit((int) ch); ch = rxvt_cmd_getc(r, &readpage)) {
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
		arg = arg * 10 + (ch - '0');
	}

	if (ch == '.') {
		if ((ch = rxvt_cmd_getc(r, &readpage)) == 'y') {
			int             n = 0;

			assert (page == readpage);
			assert (checksum == PVTS(r, page)->checksum);
			while ((ch = rxvt_cmd_getc(r, &readpage)) != '\033') {
				assert (page == readpage);
				assert (checksum == PVTS(r, page)->checksum);
				if (ch) {
					if (ch == '\t')
						ch = ' ';	/* translate '\t' to space */
					else if (ch < ' ')
						return;	/* control character - exit */

					if (n < sizeof(string) - 1)
						string[n++] = ch;
				}
			}

			if ((ch = rxvt_cmd_getc(r, &readpage)) == '\\') {
				assert (page == readpage);
				assert (checksum == PVTS(r, page)->checksum);
				string[n] = '\0';
				rxvt_xwsh_seq(r, arg, (char*) string);
			}
		}
	}
}



#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
/* INTPROTO */
void
rxvt_refresh_bg_image (rxvt_t* r, int page)
{
# ifdef TRANSPARENT
	if (r->Options & Opt_transparent)
		/* reset background */
		rxvt_check_our_parents (r);
	else
# endif	/* TRANSPARENT */
# ifdef BACKGROUND_IMAGE
	{	/* reset background */
		register int	i;
		for (i = 0; i <= LTAB(r); i ++)
			rxvt_resize_pixmap (r, i);
	}
# endif	/* BACKGROUND_IMAGE */
	{	/* empty body to suppress compile error */	}

	rxvt_scr_clear (r, page);
	rxvt_scr_touch (r, page, True);
}
#endif	/* TRANSPARENT || BACKGROUND_IMAGE */


/*
 * XTerm escape sequences: ESC ] Ps;Pt (ST|BEL)
 *	   0 = change iconName/title
 *	   1 = change iconName
 *	   2 = change title
 *	   4 = change color
 *	  12 = change text color
 *	  13 = change mouse foreground color 
 *	  17 = change highlight character colour
 *	  18 = change bold character color
 *	  19 = change underlined character color 
 *	  46 = change logfile (not implemented)
 *	  50 = change font
 *
 * rxvt extensions:
 *	  10 = menu (may change in future)
 *	  20 = bg pixmap
 *	  39 = change default fg color
 *	  49 = change default bg color
 *	  55 = dump scrollback buffer and all of screen
 */
/* EXTPROTO */
void
rxvt_xterm_seq(rxvt_t* r, int page, int op, const char *str, unsigned char resp __attribute__((unused)))
{
#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
	int			changed = 0;
#endif
	int			color;
	char		*buf, *name;
#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
	int			bgfade;
# ifdef TINTING_SUPPORT
	int			shade;
# endif
#endif


	assert(str != NULL);
	switch (op) {
	case XTerm_name:
		rxvt_set_term_title(r, (const unsigned char*) str);
		/* FALLTHROUGH */
	case XTerm_iconName:
		rxvt_set_icon_name(r, (const unsigned char*) str);
		if (r->Options2 & Opt2_syncTabIcon)
		    rxvt_tabbar_set_title (r, ATAB(r), (const unsigned char TAINTED*) str);
		break;
	case XTerm_title:
#ifndef SET_TAB_TITLE_NOT_WIN_TITLE
		rxvt_set_term_title(r, (const unsigned char*) str);
#endif
#ifdef SET_TAB_TITLE_ON_XTERM_SEQUENCE
		rxvt_tabbar_set_title (r, page, (const unsigned char TAINTED*) str);
#endif
		break;
	case XTerm_Color:
		for (buf = (char *)str; buf && *buf;) {
			if ((name = STRCHR(buf, ';')) == NULL)
				break;
			*name++ = '\0';
			color = atoi(buf);
			if (color < 0 || color >= TOTAL_COLORS)
				break;
			if ((buf = STRCHR(name, ';')) != NULL)
				*buf++ = '\0';
			rxvt_set_window_color(r, color + minCOLOR, name);
		}
		break;
#ifndef NO_CURSORCOLOR
	case XTerm_Color_cursor:
		rxvt_set_window_color(r, Color_cursor, str);
		break;
#endif
	case XTerm_Color_pointer:
		rxvt_set_window_color(r, Color_pointer, str);
		break;
#ifndef NO_BOLD_UNDERLINE_REVERSE
	case XTerm_Color_BD:
		rxvt_set_window_color(r, Color_BD, str);
		break;
	case XTerm_Color_UL:
		rxvt_set_window_color(r, Color_UL, str);
		break;
	case XTerm_Color_RV:
		rxvt_set_window_color(r, Color_RV, str);
		break;
#endif

	case XTerm_Menu:
		/*
		 * rxvt_menubar_dispatch() violates the constness of the string,
		 * so DON'T do it here
		 */
		break;
#ifdef BACKGROUND_IMAGE
	case XTerm_Pixmap:
		if (*str != ';') {
			/* reset to default scaling */
			rxvt_scale_pixmap(r, page, "");
			/* change pixmap */
			rxvt_load_bg_pixmap(r, page, str);
			rxvt_scr_touch(r, page, True);
		}
		while ((str = STRCHR(str, ';')) != NULL) {
			str++;
			changed += rxvt_scale_pixmap(r, page, str);
		}
		if (changed) {
			rxvt_resize_pixmap(r, page);
			rxvt_scr_touch(r, page, True);
		}
		break;
#endif

	case XTerm_restoreFG:
		rxvt_set_window_color(r, Color_fg, str);
		break;
	case XTerm_restoreBG:
		rxvt_set_window_color(r, Color_bg, str);
		break;
	case XTerm_logfile:
		break;
	case XTerm_font:
		rxvt_resize_on_font (r, (char*) str);
		break;
	/*
	case XTerm_dumpscreen:
		{
			int			 fd;
			if ((fd=open(str, O_RDWR | O_CREAT | O_EXCL, 0600))>=0) {
			rxvt_scr_dump(r, page, fd);
			close(fd);
			}
		}
		break;
	*/
	/*
	** Mrxvt extension to set tab title and terminal title
	** Example: echo "\e]61;newtitle\a"
	*/
	case Xterm_tabterm:
		rxvt_set_term_title(r, (const unsigned char*) str);
		/* FALLTHROUGH */
	case Xterm_tab:
		rxvt_tabbar_set_title (r, page, (const unsigned char TAINTED*) str);
		break;
	case Xterm_newtab:
		rxvt_append_page (r, str);
		break;
	case Xterm_prevtab:
		if (0 != page)
			rxvt_activate_page (r, page-1);
		else if (0 != LTAB(r))
			rxvt_activate_page (r, LTAB(r));
		break;
	case Xterm_nexttab:
		if (page != LTAB(r))
			rxvt_activate_page (r, page+1);
		else if (0 != LTAB(r))
			rxvt_activate_page (r, 0);
		break;

#ifdef MULTICHAR_SET
	case Xterm_encode:
		/* We only change encoding method, but not font ;-) */
		rxvt_set_multichar_encoding (r, str);	
		break;
#endif	/* MULTICHAR_SET */

	case Xterm_hide:
#ifdef HAVE_SCROLLBARS
		if ('s' == *str || 'S' == *str)	{ /* show/hide scrollbar */
			rxvt_hotkey_hide_scrollbar (r, 0);
		}
		else 
#endif	/* HAVE_SCROLLBARS */
#ifdef HAVE_MENUBAR
		if ('m' == *str || 'M' == *str)	{ /* show/hide menubar */
			rxvt_hotkey_hide_menubar (r, 0);
		}
		else
#endif	/* HAVE_MENUBAR */
		{
			rxvt_hotkey_hide_tabbar (r, 0);
		}
		break;

	case Xterm_tabbtn:
		rxvt_hotkey_hide_button (r, 0);
		break;

	case Xterm_opacity:
		if (None != r->h->xa[XA_NET_WM_WINDOW_OPACITY])	{
			int		oldopacity = r->TermWin.opacity;
			int		tmp;

			if ('-' == *str && (char) 0 == *(str+1))	{
				/* handle '-' only, remember it's opposite  */
				tmp = (r->h->rs[Rs_opacityDegree]) ?
						r->TermWin.opacity_degree : 1;
			}
			else if ('+' == *str && (char) 0 == *(str+1))	{
				/* handle '+' only, remember it's opposite  */
				tmp = (r->h->rs[Rs_opacityDegree]) ?
						-(r->TermWin.opacity_degree) : -1;
			}
			else	{
				/* other cases, remember it's opposite  */
				tmp = 100 - atoi (str);
				/* only change opacity when it is valid */
				if (tmp < 0 || tmp > 100)
					tmp = oldopacity;
			}
			if ('-' == *str || '+' == *str)
				/* use input as offset */
				r->TermWin.opacity += tmp;
			else
				/* use input as absolute value */
				r->TermWin.opacity = tmp;
			if (r->TermWin.opacity < 0)
				r->TermWin.opacity = 0;
			if (r->TermWin.opacity > 100)
				r->TermWin.opacity = 100;
			/* only change opacity when it has changed */
			if (r->TermWin.opacity != oldopacity)
				rxvt_set_opacity (r);
		}
		break;

	case Xterm_tabfg:
	case Xterm_tabbg:
	case Xterm_itabfg:
	case Xterm_itabbg:
		rxvt_tabbar_change_color (r, op, str);
		break;

#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
# ifdef TINTING_SUPPORT
	case Xterm_tint:
		if (ISSET_PIXCOLOR (r->h, Color_tint) &&
			r->h->rs[Rs_shade])
			rxvt_set_window_color(r, Color_tint, str);
		break;

	case Xterm_shade:
		if (!ISSET_PIXCOLOR (r->h, Color_tint) ||
			!r->h->rs[Rs_shade])
			break;

		shade = atoi (str);
		if (shade >=0 && shade <= 100)	{
			shade = 100 - shade;	/* reverse it */
			changed = (r->TermWin.shade != shade);
			r->TermWin.shade = shade;
		}

		if (changed)	{
			/* shade value is changed, need to refresh terminals */
			rxvt_refresh_bg_image (r, page);
		}
		break;
# endif	/* TINTING_SUPPORT */
#endif	/* TRANSPARENT || BACKGROUND_IMAGE */

#ifdef TRANSPARENT
	case Xterm_trans:
		rxvt_toggle_transparency (r);
		break;
#endif	/* TRANSPARENT */

	case Xterm_moveleft:
		rxvt_tabbar_move_tab (r, 0);
		break;
	case Xterm_moveright:
		rxvt_tabbar_move_tab (r, 1);
		break;

	case Xterm_verybold:
		rxvt_hotkey_verybold (r, 0);
		break;

	case Xterm_hotkeys:
		if (r->Options2 & Opt2_disableHotkeys)	{
			r->Options2 &= ~Opt2_disableHotkeys;
			rxvt_toggle_hotkeys (r, 1);
		}
		else	{
			r->Options2 |= Opt2_disableHotkeys;
			rxvt_toggle_hotkeys (r, 0);
		}
		break;

	case Xterm_saveconfig:
		rxvt_hotkey_save_config (r, 0);
		break;

#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
	case Xterm_bgfade:
		if (r->h->rs[Rs_backgroundFade])	{
			bgfade = atoi (str);
			if (bgfade >= 0 && bgfade <= 100 &&
				r->TermWin.bgfade != (100 - bgfade))	{
				r->TermWin.bgfade = 100 - bgfade;

				rxvt_refresh_bg_image (r, page);
			}
		}
		break;
#endif

	case Xterm_termenv:
		PVTS(r, page)->termenv = rxvt_get_termenv ((const char*) str);
		break;

	default:
		break;
	}
}
/*----------------------------------------------------------------------*/


/*{{{ process DEC private mode sequences `ESC [ ? Ps mode' */
/*
 * mode can only have the following values:
 *	  'l' = low
 *	  'h' = high
 *	  's' = save
 *	  'r' = restore
 *	  't' = toggle
 * so no need for fancy checking
 */
/* INTPROTO */
int
rxvt_privcases(rxvt_t* r, int page, int mode, unsigned long bit)
{
	int			 state;

	if (mode == 's') {
		PVTS(r, page)->SavedModes |= (PVTS(r, page)->PrivateModes & bit);
		return -1;
	}
	else {
		if (mode == 'r')
			/* no overlapping */
			state = (PVTS(r, page)->SavedModes & bit) ? 1 : 0;
		else
			state = (mode == 't') ? !(PVTS(r, page)->PrivateModes & bit) : mode;
		PrivMode(state, bit, page);
	}
	return state;
}


/* we're not using priv _yet_ */
/* INTPROTO */
void
rxvt_process_terminal_mode(rxvt_t* r, int page, int mode, int priv __attribute__((unused)), unsigned int nargs, const int *arg)
{
	unsigned int	i, j;
	int				state;
	static const struct {
		const int	   argval;
		const unsigned long bit;
	} argtopriv[] = {
		{ 1, PrivMode_aplCUR },
		{ 2, PrivMode_vt52 },
		{ 3, PrivMode_132 },
		{ 4, PrivMode_smoothScroll },
		{ 5, PrivMode_rVideo },
		{ 6, PrivMode_relOrigin },
		{ 7, PrivMode_Autowrap },
		{ 9, PrivMode_MouseX10 },
#ifdef menuBar_esc
		{ menuBar_esc, PrivMode_menuBar },
#endif
#ifdef scrollBar_esc
		{ scrollBar_esc, PrivMode_scrollBar },
#endif
		{ 25, PrivMode_VisibleCursor },
		{ 35, PrivMode_ShiftKeys },
		{ 40, PrivMode_132OK },
		{ 47, PrivMode_Screen },
		{ 66, PrivMode_aplKP },
#ifndef NO_BACKSPACE_KEY
		{ 67, PrivMode_BackSpace },
#endif
		{ 1000, PrivMode_MouseX11 },
		{ 1010, PrivMode_TtyOutputInh },
		{ 1011, PrivMode_Keypress },
		{ 1047, PrivMode_Screen },
	};

	if (nargs == 0)
		return;

	/* make lo/hi boolean */
	if (mode == 'l')
		mode = 0;		/* reset */
	else if (mode == 'h')
		mode = 1;		/* set */

	for (i = 0; i < nargs; i++) {
		state = -1;

		/* basic handling */
		for (j = 0; j < (sizeof(argtopriv)/sizeof(argtopriv[0])); j++)
			if (argtopriv[j].argval == arg[i]) {
				state = rxvt_privcases(r, page, mode, argtopriv[j].bit);
				break;
			}
	
		/* extra handling for values with state unkept  */
		if (state == -1)
			switch (arg[i]) {
			case 1048:		/* alternative cursor save */
				if (mode == 0)
					rxvt_scr_cursor(r, page, RESTORE);
				else if (mode == 1)
					rxvt_scr_cursor(r, page, SAVE);
				/* FALLTHROUGH */
			default:
				continue;	/* for(;i;) */
			}

		/* extra handling for values with valid 0 or 1 state */
		switch (arg[i]) {
		/* case 1:	- application cursor keys */
		case 2:			/* VT52 mode */
			  /* oddball mode.  should be set regardless of set/reset
			   * parameter.  Return from VT52 mode with an ESC < from
			   * within VT52 mode
			   */
			PrivMode(1, PrivMode_vt52, page);
			break;
		case 3:			/* 80/132 */
			if (PVTS(r, page)->PrivateModes & PrivMode_132OK)	{
				unsigned int w = Width2Pixel((state ? 132 : 80)) + r->szHint.base_width;
				unsigned int h = r->szHint.base_height;
				XResizeWindow (r->Xdisplay, r->TermWin.parent, w, h);
			}
			break;
		case 4:			/* smooth scrolling */
			if (state)
			r->Options &= ~Opt_jumpScroll;
			else
			r->Options |= Opt_jumpScroll;
			break;
		case 5:			/* reverse video */
			rxvt_scr_rvideo_mode(r, page, state);
			break;
		case 6:			/* relative/absolute origins  */
			rxvt_scr_relative_origin(r, page, state);
			break;
		case 7:			/* autowrap */
			rxvt_scr_autowrap(r, page, state);
			break;
		/* case 8:	- auto repeat, can't do on a per window basis */
		case 9:			/* X10 mouse reporting */
			if (state)		/* orthogonal */
				PVTS(r, page)->PrivateModes &= ~(PrivMode_MouseX11);
			break;
#ifdef HAVE_MENUBAR
# ifdef menuBar_esc
		case menuBar_esc:
			if (state)	{
				if (rxvt_menubar_show(r))	{
					rxvt_resize_on_subwin (r, SHOW_MENUBAR);
				}
			}
			else	{
				if (rxvt_menubar_hide(r))	{
					rxvt_resize_on_subwin (r, HIDE_MENUBAR);
				}
			}
			break;
# endif
#endif
#ifdef HAVE_SCROLLBARS
# ifdef scrollBar_esc
		case scrollBar_esc:
			if (state)	{
				if (rxvt_scrollbar_show (r))	{
					rxvt_resize_on_subwin (r, SHOW_SCROLLBAR);
				}
			}
			else	{
				if (rxvt_scrollbar_hide (r))	{
					rxvt_resize_on_subwin (r, HIDE_SCROLLBAR);
				}
			}
			break;
# endif
#endif
		case 25:		/* visible/invisible cursor */
			rxvt_scr_cursor_visible(r, page, state);
			break;
		/* case 35:	- shift keys */
		/* case 40:	- 80 <--> 132 mode */
		case 47:		/* secondary screen */
			rxvt_scr_change_screen(r, page, state);
			break;
		/* case 66:	- application key pad */
		/* case 67:	- backspace key */
		case 1000:		/* X11 mouse reporting */
			if (state)		/* orthogonal */
				PVTS(r, page)->PrivateModes &= ~(PrivMode_MouseX10);
			break;
#if 0
		case 1001:
			break;		/* X11 mouse highlighting */
#endif
		case 1010:		/* scroll to bottom on TTY output inhibit */
			if (state)
				r->Options |= Opt_scrollTtyOutputInhibit;
			else
				r->Options &= ~Opt_scrollTtyOutputInhibit;
			break;
		case 1011:		/* scroll to bottom on key press */
			if (state)
				r->Options |= Opt_scrollTtyKeypress;
			else
				r->Options &= ~Opt_scrollTtyKeypress;
			break;
		case 1047:		/* secondary screen w/ clearing */
			if (PVTS(r, page)->current_screen != PRIMARY)
				rxvt_scr_erase_screen(r, page, 2);
			rxvt_scr_change_screen(r, page, state);
		/* FALLTHROUGH */
		default:
			break;
		}
	}
}
/*}}} */

/*{{{ process sgr sequences */
/* INTPROTO */
void
rxvt_process_sgr_mode(rxvt_t* r, int page, unsigned int nargs, const int *arg)
{
	unsigned int	i;
	short			rendset;
	int				rendstyle = 0;

	if (nargs == 0) {
		rxvt_scr_rendition(r, page, 0, ~RS_None);
		return;
	}
	for (i = 0; i < nargs; i++) {
		rendset = -1;
		switch (arg[i]) {
		case 0:
			rendset = 0, rendstyle = ~RS_None;
			break;
		case 1:
			rendset = 1, rendstyle = RS_Bold;
			break;
		case 4:
			rendset = 1, rendstyle = RS_Uline;
			break;
		case 5:
			rendset = 1, rendstyle = RS_Blink;
			break;
		case 7:
			rendset = 1, rendstyle = RS_RVid;
			break;
		case 22:
			rendset = 0, rendstyle = RS_Bold;
			break;
		case 24:
			rendset = 0, rendstyle = RS_Uline;
			break;
		case 25:
			rendset = 0, rendstyle = RS_Blink;
			break;
		case 27:
			rendset = 0, rendstyle = RS_RVid;
			break;
		}
		if (rendset != -1) {
			rxvt_scr_rendition(r, page, rendset, rendstyle);
			continue;		/* for(;i;) */
		}

		switch (arg[i]) {
		case 30:
		case 31:		/* set fg color */
		case 32:
		case 33:
		case 34:
		case 35:
		case 36:
		case 37:
			rxvt_scr_color(r, page,
				(unsigned int)(minCOLOR+(arg[i]-30)), Color_fg);
			break;
#ifdef TTY_256COLOR
		case 38:
			if (nargs > i + 2 && arg[i + 1] == 5) {
				rxvt_scr_color(r, page,
					(unsigned int)(minCOLOR+arg[i+2]), Color_fg);
				i += 2;
			}
			break;
#endif
		case 39:		/* default fg */
			rxvt_scr_color(r, page, Color_fg, Color_fg);
			break;

		case 40:
		case 41:		/* set bg color */
		case 42:
		case 43:
		case 44:
		case 45:
		case 46:
		case 47:
			rxvt_scr_color(r, page,
				(unsigned int)(minCOLOR+(arg[i]-40)), Color_bg);
			break;
#ifdef TTY_256COLOR
		case 48:
			if (nargs > i + 2 && arg[i + 1] == 5) {
				rxvt_scr_color(r, page,
					(unsigned int)(minCOLOR+arg[i+2]), Color_bg);
				i += 2;
			}
			break;
#endif
		case 49:		/* default bg */
			rxvt_scr_color(r, page, Color_bg, Color_bg);
			break;

#ifndef NO_BRIGHTCOLOR
		case 90:
		case 91:		/* set bright fg color */
		case 92:
		case 93:
		case 94:
		case 95:
		case 96:
		case 97:
			rxvt_scr_color(r, page,
				(unsigned int)(minBrightCOLOR+(arg[i]-90)), Color_fg);
			break;
		case 100:
		case 101:		/* set bright bg color */
		case 102:
		case 103:
		case 104:
		case 105:
		case 106:
		case 107:
			rxvt_scr_color(r, page,
				(unsigned int)(minBrightCOLOR+(arg[i]-100)), Color_bg);
			break;
#endif
		}
	}
}
/*}}} */

/*{{{ process Rob Nation's own graphics mode sequences */
/* INTPROTO */
void
rxvt_process_graphics(rxvt_t* r, int page)
{
	int				readpage = page;
#ifdef DEBUG
	clock_t			checksum = PVTS(r, page)->checksum;
#endif
	unsigned char   ch, cmd = rxvt_cmd_getc(r, &readpage);

	assert (page == readpage);
	assert (checksum == PVTS(r, page)->checksum);
	if (cmd == 'Q') {		/* query graphics */
		rxvt_tt_printf(r, page, "\033G0\n");	/* no graphics */
		return;
	}
/* swallow other graphics sequences until terminating ':' */
	do	{
		ch = rxvt_cmd_getc(r, &readpage);
		assert (page == readpage);
		assert (checksum == PVTS(r, page)->checksum);
	}
	while (ch != ':')
		;
}
/*}}} */

/* ------------------------------------------------------------------------- */

/*{{{ Read and process output from the application */
/* LIBPROTO */
void
rxvt_main_loop(rxvt_t *r)
{
	register int		i;
	unsigned char		ch, *str;
	int					page, nlines, refreshnow;
	struct rxvt_hidden*	h = r->h;


	/* Send the screen size. */
	for (i = 0; i <= LTAB(r); i ++)	{
		rxvt_tt_winsize(PVTS(r, i)->cmd_fd,
			r->TermWin.ncol, r->TermWin.nrow, PVTS(r, i)->cmd_pid);
	}

	refreshnow = 0;
	while (1) {
		/* wait for something */
		page = -1;
		while (r->vt_died <= 0 &&
			(0 == (ch = rxvt_cmd_getc(r, &page))))
			;

		/* handle the case that some children have died */
		if (r->vt_died > 0)	{
			rxvt_clean_cmd_page (r);
			continue;
		}

		if (ch >= ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
			/* Read a text string from the input buffer */
			/*
			** point `str' to the start of the string,
			** decrement first since it was post incremented in
			** rxvt_cmd_getc()
			*/
			str = --(PVTS(r, page)->cmdbuf_ptr);
			nlines = 0;
			while (PVTS(r, page)->cmdbuf_ptr <
				PVTS(r, page)->cmdbuf_endp) {
				assert (PVTS(r, page)->cmdbuf_base <=
					PVTS(r, page)->cmdbuf_endp);

				ch = *(PVTS(r, page)->cmdbuf_ptr)++;
				if (ch == '\n') {
					register int	limit;

					limit = h->refresh_limit * (r->TermWin.nrow - 1);
					nlines++;
					h->refresh_count++;
					if (!(r->Options & Opt_jumpScroll) ||
						(h->refresh_count >= limit)) {
						refreshnow = 1;
						break;
					}
				}
				else if (ch < ' ' && ch != '\t' && ch != '\r') {
					/* unprintable */
					PVTS(r, page)->cmdbuf_ptr--;
					break;
				}
			}

			DBG_MSG(2, (stderr, "adding '%s' in %d\n", str, page));
			rxvt_scr_add_lines(r, page, str, nlines,
				(PVTS(r, page)->cmdbuf_ptr - str));

			/*
			** If there have been a lot of new lines, then update the
			** screen.
			** I'll cheat and only refresh less than every page-full.
			** The number of pages between refreshes is
			** h->refresh_limit, which is incremented here because we
			** must be doing flat-out scrolling.
			**
			** refreshing should be correct for small scrolls, because
			** of the time-out
			*/
			if (refreshnow) {
				refreshnow = 0;
				if ((r->Options & Opt_jumpScroll) &&
					h->refresh_limit < REFRESH_PERIOD)
					h->refresh_limit++;
# ifdef XFT_SUPPORT
				/* disable screen refresh if XFT antialias is
				** used to improve performance */
				if (!((r->Options & Opt_xft) &&
					  (r->Options2 & Opt2_xftAntialias)))
# endif
				rxvt_scr_refresh(r, page, h->refresh_type);
			}
		}
		else	{
			switch (ch) {
			default:
				rxvt_process_nonprinting(r, page, ch);
				break;
			case C0_ESC:	/* escape char */
				rxvt_process_escape_seq(r, page);
				break;
			/* case 0x9b: */	/* CSI */
			/*  rxvt_process_csi_seq(r, ATAB(r)); */
			}
		}
	}
	/* NOTREACHED */
	assert (0);
}


/*
 * Send printf() formatted output to the command.
 * Only use for small amounts of data.
 */
/* EXTPROTO */
void
rxvt_tt_printf(rxvt_t* r, int page, const char *fmt,...)
{
	va_list		 arg_ptr;
	unsigned char   buf[256];

	va_start(arg_ptr, fmt);
	vsnprintf((char *)buf, sizeof(buf)-1, fmt, arg_ptr);
	buf[sizeof(buf)-1] = (unsigned char) 0;
	va_end(arg_ptr);
	rxvt_tt_write(r, page, buf, (unsigned int)STRLEN(buf));
}


/* ---------------------------------------------------------------------- */
/* Addresses pasting large amounts of data and rxvt hang
 * code pinched from xterm (v_write()) and applied originally to
 * rxvt-2.18 - Hops
 * Write data to the pty as typed by the user, pasted with the mouse,
 * or generated by us in response to a query ESC sequence.
 */
/* EXTPROTO */
void
rxvt_tt_write(rxvt_t* r, int page, const unsigned char *d, int len)
{
#define MAX_PTY_WRITE 128	/* 1/2 POSIX minimum MAX_INPUT */
	register int	k, beg, end;

	if (r->Options2 & Opt2_broadcast)	{
		beg = 0; end = LTAB(r);
	}
	else	{
		beg = end = page;
	}

	for (k = beg; k <= end; k ++)	{
		int				riten;
		int				p;
		/* start of physical buffer		*/
		unsigned char*	v_buffer;
		/* start of current buffer pending */
		unsigned char*	v_bufstr;
		/* end of current buffer pending   */
		unsigned char*	v_bufptr;
		/* end of physical buffer		  */
		unsigned char*	v_bufend;
	
		DBG_MSG(2, (stderr, "rxvt_tt_write %d (%s)\n", k,
			d ? (char*) d: "nil"));
	
		if (NULL == PVTS(r, k)->v_bufstr && len > 0) {
			p = (len / MAX_PTY_WRITE + 1) * MAX_PTY_WRITE;
			if (p <= 0) /* possible integer overflow */
				return ;
			v_buffer = v_bufstr = v_bufptr = rxvt_malloc(p);
			v_bufend = v_buffer + p;
		}
		else {
			v_buffer = PVTS(r, k)->v_buffer;
			v_bufstr = PVTS(r, k)->v_bufstr;
			v_bufptr = PVTS(r, k)->v_bufptr;
			v_bufend = PVTS(r, k)->v_bufend;
		}
	
		/*
		** Append to the block we already have. Always doing this
		** simplifies the code, and isn't too bad, either. If this
		** is a short block, it isn't too expensive, and if this is
		** a long block, we won't be able to write it all anyway.
		*/
		if (len > 0) {
			if (v_bufend < v_bufptr + len) {
				/* run out of room */
				if (v_bufstr != v_buffer) {
					/* there is unused space, move everything down */
					MEMMOVE(v_buffer, v_bufstr,
						(unsigned int)(v_bufptr - v_bufstr));
					v_bufptr -= v_bufstr - v_buffer;
					v_bufstr = v_buffer;
				}
				if (v_bufend < v_bufptr + len) {
					/* still won't fit: get more space, use most basic
					** realloc because an error is not fatal. */
					unsigned int	size = v_bufptr - v_buffer;
					unsigned int	reallocto;
	
					reallocto = ((size+len) / MAX_PTY_WRITE + 1) * MAX_PTY_WRITE;
					v_buffer = rxvt_realloc(v_buffer, reallocto);
					/* save across realloc */
					if (v_buffer) {
						v_bufstr = v_buffer;
						v_bufptr = v_buffer + size;
						v_bufend = v_buffer + reallocto;
					}
					else {
						/* no memory: ignore entire write request */
						rxvt_print_error("data loss: cannot allocate buffer space");
						/* restore clobbered pointer */
						v_buffer = v_bufstr;
					}
				}
			}
			if (v_bufend >= v_bufptr + len) {
				/* new stuff will fit */
				MEMCPY(v_bufptr, d, len);
				v_bufptr += len;
			}
		}
	
		/*
		** Write out as much of the buffer as we can. Be careful not
		** to overflow the pty's input silo. We are conservative here
		** and only write a small amount at a time.
		**
		** If we can't push all the data into the pty yet, we expect
		** write to return a non-negative number less than the length
		** requested (if some data written) or -1 and set errno to
		** EAGAIN, EWOULDBLOCK, or EINTR (if no data written).
		**
		** (Not all systems do this, sigh, so the code is actually
		** a little more forgiving.)
		*/
	
		if ((p = v_bufptr - v_bufstr) > 0) {
			riten = write(PVTS(r, k)->cmd_fd, v_bufstr, min(p, MAX_PTY_WRITE));
			DBG_MSG(3, (stderr, " tt_write %d chars to vts[%d].cmd_fd = %d\n", riten, k, PVTS(r, k)->cmd_fd));
			if (riten < 0)
				riten = 0;
			v_bufstr += riten;
			if (v_bufstr >= v_bufptr)	/* we wrote it all */
				v_bufstr = v_bufptr = v_buffer;
		}
	
		/*
		** If we have lots of unused memory allocated, return it
		*/
		if (v_bufend - v_bufptr > MAX_PTY_WRITE * 8) {
			/* arbitrary hysteresis, save pointers across realloc */
			unsigned int	start = v_bufstr - v_buffer;
			unsigned int	size = v_bufptr - v_buffer;
			unsigned int	reallocto;
		
			reallocto = (size / MAX_PTY_WRITE + 1) * MAX_PTY_WRITE;
			v_buffer = rxvt_realloc(v_buffer, reallocto);
			if (v_buffer) {
				v_bufstr = v_buffer + start;
				v_bufptr = v_buffer + size;
				v_bufend = v_buffer + reallocto;
			}
			else {
				/* should we print a warning if couldn't return memory? */
				/* restore clobbered pointer */
				v_buffer = v_bufstr - start;
			}
		}
		PVTS(r, k)->v_buffer = v_buffer;
		PVTS(r, k)->v_bufstr = v_bufstr;
		PVTS(r, k)->v_bufptr = v_bufptr;
		PVTS(r, k)->v_bufend = v_bufend;
	}	/* for */
}
/*----------------------- end-of-file (C source) -----------------------*/
