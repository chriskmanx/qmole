/*--------------------------------*-C-*---------------------------------*
 * File:	menubar.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997,1998   mj olesen <olesen@me.QueensU.CA>
 * Copyright (c) 2004        Jingmin Zhou <jimmyzhou@users.sourceforge.net>
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
** $Id: menubar.c,v 1.33 2004/11/24 00:10:52 cvs Exp $
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


#ifdef HAVE_MENUBAR


#define CHOOSE_GC_FG(R, PIXCOL)	\
	XSetForeground ((R)->Xdisplay, (R)->menuBar.gc, (PIXCOL))

#define Menu_PixelWidth(menu)					\
	(2 * SHADOW + Width2Pixel ((menu)->width + 3 * HSPACE))


/*--------------------------------------------------------------------*
 *         BEGIN `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/
menuitem_t*   rxvt_menuitem_find          (const menu_t*, const char*);
void          rxvt_menuitem_free          (rxvt_t*, menu_t*, menuitem_t*);
int           rxvt_action_type            (action_t*, unsigned char*);
int           rxvt_action_dispatch        (rxvt_t*, action_t*);
int           rxvt_menuarrow_find         (char);
void          rxvt_menuarrow_free         (rxvt_t*, char);
void          rxvt_menuarrow_add          (rxvt_t*, char*);
menuitem_t*   rxvt_menuitem_add           (menu_t*, const char*, const char*, const char*);
char*         rxvt_menu_find_base         (rxvt_t*, menu_t**, char*);
menu_t*       rxvt_menu_delete            (rxvt_t*, menu_t*);
menu_t*       rxvt_menu_add               (rxvt_t*, menu_t*, char*);
void          rxvt_drawbox_menubar        (rxvt_t*, int, int, int);
void          rxvt_menubar_draw_triangle  (rxvt_t*, int, int, int);
void          rxvt_drawbox_menuitem       (rxvt_t*, int, int);
#ifdef DEBUG_MENU_LAYOUT
void          rxvt_print_menu_ancestors   (menu_t*);
void          rxvt_print_menu_descendants (menu_t*);
#endif
void          rxvt_menu_show              (rxvt_t*);
void          rxvt_menu_display           (rxvt_t*, void (*update)(rxvt_t*));
void          rxvt_menu_hide_all          (rxvt_t*);
void          rxvt_menu_hide              (rxvt_t*);
void          rxvt_menu_clear             (rxvt_t*, menu_t*);
void          rxvt_menubar_clear          (rxvt_t*);
#if  (MENUBAR_MAX > 1)
bar_t*        rxvt_menubar_find           (rxvt_t*, const char*);
int           rxvt_menubar_push           (rxvt_t*, const char*);
void          rxvt_menubar_remove         (rxvt_t*, const char*);
void          rxvt_action_decode          (FILE*, action_t*);
void          rxvt_menu_dump              (FILE*, menu_t*);
void          rxvt_menubar_dump           (rxvt_t*, FILE*);
#endif
void          rxvt_draw_arrows            (rxvt_t*, int, int);
int           rxvt_menu_select            (rxvt_t*, XButtonEvent*);
void          rxvt_menubar_select         (rxvt_t*, XButtonEvent*);
/*--------------------------------------------------------------------*
 *         END   `INTERNAL' ROUTINE PROTOTYPES                        *
 *--------------------------------------------------------------------*/


static const struct {
	const char	  name;	/* (l)eft, (u)p, (d)own, (r)ight */
	const unsigned char str[5];	/* str[0] = STRLEN (str+1) */
} Arrows[NARROWS] = {
	{ 'l', "\003\033[D" },
	{ 'u', "\003\033[A" },
	{ 'd', "\003\033[B" },
	{ 'r', "\003\033[C" }
};

/*}}} */

/*
 * find an item called NAME in MENU
 */
/* INTPROTO */
menuitem_t*
rxvt_menuitem_find(const menu_t *menu, const char *name)
{
	menuitem_t	 *item;

#ifdef DEBUG
	assert(name != NULL);
	assert(menu != NULL);
#endif

/* find the last item in the menu, this is good for separators */
	for (item = menu->tail; item != NULL; item = item->prev) {
	if (item->entry.type == MenuSubMenu) {
		if (!STRCMP(name, (item->entry.submenu.menu)->name))
		break;
	} else if ((isSeparator(name) && isSeparator(item->name))
		   || !STRCMP(name, item->name))
		break;
	}
	return item;
}

/*
 * unlink ITEM from its MENU and free its memory
 */
/* INTPROTO */
void
rxvt_menuitem_free(rxvt_t *r, menu_t *menu, menuitem_t *item)
{
/* disconnect */
	menuitem_t	 *prev, *next;

#ifdef DEBUG
	assert(menu != NULL);
#endif

	prev = item->prev;
	next = item->next;
	if (prev != NULL)
	prev->next = next;
	if (next != NULL)
	next->prev = prev;

/* new head, tail */
	if (menu->tail == item)
	menu->tail = prev;
	if (menu->head == item)
	menu->head = next;

	switch (item->entry.type) {
	case MenuAction:
	case MenuTerminalAction:
	free(item->entry.action.str);
	break;
	case MenuSubMenu:
	rxvt_menu_delete(r, item->entry.submenu.menu);
	break;
	}
	if (item->name != NULL)
	free(item->name);
	if (item->name2 != NULL)
	free(item->name2);
	free(item);
}

/*
 * sort command vs. terminal actions and
 * remove the first character of STR if it's '\0'
 */
/* INTPROTO */
int
rxvt_action_type(action_t *action, unsigned char *str)
{
	unsigned int	len;

#if defined (DEBUG_MENU) || defined (DEBUG_MENUARROWS)
	len = STRLEN(str);
	fprintf(stderr, "(len %d) = %s\n", len, str);
#else
	len = rxvt_str_escaped((char *)str);
#endif

	if (!len)
	return -1;

/* sort command vs. terminal actions */
	action->type = MenuAction;
	if (str[0] == '\0') {
	/* the functional equivalent: memmove (str, str+1, len); */
	unsigned char  *dst = (str);
	unsigned char  *src = (str + 1);
	unsigned char  *end = (str + len);

	while (src <= end)
		*dst++ = *src++;

	len--;			/* decrement length */
	if (str[0] != '\0')
		action->type = MenuTerminalAction;
	}
	action->str = str;
	action->len = len;

	return 0;
}

/* INTPROTO */
int
rxvt_action_dispatch(rxvt_t *r, action_t *action)
{
	switch (action->type) {
	case MenuTerminalAction:
		rxvt_cmd_write(r, ATAB(r), action->str, action->len);
	break;

	case MenuAction:
		rxvt_tt_write(r, ATAB(r), action->str, action->len);
	break;

	default:
	return -1;
	break;
	}
	return 0;
}

/* return the arrow index corresponding to NAME */
/* INTPROTO */
int
rxvt_menuarrow_find(char name)
{
	int			 i;

	for (i = 0; i < NARROWS; i++)
	if (name == Arrows[i].name)
		return i;
	return -1;
}

/* free the memory associated with arrow NAME of the current menubar */
/* INTPROTO */
void
rxvt_menuarrow_free(rxvt_t *r, char name)
{
	int			 i;

	if (name) {
	i = rxvt_menuarrow_find(name);
	if (i >= 0) {
		action_t	   *act = &(r->h->CurrentBar->arrows[i]);

		switch (act->type) {
		case MenuAction:
		case MenuTerminalAction:
		free(act->str);
		act->str = NULL;
		act->len = 0;
		break;
		}
		act->type = MenuLabel;
	}
	} else {
	for (i = 0; i < NARROWS; i++)
		rxvt_menuarrow_free(r, Arrows[i].name);
	}
}

/* INTPROTO */
void
rxvt_menuarrow_add(rxvt_t *r, char *string)
{
	int			 i;
	unsigned		xtra_len;
	char		   *p;
	struct {
	char		   *str;
	int			 len;
	}
	beg = { NULL, 0 },
	end = { NULL, 0 },
	*cur,
	parse[NARROWS];

	MEMSET(parse, 0, sizeof(parse));

/* fprintf(stderr, "add arrows = `%s'\n", string); */
	for (p = string; p != NULL && *p; string = p) {
	p = (string + 3);
	/* fprintf(stderr, "parsing at %s\n", string); */
	switch (string[1]) {
	case 'b':
		cur = &beg;
		break;
	case 'e':
		cur = &end;
		break;

	default:
		i = rxvt_menuarrow_find(string[1]);
		if (i >= 0)
		cur = &(parse[i]);
		else
		continue;	/* not found */
		break;
	}

	string = p;
	cur->str = string;
	cur->len = 0;

	if (cur == &end) {
		p = STRCHR(string, '\0');
	} else {
		char		   *next = string;

		while (1) {
		p = STRCHR(next, '<');
		if (p != NULL) {
			if (p[1] && p[2] == '>')
			break;
			/* parsed */
		} else {
			if (beg.str == NULL)	/* no end needed */
			p = STRCHR(next, '\0');
			break;
		}
		next = (p + 1);
		}
	}

	if (p == NULL)
		return;
	cur->len = (p - string);
	}

#ifdef DEBUG_MENUARROWS
	cur = &beg;
	fprintf(stderr, "<b>(len %d) = %.*s\n",
		cur->len, cur->len, (cur->str ? cur->str : ""));
	for (i = 0; i < NARROWS; i++) {
	cur = &(parse[i]);
	fprintf(stderr, "<%c>(len %d) = %.*s\n",
		Arrows[i].name,
		cur->len, cur->len, (cur->str ? cur->str : ""));
	}
	cur = &end;
	fprintf(stderr, "<e>(len %d) = %.*s\n",
		cur->len, cur->len, (cur->str ? cur->str : ""));
#endif

	xtra_len = (beg.len + end.len);
	for (i = 0; i < NARROWS; i++) {
	if (xtra_len || parse[i].len)
		rxvt_menuarrow_free(r, Arrows[i].name);
	}

	for (i = 0; i < NARROWS; i++) {
		unsigned char  *str;
		unsigned int	len;

		if (!parse[i].len)
			continue;

		/* possible integer overflow? */
		assert (parse[i].len >= 0 && xtra_len >= 0);
		assert (parse[i].len + xtra_len + 1 > 0);
		str = rxvt_malloc(parse[i].len + xtra_len + 1);

		len = 0;
		if (beg.len) {
			STRNCPY(str + len, beg.str, beg.len);
			len += beg.len;
		}
		STRNCPY(str + len, parse[i].str, parse[i].len);
		len += parse[i].len;

		if (end.len) {
			STRNCPY(str + len, end.str, end.len);
			len += end.len;
		}
		str[len] = '\0';

#ifdef DEBUG_MENUARROWS
		fprintf(stderr, "<%c>(len %d) = %s\n", Arrows[i].name, len, str);
#endif
		if (rxvt_action_type(&(r->h->CurrentBar->arrows[i]), str) < 0)
			free(str);
	}
}

/* INTPROTO */
menuitem_t	 *
rxvt_menuitem_add(menu_t *menu, const char *name, const char *name2, const char *action)
{
	menuitem_t	 *item;
	unsigned int	len;

#ifdef DEBUG
	assert(name != NULL);
	assert(action != NULL);
#endif

	if (menu == NULL)
	return NULL;

	if (isSeparator(name)) {
	/* add separator, no action */
	name = "";
	action = "";
	} else {
	/*
	 * add/replace existing menu item
	 */
	item = rxvt_menuitem_find(menu, name);
	if (item != NULL) {
		if (item->name2 != NULL && name2 != NULL) {
		free(item->name2);
		item->len2 = 0;
		item->name2 = NULL;
		}
		switch (item->entry.type) {
		case MenuAction:
		case MenuTerminalAction:
		free(item->entry.action.str);
		item->entry.action.str = NULL;
		break;
		}
		goto Item_Found;
	}
	}
/* allocate a new itemect */
	item = (menuitem_t *) rxvt_malloc(sizeof(menuitem_t));

	item->len2 = 0;
	item->name2 = NULL;

	len = STRLEN(name);
	/* possible integer overflow? */
	assert (len >= 0 && len + 1 >= 0);
	item->name = rxvt_malloc(len + 1);
	STRCPY(item->name, name);
	if (name[0] == '.' && name[1] != '.')
	len = 0;		/* hidden menu name */
	item->len = len;

/* add to tail of list */
	item->prev = menu->tail;
	item->next = NULL;

	if (menu->tail != NULL)
	(menu->tail)->next = item;
	menu->tail = item;
/* fix head */
	if (menu->head == NULL)
	menu->head = item;

/*
 * add action
 */
  Item_Found:
	if (name2 != NULL && item->name2 == NULL) {
		len = STRLEN(name2);
		if (len == 0)
			item->name2 = NULL;
		else {
			/* possible integer overflow? */
			assert (len > 0 && len + 1 > 0);
			item->name2 = rxvt_malloc(len + 1);
			STRCPY(item->name2, name2);
		}
		item->len2 = len;
	}
	item->entry.type = MenuLabel;
	len = STRLEN(action);

	if (len == 0 && item->name2 != NULL) {
		action = item->name2;
		len = item->len2;
	}
	if (len) {
		unsigned char*	str;
		/* possible integer overflow? */
		assert (len > 0 && len + 1 > 0);
		str = rxvt_malloc(len + 1);

		STRCPY(str, action);

		if (rxvt_action_type(&(item->entry.action), str) < 0)
			free(str);
	}
/* new item and a possible increase in width */
	if (menu->width < (item->len + item->len2))
		menu->width = (item->len + item->len2);

	return item;
}

/*
 * search for the base starting menu for NAME.
 * return a pointer to the portion of NAME that remains
 */
/* INTPROTO */
char*
rxvt_menu_find_base(rxvt_t *r, menu_t **menu, char *path)
{
	menu_t		 *m = NULL;
	menuitem_t	 *item;

#ifdef DEBUG
	assert(menu != NULL);
	assert(r->h->CurrentBar != NULL);
#endif

	if (path[0] == '\0')
	return path;

	if (STRCHR(path, '/') != NULL) {
	char		   *p = path;

	while ((p = STRCHR(p, '/')) != NULL) {
		p++;
		if (*p == '/')
		path = p;
	}
	if (path[0] == '/') {
		path++;
		*menu = NULL;
	}
	while ((p = STRCHR(path, '/')) != NULL) {
		p[0] = '\0';
		if (path[0] == '\0')
		return NULL;
		if (!STRCMP(path, DOT)) {
		/* nothing to do */
		} else if (!STRCMP(path, DOTS)) {
		if (*menu != NULL)
			*menu = (*menu)->parent;
		} else {
		path = rxvt_menu_find_base(r, menu, path);
		if (path[0] != '\0') {	/* not found */
			p[0] = '/';	/* fix-up name again */
			return path;
		}
		}

		path = (p + 1);
	}
	}
	if (!STRCMP(path, DOTS)) {
	path += STRLEN(DOTS);
	if (*menu != NULL)
		*menu = (*menu)->parent;
	return path;
	}
/* find this menu */
	if (*menu == NULL) {
	for (m = r->h->CurrentBar->tail; m != NULL; m = m->prev) {
		if (!STRCMP(path, m->name))
		break;
	}
	} else {
	/* find this menu */
	for (item = (*menu)->tail; item != NULL; item = item->prev) {
		if (item->entry.type == MenuSubMenu
		&& !STRCMP(path, (item->entry.submenu.menu)->name)) {
		m = (item->entry.submenu.menu);
		break;
		}
	}
	}
	if (m != NULL) {
	*menu = m;
	path += STRLEN(path);
	}
	return path;
}

/*
 * delete this entire menu
 */
/* INTPROTO */
menu_t*
rxvt_menu_delete(rxvt_t *r, menu_t *menu)
{
	menu_t		 *parent = NULL, *prev, *next;
	menuitem_t	 *item;
	bar_t		  *CurrentBar = r->h->CurrentBar;

#ifdef DEBUG
	assert(CurrentBar != NULL);
#endif

/* delete the entire menu */
	if (menu == NULL)
	return NULL;

	parent = menu->parent;

/* unlink MENU */
	prev = menu->prev;
	next = menu->next;
	if (prev != NULL)
	prev->next = next;
	if (next != NULL)
	next->prev = prev;

/* fix the index */
	if (parent == NULL) {
	const int	   len = (menu->len + HSPACE);

	if (CurrentBar->tail == menu)
		CurrentBar->tail = prev;
	if (CurrentBar->head == menu)
		CurrentBar->head = next;

	for (next = menu->next; next != NULL; next = next->next)
		next->x -= len;
	} else {
	for (item = parent->tail; item != NULL; item = item->prev) {
		if (item->entry.type == MenuSubMenu
		&& item->entry.submenu.menu == menu) {
		item->entry.submenu.menu = NULL;
		rxvt_menuitem_free(r, menu->parent, item);
		break;
		}
	}
	}

	item = menu->tail;
	while (item != NULL) {
	menuitem_t	 *p = item->prev;

	rxvt_menuitem_free(r, menu, item);
	item = p;
	}

	if (menu->name != NULL)
	free(menu->name);
	free(menu);

	return parent;
}

/* INTPROTO */
menu_t*
rxvt_menu_add(rxvt_t *r, menu_t *parent, char *path)
{
	menu_t		 *menu;
	bar_t		  *CurrentBar = r->h->CurrentBar;

#ifdef DEBUG
	assert(CurrentBar != NULL);
#endif

	if (STRCHR(path, '/') != NULL) {
	char		   *p;

	if (path[0] == '/') {
		/* shouldn't happen */
		path++;
		parent = NULL;
	}
	while ((p = STRCHR(path, '/')) != NULL) {
		p[0] = '\0';
		if (path[0] == '\0')
		return NULL;

		parent = rxvt_menu_add(r, parent, path);
		path = (p + 1);
	}
	}
	if (!STRCMP(path, DOTS))
	return (parent != NULL ? parent->parent : parent);

	if (!STRCMP(path, DOT) || path[0] == '\0')
	return parent;

/* allocate a new menu */
	menu = (menu_t *) rxvt_malloc(sizeof(menu_t));

	menu->width = 0;
	menu->parent = parent;
	menu->len = STRLEN(path);
	/* possible integer overflow? */
	assert (menu->len > 0 && menu->len+1 > 0);
	menu->name = rxvt_malloc((menu->len + 1));
	STRCPY(menu->name, path);

/* initialize head/tail */
	menu->head = menu->tail = NULL;
	menu->prev = menu->next = NULL;

	menu->win = None;
	menu->x = menu->y = menu->w = menu->h = 0;
	menu->item = NULL;

/* add to tail of list */
	if (parent == NULL) {
	menu->prev = CurrentBar->tail;
	if (CurrentBar->tail != NULL)
		CurrentBar->tail->next = menu;
	CurrentBar->tail = menu;
	if (CurrentBar->head == NULL)
		CurrentBar->head = menu;	/* fix head */
	if (menu->prev)
		menu->x = (menu->prev->x + menu->prev->len + HSPACE);
	} else {
	menuitem_t	 *item;

	item = rxvt_menuitem_add(parent, path, "", "");
	if (item == NULL) {
		free(menu);
		return parent;
	}
#ifdef DEBUG
	assert(item->entry.type == MenuLabel);
#endif
	item->entry.type = MenuSubMenu;
	item->entry.submenu.menu = menu;
	}

	return menu;
}

/* INTPROTO */
void
rxvt_drawbox_menubar(rxvt_t *r, int x, int len, int state)
{
	unsigned long	top, bot;

	x = Width2Pixel(x);
	len = Width2Pixel(len + HSPACE);
	if (x >= TWIN_WIDTH(r))
		return;
	else if (x + len >= TWIN_WIDTH(r))
		len = (TWIN_WIDTH(r) - x);

#ifdef MENUBAR_SHADOW_IN
	state = -state;
#endif
	switch (state) {
	case +1:
		top = r->menuBar.topshadow;
		bot = r->menuBar.botshadow;
		break;			/* SHADOW_OUT */
	case -1:
		top = r->menuBar.botshadow;
		bot = r->menuBar.topshadow;
		break;			/* SHADOW_IN */
	default:
		top = bot = r->menuBar.bg;
		break;			/* neutral */
	}

	rxvt_draw_shadow(r->Xdisplay, r->menuBar.win, r->menuBar.gc,
		top, bot, x, 0, len, rxvt_menubar_height(r));
}


/* INTPROTO */
void
rxvt_menubar_draw_triangle(rxvt_t *r, int x, int y, int state)
{
	unsigned long	top, bot;
	int				w;

#ifdef MENU_SHADOW_IN
	state = -state;
#endif
	switch (state) {
	case +1:
		top = r->menuBar.topshadow;
		bot = r->menuBar.botshadow;
		break;			/* SHADOW_OUT */
	case -1:
		top = r->menuBar.botshadow;
		bot = r->menuBar.topshadow;
		break;			/* SHADOW_IN */
	default:
		top = bot = r->menuBar.bg;
		break;			/* neutral */
	}

	w = Height2Pixel(1) - 2 * SHADOW;

	x -= SHADOW + (3 * w / 2);
	y += SHADOW * 3;

	rxvt_draw_triangle (r->Xdisplay, r->h->ActiveMenu->win, r->menuBar.gc, top, bot, x, y, w, 'r');
}


/* INTPROTO */
void
rxvt_drawbox_menuitem(rxvt_t *r, int y, int state)
{
	unsigned long	top, bot;

#ifdef MENU_SHADOW_IN
	state = -state;
#endif
	switch (state) {
	case +1:
		top = r->menuBar.topshadow;
		bot = r->menuBar.botshadow;
		break;			/* SHADOW_OUT */
	case -1:
		top = r->menuBar.botshadow;
		bot = r->menuBar.topshadow;
		break;			/* SHADOW_IN */
	default:
		top = bot = r->menuBar.bg;
		break;			/* neutral */
	}

	rxvt_draw_shadow (r->Xdisplay, r->h->ActiveMenu->win, r->menuBar.gc,
		top, bot,
		SHADOW + 0, SHADOW + y,
		r->h->ActiveMenu->w - 2 * (SHADOW),
		HEIGHT_TEXT + 2 * SHADOW);
	XFlush(r->Xdisplay);
}

#ifdef DEBUG_MENU_LAYOUT
/* INTPROTO */
void
rxvt_print_menu_ancestors(menu_t *menu)
{
	if (menu == NULL) {
	fprintf(stderr, "Top Level menu\n");
	return;
	}
	fprintf(stderr, "menu %s ", menu->name);
	if (menu->parent != NULL) {
	menuitem_t	 *item;

	for (item = menu->parent->head; item != NULL; item = item->next) {
		if (item->entry.type == MenuSubMenu
		&& item->entry.submenu.menu == menu) {
		break;
		}
	}
	if (item == NULL) {
		fprintf(stderr, "is an orphan!\n");
		return;
	}
	}
	fprintf(stderr, "\n");
	rxvt_print_menu_ancestors(menu->parent);
}

/* INTPROTO */
void
rxvt_print_menu_descendants(menu_t *menu)
{
	menuitem_t	 *item;
	menu_t		 *parent;
	int			 i, level = 0;

	parent = menu;
	do {
	level++;
	parent = parent->parent;
	}
	while (parent != NULL);

	for (i = 0; i < level; i++)
	fprintf(stderr, ">");
	fprintf(stderr, "%s\n", menu->name);

	for (item = menu->head; item != NULL; item = item->next) {
	if (item->entry.type == MenuSubMenu) {
		if (item->entry.submenu.menu == NULL)
		fprintf(stderr, "> %s == NULL\n", item->name);
		else
		rxvt_print_menu_descendants(item->entry.submenu.menu);
	} else {
		for (i = 0; i < level; i++)
		fprintf(stderr, "+");
		if (item->entry.type == MenuLabel)
		fprintf(stderr, "label: ");
		fprintf(stderr, "%s\n", item->name);
	}
	}

	for (i = 0; i < level; i++)
	fprintf(stderr, "<");
	fprintf(stderr, "\n");
}
#endif

/* pop up/down the current menu and redraw the menuBar button */
/* INTPROTO */
void
rxvt_menu_show(rxvt_t *r)
{
	int			 x, y, xright;
	menu_t		 *ActiveMenu = r->h->ActiveMenu;
	menuitem_t	 *item;

	if (ActiveMenu == NULL)
		return;

	x = ActiveMenu->x;
	if (ActiveMenu->parent == NULL) {
		register int	h;

		rxvt_drawbox_menubar(r, x, ActiveMenu->len, -1);
		x = Width2Pixel(x);

		ActiveMenu->y = 1;
		ActiveMenu->w = Menu_PixelWidth(ActiveMenu);

		if ((x + ActiveMenu->w) >= TWIN_WIDTH(r))
			x = (TWIN_WIDTH(r) - ActiveMenu->w);

		/* find the height */
		for (h = 0, item = ActiveMenu->head; item != NULL; item = item->next)
			h += isSeparator(item->name) ? HEIGHT_SEPARATOR
					 : HEIGHT_TEXT + 2 * SHADOW;
		ActiveMenu->h = h + 2 * SHADOW;
	}

	if (ActiveMenu->win == None) {
		ActiveMenu->win = XCreateSimpleWindow(r->Xdisplay,
							r->TermWin.parent,
							x, ActiveMenu->y,
							ActiveMenu->w, ActiveMenu->h,
							0,
							r->PixColors[Color_fg],
							r->PixColors[Color_scroll]);
		XMapWindow(r->Xdisplay, ActiveMenu->win);
	}
	rxvt_draw_shadow (r->Xdisplay, ActiveMenu->win, r->menuBar.gc,
		r->menuBar.topshadow, r->menuBar.botshadow,
		0, 0, ActiveMenu->w, ActiveMenu->h);

	/* determine the correct right-alignment */
	for (xright = 0, item = ActiveMenu->head; item != NULL; item = item->next)
		if (item->len2 > xright)
			xright = item->len2;

	for (y = 0, item = ActiveMenu->head; item != NULL; item = item->next) {
		const int	   xoff = (SHADOW + Width2Pixel(HSPACE) / 2);
		register int	h;

		if (isSeparator(item->name)) {
			rxvt_draw_shadow (r->Xdisplay, ActiveMenu->win,
				r->menuBar.gc,
				r->menuBar.topshadow, r->menuBar.botshadow,
				SHADOW, y + SHADOW + 1,
				ActiveMenu->w - 2 * SHADOW, 0);
			h = HEIGHT_SEPARATOR;
		}
		else {
			char		   *name = item->name;
			int			 len = item->len;

			if (item->entry.type == MenuSubMenu) {
				int			 x1, y1;
				menuitem_t	 *it;
				menu_t		 *menu = item->entry.submenu.menu;

				rxvt_menubar_draw_triangle(r, ActiveMenu->w, y, +1);

				name = menu->name;
				len = menu->len;

				y1 = ActiveMenu->y + y;

				menu->w = Menu_PixelWidth(menu);

				/* place sub-menu at midpoint of parent menu */
				x1 = ActiveMenu->w / 2;
				if (x1 > menu->w)	/* right-flush menu if too small */
					x1 += (x1 - menu->w);
				x1 += x;

				/* find the height of this submenu */
				for (h = 0, it = menu->head; it != NULL; it = it->next)
					h += isSeparator(it->name) ? HEIGHT_SEPARATOR
								   : HEIGHT_TEXT + 2 * SHADOW;
				menu->h = h + 2 * SHADOW;

				/* ensure menu is in window limits */
				if ((x1 + menu->w) >= TWIN_WIDTH(r))
					x1 = (TWIN_WIDTH(r) - menu->w);

				if ((y1 + menu->h) >= TWIN_HEIGHT(r))
					y1 = (TWIN_HEIGHT(r) - menu->h);

				menu->x = (x1 < 0 ? 0 : x1);
				menu->y = (y1 < 0 ? 0 : y1);
			}
			else if (item->name2 && !STRCMP(name, item->name2))
				name = NULL;


			if (item->entry.type == MenuLabel)	{
				CHOOSE_GC_FG (r, r->menuBar.botshadow);
			}
			else	{
				CHOOSE_GC_FG (r, r->menuBar.fg);
			}


			if (len && name) {
#ifdef USE_XIM
				if (r->TermWin.fontset)	{
					XmbDrawString(r->Xdisplay,
						  ActiveMenu->win, r->TermWin.fontset,
						  r->menuBar.gc, xoff,
						  2 * SHADOW + y + r->TermWin.font->ascent + 1,
						  name, len);
				}
				else
#endif
				XDrawString(r->Xdisplay, ActiveMenu->win,
					r->menuBar.gc, xoff,
					2 * SHADOW + y + r->TermWin.font->ascent + 1,
					name, len);
			}

			len = item->len2;
			name = item->name2;
			if (len && name) {
#ifdef USE_XIM
				if (r->TermWin.fontset)
					XmbDrawString(r->Xdisplay,
						ActiveMenu->win, r->TermWin.fontset,
						r->menuBar.gc,
						ActiveMenu->w - (xoff + Width2Pixel(xright)),
						2 * SHADOW + y + r->TermWin.font->ascent + 1,
						name, len);
				else
#endif
				XDrawString(r->Xdisplay, ActiveMenu->win,
					r->menuBar.gc,
					ActiveMenu->w - (xoff + Width2Pixel(xright)),
					2 * SHADOW + y + r->TermWin.font->ascent + 1,
					name, len);
			}
			h = HEIGHT_TEXT + 2 * SHADOW;
		}
		y += h;
	}
}


/* INTPROTO */
void
rxvt_menu_display(rxvt_t *r, void (*update)(rxvt_t *))
{
	menu_t		 *ActiveMenu = r->h->ActiveMenu;

	if (ActiveMenu == NULL)
	return;
	if (ActiveMenu->win != None)
		XDestroyWindow(r->Xdisplay, ActiveMenu->win);
	ActiveMenu->win = None;
	ActiveMenu->item = NULL;

	if (ActiveMenu->parent == NULL)
		rxvt_drawbox_menubar(r, ActiveMenu->x, ActiveMenu->len, +1);
	r->h->ActiveMenu = ActiveMenu->parent;
	update(r);
}

/* INTPROTO */
void
rxvt_menu_hide_all(rxvt_t *r)
{
	rxvt_menu_display(r, rxvt_menu_hide_all);
}

/* INTPROTO */
void
rxvt_menu_hide(rxvt_t *r)
{
	rxvt_menu_display(r, rxvt_menu_show);
}

/* INTPROTO */
void
rxvt_menu_clear(rxvt_t *r, menu_t *menu)
{
	if (menu != NULL) {
	menuitem_t	 *item = menu->tail;

	while (item != NULL) {
		rxvt_menuitem_free(r, menu, item);
		/* it didn't get freed ... why? */
		if (item == menu->tail)
		return;
		item = menu->tail;
	}
	menu->width = 0;
	}
}

/* INTPROTO */
void
rxvt_menubar_clear(rxvt_t *r)
{
	bar_t		  *CurrentBar = r->h->CurrentBar;

	if (CurrentBar != NULL) {
	menu_t		 *menu = CurrentBar->tail;

	while (menu != NULL) {
		menu_t		 *prev = menu->prev;

		rxvt_menu_delete(r, menu);
		menu = prev;
	}
	CurrentBar->head = CurrentBar->tail = NULL;

	if (CurrentBar->title) {
		free(CurrentBar->title);
		CurrentBar->title = NULL;
	}
	rxvt_menuarrow_free(r, 0);	/* remove all arrow functions */
	}
	r->h->ActiveMenu = NULL;
}

#if (MENUBAR_MAX > 1)
/* find if menu already exists */
/* INTPROTO */
bar_t*
rxvt_menubar_find(rxvt_t *r, const char *name)
{
	bar_t		  *bar = r->h->CurrentBar;

#ifdef DEBUG_MENUBAR_STACKING
	fprintf(stderr, "looking for [menu:%s] ...", name ? name : "(nil)");
#endif
	if (bar == NULL || name == NULL)
	return NULL;

	if (STRLEN(name) && STRCMP(name, "*")) {
	do {
		if (!STRCMP(bar->name, name)) {
#ifdef DEBUG_MENUBAR_STACKING
		fprintf(stderr, " found!\n");
#endif
		return bar;
		}
		bar = bar->next;
	}
	while (bar != r->h->CurrentBar);
	bar = NULL;
	}
#ifdef DEBUG_MENUBAR_STACKING
	fprintf(stderr, "%s found!\n", (bar ? "" : " NOT"));
#endif

	return bar;
}

/* INTPROTO */
int
rxvt_menubar_push(rxvt_t *r, const char *name)
{
	int			 ret = 1;
	bar_t		  *bar;

	if (r->h->CurrentBar == NULL) {
	/* allocate first one */
	bar = (bar_t *) rxvt_malloc(sizeof(bar_t));

	MEMSET(bar, 0, sizeof(bar_t));
	/* circular linked-list */
	bar->next = bar->prev = bar;
	bar->head = bar->tail = NULL;
	bar->title = NULL;
	r->h->CurrentBar = bar;
	r->h->Nbars++;

	rxvt_menubar_clear(r);
	} else {
	/* find if menu already exists */
	bar = rxvt_menubar_find(r, name);
	if (bar != NULL) {
		/* found it, use it */
		r->h->CurrentBar = bar;
	} else {
		/* create if needed, or reuse the existing empty menubar */
		if (r->h->CurrentBar->head != NULL) {
		/* need to malloc another one */
		if (r->h->Nbars < MENUBAR_MAX)
			bar = (bar_t *) rxvt_malloc(sizeof(bar_t));
		else
			bar = NULL;

		/* malloc failed or too many menubars, reuse another */
		if (bar == NULL) {
			bar = r->h->CurrentBar->next;
			ret = -1;
		} else {
			bar->head = bar->tail = NULL;
			bar->title = NULL;

			bar->next = r->h->CurrentBar->next;
			r->h->CurrentBar->next = bar;
			bar->prev = r->h->CurrentBar;
			bar->next->prev = bar;

			r->h->Nbars++;
		}
		r->h->CurrentBar = bar;

		}
		rxvt_menubar_clear(r);
	}
	}

/* give menubar this name */
	STRNCPY(r->h->CurrentBar->name, name, MAXNAME);
	r->h->CurrentBar->name[MAXNAME - 1] = '\0';

	return ret;
}

/* switch to a menu called NAME and remove it */
/* INTPROTO */
void
rxvt_menubar_remove(rxvt_t *r, const char *name)
{
	bar_t		  *bar;

	if ((bar = rxvt_menubar_find(r, name)) == NULL)
	return;
	r->h->CurrentBar = bar;

	do {
	rxvt_menubar_clear(r);
	/*
	 * pop a menubar, clean it up first
	 */
	if (r->h->CurrentBar != NULL) {
		bar_t		  *prev = r->h->CurrentBar->prev;
		bar_t		  *next = r->h->CurrentBar->next;

		if (prev == next && prev == r->h->CurrentBar) {	/* only 1 left */
		prev = NULL;
		r->h->Nbars = 0;	/* safety */
		} else {
		next->prev = prev;
		prev->next = next;
		r->h->Nbars--;
		}

		free(r->h->CurrentBar);
		r->h->CurrentBar = prev;
	}
	}
	while (r->h->CurrentBar && !STRCMP(name, "*"));
}

/* INTPROTO */
void
rxvt_action_decode(FILE *fp, action_t *act)
{
	unsigned char  *str;
	short		   len;

	if (act == NULL || (len = act->len) == 0 || (str = act->str) == NULL)
	return;

	if (act->type == MenuTerminalAction) {
	fprintf(fp, "^@");
	/* can strip trailing ^G from XTerm sequence */
	if (str[0] == C0_ESC && str[1] == ']' && str[len - 1] == C0_BEL)
		len--;
	} else if (str[0] == C0_ESC) {
	switch (str[1]) {
	case '[':
	case ']':
		break;

	case 'x':
		/* can strip trailing '\r' from M-x sequence */
		if (str[len - 1] == '\r')
		len--;
		/* FALLTHROUGH */

	default:
		fprintf(fp, "M-");	/* meta prefix */
		str++;
		len--;
		break;
	}
	}
/*
 * control character form is preferred, since backslash-escaping
 * can be really ugly looking when the backslashes themselves also
 * have to be escaped to avoid Shell (or whatever scripting
 * language) interpretation
 */
	while (len > 0) {
	unsigned char   ch = *str++;

	switch (ch) {
	case C0_ESC:
		fprintf(fp, "\\E");
		break;		/* escape */
	case '\r':
		fprintf(fp, "\\r");
		break;		/* carriage-return */
	case '\\':
		fprintf(fp, "\\\\");
		break;		/* backslash */
	case '^':
		fprintf(fp, "\\^");
		break;		/* caret */
	case 127:
		fprintf(fp, "^?");
	default:
		if (ch <= 31)
		fprintf(fp, "^%c", ('@' + ch));
		else if (ch > 127)
		fprintf(fp, "\\%o", ch);
		else
		fprintf(fp, "%c", ch);
		break;
	}
	len--;
	}
	fprintf(fp, "\n");
}

/* INTPROTO */
void
rxvt_menu_dump(FILE *fp, menu_t *menu)
{
	menuitem_t	 *item;

/* create a new menu and clear it */
	fprintf(fp, (menu->parent ? "./%s/*\n" : "/%s/*\n"), menu->name);

	for (item = menu->head; item != NULL; item = item->next) {
	switch (item->entry.type) {
	case MenuSubMenu:
		if (item->entry.submenu.menu == NULL)
		fprintf(fp, "> %s == NULL\n", item->name);
		else
		rxvt_menu_dump(fp, item->entry.submenu.menu);
		break;

	case MenuLabel:
		fprintf(fp, "{%s}\n", (STRLEN(item->name) ? item->name : "-"));
		break;

	case MenuTerminalAction:
	case MenuAction:
		fprintf(fp, "{%s}", item->name);
		if (item->name2 != NULL && STRLEN(item->name2))
		fprintf(fp, "{%s}", item->name2);
		fprintf(fp, "\t");
		rxvt_action_decode(fp, &(item->entry.action));
		break;
	}
	}

	fprintf(fp, (menu->parent ? "../\n" : "/\n\n"));
}

/* INTPROTO */
void
rxvt_menubar_dump(rxvt_t *r, FILE *fp)
{
	bar_t		  *bar = r->h->CurrentBar;
	time_t		  t;

	if (bar == NULL || fp == NULL)
		return;
	time(&t);

	fprintf(fp,
		"# " APL_SUBCLASS " (%s)  Pid: %u\n# Date: %s\n\n",
		r->h->rs[Rs_name], (unsigned int)getpid(), ctime(&t));

/* dump in reverse order */
	bar = r->h->CurrentBar->prev;
	do {
	menu_t		 *menu;
	int			 i;

	fprintf(fp, "[menu:%s]\n", bar->name);

	if (bar->title != NULL)
		fprintf(fp, "[title:%s]\n", bar->title);

	for (i = 0; i < NARROWS; i++) {
		switch (bar->arrows[i].type) {
		case MenuTerminalAction:
		case MenuAction:
		fprintf(fp, "<%c>", Arrows[i].name);
		rxvt_action_decode(fp, &(bar->arrows[i]));
		break;
		}
	}
	fprintf(fp, "\n");

	for (menu = bar->head; menu != NULL; menu = menu->next)
		rxvt_menu_dump(fp, menu);

	fprintf(fp, "\n[done:%s]\n\n", bar->name);
	bar = bar->prev;
	}
	while (bar != r->h->CurrentBar->prev);
}
#endif				/* (MENUBAR_MAX > 1) */



/* INTPROTO */
void
rxvt_draw_arrows(rxvt_t *r, int name, int state)
{
	unsigned long	top, bot;

	int			 i;

#ifdef MENU_SHADOW_IN
	state = -state;
#endif
	switch (state) {
	case +1:
		top = r->menuBar.topshadow;
		bot = r->menuBar.botshadow;
		break;			/* SHADOW_OUT */
	case -1:
		top = r->menuBar.botshadow;
		bot = r->menuBar.topshadow;
		break;			/* SHADOW_IN */
	default:
		top = bot = r->menuBar.bg;
		break;			/* neutral */
	}

	if (!r->h->Arrows_x)
		return;

	for (i = 0; i < NARROWS; i++) {
		const int	   w = Width2Pixel(1);
		const int	   y = (rxvt_menubar_height(r) - w) / 2;
		int			 x = r->h->Arrows_x + (5 * Width2Pixel(i)) / 4;

		if (!name || name == Arrows[i].name)
			rxvt_draw_triangle (r->Xdisplay, r->menuBar.win,
				r->menuBar.gc, top, bot,
				x, y, w, Arrows[i].name);
	}
	XFlush(r->Xdisplay);
}


/* INTPROTO */
int
rxvt_menu_select(rxvt_t *r, XButtonEvent *ev)
{
	menuitem_t	 *thisitem, *item = NULL;
	int			 this_y, y;
	menu_t		 *ActiveMenu = r->h->ActiveMenu;

	Window		  unused_root, unused_child;
	int			 unused_root_x, unused_root_y;
	unsigned int	unused_mask;

	if (ActiveMenu == NULL)
	return 0;

	XQueryPointer(r->Xdisplay, ActiveMenu->win,
		  &unused_root, &unused_child,
		  &unused_root_x, &unused_root_y,
		  &(ev->x), &(ev->y), &unused_mask);

	if (ActiveMenu->parent != NULL && (ev->x < 0 || ev->y < 0)) {
	rxvt_menu_hide(r);
	return 1;
	}
/* determine the menu item corresponding to the Y index */
	y = SHADOW;
	if (ev->x >= 0 && ev->x <= (ActiveMenu->w - SHADOW)) {
	for (item = ActiveMenu->head; item != NULL; item = item->next) {
		int			 h = HEIGHT_TEXT + 2 * SHADOW;

		if (isSeparator(item->name))
		h = HEIGHT_SEPARATOR;
		else if (ev->y >= y && ev->y < (y + h))
		break;
		y += h;
	}
	}
	if (item == NULL && ev->type == ButtonRelease) {
	rxvt_menu_hide_all(r);
	return 0;
	}
	thisitem = item;
	this_y = y - SHADOW;

/* erase the last item */
	if (ActiveMenu->item != NULL) {
	if (ActiveMenu->item != thisitem) {
		for (y = 0, item = ActiveMenu->head; item != NULL;
		 item = item->next) {
		int			 h;

		if (isSeparator(item->name))
			h = HEIGHT_SEPARATOR;
		else if (item == ActiveMenu->item) {
			/* erase old menuitem */
			rxvt_drawbox_menuitem(r, y, 0);	/* No Shadow */
			if (item->entry.type == MenuSubMenu)
			rxvt_menubar_draw_triangle(r, ActiveMenu->w, y, +1);
			break;
		} else
			h = HEIGHT_TEXT + 2 * SHADOW;
		y += h;
		}
	} else {
		switch (ev->type) {
		case ButtonRelease:
		switch (item->entry.type) {
		case MenuLabel:
		case MenuSubMenu:
			rxvt_menu_hide_all(r);
			break;

		case MenuAction:
		case MenuTerminalAction:
			rxvt_drawbox_menuitem(r, this_y, -1);
			{
#ifdef HAVE_NANOSLEEP
			struct timespec rqt;

			rqt.tv_sec = 0;
			rqt.tv_nsec = MENU_DELAY_USEC * 1000;
			nanosleep(&rqt, NULL);
#else
			/* use select for timing */
			struct timeval  tv;

			tv.tv_sec = 0;
			tv.tv_usec = MENU_DELAY_USEC;
			select(0, NULL, NULL, NULL, &tv);
#endif
			}
			/* remove menu before sending keys to the application */
			rxvt_menu_hide_all(r);
#ifndef DEBUG_MENU
			rxvt_action_dispatch(r, &(item->entry.action));
#else				/* DEBUG_MENU */
			fprintf(stderr, "%s: %s\n", item->name,
				item->entry.action.str);
#endif				/* DEBUG_MENU */
			break;
		}
		break;

		default:
		if (item->entry.type == MenuSubMenu)
			goto DoMenu;
		break;
		}
		return 0;
	}
	}
  DoMenu:
	ActiveMenu->item = thisitem;
	y = this_y;
	if (thisitem != NULL) {
	item = ActiveMenu->item;
	if (item->entry.type != MenuLabel)
		rxvt_drawbox_menuitem(r, y, +1);
	if (item->entry.type == MenuSubMenu) {
		int			 x;

		rxvt_menubar_draw_triangle(r, ActiveMenu->w, y, -1);

		x = ev->x + (ActiveMenu->parent
			 ? ActiveMenu->x
			 : Width2Pixel(ActiveMenu->x));

		if (x >= item->entry.submenu.menu->x) {
		r->h->ActiveMenu = item->entry.submenu.menu;
		rxvt_menu_show(r);
		return 1;
		}
	}
	}
	return 0;
}

/* INTPROTO */
void
rxvt_menubar_select(rxvt_t *r, XButtonEvent *ev)
{
	menu_t		 *menu = NULL;

/* determine the pulldown menu corresponding to the X index */
	if (ev->y >= 0 &&
		ev->y <= (rxvt_menubar_height(r)-MENUBAR_MARGIN) &&
		r->h->CurrentBar != NULL) {
	for (menu = r->h->CurrentBar->head; menu != NULL; menu = menu->next) {
		int			 x = Width2Pixel(menu->x);
		int			 w = Width2Pixel(menu->len + HSPACE);

		if ((ev->x >= x && ev->x < x + w))
		break;
	}
	}
	switch (ev->type) {
	case ButtonRelease:
	rxvt_menu_hide_all(r);
	break;

	case ButtonPress:
	if (menu == NULL && r->h->Arrows_x && ev->x >= r->h->Arrows_x) {
		int			 i;

		for (i = 0; i < NARROWS; i++) {
		if (ev->x >= (r->h->Arrows_x + (Width2Pixel(4 * i + i)) / 4)
			&& ev->x < (r->h->Arrows_x
					+ (Width2Pixel(4 * i + i + 4)) / 4)) {
			rxvt_draw_arrows(r, Arrows[i].name, -1);
			{
#ifdef HAVE_NANOSLEEP
			struct timespec rqt;

			rqt.tv_sec = 0;
			rqt.tv_nsec = MENU_DELAY_USEC * 1000;
			nanosleep(&rqt, NULL);
#else
			/* use select for timing */
			struct timeval  tv;

			tv.tv_sec = 0;
			tv.tv_usec = MENU_DELAY_USEC;
			select(0, NULL, NULL, NULL, &tv);
#endif
			}
			rxvt_draw_arrows(r, Arrows[i].name, +1);
#ifdef DEBUG_MENUARROWS
			fprintf(stderr, "'%c': ", Arrows[i].name);

			if (r->h->CurrentBar == NULL
			|| (r->h->CurrentBar->arrows[i].type != MenuAction
				&& r->h->CurrentBar->arrows[i].type !=
				MenuTerminalAction)) {
			if (Arrows[i].str != NULL && Arrows[i].str[0])
				fprintf(stderr, "(default) \\033%s\n",
					&(Arrows[i].str[2]));
			} else {
			fprintf(stderr, "%s\n",
				r->h->CurrentBar->arrows[i].str);
			}
#else				/* DEBUG_MENUARROWS */
			if (r->h->CurrentBar == NULL
			|| rxvt_action_dispatch(r,
						&(r->h->CurrentBar->arrows[i]))
			   ) {
			if (Arrows[i].str != NULL && Arrows[i].str[0] != 0)
				rxvt_tt_write(r, ATAB(r), (Arrows[i].str + 1),
					  Arrows[i].str[0]);
			}
#endif				/* DEBUG_MENUARROWS */
			return;
		}
		}
	}
	/* FALLTHROUGH */

	default:
	/*
	 * press menubar or move to a new entry
	 */
	if (menu != NULL && menu != r->h->ActiveMenu) {
		rxvt_menu_hide_all(r);	/* pop down old menu */
		r->h->ActiveMenu = menu;
		rxvt_menu_show(r);	/* pop up new menu */
	}
	break;
	}
}



/* EXTPROTO */
void
rxvt_menubar_create (rxvt_t* r)
{
# if (MENUBAR_MAX > 1)
	XGCValues		gcvalue;
	unsigned long	gcmask;


	DBG_MSG(1,(stderr,"Creating menubar\n"));

	r->menuBar.state = 0;


	r->menuBar.win = XCreateSimpleWindow(r->Xdisplay,
						r->TermWin.parent,
						0, 0,
						TWIN_WIDTH(r), rxvt_menubar_rheight (r),
						0, r->PixColors[Color_fg],
						r->PixColors[Color_scroll]);
	assert (None != r->menuBar.win);

#  ifdef DEBUG_X
	rxvt_set_win_title (r, r->menuBar.win, "menubar");
#  endif

	XDefineCursor(r->Xdisplay, r->menuBar.win, r->h->bar_pointer);
	XSelectInput(r->Xdisplay, r->menuBar.win,
			  (ExposureMask | ButtonPressMask | ButtonReleaseMask
			  | Button1MotionMask));

#  ifdef BACKGROUND_IMAGE
	r->menuBar.pixmap = None;	/* Initialize it to None */
#   ifdef TRANSPARENT
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_menubar)))
#   endif
	if (r->h->rs[Rs_menubarPixmap])	{
		long	w = 0, h = 0;
		r->menuBar.pixmap = rxvt_load_pixmap (r,
								r->h->rs[Rs_menubarPixmap], &w, &h);
		if (None != r->menuBar.pixmap)
			XSetWindowBackgroundPixmap (r->Xdisplay, r->menuBar.win,
				r->menuBar.pixmap);
	}
#  endif

#  ifdef TRANSPARENT
	if ((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_menubar))	{
		XSetWindowBackgroundPixmap (r->Xdisplay, r->menuBar.win,
			ParentRelative);
	}
#  endif

	/* Initialize the colors */
	r->menuBar.fg = r->PixColors[XDEPTH <= 2 ? Color_fg : Color_Black];
	r->menuBar.bg = r->PixColors[XDEPTH <= 2 ? Color_fg : Color_scroll];
	r->menuBar.topshadow = r->PixColors[Color_topShadow];
	r->menuBar.botshadow = r->PixColors[Color_bottomShadow];


#  ifdef XFT_SUPPORT
	if (!(r->Options & Opt_xft))
#  endif	/* XFT_SUPPORT */
	gcvalue.font = r->TermWin.font->fid;
	gcvalue.foreground = r->menuBar.fg;
#  ifdef TRANSPARENT
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_menubar)))
#  endif
#  ifdef BACKGROUND_IMAGE
	if (None == r->menuBar.pixmap)
#  endif
	gcvalue.background = r->menuBar.bg;
	gcmask = GCForeground;
#  ifdef XFT_SUPPORT
	if (!(r->Options & Opt_xft))
#  endif	/* XFT_SUPPORT */
	gcmask |= GCFont;

#  ifdef TRANSPARENT
	if (!((r->Options & Opt_transparent) &&
		(r->Options & Opt_transparent_menubar)))
#  endif
#  ifdef BACKGROUND_IMAGE
	if (None == r->menuBar.pixmap)
#  endif
	gcmask |= GCBackground;
	r->menuBar.gc = XCreateGC (r->Xdisplay, r->menuBar.win,
						gcmask, &gcvalue);
	assert (None != r->menuBar.gc);
#  ifdef XFT_SUPPORT
	if (!(r->Options & Opt_xft))
#  endif	/* XFT_SUPPORT */
	XSetFont(r->Xdisplay, r->menuBar.gc, r->TermWin.font->fid);

# endif /* MENUBAR_MAX > 1 */
}


/* EXTPROTO */
void
rxvt_menubar_clean_exit (rxvt_t* r)
{
# if (MENUBAR_MAX > 1)
	r->menuBar.win = None;	/* Destroyed by XDestroySubwindows */

	if (None != r->menuBar.gc)	{
		XFreeGC (r->Xdisplay, r->menuBar.gc);
		r->menuBar.gc = None;
	}
#  ifdef BACKGROUND_IMAGE
	if (None != r->menuBar.pixmap)	{
		XFreePixmap (r->Xdisplay, r->menuBar.pixmap);
		r->menuBar.pixmap = None;
	}
#  endif
# endif
}


/*
** Is the menubar visible
*/
/* EXTPROTO */
int
rxvt_menubar_visible (rxvt_t* r)
{
	return (None != r->menuBar.win && r->menuBar.state);
}


/*
** Hide the menubar
*/
/* EXTPROTO */
int
rxvt_menubar_hide (rxvt_t* r)
{
	int		changed = 0;

	assert (None != r->menuBar.win);
	changed = r->menuBar.state;
	XUnmapWindow(r->Xdisplay, r->menuBar.win);
	r->menuBar.state = 0;

	return (changed);
}


/*
** Show the menubar
*/
/* EXTPROTO */
int
rxvt_menubar_show (rxvt_t* r)
{
	int		changed = 0;

	assert (None != r->menuBar.win);
	changed = !r->menuBar.state;
	XMapWindow(r->Xdisplay, r->menuBar.win);
	r->menuBar.state = 1;

	return (changed);
}


/*
** Menubar expose handler
*/
/* EXTPROTO */
void
rxvt_menubar_expose(rxvt_t *r)
{
	menu_t		 *menu;
	int			 x;

	if (!r->menuBar.state || None == r->menuBar.win)
		return;

	XClearWindow(r->Xdisplay, r->menuBar.win);

	rxvt_menu_hide_all(r);

	x = 0;
	if (r->h->CurrentBar != NULL) {
	for (menu = r->h->CurrentBar->head; menu != NULL; menu = menu->next) {
		int			 len = menu->len;

		x = (menu->x + menu->len + HSPACE);

# ifdef DEBUG_MENU_LAYOUT
		rxvt_print_menu_descendants(menu);
# endif

		if (x >= r->TermWin.ncol)
		len = (r->TermWin.ncol - (menu->x + HSPACE));

		rxvt_drawbox_menubar(r, menu->x, len, +1);

		CHOOSE_GC_FG (r, r->menuBar.fg);
# ifdef USE_XIM
		if (r->TermWin.fontset)
			XmbDrawString(r->Xdisplay,
				  r->menuBar.win, r->TermWin.fontset, r->menuBar.gc,
				  (Width2Pixel(menu->x) + Width2Pixel(HSPACE) / 2),
				  rxvt_menubar_height(r) - SHADOW - MENUBAR_MARGIN,
				  menu->name, len);
		else
# endif	/* USE_XIM */
		XDrawString(r->Xdisplay, r->menuBar.win, r->menuBar.gc,
				(Width2Pixel(menu->x) + Width2Pixel(HSPACE) / 2),
				rxvt_menubar_height(r) - SHADOW - MENUBAR_MARGIN,
				menu->name, len);

		if (x >= r->TermWin.ncol)
		break;
	}
	}
	rxvt_drawbox_menubar(r, x, r->TermWin.ncol, (r->h->CurrentBar ? +1 : -1));

/* add the menuBar title, if it exists and there's plenty of room */
	r->h->Arrows_x = 0;
	if (x < r->TermWin.ncol) {
	const char	 *str;
	int			 ncol;
	unsigned int	len;
	char			title[256];

	ncol = (int)r->TermWin.ncol;
	if (x < (ncol - (NARROWS + 1))) {
		ncol -= (NARROWS + 1);
		r->h->Arrows_x = Width2Pixel(ncol);
	}
	rxvt_draw_arrows(r, 0, +1);

	str = (r->h->CurrentBar
		   && r->h->CurrentBar->title) ? r->h->CurrentBar->title : "%n-%v";
	for (len = 0; str[0] && len < sizeof(title) - 1; str++) {
		const char	 *s = NULL;

		switch (str[0]) {
		case '%':
		str++;
		switch (str[0]) {
		case 'n':
			s = r->h->rs[Rs_name];
			break;	/* resource name */
		case 'v':
			s = VERSION;
			break;	/* version number */
		case '%':
			s = "%";
			break;	/* literal '%' */
		}
		if (s != NULL)
			while (*s && len < sizeof(title) - 1)
			title[len++] = *s++;
		break;

		default:
		title[len++] = str[0];
		break;
		}
	}
	title[len] = '\0';

	ncol -= (x + len + HSPACE);
	if (len > 0 && ncol >= 0) {
		CHOOSE_GC_FG (r, r->menuBar.fg);
# ifdef USE_XIM
		if (r->TermWin.fontset)
		XmbDrawString(r->Xdisplay,
				  r->menuBar.win, r->TermWin.fontset, r->menuBar.gc,
				  Width2Pixel(x) + Width2Pixel(ncol + HSPACE) / 2,
				  rxvt_menubar_height(r) - SHADOW - MENUBAR_MARGIN,
				  title, len);
		else
# endif	/* USE_XIM */
		XDrawString(r->Xdisplay, r->menuBar.win, r->menuBar.gc,
				Width2Pixel(x) + Width2Pixel(ncol + HSPACE) / 2,
				rxvt_menubar_height(r) - SHADOW - MENUBAR_MARGIN,
				title, len);
	}
	}
}


/*
** user interface for building/deleting and otherwise managing menus
*/
/* EXTPROTO */
void
rxvt_menubar_dispatcher(rxvt_t *r, char *str)
{
	int			 n, cmd;
	char		   *path, *name, *name2;

	if (rxvt_menubar_visible(r) && r->h->ActiveMenu != NULL)
		rxvt_menubar_expose(r);
	else
		r->h->ActiveMenu = NULL;

	cmd = *str;
	switch (cmd) {
	case '.':
	case '/':			/* absolute & relative path */
	case MENUITEM_BEG:		/* menuitem */
		/* add `+' prefix for these cases */
		cmd = '+';
		break;

	case '+':
	case '-':
		str++;			/* skip cmd character */
		break;

	case '<':
# if (MENUBAR_MAX > 1)
		if (r->h->CurrentBar == NULL)
			break;
# endif				/* (MENUBAR_MAX > 1) */
		if (str[1] && str[2] == '>')	/* arrow commands */
			rxvt_menuarrow_add(r, str);
		break;

	case '[':			/* extended command */
		while (str[0] == '[') {
			char		   *next = (++str);	/* skip leading '[' */

			if (str[0] == ':') {	/* [:command:] */
				do {
					next++;
					if ((next = STRCHR(next, ':')) == NULL)
						return;	/* parse error */
				}
				while (next[1] != ']');
				/* remove and skip ':]' */
				*next = '\0';
				next += 2;
			}
			else {
				if ((next = STRCHR(next, ']')) == NULL)
					return;	/* parse error */
				/* remove and skip ']' */
				*next = '\0';
				next++;
			}

			if (str[0] == ':') {
				int			 saved;

				/* try and dispatch it, regardless of read/write
				** status */
				saved = r->h->menu_readonly;
				r->h->menu_readonly = 0;
				rxvt_menubar_dispatcher(r, str + 1);
				r->h->menu_readonly = saved;
			}
			/* these ones don't require menu stacking */
			else if (!STRCMP(str, "clear")) {
				rxvt_menubar_clear(r);
			}
			else if (!STRCMP(str, "done") ||
				rxvt_str_match(str, "done:")) {
				r->h->menu_readonly = 1;
			}
			else if (!STRCMP(str, "show")) {
				if (rxvt_menubar_show(r))
					rxvt_resize_on_subwin (r, SHOW_MENUBAR);
				r->h->menu_readonly = 1;
			}
			else if (!STRCMP(str, "hide")) {
				if (rxvt_menubar_hide(r))
					rxvt_resize_on_subwin (r, HIDE_MENUBAR);
				r->h->menu_readonly = 1;
			}
			else if ((n = rxvt_str_match(str, "read:")) != 0) {
				/* read in a menu from a file */
				str += n;
				rxvt_menubar_load_file(r, str);
			}
			else if ((n = rxvt_str_match(str, "title:")) != 0) {
				str += n;
				if (r->h->CurrentBar != NULL && !r->h->menu_readonly) {
					if (*str) {
						name = rxvt_realloc(r->h->CurrentBar->title,
							STRLEN(str) + 1);
					if (name != NULL) {
						STRCPY(name, str);
						r->h->CurrentBar->title = name;
					}
					rxvt_menubar_expose(r);
				}
				else {
					free(r->h->CurrentBar->title);
					r->h->CurrentBar->title = NULL;
				}
			}
		}
		else if ((n = rxvt_str_match(str, "pixmap:")) != 0) {
			str += n;
			rxvt_xterm_seq(r, ATAB(r), XTerm_Pixmap, str, CHAR_ST);
		}
# if (MENUBAR_MAX > 1)
		else if ((n = rxvt_str_match(str, "rm")) != 0) {
			str += n;
			switch (str[0]) {
			case ':':
				str++;
				/* FALLTHROUGH */
			case '\0':
				/* FALLTHROUGH */
			case '*':
				rxvt_menubar_remove(r, str);
				break;
			}
			r->h->menu_readonly = 1;
		}
		else if ((n = rxvt_str_match(str, "menu")) != 0) {
			str += n;
			switch (str[0]) {
			case ':':
				str++;
				/* add/access menuBar */
				if (*str != '\0' && *str != '*')
					rxvt_menubar_push(r, str);
				break;
			default:
				if (r->h->CurrentBar == NULL) {
					rxvt_menubar_push(r, "default");
				}
			}

			if (r->h->CurrentBar != NULL)
				/* allow menu build commands */
				r->h->menu_readonly = 0;
		}
		else if (!STRCMP(str, "dump")) {
			/* dump current menubars to a file */
			FILE		   *fp;

			/* enough space to hold the results */
			char			buffer[32];

			sprintf(buffer, "/tmp/" APL_SUBCLASS "-%u",
				(unsigned int)getpid());

			if ((fp = fopen(buffer, "wb")) != NULL) {
				rxvt_xterm_seq(r, ATAB(r), XTerm_title, buffer, CHAR_ST);
				rxvt_menubar_dump(r, fp);
				fclose(fp);
			}
		}
		else if (!STRCMP(str, "next")) {
			if (r->h->CurrentBar) {
				r->h->CurrentBar = r->h->CurrentBar->next;
				r->h->menu_readonly = 1;
			}
		}
		else if (!STRCMP(str, "prev")) {
			if (r->h->CurrentBar) {
				r->h->CurrentBar = r->h->CurrentBar->prev;
				r->h->menu_readonly = 1;
		}
	}
	else if (!STRCMP(str, "swap")) {
		/* swap the top 2 menus */
		if (r->h->CurrentBar) {
			bar_t		  *cbprev = r->h->CurrentBar->prev;
			bar_t		  *cbnext = r->h->CurrentBar->next;

			cbprev->next = cbnext;
			cbnext->prev = cbprev;

			r->h->CurrentBar->next = cbprev;
			r->h->CurrentBar->prev = cbprev->prev;

			cbprev->prev->next = r->h->CurrentBar;
			cbprev->prev = r->h->CurrentBar;

			r->h->CurrentBar = cbprev;
			r->h->menu_readonly = 1;
		}
	}
# endif				/* (MENUBAR_MAX > 1) */
	str = next;

	r->h->BuildMenu = r->h->ActiveMenu = NULL;
	rxvt_menubar_expose(r);
# ifdef DEBUG_MENUBAR_STACKING
	fprintf(stderr, "menus are read%s\n",
		r->h->menu_readonly ? "only" : "/write");
# endif
	}
	return;
	break;
	}

# if (MENUBAR_MAX > 1)
	if (r->h->CurrentBar == NULL)
		return;
	if (r->h->menu_readonly) {
#  ifdef DEBUG_MENUBAR_STACKING
		fprintf(stderr, "menus are read%s\n",
			r->h->menu_readonly ? "only" : "/write");
#  endif
		return;
	}
# endif				/* (MENUBAR_MAX > 1) */

	switch (cmd) {
	case '+':
	case '-':
	path = name = str;

	name2 = NULL;
	/* parse STR, allow spaces inside (name)  */
	if (path[0] != '\0') {
		name = STRCHR(path, MENUITEM_BEG);
		str = STRCHR(path, MENUITEM_END);
		if (name != NULL || str != NULL) {
		if (name == NULL || str == NULL || str <= (name + 1)
			|| (name > path && name[-1] != '/')) {
			rxvt_print_error("menu error <%s>\n", path);
			break;
		}
		if (str[1] == MENUITEM_BEG) {
			name2 = (str + 2);
			str = STRCHR(name2, MENUITEM_END);

			if (str == NULL) {
			rxvt_print_error("menu error <%s>\n", path);
			break;
			}
			name2[-2] = '\0';	/* remove prev MENUITEM_END */
		}
		if (name > path && name[-1] == '/')
			name[-1] = '\0';

		*name++ = '\0';	/* delimit */
		*str++ = '\0';	/* delimit */

		while (isspace((int) *str))
			str++;	/* skip space */
		}
# ifdef DEBUG_MENU
		fprintf(stderr,
			"`%c' path = <%s>, name = <%s>, name2 = <%s>, action = <%s>\n",
			cmd, (path ? path : "(nil)"), (name ? name : "(nil)"),
			(name2 ? name2 : "(nil)"), (str ? str : "(nil)")
		);
# endif
	}
	/* process the different commands */
	switch (cmd) {
	case '+':		/* add/replace existing menu or menuitem */
		if (path[0] != '\0') {
		int			 len;

		path = rxvt_menu_find_base(r, &(r->h->BuildMenu), path);
		len = STRLEN(path);

		/* don't allow menus called `*' */
		if (path[0] == '*') {
			rxvt_menu_clear(r, r->h->BuildMenu);
			break;
		} else if (len >= 2 && !STRCMP((path + len - 2), "/*")) {
			path[len - 2] = '\0';
		}
		if (path[0] != '\0')
			r->h->BuildMenu = rxvt_menu_add(r, r->h->BuildMenu, path);
		}
		if (name != NULL && name[0] != '\0')
		rxvt_menuitem_add(r->h->BuildMenu,
				  (STRCMP(name, SEPARATOR_NAME) ? name : ""),
				  name2, str);
		break;

	case '-':		/* delete menu entry */
		if (!STRCMP(path, "/*") && (name == NULL || name[0] == '\0')) {
		rxvt_menubar_clear(r);
		r->h->BuildMenu = NULL;
		rxvt_menubar_expose(r);
		break;
		} else if (path[0] != '\0') {
		int			 len;
		menu_t		 *menu = r->h->BuildMenu;

		path = rxvt_menu_find_base(r, &menu, path);
		len = STRLEN(path);

		/* submenu called `*' clears all menu items */
		if (path[0] == '*') {
			rxvt_menu_clear(r, menu);
			break;	/* done */
		} else if (len >= 2 && !STRCMP(&path[len - 2], "/*")) {
			/* done */
			break;
		} else if (path[0] != '\0') {
			r->h->BuildMenu = NULL;
			break;
		} else
			r->h->BuildMenu = menu;
		}
		if (r->h->BuildMenu != NULL) {
		if (name == NULL || name[0] == '\0')
			r->h->BuildMenu = rxvt_menu_delete(r, r->h->BuildMenu);
		else {
			const char	 *n1;
			menuitem_t	 *item;
			menu_t		 *BuildMenu = r->h->BuildMenu;

			n1 = STRCMP(name, SEPARATOR_NAME) ? name : "";
			item = rxvt_menuitem_find(BuildMenu, n1);
			if (item != NULL && item->entry.type != MenuSubMenu) {
			rxvt_menuitem_free(r, BuildMenu, item);

			/* fix up the width */
			BuildMenu->width = 0;
			for (item = BuildMenu->head; item != NULL;
				 item = item->next) {
				short		   l = item->len + item->len2;

				MAX_IT(BuildMenu->width, l);
			}
			}
		}
		rxvt_menubar_expose(r);
		}
		break;
	}
	break;
	}
}


/*
** general dispatch routine,
** it would be nice to have `sticky' menus
*/
/* EXTPROTO */
void
rxvt_menubar_control(rxvt_t *r, XButtonEvent *ev)
{
	switch (ev->type) {
	case ButtonPress:
	if (ev->button == Button1)
		rxvt_menubar_select(r, ev);
	break;

	case ButtonRelease:
	if (ev->button == Button1)
		rxvt_menu_select(r, ev);
	break;

	case MotionNotify:
	while (XCheckTypedWindowEvent(r->Xdisplay, r->TermWin.parent,
					  MotionNotify, (XEvent *) ev)) ;

	if (r->h->ActiveMenu)
		while (rxvt_menu_select(r, ev)) ;
	else
		ev->y = -1;
	if (ev->y < 0) {
		Window		  unused_root, unused_child;
		int			 unused_root_x, unused_root_y;
		unsigned int	unused_mask;

		XQueryPointer(r->Xdisplay, r->menuBar.win,
			  &unused_root, &unused_child,
			  &unused_root_x, &unused_root_y,
			  &(ev->x), &(ev->y), &unused_mask);
		rxvt_menubar_select(r, ev);
	}
	break;
	}
}


/*
 * read in menubar commands from FILENAME
 * ignore all input before the tag line [menu] or [menu:???]
 *
 * Note that since File_find () is used, FILENAME can be semi-colon
 * delimited such that the second part can refer to a tag
 * so that a large `database' of menus can be collected together
 *
 * FILENAME = "file"
 * FILENAME = "file;"
 *	  read `file' starting with first [menu] or [menu:???] line
 *
 * FILENAME = "file;tag"
 *	  read `file' starting with [menu:tag]
 */
/* EXTPROTO */
void
rxvt_menubar_load_file(rxvt_t *r, const char *filename)
{
/* read in a menu from a file */
	FILE		   *fp;
	char			buffer[256];
	char		   *p, *file, *tag = NULL;

	file = (char *)rxvt_File_find(filename, ".menu", r->h->rs[Rs_path]);
	if (file == NULL)
		return;
	fp = fopen(file, "rb");
	free(file);
	if (fp == NULL)
		return;

# if (MENUBAR_MAX > 1)
/* semi-colon delimited */
	if ((tag = STRCHR(filename, ';')) != NULL) {
	tag++;
	if (*tag == '\0')
		tag = NULL;
	}
# endif				/* (MENUBAR_MAX > 1) */
# ifdef DEBUG_MENU
	fprintf(stderr, "[read:%s]\n", p);
	if (tag)
		fprintf(stderr, "looking for [menu:%s]\n", tag);
# endif

	while ((p = fgets(buffer, sizeof(buffer), fp)) != NULL) {
		int			 n;

		if ((n = rxvt_str_match(p, "[menu")) != 0) {
			if (tag) {
				/* looking for [menu:tag] */
				if (p[n] == ':' && p[n + 1] != ']') {
					n++;
					n += rxvt_str_match(p + n, tag);
					if (p[n] == ']') {
# ifdef DEBUG_MENU
						fprintf(stderr, "[menu:%s]\n", tag);
# endif
						break;
					}
				}
			}
			else if (p[n] == ':' || p[n] == ']')
				break;
		}
	}


	/* found [menu], [menu:???] tag */
	while (p != NULL) {
		int			 n;

# ifdef DEBUG_MENU
		fprintf(stderr, "read line = %s\n", p);
# endif

		/* looking for [done:tag] or [done:] */
		if ((n = rxvt_str_match(p, "[done")) != 0) {
			if (p[n] == ']') {
				r->h->menu_readonly = 1;
				break;
			}
			else if (p[n] == ':') {
				n++;
				if (p[n] == ']') {
					r->h->menu_readonly = 1;
					break;
				}
				else if (tag) {
					n += rxvt_str_match(p + n, tag);
					if (p[n] == ']') {
# ifdef DEBUG_MENU
						fprintf(stderr, "[done:%s]\n", tag);
# endif
						r->h->menu_readonly = 1;
						break;
					}
				}
				else {
					/* what? ... skip this line */
					p[0] = COMMENT_CHAR;
				}
			}
		}

		/*
		 * remove leading/trailing space
		 * and strip-off leading/trailing quotes
		 * skip blank or comment lines
		 */
		rxvt_str_trim(p);
		if (*p && *p != '#') {
			/* if case we read another file */
			r->h->menu_readonly = 0;
			rxvt_menubar_dispatcher(r, p);
		}
		/* get another line */
		p = fgets(buffer, sizeof(buffer), fp);
	}

	fclose(fp);
}



/* EXTPROTO */
unsigned short
rxvt_menubar_height(rxvt_t *r)
{
	/* If menubar is not created or not mapped, return 0 */
	if (None == r->menuBar.win || !r->menuBar.state)
		return 0;
	/* If menubar is not created or not mapped, return 0 */
# if (MENUBAR_MAX > 1)
	return (r->TermWin.fheight + SHADOW + MENUBAR_MARGIN);
# else
	return 0;
# endif
}


/* EXTPROTO */
unsigned short
rxvt_menubar_rheight(rxvt_t *r)
{
# if (MENUBAR_MAX > 1)
	return (r->TermWin.fheight + SHADOW + MENUBAR_MARGIN);
# else
	return 0;
# endif
}


/* EXTPROTO */
int
rxvt_is_menubar_win(rxvt_t *r, Window w)
{
	return (w == r->menuBar.win);
}

/* EXTPROTO */
void
rxvt_menubar_resize(rxvt_t *r)
{
	if (None != r->menuBar.win && r->menuBar.state)
		XMoveResizeWindow(r->Xdisplay, r->menuBar.win,
			0, 0, TWIN_WIDTH(r), rxvt_menubar_rheight(r));
}

#endif	/* HAVE_MENUBAR */
/*----------------------- end-of-file (C source) -----------------------*/
