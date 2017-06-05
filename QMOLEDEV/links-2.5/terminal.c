/* terminal.c
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "links.h"

static void in_term(struct terminal *);
static void check_if_no_terminal(void);


int hard_write(int fd, unsigned char *p, int l)
{
	int w = 1;
	int t = 0;
	while (l > 0 && w) {
		if ((w = write(fd, p, l)) < 0) {
			if (errno == EINTR) continue;
			return -1;
		}
		t += w;
		p += w;
		l -= w;
	}
	return t;
}

int hard_read(int fd, unsigned char *p, int l)
{
	int r = 1;
	int t = 0;
	while (l > 0 && r) {
		if ((r = read(fd, p, l)) < 0) {
			if (errno == EINTR) continue;
			return -1;
		}
		/*{int ww;for(ww=0;ww<r;ww++)fprintf(stderr," %02x",(int)p[ww]);fflush(stderr);}*/
		t += r;
		p += r;
		l -= r;
	}
	return t;
}

unsigned char *get_cwd(void)
{
	int bufsize = 128;
	unsigned char *buf;
	while (1) {
		buf = mem_alloc(bufsize);
		if (getcwd(buf, bufsize)) return buf;
		mem_free(buf);
		if (errno == EINTR) continue;
		if (errno != ERANGE) return NULL;
		if ((unsigned)bufsize > MAXINT - 128) overalloc();
		bufsize += 128;
	}
	return NULL;
}

void set_cwd(unsigned char *path)
{
	if (path) {
		while (chdir(path) && errno == EINTR)
			;
	}
}

struct list_head terminals = {&terminals, &terminals};

static void alloc_term_screen(struct terminal *term, int x, int y)
{
	chr *s, *t;
	NO_GFX;
	if (x && (unsigned)x * (unsigned)y / (unsigned)x != (unsigned)y) overalloc();
	if ((unsigned)x * (unsigned)y > MAXINT / sizeof(*term->screen)) overalloc();
	s = mem_realloc(term->screen, x * y * sizeof(*term->screen));
	t = mem_realloc(term->last_screen, x * y * sizeof(*term->screen));
	memset(t, -1, x * y * sizeof(*term->screen));
	term->x = x;
	term->y = y;
	term->last_screen = t;
	memset(s, 0, x * y * sizeof(*term->screen));
	term->screen = s;
	term->dirty = 1;
}

static void clear_terminal(struct terminal *term)
{
	NO_GFX;
	fill_area(term, 0, 0, term->x, term->y, ' ', 0);
	set_cursor(term, 0, 0, 0, 0);
}

void redraw_below_window(struct window *win)
{
	int tr;
	struct terminal *term = win->term;
	struct window *end = win;
	struct event ev = { EV_REDRAW, 0, 0, 0 };
	NO_GFX;
	ev.x = term->x;
	ev.y = term->y;
	if (term->redrawing >= 2) return;
	tr = term->redrawing;
	win->term->redrawing = 2;
	for (win = term->windows.prev; win != end; win = win->prev) {
		win->handler(win, &ev, 0);
	}
	term->redrawing = tr;
}

static void redraw_terminal_ev(struct terminal *term, int e)
{
	struct window *win;
	struct event ev = {0, 0, 0, 0};
	NO_GFX;
	ev.ev = e;
	ev.x = term->x;
	ev.y = term->y;
	clear_terminal(term);
	term->redrawing = 2;
	foreachback(win, term->windows) win->handler(win, &ev, 0);
	term->redrawing = 0;
}

void redraw_terminal(struct terminal *term)
{
	NO_GFX;
	redraw_terminal_ev(term, EV_REDRAW);
}

void redraw_terminal_all(struct terminal *term)
{
	NO_GFX;
	redraw_terminal_ev(term, EV_RESIZE);
}

static void erase_screen(struct terminal *term)
{
	NO_GFX;
	if (!term->master || !is_blocked()) {
		if (term->master) want_draw();
		hard_write(term->fdout, "\033[2J\033[1;1H", 10);
		if (term->master) done_draw();
	}
}

void redraw_terminal_cls(struct terminal *term)
{
	NO_GFX;
	erase_screen(term);
	alloc_term_screen(term, term->x, term->y);
	redraw_terminal_all(term);
}

void cls_redraw_all_terminals(void)
{
	struct terminal *term;
	foreach(term, terminals) {
		if (!F) redraw_terminal_cls(term);
#ifdef G
		else {
			t_resize(term->dev);
		}
#endif
	}
}

#ifdef G

int do_rects_intersect(struct rect *r1, struct rect *r2)
{
	return (r1->x1 > r2->x1 ? r1->x1 : r2->x1) < (r1->x2 > r2->x2 ? r2->x2 : r1->x2) && (r1->y1 > r2->y1 ? r1->y1 : r2->y1) < (r1->y2 > r2->y2 ? r2->y2 : r1->y2);
}

void intersect_rect(struct rect *v, struct rect *r1, struct rect *r2)
{
	v->x1 = r1->x1 > r2->x1 ? r1->x1 : r2->x1;
	v->x2 = r1->x2 > r2->x2 ? r2->x2 : r1->x2;
	v->y1 = r1->y1 > r2->y1 ? r1->y1 : r2->y1;
	v->y2 = r1->y2 > r2->y2 ? r2->y2 : r1->y2;
}

void unite_rect(struct rect *v, struct rect *r1, struct rect *r2)
{
	if (!is_rect_valid(r1)) {
		if (v != r2) memcpy(v, r2, sizeof(struct rect));
		return;
	}
	if (!is_rect_valid(r2)) {
		if (v != r1) memcpy(v, r1, sizeof(struct rect));
		return;
	}
	v->x1 = r1->x1 < r2->x1 ? r1->x1 : r2->x1;
	v->x2 = r1->x2 < r2->x2 ? r2->x2 : r1->x2;
	v->y1 = r1->y1 < r2->y1 ? r1->y1 : r2->y1;
	v->y2 = r1->y2 < r2->y2 ? r2->y2 : r1->y2;
}

int is_rect_valid(struct rect *r1)
{
	return r1->x1 < r1->x2 && r1->y1 < r1->y2;
}

#define R_GR	8

struct rect_set *init_rect_set(void)
{
	struct rect_set *s;
	s = mem_calloc(sizeof(struct rect_set) + sizeof(struct rect) * R_GR);
	s->rl = R_GR;
	s->m = 0;
	return s;
}

void add_to_rect_set(struct rect_set **s, struct rect *r)
{
	struct rect_set *ss = *s;
	int i;
	if (!is_rect_valid(r)) return;
	for (i = 0; i < ss->rl; i++) if (!ss->r[i].x1 && !ss->r[i].x2 && !ss->r[i].y1 && !ss->r[i].y2) {
		x:
		memcpy(&ss->r[i], r, sizeof(struct rect));
		if (i >= ss->m) ss->m = i + 1;
		return;
	}
	if ((unsigned)ss->rl > (MAXINT - sizeof(struct rect_set)) / sizeof(struct rect) - R_GR) overalloc();
	ss = mem_realloc(ss, sizeof(struct rect_set) + sizeof(struct rect) * (ss->rl + R_GR));
	memset(&(*s = ss)->r[i = (ss->rl += R_GR) - R_GR], 0, sizeof(struct rect) * R_GR);
	goto x;
}

void exclude_rect_from_set(struct rect_set **s, struct rect *r)
{
	int i, a;
	struct rect *rr;
	do {
		a = 0;
		for (i = 0; i < (*s)->m; i++) if (do_rects_intersect(rr = &(*s)->r[i], r)) {
			struct rect r1, r2, r3, r4;
			r1.x1 = rr->x1;
			r1.x2 = rr->x2;
			r1.y1 = rr->y1;
			r1.y2 = r->y1;

			r2.x1 = rr->x1;
			r2.x2 = r->x1;
			r2.y1 = r->y1;
			r2.y2 = r->y2;

			r3.x1 = r->x2;
			r3.x2 = rr->x2;
			r3.y1 = r->y1;
			r3.y2 = r->y2;

			r4.x1 = rr->x1;
			r4.x2 = rr->x2;
			r4.y1 = r->y2;
			r4.y2 = rr->y2;

			intersect_rect(&r2, &r2, rr);
			intersect_rect(&r3, &r3, rr);
			rr->x1 = rr->x2 = rr->y1 = rr->y2 = 0;
#ifdef DEBUG
			if (is_rect_valid(&r1) && do_rects_intersect(&r1, r)) internal("bad intersection 1");
			if (is_rect_valid(&r2) && do_rects_intersect(&r2, r)) internal("bad intersection 2");
			if (is_rect_valid(&r3) && do_rects_intersect(&r3, r)) internal("bad intersection 3");
			if (is_rect_valid(&r4) && do_rects_intersect(&r4, r)) internal("bad intersection 4");
#endif
			add_to_rect_set(s, &r1);
			add_to_rect_set(s, &r2);
			add_to_rect_set(s, &r3);
			add_to_rect_set(s, &r4);
			a = 1;
		}
	} while (a);
}

/* memory address r must contain one struct rect
 * x1 is leftmost pixel that is still valid
 * x2 is leftmost pixel that isn't valid any more
 * y1, y2 analogically
 */
int restrict_clip_area(struct graphics_device *dev, struct rect *r, int x1, int y1, int x2, int y2)
{
	struct rect v, rr;
	rr.x1 = x1, rr.x2 = x2, rr.y1 = y1, rr.y2 = y2;
	if (r) memcpy(r, &dev->clip, sizeof(struct rect));
	intersect_rect(&v, &dev->clip, &rr);
	drv->set_clip_area(dev, &v);
	return is_rect_valid(&v);
}

#endif

void draw_to_window(struct window *win, void (*fn)(struct terminal *term, void *), void *data)
{
	struct terminal *term = win->term;
	struct window *end = (void *)&term->windows;
	if (!F) {
		pr(fn(term, data)) {};
		term = win->term;
		end = (void *)&term->windows;
		if (win->prev == end || term->redrawing) return;
		term->redrawing = 1;
		{
			struct event ev = { EV_REDRAW, 0, 0, 0 };
			ev.x = term->x;
			ev.y = term->y;
			for (win = win->prev; win != end; win = win->prev) win->handler(win, &ev, 0);
		}
		term->redrawing = 0;
#ifdef G
	} else {
		struct rect r1, *r;
		struct rect_set *s;
		int i, a;
		if (win->prev == end || !(s = init_rect_set())) {
			pr(fn(term, data)) {};
			return;
		}
		intersect_rect(&r1, &win->pos, &term->dev->clip);
		add_to_rect_set(&s, &r1);
		for (win = win->prev; win != end; win = win->prev) exclude_rect_from_set(&s, &win->pos);
		a = 0;
		memcpy(&r1, &term->dev->clip, sizeof(struct rect));
		for (i = 0; i < s->m; i++) if (is_rect_valid(r = &s->r[i])) {
			drv->set_clip_area(term->dev, r);
			pr(fn(term, data)) return;
			a = 1;
		}
		if (!a) {
			struct rect empty = { 0, 0, 0, 0 };
			drv->set_clip_area(term->dev, &empty);
			fn(term, data);
		}
		drv->set_clip_area(term->dev, &r1);
		mem_free(s);
#endif
	}
}

#ifdef G

static void redraw_windows(struct terminal *term)
{
	struct terminal *t1;
	struct window *win;
	foreach(t1, terminals) if (t1 == term) goto ok;
	return;
	ok:
	foreach(win, term->windows) {
		struct event ev = { EV_REDRAW, 0, 0, 0 };
		ev.x = term->x;
		ev.y = term->y;
		drv->set_clip_area(term->dev, &win->redr);
		memset(&win->redr, 0, sizeof(struct rect));
		win->handler(win, &ev, 0);
	}
	drv->set_clip_area(term->dev, &term->dev->size);
}

void set_window_pos(struct window *win, int x1, int y1, int x2, int y2)
{
	struct terminal *term = win->term;
	struct rect r;
	NO_TXT;
	r.x1 = x1, r.y1 = y1, r.x2 = x2, r.y2 = y2;
	if (is_rect_valid(&win->pos) && (x1 > win->pos.x1 || x2 < win->pos.x2 || y1 > win->pos.y1 || y2 < win->pos.y2) && term->redrawing < 2) {
		struct window *w;
		for (w = win->next; w != (void *)&win->term->windows; w = w->next) unite_rect(&w->redr, &win->pos, &w->redr);
		register_bottom_half((void (*)(void *))redraw_windows, term);
	}
	memcpy(&win->pos, &r, sizeof(struct rect));
}

#endif

void add_window_at_pos(struct terminal *term, void (*handler)(struct window *, struct event *, int), void *data, struct window *at)
{
	struct event ev = { EV_INIT, 0, 0, 0 };
	struct window *win;
	ev.x = term->x;
	ev.y = term->y;
	win = mem_calloc(sizeof(struct window));
	win->handler = handler;
	win->data = data;
	win->term = term;
	win->xp = win->yp = 0;
	add_at_pos(at, win);
	win->handler(win, &ev, 0);
}

void add_window(struct terminal *term, void (*handler)(struct window *, struct event *, int), void *data)
{
	add_window_at_pos(term, handler, data, (struct window *)(void *)&term->windows);
}

void delete_window(struct window *win)
{
	struct event ev = { EV_ABORT, 0, 0, 0 };
	win->handler(win, &ev, 1);
	del_from_list(win);
	if (win->data) mem_free(win->data);
	if (!F) redraw_terminal(win->term);
#ifdef G
	else {
		struct window *w;
		for (w = win->next; w != (void *)&win->term->windows; w = w->next) unite_rect(&w->redr, &win->pos, &w->redr);
		register_bottom_half((void (*)(void *))redraw_windows, win->term);
	}
#endif
	mem_free(win);
}

void delete_window_ev(struct window *win, struct event *ev)
{
	struct window *w = win->next;
	if ((void *)w == &win->term->windows) w = NULL;
	delete_window(win);
	if (ev && w && w->next != w) w->handler(w, ev, 1);
}

void set_window_ptr(struct window *win, int x, int y)
{
	if (win->xp == x && win->yp == y) return;
	win->xp = x;
	win->yp = y;
#ifdef G
	if (F && win->prev != (void *)&win->term->windows) {
		memcpy(&win->prev->redr, &win->term->dev->size, sizeof(struct rect));
		register_bottom_half((void (*)(void *))redraw_windows, win->term);
	}
#endif
}

void get_parent_ptr(struct window *win, int *x, int *y)
{
	if ((void *)win->next != &win->term->windows) {
		*x = win->next->xp;
		*y = win->next->yp;
	} else {
		*x = *y = 0;
	}
}

struct ewd {
	void (*fn)(void *);
	void *data;
	int b;
};

static void empty_window_handler(struct window *win, struct event *ev, int fwd)
{
	struct window *n;
	struct ewd *ewd = win->data;
	int x, y;
	void (*fn)(void *) = ewd->fn;
	void *data = ewd->data;
	if (ewd->b) return;
	switch ((int)ev->ev) {
		case EV_INIT:
		case EV_RESIZE:
		case EV_REDRAW:
			get_parent_ptr(win, &x, &y);
			set_window_ptr(win, x, y);
			return;
		case EV_ABORT:
			fn(data);
			return;
	}
	ewd->b = 1;
	n = win->next;
	delete_window(win);
	fn(data);
	if (n->next != n) n->handler(n, ev, fwd);
}

void add_empty_window(struct terminal *term, void (*fn)(void *), void *data)
{
	struct ewd *ewd;
	ewd = mem_alloc(sizeof(struct ewd));
	ewd->fn = fn;
	ewd->data = data;
	ewd->b = 0;
	add_window(term, empty_window_handler, ewd);
}

void free_term_specs(void)
{
	free_list(term_specs);
}

struct list_head term_specs = {&term_specs, &term_specs};

static struct term_spec dumb_term = { NULL, NULL, "", 0, 1, 0, 0, 0, 0, 0 };

static struct term_spec *get_term_spec(unsigned char *term)
{
	struct term_spec *t;
	NO_GFX;
	foreach(t, term_specs) if (!strcasecmp(t->term, term)) return t;
	return &dumb_term;
}

struct term_spec *new_term_spec(unsigned char *term)
{
	struct term_spec *t;
	foreach(t, term_specs) if (!strcasecmp(t->term, term)) return t;
	t = mem_alloc(sizeof(struct term_spec));
	memcpy(t, &dumb_term, sizeof(struct term_spec));
	if (strlen(term) < MAX_TERM_LEN) strcpy(t->term, term);
	else memcpy(t->term, term, MAX_TERM_LEN - 1), t->term[MAX_TERM_LEN - 1] = 0;
	add_to_list(term_specs, t);
	sync_term_specs();
	return t;
}

void sync_term_specs(void)
{
	struct terminal *term;
	foreach(term, terminals) term->spec = get_term_spec(term->term);
}

struct terminal *init_term(int fdin, int fdout, void (*root_window)(struct window *, struct event *, int))
{
	static tcount terminal_count = 0;
	struct terminal *term;
	struct window *win;
	NO_GFX;
	term = mem_calloc(sizeof(struct terminal));
	term->count = terminal_count++;
	term->fdin = fdin;
	term->fdout = fdout;
	term->master = term->fdout == get_output_handle();
	term->lcx = -1;
	term->lcy = -1;
	term->dirty = 1;
	term->blocked = -1;
	term->screen = DUMMY;
	term->last_screen = DUMMY;
	term->spec = &dumb_term;
	term->input_queue = DUMMY;
	init_list(term->windows);
	win = mem_calloc(sizeof(struct window));
	win->handler = root_window;
	win->term = term;
	add_to_list(term->windows, win);
	/*alloc_term_screen(term, 80, 25);*/
	add_to_list(terminals, term);
	set_handlers(fdin, (void (*)(void *))in_term, NULL, (void (*)(void *))destroy_terminal, term);
	return term;
}

static int process_utf_8(struct terminal *term, struct event *ev)
{
#if defined(G) || defined(ENABLE_UTF8)
	if (ev->ev == EV_KBD && ((!F && term->spec->charset == utf8_table)
#ifdef G
	    || (F && drv->codepage == utf8_table)
#endif
	    )) {
		size_t l;
		unsigned char *p;
		unsigned c;
		if (ev->x <= 0 || ev->x >= 0x100) goto direct;
		if ((l = strlen(term->utf8_buffer)) >= sizeof(term->utf8_buffer) - 1 || ev->x < 0x80 || ev->x >= 0xc0)
			term->utf8_buffer[0] = 0, l = 0;
		term->utf8_buffer[l] = ev->x;
		term->utf8_buffer[l + 1] = 0;
		p = term->utf8_buffer;
		GET_UTF_8(p, c);
		if (!c) return 0;
		ev->x = c;
		direct:
		term->utf8_buffer[0] = 0;
	}
#endif
	return 1;
}

#ifdef G

static struct term_spec gfx_term = { NULL, NULL, "", 0, 0, 0, 0, 0, 0, 0 };

struct terminal *init_gfx_term(void (*root_window)(struct window *, struct event *, int), void *info, int len)
{
	static tcount terminal_count = 0;
	struct terminal *term;
	struct graphics_device *dev;
	struct window *win;
	unsigned char *cwd;
	NO_TXT;
	term = mem_calloc(sizeof(struct terminal));
	term->count = terminal_count++;
	term->fdin = -1;
	if (!(term->dev = dev = drv->init_device())) {
		mem_free(term);
		check_if_no_terminal();
		return NULL;
	}
	dev->user_data = term;
	term->master = 1;
	term->blocked = -1;
	term->x = dev->size.x2;
	term->y = dev->size.y2;
	term->last_mouse_x = term->last_mouse_y = term->last_mouse_b = MAXINT;
	term->environment = !(drv->flags & GD_ONLY_1_WINDOW) ? ENV_G : 0;
	if (!strcasecmp(drv->name, "x")) term->environment |= ENV_XWIN;
	term->spec = &gfx_term;
	if ((cwd = get_cwd())) {
		safe_strncpy(term->cwd, cwd, MAX_CWD_LEN);
		mem_free(cwd);
	}
	gfx_term.charset = get_cp_index("utf-8");
	if (gfx_term.charset == -1) gfx_term.charset = 0;
	init_list(term->windows);
	win = mem_calloc(sizeof (struct window));
	win->handler = root_window;
	win->term = term;
	win->pos.x2 = dev->size.x2;
	win->pos.y2 = dev->size.y2;
	add_to_list(term->windows, win);
	add_to_list(terminals, term);
	dev->redraw_handler = t_redraw;
	dev->resize_handler = t_resize;
	dev->keyboard_handler = t_kbd;
	dev->mouse_handler = t_mouse;
	{
		int *ptr;
		struct event ev = { EV_INIT, 0, 0, 0 };
		ev.x = dev->size.x2;
		ev.y = dev->size.y2;
		if ((unsigned)len > MAXINT - sizeof(int)) overalloc();
		ptr = mem_alloc(sizeof(int) + len);
		*ptr = len;
		memcpy(ptr + 1, info, len);
		ev.b = (long)ptr;
		root_window(win, &ev, 0);
		mem_free(ptr);
	}
	return term;
}

void t_redraw(struct graphics_device *dev, struct rect *r)
{
	struct terminal *term = dev->user_data;
	struct window *win;
	/*debug("%d %d %d %d", r->x1, r->x2, r->y1, r->y2);*/
	/*fprintf(stderr, "t_redraw: %d,%d %d,%d\n", r->x1, r->y1, r->x2, r->y2);*/
	foreach(win, term->windows) unite_rect(&win->redr, r, &win->redr);
	register_bottom_half((void (*)(void *))redraw_windows, term);
}

void t_resize(struct graphics_device *dev)
{
	struct terminal *term = dev->user_data;
	struct window *win;
	struct event ev = { EV_RESIZE, 0, 0, 0 };
	term->x = ev.x = dev->size.x2;
	term->y = ev.y = dev->size.y2;
	drv->set_clip_area(dev, &dev->size);
	foreach(win, term->windows) {
		win->handler(win, &ev, 0);
	}
	drv->set_clip_area(dev, &dev->size);
}

void t_kbd(struct graphics_device *dev, int key, int flags)
{
	struct terminal *term = dev->user_data;
	struct event ev = { EV_KBD, 0, 0, 0 };
	struct rect r = {0, 0, 0, 0};
	r.x2 = dev->size.x2, r.y2 = dev->size.y2;
	ev.x = key;
	ev.y = flags;
	if (upcase(key) == 'L' && flags == KBD_CTRL) {
		t_redraw(dev, &r);
		return;
	} else {
		drv->set_clip_area(dev, &r);
		if (list_empty(term->windows)) return;
		if (ev.x == KBD_CTRL_C || ev.x == KBD_CLOSE) ((struct window *)term->windows.prev)->handler(term->windows.prev, &ev, 0);
		else {
			if (process_utf_8(term, &ev))
				((struct window *)term->windows.next)->handler(term->windows.next, &ev, 0);
		}
	}
}

void t_mouse(struct graphics_device *dev, int x, int y, int b)
{
	struct terminal *term = dev->user_data;
	struct event ev = { EV_MOUSE, 0, 0, 0 };
	struct rect r = {0, 0, 0, 0};
	int bt, ac;
	if (x == term->last_mouse_x && y == term->last_mouse_y && b == term->last_mouse_b) {
		return;
	}
	bt = b & BM_BUTT;
	ac = b & BM_ACT;
	if ((ac == B_MOVE || ac == B_DRAG) &&
	    (bt == B_LEFT || bt == B_MIDDLE || bt == B_RIGHT || bt == B_FOURTH || bt == B_FIFTH || bt == B_SIXTH)) {
		term->last_mouse_x = x;
		term->last_mouse_y = y;
		term->last_mouse_b = b;
	} else {
		term->last_mouse_x = term->last_mouse_y = term->last_mouse_b = MAXINT;
	}
	r.x2 = dev->size.x2, r.y2 = dev->size.y2;
	ev.x = x, ev.y = y, ev.b = b;
	drv->set_clip_area(dev, &r);
	if (list_empty(term->windows)) return;
	((struct window *)term->windows.next)->handler(term->windows.next, &ev, 0);
}

#endif

static void in_term(struct terminal *term)
{
	struct event *ev;
	int r;
	unsigned char *iq;
	NO_GFX;
	if ((unsigned)term->qlen + ALLOC_GR > MAXINT) overalloc();
	iq = mem_realloc(term->input_queue, term->qlen + ALLOC_GR);
	term->input_queue = iq;
	if ((r = read(term->fdin, iq + term->qlen, ALLOC_GR)) <= 0) {
		if (r == -1 && errno != ECONNRESET) error("ERROR: error %d on terminal: could not read event", errno);
		destroy_terminal(term);
		return;
	}
	term->qlen += r;
	test_queue:
	if ((size_t)term->qlen < sizeof(struct event)) return;
	ev = (struct event *)iq;
	r = sizeof(struct event);
	if (ev->ev != EV_INIT && ev->ev != EV_RESIZE && ev->ev != EV_REDRAW && ev->ev != EV_KBD && ev->ev != EV_MOUSE && ev->ev != EV_ABORT) {
		error("ERROR: error on terminal: bad event %d", ev->ev);
		goto mm;
	}
	if (ev->ev == EV_INIT) {
		int init_len;
		if ((size_t)term->qlen < sizeof(struct event) + MAX_TERM_LEN + MAX_CWD_LEN + 2 * sizeof(int)) return;
		init_len = *(int *)(iq + sizeof(struct event) + MAX_TERM_LEN + MAX_CWD_LEN + sizeof(int));
		if ((size_t)term->qlen < sizeof(struct event) + MAX_TERM_LEN + MAX_CWD_LEN + 2 * sizeof(int) + init_len) return;
		memcpy(term->term, iq + sizeof(struct event), MAX_TERM_LEN);
		term->term[MAX_TERM_LEN - 1] = 0;
		memcpy(term->cwd, iq + sizeof(struct event) + MAX_TERM_LEN, MAX_CWD_LEN);
		term->cwd[MAX_CWD_LEN - 1] = 0;
		term->environment = *(int *)(iq + sizeof(struct event) + MAX_TERM_LEN + MAX_CWD_LEN);
		ev->b = (long)(iq + sizeof(struct event) + MAX_TERM_LEN + MAX_CWD_LEN + sizeof(int));
		r = sizeof(struct event) + MAX_TERM_LEN + MAX_CWD_LEN + 2 * sizeof(int) + init_len;
		sync_term_specs();
	}
	if (ev->ev == EV_REDRAW || ev->ev == EV_RESIZE || ev->ev == EV_INIT) {
		struct window *win;
		send_redraw:
		if (ev->x < 0 || ev->y < 0) {
			error("ERROR: bad terminal size: %d, %d", (int)ev->x, (int)ev->y);
			goto mm;
		}
		alloc_term_screen(term, ev->x, ev->y);
		clear_terminal(term);
		erase_screen(term);
		term->redrawing = 1;
		foreachback(win, term->windows) win->handler(win, ev, 0);
		term->redrawing = 0;
	}
	if (ev->ev == EV_KBD || ev->ev == EV_MOUSE) {
		if (ev->ev == EV_KBD && upcase(ev->x) == 'L' && ev->y & KBD_CTRL) {
			ev->ev = EV_REDRAW;
			ev->x = term->x;
			ev->y = term->y;
			goto send_redraw;
		}
		else if (ev->ev == EV_KBD && ev->x == KBD_CTRL_C) ((struct window *)term->windows.prev)->handler(term->windows.prev, ev, 0);
		else {
			if (process_utf_8(term, ev))
				((struct window *)term->windows.next)->handler(term->windows.next, ev, 0);
		}
	}
	if (ev->ev == EV_ABORT) {
		destroy_terminal(term);
		return;
	}
	/*redraw_screen(term);*/
	mm:
	if (term->qlen == r) term->qlen = 0;
	else memmove(iq, iq + r, term->qlen -= r);
	goto test_queue;
}

static inline int getcompcode(int c)
{
	return (c<<1 | (c&4)>>2) & 7;
}

unsigned char frame_dumb[48] =	"   ||||++||++++++--|-+||++--|-+----++++++++     ";
static unsigned char frame_vt100[48] =	"aaaxuuukkuxkjjjkmvwtqnttmlvwtqnvvwwmmllnnjla    ";
static unsigned char frame_koi[48] = {
	144,145,146,129,135,178,180,167,
	166,181,161,168,174,173,172,131,
	132,137,136,134,128,138,175,176,
	171,165,187,184,177,160,190,185,
	186,182,183,170,169,162,164,189,
	188,133,130,141,140,142,143,139,
};
static unsigned char frame_freebsd[48] = {
	130,138,128,153,150,150,150,140,
	140,150,153,140,139,139,139,140,
	142,151,152,149,146,143,149,149,
	142,141,151,152,149,146,143,151,
	151,152,152,142,142,141,141,143,
	143,139,141,128,128,128,128,128,
};
static unsigned char frame_restrict[48] = {
	  0,  0,  0,  0,  0,179,186,186,
	205,  0,  0,  0,  0,186,205,  0,
	  0,  0,  0,  0,  0,  0,179,186,
	  0,  0,  0,  0,  0,  0,  0,205,
	196,205,196,186,205,205,186,186,
	179,  0,  0,  0,  0,  0,  0,  0,
};

#if defined(ENABLE_UTF8) && defined(WIN32)
static inline char_t utf8_hack(char_t c)
{
	/*
	 * These characters produce beeps on Cygwin.
	 */
	switch (c) {
		case 0xb7:
		case 0x2022:
		case 0x2024:
		case 0x2026:
		case 0x2219:
		case 0x22c5:
		case 0x30fb:
			return '.';
		default:
			return c;
	}
}
#else
#define utf8_hack(x)	(x)
#endif

#define SETPOS(x, y)							\
{									\
	add_to_str(&a, &l, "\033[");					\
	add_num_to_str(&a, &l, (y) + 1);				\
	add_to_str(&a, &l, ";");					\
	add_num_to_str(&a, &l, (x) + 1);				\
	add_to_str(&a, &l, "H");					\
}

#define PRINT_CHAR(p)							\
{									\
	char_t c = term->screen[p].ch;					\
	unsigned char A = term->screen[p].at & 0x7f;			\
	unsigned char frm = !!(term->screen[p].at & ATTR_FRAME);	\
	if (s->mode == TERM_LINUX) {					\
		if (s->m11_hack) {					\
			if (frm != mode) {				\
				if (!(mode = frm)) add_to_str(&a, &l, "\033[10m");\
				else add_to_str(&a, &l, "\033[11m");	\
			}						\
		}							\
		if (s->restrict_852 && frm && c >= 176 && c < 224) {	\
			if (frame_restrict[c - 176]) c = frame_restrict[c - 176];\
		}							\
	} else if (s->mode == TERM_VT100) {				\
		if (frm != mode) {					\
			if (!(mode = frm)) add_to_str(&a, &l, "\017");	\
			else add_to_str(&a, &l, "\016");		\
		}							\
		if (frm && c >= 176 && c < 224) c = frame_vt100[c - 176];\
	} else if (s->mode == TERM_KOI8 && frm && c >= 176 && c < 224) { c = frame_koi[c - 176];\
	} else if (s->mode == TERM_FREEBSD && frm && c >= 176 && c < 224) { c = frame_freebsd[c - 176];\
	} else if (s->mode == TERM_DUMB && frm && c >= 176 && c < 224) c = frame_dumb[c - 176];\
	if (!(A & 0100) && (A >> 3) == (A & 7)) A = (A & 070) | 7 * !(A & 020);\
	if (A != attrib) {						\
		attrib = A;						\
		add_to_str(&a, &l, "\033[0");				\
		if (s->col) {						\
			unsigned char m[4];				\
			m[0] = ';'; m[1] = '3'; m[3] = 0;		\
			m[2] = (attrib & 7) + '0';			\
			add_to_str(&a, &l, m);				\
			m[1] = '4';					\
			m[2] = ((attrib >> 3) & 7) + '0';		\
			add_to_str(&a, &l, m);				\
		} else if (getcompcode(attrib & 7) < getcompcode(attrib >> 3 & 7))\
			add_to_str(&a, &l, ";7");			\
		if (attrib & 0100) add_to_str(&a, &l, ";1");		\
		add_to_str(&a, &l, "m");				\
	}								\
	if (c >= ' ' && c != 127/* && c != 155*/) {			\
		if (c < 128 || frm || !is_cp_special(term->spec->charset)) {\
			add_chr_to_str(&a, &l, c);			\
		} else {						\
		/*							\
		 * Linux UTF-8 console is broken and doesn't advance cursor\
		 * on some characters. So we first print an one-byte	\
		 * replacement, then set the cursor back, then print	\
		 * the UTF-8 character and finally set the cursor again.\
		 */							\
			unsigned char *r;				\
			c = utf8_hack(c);				\
			r = u2cp(c, 0, 1);				\
			if (!(r && r[0] >= 32 && r[0] < 127 && !r[1])) r = "*";	\
			add_chr_to_str(&a, &l, r[0]);			\
			if (cx + 1 < term->x)				\
				add_chr_to_str(&a, &l, 8);		\
			else						\
				SETPOS(cx, y);				\
			add_to_str(&a, &l, encode_utf_8(c));		\
			SETPOS(cx + 1, y);				\
		}							\
	}								\
	else if (!c || c == 1) add_chr_to_str(&a, &l, ' ');		\
	else add_chr_to_str(&a, &l, '.');				\
	cx++;								\
}									\

void redraw_all_terminals(void)
{
	struct terminal *term;
	foreach(term, terminals) redraw_screen(term);
}

void redraw_screen(struct terminal *term)
{
	int x, y, p = 0;
	int cx = term->lcx, cy = term->lcy;
	unsigned char *a;
	int attrib = -1;
	int mode = -1;
	int l = 0;
	struct term_spec *s;
	NO_GFX;
	if (!term->dirty || (term->master && is_blocked())) return;
	a = init_str();
	s = term->spec;
	for (y = 0; y < term->y; y++)
		for (x = 0; x < term->x; x++, p++) {
			int i;
			if (y == term->y - 1 && x == term->x - 1) break;
			if (!memcmp(&term->screen[p], &term->last_screen[p], sizeof(chr))) continue;
			if ((term->screen[p].at & 0x38) == (term->last_screen[p].at & 0x38) && (term->screen[p].ch == 0 || term->screen[p].ch == 1 || term->screen[p].ch == ' ') && (term->last_screen[p].ch == 0 || term->last_screen[p].ch == 1 || term->last_screen[p].ch == ' ') && (x != term->cx || y != term->cy)) continue;
			memcpy(&term->last_screen[p], &term->screen[p], sizeof(chr));
			if (cx == x && cy == y) goto pc;/*PRINT_CHAR(p)*/
			else if (cy == y && x - cx < 10 && x - cx > 0) {
				for (i = x - cx; i >= 0; i--) {
					ppc:
					PRINT_CHAR(p - i);
				}
			} else {
				SETPOS(x, y);
				cx = x; cy = y;
				pc:
				i = 0;
				goto ppc;
			}
		}
	if (l) {
		if (s->col) add_to_str(&a, &l, "\033[37;40m");
		add_to_str(&a, &l, "\033[0m");
		if (s->mode == TERM_LINUX && s->m11_hack) add_to_str(&a, &l, "\033[10m");
		if (s->mode == TERM_VT100) add_to_str(&a, &l, "\017");
	}
	term->lcx = cx;
	term->lcy = cy;
	if (term->cx != term->lcx || term->cy != term->lcy) {
		term->lcx = term->cx;
		term->lcy = term->cy;
		add_to_str(&a, &l, "\033[");
		add_num_to_str(&a, &l, term->cy + 1);
		add_to_str(&a, &l, ";");
		add_num_to_str(&a, &l, term->cx + 1);
		add_to_str(&a, &l, "H");
	}
	if (l && term->master) want_draw();
	hard_write(term->fdout, a, l);
	if (l && term->master) done_draw();
	mem_free(a);
	term->dirty = 0;
}

void destroy_terminal(struct terminal *term)
{
	while ((term->windows.next) != &term->windows) delete_window(term->windows.next);
	/*if (term->cwd) mem_free(term->cwd);*/
	del_from_list(term);
	if (term->blocked != -1) {
		close(term->blocked);
		set_handlers(term->blocked, NULL, NULL, NULL, NULL);
	}
	if (term->title) mem_free(term->title);
	if (!F) {
		mem_free(term->screen);
		mem_free(term->last_screen);
		set_handlers(term->fdin, NULL, NULL, NULL, NULL);
		mem_free(term->input_queue);
		close(term->fdin);
		if (!term->master) {
			if (term->fdout != term->fdin) close(term->fdout);
		} else {
			unhandle_terminal_signals(term);
			free_all_itrms();
#ifndef NO_FORK_ON_EXIT
			if (!list_empty(terminals)) {
				if (fork() > 0) _exit(0);
			}
#endif
		}
#ifdef G
	} else {
		drv->shutdown_device(term->dev);
#endif
	}
	mem_free(term);
	check_if_no_terminal();
}

void destroy_all_terminals(void)
{
	struct terminal *term;
	while ((void *)(term = terminals.next) != &terminals) destroy_terminal(term);
}

static void check_if_no_terminal(void)
{
	if (!list_empty(terminals)) return;
	terminate_loop = 1;
}

void set_char(struct terminal *t, int x, int y, unsigned ch, unsigned at)
{
	NO_GFX;
	t->dirty = 1;
	if (x >= 0 && x < t->x && y >= 0 && y < t->y) {
		chr *cc = &t->screen[x + t->x * y];
		cc->ch = ch;
		cc->at = at;
	}
}

chr *get_char(struct terminal *t, int x, int y)
{
	NO_GFX;
	if (x >= t->x) x = t->x - 1;
	if (x < 0) x = 0;
	if (y >= t->y) y = t->y - 1;
	if (y < 0) y = 0;
	return &t->screen[x + t->x * y];
}

void set_color(struct terminal *t, int x, int y, unsigned c)
{
	NO_GFX;
	t->dirty = 1;
	if (x >= 0 && x < t->x && y >= 0 && y < t->y) t->screen[x + t->x * y].at = (t->screen[x + t->x * y].at & ATTR_FRAME) | (c & ~ATTR_FRAME);
}

void set_only_char(struct terminal *t, int x, int y, unsigned ch, unsigned at)
{
	chr *cc;
	NO_GFX;
	t->dirty = 1;
	cc = get_char(t, x, y);
	at = (at & ATTR_FRAME) | (cc->at & ~ATTR_FRAME);
	set_char(t, x, y, ch, at);
}

void set_line(struct terminal *t, int x, int y, int l, chr *line)
{
	int i;
	chr *cc;
	NO_GFX;
	t->dirty = 1;
	if (y < 0 || y >= t->y) return;
	i = x >= 0 ? 0 : -x;
	cc = &t->screen[x+i + t->x * y];
	line = &line[i];
	i = (x+l <= t->x ? l : t->x-x) - i;
	if (i <= 0) return;
	memcpy(cc, line, i * sizeof(chr));
}

void set_line_color(struct terminal *t, int x, int y, int l, unsigned c)
{
	int i;
	NO_GFX;
	t->dirty = 1;
	if (y < 0 || y >= t->y) return;
	for (i = x >= 0 ? 0 : -x; i < (x+l <= t->x ? l : t->x-x); i++)
		t->screen[x+i + t->x * y].at = (t->screen[x+i + t->x * y].at & ATTR_FRAME) | (c & ~ATTR_FRAME);
}

void fill_area(struct terminal *t, int x, int y, int xw, int yw, unsigned ch, unsigned at)
{
	int i,j;
	NO_GFX;
	t->dirty = 1;
	for (j = y >= 0 ? 0 : -y; j < yw && y+j < t->y; j++) {
		chr *cc;
		i = x >= 0 ? 0 : -x;
		cc = &t->screen[x+i + t->x*(y+j)];
		for (; i < xw && x+i < t->x; i++) {
			cc->ch = ch;
			cc->at = at;
			cc++;
		}
	}
}

static int p1[] = { 218, 191, 192, 217, 179, 196 };
static int p2[] = { 201, 187, 200, 188, 186, 205 };

void draw_frame(struct terminal *t, int x, int y, int xw, int yw, unsigned c, int w)
{
	int *p = w > 1 ? p2 : p1;
	NO_GFX;
	c |= ATTR_FRAME;
	set_char(t, x, y, p[0], c);
	set_char(t, x+xw-1, y, p[1], c);
	set_char(t, x, y+yw-1, p[2], c);
	set_char(t, x+xw-1, y+yw-1, p[3], c);
	fill_area(t, x, y+1, 1, yw-2, p[4], c);
	fill_area(t, x+xw-1, y+1, 1, yw-2, p[4], c);
	fill_area(t, x+1, y, xw-2, 1, p[5], c);
	fill_area(t, x+1, y+yw-1, xw-2, 1, p[5], c);
}

void print_text(struct terminal *t, int x, int y, int l, unsigned char *text, unsigned c)
{
	NO_GFX;
	for (; l--; x++) {
		unsigned u = GET_TERM_CHAR(t, &text);
		if (!u) break;
		set_char(t, x, y, u, c);
	}
}

void set_cursor(struct terminal *term, int x, int y, int altx, int alty)
{
	NO_GFX;
	term->dirty = 1;
	if (term->spec->block_cursor && !term->spec->braille) x = altx, y = alty;
	if (x >= term->x) x = term->x - 1;
	if (y >= term->y) y = term->y - 1;
	if (x < 0) x = 0;
	if (y < 0) y = 0;
	term->cx = x;
	term->cy = y;
}

void exec_thread(unsigned char *path, int p)
{
#if defined(HAVE_SETPGID) && !defined(BEOS) && !defined(HAVE_BEGINTHREAD)
	if (path[0] == 2) setpgid(0, 0);
#endif
	exe(path + 1, path[0]);
	/*close(p);*/
	if (path[1 + strlen(path + 1) + 1]) unlink(path + 1 + strlen(path + 1) + 1);
}

void close_handle(void *p)
{
	long h = (my_intptr_t)p;
	close(h);
	set_handlers(h, NULL, NULL, NULL, NULL);
}

static void unblock_terminal(struct terminal *term)
{
	close_handle((void *)(my_intptr_t)term->blocked);
	term->blocked = -1;
	if (!F) {
		set_handlers(term->fdin, (void (*)(void *))in_term, NULL, (void (*)(void *))destroy_terminal, term);
		unblock_itrm(term->fdin);
		redraw_terminal_cls(term);
#ifdef G
	} else {
		drv->unblock(term->dev);
#endif
	}
}

void exec_on_terminal(struct terminal *term, unsigned char *path, unsigned char *delete, int fg)
{
	if (path && !*path) return;
	if (!path) path="";
#ifdef NO_FG_EXEC
	fg = 0;
#endif
	if (term->master) {
		if (!*path) {
			if (!F) dispatch_special(delete);
		} else {
			long blockh;
			unsigned char *param;
			if ((!F ? is_blocked() : term->blocked != -1) && fg) {
				unlink(delete);
				return;
			}
			param = mem_alloc(strlen(path) + strlen(delete) + 3);
			param[0] = fg;
			strcpy(param + 1, path);
			strcpy(param + 1 + strlen(path) + 1, delete);
			if (fg == 1) {
				if (!F) block_itrm(term->fdin);
#ifdef G
				else if (drv->block(term->dev)) {
					mem_free(param);
					unlink(delete);
					return;
				}
#endif
			}
			if ((blockh = start_thread((void (*)(void *, int))exec_thread, param, strlen(path) + strlen(delete) + 3)) == -1) {
				if (fg == 1) {
					if (!F) unblock_itrm(term->fdin);
#ifdef G
					else drv->unblock(term->dev);
#endif
				}
				mem_free(param);
				return;
			}
			mem_free(param);
			if (fg == 1) {
				term->blocked = blockh;
				set_handlers(blockh, (void (*)(void *))unblock_terminal, NULL, (void (*)(void *))unblock_terminal, term);
				if (!F) set_handlers(term->fdin, NULL, NULL, (void (*)(void *))destroy_terminal, term);
				/*block_itrm(term->fdin);*/
			} else {
				set_handlers(blockh, close_handle, NULL, close_handle, (void *)blockh);
			}
		}
	} else {
		unsigned char *data;
		data = mem_alloc(strlen(path) + strlen(delete) + 4);
		data[0] = 0;
		data[1] = fg;
		strcpy(data + 2, path);
		strcpy(data + 3 + strlen(path), delete);
		hard_write(term->fdout, data, strlen(path) + strlen(delete) + 4);
		mem_free(data);
		/*char x = 0;
		hard_write(term->fdout, &x, 1);
		x = fg;
		hard_write(term->fdout, &x, 1);
		hard_write(term->fdout, path, strlen(path) + 1);
		hard_write(term->fdout, delete, strlen(delete) + 1);*/
	}
}

void do_terminal_function(struct terminal *term, unsigned char code, unsigned char *data)
{
	unsigned char *x_data;
	NO_GFX;
	x_data = mem_alloc(strlen(data) + 2);
	x_data[0] = code;
	strcpy(x_data + 1, data);
	exec_on_terminal(term, NULL, x_data, 0);
	mem_free(x_data);
}

void set_terminal_title(struct terminal *term, unsigned char *title)
{
	int i;
	for (i = 0; i < 10000; i++) if (!title[i]) goto s;
	title[10000] = 0;
	s:
	if (strchr(title, 1)) {
		unsigned char *a, *b;
		for (a = title, b = title; *a; a++) if (*a != 1) *b++ = *a;
		*b = 0;
	}
	if (term->title && !strcmp(title, term->title)) goto ret;
	if (term->title) mem_free(term->title);
	term->title = stracpy(title);
#ifdef SET_WINDOW_TITLE_UTF_8
	{
		struct conv_table *table;
		mem_free(title);
		table = get_translation_table(term->spec->charset, get_cp_index("utf-8"));
		title = convert_string(table, term->title, strlen(term->title), NULL);
	}
#endif
	if (!F) do_terminal_function(term, TERM_FN_TITLE, title);
#ifdef G
	else if (drv->set_title) drv->set_title(term->dev, title);
#endif
	ret:
	mem_free(title);
}
