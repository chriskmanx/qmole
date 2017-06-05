/* session.c
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "links.h"

struct list_head downloads = {&downloads, &downloads};

/* prototypes */
static void increase_download_file(unsigned char **f);
static struct location *new_location(void);
static int get_file_by_term(struct terminal *term, struct cache_entry *ce, unsigned char **start, unsigned char **end, int *err);
static void refresh_timer(struct f_data_c *);
static void destroy_location(struct location *loc);


int are_there_downloads(void)
{
	int d = 0;
	struct download *down;
	foreach(down, downloads) if (!down->prog) d = 1;
	return d;
}

struct list_head sessions = {&sessions, &sessions};

struct strerror_val {
	struct strerror_val *next;
	struct strerror_val *prev;
	unsigned char msg[1];
};

static struct list_head strerror_buf = { &strerror_buf, &strerror_buf };

void free_strerror_buf(void)
{
	free_list(strerror_buf);
}

int get_error_from_errno(int errn)
{
	if (errn > 0 && (errn < -S_OK || errn > -S_MAX))
		return -errn;
#ifdef BEOS
	if (-errn > 0 && (-errn < -S_OK || -errn > -S_MAX))
		return errn;
#endif
	return S_UNKNOWN_ERROR;
}

unsigned char *get_err_msg(int state)
{
	unsigned char *e;
	struct strerror_val *s;
	if ((state >= S_MAX && state <= S_OK) || state >= S_WAIT) {
		int i;
		for (i = 0; msg_dsc[i].msg; i++)
			if (msg_dsc[i].n == state) return msg_dsc[i].msg;
		unk:
		return TEXT_(T_UNKNOWN_ERROR);
	}
#ifdef BEOS
	if ((e = strerror(state)) && *e && !strstr(e, "No Error")) goto have_error;
#endif
	if ((e = strerror(-state)) && *e) goto have_error;
	goto unk;
have_error:
	foreach(s, strerror_buf) if (!strcmp(s->msg, e)) return s->msg;
	s = mem_alloc(sizeof(struct strerror_val) + strlen(e));
	strcpy(s->msg, e);
	add_to_list(strerror_buf, s);
	return s->msg;
}

static void add_xnum_to_str(unsigned char **s, int *l, off_t n)
{
	unsigned char suff = 0;
	int d = -1;
	if (n >= 1000000000) suff = 'G', d = (n / 100000000) % 10, n /= 1000000000;
	else if (n >= 1000000) suff = 'M', d = (n / 100000) % 10, n /= 1000000;
	else if (n >= 1000) suff = 'k', d = (n / 100) % 10, n /= 1000;
	add_num_to_str(s, l, n);
	if (n < 10 && d != -1) add_chr_to_str(s, l, '.'), add_num_to_str(s, l, d);
	add_chr_to_str(s, l, ' ');
	if (suff) add_chr_to_str(s, l, suff);
	add_chr_to_str(s, l, 'B');
}

static void add_time_to_str(unsigned char **s, int *l, ttime t)
{
	unsigned char q[64];
	if (t < 0) t = 0;
	t &= 0xffffffff;
	if (t >= 86400) sprintf(q, "%dd ", (int)(t / 86400)), add_to_str(s, l, q);
	if (t >= 3600) t %= 86400, sprintf(q, "%d:%02d", (int)(t / 3600), (int)(t / 60 % 60)), add_to_str(s, l, q);
	else sprintf(q, "%d", (int)(t / 60)), add_to_str(s, l, q);
	sprintf(q, ":%02d", (int)(t % 60)), add_to_str(s, l, q);
}

static unsigned char *get_stat_msg(struct status *stat, struct terminal *term)
{
	if (stat->state == S_TRANS && stat->prg->elapsed / 100) {
		unsigned char *m = init_str();
		int l = 0;
		add_to_str(&m, &l, _(TEXT_(T_RECEIVED), term));
		add_to_str(&m, &l, " ");
		add_xnum_to_str(&m, &l, stat->prg->pos);
		if (stat->prg->size >= 0)
			add_to_str(&m, &l, " "), add_to_str(&m, &l, _(TEXT_(T_OF), term)), add_to_str(&m, &l, " "), add_xnum_to_str(&m, &l, stat->prg->size);
		add_to_str(&m, &l, ", ");
		if (stat->prg->elapsed >= CURRENT_SPD_AFTER * SPD_DISP_TIME)
			add_to_str(&m, &l, _(TEXT_(T_AVG), term)), add_to_str(&m, &l, " ");
		add_xnum_to_str(&m, &l, stat->prg->loaded * 10 / (stat->prg->elapsed / 100));
		add_to_str(&m, &l, "/s");
		if (stat->prg->elapsed >= CURRENT_SPD_AFTER * SPD_DISP_TIME)
			add_to_str(&m, &l, ", "), add_to_str(&m, &l, _(TEXT_(T_CUR), term)), add_to_str(&m, &l, " "),
			add_xnum_to_str(&m, &l, stat->prg->cur_loaded / (CURRENT_SPD_SEC * SPD_DISP_TIME / 1000)),
			add_to_str(&m, &l, "/s");
		return m;
	}
	return stracpy(_(get_err_msg(stat->state), term));
}

void change_screen_status(struct session *ses)
{
	struct status *stat = NULL;
	if (ses->rq) {
		stat = &ses->rq->stat;
	} else {
		struct f_data_c *fd = current_frame(ses);
		if (fd->rq) stat = &fd->rq->stat;
		if (stat && stat->state == S_OK && fd->af) {
			struct additional_file *af;
			foreach(af, fd->af->af) {
				if (af->rq && af->rq->stat.state >= 0) {
					if (af->rq->stat.state > stat->state ||
					    (af->rq->stat.state == S_TRANS &&
					     stat->state == S_TRANS &&
					     af->rq->stat.prg->pos >
					     stat->prg->pos))
						stat = &af->rq->stat;
				}
			}
		}
	}
	if (ses->st) mem_free(ses->st);

	/* default status se ukazuje, kdyz
	 *			a) by se jinak ukazovalo prazdno
	 *			b) neni NULL a ukazovalo by se OK
	 */
	ses->st = NULL;
	if (stat) {
		if (stat->state == S_OK) ses->st = print_current_link(ses);
		if (!ses->st) ses->st = ses->default_status ? stracpy(ses->default_status) : get_stat_msg(stat, ses->term);
	} else {
		ses->st = stracpy(ses->default_status);
	}
}

static void x_print_screen_status(struct terminal *term, struct session *ses)
{
	unsigned char *m;
	if (!F) {
		fill_area(term, 0, term->y - 1, term->x, 1, ' ', COLOR_STATUS_BG);
		if (ses->st) print_text(term, 0, term->y - 1, strlen(ses->st), ses->st, COLOR_STATUS);
		fill_area(term, 0, 0, term->x, 1, ' ', COLOR_TITLE_BG);
		if ((m = print_current_title(ses))) {
			int p = term->x - 1 - cp_len(ses->term->spec->charset, m);
			if (p < 0) p = 0;
			if (term->spec->braille) p = 0;
			print_text(term, p, 0, cp_len(ses->term->spec->charset, m), m, COLOR_TITLE);
			mem_free(m);
		}
#ifdef G
	} else {
		int l = 0;
		if (ses->st) g_print_text(drv, term->dev, 0, term->y - G_BFU_FONT_SIZE, bfu_style_wb_mono, ses->st, &l);
		drv->fill_area(term->dev, l, term->y - G_BFU_FONT_SIZE, term->x, term->y, bfu_bg_color);
#endif
	}
}

void print_screen_status(struct session *ses)
{
	unsigned char *m;
#ifdef G
	if (F) {
		/*debug("%s - %s", ses->st_old, ses->st);
		debug("clip: %d.%d , %d.%d", ses->term->dev->clip.x1, ses->term->dev->clip.y1, ses->term->dev->clip.x2, ses->term->dev->clip.y2);
		debug("size: %d.%d , %d.%d", ses->term->dev->size.x1, ses->term->dev->size.y1, ses->term->dev->size.x2, ses->term->dev->size.y2);*/
		if (ses->st_old) {
			if (ses->st && !strcmp(ses->st, ses->st_old)) goto skip_status;
			mem_free(ses->st_old);
			ses->st_old = NULL;
		}
		if (!memcmp(&ses->term->dev->clip, &ses->term->dev->size, sizeof(struct rect))) ses->st_old = stracpy(ses->st);
	}
#endif
	draw_to_window(ses->win, (void (*)(struct terminal *, void *))x_print_screen_status, ses);
#ifdef G
	skip_status:
#endif
	if ((m = stracpy("Links"))) {
		if (ses->screen && ses->screen->f_data && ses->screen->f_data->title && ses->screen->f_data->title[0]) add_to_strn(&m, " - "), add_to_strn(&m, ses->screen->f_data->title);
		set_terminal_title(ses->term, m);
		/*mem_free(m); -- set_terminal_title frees it */
	}
	if (!F && ses->brl_cursor_mode) {
		if (ses->brl_cursor_mode == 1) set_cursor(ses->term, 0, 0, 0, 0);
		if (ses->brl_cursor_mode == 2) set_cursor(ses->term, 0, ses->term->y - 1, 0, ses->term->y - 1);
	}
}

void print_error_dialog(struct session *ses, struct status *stat, unsigned char *title)
{
	unsigned char *t = get_err_msg(stat->state);
	unsigned char *u = stracpy(title);
	unsigned char *pc;
	if ((pc = strchr(u, POST_CHAR))) *pc = 0;
	if (!t) return;
	msg_box(ses->term, getml(u, NULL), TEXT_(T_ERROR), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_ERROR_LOADING), " ", u, ":\n\n", t, NULL, ses, 1, TEXT_(T_CANCEL), NULL, B_ENTER | B_ESC/*, _("Retry"), NULL, 0 !!! FIXME: retry */);
}

static inline unsigned char hx(int a)
{
	return a >= 10 ? a + 'A' - 10 : a + '0';
}

static inline int unhx(unsigned char a)
{
	if (a >= '0' && a <= '9') return a - '0';
	if (a >= 'A' && a <= 'F') return a - 'A' + 10;
	if (a >= 'a' && a <= 'f') return a - 'a' + 10;
	return -1;
}

unsigned char *encode_url(unsigned char *url)
{
	unsigned char *u = init_str();
	int l = 0;
	add_to_str(&u, &l, "+++");
	for (; *url; url++) {
		if (is_safe_in_shell(*url) && *url != '+') add_chr_to_str(&u, &l, *url);
		else add_chr_to_str(&u, &l, '+'), add_chr_to_str(&u, &l, hx(*url >> 4)), add_chr_to_str(&u, &l, hx(*url & 0xf));
	}
	return u;
}

unsigned char *decode_url(unsigned char *url)
{
	unsigned char *u;
	int l;
	if (casecmp(url, "+++", 3)) return stracpy(url);
	url += 3;
	u = init_str();
	l = 0;
	for (; *url; url++) {
		if (*url != '+' || unhx(url[1]) == -1 || unhx(url[2]) == -1) add_chr_to_str(&u, &l, *url);
		else add_chr_to_str(&u, &l, (unhx(url[1]) << 4) + unhx(url[2])), url += 2;
	}
	return u;
}

static struct session *get_download_ses(struct download *down)
{
	struct session *ses;
	foreach(ses, sessions) if (ses == down->ses) return ses;
	if (!list_empty(sessions)) return sessions.next;
	return NULL;
}

static void close_download_file(struct download *down)
{
	if (down->handle != -1) prealloc_truncate(down->handle, down->last_pos - down->file_shift), close(down->handle), down->handle = -1;
}

static void delete_download_file(struct download *down)
{
	unsigned char *file = stracpy(down->orig_file);
	unsigned char *wd = get_cwd();
	set_cwd(down->cwd);
	while (1) {
		unsigned char *f = translate_download_file(file);
		unlink(f);
		mem_free(f);
		if (!strcmp(file, down->file)) break;
		increase_download_file(&file);
	}
	mem_free(file);
	if (wd) set_cwd(wd), mem_free(wd);
}

static void abort_download(struct download *down)
{
	if (down->win) delete_window(down->win);
	if (down->ask) delete_window(down->ask);
	if (down->stat.state >= 0) change_connection(&down->stat, NULL, PRI_CANCEL);
	mem_free(down->url);
	close_download_file(down);
	if (down->prog) {
		delete_download_file(down);

		mem_free(down->prog);
	}
	mem_free(down->cwd);
	mem_free(down->orig_file);
	mem_free(down->file);
	del_from_list(down);
	mem_free(down);
}

static void abort_and_delete_download(struct download *down)
{
	if (!down->prog) down->prog = DUMMY;
	abort_download(down);
}

int test_abort_downloads_to_file(unsigned char *file, unsigned char *cwd, int abort_downloads)
{
	int ret = 0;
	struct download *down;
	foreach(down, downloads) {
		if (strcmp(down->cwd, cwd)) {
#if defined(DOS_FS)
			if (file[0] && file[1] == ':' && dir_sep(file[2])) goto abs;
#elif defined(SPAD)
			if (_is_absolute(file) == _ABS_TOTAL) goto abs;
#else
			if (file[0] == '/') goto abs;
#endif
			continue;
		}
		abs:
		if (!strcmp(down->file, file) || !strcmp(down->orig_file, file)) {
			ret = 1;
			if (!abort_downloads) break;
			down = down->prev;
			abort_download(down->next);
		}
	}
	return ret;
}

static void undisplay_download(struct download *down)
{
	if (down->win) delete_window(down->win);
}

static int dlg_abort_download(struct dialog_data *dlg, struct dialog_item_data *di)
{
	register_bottom_half((void (*)(void *))abort_download, dlg->dlg->udata);
	return 0;
}

static int dlg_abort_and_delete_download(struct dialog_data *dlg, struct dialog_item_data *di)
{
	register_bottom_half((void (*)(void *))abort_and_delete_download, dlg->dlg->udata);
	return 0;
}

static int dlg_undisplay_download(struct dialog_data *dlg, struct dialog_item_data *di)
{
	register_bottom_half((void (*)(void *))undisplay_download, dlg->dlg->udata);
	return 0;
}

static void download_abort_function(struct dialog_data *dlg)
{
	struct download *down = dlg->dlg->udata;
	down->win = NULL;
}

void download_window_function(struct dialog_data *dlg)
{
	struct download *down = dlg->dlg->udata;
	struct terminal *term = dlg->win->term;
	int max = 0, min = 0;
	int w, x, y;
	int t = 0;
	unsigned char *m, *u;
	struct status *stat = &down->stat;
	if (!F) redraw_below_window(dlg->win);
	down->win = dlg->win;
	if (stat->state == S_TRANS && stat->prg->elapsed / 100) {
		int l = 0;
		m = init_str();
		t = 1;
		add_to_str(&m, &l, _(TEXT_(T_RECEIVED), term));
		add_to_str(&m, &l, " ");
		add_xnum_to_str(&m, &l, stat->prg->pos);
		if (stat->prg->size >= 0)
			add_to_str(&m, &l, " "), add_to_str(&m, &l, _(TEXT_(T_OF),term)), add_to_str(&m, &l, " "), add_xnum_to_str(&m, &l, stat->prg->size), add_to_str(&m, &l, " ");
		add_to_str(&m, &l, "\n");
		if (stat->prg->elapsed >= CURRENT_SPD_AFTER * SPD_DISP_TIME)
			add_to_str(&m, &l, _(TEXT_(T_AVERAGE_SPEED), term));
		else add_to_str(&m, &l, _(TEXT_(T_SPEED), term));
		add_to_str(&m, &l, " ");
		add_xnum_to_str(&m, &l, (longlong)stat->prg->loaded * 10 / (stat->prg->elapsed / 100));
		add_to_str(&m, &l, "/s");
		if (stat->prg->elapsed >= CURRENT_SPD_AFTER * SPD_DISP_TIME)
			add_to_str(&m, &l, ", "), add_to_str(&m, &l, _(TEXT_(T_CURRENT_SPEED), term)), add_to_str(&m, &l, " "),
			add_xnum_to_str(&m, &l, stat->prg->cur_loaded / (CURRENT_SPD_SEC * SPD_DISP_TIME / 1000)),
			add_to_str(&m, &l, "/s");
		add_to_str(&m, &l, "\n");
		add_to_str(&m, &l, _(TEXT_(T_ELAPSED_TIME), term));
		add_to_str(&m, &l, " ");
		add_time_to_str(&m, &l, stat->prg->elapsed / 1000);
		if (stat->prg->size >= 0 && stat->prg->loaded > 0) {
			add_to_str(&m, &l, ", ");
			add_to_str(&m, &l, _(TEXT_(T_ESTIMATED_TIME), term));
			add_to_str(&m, &l, " ");
			/*add_time_to_str(&m, &l, stat->prg->elapsed / 1000 * stat->prg->size / stat->prg->loaded * 1000 - stat->prg->elapsed);*/
			add_time_to_str(&m, &l, (stat->prg->size - stat->prg->pos) / ((longlong)stat->prg->loaded * 10 / (stat->prg->elapsed / 100)));
		}
	} else m = stracpy(_(get_err_msg(stat->state), term));
	u = stracpy(down->url);
	if (strchr(u, POST_CHAR)) *strchr(u, POST_CHAR) = 0;
	max_text_width(term, u, &max, AL_LEFT);
	min_text_width(term, u, &min, AL_LEFT);
	max_text_width(term, m, &max, AL_LEFT);
	min_text_width(term, m, &min, AL_LEFT);
	max_buttons_width(term, dlg->items, dlg->n, &max);
	min_buttons_width(term, dlg->items, dlg->n, &min);
	w = dlg->win->term->x * 9 / 10 - 2 * DIALOG_LB;
	if (w < min) w = min;
	if (!dlg->win->term->spec->braille && w > dlg->win->term->x - 2 * DIALOG_LB) w = dlg->win->term->x - 2 * DIALOG_LB;
	if (t && stat->prg->size >= 0) {
		if (w < DOWN_DLG_MIN) w = DOWN_DLG_MIN;
	} else {
		if (w > max) w = max;
	}
	if (w < 1) w = 1;
	y = 0;
	dlg_format_text(dlg, NULL, u, 0, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE);
	if (t && stat->prg->size >= 0) y += gf_val(2, 2 * G_BFU_FONT_SIZE);
	dlg_format_text(dlg, NULL, m, 0, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_buttons(dlg, NULL, dlg->items, dlg->n, 0, &y, w, NULL, AL_CENTER);
	dlg->xw = w + 2 * DIALOG_LB;
	dlg->yw = y + 2 * DIALOG_TB;
	center_dlg(dlg);
	draw_dlg(dlg);
	y = dlg->y + DIALOG_TB + gf_val(1, G_BFU_FONT_SIZE);
	x = dlg->x + DIALOG_LB;
	dlg_format_text(dlg, term, u, x, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	if (t && stat->prg->size >= 0) {
		if (!F) {
			unsigned char q[64];
			int p = w - 6;
			if (term->spec->braille && p > 39 - 6) p = 39 - 6;
			y++;
			set_only_char(term, x, y, '[', 0);
			set_only_char(term, x + p + 1, y, ']', 0);
			fill_area(term, x + 1, y, (int)((longlong)p * (longlong)stat->prg->pos / (longlong)stat->prg->size), 1, CHAR_DIALOG_METER, COLOR_DIALOG_METER);
			sprintf(q, "%3d%%", (int)((longlong)100 * (longlong)stat->prg->pos / (longlong)stat->prg->size));
			print_text(term, x + p + 2, y, strlen(q), q, COLOR_DIALOG_TEXT);
			y++;
#ifdef G
		} else {
			unsigned char q[64];
			int p, s, ss, m;
			y += G_BFU_FONT_SIZE;
			sprintf(q, "]%3d%%", (int)((longlong)100 * (longlong)stat->prg->pos / (longlong)stat->prg->size));
			s = g_text_width(bfu_style_bw_mono, "[");
			ss = g_text_width(bfu_style_bw_mono, q);
			p = w - s - ss;
			if (p < 0) p = 0;
			m = (int)((longlong)p * (longlong)stat->prg->pos / (longlong)stat->prg->size);
			g_print_text(drv, term->dev, x, y, bfu_style_bw_mono, "[", NULL);
			drv->fill_area(term->dev, x + s, y, x + s + m, y + G_BFU_FONT_SIZE, bfu_fg_color);
			drv->fill_area(term->dev, x + s + m, y, x + s + p, y + G_BFU_FONT_SIZE, bfu_bg_color);
			g_print_text(drv, term->dev, x + w - ss, y, bfu_style_bw_mono, q, NULL);
			if (dlg->s) exclude_from_set(&dlg->s, x, y, x + w, y + G_BFU_FONT_SIZE);
			y += G_BFU_FONT_SIZE;
#endif
		}
	}
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_text(dlg, term, m, x, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_buttons(dlg, term, dlg->items, dlg->n, x, &y, w, NULL, AL_CENTER);
	mem_free(u);
	mem_free(m);
}

void display_download(struct terminal *term, struct download *down, struct session *ses)
{
	struct dialog *dlg;
	struct download *dd;
	foreach(dd, downloads) if (dd == down) goto found;
	return;
	found:
	dlg = mem_calloc(sizeof(struct dialog) + 4 * sizeof(struct dialog_item));
	undisplay_download(down);
	down->ses = ses;
	dlg->title = TEXT_(T_DOWNLOAD);
	dlg->fn = download_window_function;
	dlg->abort = download_abort_function;
	dlg->udata = down;
	dlg->align = AL_CENTER;
	dlg->items[0].type = D_BUTTON;
	dlg->items[0].gid = B_ENTER | B_ESC;
	dlg->items[0].fn = dlg_undisplay_download;
	dlg->items[0].text = TEXT_(T_BACKGROUND);
	dlg->items[1].type = D_BUTTON;
	dlg->items[1].gid = 0;
	dlg->items[1].fn = dlg_abort_download;
	dlg->items[1].text = TEXT_(T_ABORT);
	if (!down->prog) {
		dlg->items[2].type = D_BUTTON;
		dlg->items[2].gid = 0;
		dlg->items[2].fn = dlg_abort_and_delete_download;
		dlg->items[2].text = TEXT_(T_ABORT_AND_DELETE_FILE);
		dlg->items[3].type = D_END;
	} else {
		dlg->items[2].type = D_END;
	}
	do_dialog(term, dlg, getml(dlg, NULL));
}

time_t parse_http_date(unsigned char *date)	/* this functions is bad !!! */
{
	static unsigned char *months[12] =
		{"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

	time_t t = 0;
	/* Mon, 03 Jan 2000 21:29:33 GMT */
	int y;
	struct tm tm;
	memset(&tm, 0, sizeof(struct tm));

	date = strchr(date, ' ');
	if (!date) return 0;
	date++;
	if (*date >= '0' && *date <= '9') {
			/* Sun, 06 Nov 1994 08:49:37 GMT */
			/* Sunday, 06-Nov-94 08:49:37 GMT */
		y = 0;
		if (date[0] < '0' || date[0] > '9') return 0;
		if (date[1] < '0' || date[1] > '9') return 0;
		tm.tm_mday = (date[0] - '0') * 10 + date[1] - '0';
		date += 2;
		if (*date != ' ' && *date != '-') return 0;
		date += 1;
		for (tm.tm_mon = 0; tm.tm_mon < 12; tm.tm_mon++)
			if (!casecmp(date, months[tm.tm_mon], 3)) goto f1;
		return 0;
		f1:
		date += 3;
		if (*date == ' ') {
				/* Sun, 06 Nov 1994 08:49:37 GMT */
			date++;
			if (date[0] < '0' || date[0] > '9') return 0;
			if (date[1] < '0' || date[1] > '9') return 0;
			if (date[2] < '0' || date[2] > '9') return 0;
			if (date[3] < '0' || date[3] > '9') return 0;
			tm.tm_year = (date[0] - '0') * 1000 + (date[1] - '0') * 100 + (date[2] - '0') * 10 + date[3] - '0' - 1900;
			date += 4;
		} else if (*date == '-') {
				/* Sunday, 06-Nov-94 08:49:37 GMT */
			date++;
			if (date[0] < '0' || date[0] > '9') return 0;
			if (date[1] < '0' || date[1] > '9') return 0;
			tm.tm_year = (date[0] >= '7' ? 1900 : 2000) + (date[0] - '0') * 10 + date[1] - '0' - 1900;
			date += 2;
		} else return 0;
		if (*date != ' ') return 0;
		date++;
	} else {
			/* Sun Nov  6 08:49:37 1994 */
		y = 1;
		for (tm.tm_mon = 0; tm.tm_mon < 12; tm.tm_mon++)
			if (!casecmp(date, months[tm.tm_mon], 3)) goto f2;
		return 0;
		f2:
		date += 3;
		while (*date == ' ') date++;
		if (date[0] < '0' || date[0] > '9') return 0;
		tm.tm_mday = date[0] - '0';
		date++;
		if (*date != ' ') {
			if (date[0] < '0' || date[0] > '9') return 0;
			tm.tm_mday = tm.tm_mday * 10 + date[0] - '0';
			date++;
		}
		if (*date != ' ') return 0;
		date++;
	}

	if (date[0] < '0' || date[0] > '9') return 0;
	if (date[1] < '0' || date[1] > '9') return 0;
	tm.tm_hour = (date[0] - '0') * 10 + date[1] - '0';
	date += 2;
	if (*date != ':') return 0;
	date++;
	if (date[0] < '0' || date[0] > '9') return 0;
	if (date[1] < '0' || date[1] > '9') return 0;
	tm.tm_min = (date[0] - '0') * 10 + date[1] - '0';
	date += 2;
	if (*date != ':') return 0;
	date++;
	if (date[0] < '0' || date[0] > '9') return 0;
	if (date[1] < '0' || date[1] > '9') return 0;
	tm.tm_sec = (date[0] - '0') * 10 + date[1] - '0';
	date += 2;
	if (y) {
		if (*date != ' ') return 0;
		date++;
		if (date[0] < '0' || date[0] > '9') return 0;
		if (date[1] < '0' || date[1] > '9') return 0;
		if (date[2] < '0' || date[2] > '9') return 0;
		if (date[3] < '0' || date[3] > '9') return 0;
		tm.tm_year = (date[0] - '0') * 1000 + (date[1] - '0') * 100 + (date[2] - '0') * 10 + date[3] - '0' - 1900;
		date += 4;
	}
	if (*date != ' ' && *date) return 0;

	t = mktime(&tm);
	if (t == (time_t) -1) return 0;
	return t;
}

static int download_write(struct download *down, void *ptr, off_t to_write)
{
	int w;
	int err;
	if (to_write != (int)to_write || (int)to_write < 0) to_write = MAXINT;
	try_write_again:
	w = hard_write(down->handle, ptr, to_write);
	if (w >= 0) err = 0;
	else err = errno;
	if (w <= -!to_write) {
#ifdef EFBIG
		if (err == EFBIG && !down->prog) {
			if (to_write > 1) {
				to_write >>= 1;
				goto try_write_again;
			}
			if (down->last_pos == down->file_shift) goto no_e2big;
			close_download_file(down);
			increase_download_file(&down->file);
			if ((down->handle = create_download_file(get_download_ses(down), down->cwd, down->file, 0, down->last_pos - down->file_shift)) < 0) return -1;
			down->file_shift = down->last_pos;
			goto try_write_again;
			no_e2big:;
		}
#endif
		if (get_download_ses(down)) {
			unsigned char *emsg = stracpy(err ? strerror(err) : "Zero returned");
			unsigned char *msg = stracpy(down->file);
			msg_box(get_download_ses(down)->term, getml(msg, emsg, NULL), TEXT_(T_DOWNLOAD_ERROR), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_COULD_NOT_WRITE_TO_FILE), " ", msg, ": ", emsg, NULL, NULL, 1, TEXT_(T_CANCEL), NULL, B_ENTER | B_ESC);
		}
		return -1;
	}
	down->last_pos += w;
	return 0;
}

static void download_data(struct status *stat, struct download *down)
{
	struct cache_entry *ce;
	struct fragment *frag;
	if (!(ce = stat->ce)) goto end_store;
	if (stat->state >= S_WAIT && stat->state < S_TRANS) goto end_store;
	if (!down->remotetime && ce->last_modified) down->remotetime = parse_http_date(ce->last_modified);
/*	  fprintf(stderr,"\nFEFE date %s\n",ce->last_modified); */
	if (!down->last_pos) {
		unsigned char *enc = get_content_encoding(ce->head, ce->url);
		if (enc) {
			if (!encoding_2_extension(enc)) down->decompress = 1;
			mem_free(enc);
		}
		if (ce->redirect) {
			if (down->redirect_cnt++ < MAX_REDIRECTS) {
				unsigned char *u, *p, *pos;
				unsigned char *prev_down_url;
				int cache;
				if (stat->state >= 0) change_connection(&down->stat, NULL, PRI_CANCEL);
				u = join_urls(down->url, ce->redirect);
				if (!u) goto x;
				if ((pos = extract_position(u))) mem_free(pos);
				if (!http_options.bug_302_redirect) if (!ce->redirect_get && (p = strchr(down->url, POST_CHAR))) add_to_strn(&u, p);
				prev_down_url = down->url;
				down->url = u;
				down->stat.state = S_WAIT_REDIR;
				if (down->win) {
					struct event ev = { EV_REDRAW, 0, 0, 0 };
					ev.x = down->win->term->x;
					ev.y = down->win->term->y;
					down->win->handler(down->win, &ev, 0);
				}
				cache = NC_CACHE;
				if (!strcmp(down->url, prev_down_url) || down->redirect_cnt >= MAX_CACHED_REDIRECTS) cache = NC_RELOAD;
				mem_free(prev_down_url);
				load_url(down->url, NULL, &down->stat, PRI_DOWNLOAD, cache);
				return;
			} else {
				if (stat->state >= 0) change_connection(&down->stat, NULL, PRI_CANCEL);
				stat->state = S_CYCLIC_REDIRECT;
				goto end_store;
			}
		}
	}
	x:
	if (!down->decompress) foreach(frag, ce->frag) while (frag->offset <= down->last_pos && frag->offset + frag->length > down->last_pos) {
#ifdef HAVE_OPEN_PREALLOC
		if (!down->last_pos && (!down->stat.prg || down->stat.prg->size > 0)) {
			struct stat st;
			if (fstat(down->handle, &st) || !S_ISREG(st.st_mode)) goto skip_prealloc;
			close_download_file(down);
			delete_download_file(down);
			if ((down->handle = create_download_file(get_download_ses(down), down->cwd, down->file, !!down->prog, down->stat.prg ? down->stat.prg->size : ce->length)) < 0) goto det_abt;
			skip_prealloc:;
		}
#endif
		if (download_write(down, frag->data + (down->last_pos - frag->offset), frag->length - (down->last_pos - frag->offset))) {
			det_abt:
			detach_connection(stat, down->last_pos);
			abort_download(down);
			return;
		}
	}
	detach_connection(stat, down->last_pos);
	end_store:;
	if (stat->state < 0) {
		if (down->decompress) {
			struct session *ses = get_download_ses(down);
			unsigned char *start, *end;
			int err;
			get_file_by_term(ses ? ses->term : NULL, ce, &start, &end, &err);
			if (err) goto det_abt;
			while (down->last_pos < end - start) {
				if (download_write(down, start + down->last_pos, end - start - down->last_pos)) goto det_abt;
			}
		}
		if (stat->state != S_OK) {
			unsigned char *t = get_err_msg(stat->state);
			if (t) {
				unsigned char *tt = stracpy(down->url);
				if (strchr(tt, POST_CHAR)) *strchr(tt, POST_CHAR) = 0;
				msg_box(get_download_ses(down)->term, getml(tt, NULL), TEXT_(T_DOWNLOAD_ERROR), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_ERROR_DOWNLOADING), " ", tt, ":\n\n", t, NULL, get_download_ses(down), 1, TEXT_(T_CANCEL), NULL, B_ENTER | B_ESC/*, TEXT_(T_RETRY), NULL, 0 !!! FIXME: retry */);
			}
		} else {
			if (down->prog) {
				close_download_file(down);
				exec_on_terminal(get_download_ses(down)->term, down->prog, down->orig_file, !!down->prog_flag_block);
				mem_free(down->prog), down->prog = NULL;
			} else if (down->remotetime && download_utime) {
				struct utimbuf foo;
				unsigned char *file = stracpy(down->orig_file);
				unsigned char *wd = get_cwd();
				set_cwd(down->cwd);
				foo.actime = foo.modtime = down->remotetime;
				while (1) {
					unsigned char *f = translate_download_file(file);
					utime(file, &foo);
					mem_free(f);
					if (!strcmp(file, down->file)) break;
					increase_download_file(&file);
				}
				mem_free(file);
				if (wd) set_cwd(wd), mem_free(wd);
			}
		}
		abort_download(down);
		return;
	}
	if (down->win) {
		struct event ev = { EV_REDRAW, 0, 0, 0 };
		ev.x = down->win->term->x;
		ev.y = down->win->term->y;
		down->win->handler(down->win, &ev, 0);
	}
}

unsigned char *translate_download_file(unsigned char *fi)
{
	unsigned char *file = stracpy("");
	unsigned char *h;
	if (fi[0] == '~' && dir_sep(fi[1]) && (h = getenv("HOME"))) {
		add_to_strn(&file, h);
		fi++;
	}
	add_to_strn(&file, fi);
	return file;
}

int create_download_file(struct session *ses, unsigned char *cwd, unsigned char *fi, int safe, off_t siz)
{
	unsigned char *wd;
	unsigned char *file;
	int h;
#ifdef NO_FILE_SECURITY
	int sf = 0;
#else
	int sf = safe;
#endif
	wd = get_cwd();
	set_cwd(cwd);
	file = translate_download_file(fi);
#ifdef HAVE_OPEN_PREALLOC
	if (siz) {
		h = open_prealloc(file, O_CREAT|O_WRONLY|O_TRUNC|(safe?O_EXCL:0), sf ? 0600 : 0666, siz);
	} else
#endif
	{
		h = open(file, O_CREAT|O_WRONLY|O_TRUNC|(safe?O_EXCL:0), sf ? 0600 : 0666);
	}
	if (h == -1) {
		unsigned char *msg, *msge;
		int errn = errno;
		if (errn == EEXIST && safe) {
			h = -2;
			goto x;
		}
		if (!ses) goto x;
		msg = stracpy(file);
		msge = stracpy(strerror(errn));
		msg_box(ses->term, getml(msg, msge, NULL), TEXT_(T_DOWNLOAD_ERROR), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_COULD_NOT_CREATE_FILE), " ", msg, ": ", msge, NULL, NULL, 1, TEXT_(T_CANCEL), NULL, B_ENTER | B_ESC);
		goto x;
	}
	set_bin(h);
	x:
	mem_free(file);
	if (wd) set_cwd(wd), mem_free(wd);
	return h;
}

static void increase_download_file(unsigned char **f)
{
	unsigned char *p = NULL, *pp = *f;
	unsigned char *q;
	while ((pp = strstr(pp, ".part-"))) p = pp += 6;
	if (!p || !*p) {
		no_suffix:
		add_to_strn(f, ".part-2");
		return;
	}
	for (q = p; *q; q++) if (*q < '0' || *q > '9') goto no_suffix;
	for (q--; q >= p; q--) {
		if (*q < '9') {
			(*q)++;
			return;
		}
		*q = '0';
	}
	*p = '1';
	add_to_strn(f, "0");
}

static unsigned char *get_temp_name(unsigned char *url, unsigned char *head)
{
	int nl;
	unsigned char *name, *fn, *fnx;
	unsigned char *nm;
	unsigned char *directory = NULL;
#ifdef WIN32
	directory = getenv("TMP");
	if (!directory) directory = getenv("TEMP");
#endif
	nm = tempnam(directory, "links");
	if (!nm) return NULL;
#ifdef OS2
	if (strlen(nm) > 4 && !strcasecmp(nm + strlen(nm) - 4, ".tmp")) nm[strlen(nm) - 4] = 0;
#endif
	name = init_str();
	nl = 0;
	add_to_str(&name, &nl, nm);
	free(nm);
	fn = get_filename_from_url(url, head, 1);
#ifndef OS2
	fnx = strchr(fn, '.');
#else
	fnx = strrchr(fn, '.');
#endif
	if (fnx) {
		unsigned char *s;
#ifdef OS2
		if (strlen(fnx) > 4) fnx[4] = 0;
#endif
		s = stracpy(fnx);
		check_shell_security(&s);
		add_to_str(&name, &nl, s);
		mem_free(s);
	}
	mem_free(fn);
	return name;
}

unsigned char *subst_file(unsigned char *prog, unsigned char *file, int cyg_subst)
{
	unsigned char *orig_prog = prog;
	unsigned char *nn;
	unsigned char *n = init_str();
	int l = 0;
	while (*prog) {
		int p;
		for (p = 0; prog[p] && prog[p] != '%'; p++)
			;
		add_bytes_to_str(&n, &l, prog, p);
		prog += p;
		if (*prog == '%') {
			if (cyg_subst) {
				unsigned char *conv = os_conv_to_external_path(file, orig_prog);
				add_to_str(&n, &l, conv);
				mem_free(conv);
			} else {
				add_to_str(&n, &l, file);
			}
			prog++;
		}
	}
	nn = os_fixup_external_program(n);
	mem_free(n);
	return nn;
}

void start_download(struct session *ses, unsigned char *file)
{
	struct download *down;
	int h;
	unsigned char *url = ses->dn_url;
	unsigned char *pos;
	if (!url) return;
	if ((pos = extract_position(url))) mem_free(pos);
	test_abort_downloads_to_file(file, ses->term->cwd, 1);
	if ((h = create_download_file(ses, ses->term->cwd, file, 0, 0)) < 0) return;
	down = mem_calloc(sizeof(struct download));
	down->url = stracpy(url);
	down->stat.end = (void (*)(struct status *, void *))download_data;
	down->stat.data = down;
	down->decompress = 0;
	down->last_pos = 0;
	down->file_shift = 0;
	down->cwd = stracpy(ses->term->cwd);
	down->orig_file = stracpy(file);
	down->file = stracpy(file);
	down->handle = h;
	down->ses = ses;
	down->remotetime = 0;
	add_to_list(downloads, down);
	load_url(url, NULL, &down->stat, PRI_DOWNLOAD, NC_CACHE);
	display_download(ses->term, down, ses);
}

void abort_all_downloads(void)
{
	while (!list_empty(downloads)) abort_download(downloads.next);
}

int f_is_finished(struct f_data *f)
{
	struct additional_file *af;
	if (!f || f->rq->state >= 0) return 0;
	if (f->fd && f->fd->rq && f->fd->rq->state >= 0) return 0;
	if (f->af) foreach(af, f->af->af) if (!af->rq || af->rq->state >= 0) return 0;
	return 1;
}

static int f_is_cacheable(struct f_data *f)
{
	if (!f || f->rq->state >= 0) return 0;
	if (f->fd && f->fd->rq && f->fd->rq->state >= 0) return 0;
	return 1;
}

static int f_need_reparse(struct f_data *f)
{
	struct additional_file *af;
	if (!f || f->rq->state >= 0) return 1;
	if (f->af) foreach(af, f->af->af) if (af->need_reparse > 0) return 1;
	return 0;
}

static struct f_data *format_html(struct f_data_c *fd, struct object_request *rq, unsigned char *url, struct document_options *opt, int *cch)
{
	struct f_data *f;
	pr(
	if (cch) *cch = 0;
	if (!rq->ce || !(f = init_formatted(opt))) goto nul;
	f->fd = fd;
	f->ses = fd->ses;
	f->time_to_get = -get_time();
	clone_object(rq, &f->rq);
	if (f->rq->ce) {
		unsigned char *start; unsigned char *end;
		int stl = -1;
		struct additional_file *af;

		if (fd->af) foreach(af, fd->af->af) if (af->need_reparse > 0) af->need_reparse = 0;

		get_file(rq, &start, &end);
		if (jsint_get_source(fd, &start, &end)) f->uncacheable = 1;
		if (opt->plain == 2) {
			start = init_str();
			stl = 0;
			add_to_str(&start, &stl, "<img src=\"");
			add_to_str(&start, &stl, f->rq->ce->url);
			add_to_str(&start, &stl, "\">");
			end = start + stl;
		}
		really_format_html(f->rq->ce, start, end, f, fd->ses ? fd != fd->ses->screen : 0);
		if (stl != -1) mem_free(start);
		f->use_tag = f->rq->ce->count;
		if (f->af) foreach(af, f->af->af) {
			if (af->rq && af->rq->ce) {
				af->use_tag = af->rq->ce->count;
				af->use_tag2 = af->rq->ce->count2;
			} else {
				af->use_tag = 0;
				af->use_tag2 = 0;
			}
		}
	} else f->use_tag = 0;
	f->time_to_get += get_time();
	) nul:return NULL;
	return f;
}

static void count_frames(struct f_data_c *fd, int *i)
{
	struct f_data_c *sub;
	if (!fd) return;
	if (fd->f_data) (*i)++;
	foreach(sub, fd->subframes) count_frames(sub, i);
}

long formatted_info(int type)
{
	int i = 0;
	struct session *ses;
	struct f_data *ce;
	switch (type) {
		case CI_FILES:
			foreach(ses, sessions)
				foreach(ce, ses->format_cache) i++;
			/* fall through */
		case CI_LOCKED:
			foreach(ses, sessions)
				count_frames(ses->screen, &i);
			return i;
		default:
			internal("formatted_info: bad request");
	}
	return 0;
}

static void f_data_attach(struct f_data_c *fd, struct f_data *f)
{
	struct additional_file *af;
	f->rq->upcall = (void (*)(struct object_request *, void *))fd_loaded;
	f->rq->data = fd;
	free_additional_files(&fd->af);
	fd->af = f->af;
	if (f->af) {
		f->af->refcount++;
		foreachback(af, f->af->af) {
			if (af->rq) {
				af->rq->upcall = (void (*)(struct object_request *, void *))fd_loaded;
				af->rq->data = fd;
			} else {
				request_object(fd->ses->term, af->url, f->rq->url, PRI_IMG, NC_CACHE, f->rq->upcall, f->rq->data, &af->rq);
			}
		}
	}
}

static void detach_f_data(struct f_data **ff)
{
	struct f_data *f = *ff;
	if (!f) return;
#ifdef G
	f->hlt_pos = -1;
	f->hlt_len = 0;
	f->start_highlight_x = -1;
	f->start_highlight_y = -1;
#endif
	*ff = NULL;
	if (f->frame_desc_link) {
		destroy_formatted(f);
		return;
	}
	f->fd = NULL;
#ifdef G
	f->locked_on = NULL;
	free_list(f->image_refresh);
#endif
	if (f->uncacheable || !f_is_cacheable(f) || !f->ses) destroy_formatted(f);
	else add_to_list(f->ses->format_cache, f);
}

static inline int is_format_cache_entry_uptodate(struct f_data *f)
{
	struct cache_entry *ce = f->rq->ce;
	struct additional_file *af;
	if (!ce || ce->count != f->use_tag) return 0;
	if (f->af) foreach(af, f->af->af) {
		struct cache_entry *ce = af->rq ? af->rq->ce : NULL;
		tcount tag = ce ? ce->count : 0;
		tcount tag2 = ce ? ce->count2 : 0;
		if (af->need_reparse > 0) if (tag != af->use_tag) return 0;
		if (af->unknown_image_size) if (tag2 != af->use_tag2) return 0;
	}
	return 1;
}

static int shrink_format_cache(int u)
{
	static int sc = 0;
	int scc;
	int r = 0;
	struct f_data *f;
	int c = 0;
	struct session *ses;
	foreach(ses, sessions) foreach(f, ses->format_cache) {
		if (u == SH_FREE_ALL || !is_format_cache_entry_uptodate(f)) {
			struct f_data *ff = f;
			f = f->prev;
			del_from_list(ff);
			destroy_formatted(ff);
			r |= ST_SOMETHING_FREED;
		} else c++;
	}
	if (c > max_format_cache_entries || (c && u == SH_FREE_SOMETHING)) {
		int sc_cycle = 0;
		unsigned char freed_in_cycle = 0;
		a:
		scc = sc++;
		foreach (ses, sessions) if (!scc--) {
			foreachback(f, ses->format_cache) {
				struct f_data *ff = f;
				f = f->next;
				del_from_list(ff);
				destroy_formatted(ff);
				r |= ST_SOMETHING_FREED;
				if (--c <= max_format_cache_entries ||
				    u == SH_FREE_SOMETHING) goto ret;
				freed_in_cycle = 1;
				goto a;
			}
			goto a;
		}
		sc = 0;
		sc_cycle++;
		if (sc_cycle >= 2 && !freed_in_cycle)
			goto ret;
		freed_in_cycle = 0;
		goto a;
	}
	ret:
	return r | (!c ? ST_CACHE_EMPTY : 0);
}

void init_fcache(void)
{
	register_cache_upcall(shrink_format_cache, "format");
}

static void calculate_scrollbars(struct f_data_c *fd, struct f_data *f)
{
	fd->hsb = 0, fd->vsb = 0;
	fd->hsbsize = 0, fd->vsbsize = 0;
	if (!f)
		return;
	if (f->opt.scrolling == SCROLLING_YES) {
		fd->hsb = 1, fd->vsb = 1;
	} else if (f->opt.scrolling == SCROLLING_AUTO) {
		x:
		if (!fd->hsb && f->x > fd->xw - fd->vsb * G_SCROLL_BAR_WIDTH) {
			fd->hsb = 1;
			goto x;
		}
		if (!fd->vsb && f->y > fd->yw - fd->hsb * G_SCROLL_BAR_WIDTH) {
			fd->vsb = 1;
			goto x;
		}
	}
	if (fd->hsb) fd->hsbsize = fd->xw - fd->vsb * G_SCROLL_BAR_WIDTH;
	if (fd->vsb) fd->vsbsize = fd->yw - fd->hsb * G_SCROLL_BAR_WIDTH;
	if (fd->hsbsize < 0) fd->hsb = 0;
	if (fd->vsbsize < 0) fd->vsb = 0;
}

struct f_data *cached_format_html(struct f_data_c *fd, struct object_request *rq, unsigned char *url, struct document_options *opt, int *cch)
{
	struct f_data *f;
	/*if (F) opt->tables = 0;*/
	if (fd->marginwidth != -1) {
		int marg = (fd->marginwidth + G_HTML_MARGIN - 1) / G_HTML_MARGIN;
		if (marg >= 0 && marg < 9) opt->margin = marg;
	}
	if (opt->plain == 2) opt->margin = 0, opt->display_images = 1;
	pr(
	if (!jsint_get_source(fd, NULL, NULL) && fd->ses) {
		if (fd->f_data && !strcmp(fd->f_data->rq->url, url) && !compare_opt(&fd->f_data->opt, opt) && is_format_cache_entry_uptodate(fd->f_data)) {
			f = fd->f_data;
			xpr();
			goto ret_f;
		}
		foreach(f, fd->ses->format_cache) {
			if (!strcmp(f->rq->url, url) && !compare_opt(&f->opt, opt)) {
				if (!is_format_cache_entry_uptodate(f)) {
					struct f_data *ff = f;
					f = f->prev;
					del_from_list(ff);
					destroy_formatted(ff);
					continue;
				}
				detach_f_data(&fd->f_data);
				del_from_list(f);
				f->fd = fd;
				if (cch) *cch = 1;
				f_data_attach(fd, f);
				xpr();
				goto ret_f;
			}
		}
	}) {};
	detach_f_data(&fd->f_data);
	f = format_html(fd, rq, url, opt, cch);
	if (f) f->fd = fd;
	shrink_memory(SH_CHECK_QUOTA);
ret_f:
	calculate_scrollbars(fd, f);
	return f;
}

static void create_new_frames(struct f_data_c *fd, struct frameset_desc *fs, struct document_options *o)
{
	struct location *loc;
	struct frame_desc *frm;
	int c_loc;
	int i;
	int x, y;
	int xp, yp;

	i = 0;
#ifdef JS
	if (fd->onload_frameset_code)mem_free(fd->onload_frameset_code);
	fd->onload_frameset_code=stracpy(fs->onload_code);
#endif
	foreach(loc, fd->loc->subframes) i++;
	if (i != fs->n) {
		while (!list_empty(fd->loc->subframes)) destroy_location(fd->loc->subframes.next);
		c_loc = 1;
	} else {
		c_loc = 0;
		loc = fd->loc->subframes.next;
	}

	yp = fd->yp;
	frm = &fs->f[0];
	for (y = 0; y < fs->y; y++) {
		xp = fd->xp;
		for (x = 0; x < fs->x; x++) {
			struct f_data_c *nfdc;
			if (!(nfdc = create_f_data_c(fd->ses, fd))) return;
			if (c_loc) {
				struct list_head *l = (struct list_head *)fd->loc->subframes.prev;
				loc = new_location();
				add_to_list(*l, loc);
				loc->parent = fd->loc;
				loc->name = stracpy(frm->name);
				if ((loc->url = stracpy(frm->url)))
					nfdc->goto_position = extract_position(loc->url);
			}
			nfdc->xp = xp; nfdc->yp = yp;
			nfdc->xw = frm->xw;
			nfdc->yw = frm->yw;
			nfdc->scrolling = frm->scrolling;
			nfdc->loc = loc;
			nfdc->vs = loc->vs;
			if (frm->marginwidth != -1) nfdc->marginwidth = frm->marginwidth;
			else nfdc->marginwidth = fd->marginwidth;
			if (frm->marginheight != -1) nfdc->marginheight = frm->marginheight;
			else nfdc->marginheight = fd->marginheight;
			/*debug("frame: %d %d, %d %d\n", nfdc->xp, nfdc->yp, nfdc->xw, nfdc->yw);*/
			{
				struct list_head *l = (struct list_head *)fd->subframes.prev;
				add_to_list(*l, nfdc);
			}
			if (frm->subframe) {
				create_new_frames(nfdc, frm->subframe, o);
				/*nfdc->f_data = init_formatted(&fd->f_data->opt);*/
				nfdc->f_data = init_formatted(o);
				nfdc->f_data->frame_desc = copy_frameset_desc(frm->subframe);
				nfdc->f_data->frame_desc_link = 1;
			} else {
				if (fd->depth < HTML_MAX_FRAME_DEPTH && loc->url && *loc->url)
					request_object(fd->ses->term, loc->url, fd->loc->url, PRI_FRAME, NC_CACHE, (void (*)(struct object_request *, void *))fd_loaded, nfdc, &nfdc->rq);
			}
			xp += frm->xw + gf_val(1, 0);
			frm++;
			if (!c_loc) loc = loc->next;
		}
		yp += (frm - 1)->yw + gf_val(1, 0);
	}
}

static void html_interpret(struct f_data_c *fd)
{
	int i;
	int oxw; int oyw; int oxp; int oyp;
	struct f_data_c *sf;
	int cch;
	/*int first = !fd->f_data;*/
	struct document_options o;
	struct js_event_spec *doc_js;
	struct js_event_spec **link_js;
	int nlink_js;
	if (!fd->loc) goto d;
	if (fd->f_data) {
		oxw = fd->f_data->opt.xw;
		oyw = fd->f_data->opt.yw;
		oxp = fd->f_data->opt.xp;
		oyp = fd->f_data->opt.yp;
	} else {
		oxw = oyw = oxp = oyp = -1;
	}
	memset(&o, 0, sizeof(struct document_options));
	ds2do(&fd->ses->ds, &o);
	if (fd->parent && fd->parent->f_data && !o.hard_assume) {
		o.assume_cp = fd->parent->f_data->cp;
	}
#ifdef JS
	o.js_enable = js_enable;
#else
	o.js_enable=0;
#endif
#ifdef G
	o.aspect_on=aspect_on;
	o.bfu_aspect=bfu_aspect;
#else
	o.aspect_on=0;
	o.bfu_aspect=0;
#endif
	o.plain = fd->vs->plain;
	if (o.plain == 1) {
		o.xp = 0;
		o.yp = 0;
		o.xw = MAXINT;
		o.yw = MAXINT;
	} else {
		o.xp = fd->xp;
		o.yp = fd->yp;
		o.xw = fd->xw;
		o.yw = fd->yw;
	}
	o.scrolling = fd->scrolling;
	if (fd->ses->term->spec) {
		o.col = fd->ses->term->spec->col;
		o.cp = fd->ses->term->spec->charset;
		o.braille = fd->ses->term->spec->braille;
	} else {
		o.col = 3;
		o.cp = 0;
		o.braille = 0;
	}
	if (!F) {
		memcpy(&o.default_fg, &default_fg, sizeof(struct rgb));
		memcpy(&o.default_bg, &default_bg, sizeof(struct rgb));
		memcpy(&o.default_link, &default_link, sizeof(struct rgb));
		memcpy(&o.default_vlink, &default_vlink, sizeof(struct rgb));
#ifdef G
	} else {
		memcpy(&o.default_fg, &default_fg_g, sizeof(struct rgb));
		memcpy(&o.default_bg, &default_bg_g, sizeof(struct rgb));
		memcpy(&o.default_link, &default_link_g, sizeof(struct rgb));
		memcpy(&o.default_vlink, &default_vlink_g, sizeof(struct rgb));
#endif
	}
	if (!(o.framename = fd->loc->name)) o.framename = NULL;
	doc_js = NULL;
	link_js = DUMMY;
	nlink_js = 0;
	if (fd->f_data) {
		copy_js_event_spec(&doc_js, fd->f_data->js_event);
		if (fd->f_data->nlinks > fd->f_data->nlink_events) nlink_js = fd->f_data->nlinks; else nlink_js = fd->f_data->nlink_events;
		if ((unsigned)nlink_js > MAXINT / sizeof(struct js_event_spec *)) overalloc();
		link_js = mem_alloc(nlink_js * sizeof(struct js_event_spec *));
		for (i = 0; i < fd->f_data->nlinks; i++) copy_js_event_spec(&link_js[i], fd->f_data->links[i].js_event);
		for (; i < fd->f_data->nlink_events; i++) copy_js_event_spec(&link_js[i], fd->f_data->link_events[i]);
	}
	if (!(fd->f_data = cached_format_html(fd, fd->rq, fd->rq->url, &o, &cch))) {
		for (i = 0; i < nlink_js; i++) free_js_event_spec(link_js[i]);
		mem_free(link_js);
		free_js_event_spec(doc_js);
		goto d;
	}
	if (join_js_event_spec(&fd->f_data->js_event, doc_js)) fd->f_data->uncacheable = 1;
	for (i = 0; i < fd->f_data->nlink_events; i++) free_js_event_spec(fd->f_data->link_events[i]);
	mem_free(fd->f_data->link_events);
	fd->f_data->link_events = link_js;
	fd->f_data->nlink_events = nlink_js;
	for (i = 0; i < fd->f_data->nlinks && i < nlink_js; i++) if (join_js_event_spec(&fd->f_data->links[i].js_event, link_js[i])) fd->f_data->uncacheable = 1;
	free_js_event_spec(doc_js);

	/* erase frames if changed */
	i = 0;
	foreach(sf, fd->subframes) i++;
	if (i != (fd->f_data->frame_desc ? fd->f_data->frame_desc->n : 0) && (f_is_finished(fd->f_data) || !f_need_reparse(fd->f_data))) {
		rd:
		foreach(sf, fd->subframes) reinit_f_data_c(sf);
		free_list(fd->subframes);

		/* create new frames */
		if (fd->f_data->frame_desc) create_new_frames(fd, fd->f_data->frame_desc, &fd->f_data->opt);
	} else {
		if (fd->f_data->frame_desc && fd->f_data->rq->state < 0) {
			if (fd->f_data->opt.xw != oxw ||
			    fd->f_data->opt.yw != oyw ||
			    fd->f_data->opt.xp != oxp ||
			    fd->f_data->opt.yp != oyp) goto rd;
		}
	}

	d:;
	/*draw_fd(fd);*/
}

void html_interpret_recursive(struct f_data_c *f)
{
	struct f_data_c *fd;
	if (f->rq) html_interpret(f);
	foreach(fd, f->subframes) html_interpret_recursive(fd);
}

/* You get a struct_additionl_file. never mem_free it. When you stop
 * using it, just forget the pointer.
 */
struct additional_file *request_additional_file(struct f_data *f, unsigned char *url_)
{
	struct additional_file *af;
	unsigned char *u, *url;
	url = stracpy(url_);
	if ((u = extract_position(url))) mem_free(u);
	if (!f->af) {
		if (!(f->af = f->fd->af)) {
			f->af = f->fd->af = mem_calloc(sizeof(struct additional_files));
			f->af->refcount = 1;
			init_list(f->af->af);
		}
		f->af->refcount++;
	}
	foreach(af, f->af->af) if (!strcmp(af->url, url)) {
		mem_free(url);
		return af;
	}
	af = mem_alloc(sizeof(struct additional_file) + strlen(url) + 1);
	af->use_tag = 0;
	af->use_tag2 = 0;
	strcpy(af->url, url);
	request_object(f->ses->term, url, f->rq->url, PRI_IMG, NC_CACHE, f->rq->upcall, f->rq->data, &af->rq);
	af->need_reparse = 0;
	af->unknown_image_size = 0;
	add_to_list(f->af->af, af);
	mem_free(url);
	return af;
}

#ifdef G

static void image_timer(struct f_data_c *fd)
{
	struct image_refresh *ir;
	struct list_head new;
	struct list_head *newp = do_not_optimize_here(&new);
	init_list(*newp);
	fd->image_timer = -1;
	if (!fd->f_data) return;
	foreach (ir, fd->f_data->image_refresh) {
		if (ir->t > G_IMG_REFRESH) ir->t -= G_IMG_REFRESH;
		else {
			struct image_refresh *irr = ir->prev;
			del_from_list(ir);
			add_to_list(*newp, ir);
			ir = irr;
		}
	}
	foreach (ir, *newp) {
		/*fprintf(stderr, "DRAW: %p\n", ir->img);*/
		draw_one_object(fd, ir->img);
	}
	free_list(*newp);
	if (fd->image_timer == -1 && !list_empty(fd->f_data->image_refresh)) fd->image_timer = install_timer(G_IMG_REFRESH, (void (*)(void *))image_timer, fd);
}

void refresh_image(struct f_data_c *fd, struct g_object *img, ttime tm)
{
	struct image_refresh *ir;
	if (!fd->f_data) return;
	foreach (ir, fd->f_data->image_refresh) if (ir->img == img) {
		if (ir->t > tm) ir->t = tm;
		return;
	}
	ir = mem_alloc(sizeof(struct image_refresh));
	ir->img = img;
	ir->t = tm;
	add_to_list(fd->f_data->image_refresh, ir);
	if (fd->image_timer == -1) fd->image_timer = install_timer(G_IMG_REFRESH, (void (*)(void *))image_timer, fd);
}

#endif

void reinit_f_data_c(struct f_data_c *fd)
{
	struct additional_file *af;
	struct f_data_c *fd1;
#ifdef G
	if (F)
		if (fd == current_frame(fd->ses))
			fd->ses->locked_link = 0;
#endif
	jsint_destroy(fd);
	foreach(fd1, fd->subframes) {
		if (fd->ses->wtd_target_base == fd1) fd->ses->wtd_target_base = NULL;
		reinit_f_data_c(fd1);
		if (fd->ses->wtd_target_base == fd1) fd->ses->wtd_target_base = fd;
		if (fd->ses->defered_target_base == fd1) fd->ses->defered_target_base = fd;
	}
	free_list(fd->subframes);
#ifdef JS
	if (fd->onload_frameset_code)mem_free(fd->onload_frameset_code),fd->onload_frameset_code=NULL;
#endif
	fd->loc = NULL;
	if (fd->f_data && fd->f_data->rq) fd->f_data->rq->upcall = NULL;
	if (fd->f_data && fd->f_data->af) foreach(af, fd->f_data->af->af) if (af->rq) {
		af->rq->upcall = NULL;
		if (af->rq->state != O_OK) release_object(&af->rq);
	}
	if (fd->af) foreach(af, fd->af->af) if (af->rq) af->rq->upcall = NULL;
	detach_f_data(&fd->f_data);
	if (fd->link_bg) mem_free(fd->link_bg), fd->link_bg = NULL;
	fd->link_bg_n = 0;
	if (fd->goto_position) mem_free(fd->goto_position), fd->goto_position = NULL;
	if (fd->went_to_position) mem_free(fd->went_to_position), fd->went_to_position = NULL;
	release_object(&fd->rq);
	free_additional_files(&fd->af);
	fd->next_update = get_time();
	fd->done = 0;
	fd->parsed_done = 0;
	if (fd->image_timer != -1) kill_timer(fd->image_timer), fd->image_timer = -1;
	if (fd->refresh_timer != -1) kill_timer(fd->refresh_timer), fd->refresh_timer = -1;
}

struct f_data_c *create_f_data_c(struct session *ses, struct f_data_c *parent)
{
	static long id = 1;
	struct f_data_c *fd;
	fd = mem_calloc(sizeof(struct f_data_c));
	fd->parent = parent;
	fd->ses = ses;
	fd->depth = parent ? parent->depth + 1 : 1;
	init_list(fd->subframes);
	fd->next_update = get_time();
	fd->done = 0;
	fd->parsed_done = 0;
	fd->script_t = 0;
	fd->id = id++;
	fd->marginwidth = fd->marginheight = -1;
	fd->image_timer = -1;
	fd->refresh_timer = -1;
	fd->scrolling = SCROLLING_AUTO;
	return fd;
}

static int plain_type(struct session *ses, struct object_request *rq, unsigned char **p)
{
	struct cache_entry *ce;
	unsigned char *ct;
	int r = 0;
	if (p) *p = NULL;
	if (!rq || !(ce = rq->ce)) {
		r = 1;
		goto f;
	}
	if (!(ct = get_content_type(ce->head, ce->url))) goto f;
	if (is_html_type(ct)) goto ff;
	r = 1;
	if (!strcasecmp(ct, "text/plain") || !strcasecmp(ct, "file/txt")) goto ff;
	r = 2;
	if (F && known_image_type(ct)) goto ff;
	r = -1;

	ff:
	if (!p) mem_free(ct);
	else *p = ct;
	f:
	return r;
}

#if defined(HAVE_ZLIB) || defined(HAVE_BZIP2) || defined(HAVE_LZMA)
#define HAVE_ANY_COMPRESSION
#endif

#ifdef HAVE_ANY_COMPRESSION

static void decoder_memory_init(unsigned char **p, size_t *size, off_t init_length)
{
	if (init_length > 0 && init_length < MAXINT) *size = init_length;
	else *size = 4096;
	*p = mem_alloc(*size);
}

static void decoder_memory_expand(unsigned char **p, size_t size, size_t *addsize)
{
	size_t add = size / 4 + 1;
	if (size + add < size) {
		if (add > 1) add >>= 1;
		else overalloc();
	}
	*addsize = add;
	*p = mem_realloc(*p, size + add);
}

static void decompress_error(struct terminal *term, struct cache_entry *ce, unsigned char *lib, unsigned char *msg, int *errp)
{
	struct window *win;
	unsigned char *u, *uu, *server;
	if ((u = parse_http_header(ce->head, "Content-Encoding", NULL))) {
		mem_free(u);
		if ((server = get_host_name(ce->url))) {
			add_blacklist_entry(server, BL_NO_COMPRESSION);
			mem_free(server);
		}
	}
	if (!term) return;
	if (errp) *errp = 1;
	else foreach(win, term->windows) if (win->handler == dialog_func) {
		struct dialog_data *d = win->data;
		if (d->dlg->title == TEXT_(T_DECOMPRESSION_ERROR)) {
			return;
		}
	}
	u = stracpy(ce->url);
	if ((uu = strchr(u, POST_CHAR))) *uu = 0;
	msg_box(term, getml(u, NULL), TEXT_(T_DECOMPRESSION_ERROR), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_ERROR_DECOMPRESSING_), u, TEXT_(T__wITH_), lib, ": ", msg, NULL, NULL, 1, TEXT_(T_CANCEL), NULL, B_ENTER | B_ESC);
}

#endif

#ifdef HAVE_ZLIB
#include <zlib.h>
static int decode_gzip(struct terminal *term, struct cache_entry *ce, unsigned char **p_start, size_t *p_len, int defl, int *errp)
{
	unsigned char err;
	unsigned char memory_error;
	unsigned char skip_gzip_header;
	unsigned char old_zlib;
	z_stream z;
	off_t offset;
	size_t header;
	int r;
	unsigned char *p;
	struct fragment *f;
	size_t size;

	retry_after_memory_error:
	memory_error = 0;
	header = 0;
	decoder_memory_init(&p, &size, ce->length);
	init_again:
	err = 0;
	skip_gzip_header = 0;
	old_zlib = 0;
	memset(&z, 0, sizeof z);
	z.next_in = NULL;
	z.avail_in = 0;
	z.next_out = p;
	z.avail_out = size;
	z.zalloc = NULL;
	z.zfree = NULL;
	z.opaque = NULL;
	r = inflateInit2(&z, defl == 1 ? 15 : defl == 2 ? -15 : 15 + 16);
	init_failed:
	switch (r) {
		case Z_OK:		break;
		case Z_MEM_ERROR:	memory_error = 1;
					err = 1;
					goto after_inflateend;
		case Z_STREAM_ERROR:	
					if (!defl && !old_zlib) {
						defrag_entry(ce);
						r = inflateInit2(&z, -15);
						skip_gzip_header = 1;
						old_zlib = 1;
						goto init_failed;
					}
					decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : (unsigned char *)"Invalid parameter", errp);
					err = 1;
					goto after_inflateend;
		case Z_VERSION_ERROR:	decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : (unsigned char *)"Bad zlib version", errp);
					err = 1;
					goto after_inflateend;
		default:		decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : (unsigned char *)"Unknown return value on inflateInit2", errp);
					err = 1;
					goto after_inflateend;
	}
	offset = 0;
	foreach(f, ce->frag) {
		if (f->offset != offset) break;
		z.next_in = f->data;
		z.avail_in = f->length;
		if (header && !offset) {
			z.next_in = (unsigned char *)z.next_in + header;
			z.avail_in -= header;
		}
		repeat_frag:
		if (skip_gzip_header == 2) {
			if (z.avail_in < 8) goto finish;
			z.next_in = (unsigned char *)z.next_in + 8;
			z.avail_in -= 8;
			skip_gzip_header = 1;
		}
		if (skip_gzip_header) {
			/* if zlib is old, we have to skip gzip header manually
			   otherwise zlib 1.2.x can do it automatically */
			unsigned char *head = z.next_in;
			unsigned headlen = 10;
			if (z.avail_in <= 11) goto finish;
			if (head[0] != 0x1f || head[1] != 0x8b) {
				decompress_error(term, ce, "zlib", TEXT_(T_COMPRESSED_ERROR), errp);
				err = 1;
				goto finish;
			}
			if (head[2] != 8 || head[3] & 0xe0) {
				decompress_error(term, ce, "zlib", TEXT_(T_UNKNOWN_COMPRESSION_METHOD), errp);
				err = 1;
				goto finish;
			}
			if (head[3] & 0x04) {
				headlen += 2 + head[10] + (head[11] << 8);
				if (headlen >= z.avail_in) goto finish;
			}
			if (head[3] & 0x08) {
				do {
					headlen++;
					if (headlen >= z.avail_in) goto finish;
				} while (head[headlen - 1]);
			}
			if (head[3] & 0x10) {
				do {
					headlen++;
					if (headlen >= z.avail_in) goto finish;
				} while (head[headlen - 1]);
			}
			if (head[3] & 0x01) {
				headlen += 2;
				if (headlen >= z.avail_in) goto finish;
			}
			z.next_in = (unsigned char *)z.next_in + headlen;
			z.avail_in -= headlen;
			skip_gzip_header = 0;
		}
		r = inflate(&z, (void *)f->next == (void *)&ce->frag ? Z_SYNC_FLUSH : Z_NO_FLUSH);
		switch (r) {
			case Z_OK:		break;
			case Z_BUF_ERROR:	break;
			case Z_STREAM_END:	r = inflateEnd(&z);
						if (r != Z_OK) goto end_failed;
						r = inflateInit2(&z, old_zlib ? -15 : defl ? 15 : 15 + 16);
						if (r != Z_OK) {
							old_zlib = 0;
							goto init_failed;
						}
						if (old_zlib) {
							skip_gzip_header = 2;
						}
						break;
			case Z_NEED_DICT:
			case Z_DATA_ERROR:	if (defl == 1) {
							defl = 2;
							r = inflateEnd(&z);
							if (r != Z_OK) goto end_failed;
							goto init_again;
						}
						decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : TEXT_(T_COMPRESSED_ERROR), errp);
						err = 1;
						goto finish;
			case Z_STREAM_ERROR:	decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : (unsigned char *)"Internal error on inflate", errp);
						err = 1;
						goto finish;
			case Z_MEM_ERROR:	memory_error = 1;
						err = 1;
						goto finish;
			default:		decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : (unsigned char *)"Unknown return value on inflate", errp);
						err = 1;
						break;
		}
		if (!z.avail_out) {
			size_t addsize;
			decoder_memory_expand(&p, size, &addsize);
			z.next_out = p + size;
			z.avail_out = addsize;
			size += addsize;
		}
		if (z.avail_in) goto repeat_frag;
		/* In zlib 1.1.3, inflate(Z_SYNC_FLUSH) doesn't work.
		   The following line fixes it --- for last fragment, loop until
		   we get an eof. */
		if (r == Z_OK && (void *)f->next == (void *)&ce->frag) goto repeat_frag;
		offset += f->length;
	}
	finish:
	r = inflateEnd(&z);
	end_failed:
	switch (r) {
		case Z_OK:		break;
		case Z_STREAM_ERROR:	decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : (unsigned char *)"Internal error on inflateEnd", errp);
					err = 1;
					break;
		case Z_MEM_ERROR:	memory_error = 1;
					err = 1;
					break;
		default:		decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : (unsigned char *)"Unknown return value on inflateEnd", errp);
					err = 1;
					break;
	}
	after_inflateend:
	if (memory_error) {
		mem_free(p);
		if (out_of_memory(NULL, 0))
			goto retry_after_memory_error;
		decompress_error(term, ce, "zlib", z.msg ? (unsigned char *)z.msg : TEXT_(T_OUT_OF_MEMORY), errp);
		return 1;
	}
	if (err && (unsigned char *)z.next_out == p) {
		mem_free(p);
		return 1;
	}
	*p_start = p;
	*p_len = (unsigned char *)z.next_out - (unsigned char *)p;
	*p_start = mem_realloc(*p_start, *p_len);
	return 0;
}
#endif

#ifdef HAVE_BZIP2
#include <bzlib.h>
static int decode_bzip2(struct terminal *term, struct cache_entry *ce, unsigned char **p_start, size_t *p_len, int *errp)
{
	unsigned char err;
	unsigned char memory_error;
	bz_stream z;
	off_t offset;
	int r;
	unsigned char *p;
	struct fragment *f;
	size_t size;

	retry_after_memory_error:
	err = 0;
	memory_error = 0;
	decoder_memory_init(&p, &size, ce->length);
	memset(&z, 0, sizeof z);
	z.next_in = NULL;
	z.avail_in = 0;
	z.next_out = p;
	z.avail_out = size;
	z.bzalloc = NULL;
	z.bzfree = NULL;
	z.opaque = NULL;
	r = BZ2_bzDecompressInit(&z, 0, 0);
	init_failed:
	switch (r) {
		case BZ_OK:		break;
		case BZ_MEM_ERROR:	memory_error = 1;
					err = 1;
					goto after_inflateend;
		case BZ_PARAM_ERROR:	
					decompress_error(term, ce, "bzip2", "Invalid parameter", errp);
					err = 1;
					goto after_inflateend;
		case BZ_CONFIG_ERROR:	decompress_error(term, ce, "bzip2", "Bzlib is miscompiled", errp);
					err = 1;
					goto after_inflateend;
		default:		decompress_error(term, ce, "bzip2", "Unknown return value on BZ2_bzDecompressInit", errp);
					err = 1;
					goto after_inflateend;
	}
	offset = 0;
	foreach(f, ce->frag) {
		if (f->offset != offset) break;
		z.next_in = f->data;
		z.avail_in = f->length;
		repeat_frag:
		r = BZ2_bzDecompress(&z);
		switch (r) {
			case BZ_OK:		break;
			case BZ_STREAM_END:
						r = BZ2_bzDecompressEnd(&z);
						if (r != BZ_OK) goto end_failed;
						r = BZ2_bzDecompressInit(&z, 0, 0);
						if (r != BZ_OK) goto init_failed;
						break;
			case BZ_DATA_ERROR_MAGIC:
			case BZ_DATA_ERROR:	decompress_error(term, ce, "bzip2", TEXT_(T_COMPRESSED_ERROR), errp);
						err = 1;
						goto finish;
			case BZ_PARAM_ERROR:	decompress_error(term, ce, "bzip2", "Internal error on BZ2_bzDecompress", errp);
						err = 1;
						goto finish;
			case BZ_MEM_ERROR:	memory_error = 1;
						err = 1;
						goto finish;
			default:		decompress_error(term, ce, "bzip2", "Unknown return value on BZ2_bzDecompress", errp);
						err = 1;
						break;
		}
		if (!z.avail_out) {
			size_t addsize;
			decoder_memory_expand(&p, size, &addsize);
			z.next_out = p + size;
			z.avail_out = addsize;
			size += addsize;
		}
		if (z.avail_in) goto repeat_frag;
		offset += f->length;
	}
	finish:
	r = BZ2_bzDecompressEnd(&z);
	end_failed:
	switch (r) {
		case BZ_OK:		break;
		case BZ_PARAM_ERROR:	decompress_error(term, ce, "bzip2", "Internal error on BZ2_bzDecompressEnd", errp);
					err = 1;
					break;
		case BZ_MEM_ERROR:	memory_error = 1;
					err = 1;
					break;
		default:		decompress_error(term, ce, "bzip2", "Unknown return value on BZ2_bzDecompressEnd", errp);
					err = 1;
					break;
	}
	after_inflateend:
	if (memory_error) {
		mem_free(p);
		if (out_of_memory(NULL, 0))
			goto retry_after_memory_error;
		decompress_error(term, ce, "bzip2", TEXT_(T_OUT_OF_MEMORY), errp);
		return 1;
	}
	if (err && (unsigned char *)z.next_out == p) {
		mem_free(p);
		return 1;
	}
	*p_start = p;
	*p_len = (unsigned char *)z.next_out - (unsigned char *)p;
	*p_start = mem_realloc(*p_start, *p_len);
	return 0;
}
#endif

#ifdef HAVE_LZMA
#undef internal
#include <lzma.h>
#define internal internal_
static int decode_lzma(struct terminal *term, struct cache_entry *ce, unsigned char **p_start, size_t *p_len, int *errp)
{
	unsigned char err;
	unsigned char memory_error;
	lzma_stream z = LZMA_STREAM_INIT;
	off_t offset;
	int r;
	unsigned char *p;
	struct fragment *f;
	size_t size;

	retry_after_memory_error:
	err = 0;
	memory_error = 0;
	decoder_memory_init(&p, &size, ce->length);
	z.next_in = NULL;
	z.avail_in = 0;
	z.next_out = p;
	z.avail_out = size;
	r = lzma_auto_decoder(&z, UINT64_MAX, 0);
	init_failed:
	switch (r) {
		case LZMA_OK:		break;
		case LZMA_MEM_ERROR:	memory_error = 1;
					err = 1;
					goto after_inflateend;
		case LZMA_OPTIONS_ERROR:	
					decompress_error(term, ce, "lzma", "Invalid parameter", errp);
					err = 1;
					goto after_inflateend;
		case LZMA_PROG_ERROR:	decompress_error(term, ce, "lzma", "Lzma is miscompiled", errp);
					err = 1;
					goto after_inflateend;
		default:		decompress_error(term, ce, "lzma", "Unknown return value on lzma_auto_decoder", errp);
					err = 1;
					goto after_inflateend;
	}
	offset = 0;
	foreach(f, ce->frag) {
		if (f->offset != offset) break;
		z.next_in = f->data;
		z.avail_in = f->length;
		repeat_frag:
		r = lzma_code(&z, LZMA_RUN);
		switch (r) {
			case LZMA_OK:		
			case LZMA_NO_CHECK:	
			case LZMA_UNSUPPORTED_CHECK:
			case LZMA_GET_CHECK:
						break;
			case LZMA_STREAM_END:
						lzma_end(&z);
						r = lzma_auto_decoder(&z, UINT64_MAX, 0);
						if (r != LZMA_OK) goto init_failed;
						break;
			case LZMA_MEM_ERROR:	memory_error = 1;
						err = 1;
						goto finish;
			case LZMA_MEMLIMIT_ERROR:
						decompress_error(term, ce, "lzma", "Memory limit was exceeded", errp);
						err = 1;
						goto finish;
			case LZMA_FORMAT_ERROR:
			case LZMA_DATA_ERROR:
			case LZMA_BUF_ERROR:
						decompress_error(term, ce, "lzma", TEXT_(T_COMPRESSED_ERROR), errp);
						err = 1;
						goto finish;
			case LZMA_OPTIONS_ERROR:decompress_error(term, ce, "lzma", "File contains unsupported options", errp);
						err = 1;
						goto finish;
			case LZMA_PROG_ERROR:	decompress_error(term, ce, "lzma", "Lzma is miscompiled", errp);
						err = 1;
						goto finish;
			default:		decompress_error(term, ce, "lzma", "Unknown return value on lzma_code", errp);
						err = 1;
						break;
		}
		if (!z.avail_out) {
			size_t addsize;
			decoder_memory_expand(&p, size, &addsize);
			z.next_out = p + size;
			z.avail_out = addsize;
			size += addsize;
		}
		if (z.avail_in) goto repeat_frag;
		offset += f->length;
	}
	finish:
	lzma_end(&z);
	after_inflateend:
	if (memory_error) {
		mem_free(p);
		if (out_of_memory(NULL, 0))
			goto retry_after_memory_error;
		decompress_error(term, ce, "lzma", TEXT_(T_OUT_OF_MEMORY), errp);
		return 1;
	}
	if (err && (unsigned char *)z.next_out == p) {
		mem_free(p);
		return 1;
	}
	*p_start = p;
	*p_len = (unsigned char *)z.next_out - (unsigned char *)p;
	*p_start = mem_realloc(*p_start, *p_len);
	return 0;
}
#endif

static int get_file_by_term(struct terminal *term, struct cache_entry *ce, unsigned char **start, unsigned char **end, int *errp)
{
	unsigned char *enc;
	struct fragment *fr;
	if (errp) *errp = 0;
	*start = *end = NULL;
	if (!ce) return 1;
	if (ce->decompressed) {
#if defined(HAVE_ANY_COMPRESSION)
		return_decompressed:
#endif
		*start = ce->decompressed;
		*end = ce->decompressed + ce->decompressed_len;
		return 0;
	}
	enc = get_content_encoding(ce->head, ce->url);
	if (enc) {
#ifdef HAVE_ZLIB
		if (!strcasecmp(enc, "gzip") || !strcasecmp(enc, "x-gzip") || !strcasecmp(enc, "deflate")) {
			int defl = !strcasecmp(enc, "deflate");
			mem_free(enc);
			if (decode_gzip(term, ce, &ce->decompressed, &ce->decompressed_len, defl, errp)) goto uncompressed;
			goto return_decompressed;
		}
#endif
#ifdef HAVE_BZIP2
		if (!strcasecmp(enc, "bzip2")) {
			mem_free(enc);
			if (decode_bzip2(term, ce, &ce->decompressed, &ce->decompressed_len, errp)) goto uncompressed;
			goto return_decompressed;
		}
#endif
#ifdef HAVE_LZMA
		if (!strcasecmp(enc, "lzma") || !strcasecmp(enc, "lzma2")) {
			mem_free(enc);
			if (decode_lzma(term, ce, &ce->decompressed, &ce->decompressed_len, errp)) goto uncompressed;
			goto return_decompressed;
		}
#endif
		mem_free(enc);
		goto uncompressed;
	}
	uncompressed:
	defrag_entry(ce);
	fr = ce->frag.next;
	if ((void *)fr == &ce->frag || fr->offset || !fr->length) return 1;
	*start = fr->data, *end = fr->data + fr->length;
	return 0;
}

int get_file(struct object_request *o, unsigned char **start, unsigned char **end)
{
	struct terminal *term;
	*start = *end = NULL;
	if (!o) return 1;
	foreach(term, terminals) if (o->term == term->count) goto ok;
	term = NULL;
	ok:
	return get_file_by_term(term, o->ce, start, end, NULL);
}

static void refresh_timer(struct f_data_c *fd)
{
	if (fd->ses->rq) {
		fd->refresh_timer = install_timer(500, (void (*)(void *))refresh_timer, fd);
		return;
	}
	fd->refresh_timer = -1;
	if (fd->f_data && fd->f_data->refresh) {
		fd->refresh_timer = install_timer(fd->f_data->refresh_seconds * 1000, (void (*)(void *))refresh_timer, fd);
		goto_url_f(fd->ses, NULL, fd->f_data->refresh, "_self", fd, -1, 0, 0, 1);
	}
}

#ifdef JS
static int frame_and_all_subframes_loaded(struct f_data_c *fd)
{
	struct f_data_c *f;
	int loaded=fd->done||fd->rq==NULL;

	if (loaded)		/* this frame is loaded */
		foreach(f,fd->subframes)
		{
			loaded=frame_and_all_subframes_loaded(f);
			if (!loaded)break;
		}
	return loaded;
}
#endif

void fd_loaded(struct object_request *rq, struct f_data_c *fd)
{
	if (fd->done) {
		if (f_is_finished(fd->f_data)) goto priint;
		else fd->done = 0, fd->parsed_done = 1;
	}
	if (fd->parsed_done && f_need_reparse(fd->f_data)) fd->parsed_done = 0;
	if (fd->vs->plain == -1 && rq->state != O_WAITING) {
		fd->vs->plain = plain_type(fd->ses, fd->rq, NULL);
	}
	if (fd->rq->state < 0 && (f_is_finished(fd->f_data) || !fd->f_data)) {
		if (!fd->parsed_done) {
			html_interpret(fd);
			if (fd->went_to_position) {
				if (!fd->goto_position) fd->goto_position = fd->went_to_position, fd->went_to_position = NULL;
				else mem_free(fd->went_to_position), fd->went_to_position = NULL;
			}
		}
		draw_fd(fd);
		/* it may happen that html_interpret requests load of additional file */
		if (!f_is_finished(fd->f_data)) goto more_data;
		fn:
		if (fd->f_data->are_there_scripts) {
			jsint_scan_script_tags(fd);
			if (!f_is_finished(fd->f_data)) goto more_data;
		}
		fd->done = 1;
		fd->parsed_done = 0;
		if (fd->f_data->refresh) {
			if (fd->refresh_timer != -1) kill_timer(fd->refresh_timer);
			fd->refresh_timer = install_timer(fd->f_data->refresh_seconds * 1000, (void (*)(void *))refresh_timer, fd);
		}
#ifdef JS
		jsint_run_queue(fd);
#endif
	} else more_data: if (get_time() >= fd->next_update) {
		ttime t;
		int first = !fd->f_data;
		if (!fd->parsed_done) {
			html_interpret(fd);
			if (fd->rq->state < 0 && !f_need_reparse(fd->f_data)) {
				if (fd->went_to_position) {
					if (!fd->goto_position) fd->goto_position = fd->went_to_position, fd->went_to_position = NULL;
					else mem_free(fd->went_to_position), fd->went_to_position = NULL;
				}
				fd->parsed_done = 1;
			}
		}
		draw_fd(fd);
		if (fd->rq->state < 0 && f_is_finished(fd->f_data)) goto fn;
		t = fd->f_data ? ((fd->parsed_done ? 0 : fd->f_data->time_to_get * DISPLAY_TIME) + fd->f_data->time_to_draw * IMG_DISPLAY_TIME) : 0;
		if (t < DISPLAY_TIME_MIN) t = DISPLAY_TIME_MIN;
		if (first && t > DISPLAY_TIME_MAX_FIRST) t = DISPLAY_TIME_MAX_FIRST;
		fd->next_update = get_time() + t;
	} else {
		change_screen_status(fd->ses);
		print_screen_status(fd->ses);
	}
	priint:
	/* process onload handler of a frameset */
#ifdef JS
	{
		int all_loaded;

		/* go to parent and test if all subframes are loaded, if yes, call onload handler */

		if (!fd->parent) goto hell;	/* this frame has no parent, skip */
		if (!fd->parent->onload_frameset_code)goto hell;	/* no onload handler, skip all this */
		all_loaded=frame_and_all_subframes_loaded(fd->parent);
		if (!all_loaded) goto hell;
		/* parent has all subframes loaded */
		jsint_execute_code(fd->parent,fd->parent->onload_frameset_code,strlen(fd->parent->onload_frameset_code),-1,-1,-1, NULL);
		mem_free(fd->parent->onload_frameset_code), fd->parent->onload_frameset_code=NULL;
	hell:;
	}
#endif
	if (rq && (rq->state == O_FAILED || rq->state == O_INCOMPLETE) && (fd->rq == rq || fd->ses->rq == rq)) print_error_dialog(fd->ses, &rq->stat, rq->url);
}

static tcount location_id = 0;

static struct location *new_location(void)
{
	struct location *loc;
	loc = mem_calloc(sizeof(struct location));
	loc->location_id = location_id++;
	init_list(loc->subframes);
	loc->vs = create_vs();
	return loc;
}

static struct location *alloc_ses_location(struct session *ses)
{
	struct location *loc;
	if (!(loc = new_location())) return NULL;
	add_to_list(ses->history, loc);
	return loc;
}

static void subst_location(struct f_data_c *fd, struct location *old, struct location *new)
{
	struct f_data_c *f;
	foreach(f, fd->subframes) subst_location(f, old, new);
	if (fd->loc == old) fd->loc = new;
}

static struct location *copy_sublocations(struct session *ses, struct location *d, struct location *s, struct location *x)
{
	struct location *dl, *sl, *y, *z;
	d->name = stracpy(s->name);
	if (s == x) return d;
	d->url = stracpy(s->url);
	d->prev_url = stracpy(s->prev_url);
	destroy_vs(d->vs);
	d->vs = s->vs; s->vs->refcount++;
	subst_location(ses->screen, s, d);
	y = NULL;
	foreach(sl, s->subframes) {
		if ((dl = new_location())) {
			struct list_head *l = (struct list_head *)d->subframes.prev;
			add_to_list(*l, dl);
			dl->parent = d;
			z = copy_sublocations(ses, dl, sl, x);
			if (z && y) internal("copy_sublocations: crossed references");
			if (z) y = z;
		}
	}
	return y;
}

static struct location *copy_location(struct session *ses, struct location *loc)
{
	struct location *l2, *l1, *nl;
	l1 = cur_loc(ses);
	if (!(l2 = alloc_ses_location(ses))) return NULL;
	if (!(nl = copy_sublocations(ses, l2, l1, loc))) internal("copy_location: sublocation not found");
	return nl;
}

static struct f_data_c *new_main_location(struct session *ses)
{
	struct location *loc;
	if (!(loc = alloc_ses_location(ses))) return NULL;
	reinit_f_data_c(ses->screen);
	ses->screen->loc = loc;
	ses->screen->vs = loc->vs;
	if (ses->wanted_framename) loc->name=ses->wanted_framename, ses->wanted_framename=NULL;
	return ses->screen;
}

static struct f_data_c *copy_location_and_replace_frame(struct session *ses, struct f_data_c *fd)
{
	struct location *loc;
	if (!(loc = copy_location(ses, fd->loc))) return NULL;
	reinit_f_data_c(fd);
	fd->loc = loc;
	fd->vs = loc->vs;
	return fd;
}

/* vrati frame prislusici danemu targetu
   pokud takovy frame nenajde, vraci NULL
 */
struct f_data_c *find_frame(struct session *ses, unsigned char *target, struct f_data_c *base)
{
	struct f_data_c *f, *ff;
	if (!base) base = ses->screen;
	if (!target || !*target) return base;
	if (!strcasecmp(target, "_blank"))
		return NULL;  /* open in new window */
	if (!strcasecmp(target, "_top"))
		return ses->screen;
	if (!strcasecmp(target, "_self")) return base;
	if (!strcasecmp(target, "_parent")) {
		for (ff = base->parent; ff && !ff->rq; ff = ff->parent)
			;
		return ff ? ff : ses->screen;
	}
	f = ses->screen;
	if (f->loc && f->loc->name && !strcasecmp(f->loc->name, target)) return f;
	d:
	foreach(ff, f->subframes) if (ff->loc && ff->loc->name && !strcasecmp(ff->loc->name, target)) return ff;
	if (!list_empty(f->subframes)) {
		f = f->subframes.next;
		goto d;
	}
	u:
	if (!f->parent) return NULL;
	if (f->next == (void *)&f->parent->subframes) {
		f = f->parent;
		goto u;
	}
	f = f->next;
	goto d;
}

static void destroy_location(struct location *loc)
{
	while (loc->subframes.next != &loc->subframes) destroy_location(loc->subframes.next);
	del_from_list(loc);
	if (loc->name) mem_free(loc->name);
	if (loc->url) mem_free(loc->url);
	if (loc->prev_url) mem_free(loc->prev_url);
	destroy_vs(loc->vs);
	mem_free(loc);
}

static void clear_forward_history(struct session *ses)
{
	while (!list_empty(ses->forward_history)) destroy_location(ses->forward_history.next);
}

static void ses_go_forward(struct session *ses, int plain, int refresh)
{
	struct location *cl;
	struct f_data_c *fd;
	clear_forward_history(ses);
	if (ses->search_word) mem_free(ses->search_word), ses->search_word = NULL;
	if (ses->default_status){mem_free(ses->default_status);ses->default_status=NULL;}	/* smazeme default status, aby neopruzoval na jinych strankach */
	if ((fd = find_frame(ses, ses->wtd_target, ses->wtd_target_base))&&fd!=ses->screen) {
		cl = NULL;
		if (refresh && fd->loc && !strcmp(fd->loc->url, ses->rq->url)) cl = cur_loc(ses);
		fd = copy_location_and_replace_frame(ses, fd);
		if (cl) destroy_location(cl);
	} else fd = new_main_location(ses);
	if (!fd) return;
	fd->vs->plain = plain;
	ses->wtd = NULL;
	fd->rq = ses->rq; ses->rq = NULL;
	fd->goto_position = ses->goto_position; ses->goto_position = NULL;
	fd->loc->url = stracpy(fd->rq->url);
	fd->loc->prev_url = stracpy(fd->rq->prev_url);
	fd->rq->upcall = (void (*)(struct object_request *, void *))fd_loaded;
	fd->rq->data = fd;
	fd->rq->upcall(fd->rq, fd);
	draw_formatted(ses);
}

static void ses_go_backward(struct session *ses)
{
	int n;
	struct location *loc;
	if (ses->search_word) mem_free(ses->search_word), ses->search_word = NULL;
	if (ses->default_status){mem_free(ses->default_status);ses->default_status=NULL;}	/* smazeme default status, aby neopruzoval na jinych strankach */
	reinit_f_data_c(ses->screen);
	if (!ses->wtd_num_steps) internal("ses_go_backward: wtd_num_steps is zero");
	if (ses->wtd_num_steps > 0) {
		n = ses->wtd_num_steps;
		foreach(loc, ses->history) {
			if (!n--) goto have_back_loc;
		}
		internal("ses_go_backward: session history disappeared");
		return;
		have_back_loc:
		for (n = 0; n < ses->wtd_num_steps; n++) {
			loc = cur_loc(ses);
			del_from_list(loc);
			add_to_list(ses->forward_history, loc);
		}
	} else {
		n = ses->wtd_num_steps;
		foreach(loc, ses->forward_history) {
			if (!++n) goto have_fwd_loc;
		}
		internal("ses_go_backward: session forward history disappeared");
		return;
		have_fwd_loc:
		for (n = 0; n < -ses->wtd_num_steps; n++) {
			loc = (struct location *)ses->forward_history.next;
			del_from_list(loc);
			add_to_list(ses->history, loc);
		}
	}
#ifdef G
	ses->locked_link = 0;
#endif
	ses->screen->loc = cur_loc(ses);
	ses->screen->vs = ses->screen->loc->vs;
	ses->wtd = NULL;
	ses->screen->rq = ses->rq; ses->rq = NULL;
	ses->screen->rq->upcall = (void (*)(struct object_request *, void *))fd_loaded;
	ses->screen->rq->data = ses->screen;
	ses->screen->rq->upcall(ses->screen->rq, ses->screen);

}

static void tp_cancel(struct session *ses)
{
	release_object(&ses->tq);
}

static void continue_download(struct session *ses, unsigned char *file)
{
	unsigned char *enc;
	struct download *down;
	int h;
	int namecount = 0;
	unsigned char *url = ses->tq->url;
	if (ses->tq_prog) {
		if (ses->tq_prog_flag_direct && ses->tq->state != O_OK && !strchr(url, POST_CHAR) && !check_shell_url(url)) {
			unsigned char *prog = subst_file(ses->tq_prog, url, 0);
			exec_on_terminal(ses->term, prog, "", !!ses->tq_prog_flag_block);
			mem_free(prog);
			tp_cancel(ses);
			abort_background_connections();
			return;
		}
		new_name:
		if (!(file = get_temp_name(url, ses->tq->ce ? ses->tq->ce->head : NULL))) {
			tp_cancel(ses);
			return;
		}
	}
	if (!ses->tq_prog) test_abort_downloads_to_file(file, ses->term->cwd, 1);
	if ((h = create_download_file(ses, ses->term->cwd, file, !!ses->tq_prog, 0)) < 0) {
		if (h == -2 && ses->tq_prog) {
			if (++namecount < DOWNLOAD_NAME_TRIES) {
				mem_free(file);
				goto new_name;
			}
			msg_box(ses->term, NULL, TEXT_(T_DOWNLOAD_ERROR), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_COULD_NOT_CREATE_TEMPORARY_FILE), NULL, NULL, 1, TEXT_(T_CANCEL), NULL, B_ENTER | B_ESC);
		}
		if (ses->tq_prog) mem_free(file);
		tp_cancel(ses);
		return;
	}
	down = mem_calloc(sizeof(struct download));
	down->url = stracpy(url);
	down->stat.end = (void (*)(struct status *, void *))download_data;
	down->stat.data = down;
	down->decompress = 0;
	down->last_pos = 0;
	down->file_shift = 0;
	down->cwd = stracpy(ses->term->cwd);
	down->orig_file = stracpy(file);
	down->file = stracpy(file);
	down->handle = h;
	down->ses = ses;
	if (ses->tq_prog) {
		down->prog = subst_file(ses->tq_prog, file, 1);
		mem_free(file);
		mem_free(ses->tq_prog);
		ses->tq_prog = NULL;
	}
	down->prog_flag_block = ses->tq_prog_flag_block;

	enc = ses->tq->ce ? get_content_encoding(ses->tq->ce->head, ses->tq->ce->url) : NULL;
	if (enc) {
		if (encoding_2_extension(enc)) {
			if (down->prog) down->decompress = 1;
		} else {
			down->decompress = 1;
		}
		mem_free(enc);
	}

	add_to_list(downloads, down);
	release_object_get_stat(&ses->tq, &down->stat, PRI_DOWNLOAD);
	display_download(ses->term, down, ses);
}


static void tp_save(struct session *ses)
{
	if (ses->tq_prog) mem_free(ses->tq_prog), ses->tq_prog = NULL;
	query_file(ses, ses->tq->url, ses->tq->ce ? ses->tq->ce->head : NULL, &continue_download, tp_cancel);
}

static void tp_open(struct session *ses)
{
	continue_download(ses, "");
}

static int ses_abort_1st_state_loading(struct session *ses)
{
	int r = !!ses->rq;
	release_object(&ses->rq);
	ses->wtd = NULL;
	if (ses->wtd_target) mem_free(ses->wtd_target), ses->wtd_target = NULL;
	ses->wtd_target_base = NULL;
	if (ses->goto_position) mem_free(ses->goto_position), ses->goto_position = NULL;
	change_screen_status(ses);
	print_screen_status(ses);
	return r;
}

static void tp_display(struct session *ses)
{
	ses_abort_1st_state_loading(ses);
	ses->rq = ses->tq;
	ses->tq = NULL;
	ses_go_forward(ses, 1, 0);
}

static int direct_download_possible(struct object_request *rq, struct assoc *a)
{
	unsigned char *proto = get_protocol_name(rq->url);
	int ret = 0;
	if (!proto) return 0;
	if (a->accept_http && !strcasecmp(proto, "http")) ret = 1;
	if (a->accept_ftp && !strcasecmp(proto, "ftp")) ret = 1;
	mem_free(proto);
	if (proxies.only_proxies) ret = 0;
	return ret;
}

static int prog_sel_save(struct dialog_data *dlg, struct dialog_item_data* idata)
{
	struct session *ses=(struct session *)(dlg->dlg->udata2);

	if (ses->tq_prog) mem_free(ses->tq_prog), ses->tq_prog = NULL;
	query_file(ses, ses->tq->url, ses->tq->ce ? ses->tq->ce->head : NULL, &continue_download, tp_cancel);

	cancel_dialog(dlg,idata);
	return 0;
}

static int prog_sel_display(struct dialog_data *dlg, struct dialog_item_data* idata)
{
	struct session *ses=(struct session *)(dlg->dlg->udata2);

	ses_abort_1st_state_loading(ses);
	ses->rq = ses->tq;
	ses->tq = NULL;
	ses_go_forward(ses, 1, 0);

	cancel_dialog(dlg,idata);
	return 0;
}

static int prog_sel_cancel(struct dialog_data *dlg, struct dialog_item_data* idata)
{
	struct session *ses=(struct session *)(dlg->dlg->udata2);

	release_object(&ses->tq);
	cancel_dialog(dlg,idata);
	return 0;
}

static int prog_sel_open(struct dialog_data *dlg, struct dialog_item_data* idata)
{
	struct assoc *a=(struct assoc*)idata->item->udata;
	struct session *ses=(struct session *)(dlg->dlg->udata2);

	if (!a) internal("This should not happen.\n");
	ses->tq_prog = stracpy(a->prog), ses->tq_prog_flag_block = a->block, ses->tq_prog_flag_direct = direct_download_possible(ses->tq, a);
	tp_open(ses);
	cancel_dialog(dlg,idata);
	return 0;
}

static void vysad_dvere(struct dialog_data *dlg)
{
	struct terminal *term = dlg->win->term;
	/*struct session *ses=(struct session *)(dlg->dlg->udata2);*/
	unsigned char *ct=(unsigned char *)(dlg->dlg->udata);
	int max = 0, min = 0;
	int w, rw;
	int y = 0;
	unsigned char *txt;
	int l=0;

	txt=init_str();
	
	/* brainovi to tady pada !!!
	add_to_str(&txt,&l,ses->tq->url);
	add_to_str(&txt,&l," ");
	add_to_str(&txt,&l,_(TEXT_(T_HAS_TYPE),term));
	*/
	add_to_str(&txt,&l,_(TEXT_(T_CONTEN_TYPE_IS),term));
	add_to_str(&txt,&l," ");
	add_to_str(&txt,&l,ct);
	add_to_str(&txt,&l,".\n");
	if (!anonymous)
		add_to_str(&txt,&l,_(TEXT_(T_DO_YOU_WANT_TO_OPEN_SAVE_OR_DISPLAY_THIS_FILE),term));
	else
		add_to_str(&txt,&l,_(TEXT_(T_DO_YOU_WANT_TO_OPEN_OR_DISPLAY_THIS_FILE),term));

	max_text_width(term, txt, &max, AL_CENTER);
	min_text_width(term, txt, &min, AL_CENTER);
	max_buttons_width(term, dlg->items , dlg->n, &max);
	min_buttons_width(term, dlg->items , dlg->n, &min);
	w = term->x * 9 / 10 - 2 * DIALOG_LB;
	if (w > max) w = max;
	if (w < min) w = min;
	if (w > term->x - 2 * DIALOG_LB) w = term->x - 2 * DIALOG_LB;
	if (w < 5) w = 5;
	rw = 0;
	dlg_format_text(dlg, NULL, txt, 0, &y, w, &rw, COLOR_DIALOG_TEXT, AL_CENTER);
	y += gf_val(1, 1 * G_BFU_FONT_SIZE);
	dlg_format_buttons(dlg, NULL, dlg->items , dlg->n, 0, &y, w, &rw, AL_CENTER);
	w = rw;
	dlg->xw = rw + 2 * DIALOG_LB;
	dlg->yw = y + 2 * DIALOG_TB;
	center_dlg(dlg);
	draw_dlg(dlg);
	y = dlg->y + DIALOG_TB + gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_text(dlg, term, txt, dlg->x + DIALOG_LB, &y, w, NULL, COLOR_DIALOG_TEXT, AL_CENTER);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_buttons(dlg, term, dlg->items , dlg->n, dlg->x + DIALOG_LB, &y, w, &rw, AL_CENTER);
	mem_free(txt);
}


static void vysad_okno(struct session *ses, unsigned char *ct, struct assoc *a, int n)
{
	int i;
	struct dialog *d;
	struct memory_list *ml;

	if ((unsigned)n > (MAXINT - sizeof(struct dialog)) / sizeof(struct dialog_item) - 4) overalloc();
	d = mem_calloc(sizeof(struct dialog) + (n+3+(!anonymous)) * sizeof(struct dialog_item));
	d->title = TEXT_(T_WHAT_TO_DO);
	d->fn = vysad_dvere;
	d->udata = ct;
	d->udata2=ses;
	ml=getml(d,a,ct,NULL);

	for (i=0;i<n;i++)
	{
		unsigned char *bla=stracpy(_(TEXT_(T_OPEN_WITH),ses->term));
		add_to_strn(&bla," ");
		add_to_strn(&bla,_(a[i].label,ses->term));
		
		d->items[i].type = D_BUTTON;
		d->items[i].fn = prog_sel_open;
		d->items[i].udata = a+i;
		d->items[i].text = bla;
		add_to_ml(&ml,bla,NULL);
	}
	if (!anonymous)
	{
		d->items[i].type = D_BUTTON;
		d->items[i].fn = prog_sel_save;
		d->items[i].text = TEXT_(T_SAVE);
		i++;
	}
	d->items[i].type = D_BUTTON;
	d->items[i].fn = prog_sel_display;
	d->items[i].text = TEXT_(T_DISPLAY);
	i++;
	d->items[i].type = D_BUTTON;
	d->items[i].fn = prog_sel_cancel;
	d->items[i].gid = B_ESC;
	d->items[i].text = TEXT_(T_CANCEL);
	d->items[i+1].type = D_END;
	do_dialog(ses->term, d, ml);
}



/* deallocates a */
static void type_query(struct session *ses, unsigned char *ct, struct assoc *a, int n)
{
	unsigned char *m1;
	unsigned char *m2;
	if (ses->tq_prog) mem_free(ses->tq_prog), ses->tq_prog = NULL;

	if (n>1){vysad_okno(ses,ct,a,n);return;}
	
	if (a) ses->tq_prog = stracpy(a[0].prog), ses->tq_prog_flag_block = a[0].block, ses->tq_prog_flag_direct = direct_download_possible(ses->tq, a);
	if (a && !a[0].ask) {
		tp_open(ses);
		if (n) mem_free(a);
		if (ct) mem_free(ct);
		return;
	}
	m1 = stracpy(ct);
	if (!a) {
		if (!anonymous) msg_box(ses->term, getml(m1, NULL), TEXT_(T_UNKNOWN_TYPE), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_CONTEN_TYPE_IS), " ", m1, ".\n", TEXT_(T_DO_YOU_WANT_TO_SAVE_OR_DISLPAY_THIS_FILE), NULL, ses, 3, TEXT_(T_SAVE), tp_save, B_ENTER, TEXT_(T_DISPLAY), tp_display, 0, TEXT_(T_CANCEL), tp_cancel, B_ESC);
		else msg_box(ses->term, getml(m1, NULL), TEXT_(T_UNKNOWN_TYPE), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_CONTEN_TYPE_IS), " ", m1, ".\n", TEXT_(T_DO_YOU_WANT_TO_SAVE_OR_DISLPAY_THIS_FILE), NULL, ses, 2, TEXT_(T_DISPLAY), tp_display, B_ENTER, TEXT_(T_CANCEL), tp_cancel, B_ESC);
	} else {
		m2 = stracpy(a[0].label ? a[0].label : (unsigned char *)"");
		if (!anonymous) msg_box(ses->term, getml(m1, m2, NULL), TEXT_(T_WHAT_TO_DO), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_CONTEN_TYPE_IS), " ", m1, ".\n", TEXT_(T_DO_YOU_WANT_TO_OPEN_FILE_WITH), " ", m2, ", ", TEXT_(T_SAVE_IT_OR_DISPLAY_IT), NULL, ses, 4, TEXT_(T_OPEN), tp_open, B_ENTER, TEXT_(T_SAVE), tp_save, 0, TEXT_(T_DISPLAY), tp_display, 0, TEXT_(T_CANCEL), tp_cancel, B_ESC);
		else msg_box(ses->term, getml(m1, m2, NULL), TEXT_(T_WHAT_TO_DO), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_CONTEN_TYPE_IS), " ", m1, ".\n", TEXT_(T_DO_YOU_WANT_TO_OPEN_FILE_WITH), " ", m2, ", ", TEXT_(T_SAVE_IT_OR_DISPLAY_IT), NULL, ses, 3, TEXT_(T_OPEN), tp_open, B_ENTER, TEXT_(T_DISPLAY), tp_display, 0, TEXT_(T_CANCEL), tp_cancel, B_ESC);
	}
	if (n)mem_free(a);
	if (ct)mem_free(ct);
}

static void ses_go_to_2nd_state(struct session *ses)
{
	struct assoc *a;
	int n;
	unsigned char *ct = NULL;
	int r = plain_type(ses, ses->rq, &ct);
	if (r == 0 || r == 1 || r == 2 || r == 3) goto go;
	if (!(a = get_type_assoc(ses->term, ct, &n)) && strlen(ct) >= 4 && !casecmp(ct, "text/", 4)) {
		r = 1;
		goto go;
	}
	if (ses->tq) {
		ses_abort_1st_state_loading(ses);
		if (n) mem_free(a);
		if (ct) mem_free(ct);
		return;
	}
	(ses->tq = ses->rq)->upcall = NULL;
	ses->rq = NULL;
	ses_abort_1st_state_loading(ses);
	type_query(ses, ct, a, n);
	return;
	go:
	ses_go_forward(ses, r, ses->wtd_refresh);
	if (ct) mem_free(ct);
}

static void ses_go_back_to_2nd_state(struct session *ses)
{
	ses_go_backward(ses);
}

static void ses_finished_1st_state(struct object_request *rq, struct session *ses)
{
	if (rq->state != O_WAITING) {
		if (ses->wtd_refresh && ses->wtd_target_base && ses->wtd_target_base->refresh_timer != -1) {
			kill_timer(ses->wtd_target_base->refresh_timer);
			ses->wtd_target_base->refresh_timer = -1;
		}
	}
	switch (rq->state) {
		case O_WAITING:
			change_screen_status(ses);
			print_screen_status(ses);
			break;
		case O_FAILED:
			print_error_dialog(ses, &rq->stat, rq->url);
			ses_abort_1st_state_loading(ses);
			break;
		case O_LOADING:
		case O_INCOMPLETE:
		case O_OK:
			if (!ses->goto_position && rq->goto_position) ses->goto_position = stracpy(rq->goto_position);
			ses->wtd(ses);
			break;
	}
}

void ses_destroy_defered_jump(struct session *ses)
{
	if (ses->defered_url) mem_free(ses->defered_url), ses->defered_url = NULL;
	if (ses->defered_target) mem_free(ses->defered_target), ses->defered_target = NULL;
	ses->defered_target_base = NULL;
}

#ifdef JS
/* test if there're any running scripts */
static inline int any_running_scripts(struct f_data_c *fd)
{
	if (!fd->js) return 0;
	return (fd->js->active) || (!list_empty(fd->js->queue));
}
#else
static inline int any_running_scripts(struct f_data_c *fd)
{
	return 0;
}
#endif

/* if from_goto_dialog is 1, set prev_url to NULL */
void goto_url_f(struct session *ses, void (*state2)(struct session *), unsigned char *url, unsigned char *target, struct f_data_c *df, int data, int defer, int from_goto_dialog, int refresh)
{
	unsigned char *u, *pos;
	unsigned char *prev_url;
	void (*fn)(struct session *, unsigned char *);
	if (!state2) state2 = ses_go_to_2nd_state;
	if (ses->defered_url && defer && any_running_scripts(ses->screen)) return;
	ses_destroy_defered_jump(ses);
	if ((fn = get_external_protocol_function(url))) {
		if (proxies.only_proxies && url_bypasses_socks(url)) {
			msg_box(ses->term, NULL, TEXT_(T_ERROR), AL_CENTER, TEXT_(T_NO_PROXY), NULL, 1, TEXT_(T_CANCEL), NULL, B_ENTER | B_ESC);
			return;
		}
		fn(ses, url);
		return;
	}
	ses->reloadlevel = NC_CACHE;
	if (!(u = translate_url(url, ses->term->cwd))) {
		struct status stat = { NULL, NULL, NULL, NULL, S_BAD_URL, PRI_CANCEL, 0, NULL, NULL, NULL };
		print_error_dialog(ses, &stat, url);
		return;
	}
	if (defer && any_running_scripts(ses->screen)) {
		ses->defered_url = u;
		ses->defered_target = stracpy(target);
		ses->defered_target_base = df;
		ses->defered_data = data;
		ses->defered_seq = jsint_execute_seq++;
		return;
	}
	pos = extract_position(u);
	if (ses->wtd == state2 && !strcmp(ses->rq->orig_url, u) && !xstrcmp(ses->wtd_target, target) && ses->wtd_target_base == df) {
		mem_free(u);
		if (ses->goto_position) mem_free(ses->goto_position);
		ses->goto_position = pos;
		return;
	}
	ses_abort_1st_state_loading(ses);
	ses->wtd = state2;
	ses->wtd_target = stracpy(target);
	ses->wtd_target_base = df;
	ses->wtd_refresh = refresh;
	if (ses->goto_position) mem_free(ses->goto_position);
	ses->goto_position = pos;
	if (!from_goto_dialog && df && df->rq) prev_url = df->rq->url;
	else prev_url = NULL;   /* previous page is empty - this probably never happens, but for sure */
	if (refresh && ses->wtd == ses_go_to_2nd_state) {
		struct f_data_c *fr = find_frame(ses, ses->wtd_target, ses->wtd_target_base);
		if (fr && fr->loc) if (!strcmp(fr->loc->url, u)) ses->reloadlevel = NC_RELOAD;
	}
	request_object(ses->term, u, prev_url, PRI_MAIN, ses->reloadlevel, (void (*)(struct object_request *, void *))ses_finished_1st_state, ses, &ses->rq);
	mem_free(u);
}

/* this doesn't send referer */
void goto_url(struct session *ses, unsigned char *url)
{
	goto_url_f(ses, NULL, url, NULL, NULL, -1, 0, 1, 0);
}

/* this one sends referer */
void goto_url_not_from_dialog(struct session *ses, unsigned char *url)
{
	goto_url_f(ses, NULL, url, NULL, NULL, -1, 0, 0, 0);
}

static void ses_imgmap(struct session *ses)
{
	unsigned char *start, *end;
	struct memory_list *ml;
	struct menu_item *menu;
	struct f_data_c *fd;
	if (ses->rq->state != O_OK && ses->rq->state != O_INCOMPLETE) return;
	if (!(fd = current_frame(ses)) || !fd->f_data) return;
	if (get_file(ses->rq, &start, &end)) return;
	d_opt = &fd->f_data->opt;
	if (get_image_map(ses->rq->ce && ses->rq->ce->head ? ses->rq->ce->head : (unsigned char *)"", start, end, ses->goto_position, &menu, &ml, ses->imgmap_href_base, ses->imgmap_target_base, ses->term->spec->charset, ses->ds.assume_cp, ses->ds.hard_assume, 0)) {
		ses_abort_1st_state_loading(ses);
		return;
	}
	add_empty_window(ses->term, (void (*)(void *))freeml, ml);
	do_menu(ses->term, menu, ses);
	ses_abort_1st_state_loading(ses);
}

void goto_imgmap(struct session *ses, unsigned char *url, unsigned char *href, unsigned char *target)
{
	if (ses->imgmap_href_base) mem_free(ses->imgmap_href_base);
	ses->imgmap_href_base = href;
	if (ses->imgmap_target_base) mem_free(ses->imgmap_target_base);
	ses->imgmap_target_base = target;
	goto_url_f(ses, ses_imgmap, url, NULL, NULL, -1, 0, 0, 0);
}

void map_selected(struct terminal *term, struct link_def *ld, struct session *ses)
{
	int x = 0;
	if (ld->onclick) {
		struct f_data_c *fd = current_frame(ses);
		jsint_execute_code(fd, ld->onclick, strlen(ld->onclick), -1, -1, -1, NULL);
		x = 1;
	}
	if (ld->link) goto_url_f(ses, NULL, ld->link, ld->target, current_frame(ses), -1, x, 0, 0);
}

void go_back(struct session *ses, int num_steps)
{
	struct location *loc;
	int n;
	if (!num_steps) return;
	ses->reloadlevel = NC_CACHE;
	ses_destroy_defered_jump(ses);
	if (ses_abort_1st_state_loading(ses)) {
		change_screen_status(ses);
		print_screen_status(ses);
		return;
	}
	n = num_steps;
	if (num_steps > 0) {
		foreach(loc, ses->history) {
			if (!n--) goto have_loc;
		}
		return;
	} else {
		foreach(loc, ses->forward_history) {
			if (!++n) goto have_loc;
		}
		return;
	}
	have_loc:
	ses->wtd = ses_go_back_to_2nd_state;
	ses->wtd_num_steps = num_steps;
	request_object(ses->term, loc->url, loc->prev_url, PRI_MAIN, NC_ALWAYS_CACHE, (void (*)(struct object_request *, void *))ses_finished_1st_state, ses, &ses->rq);
}

static void reload_frame(struct f_data_c *fd, int no_cache)
{
	unsigned char *u;
	if (!list_empty(fd->subframes)) {
		struct f_data_c *fdd;
		foreach(fdd, fd->subframes) {
			reload_frame(fdd, no_cache);
		}
		return;
	}
	if (!fd->f_data || !f_is_finished(fd->f_data)) return;
	u = stracpy(fd->rq->url);
	release_object(&fd->rq);
	release_object(&fd->f_data->rq);
	request_object(fd->ses->term, u, NULL, PRI_MAIN, no_cache, (void (*)(struct object_request *, void *))fd_loaded, fd, &fd->rq);
	clone_object(fd->rq, &fd->f_data->rq);
	fd->done = 0;
	fd->parsed_done = 0;
	mem_free(u);
	jsint_destroy(fd);
}

void reload(struct session *ses, int no_cache)
{
	ses_destroy_defered_jump(ses);
	if (no_cache == -1) no_cache = ++ses->reloadlevel;
	else ses->reloadlevel = no_cache;
	reload_frame(ses->screen, no_cache);
	/*request_object(ses->term, cur_loc(ses)->url, cur_loc(ses)->prev_url, PRI_MAIN, no_cache, (void (*)(struct object_request *, void *))ses_finished_1st_state, ses, &ses->rq);*/
}

static void set_doc_view(struct session *ses)
{
	ses->screen->xp = 0;
	ses->screen->yp = gf_val(1, G_BFU_FONT_SIZE);
	ses->screen->xw = ses->term->x;
	if (ses->term->y < gf_val(2, 2 * G_BFU_FONT_SIZE)) ses->screen->yw = 0;
	else ses->screen->yw = ses->term->y - gf_val(2, 2 * G_BFU_FONT_SIZE);
}

static struct session *create_session(struct window *win)
{
	static int session_id = 1;
	struct terminal *term = win->term;
	struct session *ses;
	ses = mem_calloc(sizeof(struct session));
	init_list(ses->history);
	init_list(ses->forward_history);
	ses->term = term;
	ses->win = win;
	ses->id = session_id++;
	ses->screen = create_f_data_c(ses, NULL);
	ses->screen->xp = 0; ses->screen->xw = term->x;
	ses->screen->yp = gf_val(1, G_BFU_FONT_SIZE);
	ses->screen->yw = term->y - gf_val(2, 2 * G_BFU_FONT_SIZE);
	memcpy(&ses->ds, &dds, sizeof(struct document_setup));
	init_list(ses->format_cache);
	add_to_list(sessions, ses);
	if (first_use) {
		first_use = 0;
		msg_box(term, NULL, TEXT_(T_WELCOME), AL_CENTER | AL_EXTD_TEXT, TEXT_(T_WELCOME_TO_LINKS), "\n\n", TEXT_(T_BASIC_HELP), NULL, NULL, 1, TEXT_(T_OK), NULL, B_ENTER | B_ESC);
	}
	return ses;
}

/*vyrobi retezec znaku, ktery se posilaj skrz unix domain socket hlavni instanci
   prohlizece
   cp=cislo session odkud se ma kopirovat (kdyz se klikne na "open new window")
   url=url kam se ma jit (v pripade kliknuti na "open link in new window" nebo pusteni linksu z prikazove radky s url)
   framename=jmeno ramu, ktery se vytvori

   vraci sekvenci bytu a delku
 */
void *create_session_info(int cp, unsigned char *url, unsigned char *framename, int *ll)
{
	size_t l = strlen(url);
	size_t l1 = framename?strlen(framename):0;
	int *i;
	if (l > MAXINT || l1 > MAXINT) overalloc();
	if (framename&&!strcmp(framename,"_blank")) l1=0;
	*ll = 3 * sizeof(int) + l + l1;
	i = mem_alloc(3 * sizeof(int) + l + l1);
	i[0] = cp;
	i[1] = l;
	i[2] = l1;
	memcpy(i + 3, url, l);
	if (l1) memcpy((unsigned char*)(i + 3) + l, framename, l1);
	return i;
}

/* dostane data z create_session_info a nainicializuje podle nich session
   vraci -1 pokud jsou data vadna
 */
static int read_session_info(struct session *ses, void *data, int len)
{
	unsigned char *h;
	int cpfrom, sz, sz1;
	struct session *s;
	if (len < 3 * (int)sizeof(int)) return -1;
	cpfrom = *(int *)data;
	sz = *((int *)data + 1);
	sz1= *((int *)data + 2);
	foreach(s, sessions) if (s->id == cpfrom) {
		if (!list_empty(s->history)) {
			struct location *loc = s->history.next;
			if (loc->url) goto_url(ses, loc->url);
		}
		return 0;
	}
	if (sz1) {
		unsigned char *tgt;
		if (len<3*(int)sizeof(int)+sz+sz1) goto bla;
		if ((unsigned)sz1 >= MAXINT) overalloc();
		tgt=mem_alloc(sz1+1);
		memcpy(tgt, (unsigned char*)((int*)data+3)+sz,sz1);
		tgt[sz1]=0;
		if (ses->wanted_framename) mem_free(ses->wanted_framename), ses->wanted_framename=NULL;
		ses->wanted_framename=tgt;
	}
	bla:
	if (sz) {
		unsigned char *u, *uu;
		if (len < 3 * (int)sizeof(int) + sz) return 0;
		if ((unsigned)sz >= MAXINT) overalloc();
		u = mem_alloc(sz + 1);
		memcpy(u, (int *)data + 3, sz);
		u[sz] = 0;
		uu = decode_url(u);
		goto_url(ses, uu);
		mem_free(u);
		mem_free(uu);
	} else if ((h = getenv("WWW_HOME")) && *h) {
		goto_url(ses, h);
	}
#if defined(HAVE_NETINET_DHCP_H) && defined(HAVE_DHCP_OPTION)
	else {
		unsigned char b[4];
		if (dhcp_option(PF_INET, BOOTP_WWW_SERVERS, b, 4) == 4) {
			unsigned char s[16];
			sprintf(s, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
			goto_url(ses, s);
		}
	}
#endif
	return 0;
}

void destroy_session(struct session *ses)
{
	struct download *d;
	foreach(d, downloads) if (d->ses == ses && d->prog) {
		d = d->prev;
		abort_download(d->next);
	}
	ses_abort_1st_state_loading(ses);
	reinit_f_data_c(ses->screen);
	mem_free(ses->screen);
	while (!list_empty(ses->format_cache)) {
		struct f_data *f = ses->format_cache.next;
		del_from_list(f);
		destroy_formatted(f);
	}
	while (!list_empty(ses->history)) destroy_location(ses->history.next);
	clear_forward_history(ses);
	if (ses->st) mem_free(ses->st);
	if (ses->st_old) mem_free(ses->st_old);
	if (ses->default_status)mem_free(ses->default_status);
	if (ses->dn_url) mem_free(ses->dn_url);
	if (ses->search_word) mem_free(ses->search_word);
	if (ses->last_search_word) mem_free(ses->last_search_word);
	if (ses->imgmap_href_base) mem_free(ses->imgmap_href_base);
	if (ses->imgmap_target_base) mem_free(ses->imgmap_target_base);
	if (ses->wanted_framename) mem_free(ses->wanted_framename);

	release_object(&ses->tq);
	if (ses->tq_prog) mem_free(ses->tq_prog);

	ses_destroy_defered_jump(ses);

	del_from_list(ses);
}

void win_func(struct window *win, struct event *ev, int fw)
{
	struct session *ses = win->data;
	switch ((int)ev->ev) {
		case EV_ABORT:
			if (ses) destroy_session(ses);
			break;
		case EV_INIT:
			if (!(ses = win->data = create_session(win)) || read_session_info(ses, (char *)ev->b + sizeof(int), *(int *)ev->b)) {
				register_bottom_half((void (*)(void *))destroy_terminal, win->term);
				return;
			}
		case EV_RESIZE:
			if (ses->st_old) mem_free(ses->st_old), ses->st_old = NULL;
			GF(set_window_pos(win, 0, 0, ev->x, ev->y));
			set_doc_view(ses);
			html_interpret_recursive(ses->screen);
			draw_fd(ses->screen);
			break;
		case EV_REDRAW:
			if (ses->st_old) mem_free(ses->st_old), ses->st_old = NULL;
			draw_formatted(ses);
			break;
		case EV_MOUSE:
#ifdef G
			if (F) set_window_ptr(win, ev->x, ev->y);
#endif
			/* fall through ... */
		case EV_KBD:
			send_event(ses, ev);
			break;
		default:
			error("ERROR: unknown event");
	}
}

/*
  Gets the url being viewed by this session. Writes it into str.
  A maximum of str_size bytes (including null) will be written.
*/
unsigned char *get_current_url(struct session *ses, unsigned char *str, size_t str_size) {
	unsigned char *here, *end_of_url;
	size_t url_len = 0;

	/* Not looking at anything */
	if (list_empty(ses->history))
		return NULL;

	here = cur_loc(ses)->url;

	/* Find the length of the url */
	if ((end_of_url = strchr(here, POST_CHAR))) {
		url_len = (size_t)(end_of_url - (unsigned char *)here);
	} else {
		url_len = strlen(here);
	}

	/* Ensure that the url size is not greater than str_size */
	if (url_len >= str_size)
			url_len = str_size - 1;

	safe_strncpy(str, here, url_len + 1);

	return str;
}


/*
  Gets the title of the page being viewed by this session. Writes it into str.
  A maximum of str_size bytes (including null) will be written.
*/
unsigned char *get_current_title(struct session *ses, unsigned char *str, size_t str_size) {
	struct f_data_c *fd;
	fd = (struct f_data_c *)current_frame(ses);

	/* Ensure that the title is defined */
	if (!fd || !fd->f_data)
		return NULL;

	return safe_strncpy(str, fd->f_data->title, str_size);
}

#if 0
/*
  Gets the url of the link currently selected. Writes it into str.
  A maximum of str_size bytes (including null) will be written.
*/
unsigned char *get_current_link_url(struct session *ses, unsigned char *str, size_t str_size) {
	struct f_data_c *fd;
    struct link *l;
	
	fd = (struct f_data_c *)current_frame(ses);
	/* What the hell is an 'fd'? */
	if (!fd)
		return NULL;
	
	/* Nothing selected? */
    if (fd->vs->current_link == -1 || fd->vs->current_link >= fd->f_data->nlinks)
		return NULL;

    l = &fd->f_data->links[fd->vs->current_link];
	/* Only write a link */
    if (l->type != L_LINK)
		return NULL;
	
	return safe_strncpy(str, l->where, str_size);
}
#endif
