/* html_gr.c
 * HTML parser in graphics mode
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "cfg.h"

#define format format_

#ifdef G

#include "links.h"

static int g_nobreak;

static int get_real_font_size(int size)
{
	int fs=d_opt->font_size;
	
	if (size < 1) size = 1;
	if (size > 7) size = 7;
	switch (size) {
		case 1:	return (14*fs)>>4;
		case 2: return (15*fs)>>4;
		case 3: return (16*fs)>>4;
		case 4: return (19*fs)>>4;
		case 5: return (22*fs)>>4;
		case 6: return (25*fs)>>4;
		case 7: return (28*fs)>>4;
	}
	return 0;
}

struct background *get_background(unsigned char *bg, unsigned char *bgcolor)
{
	struct background *b;
	struct rgb r;
	b = mem_alloc(sizeof(struct background));
	/* !!! FIXME: background image */
	{
		b->img = 0;
		if (bgcolor && !decode_color(bgcolor, &r)) {
			b->u.sRGB=(r.r << 16) + (r.g << 8) + r.b;
		} else {
			b->u.sRGB=(d_opt->default_bg.r << 16) + (d_opt->default_bg.g << 8) + d_opt->default_bg.b;
		}
	}
	return b;
}

static void g_put_chars(void *, unsigned char *, int);

/* Returns 0 to 2550 */
static int gray (int r, int g, int b)
{
	return r*3+g*6+b;
}

/* Tells if two colors are too near to be legible one on another */
/* 1=too near 0=not too near */
static int too_near(int r1, int g1, int b1, int r2, int g2, int b2)
{
	int gray1,gray2;

	gray1=gray(r1, g1, b1);
	gray2=gray(r2, g2, b2);
	if (gray1<=gray2)
		return gray2-gray1<=400;
	else
		return gray1-gray2<=400;
}

/* Fixes foreground based on background */
static void separate_fg_bg(int *fgr, int *fgg, int *fgb
	, int bgr, int bgg, int bgb)
{
	if (too_near(*fgr, *fgg, *fgb, bgr, bgg, bgb)){
		*fgr=255-bgr;
		*fgg=255-bgg;
		*fgb=255-bgb;
	}else return;
	if (too_near(*fgr, *fgg, *fgb, bgr, bgg, bgb)){
		if (gray(bgr, bgg, bgb)<=1275)
			*fgr=*fgg=*fgb=255;
		else
			*fgr=*fgg=*fgb=0;
	}
}

static unsigned char *make_html_font_name(int attr)
{
	unsigned char *str;
	int len;
	
	str=init_str();len=0;
	add_to_str(&str, &len, G_HTML_DEFAULT_FAMILY);
	add_to_str(&str, &len, attr & AT_BOLD ? "-bold" : "-medium");
	add_to_str(&str, &len, attr & AT_ITALIC ? "-italic-serif" :
			"-roman-serif");
	add_to_str(&str, &len, attr & AT_FIXED ? "-mono" : "-vari");
	return str;
}

static struct style *get_style_by_ta(struct text_attrib *ta)
{
	int fg_r,fg_g,fg_b; /* sRGB 0-255 values */
	int fs = get_real_font_size(ta->fontsize);
	struct style*stl;
	unsigned char *fontname;

	fg_r=ta->fg.r;
	fg_g=ta->fg.g;
	fg_b=ta->fg.b;
	separate_fg_bg(&fg_r,&fg_g,&fg_b,ta->bg.r,ta->bg.g,ta->bg.b);
	stl = g_get_style((fg_r << 16) + (fg_g << 8) + fg_b, (ta->bg.r << 16) +
			(ta->bg.g << 8) + ta->bg.b, fs, 
			fontname=make_html_font_name(ta->attr),
			ta->attr & AT_UNDERLINE ? FF_UNDERLINE : 0);
	mem_free(fontname);
	return stl;
}

#define rm(x) ((x).width - (x).rightmargin * G_HTML_MARGIN > 0 ? (x).width - (x).rightmargin * G_HTML_MARGIN : 0)

#ifndef SPAD
static inline int pw2(int a)
{
	int x = 1;
	while (x < a && x) {
		if ((unsigned)x > MAXINT / 2) overalloc();
		x <<= 1;
	}
	return x;
}
#else
static inline int pw2(int a)
{
	return a;
}
#endif

void flush_pending_line_to_obj(struct g_part *p, int minheight)
{
	int i, pp, pos, w, lbl;
	struct g_object_line *l = p->line;
	struct g_object_area *a;
	if (!l) {
		return;
	}
	for (i = l->n_entries - 1; i >= 0; i--) {
		struct g_object_text *go = (struct g_object_text *)l->entries[i];
		if (go->draw == g_text_draw) {
			int l;
			while ((l = strlen(go->text)) && go->text[l - 1] == ' ') go->text[l - 1] = 0, go->xw -= g_char_width(go->style, ' ');
			if (go->xw < 0) internal("xw(%d) < 0", go->xw);
		}
		if (!go->xw) continue;
		break;
	}
	scan_again:
	pp = 0;
	w = minheight;
	lbl = 0;
	for (i = 0; i < l->n_entries; i++) {
		int yy = l->entries[i]->y;
		if (yy >= G_OBJ_ALIGN_SPECIAL) yy = 0;
		pp += l->entries[i]->xw;
		if (l->entries[i]->xw && l->entries[i]->yw + yy > w) w = l->entries[i]->yw + yy;
		if (yy < lbl) lbl = yy;
	}
	if (lbl < 0) {
		for (i = 0; i < l->n_entries; i++) {
			if (l->entries[i]->y < G_OBJ_ALIGN_SPECIAL) l->entries[i]->y -= lbl;
		}
		goto scan_again;
	}
	if (par_format.align == AL_CENTER) pos = (rm(par_format) + par_format.leftmargin * G_HTML_MARGIN - pp) / 2;
	else if (par_format.align == AL_RIGHT) pos = rm(par_format) - pp;
	else pos = par_format.leftmargin * G_HTML_MARGIN;
	if (pos < par_format.leftmargin * G_HTML_MARGIN) pos = par_format.leftmargin * G_HTML_MARGIN;
	pp = pos;
	for (i = 0; i < l->n_entries; i++) {
		l->entries[i]->x = pp;
		pp += l->entries[i]->xw;
		if (l->entries[i]->y < G_OBJ_ALIGN_SPECIAL) {
			l->entries[i]->y = w - l->entries[i]->yw - l->entries[i]->y;
		} else if (l->entries[i]->y == G_OBJ_ALIGN_TOP) {
			l->entries[i]->y = 0;
		} else if (l->entries[i]->y == G_OBJ_ALIGN_MIDDLE) {
			l->entries[i]->y = (w - l->entries[i]->yw + 1) / 2;
		}
	}
	l->x = 0;
	l->xw = par_format.width;
	l->yw = w;
	l->y = p->cy;
	if (l->xw > p->root->xw) p->root->xw = l->xw;
	p->root->yw = p->cy += w;
	p->root->n_lines++;
	if ((unsigned)p->root->n_lines > (MAXINT - sizeof(struct g_object_area)) / 2 / sizeof(struct g_object_text *) + 1) overalloc();
	a = mem_realloc(p->root, sizeof(struct g_object_area) + sizeof(struct g_object_text *) * pw2(p->root->n_lines + 1));
		/* note: +1 is for extend_area */
	p->root = a;
	p->root->lines[p->root->n_lines - 1] = l;
	p->line = NULL;
	p->w.pos = 0;
	p->w.last_wrap = NULL;
	p->w.last_wrap_obj = NULL;
}

void add_object_to_line(struct g_part *pp, struct g_object_line **lp, struct g_object *go)
{
	struct g_object_line *l;
	if (go && (go->xw < 0 || go->yw < 0)) {
		internal("object has negative size: %d,%d", go->xw, go->yw);
		return;
	}
	if (!*lp) {
		l = mem_calloc(sizeof(struct g_object_line) + sizeof(struct g_object_text *));
		l->mouse_event = g_line_mouse;
		l->draw = g_line_draw;
		l->destruct = g_line_destruct;
		l->get_list = g_line_get_list;
		/*l->x = 0;
		l->y = 0;
		l->xw = 0;
		l->yw = 0;*/
		l->bg = pp->root->bg;
		if (!go) {
			*lp = l;
			return;
		}
		l->n_entries = 1;
	} else {
		if (!go) return;
		(*lp)->n_entries++;
		if ((unsigned)(*lp)->n_entries > (MAXINT - sizeof(struct g_object_line)) / sizeof(struct g_object *)) overalloc();
		l = mem_realloc(*lp, sizeof(struct g_object_line) + sizeof(struct g_object *) * (*lp)->n_entries);
		*lp = l;
	}
	l->entries[l->n_entries - 1] = go;
	*lp = l;
	if (pp->cx == -1) pp->cx = par_format.leftmargin * G_HTML_MARGIN;
	if (go->xw) {
		pp->cx += pp->cx_w;
		pp->cx_w = 0;
		pp->cx += go->xw;
	}
}

void flush_pending_text_to_line(struct g_part *p)
{
	struct g_object_text *t = p->text;
	if (!t) return;
	add_object_to_line(p, &p->line, (struct g_object *)t);
	p->text = NULL;
}

static void split_line_object(struct g_part *p, struct g_object_text *text, unsigned char *ptr)
{
	struct g_object_text *t2;
	struct g_object_line *l2;
	int n;
	if (!ptr) {
		if (p->line && p->line->n_entries && (struct g_object *)text == p->line->entries[p->line->n_entries - 1]) {
			flush_pending_line_to_obj(p, 0);
			goto wwww;
		}
		t2 = NULL;
		goto nt2;
	}
	t2 = mem_calloc(sizeof(struct g_object_text) + strlen(ptr));
	t2->mouse_event = g_text_mouse;
	t2->draw = g_text_draw;
	t2->destruct = g_text_destruct;
	t2->get_list = NULL;
	if (*ptr == ' ') {
		strcpy(t2->text, ptr + 1);
		*ptr = 0;
		/*debug("split: (%s)(%s)", text->text, ptr + 1);*/
	} else {
		strcpy(t2->text, ptr);
		ptr[0] = '-';
		ptr[1] = 0;
	}
	t2->y = text->y;
	t2->style = g_clone_style(text->style);
	t2->bg = text->bg;
	t2->link_num = text->link_num;
	t2->link_order = text->link_order;
	text->xw = g_text_width(text->style, text->text);
	nt2:
	if (p->line) for (n = 0; n < p->line->n_entries; n++) if (p->line->entries[n] == (struct g_object *)text) goto found;
	if (text != p->text) {
		internal("split_line_object: bad wrap");
		t2->destruct(t2);
		mem_free(t2);
		return;
	}
	if (0) {
		int nn;
		found:
		n += !ptr;
		l2 = mem_calloc(sizeof(struct g_object_line) + (p->line->n_entries - n) * sizeof(struct g_object_text *));
		l2->mouse_event = g_line_mouse;
		l2->draw = g_line_draw;
		l2->destruct = g_line_destruct;
		l2->get_list = g_line_get_list;
		l2->bg = p->root->bg;
		l2->n_entries = p->line->n_entries - n;
		l2->entries[0] = (struct g_object *)t2;
		memcpy(&l2->entries[!!ptr], p->line->entries + n + !!ptr, (l2->n_entries - !!ptr) * sizeof(struct g_object_text *));
		p->line->n_entries = n + !!ptr;
		flush_pending_line_to_obj(p, 0);
		p->line = l2;
		if (ptr) {
			t2->xw = g_text_width(t2->style, t2->text);
			t2->yw = text->yw;
			p->w.pos = 0;
		}
		for (nn = 0; nn < l2->n_entries; nn++) {
			p->w.pos += l2->entries[nn]->xw;	/* !!! FIXME: nastav last_wrap */
			/*debug("a1: %d (%s)", l2->entries[nn]->xw, tt->text);*/
		}
		wwww:
		if (p->text) p->w.pos += g_text_width(p->text->style, p->text->text);
		/*debug("%d", p->w.pos);*/
	} else {
		flush_pending_text_to_line(p);
		flush_pending_line_to_obj(p, 0);
		p->line = NULL;
		t2->xw = g_text_width(t2->style, t2->text);
		t2->yw = text->yw;
		p->text = t2;
		p->pending_text_len = -1;
		p->w.pos = t2->xw;
		p->cx_w = g_char_width(t2->style, ' ');
	}
	p->w.last_wrap = NULL;
	p->w.last_wrap_obj = NULL;
	t2 = p->text;
	if (t2 && *t2->text && t2->text[strlen(t2->text) - 1] == ' ') {
		p->w.last_wrap = &t2->text[strlen(t2->text) - 1];
		p->w.last_wrap_obj = t2;
	}
}

void add_object(struct g_part *p, struct g_object *o)
{
	g_nobreak = 0;
	flush_pending_text_to_line(p);
	p->w.width = rm(par_format) - par_format.leftmargin * G_HTML_MARGIN;
	if (p->w.pos + o->xw > p->w.width) flush_pending_line_to_obj(p, 0);
	add_object_to_line(p, &p->line, o);
	p->w.last_wrap = NULL;
	p->w.last_wrap_obj = o;
	p->w.pos += o->xw;
	/*
	if (p->w.pos > p->w.width && p->w.last_wrap_obj) {
		split_line_object(p, p->w.last_wrap_obj, p->w.last_wrap);
	}
	*/
}

static void g_line_break(void *p_)
{
	struct g_part *p = p_;
	if (g_nobreak) {
		g_nobreak = 0;
		return;
	}
	flush_pending_text_to_line(p);
	if (!p->line) {
		add_object_to_line(p, &p->line, NULL);
		flush_pending_line_to_obj(p, get_real_font_size(format.fontsize));
	} else /*if (p->w.pos || par_format.align == AL_NO)*/ {
		flush_pending_line_to_obj(p, 0);
	}
	if (p->cx > p->xmax) p->xmax = p->cx;
	p->cx = -1;
	p->cx_w = 0;
}

/* SHADOWED IN html_form_control */
static void g_html_form_control(struct g_part *p, struct form_control *fc)
{
	if (!p->data) {
		/*destroy_fc(fc);
		mem_free(fc);*/
		add_to_list(p->uf, fc);
		return;
	}
	fc->g_ctrl_num = g_ctrl_num++;
	if (fc->type == FC_TEXT || fc->type == FC_PASSWORD || fc->type == FC_TEXTAREA) {
		unsigned char *dv = convert_string(convert_table, fc->default_value, strlen(fc->default_value), d_opt);
		if (dv) {
			mem_free(fc->default_value);
			fc->default_value = dv;
		}
		/*
		for (i = 0; i < fc->nvalues; i++) if ((dv = convert_string(convert_table, fc->values[i], strlen(fc->values[i]), d_opt))) {
			mem_free(fc->values[i]);
			fc->values[i] = dv;
		}
		*/
	}
	if (fc->type == FC_TEXTAREA) {
		unsigned char *p;
		for (p = fc->default_value; p[0]; p++) if (p[0] == '\r') {
			if (p[1] == '\n') memmove(p, p + 1, strlen(p)), p--;
			else p[0] = '\n';
		}
	}
	add_to_list(p->data->forms, fc);
}

static struct link **putchars_link_ptr = NULL;

/* Probably releases clickable map */
void release_image_map(struct image_map *map)
{
	int i;
	if (!map) return;
	for (i = 0; i < map->n_areas; i++) mem_free(map->area[i].coords);
	mem_free(map);
}

int is_in_area(struct map_area *a, int x, int y)
{
	int i;
	int over;
	switch (a->shape) {
		case SHAPE_DEFAULT:
			return 1;
		case SHAPE_RECT:
			return a->ncoords >= 4 && x >= a->coords[0] && y >= a->coords[1] && x < a->coords[2] && y < a->coords[3];
		case SHAPE_CIRCLE:
			return a->ncoords >= 3 && (a->coords[0]-x)*(a->coords[0]-x)+(a->coords[1]-y)*(a->coords[1]-y) <= a->coords[2]*a->coords[2];
		case SHAPE_POLY:
			over = 0;
			if (a->ncoords >= 4) for (i = 0; i + 1 < a->ncoords; i += 2) {
				int x1, x2, y1, y2;
				x1 = a->coords[i];
				y1 = a->coords[i + 1];
				x2 = a->coords[0];
				y2 = a->coords[1];
				if (i + 3 < a->ncoords) {
					x2 = a->coords[i + 2];
					y2 = a->coords[i + 3];
				}
				if (y1 > y2) {
					int sw;
					sw = x1; x1 = x2; x2 = sw;
					sw = y1; y1 = y2; y2 = sw;
				}
				if (y >= y1 && y < y2) {
					int po = 10000 * (y - y1) / (y2 - y1);
					int xs = x1 + (x2 - x1) * po / 10000;
					if (xs >= x) over++;
				}
			}
			return over & 1;
		default:
			internal("is_in_area: bad shape: %d", a->shape);
	}
	return 0;
}

/* The size is requested in im->xsize and im->ysize. <0 means
 * not specified. Autoscale is requested in im->autoscale.
 * If autoscale is specified, im->xsize and im->ysize must
 * be >0. */
static void do_image(struct g_part *p, struct image_description *im)
{
	struct g_object_image *io;
	struct link *link;
	link = NULL;
	putchars_link_ptr = &link;
	g_put_chars(p, NULL, 0);
	putchars_link_ptr = NULL;
	if (!link) im->link_num = -1;
	else {
		im->link_num = link - p->data->links;
		im->link_order = link->obj_order++;
		if (link->img_alt) mem_free(link->img_alt);
		link->img_alt = stracpy(im->alt);
	}
	io = insert_image(p, im);
	if (!io) goto ab;
	io->ismap = im->ismap;
	add_object(p, (struct g_object *)io);
	if (im->usemap && p->data) {
		unsigned char *tag = extract_position(im->usemap);
		struct additional_file *af = request_additional_file(current_f_data, im->usemap);
		af->need_reparse = 1;
		if (af->rq && (af->rq->state == O_LOADING || af->rq->state == O_INCOMPLETE || af->rq->state == O_OK) && af->rq->ce) {
			struct memory_list *ml;
			struct menu_item *menu;
			struct cache_entry *ce = af->rq->ce;
			unsigned char *start, *end;
			int i;
			struct image_map *map;
			if (get_file(af->rq, &start, &end)) goto ft;
			if (start == end) goto ft;
			if (get_image_map(ce->head, start, end, tag, &menu, &ml, format.href_base, format.target_base, 0, 0, 0, 1)) goto ft;
			map = mem_alloc(sizeof(struct image_map));
			map->n_areas = 0;
			for (i = 0; menu[i].text; i++) {
				struct link_def *ld = menu[i].data;
				struct map_area *a;
				struct link *link;
				int shape = !ld->shape || !*ld->shape ? SHAPE_RECT : !strcasecmp(ld->shape, "default") ? SHAPE_DEFAULT : !strcasecmp(ld->shape, "rect") ? SHAPE_RECT : !strcasecmp(ld->shape, "circle") ? SHAPE_CIRCLE : !strcasecmp(ld->shape, "poly") || !strcasecmp(ld->shape, "polygon") ? SHAPE_POLY : -1;
				if (shape == -1) continue;
				if ((unsigned)map->n_areas > (MAXINT - sizeof(struct image_map)) / sizeof(struct map_area) - 1) overalloc();
				map = mem_realloc(map, sizeof(struct image_map) + (map->n_areas + 1) * sizeof(struct map_area));
				a = &map->area[map->n_areas++];
				a->shape = shape;
				a->coords = DUMMY;
				a->ncoords = 0;
				if (ld->coords) {
					unsigned char *p = ld->coords;
					int num;
					next_coord:
					num = 0;
					while (*p && (*p < '0' || *p > '9')) p++;
					if (!*p) goto noc;
					while (*p >= '0' && *p <= '9' && num < 10000000) num = num * 10 + *p - '0', p++;
					if (*p == '.') {
						p++;
						while (*p >= '0' && *p <= '9') p++;
					}
					if (*p == '%' && num < 1000) {
						int m = io->xw < io->yw ? io->xw : io->yw;
						num = num * m / 100;
						p++;
					} else num = num * d_opt->image_scale / 100;
					if ((unsigned)a->ncoords > MAXINT / sizeof(int) - 1) overalloc();
					a->coords = mem_realloc(a->coords, (a->ncoords + 1) * sizeof(int));
					a->coords[a->ncoords++] = num;
					goto next_coord;
				}
				noc:
				if (!(link = new_link(p->data))) a->link_num = -1;
				else {
					link->pos = DUMMY;
					link->type = L_LINK;
					link->where = stracpy(ld->link);
					link->target = stracpy(ld->target);
					link->img_alt = stracpy(ld->label);
					link->where_img = stracpy(im->url);
#ifdef JS
					if (ld->onclick || ld->ondblclick || ld->onmousedown || ld->onmouseup || ld->onmouseover || ld->onmouseout || ld->onmousemove) {
						create_js_event_spec(&link->js_event);
						link->js_event->click_code = stracpy(ld->onclick);
						link->js_event->dbl_code = stracpy(ld->ondblclick);
						link->js_event->down_code = stracpy(ld->onmousedown);
						link->js_event->up_code = stracpy(ld->onmouseup);
						link->js_event->over_code = stracpy(ld->onmouseover);
						link->js_event->out_code = stracpy(ld->onmouseout);
						link->js_event->move_code = stracpy(ld->onmousemove);
					}
#endif
					a->link_num = link - p->data->links;
				}
				if (last_link) mem_free(last_link), last_link = NULL;
			}
			io->map = map;
			freeml(ml);
			ft:;
		}
		if (tag) mem_free(tag);
	}
	ab:;
}

static void g_hr(struct g_part *gp, struct hr_param *hr)
{
	unsigned char bgstr[8];
	struct g_object_line *o;
	o = mem_calloc(sizeof(struct g_object_line));
	o->mouse_event = g_line_mouse;
	o->draw = g_line_draw;
	o->destruct = g_line_bg_destruct;
	o->get_list = g_line_get_list;
	/*o->x = 0;
	o->y = 0;*/
	o->xw = hr->width;
	o->yw = hr->size;
	table_bg(&format, bgstr);
	o->bg = get_background(NULL, bgstr);
	o->n_entries = 0;
	flush_pending_text_to_line(gp);
	/*flush_pending_line_to_obj(gp, get_real_font_size(format.fontsize));*/
	add_object_to_line(gp, &gp->line, (struct g_object *)o);
	line_breax = 0;
	gp->cx = -1;
	gp->cx_w = 0;
}


static void *g_html_special(void *p_, int c, ...)
{
	struct g_part *p = p_;
	va_list l;
	unsigned char *t;
	struct form_control *fc;
	struct frameset_param *fsp;
	struct frame_param *fp;
	struct image_description *im;
	struct g_object_tag *tag;
	struct refresh_param *rp;
	struct hr_param *hr;
	va_start(l, c);
	switch (c) {
		case SP_TAG:
			t = va_arg(l, unsigned char *);
			va_end(l);
			/* not needed to convert %AB here because html_tag will be called anyway */
			tag = mem_calloc(sizeof(struct g_object_tag) + strlen(t) + 1);
			tag->mouse_event = g_dummy_mouse;
			tag->draw = g_dummy_draw;
			tag->destruct = g_tag_destruct;
			strcpy(tag->name, t);
			flush_pending_text_to_line(p);
			add_object_to_line(p, &p->line, (struct g_object *)tag);
			break;
		case SP_CONTROL:
			fc = va_arg(l, struct form_control *);
			va_end(l);
			g_html_form_control(p, fc);
			break;
		case SP_TABLE:
			va_end(l);
			return convert_table;
		case SP_USED:
			va_end(l);
			return (void *)(my_intptr_t)!!p->data;
		case SP_FRAMESET:
			fsp = va_arg(l, struct frameset_param *);
			va_end(l);
			return create_frameset(p->data, fsp);
		case SP_FRAME:
			fp = va_arg(l, struct frame_param *);
			va_end(l);
			create_frame(fp);
			break;
		case SP_SCRIPT:
			t = va_arg(l, unsigned char *);
			va_end(l);
			if (p->data) process_script(p->data, t);
			break;
		case SP_IMAGE:
			im = va_arg(l, struct image_description *);
			va_end(l);
			do_image(p, im);
			break;
		case SP_NOWRAP:
			va_end(l);
			break;
		case SP_REFRESH:
			rp = va_arg(l, struct refresh_param *);
			va_end(l);
			html_process_refresh(p->data, rp->url, rp->time);
			break;
		case SP_SET_BASE:
			t = va_arg(l, unsigned char *);
			va_end(l);
			if (p->data) set_base(p->data, t);
			break;
		case SP_HR:
			hr = va_arg(l, struct hr_param *);
			va_end(l);
			g_hr(p, hr);
			break;
		default:
			va_end(l);
			internal("html_special: unknown code %d", c);
	}
	return NULL;
}

static unsigned char to_je_ale_prasarna[] = "";

static unsigned char *cached_font_face = to_je_ale_prasarna;
static struct text_attrib_beginning ta_cache = { -1, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, 0, 0 };

static void g_put_chars(void *p_, unsigned char *s, int l)
{
	struct g_part *p = p_;
	struct g_object_text *t = NULL;
	struct link *link;
	int ptl;
	if (putchars_link_ptr) {
		link = NULL;
		goto check_link;
	}
	while (par_format.align != AL_NO && p->cx == -1 && l && *s == ' ') s++, l--;
	if (!l) return;
	g_nobreak = 0;
	if (p->cx < par_format.leftmargin * G_HTML_MARGIN) p->cx = par_format.leftmargin * G_HTML_MARGIN;
	if (html_format_changed) {
		if (memcmp(&ta_cache, &format, sizeof(struct text_attrib_beginning)) || xstrcmp(cached_font_face, format.fontface) || cached_font_face == to_je_ale_prasarna ||
	xstrcmp(format.link, last_link) || xstrcmp(format.target, last_target) ||
	    xstrcmp(format.image, last_image) || format.form != last_form
	    || ((format.js_event || last_js_event) && compare_js_event_spec(format.js_event, last_js_event))	) {
			/*if (!html_format_changed) internal("html_format_changed not set");*/
			flush_pending_text_to_line(p);
			if (xstrcmp(cached_font_face, format.fontface) || cached_font_face == to_je_ale_prasarna) {
				if (cached_font_face && cached_font_face != to_je_ale_prasarna) mem_free(cached_font_face);
				cached_font_face = stracpy(format.fontface);
			}
			memcpy(&ta_cache, &format, sizeof(struct text_attrib_beginning));
			if (p->current_style) g_free_style(p->current_style);
			p->current_style = get_style_by_ta(&format);
		}
		html_format_changed = 0;
	}
	/*if (p->cx <= par_format.leftmargin * G_HTML_MARGIN && *s == ' ' && par_format.align != AL_NO) s++, l--;*/
	if (!p->text) {
		link = NULL;
		t = mem_calloc(sizeof(struct g_object_text) + ALLOC_GR);
		t->mouse_event = g_text_mouse;
		t->draw = g_text_draw;
		t->destruct = g_text_destruct;
		t->get_list = NULL;
		/*t->style = get_style_by_ta(format);*/
		t->style = g_clone_style(p->current_style);
		t->bg = NULL; /* FIXME!!! */
		t->yw = t->style->height;
		/*t->xw = 0;
		t->y = 0;*/
		if (format.baseline) {
			if (format.baseline < 0) t->y = -(t->style->height / 3);
			if (format.baseline > 0) t->y = get_real_font_size(format.baseline) - (t->style->height / 2);
		}
		check_link:
		if (last_link || last_image || last_form || format.link || format.image || format.form 
		|| format.js_event || last_js_event
		) goto process_link;
		back_link:
		if (putchars_link_ptr) {
			*putchars_link_ptr = link;
			return;
		}

		if (!link) t->link_num = -1;
		else {
			t->link_num = link - p->data->links;
			t->link_order = link->obj_order++;
		}

		t->text[0] = 0;
		p->pending_text_len = 0;
		p->text = t;
	}
	if (p->pending_text_len == -1) {
		p->pending_text_len = strlen(p->text->text);
		ptl = p->pending_text_len;
		if (!ptl) ptl = 1;
		goto a1;
	}
	ptl = p->pending_text_len;
	if (!ptl) ptl = 1;
	if (((ptl + ALLOC_GR - 1) & ~(ALLOC_GR - 1)) != ((ptl + l + ALLOC_GR - 1) & ~(ALLOC_GR - 1))) a1: {
		struct g_object_text *t;
		if ((unsigned)l > MAXINT) overalloc();
		if ((unsigned)ptl + (unsigned)l > MAXINT - ALLOC_GR) overalloc();
		t = mem_realloc(p->text, sizeof(struct g_object_text) + ((ptl + l + ALLOC_GR - 1) & ~(ALLOC_GR - 1)));
		if (p->w.last_wrap >= p->text->text && p->w.last_wrap < p->text->text + p->pending_text_len) p->w.last_wrap += (unsigned char *)t - (unsigned char *)p->text;
		if (p->w.last_wrap_obj == p->text) p->w.last_wrap_obj = t;
		p->text = t;
	}
	memcpy(p->text->text + p->pending_text_len, s, l), p->text->text[p->pending_text_len += l] = 0;
	p->text->xw += g_text_width(p->text->style, p->text->text + p->pending_text_len - l); /* !!! FIXME: move to g_wrap_text */
	if (par_format.align != AL_NO) {
		p->w.text = p->text->text + p->pending_text_len - l;
		p->w.style = p->text->style;
		p->w.obj = p->text;
		p->w.width = rm(par_format) - par_format.leftmargin * G_HTML_MARGIN;
		if (p->w.width < 0) p->w.width = 0;
		if (!g_wrap_text(&p->w)) {
			split_line_object(p, p->w.last_wrap_obj, p->w.last_wrap);
		}
	}
	return;

	/* !!! WARNING: THE FOLLOWING CODE IS SHADOWED IN HTML_R.C */

	process_link:
	if ((last_link /*|| last_target*/ || last_image || last_form) &&
	    !putchars_link_ptr &&
	    !xstrcmp(format.link, last_link) && !xstrcmp(format.target, last_target) &&
	    !xstrcmp(format.image, last_image) && format.form == last_form
	    && ((!format.js_event && !last_js_event) || !compare_js_event_spec(format.js_event, last_js_event))) {
		if (!p->data) goto back_link;
		if (!p->data->nlinks) {
			internal("no link");
			goto back_link;
		}
		link = &p->data->links[p->data->nlinks - 1];
		goto back_link;
	} else {
		if (last_link) mem_free(last_link);
		if (last_target) mem_free(last_target);
		if (last_image) mem_free(last_image);
		free_js_event_spec(last_js_event);
		last_link = last_target = last_image = NULL;
		last_form = NULL;
		last_js_event = NULL;
		if (!(format.link || format.image || format.form || format.js_event)) goto back_link;
		/*if (d_opt->num_links) {
			unsigned char s[64];
			unsigned char *fl = format.link, *ft = format.target, *fi = format.image;
			struct form_control *ff = format.form;
			struct js_event_spec *js = format.js_event;
			format.link = format.target = format.image = NULL;
			format.form = NULL;
			format.js_event = NULL;
			s[0] = '[';
			snzprint(s + 1, 62, p->link_num);
			strcat(s, "]");
			g_put_chars(p, s, strlen(s));
			if (ff && ff->type == FC_TEXTAREA) g_line_break(p);
			if (p->cx < par_format.leftmargin * G_HTML_MARGIN) p->cx = par_format.leftmargin * G_HTML_MARGIN;
			format.link = fl, format.target = ft, format.image = fi;
			format.form = ff;
			format.js_event = js;
		}*/
		p->link_num++;
		last_link = stracpy(format.link);
		last_target = stracpy(format.target);
		last_image = stracpy(format.image);
		last_form = format.form;
		copy_js_event_spec(&last_js_event, format.js_event);
		if (!p->data) goto back_link;
		if (!(link = new_link(p->data))) goto back_link;
		link->num = p->link_num - 1;
		link->pos = DUMMY;
		copy_js_event_spec(&link->js_event, format.js_event);
		if (!last_form) {
			link->type = L_LINK;
			link->where = stracpy(last_link);
			link->target = stracpy(last_target);
		} else {
			link->type = last_form->type == FC_TEXT || last_form->type == FC_PASSWORD || last_form->type == FC_FILE ? L_FIELD : last_form->type == FC_TEXTAREA ? L_AREA : last_form->type == FC_CHECKBOX || last_form->type == FC_RADIO ? L_CHECKBOX : last_form->type == FC_SELECT ? L_SELECT : L_BUTTON;
			link->form = last_form;
			link->target = stracpy(last_form->target);
		}
		link->where_img = stracpy(last_image);
		link->sel_color = 0;
		link->n = 0;
	}
	goto back_link;
}

static void g_do_format(char *start, char *end, struct g_part *part, unsigned char *head)
{
	pr(
	parse_html(start, end, (void (*)(void *, unsigned char *, int))g_put_chars, g_line_break, (void *(*)(void *, int, ...))g_html_special, part, head);
	/*if ((part->y -= line_breax) < 0) part->y = 0;*/
	flush_pending_text_to_line(part);
	flush_pending_line_to_obj(part, 0);
	) {};
}

struct g_table_cache_entry {
	struct g_table_cache_entry *next;
	struct g_table_cache_entry *prev;
	unsigned char *start;
	unsigned char *end;
	int align;
	int m;
	int width;
	int link_num;
	struct g_part p;
};

static struct list_head g_table_cache = { &g_table_cache, &g_table_cache };

void g_free_table_cache(void)
{
	free_list(g_table_cache);
}

struct g_part *g_format_html_part(unsigned char *start, unsigned char *end, int align, int m, int width, unsigned char *head, int link_num, unsigned char *bg, unsigned char *bgcolor, struct f_data *f_d)
{
	int wa;
	struct g_part *p;
	struct html_element *e;
	struct form_control *fc;
	int lm = margin;

	struct g_table_cache_entry *tce;

	if (!f_d) foreach(tce, g_table_cache) {
		if (tce->start == start && tce->end == end && tce->align == align && tce->m == m && tce->width == width && tce->link_num == link_num) {
			p = mem_alloc(sizeof(struct g_part));
			memcpy(p, &tce->p, sizeof(struct part));
			return p;
		}
	}
	margin = m;

	/*d_opt->tables = 0;*/

	if (last_link) mem_free(last_link);
	if (last_image) mem_free(last_image);
	if (last_target) mem_free(last_target);
	free_js_event_spec(last_js_event);
	last_link = last_image = last_target = NULL;
	last_form = NULL;
	last_js_event = NULL;

	cached_font_face = to_je_ale_prasarna;
	p = mem_calloc(sizeof(struct g_part));
	{
		struct g_object_area *a;
		a = mem_calloc(sizeof(struct g_object_area) + sizeof(struct g_object_line *));
		a->bg = get_background(bg, bgcolor);
		if (bgcolor) decode_color(bgcolor, &format.bg);
		if (bgcolor) decode_color(bgcolor, &par_format.bgcolor);
		a->mouse_event = g_area_mouse;
		a->draw = g_area_draw;
		a->destruct = g_area_destruct;
		a->get_list = g_area_get_list;
		a->lfo = DUMMY;
		a->rfo = DUMMY;
		a->n_lines = 0;
		p->root = a;
		init_list(p->uf);
	}
	p->data = f_d;
	p->x = p->y = 0;
	p->xmax = 0;
	html_stack_dup();
	e = &html_top;
	html_top.dontkill = 2;
	html_top.namelen = 0;
	par_format.align = align;
	par_format.leftmargin = m;
	par_format.rightmargin = m;
	par_format.width = width;
	par_format.list_level = 0;
	par_format.list_number = 0;
	par_format.dd_margin = 0;
	p->cx = -1;
	p->cx_w = 0;
	g_nobreak = 1;
	g_do_format(start, end, p, head);
	g_nobreak = 0;
	line_breax = 1;
	while (&html_top != e) {
		kill_html_stack_item(&html_top);
		if (!&html_top || (void *)&html_top == (void *)&html_stack) {
			internal("html stack trashed");
			break;
		}
	}
	html_top.dontkill = 0;

	wa = g_get_area_width(p->root);
	if (wa > p->x) p->x = wa;
	g_x_extend_area(p->root, p->x, 0);
	if (p->x > p->xmax) p->xmax = p->x;
	p->y = p->root->yw;
	/*debug("WIDTH: obj (%d, %d), p (%d %d)", p->root->xw, p->root->yw, p->x, p->y);*/

	kill_html_stack_item(&html_top);
	if (!f_d) g_release_part(p), p->root = NULL;
	if (cached_font_face && cached_font_face != to_je_ale_prasarna) mem_free(cached_font_face);
	cached_font_face = to_je_ale_prasarna;

	foreach(fc, p->uf) destroy_fc(fc);
	free_list(p->uf);

	margin = lm;

	if (last_link) mem_free(last_link);
	if (last_image) mem_free(last_image);
	if (last_target) mem_free(last_target);
	free_js_event_spec(last_js_event);
	last_link = last_image = last_target = NULL;
	last_form = NULL;
	last_js_event = NULL;

	if (table_level > 1 && !f_d) {
		tce = mem_alloc(sizeof(struct g_table_cache_entry));
		tce->start = start;
		tce->end = end;
		tce->align = align;
		tce->m = m;
		tce->width = width;
		tce->link_num = link_num;
		memcpy(&tce->p, p, sizeof(struct g_part));
		add_to_list(g_table_cache, tce);
	}

	return p;
}

void g_release_part(struct g_part *p)
{
	if (p->text) p->text->destruct(p->text);
	if (p->line) p->line->destruct(p->line);
	if (p->root) p->root->destruct(p->root);
	if (p->current_style) g_free_style(p->current_style);
}

static void g_scan_width(struct g_object **o, int n, int *w)
{
	while (n--) {
		if ((*o)->x + (*o)->xw > *w) *w = (*o)->x + (*o)->xw;
		o++;
	}
}

static void g_scan_lines(struct g_object_line **o, int n, int *w)
{
	while (n--) {
		if ((*o)->n_entries) {
			struct g_object *oo = (*o)->entries[(*o)->n_entries - 1];
			if ((*o)->x + oo->x + oo->xw > *w) *w = (*o)->x + oo->x + oo->xw;
		}
		o++;
	}
}

int g_get_area_width(struct g_object_area *a)
{
	int w = 0;
	g_scan_width(a->lfo, a->n_lfo, &w);
	g_scan_width(a->rfo, a->n_rfo, &w);
	g_scan_lines(a->lines, a->n_lines, &w);
	return w;
}

void g_x_extend_area(struct g_object_area *a, int width, int height)
{
	struct g_object_line *l;
	int i;
	a->xw = width;
	for (i = 0; i < a->n_lines; i++) {
		a->lines[i]->xw = width;
	}
	for (i = a->n_lines - 1; i >= 0; i--) {
		l = a->lines[i];
		if (!l->n_entries) {
			a->yw -= l->yw;
			l->destruct(l);
			a->n_lines--;
			continue;
		}
		break;
	}
	if (a->yw >= height) return;
	l = mem_calloc(sizeof(struct g_object_line));
	l->mouse_event = g_line_mouse;
	l->draw = g_line_draw;
	l->destruct = g_line_destruct;
	l->get_list = g_line_get_list;
	l->x = 0;
	l->y = a->yw;
	l->xw = width;
	l->yw = height - a->yw;
	l->bg = a->bg;
	l->n_entries = 0;
	a->lines[a->n_lines] = l;
	a->n_lines++;
}



#endif
