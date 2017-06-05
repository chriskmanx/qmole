#include <stdio.h>
#include <alloca.h>
#include "widget-utils.h"

/**************************************************************************
  Parsing utils
**************************************************************************/

static int parse_image_dimensions(int *x, int *y, int *w, int *h,
		struct config_format_entry *e)
{
	/* XXX */
	struct config_format_entry *ee = find_config_format_entry(e, "xywh");
	if (ee && ee->value) {
	  if (4 == sscanf(ee->value, "%d %d %d %d", x, y, w, h)) {
	    XWARNING("Parsed  \"xywh\" value, "
		     "the format is: \"%%d %%d %%d %%d\" (line: %u)",
		     ee->line);
	    printf ("Parsed values x:y %d %d and w:h  %d %d\n",x,x,w,h);
	    
	    return 0;
	  }
	  else
	    XWARNING("Failed to parse \"xywh\" value, "
		     "the format is: \"%%d %%d %%d %%d\" (line: %u)",
		     ee->line);
	}
	return -1;
}


static int parse_color(unsigned char *out, const char *name, 
		       struct config_format_entry *e)
{
	out[0] = out[1] = out[2] = 0;
	struct config_format_entry *ee = find_config_format_entry(e, name);
	if (ee && ee->value) {
		if (3 == sscanf(ee->value, "%hhu %hhu %hhu", &out[0], &out[1], &out[2]))
			return 0;
		else
			XWARNING("Failed to parse \"color\" value, "
				 "the format is: \"%%d %%d %%d\" (line: %u)",
				 ee->line);
	}
	return -1;
}

int parse_2ints(int *out, const char *name, struct config_format_entry *e)
{
	out[0] = out[1] = 0;
	struct config_format_entry *ee = find_config_format_entry(e, name);
	if (ee && ee->value) {
	  if (2 == sscanf(ee->value, "%d %d", &out[0], &out[1])){
	    XWARNING("Parsed 2 ints \"%s\" value, "
		     "the format is: \"%%d %%d\" (line: %u)", 
		     name, ee->line);
	    printf ("Parsed values %d %d\n",out[0],out[1]);
	    return 0;
	  }
	  else
	    XWARNING("Failed to parse 2 ints \"%s\" value, "
		     "the format is: \"%%d %%d\" (line: %u)", 
		     name, ee->line);
	}
	return -1;
}

int parse_align(const char *name, struct config_format_entry *e)
{
	struct config_format_entry *ee = find_config_format_entry(e, name);
	if (!ee || !ee->value)
		return ALIGN_LEFT;

	if (strcmp("left", ee->value) == 0)
		return ALIGN_LEFT;
	else if (strcmp("right", ee->value) == 0)
		return ALIGN_RIGHT;
	else if (strcmp("center", ee->value) == 0)
		return ALIGN_CENTER;
	XWARNING("Unknown align type: \"%s\", back to default \"left\""
		 " (line: %d)", ee->value, ee->line);
	return ALIGN_LEFT;
}

cairo_surface_t *parse_image_part(struct config_format_entry *e,
				  struct config_format_tree *tree,
				  int required)
{
	cairo_surface_t *img = 0;
	int x,y,w,h;
	x = y = w = h = -1;
	char *file;
	size_t filestrlen;
	
	if (!e->value) {
		if (required)
			XWARNING("Missing image file name which is required ",
				 "(line: %u)", e->line);
		else
			XWARNING("Missing image file name (line: %u)", e->line); 
		return img;
	}

	filestrlen = strlen(tree->dir) + 1 + strlen(e->value) + 1;

	/* compute path */
	if (filestrlen > MAX_ALLOCA)
		file = xmalloc(filestrlen);
	else
		file = alloca(filestrlen);
	if (!strcmp(tree->dir, ""))
		strcpy(file, e->value);
	else
		sprintf(file, "%s/%s", tree->dir, e->value);

	/* load file */
	if (parse_image_dimensions(&x,&y,&w,&h,e) == 0)
		img = get_image_part(file,x,y,w,h);
	else
		img = get_image(file);
	
	if (!img) {
		if (required)
			XWARNING("Failed to load image \"%s\" which is required "
				 "(line: %u)", file, e->line);
		else
			XWARNING("Failed to load image \"%s\" (line: %u)", 
				 file, e->line);
	}

	/* free path */
	if (filestrlen > MAX_ALLOCA)
		xfree(file);

	return img;
}

cairo_surface_t *parse_image_part_named(const char *name, 
					struct config_format_entry *e,
					struct config_format_tree *tree, 
					int required)
{
	struct config_format_entry *ee = find_config_format_entry(e, name);
	if (!ee) {
		if (required)
			required_entry_not_found(e, name);
		return 0;
	}
	return parse_image_part(ee, tree, required);
}

int parse_triple_image(struct triple_image *tbt, struct config_format_entry *e, 
		       struct config_format_tree *tree, int required)
{
	tbt->center = parse_image_part_named("center", e, tree, required);
	if (!tbt->center && required)
		return -1;

	tbt->left = parse_image_part_named("left", e, tree, 0);
	tbt->right = parse_image_part_named("right", e, tree, 0);
	tbt->stretched = parse_bool("stretched", e);
	tbt->stretched_overlap = parse_bool("stretched_overlap", e);
	parse_2ints(tbt->center_offsets, "center_offsets", e);
	return 0;
}

int parse_triple_image_named(struct triple_image *tri, const char *name, 
			     struct config_format_entry *e, 
			     struct config_format_tree *tree, int required)
{
	struct config_format_entry *ee = find_config_format_entry(e, name);
	if (!ee) {
		if (required)
			required_entry_not_found(e, name);
		return -1;
	}

	return parse_triple_image(tri, ee, tree, required);
}

void free_triple_image(struct triple_image *tbt)
{
	if (tbt->center)
		cairo_surface_destroy(tbt->center);
	if (tbt->left) 
		cairo_surface_destroy(tbt->left);
	if (tbt->right) 
		cairo_surface_destroy(tbt->right);
}

int parse_text_info(struct text_info *out, struct config_format_entry *e)
{
	out->pfd = pango_font_description_from_string(e->value);
	parse_color(out->color, "color", e);
	parse_2ints(out->offset, "offset", e);
	out->align = parse_align("align", e);
	parse_color(out->shadow_color, "shadow_color", e);
	parse_2ints(out->shadow_offset, "shadow_offset", e);

	return 0;
}

int parse_text_info_named(struct text_info *out, const char *name,
			  struct config_format_entry *e, int required)
{
	struct config_format_entry *ee = find_config_format_entry(e, name);
	if (!ee) {
		if (required)
			required_entry_not_found(e, name);
		return -1;
	}

	return parse_text_info(out, ee);
}

void free_text_info(struct text_info *fi)
{
	pango_font_description_free(fi->pfd);
}

int parse_int(const char *name, struct config_format_entry *e, int def)
{
	const char *v = find_config_format_entry_value(e, name);
	int i;
	if (v && 1 == sscanf(v, "%d", &i))
			return i;
	return def;
}

int parse_int_or_percents(const char *name, struct config_format_entry *e,
			  int def, int *ispercents)
{
	const char *v = find_config_format_entry_value(e, name);
	int i;
	if (v && 1 == sscanf(v, "%d", &i)) {
		if (ispercents && strchr(v, '%') != 0)
			*ispercents = 1;
		else if (ispercents) 
			*ispercents = 0;
		return i;
	}
	return def;
}

int parse_bool(const char *name, struct config_format_entry *e)
{
	struct config_format_entry *ee = find_config_format_entry(e, name);
	return (ee != 0);
}

char *parse_string(const char *name, struct config_format_entry *e, const char *def)
{
	const char *v = find_config_format_entry_value(e, name);
	if (v)
		return xstrdup(v);
	return xstrdup(def);
}

char *parse_string_or_null(const char *name, struct config_format_entry *e)
{
	const char *v = find_config_format_entry_value(e, name);
	if (v)
		return xstrdup(v);
	return 0;
}

void required_entry_not_found(struct config_format_entry *e, const char *name)
{
	char buf[2048];
	memset(buf, 0, sizeof(buf));
	config_format_entry_path(buf, sizeof(buf), e);
	XWARNING("Failed to find \"%s/%s\" entry which is required",
		 buf, name);
}

/**************************************************************************
  Drawing utils
**************************************************************************/

int image_width(cairo_surface_t *img)
{
	if (img)
		return cairo_image_surface_get_width(img);
	return 0;
}

int image_height(cairo_surface_t *img)
{
	if (img)
		return cairo_image_surface_get_height(img);
	return 0;
}

void blit_image(cairo_surface_t *src, cairo_t *dest, int dstx, int dsty)
{
	size_t sh = image_height(src);
	size_t sw = image_width(src);

	blit_image_ex(src, dest, 0, 0, sw, sh, dstx, dsty);
}

void blit_image_ex(cairo_surface_t *src, cairo_t *dest, int srcx, int srcy,
		   int width, int height, int dstx, int dsty)
{
	cairo_save(dest);
	cairo_set_source_surface(dest, src, dstx-srcx, dsty-srcy);
	cairo_translate(dest, dstx, dsty);
	cairo_rectangle(dest, 0, 0, width, height);
	cairo_clip(dest);
	cairo_paint(dest);
	cairo_restore(dest);
}

void pattern_image(cairo_surface_t *src, cairo_t *dest, 
		   int dstx, int dsty, int w, int align)
{
	size_t sh = image_height(src);

	cairo_save(dest);
	if (align)
		cairo_set_source_surface(dest, src, dstx, dsty);
	else
		cairo_set_source_surface(dest, src, 0, 0);
	cairo_pattern_set_extend(cairo_get_source(dest), CAIRO_EXTEND_REPEAT);
	cairo_rectangle(dest, dstx, dsty, w, sh);
	cairo_clip(dest);
	cairo_paint(dest);
	cairo_restore(dest);
}

void stretch_image(cairo_surface_t *src, cairo_t *dest,
		   int dstx, int dsty, int w)
{
	cairo_matrix_t scale;
	cairo_matrix_t m;
	cairo_pattern_t *srcp;

	size_t sh = image_height(src);
	size_t sw = image_width(src);
	cairo_matrix_init_scale(&scale, (double)sw / w, 1.0);

	cairo_save(dest);

	cairo_set_source_surface(dest, src, dstx, dsty);
	srcp = cairo_get_source(dest);

	cairo_pattern_set_extend(srcp, CAIRO_EXTEND_PAD);
	cairo_pattern_get_matrix(srcp, &m);
	cairo_matrix_multiply(&m, &m, &scale);
	cairo_pattern_set_matrix(srcp, &m);

	cairo_rectangle(dest, dstx, dsty, w, sh);
	cairo_clip(dest);
	cairo_paint(dest);
	cairo_restore(dest);
}

void draw_text(cairo_t *cr, PangoLayout *dest, struct text_info *ti, 
		const char *text, int x, int y, int w, int h, int ellipsized)
{
	const static PangoEllipsizeMode ellipsize_table[] = {
		PANGO_ELLIPSIZE_MIDDLE,
		PANGO_ELLIPSIZE_END,
		PANGO_ELLIPSIZE_START
	};

	PangoRectangle r;
	int offsetx = 0, offsety = 0;

	cairo_save(cr);
	cairo_set_source_rgb(cr,
			(double)ti->color[0] / 255.0,
			(double)ti->color[1] / 255.0,
			(double)ti->color[2] / 255.0);
	pango_layout_set_font_description(dest, ti->pfd);
	pango_layout_set_text(dest, text, -1);
	pango_layout_set_width(dest, -1);
	
	pango_layout_get_pixel_extents(dest, 0, &r);

	offsety = (h - r.height) / 2;
	switch (ti->align) {
	default:
	case ALIGN_CENTER:
		offsetx = (w - r.width) / 2;
		break;
	case ALIGN_LEFT:
		offsetx = 0;
		break;
	case ALIGN_RIGHT:
		offsetx = w - r.width;
		break;
	}
	
	offsetx += ti->offset[0];
	offsety += ti->offset[1];

	cairo_translate(cr, x, y);
	cairo_rectangle(cr, 0, 0, w, h);
	cairo_translate(cr, offsetx, offsety);
	cairo_clip(cr);
	if (ellipsized) {
		pango_layout_set_ellipsize(dest, ellipsize_table[ti->align]);
		pango_layout_set_width(dest, (w - offsetx) * PANGO_SCALE);
	}
	pango_cairo_update_layout(cr, dest);

	if (ti->shadow_offset[0] != 0 || ti->shadow_offset[1] != 0) {
		cairo_save(cr);
		cairo_translate(cr, ti->shadow_offset[0], ti->shadow_offset[1]);
		cairo_set_source_rgb(cr,
				(double)ti->shadow_color[0] / 255.0,
				(double)ti->shadow_color[1] / 255.0,
				(double)ti->shadow_color[2] / 255.0);
		pango_cairo_show_layout(cr, dest);
		cairo_restore(cr);
	}

	pango_cairo_show_layout(cr, dest);
	cairo_restore(cr);
}

void text_extents(PangoLayout *layout, PangoFontDescription *font,
		const char *text, int *w, int *h)
{
	PangoRectangle r;
	pango_layout_set_font_description(layout, font);
	pango_layout_set_text(layout, text, -1);
	pango_layout_get_pixel_extents(layout, 0, &r);
	if (w)
		*w = r.width;
	if (h)
		*h = r.height;
}

/**************************************************************************
  Buffer utils
**************************************************************************/

static char *static_buf;

void *get_static_buf_or_xalloc(size_t size)
{
	if (size <= STATIC_BUF_SIZE) {
		if (!static_buf)
			static_buf = xmalloc(STATIC_BUF_SIZE);
		return static_buf;
	}
	return xmalloc(size);
}

void free_static_buf(void *ptr)
{
	if (ptr != static_buf)
		xfree(ptr);
}

void clean_static_buf()
{
	if (static_buf) {
		xfree(static_buf);
		static_buf = 0;
	}
}

/**************************************************************************
  X imaging utils
**************************************************************************/

static cairo_user_data_key_t surface_data_key;

static void free_custom_surface_data(void *ptr)
{
	free_static_buf(ptr);
}

static cairo_surface_t *get_icon_from_netwm(long *data)
{
	cairo_surface_t *ret = 0;
	uint32_t *array = 0; 
	uint32_t w,h,size,i;
	long *locdata = data;

	w = *locdata++;
	h = *locdata++;
	size = w * h;

	/* convert netwm icon format to cairo data */
	/* array = xmalloc(sizeof(uint32_t) * size); */
	array = get_static_buf_or_xalloc(sizeof(uint32_t) * size);
	for (i = 0; i < size; ++i) {
		unsigned char *a, *d;
		a = (unsigned char*)&array[i];
		d = (unsigned char*)&locdata[i];
		a[0] = d[0];
		a[1] = d[1];
		a[2] = d[2];
		a[3] = d[3];
		/* premultiply alpha */
		a[0] *= (float)d[3] / 255.0f;
		a[1] *= (float)d[3] / 255.0f;
		a[2] *= (float)d[3] / 255.0f;
	}

	/* TODO: I'm pretending here, that stride is w*4, but it should
	 * be tested/verified 
	 */
	ret = cairo_image_surface_create_for_data((unsigned char*)array, 
			CAIRO_FORMAT_ARGB32, w, h, w*4);		
	ENSURE(cairo_surface_status(ret) == CAIRO_STATUS_SUCCESS,
	       "Failed to create cairo image surface");
	cairo_status_t st;
	st = cairo_surface_set_user_data(ret, &surface_data_key, 
					 array, free_custom_surface_data);
	ENSURE(st == CAIRO_STATUS_SUCCESS,
	       "Failed to set user data for surface");

	return ret;
}

static cairo_surface_t *get_icon_from_pixmap(struct x_connection *c, 
		Pixmap icon, Pixmap icon_mask)
{
	Window root_ret;
	int x = 0, y = 0;
	unsigned int w = 0, h = 0, d = 0, bw = 0;
	cairo_surface_t *ret = 0;
	cairo_surface_t *sicon = 0, *smask = 0;
	
	XGetGeometry(c->dpy, icon, &root_ret, 
			&x, &y, &w, &h, &bw, &d);

	/* yep, it is that bad */
	if (d == 1)
		sicon = cairo_xlib_surface_create_for_bitmap(c->dpy, icon,
				DefaultScreenOfDisplay(c->dpy), w, h);
	else
		sicon = cairo_xlib_surface_create(c->dpy, icon, 
				c->default_visual, w, h);
	ENSURE(cairo_surface_status(sicon) == CAIRO_STATUS_SUCCESS,
	       "Failed to create cairo/xlib surface");

	if (icon_mask != None) {
		smask = cairo_xlib_surface_create_for_bitmap(c->dpy, icon_mask,
				DefaultScreenOfDisplay(c->dpy), w, h);
		ENSURE(cairo_surface_status(smask) == CAIRO_STATUS_SUCCESS,
		       "Failed to create cairo/xlib surface");
	}

	ret = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, w, h);
	ENSURE(cairo_surface_status(ret) == CAIRO_STATUS_SUCCESS,
	       "Failed to create cairo image surface");

	cairo_t *cr = cairo_create(ret);
	ENSURE(cairo_status(cr) == CAIRO_STATUS_SUCCESS,
	       "Failed to create cairo context");

	/* fill with transparent (alpha == 0) */
	cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);
	cairo_set_source_rgba(cr, 0, 0, 0, 0);
	cairo_paint(cr);

	/* draw icon with mask */
	cairo_set_source_surface(cr, sicon, 0, 0);
	if (smask)
		cairo_mask_surface(cr, smask, 0, 0);
	else
		cairo_paint(cr);

	/* clean up */
	cairo_destroy(cr);
	cairo_surface_destroy(sicon);
	if (smask)
		cairo_surface_destroy(smask);

	return ret;
}

cairo_surface_t *get_window_icon(struct x_connection *c, Window win,
		cairo_surface_t *default_icon)
{
	cairo_surface_t *ret = 0;
	
	int num = 0;
	long *data = x_get_prop_data(c, win, c->atoms[XATOM_NET_WM_ICON],
			XA_CARDINAL, &num);

	/* TODO: look for best sized icon? */
	if (data) {
		ret = get_icon_from_netwm(data);
		XFree(data);
	}

	if (!ret) {
	        XWMHints *hints = XGetWMHints(c->dpy, win);
		if (hints) {
			if (hints->flags & IconPixmapHint) {
				ret = get_icon_from_pixmap(c, hints->icon_pixmap,
						hints->icon_mask);
			}
	        	XFree(hints);
		}
	}

	if (!ret) {
		cairo_surface_reference(default_icon);
		return default_icon;
	}

	int w = image_width(default_icon);
	int h = image_height(default_icon);

	cairo_surface_t *sizedret = copy_resized(ret, w, h);
	cairo_surface_destroy(ret);

	return sizedret;
}

cairo_surface_t *copy_resized(cairo_surface_t *source, int w, int h)
{
	double dw = (double)w;
	double dh = (double)h;
	double ow = image_width(source);
	double oh = image_height(source);

        printf ("copy_resized from x:y %f %f to x:y %f %f\n",ow,oh,dw,dh);

	cairo_surface_t *sizedret = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
							       dw, dh);
	ENSURE(cairo_surface_status(sizedret) == CAIRO_STATUS_SUCCESS,
	       "Failed to create cairo image surface");

	cairo_t *cr = cairo_create(sizedret);
	ENSURE(cairo_status(cr) == CAIRO_STATUS_SUCCESS,
	       "Failed to create cairo context");

	cairo_scale(cr, dw / ow, dh / oh);
	cairo_set_source_surface(cr, source, 0, 0);
	cairo_paint(cr);

	cairo_destroy(cr);

	return sizedret;
}

cairo_t *create_cairo_for_pixmap(struct x_connection *c, Pixmap p, int w, int h)
{
	cairo_surface_t *surface = cairo_xlib_surface_create(c->dpy, 
							     p, c->default_visual,
							     w, h);
	ENSURE(cairo_surface_status(surface) == CAIRO_STATUS_SUCCESS, 
	       "Error creating xlib/cairo surface");

	cairo_t *cr = cairo_create(surface);
	cairo_surface_destroy(surface);

	ENSURE(cairo_status(cr) == CAIRO_STATUS_SUCCESS, 
	       "Error creating cairo context");

	return cr;
}

cairo_surface_t *create_cairo_surface_for_pixmap(struct x_connection *c, Pixmap p,
						 int w, int h)
{
	cairo_surface_t *surface = cairo_xlib_surface_create(c->dpy, 
							     p, c->default_visual,
							     w, h);
	ENSURE(cairo_surface_status(surface) == CAIRO_STATUS_SUCCESS, 
	       "Error creating xlib/cairo surface");

	return surface;
}

cairo_t *create_cairo_for_bitmap(struct x_connection *c, Pixmap p, int w, int h)
{
	cairo_surface_t *surface = cairo_xlib_surface_create_for_bitmap(
			c->dpy, p, DefaultScreenOfDisplay(c->dpy), w, h);

	ENSURE(cairo_surface_status(surface) == CAIRO_STATUS_SUCCESS, 
	       "Error creating xlib/cairo surface");

	cairo_t *cr = cairo_create(surface);
	cairo_surface_destroy(surface);

	ENSURE(cairo_status(cr) == CAIRO_STATUS_SUCCESS, 
	       "Error creating cairo context");

	return cr;
}
