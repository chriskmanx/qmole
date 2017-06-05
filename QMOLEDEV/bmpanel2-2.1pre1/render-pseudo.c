#include "gui.h"
#include "widget-utils.h"

static void create_dc(struct panel *p);
static void blit(struct panel *p, int x, int y, unsigned int w, unsigned int h);
static void create_private(struct panel *p);
static void free_private(struct panel *p);
static void update_bg(struct panel *p);
static void panel_resize(struct panel *p);

struct render_interface render_pseudo = {
	.name = "pseudo",
	.create_dc = create_dc,
	.blit = blit,
	.create_private = create_private,
	.free_private = free_private,
	.update_bg = update_bg,
	.panel_resize = panel_resize
};

/*
 * blit_cr is a real p->bg interface
 * backbuf is a storage for p->cr
 */
struct pseudo_render {
	Pixmap buf;
	cairo_t *buf_cr;
	cairo_t *blit_cr;
	cairo_surface_t *wallpaper;
};

static void create_private(struct panel *p)
{
	struct x_connection *c = &p->connection;
	struct pseudo_render *pr = xmallocz(sizeof(struct pseudo_render));

	pr->blit_cr = create_cairo_for_pixmap(c, p->bg, p->width, p->height);
	pr->buf = x_create_default_pixmap(c, p->width, p->height);
	pr->buf_cr = create_cairo_for_pixmap(c, pr->buf, p->width, p->height);
	
	if (c->root_pixmap != None)
		pr->wallpaper = create_cairo_surface_for_pixmap(c, c->root_pixmap,
								c->screen_width,
								c->screen_height);
	p->render_private = (void*)pr;
}

static void free_private(struct panel *p)
{
	Display *dpy = p->connection.dpy;
	struct pseudo_render *pr = p->render_private;
	cairo_destroy(pr->buf_cr);
	cairo_destroy(pr->blit_cr);
	if (pr->wallpaper)
		cairo_surface_destroy(pr->wallpaper);
	XFreePixmap(dpy, pr->buf);
	xfree(pr);
}

static void create_dc(struct panel *p)
{
	cairo_surface_t *backbuf = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
							      p->width, p->height);

	p->cr = cairo_create(backbuf);
	cairo_surface_destroy(backbuf);
}
	
static void blit(struct panel *p, int x, int y, unsigned int w, unsigned int h)
{
	Display *dpy = p->connection.dpy;
	struct pseudo_render *pr = p->render_private;

	/* draw wallpaper or clear buffer */
	if (pr->wallpaper) {
		blit_image_ex(pr->wallpaper, pr->buf_cr, p->x + x, p->y + y, 
			      w, h, x, y);
	} else {
		cairo_save(pr->buf_cr);
		cairo_set_source_rgb(pr->buf_cr, 0,0,0);
		cairo_paint(pr->buf_cr);
		cairo_restore(pr->buf_cr);
	}
	/* composite gui with background */
	blit_image_ex(cairo_get_target(p->cr), pr->buf_cr, x, y, w, h, x, y);
	cairo_save(p->cr);
	cairo_set_operator(p->cr, CAIRO_OPERATOR_SOURCE);
	cairo_set_source_rgba(p->cr, 0, 0, 0, 0);
	cairo_paint(p->cr);
	cairo_restore(p->cr);
	
	/* put everything to the background pixmap and clear area */
	blit_image_ex(cairo_get_target(pr->buf_cr), pr->blit_cr, x, y, w, h, x, y);
	XClearArea(dpy, p->win, x, y, w, h, False);
}

static void update_bg(struct panel *p)
{
	struct x_connection *c = &p->connection;
	struct pseudo_render *pr = p->render_private;

	x_update_root_pmap(c);
	cairo_surface_destroy(pr->wallpaper);
	
	if (c->root_pixmap != None)
		pr->wallpaper = create_cairo_surface_for_pixmap(c, c->root_pixmap,
								c->screen_width,
								c->screen_height);
	p->needs_expose = 1;
}

static void panel_resize(struct panel *p)
{
	struct x_connection *c = &p->connection;
	struct pseudo_render *pr = (struct pseudo_render*)p->render_private;

	/* pr->wallpaper */
	update_bg(p);

	/* p->cr */
	cairo_destroy(p->cr);
	cairo_surface_t *backbuf = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
							      p->width, p->height);

	p->cr = cairo_create(backbuf);
	cairo_surface_destroy(backbuf);

	/* p->bg */
	XFreePixmap(c->dpy, p->bg);
	p->bg = x_create_default_pixmap(c, p->width, p->height);
	XSetWindowBackgroundPixmap(c->dpy, p->win, p->bg);

	/* pr->blit_cr */
	cairo_destroy(pr->blit_cr);
	pr->blit_cr = create_cairo_for_pixmap(c, p->bg, p->width, p->height);

	/* pr->buf */
	XFreePixmap(c->dpy, pr->buf);
	pr->buf = x_create_default_pixmap(c, p->width, p->height);

	/* pr->buf_cr */
	cairo_destroy(pr->buf_cr);
	pr->buf_cr = create_cairo_for_pixmap(c, pr->buf, p->width, p->height);
}
