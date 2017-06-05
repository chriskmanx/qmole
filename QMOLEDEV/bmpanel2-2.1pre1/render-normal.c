#include "gui.h"
#include "widget-utils.h"

static void create_dc(struct panel *p);
static void blit(struct panel *p, int x, int y, unsigned int w, unsigned int h);
static void panel_resize(struct panel *p);

struct render_interface render_normal = {
	.name = "normal",
	.create_dc = create_dc,
	.blit = blit,
	.panel_resize = panel_resize
};

static void create_dc(struct panel *p)
{
	p->cr = create_cairo_for_pixmap(&p->connection, p->bg, 
					p->width, p->height);
}
	
static void blit(struct panel *p, int x, int y, unsigned int w, unsigned int h)
{
	XClearArea(p->connection.dpy, p->win, x, y, w, h, False);
}

static void panel_resize(struct panel *p)
{
	struct x_connection *c = &p->connection;

	cairo_destroy(p->cr);
	XFreePixmap(c->dpy, p->bg);

	p->bg = x_create_default_pixmap(c, p->width, p->height);
	XSetWindowBackgroundPixmap(c->dpy, p->win, p->bg);
	p->cr = create_cairo_for_pixmap(c, p->bg, p->width, p->height);
}
